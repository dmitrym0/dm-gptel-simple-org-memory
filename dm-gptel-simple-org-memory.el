;;; dm-gptel-simple-org-memory.el --- Simple org-mode memory for GPTel via filesystem search -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides retrieval-augmented generation for gptel using simple filesystem
;; search of org-mode notes. Uses ag (silver searcher) to perform parallel
;; searches across multiple terms and returns structured results.

;;; Code:

(require 'cl-lib)
(require 'async)

(defcustom dm/gptel-org-memory-directory "~/org-roam/"
  "Directory containing org files to search for memory augmentation."
  :type 'string
  :group 'dm-gptel-memory)

(defcustom dm/gptel-org-memory-max-search-terms 10
  "Maximum number of search terms to process in parallel."
  :type 'integer
  :group 'dm-gptel-memory)

(defcustom dm/gptel-org-memory-default-context-lines 3
  "Default number of context lines around each match."
  :type 'integer
  :group 'dm-gptel-memory)

(defcustom dm/gptel-org-memory-max-results-per-term 10
  "Maximum number of results to return per search term."
  :type 'integer
  :group 'dm-gptel-memory)

(defcustom dm/gptel-org-memory-log-buffer "*GPTel Org Search Log*"
  "Buffer name for logging org memory search operations."
  :type 'string
  :group 'dm-gptel-memory)

(defun dm/gptel-org-memory-log (message &rest args)
  "Log MESSAGE with ARGS to the org memory search log buffer."
  (let ((log-buffer (get-buffer-create dm/gptel-org-memory-log-buffer))
        (timestamp (format-time-string "[%Y-%m-%d %H:%M:%S.%3N]"))
        (formatted-msg (if args (apply #'format message args) message)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (format "%s %s\n" timestamp formatted-msg))
      (when (> (buffer-size) 50000) ; Keep log size reasonable
        (goto-char (point-min))
        (delete-region (point-min) (progn (forward-line 100) (point)))))))

(defun dm/gptel-org-memory-clear-log ()
  "Clear the org memory search log buffer."
  (interactive)
  (let ((log-buffer (get-buffer dm/gptel-org-memory-log-buffer)))
    (when log-buffer
      (with-current-buffer log-buffer
        (erase-buffer))
      (message "Cleared org memory search log"))))

(defun dm/gptel-org-memory-show-log ()
  "Show the org memory search log buffer."
  (interactive)
  (display-buffer (get-buffer-create dm/gptel-org-memory-log-buffer)))


(defun dm/parse-ag-output (ag-output search-term)
  "Parse AG-OUTPUT and return structured match data for SEARCH-TERM."
  (dm/gptel-org-memory-log "Parsing ag output for search-term: '%s' (type: %s)"
                          search-term (type-of search-term))
  (let ((search-term-str (if (stringp search-term) search-term (format "%s" search-term)))
        (lines (split-string ag-output "\n"))
        (current-file nil)
        (matches '())
        (context-buffer '()))

    (dolist (line lines)
      (cond
       ;; Empty line or separator - reset context buffer
       ((or (string-empty-p line) (string= line "--"))
        (setq context-buffer '()))

       ;; File path line - doesn't start with number followed by : or -
       ((and (not (string-empty-p line))
             (not (string-match "^[0-9]+[-:]" line)))
        (dm/gptel-org-memory-log "Found file path: %s" line)
        (setq current-file line)
        (setq context-buffer '()))

       ;; Match line - starts with number:
       ((string-match "^\\([0-9]+\\):\\(.*\\)$" line)
        (let ((line-num (string-to-number (match-string 1 line)))
              (content (match-string 2 line)))
          (dm/gptel-org-memory-log "Found match line %d: %s" line-num content)
          ;; Add to context
          (push (format "%d:%s" line-num content) context-buffer)
          ;; Create match for this line
          (push (list :file current-file
                     :line-number line-num
                     :search-term search-term-str
                     :match-text content
                     :context (reverse context-buffer))
                matches)))

       ;; Context line - starts with number-
       ((string-match "^\\([0-9]+\\)-\\(.*\\)$" line)
        (let ((line-num (string-to-number (match-string 1 line)))
              (content (match-string 2 line)))
          (dm/gptel-org-memory-log "Found context line %d: %s" line-num content)
          ;; Add to context buffer
          (push (format "%d-%s" line-num content) context-buffer)))

       ;; Unrecognized line format
       (t
        (when (not (string-empty-p line))
          (dm/gptel-org-memory-log "Unrecognized line format: %s" line)))))

    (dm/gptel-org-memory-log "Parsed %d matches from ag output" (length matches))
    (nreverse matches)))

(defun dm/gptel-org-search-parallel (search-terms &optional context-lines file-pattern max-results-per-term)
  "Search for multiple SEARCH-TERMS sequentially using ag.
Returns results grouped by file path to naturally deduplicate.
CONTEXT-LINES: number of context lines around matches
FILE-PATTERN: file pattern to limit search
MAX-RESULTS-PER-TERM: maximum results per search term"
  (let* ((start-time (current-time))
         (context (or context-lines dm/gptel-org-memory-default-context-lines))
         (max-terms (min (length search-terms) dm/gptel-org-memory-max-search-terms))
         (terms-to-search (cl-subseq search-terms 0 max-terms))
         (max-results (or max-results-per-term dm/gptel-org-memory-max-results-per-term))
         (completed-results '())
         (results-by-file (make-hash-table :test 'equal)))

    (dm/gptel-org-memory-log "Starting sequential search for %d terms: %s"
                            (length terms-to-search)
                            (mapconcat #'identity terms-to-search ", "))

    ;; Run all searches sequentially
    (dolist (term terms-to-search)
      (let* ((search-dir (expand-file-name dm/gptel-org-memory-directory))
             (cmd (format "/opt/homebrew/bin/ag --nocolor -i -C %d --group %s %s"
                         context
                         (shell-quote-argument term)
                         (shell-quote-argument search-dir))))
        (dm/gptel-org-memory-log "Starting search for term: '%s' with command: %s" term cmd)
        (let ((default-directory search-dir))
          (with-temp-buffer
            (condition-case err
                (let ((exit-code (shell-command cmd (current-buffer)))
                      (output (buffer-string)))
                  (if (or (= exit-code 0) (= exit-code 1))
                      (let ((term-matches (dm/parse-ag-output output term)))
                        (dm/gptel-org-memory-log "Term '%s': found %d matches (exit-code: %d, output length: %d)"
                                                term (length term-matches) exit-code (length output))
                        (dm/gptel-org-memory-log "Raw ag output: %s" output)
                        (setq completed-results
                              (append completed-results
                                      (cl-subseq term-matches 0 (min max-results (length term-matches))))))
                    (dm/gptel-org-memory-log "Term '%s': search failed (exit-code: %d, cmd: %s, dir: %s)"
                                            term exit-code cmd search-dir)))
              (error (dm/gptel-org-memory-log "Term '%s': search error - %s (cmd: %s, dir: %s)"
                                            term (error-message-string err) cmd search-dir)))))))

    ;; Group matches by file for deduplication
    (dm/gptel-org-memory-log "Grouping %d total matches by file..." (length completed-results))
    (dolist (match completed-results)
      (let* ((file (plist-get match :file))
             (existing-matches (gethash file results-by-file '())))
        (puthash file (cons match existing-matches) results-by-file)))

    ;; Convert hash table to list format and log final results
    (let ((final-results '())
          (total-files 0)
          (total-matches 0))
      (maphash (lambda (file matches)
                 (push (list :file file :matches (nreverse matches)) final-results)
                 (setq total-files (1+ total-files))
                 (setq total-matches (+ total-matches (length matches))))
               results-by-file)

      (let ((elapsed-time (float-time (time-subtract (current-time) start-time))))
        (dm/gptel-org-memory-log "Search completed in %.2fs: %d files, %d total matches"
                                elapsed-time total-files total-matches))

      (nreverse final-results))))

(defun dm/format-search-results-json (results)
  "Format RESULTS into JSON structure with filename/context pairs."
  (if (null results)
      (json-encode (make-hash-table))
    (let ((result-dict (make-hash-table :test 'equal)))
      (dolist (file-result results)
        (let ((file (or (plist-get file-result :file) "unknown-file"))
              (matches (or (plist-get file-result :matches) '()))
              (contexts '()))
          ;; Collect all contexts for this file
          (dolist (match matches)
            (let ((context (or (plist-get match :context) '())))
              (when context
                (push (string-join (reverse context) "\n") contexts))))
          ;; Store in hash table
          (puthash file (string-join (reverse contexts) "\n\n---\n\n") result-dict)))
      (json-encode result-dict))))


(defun dm/gptel-search-org-memory-tool (search_terms &optional context_lines)
  "Tool function for searching org memory via MCP/gptel integration.
SEARCH_TERMS can be a vector, list, or single string of search terms.
CONTEXT_LINES specifies number of lines of context around matches."
  (dm/gptel-org-memory-log "=== MCP TOOL INVOKED ===")
  (dm/gptel-org-memory-log "Raw search_terms: %S (type: %s)" search_terms (type-of search_terms))
  (condition-case err
      (let* ((terms (cond
                     ((vectorp search_terms) (append search_terms nil))
                     ((listp search_terms) search_terms)
                     (t (list search_terms)))))
        (dm/gptel-org-memory-log "Converted terms: %S" terms)
        (let ((results (dm/gptel-org-search-parallel terms context_lines)))
          (dm/gptel-org-memory-log "Search completed, formatting results...")
          (dm/format-search-results-json results)))
    (error
     (dm/gptel-org-memory-log "MCP tool error: %s" (error-message-string err))
     (format "Search failed: %s" (error-message-string err)))))

(gptel-make-tool
 :name "search_org_memory"
 :function #'dm/gptel-search-org-memory-tool
 :description "Search org-mode notes using multiple search terms with ag (silver searcher). The output contains a json map that includes filenames as keys and context as values. WHen presenting context to the user show the file path where context was found."
 :args (list '(:name "search_terms"
               :type "array"
               :items (:type "string")
               :description "Array of search terms to look for in org files. Use regular expressions where appropriate.")
             '(:name "context_lines"
               :type "integer"
               :description "Number of lines of context around each match (default: 3)"))
 :category "org-memory")

(defun dm/test-org-search (&optional terms)
  "Test the org memory search functionality interactively.
TERMS is an optional list of search terms. If not provided, prompts for input."
  (interactive)
  (let* ((search-terms (or terms
                          (split-string (read-string "Enter search terms (space-separated): ")
                                       "\\s-+" t)))
         (results (dm/gptel-org-search-parallel search-terms)))
    (with-current-buffer (get-buffer-create "*Org Search Test Results*")
      (erase-buffer)
      (insert "=== Org Memory Search Test Results ===\n\n")
      (insert (format "Search terms: %s\n" (mapconcat #'identity search-terms ", ")))
      (insert (format "Results found: %d files\n\n" (length results)))
      (insert "JSON Output:\n")
      (insert (dm/format-search-results-json results))
      (insert "\n\nReadable Output:\n")
      ;; (insert (dm/format-search-results results)) ;
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message "Search completed. Check *Org Search Test Results* and *GPTel Org Search Log* buffers.")))

(defun dm/test-org-search-simple (term)
  "Quick test with a single search term."
  (interactive "sSearch term: ")
  (dm/test-org-search (list term)))

(provide 'dm-gptel-simple-org-memory)

;;; dm-gptel-simple-org-memory.el ends here
