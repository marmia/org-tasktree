;;; org-tasktree-search-by-query-ert.el --- ERT helpers for search-by-query -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Helper utilities for org-tasktree search-by-query ERT tests.
;; Provides by-query data seeding and query file installation helpers.
;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'rx)
(require 'seq)
(require 'sqlite)
(require 'subr-x)
(require 'org-tasktree-db)
(require 'org-tasktree-search-ert)
(require 'org-tasktree-test-helper)
(require 'org-tasktree-ui-minibuffer)

(defun org-tasktree-search-by-query-ert--replace-sql-now (text)
  "Return TEXT with SQLite now() dates replaced by the base time."
  (let ((regex
         (rx "date('now','localtime'"
             (? ",'" (group (or "+" "-") (+ digit)) " day" (? "s") "'")
             ")"))
        (pos 0)
        (parts nil))
    (while (string-match regex text pos)
      (let* ((delta-str (match-string 1 text))
             (days (if delta-str (string-to-number delta-str) 0))
             (date (org-tasktree-search-ert--format-date
                    (org-tasktree-search-ert--time-days-from
                     org-tasktree-search-ert--base-time days))))
        (push (substring text pos (match-beginning 0)) parts)
        (push (format "'%s'" date) parts)
        (setq pos (match-end 0))))
    (push (substring text pos) parts)
    (apply #'concat (nreverse parts))))

(defun org-tasktree-search-by-query-ert--exec-sql-file (file)
  "Execute SQL statements loaded from FILE."
  (let* ((text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (text (org-tasktree-search-by-query-ert--replace-sql-now text))
         (stmts (seq-filter
                 (lambda (s) (string-match-p "\\S-" s))
                 (mapcar #'string-trim
                         (split-string text ";" t)))))
    (org-tasktree-db--with-db db
      (dolist (stmt stmts)
        (sqlite-execute db stmt)))))

(defun org-tasktree-search-by-query-ert--by-query-sql-path ()
  "Return absolute path for by-query SQL seed file."
  (expand-file-name "test/by-query-testdata.sql"
                    (org-tasktree-search-ert--repo-root)))

(defun org-tasktree-search-by-query-ert--by-query-file-path (name)
  "Return absolute path for query file NAME."
  (expand-file-name (concat "test/test-data/query/" name)
                    (org-tasktree-search-ert--repo-root)))

(defun org-tasktree-search-by-query-ert--seed-data ()
  "Seed DB with by-query test data."
  (org-tasktree-test-helper-reset-db)
  (org-tasktree-search-by-query-ert--exec-sql-file
   (org-tasktree-search-by-query-ert--by-query-sql-path)))

(defun org-tasktree-search-by-query-ert--install-query-file (name)
  "Copy query file NAME into `org-tasktree-query-dir'."
  (let* ((src (org-tasktree-search-by-query-ert--by-query-file-path name))
         (dest-dir (org-tasktree-test-helper--query-dir))
         (dest (expand-file-name name dest-dir)))
    (make-directory dest-dir t)
    (copy-file src dest t)))

(defun org-tasktree-search-by-query-ert--query-title (name)
  "Return result buffer title for query file NAME."
  (file-name-base name))

(defmacro org-tasktree-search-by-query-ert-with-selection (choice &rest body)
  "Run BODY with query selection stubbed to return CHOICE."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-tasktree-ui-minibuffer--completing-read)
              (lambda (_prompt _cands &rest _args)
                ,choice)))
     ,@body))

(defmacro org-tasktree-search-by-query-ert-run-case (case)
  "Run search-by-query test CASE.
CASE is a plist with :query and either :expected or :message."
  (declare (indent 1))
  `(let* ((query (plist-get ,case :query))
          (expected (plist-get ,case :expected))
          (message (plist-get ,case :message)))
     (when (and (null expected) (null message))
       (error "Missing :expected or :message for case: %S" ,case))
     (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
       (org-tasktree-search-by-query-ert--seed-data)
       (org-tasktree-search-by-query-ert--install-query-file query)
       (org-tasktree-search-by-query-ert-with-selection query
         (if message
             (let (msg)
               (cl-letf (((symbol-function 'message)
                          (lambda (fmt &rest args)
                            (setq msg (apply #'format fmt args)))))
                 (save-window-excursion
                   (org-tasktree-search-by-query))
                 (should (string= msg message))
                 (should-not
                  (get-buffer
                   (format "*org-tasktree %s*"
                           (org-tasktree-search-by-query-ert--query-title
                            query))))
                 (should-not (get-buffer "*org-tasktree nil*"))))
           (save-window-excursion
             (org-tasktree-search-by-query))
           (org-tasktree-search-ert--assert-search-output
            (org-tasktree-search-by-query-ert--query-title query)
            expected))))))

(provide 'org-tasktree-search-by-query-ert)
;;; org-tasktree-search-by-query-ert.el ends here
