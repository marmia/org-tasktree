;;; org-tasktree-test-helper.el --- Helpers for ERT tests -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Test helper utilities for org-tasktree ERT suites.
;; Provides DB reset helpers and a fixed-time wrapper for date-dependent tests.
;;

;;; Code:

(require 'cl-lib)
(require 'org-tasktree)

(defun org-tasktree-test-helper--assert-string (value name)
  "Signal an error when VALUE is not a string for NAME."
  (unless (stringp value)
    (error "%s must be a string" name)))

(defun org-tasktree-test-helper--assert-safe-path (path)
  "Signal an error when PATH is not under a test/temp directory."
  (org-tasktree-test-helper--assert-string path "path")
  (unless (string-match-p "/\\(test\\|tmp\\)/" (file-truename path))
    (error "Unsafe test path: %s" path)))

(defun org-tasktree-test-helper--db-path ()
  "Return org-tasktree database path for test."
  (org-tasktree-test-helper--assert-string
   org-tasktree-database-location
   "org-tasktree-database-location")
  org-tasktree-database-location)

(defun org-tasktree-test-helper--query-dir ()
  "Return org-tasktree query directory for test."
  (org-tasktree-test-helper--assert-string
   org-tasktree-query-dir
   "org-tasktree-query-dir")
  org-tasktree-query-dir)

(defun org-tasktree-test-helper-reset-db ()
  "Delete test DB/queries and initialize a fresh database."
  (let ((db-path (org-tasktree-test-helper--db-path))
        (query-dir (org-tasktree-test-helper--query-dir)))
    (org-tasktree-test-helper--assert-safe-path db-path)
    (org-tasktree-test-helper--assert-safe-path query-dir)
    (when (file-exists-p db-path)
      (delete-file db-path))
    (when (file-directory-p query-dir)
      (delete-directory query-dir t))
    (org-tasktree-init)))

(defmacro org-tasktree-test-helper-with-fixed-time (time &rest body)
  "Run BODY with TIME fixed as the current time.
TIME must be a time value suitable for `current-time'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'current-time) (lambda () ,time))
             (org-tasktree-query--now-function (lambda () ,time)))
     ,@body))

(provide 'org-tasktree-test-helper)
;;; org-tasktree-test-helper.el ends here
