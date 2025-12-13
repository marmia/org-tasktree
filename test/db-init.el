;;; db-init.el --- DB init helper (paths supplied by shell) -*- lexical-binding: t; -*-

;; Expected variables are set before load:
;; - org-tasktree-database-location : string
;; - org-tasktree-query-dir         : string
;; - repo-root                      : string (for load-path)

(unless (and (boundp 'org-tasktree-database-location)
             (stringp org-tasktree-database-location))
  (error "org-tasktree-database-location must be set to a string"))

(unless (and (boundp 'org-tasktree-query-dir)
             (stringp org-tasktree-query-dir))
  (error "org-tasktree-query-dir must be set to a string"))

(unless (and (boundp 'repo-root) (stringp repo-root))
  (error "repo-root must be set to repository path"))

(add-to-list 'load-path repo-root)
(require 'org-tasktree)
(org-tasktree-init)

(kill-emacs 0)

;;; db-init.el ends here
