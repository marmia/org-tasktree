;;; org-tasktree.el --- Task management via org-mode + SQLite -*- lexical-binding: t; -*-

(require 'org)
(require 'org-id)

(defgroup org-tasktree nil
  "Task management via org-mode UI backed by SQLite."
  :group 'org)

(defcustom org-tasktree-database-location
  (expand-file-name "org-tasktree/tasktree.db" user-emacs-directory)
  "SQLite database file path for org-tasktree."
  :type 'string
  :group 'org-tasktree)

(defcustom org-tasktree-query-dir
  (expand-file-name "org-tasktree/queries/" user-emacs-directory)
  "Directory path for saved query YAML files."
  :type 'string
  :group 'org-tasktree)

(defcustom org-tasktree-default-project
  "inbox"
  "Default project name for tasks without explicit project/phase."
  :type 'string
  :group 'org-tasktree)

(require 'org-tasktree-db)

(defun org-tasktree-init ()
  "Initialize org-tasktree database and query directory." 
  (interactive)
  (make-directory (expand-file-name org-tasktree-query-dir) t)
  (org-tasktree-db-init)
  (message "org-tasktree: initialized"))

(defun org-tasktree-find-project ()
  "Find or create a project, then open its edit buffer."
  (interactive)
  (user-error "Not implemented yet"))

(defun org-tasktree-find-phase ()
  "Find or create a phase, then open its edit buffer."
  (interactive)
  (user-error "Not implemented yet"))

(defun org-tasktree-find-task ()
  "Find or create a task, then open its edit buffer."
  (interactive)
  (user-error "Not implemented yet"))

(defun org-tasktree-search-today-task ()
  "Search tasks scheduled for today and display as an org tree."
  (interactive)
  (user-error "Not implemented yet"))

(defun org-tasktree-search-before-today-task ()
  "Search tasks scheduled on/before today and display as an org tree."
  (interactive)
  (user-error "Not implemented yet"))

(defun org-tasktree-search-overdue-task ()
  "Search tasks with deadline before today and display as an org tree."
  (interactive)
  (user-error "Not implemented yet"))

(defun org-tasktree-search-next-7day-task ()
  "Search tasks scheduled between tomorrow and the next 7 days."
  (interactive)
  (user-error "Not implemented yet"))

(provide 'org-tasktree)
;;; org-tasktree.el ends here