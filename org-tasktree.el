;;; org-tasktree.el --- Task management via org-mode + SQLite -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; Task management via `org-mode' UI backed by SQLite as the single
;; source of truth.  This file provides user-facing entry points and
;; customization variables.  Implementation details are split into
;; dedicated modules.
;;

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-tasktree-model)
(require 'org-tasktree-query)
(require 'org-tasktree-view)
(require 'org-tasktree-ui)

(defgroup org-tasktree nil
  "Task management via `org-mode' UI backed by SQLite."
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
  "Default project name for tasks without explicit project or phase."
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
  (let* ((sel (org-tasktree-ui-read-project))
         (title (plist-get sel :project-title))
         (pid (plist-get sel :project-id)))
    (message "org-tasktree: project %s (id=%s)" title pid)))

(defun org-tasktree-find-phase ()
  "Find or create a phase, then open its edit buffer."
  (interactive)
  (let* ((sel (org-tasktree-ui-read-phase))
         (proj (plist-get sel :project-title))
         (proj-id (plist-get sel :project-id))
         (phase (plist-get sel :phase-title))
         (phase-id (plist-get sel :phase-id)))
    (message "org-tasktree: phase %s / %s (ids %s / %s)"
             proj phase proj-id phase-id)))

(defun org-tasktree-find-task ()
  "Find or create a task, then open its edit buffer."
  (interactive)
  (let* ((sel (org-tasktree-ui-read-task))
         (proj (plist-get sel :project-title))
         (phase (plist-get sel :phase-title))
         (task (plist-get sel :task-title))
         (task-id (plist-get sel :task-id)))
    (message "org-tasktree: task %s / %s / %s (task-id %s)"
             proj phase task task-id)))

(defun org-tasktree-search-today-task ()
  "Search tasks scheduled for today and display as an org tree."
  (interactive)
  (org-tasktree-view-display-tree
   (org-tasktree-query-search-today)
   "Today"))

(defun org-tasktree-search-before-today-task ()
  "Search tasks scheduled on or before today."
  (interactive)
  (org-tasktree-view-display-tree
   (org-tasktree-query-search-before-today)
   "Before today"))

(defun org-tasktree-search-overdue-task ()
  "Search tasks with a deadline before today."
  (interactive)
  (org-tasktree-view-display-tree
   (org-tasktree-query-search-overdue)
   "Overdue"))

(defun org-tasktree-search-next-7day-task ()
  "Search tasks scheduled between tomorrow and the next seven days."
  (interactive)
  (org-tasktree-view-display-tree
   (org-tasktree-query-search-next-7day)
   "Next 7 days"))

(provide 'org-tasktree)
;;; org-tasktree.el ends here
