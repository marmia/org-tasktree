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
(require 'org-tasktree-sync)

(declare-function org-tasktree-ui-edit-node "org-tasktree-ui")

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

(defvaralias 'org-tasktree-default-project
  'org-tasktree-default-project-name)
(make-obsolete-variable 'org-tasktree-default-project
                        'org-tasktree-default-project-name
                        "0.1.0")

(defcustom org-tasktree-default-project-name
  "inbox"
  "Default project name for tasks without explicit project or phase.
This value is used as the inbox title only at initialization time."
  :type 'string
  :group 'org-tasktree)

(require 'org-tasktree-db)
(require 'org-tasktree-query-edit)

(defun org-tasktree-init ()
  "Initialize org-tasktree database and query directory."
  (interactive)
  (make-directory (expand-file-name org-tasktree-query-dir) t)
  (org-tasktree-db-init)
  (message "org-tasktree: initialized"))

(defun org-tasktree-find-node ()
  "Find or create a node, then open its edit buffer."
  (interactive)
  (let* ((sel (org-tasktree-ui-read-node)))
    (org-tasktree-ui-edit-node sel)))

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

(defun org-tasktree-search-unscheduled-task ()
  "Search tasks with no scheduled date."
  (interactive)
  (org-tasktree-view-display-tree
   (org-tasktree-query-search-unscheduled)
   "Unscheduled"))

(defun org-tasktree-search-all ()
  "Search all nodes including DONE."
  (interactive)
  (org-tasktree-view-display-tree
   (org-tasktree-query-search-all)
   "All"))

(defun org-tasktree-search-by-query ()
  "Search task tree by YAML query."
  (interactive)
  (org-tasktree-query-edit--ensure-query-dir)
  (let* ((file (org-tasktree-query-edit--select-file)))
    (when (stringp file)
      (if (called-interactively-p 'interactive)
          (org-tasktree-query-edit--open-buffer file)
        (let* ((abs (expand-file-name file org-tasktree-query-dir))
               (text (org-tasktree-query-edit--read-file abs))
               (title (org-tasktree-query-edit--query-title abs)))
          (org-tasktree-query-edit--execute text title))))))

(provide 'org-tasktree)
;;; org-tasktree.el ends here
