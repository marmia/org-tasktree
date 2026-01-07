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
(require 'seq)
(require 'subr-x)
(require 'org-tasktree-model)
(require 'org-tasktree-query)
(require 'org-tasktree-view)
(require 'org-tasktree-ui)
(require 'org-tasktree-ui-minibuffer)
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

(defvar-local org-tasktree-search-by-query--file nil
  "Query file path for the current query edit buffer.")

(defvar-local org-tasktree-search-by-query--title nil
  "Query title for the current query edit buffer.")

(defvar org-tasktree-search-by-query-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-tasktree-search-by-query--accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-search-by-query--cancel)
    map)
  "Keymap for `org-tasktree-search-by-query-edit-mode'.")

(define-minor-mode org-tasktree-search-by-query-edit-mode
  "Minor mode for org-tasktree query edit buffers."
  :lighter " org-tasktree-query"
  :keymap org-tasktree-search-by-query-edit-mode-map)

(defun org-tasktree-search-by-query--query-files ()
  "Return sorted list of query file names under `org-tasktree-query-dir'."
  (let ((dir (expand-file-name org-tasktree-query-dir)))
    (when (file-directory-p dir)
      (sort
       (seq-filter
        (lambda (name)
          (string-match-p "\\.ya?ml\\'" name))
        (directory-files dir nil "^[^.].*"))
       #'string<))))

(defun org-tasktree-search-by-query--normalize-file-name (name)
  "Return normalized query file NAME with extension."
  (let ((trimmed (string-trim name)))
    (when (string-empty-p trimmed)
      (user-error "Query file name is empty"))
    (cond
     ((string-match-p "\\.ya?ml\\'" trimmed) trimmed)
     (t (concat trimmed ".yml")))))

(defun org-tasktree-search-by-query--select-file ()
  "Prompt for a query file name and return it."
  (let* ((cands (or (org-tasktree-search-by-query--query-files) '()))
         (choice (org-tasktree-ui-minibuffer--completing-read
                  "Query file: " cands nil nil)))
    (when (and (stringp choice) (not (string-empty-p choice)))
      (org-tasktree-search-by-query--normalize-file-name choice))))

(defun org-tasktree-search-by-query--ensure-query-dir ()
  "Ensure `org-tasktree-query-dir' exists."
  (let ((dir (expand-file-name org-tasktree-query-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun org-tasktree-search-by-query--query-title (file)
  "Return buffer title for query FILE."
  (file-name-base file))

(defun org-tasktree-search-by-query--read-file (file)
  "Return content string for FILE."
  (unless (file-exists-p file)
    (user-error "Query file not found: %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-tasktree-search-by-query--execute (text title)
  "Execute query TEXT and show results using TITLE."
  (let* ((nodes (org-tasktree-query-search-by-query text))
         (display-title (if (and (stringp title)
                                 (not (string-empty-p title)))
                            title
                          "By query")))
    (if nodes
        (org-tasktree-view-display-tree nodes display-title)
      (message "org-tasktree: no results"))))

(defun org-tasktree-search-by-query--open-buffer (file)
  "Open query edit buffer for FILE."
  (org-tasktree-search-by-query--ensure-query-dir)
  (let* ((abs (expand-file-name file org-tasktree-query-dir))
         (buf (find-file-noselect abs)))
    (with-current-buffer buf
      (setq-local org-tasktree-search-by-query--file abs)
      (setq-local org-tasktree-search-by-query--title
                  (org-tasktree-search-by-query--query-title abs))
      (when (fboundp 'yaml-mode)
        (yaml-mode))
      (org-tasktree-search-by-query-edit-mode 1)
      (when (and (not (file-exists-p abs))
                 (= (buffer-size) 0))
        (insert (org-tasktree-query-default-template))
        (set-buffer-modified-p t))
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (delete-other-windows)))

(defun org-tasktree-search-by-query--accept ()
  "Save query buffer, execute search, and close the buffer."
  (interactive)
  (let* ((buf (current-buffer))
         (file org-tasktree-search-by-query--file)
         (title org-tasktree-search-by-query--title)
         (text (buffer-substring-no-properties (point-min) (point-max))))
    (when (and file (buffer-modified-p))
      (write-region (point-min) (point-max) file nil 'quiet))
    (org-tasktree-search-by-query--execute text title)
    (org-tasktree-search-by-query--close-buffer buf)))

(defun org-tasktree-search-by-query--cancel ()
  "Cancel query editing and close the buffer."
  (interactive)
  (org-tasktree-search-by-query--close-buffer (current-buffer)))

(defun org-tasktree-search-by-query--close-buffer (buffer)
  "Close query edit BUFFER."
  (when (buffer-live-p buffer)
    (let ((win (get-buffer-window buffer t)))
      (when (window-live-p win)
        (quit-window 'kill win)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun org-tasktree-search-by-query ()
  "Search task tree by YAML query."
  (interactive)
  (org-tasktree-search-by-query--ensure-query-dir)
  (let* ((file (org-tasktree-search-by-query--select-file)))
    (when (stringp file)
      (if (called-interactively-p 'interactive)
          (org-tasktree-search-by-query--open-buffer file)
        (let* ((abs (expand-file-name file org-tasktree-query-dir))
               (text (org-tasktree-search-by-query--read-file abs))
               (title (org-tasktree-search-by-query--query-title abs)))
          (org-tasktree-search-by-query--execute text title))))))


(provide 'org-tasktree)
;;; org-tasktree.el ends here
