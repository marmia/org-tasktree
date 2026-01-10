;;; org-tasktree-query-edit.el --- Query editing for org-tasktree -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1"))
;; Package: org-tasktree

;;; Commentary:
;;
;; Editing helpers for saved YAML query files used by org-tasktree search.
;; Provides a small edit buffer workflow and execution entry point.
;;
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'org-tasktree-query)
(require 'org-tasktree-ui-minibuffer)
(require 'org-tasktree-view)

(defun org-tasktree-query-edit--query-dir ()
  "Return query directory path from `org-tasktree-query-dir'."
  (if (and (boundp 'org-tasktree-query-dir)
           (stringp (symbol-value 'org-tasktree-query-dir)))
      (symbol-value 'org-tasktree-query-dir)
    (user-error "Org-tasktree-query-dir is not set")))

(defvar-local org-tasktree-query-edit--file nil
  "Query file path for the current query edit buffer.")

(defvar-local org-tasktree-query-edit--title nil
  "Query title for the current query edit buffer.")

(defvar org-tasktree-query-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-tasktree-query-edit--accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-query-edit--cancel)
    map)
  "Keymap for `org-tasktree-query-edit-mode'.")

(define-minor-mode org-tasktree-query-edit-mode
  "Minor mode for org-tasktree query edit buffers."
  :lighter " org-tasktree-query"
  :keymap org-tasktree-query-edit-mode-map)

(defun org-tasktree-query-edit--query-files ()
  "Return sorted list of query file names under `org-tasktree-query-dir'."
  (let ((dir (expand-file-name (org-tasktree-query-edit--query-dir))))
    (when (file-directory-p dir)
      (sort
       (seq-filter
        (lambda (name)
          (string-match-p "\\.ya?ml\\'" name))
        (directory-files dir nil "^[^.].*"))
       #'string<))))

(defun org-tasktree-query-edit--normalize-file-name (name)
  "Return normalized query file NAME with extension."
  (let ((trimmed (string-trim name)))
    (when (string-empty-p trimmed)
      (user-error "Query file name is empty"))
    (cond
     ((string-match-p "\\.ya?ml\\'" trimmed) trimmed)
     (t (concat trimmed ".yml")))))

(defun org-tasktree-query-edit--select-file ()
  "Prompt for a query file name and return it."
  (let* ((cands (or (org-tasktree-query-edit--query-files) '()))
         (choice (org-tasktree-ui-minibuffer--completing-read
                  "Query file: " cands nil nil)))
    (when (and (stringp choice) (not (string-empty-p choice)))
      (org-tasktree-query-edit--normalize-file-name choice))))

(defun org-tasktree-query-edit--ensure-query-dir ()
  "Ensure `org-tasktree-query-dir' exists."
  (let ((dir (expand-file-name (org-tasktree-query-edit--query-dir))))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun org-tasktree-query-edit--query-title (file)
  "Return buffer title for query FILE."
  (file-name-base file))

(defun org-tasktree-query-edit--read-file (file)
  "Return content string for FILE."
  (unless (file-exists-p file)
    (user-error "Query file not found: %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-tasktree-query-edit--execute (text title)
  "Execute query TEXT and show results using TITLE."
  (let* ((nodes (org-tasktree-query-search-by-query text))
         (display-title (if (and (stringp title)
                                 (not (string-empty-p title)))
                            title
                          "By query")))
    (if nodes
        (org-tasktree-view-display-tree nodes display-title)
      (message "org-tasktree: no results"))))

(defun org-tasktree-query-edit--open-buffer (file)
  "Open query edit buffer for FILE."
  (org-tasktree-query-edit--ensure-query-dir)
  (let* ((abs (expand-file-name file (org-tasktree-query-edit--query-dir)))
         (buf (find-file-noselect abs)))
    (with-current-buffer buf
      (setq-local org-tasktree-query-edit--file abs)
      (setq-local org-tasktree-query-edit--title
                  (org-tasktree-query-edit--query-title abs))
      (when (fboundp 'yaml-mode)
        (yaml-mode))
      (org-tasktree-query-edit-mode 1)
      (when (and (not (file-exists-p abs))
                 (= (buffer-size) 0))
        (insert (org-tasktree-query-default-template))
        (set-buffer-modified-p t))
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (delete-other-windows)))

(defun org-tasktree-query-edit--accept ()
  "Save query buffer, execute search, and close the buffer."
  (interactive)
  (let* ((buf (current-buffer))
         (file org-tasktree-query-edit--file)
         (title org-tasktree-query-edit--title)
         (text (buffer-substring-no-properties (point-min) (point-max))))
    (when (and file (buffer-modified-p))
      (write-region (point-min) (point-max) file nil 'quiet))
    (org-tasktree-query-edit--execute text title)
    (org-tasktree-query-edit--close-buffer buf)))

(defun org-tasktree-query-edit--cancel ()
  "Cancel query editing and close the buffer."
  (interactive)
  (org-tasktree-query-edit--close-buffer (current-buffer)))

(defun org-tasktree-query-edit--close-buffer (buffer)
  "Close query edit BUFFER."
  (when (buffer-live-p buffer)
    (let ((win (get-buffer-window buffer t)))
      (when (window-live-p win)
        (quit-window 'kill win)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(provide 'org-tasktree-query-edit)
;;; org-tasktree-query-edit.el ends here
