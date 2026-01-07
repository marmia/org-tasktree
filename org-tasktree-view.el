;;; org-tasktree-view.el --- Org buffer rendering for org-tasktree -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; Rendering helpers that turn DB nodes into a read-only `org-mode'
;; buffer for search results.
;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'org-tasktree-model)

(defconst org-tasktree-view--buffer-prefix
  "*org-tasktree "
  "Prefix for org-tasktree view buffer names.")

(defvar org-tasktree-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-tasktree-view-mode'.")

(defun org-tasktree-view--setup-evil ()
  "Ensure `q' quits in `org-tasktree-view-mode' under Evil."
  (when (featurep 'evil)
    ;; Allow editing by using normal state instead of motion.
    (when (fboundp 'evil-set-initial-state)
      (evil-set-initial-state 'org-tasktree-view-mode 'normal))
    ;; Define q key in normal/motion states for this mode
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal org-tasktree-view-mode-map
                        (kbd "q") #'quit-window)
      (evil-define-key* 'motion org-tasktree-view-mode-map
                        (kbd "q") #'quit-window))
    (when (fboundp 'evil-normalize-keymaps)
      (evil-normalize-keymaps))))

(add-hook 'org-tasktree-view-mode-hook
          #'org-tasktree-view--setup-evil)

(when (boundp 'evil-mode-hook)
  (add-hook 'evil-mode-hook #'org-tasktree-view--setup-evil))

;; Initial setup at load time
(org-tasktree-view--setup-evil)

(define-derived-mode org-tasktree-view-mode
  org-mode "org-tasktree-view"
  "Writable mode for org-tasktree search results."
  (use-local-map org-tasktree-view-mode-map)
  (setq buffer-read-only nil)
  (setq truncate-lines nil))

(defun org-tasktree-view--heading-line (node &optional level)
  "Return org heading line string for NODE.
LEVEL overrides computed level when non-nil."
  (let* ((level (max 1 (or level 1)))
         (stars (make-string level ?*))
         (todo (or (org-tasktree-model-node-todo-keyword node) ""))
         (todo-part (if (string-empty-p todo)
                        ""
                      (concat todo " ")))
         (priority (org-tasktree-model-node-priority node))
         (priority-part (if priority
                            (format "[#%s] "
                                    (string-remove-prefix
                                     "#" priority))
                          ""))
         (title (org-tasktree-model-node-title node))
         (tags (org-tasktree-model-node-tags node))
         (tags-part (let ((org-tags (org-tasktree-model-tags->org-string tags)))
                      (if org-tags (concat " " org-tags) ""))))
    (format "%s %s%s%s%s"
            stars todo-part priority-part title tags-part)))

(defun org-tasktree-view--scheduled-line (node)
  "Return scheduled/deadline line for NODE or nil when empty."
  (let ((scheduled (org-tasktree-model-node-scheduled node))
        (deadline (org-tasktree-model-node-deadline node))
        (repeat (org-tasktree-model-node-repeat node)))
    (when (or scheduled deadline)
      (concat
       (when scheduled
         (format "SCHEDULED: <%s%s> "
                 scheduled
                 (if (and repeat (not (string-empty-p repeat)))
                     (concat " " repeat)
                   "")))
       (when deadline
         (format "DEADLINE: <%s>" deadline))))))

(defun org-tasktree-view--properties (node)
  "Return property drawer string for NODE or nil."
  (let ((uid (org-tasktree-model-node-uid node)))
    (when uid
      (format ":PROPERTIES:\n:UID: %s\n:END:" uid))))

(defun org-tasktree-view--insert-node (node &optional level)
  "Insert NODE as org-formatted text at point.
LEVEL overrides computed level for display."
  (insert (org-tasktree-view--heading-line node level) "\n")
  (let ((scheduled-line (org-tasktree-view--scheduled-line node)))
    (when scheduled-line
      (insert scheduled-line "\n")))
  (let ((props (org-tasktree-view--properties node)))
    (when props
      (insert props "\n")))
  (let ((content (org-tasktree-model-node-content node)))
    (when (and (stringp content)
               (string-match-p "\\S-" content))
      (insert content)
      (unless (string-suffix-p "\n" content)
        (insert "\n")))))

(defun org-tasktree-view--level-map (nodes)
  "Return hash table mapping node id to computed org level for NODES."
  (let ((node-by-id (make-hash-table :test 'equal))
        (level-by-id (make-hash-table :test 'equal))
        (visiting (make-hash-table :test 'equal)))
    (dolist (node nodes)
      (puthash (org-tasktree-model-node-id node) node node-by-id))
    (cl-labels
        ((compute-level (node)
           (let* ((id (org-tasktree-model-node-id node))
                  (cached (gethash id level-by-id)))
             (if cached
                 cached
               (when (gethash id visiting)
                 (puthash id 1 level-by-id))
               (puthash id t visiting)
               (let* ((parent-id (org-tasktree-model-node-parent-id node))
                      (parent (and parent-id
                                   (gethash parent-id node-by-id)))
                      (level (if parent
                                 (1+ (compute-level parent))
                               1)))
                 (puthash id level level-by-id)
                 (remhash id visiting)
                 level)))))
      (dolist (node nodes)
        (compute-level node)))
    level-by-id))

(defun org-tasktree-view-display-tree (nodes title)
  "Display NODES as an org tree in a writable buffer titled TITLE."
  (let* ((buffer-name (format "%s%s*"
                              org-tasktree-view--buffer-prefix
                              title))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (org-tasktree-view-mode)
        (erase-buffer)
        (let ((level-map (org-tasktree-view--level-map nodes)))
          (dolist (node nodes)
            (org-tasktree-view--insert-node
             node
             (gethash (org-tasktree-model-node-id node) level-map))))
        (goto-char (point-min))
        (setq buffer-read-only nil)
        (setq-local view-read-only nil)
        (when (bound-and-true-p view-mode)
          (view-mode -1))
        (remove-text-properties (point-min) (point-max) '(read-only nil))))
    (pop-to-buffer buffer)
    (delete-other-windows)))

(provide 'org-tasktree-view)
;;; org-tasktree-view.el ends here
