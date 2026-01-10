;;; org-tasktree-ui-edit.el --- Edit flow for org-tasktree -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1"))
;; Package: org-tasktree

;;; Commentary:
;;
;; Edit flow helpers for org-tasktree nodes, including validation and DB commit.
;; Provides metadata setup and edit buffer submission logic.
;;
;;; Code:

(require 'org-tasktree-model)
(require 'org-tasktree-query)
(require 'org-tasktree-db)
(require 'org-tasktree-ui-widget)

(declare-function org-tasktree-ui-read-node "org-tasktree-ui")

(defun org-tasktree-ui-edit--node-title-by-id (id)
  "Return node title for numeric ID or nil."
  (let ((node (org-tasktree-query-get-node-by-id id)))
    (and node (org-tasktree-model-node-title node))))

(defun org-tasktree-ui-edit--node-by-uid (uid)
  "Return node by UID or nil."
  (when uid
    (org-tasktree-db--with-db db
      (let ((row (car (sqlite-select
                       db
                       (string-join
                        '("SELECT id, uid, parent_id, todo_keyword, title,"
                          " priority, scheduled, deadline, repeat, closed_at,"
                          " tags, content, status, created_at, updated_at"
                          " FROM nodes WHERE uid = ? LIMIT 1;")
                        "")
                       (vector uid)))))
        (when row
          (org-tasktree-model-node-from-db-row row))))))

(defun org-tasktree-ui-edit--node-edit-meta-existing (node path-titles)
  "Return widget META for existing NODE using PATH-TITLES."
  (let ((parent-path (and (listp path-titles) (butlast path-titles))))
    (list :type 'node
          :uid (org-tasktree-model-node-uid node)
          :title (org-tasktree-model-node-title node)
          :priority (org-tasktree-model-node-priority node)
          :scheduled (org-tasktree-model-node-scheduled node)
          :deadline (org-tasktree-model-node-deadline node)
          :repeat (org-tasktree-model-node-repeat node)
          :tags (org-tasktree-model-node-tags node)
          :content (org-tasktree-model-node-content node)
          :path-titles parent-path
          :parent-id (org-tasktree-model-node-parent-id node))))

(defun org-tasktree-ui-edit--node-edit-meta-new (title parent-node parent-path-titles)
  "Return widget META for new node with TITLE, PARENT-NODE, and PARENT-PATH-TITLES."
  (list :type 'node
        :uid nil
        :title title
        :priority nil
        :scheduled nil
        :deadline nil
        :repeat nil
        :tags nil
        :content nil
        :path-titles parent-path-titles
        :parent-id (and parent-node (org-tasktree-model-node-id parent-node))))

(defun org-tasktree-ui-edit-node (selection)
  "Open node edit buffer using SELECTION plist."
  (let* ((existing (plist-get selection :existing))
         (node (plist-get selection :node))
         (path-titles (plist-get selection :path-titles))
         (title (plist-get selection :title))
         (parent-node (plist-get selection :parent-node))
         (parent-path-titles (plist-get selection :parent-path-titles))
         (meta
          (if existing
              (org-tasktree-ui-edit--node-edit-meta-existing node path-titles)
            (org-tasktree-ui-edit--node-edit-meta-new title parent-node parent-path-titles))))
    (setq meta (plist-put meta :return-to 'find-node))
    (setq meta (plist-put meta :show-repeat t))
    (org-tasktree-ui-widget--open-buffer meta)))

(defun org-tasktree-ui-edit--quit-buffer ()
  "Close current edit buffer and its window."
  (let* ((buf (current-buffer))
         (win (get-buffer-window buf t)))
    (when (and win (eq (window-buffer win) buf))
      (quit-window 'kill win))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun org-tasktree-ui-edit-cancel ()
  "Cancel current org-tasktree widget edit buffer."
  (interactive)
  (let ((meta (org-tasktree-ui-widget--current-meta)))
    (org-tasktree-ui-edit--quit-buffer)
    (message "org-tasktree: edit cancelled")
    (when (eq (plist-get meta :return-to) 'find-node)
      (run-at-time
       0
       nil
       (lambda ()
         (let ((sel (org-tasktree-ui-read-node)))
           (org-tasktree-ui-edit-node sel)))))))

(defun org-tasktree-ui-edit--normalize-content (value)
  "Return VALUE or nil when empty/whitespace."
  (let ((text (or value "")))
    (if (string-match-p "\\S-" text)
        text
      nil)))

(defun org-tasktree-ui-edit--submit-widget (meta)
  "Submit widget edit META to DB and return saved node."
  (let* ((uid (or (plist-get meta :uid) (org-tasktree-db-generate-uid)))
         (existing (and (plist-get meta :uid)
                        (org-tasktree-ui-edit--node-by-uid uid)))
         (parent-id (or (plist-get meta :parent-id)
                        (and existing
                             (org-tasktree-model-node-parent-id existing))))
         (title (org-tasktree-model-validate-title
                 (org-tasktree-ui-widget--value :title)))
         (priority (org-tasktree-model-validate-priority
                    (org-tasktree-ui-widget--value :priority)))
         (scheduled (org-tasktree-ui-widget--parse-date-input
                     (org-tasktree-ui-widget--value :scheduled)
                     "scheduled"))
         (deadline (org-tasktree-ui-widget--parse-date-input
                    (org-tasktree-ui-widget--value :deadline)
                    "deadline"))
         (repeat (org-tasktree-model-validate-repeat
                  (org-tasktree-ui-widget--value :repeat)))
         (content-raw (org-tasktree-ui-widget--value-raw :content))
         (content (org-tasktree-ui-edit--normalize-content
                   (and (stringp content-raw) content-raw)))
         (tags (org-tasktree-model-validate-tags
                (org-tasktree-ui-widget--value :tags)))
         (todo-keyword (and existing
                            (org-tasktree-model-node-todo-keyword existing)))
         (status (or (and existing (org-tasktree-model-node-status existing))
                     "OPEN")))
    (org-tasktree-model-validate-schedule-deadline scheduled deadline)
    (when (and parent-id (not (org-tasktree-query-get-node-by-id parent-id)))
      (user-error "Parent must exist: %s" parent-id))
    (let ((node (org-tasktree-model-node-create
                 :uid uid
                 :parent-id parent-id
                 :todo-keyword todo-keyword
                 :title title
                 :priority priority
                 :scheduled scheduled
                 :deadline deadline
                 :repeat repeat
                 :content content
                 :tags tags
                 :status status)))
      (org-tasktree-db-commit-nodes (list node))
      node)))

(defun org-tasktree-ui-edit-accept ()
  "Commit current widget edit buffer to DB."
  (interactive)
  (let* ((meta (org-tasktree-ui-widget--current-meta))
         (node (org-tasktree-ui-edit--submit-widget meta)))
    (org-tasktree-ui-edit--quit-buffer)
    (message "org-tasktree: saved uid=%s" (org-tasktree-model-node-uid node))))

(provide 'org-tasktree-ui-edit)
;;; org-tasktree-ui-edit.el ends here
