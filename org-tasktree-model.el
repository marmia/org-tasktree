;;; org-tasktree-model.el --- Data model for org-tasktree -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; Data model structures, conversions, and validations for
;; org-tasktree nodes and tags.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)

(defconst org-tasktree-model--allowed-statuses
  '("OPEN" "DONE")
  "Allowed `status' values.")

(cl-defstruct org-tasktree-model-node
  id uid parent-id todo-keyword title priority
  scheduled deadline repeat closed-at tags content status
  created-at updated-at)

(cl-defun org-tasktree-model-node-create
    (&key id uid parent-id todo-keyword title priority
          scheduled deadline repeat closed-at tags content status
          created-at updated-at)
  "Create node from keyword arguments.
Accepts ID, UID, PARENT-ID, TODO-KEYWORD, TITLE, PRIORITY, SCHEDULED,
DEADLINE, REPEAT, CLOSED-AT, TAGS, STATUS, CONTENT, CREATED-AT, and
UPDATED-AT."
  (make-org-tasktree-model-node
   :id id
   :uid uid
   :parent-id parent-id
   :todo-keyword todo-keyword
   :title title
   :priority priority
   :scheduled scheduled
   :deadline deadline
   :repeat repeat
   :closed-at closed-at
   :tags tags
   :content content
   :status status
   :created-at created-at
   :updated-at updated-at))

(defun org-tasktree-model--row-nth (row index)
  "Return ROW element at INDEX supporting vectors or lists."
  (if (vectorp row) (aref row index) (nth index row)))

(defun org-tasktree-model-node-from-db-row (row)
  "Create `org-tasktree-model-node' from DB ROW.
ROW must follow table column order: id, uid, parent_id, todo_keyword,
title, priority, scheduled, deadline, repeat, closed_at, tags,
content, status, created_at, and updated_at."
  (org-tasktree-model-node-create
   :id (org-tasktree-model--row-nth row 0)
   :uid (org-tasktree-model--row-nth row 1)
   :parent-id (org-tasktree-model--row-nth row 2)
   :todo-keyword (org-tasktree-model--row-nth row 3)
   :title (org-tasktree-model--row-nth row 4)
   :priority (org-tasktree-model--row-nth row 5)
   :scheduled (org-tasktree-model--row-nth row 6)
   :deadline (org-tasktree-model--row-nth row 7)
   :repeat (org-tasktree-model--row-nth row 8)
   :closed-at (org-tasktree-model--row-nth row 9)
   :tags (org-tasktree-model--row-nth row 10)
   :content (org-tasktree-model--row-nth row 11)
   :status (org-tasktree-model--row-nth row 12)
   :created-at (org-tasktree-model--row-nth row 13)
   :updated-at (org-tasktree-model--row-nth row 14)))

(defun org-tasktree-model-node-from-plist (plist)
  "Create `org-tasktree-model-node' from PLIST with keyword keys."
  (org-tasktree-model-node-create
   :id (plist-get plist :id)
   :uid (plist-get plist :uid)
   :parent-id (plist-get plist :parent-id)
   :todo-keyword (plist-get plist :todo-keyword)
   :title (plist-get plist :title)
   :priority (plist-get plist :priority)
   :scheduled (plist-get plist :scheduled)
   :deadline (plist-get plist :deadline)
   :repeat (plist-get plist :repeat)
   :closed-at (plist-get plist :closed-at)
   :tags (plist-get plist :tags)
   :content (plist-get plist :content)
   :status (plist-get plist :status)
   :created-at (plist-get plist :created-at)
   :updated-at (plist-get plist :updated-at)))

(defun org-tasktree-model-node-to-plist (node)
  "Return PLIST representation of NODE."
  (list :id (org-tasktree-model-node-id node)
        :uid (org-tasktree-model-node-uid node)
        :parent-id (org-tasktree-model-node-parent-id node)
        :todo-keyword (org-tasktree-model-node-todo-keyword node)
        :title (org-tasktree-model-node-title node)
        :priority (org-tasktree-model-node-priority node)
        :scheduled (org-tasktree-model-node-scheduled node)
        :deadline (org-tasktree-model-node-deadline node)
        :repeat (org-tasktree-model-node-repeat node)
        :closed-at (org-tasktree-model-node-closed-at node)
        :tags (org-tasktree-model-node-tags node)
        :content (org-tasktree-model-node-content node)
        :status (org-tasktree-model-node-status node)
        :created-at (org-tasktree-model-node-created-at node)
        :updated-at (org-tasktree-model-node-updated-at node)))

(defun org-tasktree-model-node->db-vector (node &optional include-id)
  "Return NODE fields as vector in DB column order.
When INCLUDE-ID is non-nil, the first element is `id'; otherwise it is
omitted, starting from `uid'."
  (let ((vals (list (org-tasktree-model-node-id node)
                    (org-tasktree-model-node-uid node)
                    (org-tasktree-model-node-parent-id node)
                    (org-tasktree-model-node-todo-keyword node)
                    (org-tasktree-model-node-title node)
                    (org-tasktree-model-node-priority node)
                    (org-tasktree-model-node-scheduled node)
                    (org-tasktree-model-node-deadline node)
                    (org-tasktree-model-node-repeat node)
                    (org-tasktree-model-node-closed-at node)
                    (org-tasktree-model-node-tags node)
                    (org-tasktree-model-node-content node)
                    (org-tasktree-model-node-status node)
                    (org-tasktree-model-node-created-at node)
                    (org-tasktree-model-node-updated-at node))))
    (apply #'vector (if include-id vals (cdr vals)))))

(defun org-tasktree-model--string-nonempty-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun org-tasktree-model--valid-uid-p (uid)
  "Return non-nil when UID matches UUID format."
  (and (stringp uid)
       (string-match-p
        "\\`[0-9a-fA-F]\\{8\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-\
[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{12\\}\\'"
        uid)))

(defun org-tasktree-model--valid-date-p (value)
  "Return non-nil when VALUE matches YYYY-MM-DD format."
  (and (stringp value)
       (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" value)))

(defun org-tasktree-model--valid-repeat-p (value)
  "Return non-nil when VALUE matches org repeat syntax."
  (and (stringp value)
       (string-match-p
        "\\`\\(?:\\+\\|\\+\\+\\|\\.\\+\\)[0-9]+[dwmy]\\(?:/[0-9]+\\)?\\'"
        value)))

(defun org-tasktree-model--title-valid-p (title)
  "Return non-nil when TITLE does not include control characters."
  (and (stringp title)
       (not (string-match-p "[[:cntrl:]]" title))))

(defun org-tasktree-model--normalize-priority (priority)
  "Return normalized PRIORITY string without leading '#'."
  (when (org-tasktree-model--string-nonempty-p priority)
    (let ((p (string-remove-prefix "#" priority)))
      (if (= (length p) 1) p nil))))

(defun org-tasktree-model--priority-valid-p (priority)
  "Return non-nil if PRIORITY is a single alphanumeric character."
  (let ((p (org-tasktree-model--normalize-priority priority)))
    (and p (string-match-p "\\`[[:alnum:]]\\'" p))))

(defun org-tasktree-model--dedupe-preserve-order (strings)
  "Return STRINGS without duplicates, preserving order."
  (let ((seen (make-hash-table :test 'equal))
        result)
    (dolist (s strings (nreverse result))
      (unless (gethash s seen)
        (puthash s t seen)
        (push s result)))))

(defun org-tasktree-model-normalize-tags (tags)
  "Normalize TAGS string or list.
Return (STRING . LIST) where STRING is org tag suffix `:tag1:tag2:'
or nil when empty, and LIST is de-duplicated tag strings."
  (cond
   ((null tags) (cons nil nil))
   ((stringp tags)
    (org-tasktree-model-normalize-tags
     (seq-filter #'org-tasktree-model--string-nonempty-p
                 (mapcar #'string-trim
                         (split-string tags ":" t)))))
   ((listp tags)
    (let* ((trimmed (mapcar #'string-trim tags))
           (filtered (seq-filter
                      #'org-tasktree-model--string-nonempty-p
                      trimmed))
           (unique (org-tasktree-model--dedupe-preserve-order
                    filtered)))
      (cons (when unique
              (concat ":" (string-join unique ":") ":"))
            unique)))
   (t (user-error "Invalid tags value: %S" tags))))

(defun org-tasktree-model-tags->org-string (tags)
  "Return TAGS as org tag suffix `:tag1:tag2:' or nil."
  (car (org-tasktree-model-normalize-tags tags)))

(defun org-tasktree-model-node-tags-list (node)
  "Return normalized tag list for NODE."
  (cdr (org-tasktree-model-normalize-tags
        (org-tasktree-model-node-tags node))))

(defun org-tasktree-model-validate-node (node)
  "Validate NODE and signal `user-error' when invalid.
Returns NODE when validation succeeds."
  (unless (org-tasktree-model-node-p node)
    (user-error "NODE is not an `org-tasktree-model-node'"))
  (let ((uid (org-tasktree-model-node-uid node))
        (title (org-tasktree-model-node-title node))
        (status (org-tasktree-model-node-status node))
        (priority (org-tasktree-model-node-priority node))
        (scheduled (org-tasktree-model-node-scheduled node))
        (deadline (org-tasktree-model-node-deadline node))
        (repeat (org-tasktree-model-node-repeat node))
        (tags (org-tasktree-model-node-tags node))
        (created-at (org-tasktree-model-node-created-at node))
        (updated-at (org-tasktree-model-node-updated-at node)))
    (unless (org-tasktree-model--string-nonempty-p uid)
      (user-error "UID is required"))
    (unless (org-tasktree-model--valid-uid-p uid)
      (user-error "UID must be UUID format"))
    (unless (org-tasktree-model--string-nonempty-p title)
      (user-error "Title is required"))
    (unless (org-tasktree-model--title-valid-p title)
      (user-error "Title must not include control characters"))
    (unless (member status org-tasktree-model--allowed-statuses)
      (user-error "Status must be one of %S"
                  org-tasktree-model--allowed-statuses))
    (when (and scheduled
               (not (org-tasktree-model--valid-date-p scheduled)))
      (user-error "Scheduled must be YYYY-MM-DD or nil"))
    (when (and deadline
               (not (org-tasktree-model--valid-date-p deadline)))
      (user-error "Deadline must be YYYY-MM-DD or nil"))
    (when (and repeat
               (not (org-tasktree-model--valid-repeat-p repeat)))
      (user-error "Repeat must follow org repeat syntax"))
    (when (and (org-tasktree-model--valid-date-p scheduled)
               (org-tasktree-model--valid-date-p deadline)
               (string-lessp deadline scheduled))
      (user-error "Deadline must be >= scheduled"))
    (when (and priority
               (not (org-tasktree-model--priority-valid-p priority)))
      (user-error "Priority must be a single alphanumeric character"))
    (when (and tags (not (stringp tags)))
      (user-error "Tags must be string or nil"))
    (unless (org-tasktree-model--string-nonempty-p created-at)
      (user-error "Created_at is required"))
    (unless (org-tasktree-model--string-nonempty-p updated-at)
      (user-error "Updated_at is required"))
    node))

(provide 'org-tasktree-model)
;;; org-tasktree-model.el ends here
