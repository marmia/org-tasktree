;;; org-tasktree-model.el --- Data model for org-tasktree -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
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

(defconst org-tasktree-model--allowed-node-types
  '("project" "phase" "group" "task")
  "Allowed `node_type' values.")

(defconst org-tasktree-model--allowed-statuses
  '("OPEN" "DONE")
  "Allowed `status' values.")

(cl-defstruct org-tasktree-model-node
  id uid parent-id node-type todo-keyword title level priority
  scheduled deadline closed-at tags status project-id phase-id
  created-at updated-at)

(cl-defun org-tasktree-model-node-create
    (&key id uid parent-id node-type todo-keyword title level priority
          scheduled deadline closed-at tags status project-id phase-id
          created-at updated-at)
  "Create node from keyword arguments.
Accepts ID, UID, PARENT-ID, NODE-TYPE, TODO-KEYWORD, TITLE, LEVEL,
PRIORITY, SCHEDULED, DEADLINE, CLOSED-AT, TAGS, STATUS, PROJECT-ID,
PHASE-ID, CREATED-AT, and UPDATED-AT."
  (make-org-tasktree-model-node
   :id id
   :uid uid
   :parent-id parent-id
   :node-type node-type
   :todo-keyword todo-keyword
   :title title
   :level level
   :priority priority
   :scheduled scheduled
   :deadline deadline
   :closed-at closed-at
   :tags tags
   :status status
   :project-id project-id
   :phase-id phase-id
   :created-at created-at
   :updated-at updated-at))

(defun org-tasktree-model--row-nth (row index)
  "Return ROW element at INDEX supporting vectors or lists."
  (if (vectorp row) (aref row index) (nth index row)))

(defun org-tasktree-model-node-from-db-row (row)
  "Create `org-tasktree-model-node' from DB ROW.
ROW must follow table column order: id, uid, parent_id, node_type,
TODO keyword, title, level, priority, scheduled, deadline,
closed_at, tags, status, project_id, phase_id, created_at, and
updated_at."
  (org-tasktree-model-node-create
   :id (org-tasktree-model--row-nth row 0)
   :uid (org-tasktree-model--row-nth row 1)
   :parent-id (org-tasktree-model--row-nth row 2)
   :node-type (org-tasktree-model--row-nth row 3)
   :todo-keyword (org-tasktree-model--row-nth row 4)
   :title (org-tasktree-model--row-nth row 5)
   :level (org-tasktree-model--row-nth row 6)
   :priority (org-tasktree-model--row-nth row 7)
   :scheduled (org-tasktree-model--row-nth row 8)
   :deadline (org-tasktree-model--row-nth row 9)
   :closed-at (org-tasktree-model--row-nth row 10)
   :tags (org-tasktree-model--row-nth row 11)
   :status (org-tasktree-model--row-nth row 12)
   :project-id (org-tasktree-model--row-nth row 13)
   :phase-id (org-tasktree-model--row-nth row 14)
   :created-at (org-tasktree-model--row-nth row 15)
   :updated-at (org-tasktree-model--row-nth row 16)))

(defun org-tasktree-model-node-from-plist (plist)
  "Create `org-tasktree-model-node' from PLIST with keyword keys."
  (org-tasktree-model-node-create
   :id (plist-get plist :id)
   :uid (plist-get plist :uid)
   :parent-id (plist-get plist :parent-id)
   :node-type (plist-get plist :node-type)
   :todo-keyword (plist-get plist :todo-keyword)
   :title (plist-get plist :title)
   :level (plist-get plist :level)
   :priority (plist-get plist :priority)
   :scheduled (plist-get plist :scheduled)
   :deadline (plist-get plist :deadline)
   :closed-at (plist-get plist :closed-at)
   :tags (plist-get plist :tags)
   :status (plist-get plist :status)
   :project-id (plist-get plist :project-id)
   :phase-id (plist-get plist :phase-id)
   :created-at (plist-get plist :created-at)
   :updated-at (plist-get plist :updated-at)))

(defun org-tasktree-model-node-to-plist (node)
  "Return PLIST representation of NODE."
  (list :id (org-tasktree-model-node-id node)
        :uid (org-tasktree-model-node-uid node)
        :parent-id (org-tasktree-model-node-parent-id node)
        :node-type (org-tasktree-model-node-node-type node)
        :todo-keyword (org-tasktree-model-node-todo-keyword node)
        :title (org-tasktree-model-node-title node)
        :level (org-tasktree-model-node-level node)
        :priority (org-tasktree-model-node-priority node)
        :scheduled (org-tasktree-model-node-scheduled node)
        :deadline (org-tasktree-model-node-deadline node)
        :closed-at (org-tasktree-model-node-closed-at node)
        :tags (org-tasktree-model-node-tags node)
        :status (org-tasktree-model-node-status node)
        :project-id (org-tasktree-model-node-project-id node)
        :phase-id (org-tasktree-model-node-phase-id node)
        :created-at (org-tasktree-model-node-created-at node)
        :updated-at (org-tasktree-model-node-updated-at node)))

(defun org-tasktree-model-node->db-vector (node &optional include-id)
  "Return NODE fields as vector in DB column order.
When INCLUDE-ID is non-nil, the first element is `id'; otherwise it is
omitted, starting from `uid'."
  (let ((vals (list (org-tasktree-model-node-id node)
                    (org-tasktree-model-node-uid node)
                    (org-tasktree-model-node-parent-id node)
                    (org-tasktree-model-node-node-type node)
                    (org-tasktree-model-node-todo-keyword node)
                    (org-tasktree-model-node-title node)
                    (org-tasktree-model-node-level node)
                    (org-tasktree-model-node-priority node)
                    (org-tasktree-model-node-scheduled node)
                    (org-tasktree-model-node-deadline node)
                    (org-tasktree-model-node-closed-at node)
                    (org-tasktree-model-node-tags node)
                    (org-tasktree-model-node-status node)
                    (org-tasktree-model-node-project-id node)
                    (org-tasktree-model-node-phase-id node)
                    (org-tasktree-model-node-created-at node)
                    (org-tasktree-model-node-updated-at node))))
    (apply #'vector (if include-id vals (cdr vals)))))

(defun org-tasktree-model--string-nonempty-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun org-tasktree-model--valid-date-p (value)
  "Return non-nil when VALUE matches YYYY-MM-DD format."
  (and (stringp value)
       (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" value)))

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
        (node-type (org-tasktree-model-node-node-type node))
        (title (org-tasktree-model-node-title node))
        (level (org-tasktree-model-node-level node))
        (status (org-tasktree-model-node-status node))
        (project-id (org-tasktree-model-node-project-id node))
        (phase-id (org-tasktree-model-node-phase-id node))
        (priority (org-tasktree-model-node-priority node))
        (scheduled (org-tasktree-model-node-scheduled node))
        (deadline (org-tasktree-model-node-deadline node))
        (tags (org-tasktree-model-node-tags node))
        (created-at (org-tasktree-model-node-created-at node))
        (updated-at (org-tasktree-model-node-updated-at node)))
    (unless (org-tasktree-model--string-nonempty-p uid)
      (user-error "UID is required"))
    (unless (member node-type org-tasktree-model--allowed-node-types)
      (user-error "Node type must be one of %S"
                  org-tasktree-model--allowed-node-types))
    (unless (org-tasktree-model--string-nonempty-p title)
      (user-error "Title is required"))
    (unless (org-tasktree-model--title-valid-p title)
      (user-error "Title must not include control characters"))
    (unless (and (integerp level) (>= level 1))
      (user-error "Level must be integer >= 1"))
    (unless (member status org-tasktree-model--allowed-statuses)
      (user-error "Status must be one of %S"
                  org-tasktree-model--allowed-statuses))
    (when (and (not (equal node-type "project"))
               (not (numberp project-id)))
      (user-error "Project ID is required for non-project nodes"))
    (when (and phase-id (not (numberp phase-id)))
      (user-error "Phase ID must be a number or nil"))
    (when (and scheduled
               (not (org-tasktree-model--valid-date-p scheduled)))
      (user-error "Scheduled must be YYYY-MM-DD or nil"))
    (when (and deadline
               (not (org-tasktree-model--valid-date-p deadline)))
      (user-error "Deadline must be YYYY-MM-DD or nil"))
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
