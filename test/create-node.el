;;; create-node.el --- Insert parent/child nodes via org-tasktree-db -*- lexical-binding: t; -*-

;; This script expects the following variables to be set before load:
;; - org-tasktree-database-location : string (DB path)
;; - org-tasktree-query-dir         : string (queries dir; unused here but required by db helpers)
;; - org-tasktree-parent-node       : plist (uid title level todo-keyword node-type status)
;; - org-tasktree-child-nodes       : list of plist (same keys as parent)

(require 'sqlite)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'org-tasktree-db)
(require 'org-tasktree-model)
(require 'subr-x)

(defun create-node--plist->node (plist)
  "Convert PLIST to `org-tasktree-model-node'."
  (let* ((uid (or (plist-get plist :uid) (org-tasktree-db-generate-uid)))
         (now (format-time-string "%FT%T%:z" (current-time)))
         (created (plist-get plist :created-at)))
    (org-tasktree-model-node-create
     :uid uid
     :node-type (plist-get plist :node-type)
     :todo-keyword (plist-get plist :todo-keyword)
     :title (plist-get plist :title)
     :level (plist-get plist :level)
     :status (plist-get plist :status)
     :project-id (plist-get plist :project-id)
     :phase-id (plist-get plist :phase-id)
     :parent-id (plist-get plist :parent-id)
     :priority (plist-get plist :priority)
     :scheduled (plist-get plist :scheduled)
     :deadline (plist-get plist :deadline)
     :tags (plist-get plist :tags)
     :created-at created
     :updated-at now)))

(unless (and (boundp 'org-tasktree-database-location)
             (stringp org-tasktree-database-location))
  (error "org-tasktree-database-location must be set to a string"))

(unless (and (boundp 'org-tasktree-query-dir)
             (stringp org-tasktree-query-dir))
  (error "org-tasktree-query-dir must be set to a string"))

(unless (boundp 'org-tasktree-parent-node)
  (error "org-tasktree-parent-node must be provided as a plist"))

(unless (boundp 'org-tasktree-child-nodes)
  (setq org-tasktree-child-nodes nil))

(unless (file-exists-p org-tasktree-database-location)
  (error "Database not found at %s (run db-init first)" org-tasktree-database-location))

;; Insert parent first, then children after resolving parent row id.
(let* ((parent-node (create-node--plist->node org-tasktree-parent-node))
       (parent-uid (org-tasktree-model-node-uid parent-node)))
  (org-tasktree-db-commit-nodes (list parent-node))
  (let* ((db (sqlite-open org-tasktree-database-location))
         (row (car (sqlite-select db "SELECT id FROM nodes WHERE uid = ?;"
                                  (vector parent-uid))))
         (parent-id (when row (org-tasktree-db--row-nth row 0))))
    (sqlite-close db)
    (unless parent-id
      (error "Parent row not found after insert"))
    (let ((children
           (mapcar
            (lambda (plist)
              (let ((plist (copy-sequence plist)))
                (unless (plist-get plist :project-id)
                  (plist-put plist :project-id parent-id))
                (unless (plist-get plist :parent-id)
                  (plist-put plist :parent-id parent-id))
                (create-node--plist->node plist)))
            org-tasktree-child-nodes)))
      (when children
        (org-tasktree-db-commit-nodes children)))
    (message "Inserted parent UID %s (id=%s) and %d children"
             parent-uid parent-id (length org-tasktree-child-nodes))))

(kill-emacs 0)
