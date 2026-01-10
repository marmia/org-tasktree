;;; org-tasktree-query.el --- Query layer for org-tasktree -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; Query helpers to retrieve task trees from SQLite.
;; Follows the v0.1 search specifications.
;;
;;; Code:

(require 'seq)
(require 'sqlite)
(require 'subr-x)
(require 'org-tasktree-db)
(require 'org-tasktree-model)
(require 'org-tasktree-query-parser)
(require 'org-tasktree-query-sql)

(defun org-tasktree-query-search-today ()
  "Return tasks scheduled for today with their parent tree."
  (org-tasktree-query-sql--fetch
   (string-join
    '("status='OPEN'"
      "AND scheduled IS NOT NULL"
      "AND DATE(scheduled) IS NOT NULL"
      "AND DATE(scheduled) = DATE(?)")
    " ")
   (vector (org-tasktree-query-parser--today))
   "scheduled"))

(defun org-tasktree-query-search-before-today ()
  "Return tasks scheduled on or before today with their parent tree."
  (org-tasktree-query-sql--fetch
   (string-join
    '("status='OPEN'"
      "AND scheduled IS NOT NULL"
      "AND DATE(scheduled) IS NOT NULL"
      "AND DATE(scheduled) <= DATE(?)")
    " ")
   (vector (org-tasktree-query-parser--today))
   "scheduled"))

(defun org-tasktree-query-search-overdue ()
  "Return tasks whose deadline is before today.
Parents are included."
  (org-tasktree-query-sql--fetch
   (string-join
    '("status='OPEN'"
      "AND deadline IS NOT NULL"
      "AND DATE(deadline) IS NOT NULL"
      "AND DATE(deadline) < DATE(?)")
    " ")
   (vector (org-tasktree-query-parser--today))
   "deadline"))

(defun org-tasktree-query-search-next-7day ()
  "Return tasks scheduled between tomorrow and the next seven days.
Both boundaries are inclusive, and parents are included."
  (org-tasktree-query-sql--fetch
   (string-join
    '("status='OPEN'"
      "AND scheduled IS NOT NULL"
      "AND DATE(scheduled) IS NOT NULL"
      "AND DATE(scheduled) BETWEEN DATE(?) AND DATE(?)")
    " ")
   (vector (org-tasktree-query-parser--days-from-now 1)
           (org-tasktree-query-parser--days-from-now 7))
   "scheduled"))

(defun org-tasktree-query-search-unscheduled ()
  "Return tasks with no scheduled date set.
Parents are included."
  (org-tasktree-query-sql--fetch
   (string-join
    '("status='OPEN'"
      "AND scheduled IS NULL")
    " ")
   []
   "scheduled"))

(defun org-tasktree-query-search-all ()
  "Return all nodes including DONE.
Parents are included."
  (org-tasktree-db--with-db db
    (org-tasktree-query-sql--validate-date-field db "scheduled")
    (org-tasktree-query-sql--validate-date-field db "deadline"))
  (org-tasktree-query-sql--fetch "1=1" []))

(defun org-tasktree-query-default-template ()
  "Return default query YAML template string."
  (string-join
   '("todo_keyword:"
     "title:"
     "priority:"
     "scheduled:"
     "deadline:"
     "repeat:"
     "closed_at:"
     "tags:"
     "content:"
     "status:"
     "created_at:"
     "updated_at:"
     "include_ancestor: true"
     "include_descendants: true")
   "\n"))

(defun org-tasktree-query-search-by-query (text)
  "Return nodes matching query TEXT."
  (let* ((parsed (org-tasktree-query-parser--parse-query-text text))
         (where (plist-get parsed :where))
         (params (plist-get parsed :params))
         (include-ancestor (plist-get parsed :include-ancestor))
         (include-descendants (plist-get parsed :include-descendants))
         (sql (org-tasktree-query-sql--build-sql-by-query
               where include-ancestor include-descendants)))
    (org-tasktree-db--with-db db
      (let* ((rows (sqlite-select db sql params))
             (nodes (mapcar #'org-tasktree-model-node-from-db-row rows)))
        (when (and (not include-ancestor) (not include-descendants))
          (dolist (node nodes)
            (setf (org-tasktree-model-node-parent-id node) nil)))
        (org-tasktree-query-sql--preorder-sort nodes)))))

(defun org-tasktree-query-open-tree ()
  "Return OPEN nodes in preorder."
  (seq-filter
   (lambda (n)
     (equal (org-tasktree-model-node-status n) "OPEN"))
   (org-tasktree-query-sql--fetch "status='OPEN'" [])))

(defun org-tasktree-query-get-node-by-id (id)
  "Return node struct for numeric ID, or nil."
  (when id
    (org-tasktree-db--with-db db
      (let ((rows (sqlite-select
                   db
                   (concat
                    "SELECT id, uid, parent_id, todo_keyword, title, priority,"
                    " scheduled, deadline, repeat, closed_at, tags, content,"
                    " status, created_at, updated_at"
                    " FROM nodes WHERE id = ? LIMIT 1;")
                   (vector id))))
        (when rows
          (org-tasktree-model-node-from-db-row (car rows)))))))

(provide 'org-tasktree-query)
;;; org-tasktree-query.el ends here
