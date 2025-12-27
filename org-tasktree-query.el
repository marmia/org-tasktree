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

(require 'sqlite)
(require 'subr-x)
(require 'seq)
(require 'org-tasktree-db)
(require 'org-tasktree-model)

(defconst org-tasktree-query--order-clause
  (concat
   "ORDER BY CASE node_type"
   " WHEN 'project' THEN 1"
   " WHEN 'phase' THEN 2"
   " WHEN 'group' THEN 3"
   " ELSE 4 END, LOWER(title)")
  "Deterministic order for tree output (title ascending per level).")

(defun org-tasktree-query--row-nth (row index)
  "Return ROW element at INDEX supporting vectors or lists."
  (if (vectorp row) (aref row index) (nth index row)))

(defun org-tasktree-query--format-date (time)
  "Return YYYY-MM-DD string from TIME."
  (format-time-string "%Y-%m-%d" time))

(defvar org-tasktree-query--now-function #'current-time
  "Function returning current time for query date calculations.")

(defun org-tasktree-query--today ()
  "Return today's date string (local time)."
  (org-tasktree-query--format-date
   (funcall org-tasktree-query--now-function)))

(defun org-tasktree-query--days-from-now (days)
  "Return date string DAYS from now (local time)."
  (org-tasktree-query--format-date
   (time-add (funcall org-tasktree-query--now-function)
             (days-to-time days))))

(defun org-tasktree-query--validate-date-field (db field)
  "Signal `user-error' when FIELD contain invalid dates on DB.
FIELD must be a column name such as scheduled or deadline."
  (let* ((sql (format
               (string-join
                '("SELECT uid, %s FROM nodes"
                  " WHERE status='OPEN'"
                  "   AND %s IS NOT NULL"
                  "   AND DATE(%s) IS NULL"
                  " LIMIT 1;")
                "")
               field field field))
         (rows (sqlite-select db sql)))
    (when rows
      (let* ((row (car rows))
             (uid (org-tasktree-query--row-nth row 0))
             (value (org-tasktree-query--row-nth row 1)))
        (user-error "Invalid %s value for UID %s: %s"
                    field uid value)))))

(defun org-tasktree-query--build-sql (where-clause)
  "Return full SQL string using WHERE-CLAUSE for target selection."
  (mapconcat
   #'identity
   (list
    "WITH RECURSIVE targets AS ("
    (format "  SELECT id FROM nodes WHERE %s" where-clause)
    ")"
    " , tree AS ("
    "   SELECT n.* FROM nodes n JOIN targets t ON n.id = t.id"
    "   UNION"
    "   SELECT p.* FROM nodes p JOIN tree t ON p.id = t.parent_id"
    " )"
    "SELECT DISTINCT"
    "  id, uid, parent_id, node_type, todo_keyword, title, level,"
    "  priority, scheduled, deadline, repeat, closed_at, tags, content,"
    "  status, project_id, phase_id, created_at, updated_at"
    "FROM tree"
    org-tasktree-query--order-clause
    ";")
   "\n"))

(defun org-tasktree-query--sort-children (nodes)
  "Return alist parent-id -> children list for NODES.
Children are sorted by title (case-insensitive)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (node nodes)
      (let* ((parent (org-tasktree-model-node-parent-id node))
             (list (gethash parent table)))
        (puthash parent
                 (sort (cons node list)
                       (lambda (a b)
                         (string-lessp
                          (downcase
                           (org-tasktree-model-node-title a))
                          (downcase
                           (org-tasktree-model-node-title b)))))
                 table)))
    table))

(defun org-tasktree-query--preorder (parent-id children-table result)
  "Depth-first preorder from PARENT-ID using CHILDREN-TABLE.
RESULT is the accumulating list (reversed)."
  (let ((kids (gethash parent-id children-table)))
    (dolist (child kids result)
      (setq result (push child result))
      (setq result (org-tasktree-query--preorder
                    (org-tasktree-model-node-id child)
                    children-table result)))))

(defun org-tasktree-query--preorder-sort (nodes)
  "Return NODES sorted in parent-before-children preorder."
  (let* ((ids (mapcar #'org-tasktree-model-node-id nodes))
         (id-set (let ((ht (make-hash-table :test 'equal)))
                   (dolist (id ids) (puthash id t ht))
                   ht))
         (children (org-tasktree-query--sort-children nodes))
         (roots (seq-filter
                 (lambda (node)
                   (let ((parent
                          (org-tasktree-model-node-parent-id
                           node)))
                     (or (null parent)
                         (not (gethash parent id-set)))))
                 nodes))
         (roots-sorted
          (sort roots
                (lambda (a b)
                  (string-lessp
                   (downcase
                    (org-tasktree-model-node-title a))
                   (downcase
                    (org-tasktree-model-node-title b))))))
         (result nil))
    (dolist (root roots-sorted)
      (setq result (push root result))
      (setq result (org-tasktree-query--preorder
                    (org-tasktree-model-node-id root)
                    children result)))
    (nreverse result)))

(defun org-tasktree-query--fetch (where-clause params
                                               &optional date-field)
  "Execute search defined by WHERE-CLAUSE and PARAMS.
DATE-FIELD optionally validates date strings before running the query.
Returns a list of `org-tasktree-model-node' in preorder."
  (org-tasktree-db--with-db db
    (when date-field
      (org-tasktree-query--validate-date-field db date-field))
    (let* ((sql (org-tasktree-query--build-sql
                 where-clause))
           (rows (sqlite-select db sql params))
           (nodes (mapcar
                   #'org-tasktree-model-node-from-db-row
                   rows)))
      (org-tasktree-query--preorder-sort nodes))))

(defun org-tasktree-query-search-today ()
  "Return tasks scheduled for today with their parent tree."
  (org-tasktree-query--fetch
   (string-join
    '("status='OPEN'"
      "AND scheduled IS NOT NULL"
      "AND DATE(scheduled) IS NOT NULL"
      "AND DATE(scheduled) = DATE(?)")
    " ")
   (vector (org-tasktree-query--today))
   "scheduled"))

(defun org-tasktree-query-search-before-today ()
  "Return tasks scheduled on or before today with their parent tree."
  (org-tasktree-query--fetch
   (string-join
    '("status='OPEN'"
      "AND scheduled IS NOT NULL"
      "AND DATE(scheduled) IS NOT NULL"
      "AND DATE(scheduled) <= DATE(?)")
    " ")
   (vector (org-tasktree-query--today))
   "scheduled"))

(defun org-tasktree-query-search-overdue ()
  "Return tasks whose deadline is before today.
Parents are included."
  (org-tasktree-query--fetch
   (string-join
    '("status='OPEN'"
      "AND deadline IS NOT NULL"
      "AND DATE(deadline) IS NOT NULL"
      "AND DATE(deadline) < DATE(?)")
    " ")
   (vector (org-tasktree-query--today))
   "deadline"))

(defun org-tasktree-query-search-next-7day ()
  "Return tasks scheduled between tomorrow and the next seven days.
Both boundaries are inclusive, and parents are included."
  (org-tasktree-query--fetch
   (string-join
    '("status='OPEN'"
      "AND scheduled IS NOT NULL"
      "AND DATE(scheduled) IS NOT NULL"
      "AND DATE(scheduled) BETWEEN DATE(?) AND DATE(?)")
    " ")
   (vector (org-tasktree-query--days-from-now 1)
           (org-tasktree-query--days-from-now 7))
   "scheduled"))

(defun org-tasktree-query-search-unscheduled ()
  "Return tasks with no scheduled date set.
Parents are included."
  (org-tasktree-query--fetch
   (string-join
    '("status='OPEN'"
      "AND node_type='task'"
      "AND scheduled IS NULL")
    " ")
   []
   "scheduled"))

(defun org-tasktree-query-open-tree ()
  "Return OPEN nodes (project/phase/group/task) in preorder."
  (seq-filter
   (lambda (n)
     (equal (org-tasktree-model-node-status n) "OPEN"))
   (org-tasktree-query--fetch "status='OPEN'" [])))

(defun org-tasktree-query-get-node-by-id (id)
  "Return node struct for numeric ID, or nil."
  (when id
    (org-tasktree-db--with-db db
      (let ((rows (sqlite-select
                   db
                   (concat
                    "SELECT id, uid, parent_id, node_type, todo_keyword, title,"
                    " level, priority, scheduled, deadline, repeat, closed_at,"
                    " tags, content, status, project_id, phase_id, created_at,"
                    " updated_at"
                    " FROM nodes WHERE id = ? LIMIT 1;")
                   (vector id))))
        (when rows
          (org-tasktree-model-node-from-db-row (car rows)))))))

(provide 'org-tasktree-query)
;;; org-tasktree-query.el ends here
