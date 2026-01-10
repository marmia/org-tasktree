;;; org-tasktree-query-sql.el --- Query SQL helpers for org-tasktree -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1"))
;; Package: org-tasktree-query

;;; Commentary:
;;
;; SQL building and fetch helpers for org-tasktree queries.
;; Centralizes tree expansion and ordering for query results.
;;
;;; Code:

(require 'seq)
(require 'sqlite)
(require 'subr-x)
(require 'org-tasktree-db)
(require 'org-tasktree-model)

(defconst org-tasktree-query-sql--order-clause
  "ORDER BY LOWER(title), uid"
  "Deterministic order for tree output (title ascending).")

(defun org-tasktree-query-sql--row-nth (row index)
  "Return ROW element at INDEX supporting vectors or lists."
  (if (vectorp row) (aref row index) (nth index row)))

(defun org-tasktree-query-sql--validate-date-field (db field)
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
             (uid (org-tasktree-query-sql--row-nth row 0))
             (value (org-tasktree-query-sql--row-nth row 1)))
        (user-error "Invalid %s value for UID %s: %s"
                    field uid value)))))

(defun org-tasktree-query-sql--build-sql (where-clause)
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
    "  id, uid, parent_id, todo_keyword, title, priority, scheduled,"
    "  deadline, repeat, closed_at, tags, content, status, created_at,"
    "  updated_at"
    "FROM tree"
    org-tasktree-query-sql--order-clause
    ";")
   "\n"))

(defun org-tasktree-query-sql--sort-children (nodes)
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

(defun org-tasktree-query-sql--preorder (parent-id children-table result)
  "Depth-first preorder from PARENT-ID using CHILDREN-TABLE.
RESULT is the accumulating list (reversed)."
  (let ((kids (gethash parent-id children-table)))
    (dolist (child kids result)
      (setq result (push child result))
      (setq result (org-tasktree-query-sql--preorder
                    (org-tasktree-model-node-id child)
                    children-table result)))))

(defun org-tasktree-query-sql--preorder-sort (nodes)
  "Return NODES sorted in parent-before-children preorder."
  (let* ((ids (mapcar #'org-tasktree-model-node-id nodes))
         (id-set (let ((ht (make-hash-table :test 'equal)))
                   (dolist (id ids) (puthash id t ht))
                   ht))
         (children (org-tasktree-query-sql--sort-children nodes))
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
      (setq result (org-tasktree-query-sql--preorder
                    (org-tasktree-model-node-id root)
                    children result)))
    (nreverse result)))

(defun org-tasktree-query-sql--fetch (where-clause params
                                               &optional date-field)
  "Execute search defined by WHERE-CLAUSE and PARAMS.
DATE-FIELD optionally validates date strings before running the query.
Returns a list of `org-tasktree-model-node' in preorder."
  (org-tasktree-db--with-db db
    (when date-field
      (org-tasktree-query-sql--validate-date-field db date-field))
    (let* ((sql (org-tasktree-query-sql--build-sql
                 where-clause))
           (rows (sqlite-select db sql params))
           (nodes (mapcar
                   #'org-tasktree-model-node-from-db-row
                   rows)))
      (org-tasktree-query-sql--preorder-sort nodes))))

(defun org-tasktree-query-sql--build-sql-descendants (where-clause)
  "Return SQL string for WHERE-CLAUSE with descendants only."
  (mapconcat
   #'identity
   (list
    "WITH RECURSIVE targets AS ("
    (format "  SELECT id FROM nodes WHERE %s" where-clause)
    ")"
    " , tree AS ("
    "   SELECT n.* FROM nodes n JOIN targets t ON n.id = t.id"
    "   UNION"
    "   SELECT c.* FROM nodes c JOIN tree t ON c.parent_id = t.id"
    " )"
    "SELECT DISTINCT"
    "  id, uid, parent_id, todo_keyword, title, priority, scheduled,"
    "  deadline, repeat, closed_at, tags, content, status, created_at,"
    "  updated_at"
    "FROM tree"
    org-tasktree-query-sql--order-clause
    ";")
   "\n"))

(defun org-tasktree-query-sql--build-sql-both-directions (where-clause)
  "Return SQL string for WHERE-CLAUSE with ancestors and descendants."
  (mapconcat
   #'identity
   (list
    "WITH RECURSIVE targets(id) AS ("
    (format "  SELECT id FROM nodes WHERE %s" where-clause)
    ")"
    " , ancestors(id) AS ("
    "   SELECT id FROM targets"
    "   UNION"
    "   SELECT n.parent_id FROM nodes n"
    "     JOIN ancestors a ON n.id = a.id"
    "    WHERE n.parent_id IS NOT NULL"
    " )"
    " , descendants(id) AS ("
    "   SELECT id FROM targets"
    "   UNION"
    "   SELECT n.id FROM nodes n"
    "     JOIN descendants d ON n.parent_id = d.id"
    " )"
    " , tree(id) AS ("
    "   SELECT id FROM ancestors"
    "   UNION"
    "   SELECT id FROM descendants"
    " )"
    "SELECT DISTINCT"
    "  id, uid, parent_id, todo_keyword, title, priority, scheduled,"
    "  deadline, repeat, closed_at, tags, content, status, created_at,"
    "  updated_at"
    "FROM nodes"
    "WHERE id IN (SELECT id FROM tree)"
    org-tasktree-query-sql--order-clause
    ";")
   "\n"))

(defun org-tasktree-query-sql--build-sql-targets-only (where-clause)
  "Return SQL string for WHERE-CLAUSE without ancestors or descendants."
  (mapconcat
   #'identity
   (list
    "SELECT"
    "  id, uid, parent_id, todo_keyword, title, priority, scheduled,"
    "  deadline, repeat, closed_at, tags, content, status, created_at,"
    "  updated_at"
    "FROM nodes"
    (format "WHERE %s" where-clause)
    org-tasktree-query-sql--order-clause
    ";")
   "\n"))

(defun org-tasktree-query-sql--build-sql-by-query (where-clause include-ancestor
                                                            include-descendants)
  "Return SQL string for WHERE-CLAUSE with INCLUDE-ANCESTOR and INCLUDE-DESCENDANTS."
  (cond
   ((and include-ancestor include-descendants)
    (org-tasktree-query-sql--build-sql-both-directions where-clause))
   (include-ancestor
    (org-tasktree-query-sql--build-sql where-clause))
   (include-descendants
    (org-tasktree-query-sql--build-sql-descendants where-clause))
   (t
    (org-tasktree-query-sql--build-sql-targets-only where-clause))))

(provide 'org-tasktree-query-sql)
;;; org-tasktree-query-sql.el ends here
