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

(require 'cl-lib)
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

(defconst org-tasktree-query--escaped-star-token
  "__ORG_TASKTREE_ESC_STAR__"
  "Placeholder token for escaped '*' in query values.")

(defconst org-tasktree-query--escaped-qmark-token
  "__ORG_TASKTREE_ESC_QMARK__"
  "Placeholder token for escaped '?' in query values.")

(defconst org-tasktree-query--query-keys
  '("node_type"
    "todo_keyword"
    "title"
    "priority"
    "scheduled"
    "deadline"
    "repeat"
    "closed_at"
    "tags"
    "content"
    "status"
    "project_id"
    "phase_id"
    "created_at"
    "updated_at"
    "include_ancestor"
    "include_descendants")
  "Supported query keys for `org-tasktree-search-by-query'.")

(defconst org-tasktree-query--field-definitions
  '(("node_type" :column "node_type" :type string)
    ("todo_keyword" :column "todo_keyword" :type string)
    ("title" :column "title" :type string)
    ("priority" :column "priority" :type string)
    ("scheduled" :column "scheduled" :type date)
    ("deadline" :column "deadline" :type date)
    ("repeat" :column "repeat" :type string)
    ("closed_at" :column "closed_at" :type string)
    ("tags" :column "tags" :type tags)
    ("content" :column "content" :type string)
    ("status" :column "status" :type string)
    ("project_id" :column "project_id" :type number)
    ("phase_id" :column "phase_id" :type number)
    ("created_at" :column "created_at" :type date)
    ("updated_at" :column "updated_at" :type date))
  "Field definitions for query parsing.")

(defconst org-tasktree-query--date-fields
  '("scheduled" "deadline" "created_at" "updated_at")
  "Fields treated as date columns for query validation.")

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

(defun org-tasktree-query-search-all ()
  "Return all nodes including DONE.
Parents are included."
  (org-tasktree-db--with-db db
    (org-tasktree-query--validate-date-field db "scheduled")
    (org-tasktree-query--validate-date-field db "deadline"))
  (org-tasktree-query--fetch "1=1" []))

(defun org-tasktree-query-default-template ()
  "Return default query YAML template string."
  (string-join
   '("node_type:"
     "todo_keyword:"
     "title:"
     "priority:"
     "scheduled:"
     "deadline:"
     "repeat:"
     "closed_at:"
     "tags:"
     "content:"
     "status:"
     "project_id:"
     "phase_id:"
     "created_at:"
     "updated_at:"
     "include_ancestor: true"
     "include_descendants: true")
   "\n"))

(defun org-tasktree-query--field-definition (key)
  "Return field definition plist for KEY or nil."
  (assoc-default key org-tasktree-query--field-definitions))

(defun org-tasktree-query--tokenize (text)
  "Return list of tokens parsed from TEXT.
Tokens are plist objects with :text and :quoted."
  (let* ((len (length text))
         (i 0)
         tokens)
    (cl-labels ((skip-space ()
                  (while (and (< i len)
                              (memq (aref text i)
                                    '(?\s ?\t ?\n ?\r)))
                    (setq i (1+ i)))))
      (while (< i len)
        (skip-space)
        (when (< i len)
          (let ((ch (aref text i)))
            (if (or (eq ch ?\") (eq ch ?\'))
                (let ((quote ch)
                      (buf "")
                      (escaped nil)
                      (closed nil))
                  (setq i (1+ i))
                  (while (and (< i len) (not closed))
                    (let ((c (aref text i)))
                      (cond
                       ((and (eq c quote) (not escaped))
                        (setq closed t)
                        (setq i (1+ i)))
                       (t
                        (setq buf (concat buf (string c)))
                        (setq escaped (and (eq c ?\\) (not escaped)))
                        (setq i (1+ i))))))
                  (unless closed
                    (user-error "Unclosed quote in query value"))
                  (push (list :text buf :quoted t) tokens))
              (let ((start i))
                (while (and (< i len)
                            (not (memq (aref text i)
                                       '(?\s ?\t ?\n ?\r))))
                  (setq i (1+ i)))
                (push (list :text (substring text start i) :quoted nil)
                      tokens))))))
      (nreverse tokens))))

(defun org-tasktree-query--token-connector-p (token word)
  "Return non-nil when TOKEN is the connector WORD."
  (and (not (plist-get token :quoted))
       (string= (downcase (plist-get token :text)) word)))

(defun org-tasktree-query--operator-token-p (text)
  "Return non-nil when TEXT is a standalone operator."
  (member text '("not" "=" "<>" "<=" ">=" "<" ">")))

(defun org-tasktree-query--split-leading-operator (text)
  "Return (OP . REST) when TEXT start with an operator."
  (when (string-match
         "\\`\\(>=\\|<=\\|<>\\|=\\|<\\|>\\)\\(.+\\)\\'" text)
    (cons (match-string 1 text) (match-string 2 text))))

(defun org-tasktree-query--parse-term (token next-token)
  "Return term plist from TOKEN using NEXT-TOKEN function."
  (let* ((text (plist-get token :text))
         (quoted (plist-get token :quoted))
         (lower (downcase text)))
    (cond
     ((and (not quoted)
           (org-tasktree-query--operator-token-p lower))
      (let ((op lower)
            (value-token (funcall next-token)))
        (unless value-token
          (user-error "Missing value after operator: %s" text))
        (when (or (org-tasktree-query--token-connector-p value-token "and")
                  (org-tasktree-query--token-connector-p value-token "or"))
          (user-error "Missing value after operator: %s" text))
        (list :op op
              :value (plist-get value-token :text)
              :quoted (plist-get value-token :quoted))))
     ((and (not quoted)
           (org-tasktree-query--split-leading-operator text))
      (let* ((pair (org-tasktree-query--split-leading-operator text))
             (op (car pair))
             (value (cdr pair)))
        (list :op op
              :value value
              :quoted nil)))
     (t
      (list :op nil :value text :quoted quoted)))))

(defun org-tasktree-query--parse-expression (value)
  "Return OR-grouped terms parsed from VALUE."
  (let* ((value (string-trim value))
         (tokens (org-tasktree-query--tokenize value))
         groups
         current)
    (unless tokens
      (user-error "Empty query value"))
    (cl-labels ((next-token () (pop tokens)))
      (while tokens
        (let ((token (next-token)))
          (cond
           ((org-tasktree-query--token-connector-p token "or")
            (when (null current)
              (user-error "Unexpected OR in query value"))
            (push (nreverse current) groups)
            (setq current nil))
           ((org-tasktree-query--token-connector-p token "and")
            (when (null current)
              (user-error "Unexpected AND in query value")))
           (t
            (push (org-tasktree-query--parse-term token #'next-token)
                  current)))))
      (when current
        (push (nreverse current) groups)))
    (nreverse groups)))

(defun org-tasktree-query--unescape-value (raw)
  "Return (VALUE HAS-WILDCARD LITERAL-NULL) from RAW."
  (let ((i 0)
        (len (length raw))
        (buf "")
        (has-wildcard nil)
        (literal-null nil))
    (while (< i len)
      (let ((ch (aref raw i)))
        (if (eq ch ?\\)
            (if (< (1+ i) len)
                (let ((next (aref raw (1+ i))))
                  (cond
                   ((and (eq next ?N)
                         (<= (+ i 4) len)
                         (string= (substring raw (1+ i) (+ i 5)) "NULL"))
                    (setq buf (concat buf "NULL"))
                    (setq literal-null t)
                    (setq i (+ i 5)))
                   ((eq next ?*)
                    (setq buf (concat buf org-tasktree-query--escaped-star-token))
                    (setq i (+ i 2)))
                   ((eq next ??)
                    (setq buf (concat buf org-tasktree-query--escaped-qmark-token))
                    (setq i (+ i 2)))
                   ((or (eq next ?\\) (eq next ?\") (eq next ?\'))
                    (setq buf (concat buf (string next)))
                    (setq i (+ i 2)))
                   (t
                    (setq buf (concat buf (string next)))
                    (setq i (+ i 2)))))
              (setq buf (concat buf "\\"))
              (setq i (1+ i)))
          (when (or (eq ch ?*) (eq ch ??))
            (setq has-wildcard t))
          (setq buf (concat buf (string ch)))
          (setq i (1+ i)))))
    (list buf has-wildcard literal-null)))

(defun org-tasktree-query--apply-wildcards (value)
  "Return VALUE converted for SQL LIKE patterns."
  (let ((value (replace-regexp-in-string "\\*" "%" value t t)))
    (setq value (replace-regexp-in-string "\\?" "_" value t t))
    (setq value (replace-regexp-in-string
                 (regexp-quote org-tasktree-query--escaped-star-token)
                 "*"
                 value
                 t t))
    (setq value (replace-regexp-in-string
                 (regexp-quote org-tasktree-query--escaped-qmark-token)
                 "?"
                 value
                 t t))
    value))

(defun org-tasktree-query--finalize-literals (value)
  "Return VALUE with escaped wildcard tokens restored."
  (let ((value (replace-regexp-in-string
                (regexp-quote org-tasktree-query--escaped-star-token)
                "*"
                value
                t t)))
    (replace-regexp-in-string
     (regexp-quote org-tasktree-query--escaped-qmark-token)
     "?"
     value
     t t)))

(defun org-tasktree-query--normalize-term-value (raw quoted)
  "Return (VALUE HAS-WILDCARD LITERAL-NULL NULL-P) from RAW and QUOTED."
  (let* ((raw (string-trim raw))
         (info (org-tasktree-query--unescape-value raw))
         (value (nth 0 info))
         (has-wildcard (nth 1 info))
         (literal-null (nth 2 info))
         (null-p (and (not quoted)
                      (not literal-null)
                      (string= (upcase value) "NULL"))))
    (list value has-wildcard literal-null null-p)))

(defun org-tasktree-query--normalize-date-expression (value)
  "Return VALUE with relative date tokens normalized."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     "\\b\\(today\\|yesterday\\|tomorrow\\)\\s-*\\([+-][0-9]+d\\)\\b"
     "\\1\\2"
     value)))

(defun org-tasktree-query--normalize-date (value)
  "Return YYYY-MM-DD string parsed from VALUE."
  (let* ((trimmed (string-trim value))
         (lower (downcase trimmed)))
    (cond
     ((string-match
       "\\`\\(today\\|yesterday\\|tomorrow\\)\\([+-][0-9]+d\\)?\\'"
       lower)
      (let* ((base (match-string 1 lower))
             (delta-str (match-string 2 lower))
             (delta (when delta-str
                      (string-to-number
                       (substring delta-str 0 -1))))
             (base-offset (pcase base
                            ("today" 0)
                            ("yesterday" -1)
                            ("tomorrow" 1)))
             (days (+ base-offset (or delta 0))))
        (org-tasktree-query--days-from-now days)))
     ((string-match
       "\\`\\([0-9]\\{4\\}\\)[-/]\\([0-9]\\{2\\}\\)[-/]\\([0-9]\\{2\\}\\)\\'"
       trimmed)
      (let* ((date (format "%s-%s-%s"
                           (match-string 1 trimmed)
                           (match-string 2 trimmed)
                           (match-string 3 trimmed)))
             (parts (mapcar #'string-to-number (split-string date "-")))
             (time (encode-time 0 0 0
                                (nth 2 parts)
                                (nth 1 parts)
                                (nth 0 parts)))
             (normalized (format-time-string "%Y-%m-%d" time)))
        (unless (string= normalized date)
          (user-error "Invalid date value: %s" value))
        date))
     (t
      (user-error "Invalid date value: %s" value)))))

(defun org-tasktree-query--date-column-expr (column field)
  "Return SQL expression for date comparisons on COLUMN for FIELD."
  (if (member field '("created_at" "updated_at"))
      (format "substr(%s, 1, 10)" column)
    (format "DATE(%s)" column)))

(defun org-tasktree-query--build-string-term (column term)
  "Return (SQL . PARAMS) for string TERM on COLUMN."
  (let* ((op (plist-get term :op))
         (raw (plist-get term :value))
         (quoted (plist-get term :quoted))
         (norm (org-tasktree-query--normalize-term-value raw quoted))
         (value (nth 0 norm))
         (has-wildcard (nth 1 norm))
         (null-p (nth 3 norm)))
    (cond
     (null-p
      (cond
       ((or (null op) (string= op "="))
        (cons (format "%s IS NULL" column) nil))
       ((or (string= op "not") (string= op "<>"))
        (cons (format "%s IS NOT NULL" column) nil))
       (t
        (user-error "Invalid operator for NULL: %s" op))))
     (has-wildcard
      (let ((pattern (org-tasktree-query--apply-wildcards value))
            (op (or op "=")))
        (cond
         ((or (string= op "=") (null op))
          (cons (format "%s LIKE ? COLLATE NOCASE" column)
                (list pattern)))
         ((or (string= op "not") (string= op "<>"))
          (cons (format "%s NOT LIKE ? COLLATE NOCASE" column)
                (list pattern)))
         (t
          (user-error "Invalid operator for wildcard: %s" op)))))
     (t
      (let ((value (org-tasktree-query--finalize-literals value))
            (op (or op "=")))
        (cond
         ((or (string= op "=") (null op))
          (cons (format "%s = ?" column) (list value)))
         ((or (string= op "not") (string= op "<>"))
          (cons (format "%s <> ?" column) (list value)))
         ((member op '("<" ">" "<=" ">="))
          (cons (format "%s %s ?" column op) (list value)))
         (t
          (user-error "Invalid operator: %s" op))))))))

(defun org-tasktree-query--build-number-term (column term)
  "Return (SQL . PARAMS) for numeric TERM on COLUMN."
  (let* ((op (plist-get term :op))
         (raw (plist-get term :value))
         (quoted (plist-get term :quoted))
         (norm (org-tasktree-query--normalize-term-value raw quoted))
         (value (nth 0 norm))
         (has-wildcard (nth 1 norm))
         (null-p (nth 3 norm)))
    (when has-wildcard
      (user-error "Wildcard is not allowed for numeric fields"))
    (cond
     (null-p
      (cond
       ((or (null op) (string= op "="))
        (cons (format "%s IS NULL" column) nil))
       ((or (string= op "not") (string= op "<>"))
        (cons (format "%s IS NOT NULL" column) nil))
       (t
        (user-error "Invalid operator for NULL: %s" op))))
     (t
      (let ((op (or op "=")))
        (cond
         ((or (string= op "=") (null op))
          (cons (format "%s = ?" column) (list value)))
         ((or (string= op "not") (string= op "<>"))
          (cons (format "%s <> ?" column) (list value)))
         ((member op '("<" ">" "<=" ">="))
          (cons (format "%s %s ?" column op) (list value)))
         (t
          (user-error "Invalid operator: %s" op))))))))

(defun org-tasktree-query--build-date-term (column field term)
  "Return (SQL . PARAMS) for date TERM on COLUMN and FIELD."
  (let* ((op (plist-get term :op))
         (raw (plist-get term :value))
         (quoted (plist-get term :quoted))
         (norm (org-tasktree-query--normalize-term-value raw quoted))
         (value (nth 0 norm))
         (has-wildcard (nth 1 norm))
         (null-p (nth 3 norm))
         (expr (org-tasktree-query--date-column-expr column field)))
    (when has-wildcard
      (user-error "Wildcard is not allowed for date fields"))
    (cond
     (null-p
      (cond
       ((or (null op) (string= op "="))
        (cons (format "%s IS NULL" column) nil))
       ((or (string= op "not") (string= op "<>"))
        (cons (format "%s IS NOT NULL" column) nil))
       (t
        (user-error "Invalid operator for NULL: %s" op))))
     (t
      (let* ((date (org-tasktree-query--normalize-date value))
             (op (or op "=")))
        (cond
         ((or (string= op "=") (null op))
          (cons (format "%s = DATE(?)" expr) (list date)))
         ((or (string= op "not") (string= op "<>"))
          (cons (format "%s <> DATE(?)" expr) (list date)))
         ((member op '("<" ">" "<=" ">="))
          (cons (format "%s %s DATE(?)" expr op) (list date)))
         (t
          (user-error "Invalid operator: %s" op))))))))

(defun org-tasktree-query--build-tags-term (column term)
  "Return (SQL . PARAMS) for tags TERM on COLUMN."
  (let* ((op (plist-get term :op))
         (raw (plist-get term :value))
         (quoted (plist-get term :quoted))
         (norm (org-tasktree-query--normalize-term-value raw quoted))
         (value (nth 0 norm))
         (has-wildcard (nth 1 norm))
         (null-p (nth 3 norm))
         (expr (format "':' || %s || ':'" column)))
    (cond
     (null-p
      (cond
       ((or (null op) (string= op "="))
        (cons (format "%s IS NULL" column) nil))
       ((or (string= op "not") (string= op "<>"))
        (cons (format "%s IS NOT NULL" column) nil))
       (t
        (user-error "Invalid operator for NULL: %s" op))))
     (t
      (let* ((pattern (if has-wildcard
                          (org-tasktree-query--apply-wildcards value)
                        (org-tasktree-query--finalize-literals value)))
             (pattern (format "%%:%s:%%" pattern))
             (op (or op "=")))
        (cond
         ((or (string= op "=") (null op))
          (cons (format "%s LIKE ? COLLATE NOCASE" expr)
                (list pattern)))
         ((or (string= op "not") (string= op "<>"))
          (cons (format "%s NOT LIKE ? COLLATE NOCASE" expr)
                (list pattern)))
         (t
          (user-error "Invalid operator for tags: %s" op))))))))

(defun org-tasktree-query--build-field-condition (field value)
  "Return (SQL . PARAMS) for FIELD using VALUE string."
  (let* ((definition (org-tasktree-query--field-definition field))
         (column (plist-get definition :column))
         (type (plist-get definition :type))
         (value (if (member field org-tasktree-query--date-fields)
                    (org-tasktree-query--normalize-date-expression value)
                  value))
         (groups (org-tasktree-query--parse-expression value))
         (clauses nil)
         (params nil))
    (dolist (group groups)
      (let ((term-sqls nil)
            (term-params nil))
        (dolist (term group)
          (let* ((result
                  (pcase type
                    ('date
                     (org-tasktree-query--build-date-term column field term))
                    ('tags
                     (org-tasktree-query--build-tags-term column term))
                    ('number
                     (org-tasktree-query--build-number-term column term))
                    (_
                     (org-tasktree-query--build-string-term column term))))
                 (sql (car result))
                 (vals (cdr result)))
            (push sql term-sqls)
            (when vals
              (setq term-params (append term-params vals)))))
        (push (string-join (nreverse term-sqls) " AND ") clauses)
        (when term-params
          (setq params (append params term-params)))))
    (let* ((ordered (nreverse clauses))
           (sql (string-join ordered " OR ")))
      (when (> (length ordered) 1)
        (setq sql (format "(%s)" sql)))
      (cons sql params))))

(defun org-tasktree-query--parse-query-text (text)
  "Parse query TEXT and return plist with WHERE and flags."
  (let* ((lines (split-string text "\n" nil))
         (pairs (make-hash-table :test 'equal)))
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ((string-empty-p trimmed) nil)
         ((string-prefix-p "#" trimmed) nil)
         ((string-match "\\`\\([^:]+\\):\\(.*\\)\\'" trimmed)
          (let* ((key (string-trim (match-string 1 trimmed)))
                 (value (string-trim (match-string 2 trimmed))))
            (when (string-empty-p key)
              (user-error "Empty query key"))
            (puthash (downcase key) value pairs)))
         (t
          (user-error "Invalid query line: %s" trimmed)))))
    (let* ((include-ancestor-raw (gethash "include_ancestor" pairs))
           (include-desc-raw (gethash "include_descendants" pairs))
           (include-ancestor
            (if (null include-ancestor-raw)
                t
              (let ((v (string-trim include-ancestor-raw)))
                (when (string-empty-p v)
                  (user-error "Include_ancestor is empty"))
                (pcase (downcase v)
                  ("true" t)
                  ("false" nil)
                  (_ (user-error "Invalid include_ancestor value: %s" v))))))
           (include-descendants
            (if (null include-desc-raw)
                t
              (let ((v (string-trim include-desc-raw)))
                (when (string-empty-p v)
                  (user-error "Include_descendants is empty"))
                (pcase (downcase v)
                  ("true" t)
                  ("false" nil)
                  (_ (user-error "Invalid include_descendants value: %s" v))))))
           (clauses nil)
           (params nil))
      (mapc
       (lambda (field)
         (let ((value (gethash field pairs)))
           (when (and value (not (string-empty-p (string-trim value))))
             (let* ((result
                     (org-tasktree-query--build-field-condition field value))
                    (sql (car result))
                    (vals (cdr result)))
               (push sql clauses)
               (when vals
                 (setq params (append params vals)))))))
       (mapcar #'car org-tasktree-query--field-definitions))
      (when (null clauses)
        (setq clauses (list "1=1")))
      (let ((inbox-id (org-tasktree-db-inbox-id)))
        (push "id <> ?" clauses)
        (setq params (append params (list inbox-id))))
      (list :where (string-join (nreverse clauses) " AND ")
            :params (apply #'vector params)
            :include-ancestor include-ancestor
            :include-descendants include-descendants))))

(defun org-tasktree-query--build-sql-descendants (where-clause)
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
    "  id, uid, parent_id, node_type, todo_keyword, title, level,"
    "  priority, scheduled, deadline, repeat, closed_at, tags, content,"
    "  status, project_id, phase_id, created_at, updated_at"
    "FROM tree"
    org-tasktree-query--order-clause
    ";")
   "\n"))

(defun org-tasktree-query--build-sql-both-directions (where-clause)
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
    "  id, uid, parent_id, node_type, todo_keyword, title, level,"
    "  priority, scheduled, deadline, repeat, closed_at, tags, content,"
    "  status, project_id, phase_id, created_at, updated_at"
    "FROM nodes"
    "WHERE id IN (SELECT id FROM tree)"
    org-tasktree-query--order-clause
    ";")
   "\n"))

(defun org-tasktree-query--build-sql-targets-only (where-clause)
  "Return SQL string for WHERE-CLAUSE without ancestors or descendants."
  (mapconcat
   #'identity
   (list
    "SELECT"
    "  id, uid, parent_id, node_type, todo_keyword, title, level,"
    "  priority, scheduled, deadline, repeat, closed_at, tags, content,"
    "  status, project_id, phase_id, created_at, updated_at"
    "FROM nodes"
    (format "WHERE %s" where-clause)
    org-tasktree-query--order-clause
    ";")
   "\n"))

(defun org-tasktree-query--build-sql-by-query (where-clause include-ancestor
                                                            include-descendants)
  "Return SQL string for WHERE-CLAUSE with INCLUDE-ANCESTOR and INCLUDE-DESCENDANTS."
  (cond
   ((and include-ancestor include-descendants)
    (org-tasktree-query--build-sql-both-directions where-clause))
   (include-ancestor
    (org-tasktree-query--build-sql where-clause))
   (include-descendants
    (org-tasktree-query--build-sql-descendants where-clause))
   (t
    (org-tasktree-query--build-sql-targets-only where-clause))))

(defun org-tasktree-query-search-by-query (text)
  "Return nodes matching query TEXT."
  (let* ((parsed (org-tasktree-query--parse-query-text text))
         (where (plist-get parsed :where))
         (params (plist-get parsed :params))
         (include-ancestor (plist-get parsed :include-ancestor))
         (include-descendants (plist-get parsed :include-descendants))
         (sql (org-tasktree-query--build-sql-by-query
               where include-ancestor include-descendants)))
    (org-tasktree-db--with-db db
      (let* ((rows (sqlite-select db sql params))
             (nodes (mapcar #'org-tasktree-model-node-from-db-row rows))
             (inbox-id (org-tasktree-db-inbox-id)))
        (setq nodes
              (seq-remove
               (lambda (node)
                 (let ((id (org-tasktree-model-node-id node)))
                   (and (numberp id) (= id inbox-id))))
               nodes))
        (when (and (not include-ancestor) (not include-descendants))
          (dolist (node nodes)
            (setf (org-tasktree-model-node-parent-id node) nil)))
        (org-tasktree-query--preorder-sort nodes)))))

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
