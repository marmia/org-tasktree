;;; org-tasktree-db.el --- Org Tasktree DB -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; Low-level SQLite helpers: connection lifecycle, schema creation,
;; metadata, and Inbox bootstrapping.  This module is idempotent and
;; safe to call multiple times.
;;

;;; Code:

(require 'sqlite)
(require 'subr-x)
(require 'seq)
(require 'org-id)
(require 'org-tasktree-model)

(defvar org-tasktree-database-location)

(defconst org-tasktree-db--schema-version
  "1"
  "Schema version string for org-tasktree database.")

(defconst org-tasktree-db--inbox-id
  1
  "Fixed id for the system Inbox project.")

(defconst org-tasktree-db--inbox-uid
  "00000000-0000-0000-0000-000000000001"
  "Fixed UID for the system Inbox project.")

(defconst org-tasktree-db--sqlite-foreign-keys-pragma
  "PRAGMA foreign_keys = ON;"
  "PRAGMA to enable SQLite foreign key constraints.")

(defun org-tasktree-db--normalize-path (path)
  "Return normalized absolute PATH."
  (expand-file-name path))

(defun org-tasktree-db--database-path ()
  "Return database file path from customization."
  (org-tasktree-db--normalize-path org-tasktree-database-location))

(defun org-tasktree-db--ensure-parent-dir (path)
  "Ensure parent directory for PATH exists."
  (let ((dir (file-name-directory
              (org-tasktree-db--normalize-path path))))
    (unless (and dir (file-directory-p dir))
      (make-directory dir t))))

(defun org-tasktree-db--open ()
  "Open and return a sqlite connection.

The caller must close the returned connection using
`org-tasktree-db--close`."
  (let ((db (sqlite-open (org-tasktree-db--database-path))))
    (sqlite-execute db org-tasktree-db--sqlite-foreign-keys-pragma)
    db))

(defun org-tasktree-db--close (db)
  "Close sqlite connection DB."
  (when db
    (sqlite-close db)))

(defmacro org-tasktree-db--with-db (db-var &rest body)
  "Evaluate BODY with DB-VAR bound to an open sqlite connection."
  (declare (indent 1))
  `(let ((,db-var (org-tasktree-db--open)))
     (unwind-protect
         (progn ,@body)
       (org-tasktree-db--close ,db-var))))

(defmacro org-tasktree-db--with-transaction (db &rest body)
  "Execute BODY in a transaction on DB connection DB."
  (declare (indent 1))
  `(progn
     (sqlite-execute ,db "BEGIN;")
     (condition-case err
         (prog1 (progn ,@body)
           (sqlite-execute ,db "COMMIT;"))
       (error
        (ignore-errors (sqlite-execute ,db "ROLLBACK;"))
        (signal (car err) (cdr err))))))

(defun org-tasktree-db--row-nth (row index)
  "Return ROW element at INDEX supporting vectors or lists."
  (if (vectorp row) (aref row index) (nth index row)))

(defun org-tasktree-db--uid-exists-p (db uid)
  "Return non-nil if UID already exists in nodes table on DB."
  (let ((rows (sqlite-select
               db
               "SELECT 1 FROM nodes WHERE uid = ? LIMIT 1;"
               (vector uid))))
    (and rows t)))

(defun org-tasktree-db-generate-uid ()
  "Generate a UID unique within the current database."
  (org-tasktree-db--with-db db
    (let ((attempts 0)
          uid)
      (while (or (null uid)
                 (org-tasktree-db--uid-exists-p db uid))
        (setq uid (org-id-new))
        (setq attempts (1+ attempts))
        (when (> attempts 8)
          (sleep-for 0.05)))
      uid)))

(defun org-tasktree-db--sql-in (count)
  "Return parameter placeholder string for COUNT items."
  (concat "(" (string-join (make-list count "?") ",") ")"))

(defun org-tasktree-db--existing-cache (db uids)
  "Return hash from UID to existing fields for UIDS on DB."
  (let ((cache (make-hash-table :test 'equal)))
    (when uids
      (let* ((placeholder (org-tasktree-db--sql-in (length uids)))
             (sql (format
                   (mapconcat
                    #'identity
                    '("SELECT uid, id, parent_id,"
                      " project_id, phase_id, created_at"
                      " FROM nodes WHERE uid IN %s;")
                    "")
                   placeholder))
             (rows (sqlite-select db sql (apply #'vector uids))))
        (dolist (row rows)
          (puthash (org-tasktree-db--row-nth row 0)
                   (list :id (org-tasktree-db--row-nth row 1)
                         :parent-id (org-tasktree-db--row-nth row 2)
                         :project-id (org-tasktree-db--row-nth row 3)
                         :phase-id (org-tasktree-db--row-nth row 4)
                         :created-at (org-tasktree-db--row-nth row 5))
                   cache))))
    cache))

(defun org-tasktree-db--last-rowid (db)
  "Return last inserted rowid on DB."
  (let ((rows (sqlite-select db "SELECT last_insert_rowid();")))
    (when (and rows (car rows))
      (org-tasktree-db--row-nth (car rows) 0))))

(defun org-tasktree-db--delete-scope (db uids)
  "Delete nodes and tags for UIDS on DB."
  (when uids
    (let* ((placeholder (org-tasktree-db--sql-in (length uids)))
           (vec (apply #'vector uids)))
      (sqlite-execute
       db
       (format
        (concat
         "DELETE FROM node_tags WHERE node_id IN ("
         "SELECT id FROM nodes WHERE uid IN %s);")
        placeholder)
       vec)
      (sqlite-execute
       db
       (format "DELETE FROM nodes WHERE uid IN %s;" placeholder)
       vec))))

(defun org-tasktree-db--sort-nodes (nodes)
  "Return NODES sorted by level then title for deterministic insert."
  (seq-sort
   (lambda (a b)
     (let ((la (org-tasktree-model-node-level a))
           (lb (org-tasktree-model-node-level b)))
       (if (= la lb)
           (string-lessp (org-tasktree-model-node-title a)
                         (org-tasktree-model-node-title b))
         (< la lb))))
   nodes))

(defun org-tasktree-db--prepare-nodes (nodes cache now)
  "Set created_at/updated_at on NODES using CACHE and NOW.
Return validated nodes list."
  (mapcar
   (lambda (node)
     (let* ((cached (gethash
                     (org-tasktree-model-node-uid node)
                     cache))
            (created (or (org-tasktree-model-node-created-at node)
                         (plist-get cached :created-at)
                         now)))
       (setf (org-tasktree-model-node-created-at node) created)
       (setf (org-tasktree-model-node-updated-at node) now)
       (org-tasktree-model-validate-node node)))
   nodes))

(defun org-tasktree-db--cache-id-map (cache)
  "Return hash of old id -> uid from CACHE."
  (let ((table (make-hash-table :test 'equal)))
    (maphash
     (lambda (uid plist)
       (let ((id (plist-get plist :id)))
         (when id (puthash id uid table))))
     cache)
    table))

(defun org-tasktree-db--remap-id (id id->uid uid->row)
  "Remap old numeric ID to new row id when present.
ID is the original parent/project/phase id.  ID->UID maps old ids to
uids within the current scope.  UID->ROW maps uids to freshly inserted
row ids.  Returns new row id or the original ID when not remapped."
  (when id
    (let* ((uid (gethash id id->uid))
           (new (and uid (gethash uid uid->row))))
      (or new id))))

(defun org-tasktree-db--insert-nodes (db nodes cache)
  "Insert NODES into DB along with normalized tags.
Remap parent/project/phase ids to freshly inserted row ids when their
uids are in the current scope."
  (let ((sql (mapconcat
              #'identity
              '("INSERT INTO nodes("
                "  uid, parent_id, node_type, todo_keyword, title,"
                "  level, priority, scheduled, deadline, closed_at,"
                "  tags, status, project_id, phase_id, created_at,"
                "  updated_at"
                ") VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?,"
                " ?, ?);")
              "\n"))
        (id->uid (org-tasktree-db--cache-id-map cache))
        (uid->row (make-hash-table :test 'equal)))
    (dolist (node (org-tasktree-db--sort-nodes nodes))
      (let* ((parent (org-tasktree-db--remap-id
                      (org-tasktree-model-node-parent-id node)
                      id->uid uid->row))
             (project (org-tasktree-db--remap-id
                       (org-tasktree-model-node-project-id node)
                       id->uid uid->row))
             (phase (org-tasktree-db--remap-id
                     (org-tasktree-model-node-phase-id node)
                     id->uid uid->row)))
        (setf (org-tasktree-model-node-parent-id node) parent)
        (setf (org-tasktree-model-node-project-id node) project)
        (setf (org-tasktree-model-node-phase-id node) phase)
        (sqlite-execute
         db sql (org-tasktree-model-node->db-vector node))
        (let ((row-id (org-tasktree-db--last-rowid db)))
          (puthash (org-tasktree-model-node-uid node) row-id uid->row)
          (dolist (tag (org-tasktree-model-node-tags-list node))
            (sqlite-execute
             db
             "INSERT INTO node_tags(node_id, tag) VALUES(?, ?);"
             (vector row-id tag))))))))

(defun org-tasktree-db-commit-nodes (nodes)
  "Replace DB rows for UIDS in NODES with fresh insertions.
Performs scoped DELETE/INSERT; preserves existing `created_at' for
existing UIDs and sets `updated_at' to current time."
  (org-tasktree-db--with-db db
    (org-tasktree-db--with-transaction db
      (let* ((uids (delq nil (mapcar #'org-tasktree-model-node-uid
                                     nodes)))
             (cache (org-tasktree-db--existing-cache db uids))
             (now (format-time-string "%FT%T%:z" (current-time)))
             (prepared (org-tasktree-db--prepare-nodes
                        nodes cache now)))
        (org-tasktree-db--delete-scope db uids)
        (org-tasktree-db--insert-nodes db prepared cache)))))

(defun org-tasktree-db--ensure-schema (db)
  "Ensure all tables and indexes exist in DB."
  (let ((stmts
         (list
          (concat
           "CREATE TABLE IF NOT EXISTS nodes ("
           "  id INTEGER PRIMARY KEY,"
           "  uid TEXT NOT NULL,"
           "  parent_id INTEGER,"
           "  node_type TEXT NOT NULL,"
           "  todo_keyword TEXT,"
           "  title TEXT NOT NULL,"
           "  level INTEGER NOT NULL,"
           "  priority TEXT,"
           "  scheduled TEXT,"
           "  deadline TEXT,"
           "  closed_at TEXT,"
           "  tags TEXT,"
           "  status TEXT NOT NULL,"
           "  project_id INTEGER,"
           "  phase_id INTEGER,"
           "  created_at TEXT NOT NULL,"
           "  updated_at TEXT NOT NULL"
           ");")
          (concat
           "CREATE TABLE IF NOT EXISTS node_tags ("
           "  node_id INTEGER NOT NULL,"
           "  tag TEXT NOT NULL,"
           "  PRIMARY KEY (node_id, tag),"
           "  FOREIGN KEY (node_id) REFERENCES nodes(id)"
           "    ON DELETE CASCADE"
           ");")
          (concat
           "CREATE TABLE IF NOT EXISTS meta ("
           "  key TEXT NOT NULL PRIMARY KEY,"
           "  value TEXT NOT NULL"
           ");")
          (concat
           "CREATE UNIQUE INDEX IF NOT EXISTS idx_nodes_uid "
           "ON nodes(uid);")
          (concat
           "CREATE UNIQUE INDEX IF NOT EXISTS "
           "idx_nodes_project_title "
           "ON nodes(title) WHERE node_type = 'project';")
          (concat
           "CREATE UNIQUE INDEX IF NOT EXISTS idx_nodes_phase_title "
           "ON nodes(project_id, title) WHERE node_type = 'phase';")
          (concat
           "CREATE UNIQUE INDEX IF NOT EXISTS idx_nodes_task_title "
           "ON nodes(parent_id, title) WHERE node_type = 'task';")
          (concat
           "CREATE INDEX IF NOT EXISTS idx_nodes_status_scheduled "
           "ON nodes(status, scheduled);")
          (concat
           "CREATE INDEX IF NOT EXISTS idx_nodes_status_deadline "
           "ON nodes(status, deadline);")
          (concat
           "CREATE INDEX IF NOT EXISTS idx_nodes_parent "
           "ON nodes(parent_id);")
          (concat
           "CREATE INDEX IF NOT EXISTS idx_node_tags_tag "
           "ON node_tags(tag, node_id);"))))
    (dolist (sql stmts)
      (sqlite-execute db sql))))

(defun org-tasktree-db--ensure-meta (db)
  "Ensure initial meta keys exist in DB."
  (let ((now (format-time-string "%FT%T%:z" (current-time))))
    (sqlite-execute
     db
     (concat
      "INSERT OR IGNORE INTO meta(key, value) "
      "VALUES('schema_version', ?);")
     (vector org-tasktree-db--schema-version))
    (sqlite-execute
     db
     (concat
      "INSERT OR IGNORE INTO meta(key, value) "
      "VALUES('created_at', ?);")
     (vector now))))

(defun org-tasktree-db--ensure-inbox (db)
  "Ensure the system Inbox project exists and is valid on DB."
  (let ((now (format-time-string "%FT%T%:z" (current-time))))
    (sqlite-execute
     db
     (mapconcat
      #'identity
      '("INSERT OR IGNORE INTO nodes("
        "  id, uid, parent_id, node_type, todo_keyword, title,"
        "  level, priority, scheduled, deadline, closed_at, tags,"
        "  status, project_id, phase_id, created_at,"
        "  updated_at"
        ") VALUES(?, ?, NULL, 'project', 'PROJ', 'inbox', 1,"
        "  NULL, NULL, NULL, NULL, NULL, 'OPEN',"
        "  NULL, NULL, ?, ?);")
      "\n")
     (vector org-tasktree-db--inbox-id
             org-tasktree-db--inbox-uid
             now
             now))
    (let ((rows (sqlite-select
                 db
                 (concat
                  "SELECT id, uid, node_type, title, status "
                  "FROM nodes WHERE id = ?;")
                 (vector org-tasktree-db--inbox-id))))
      (unless rows
        (error "Failed to create inbox project"))
      (let* ((row (car rows))
             (id (if (vectorp row) (aref row 0) (nth 0 row)))
             (uid (if (vectorp row) (aref row 1) (nth 1 row)))
             (node-type (if (vectorp row) (aref row 2) (nth 2 row)))
             (title (if (vectorp row) (aref row 3) (nth 3 row)))
             (status (if (vectorp row) (aref row 4) (nth 4 row))))
        (unless (and (equal id org-tasktree-db--inbox-id)
                     (equal uid org-tasktree-db--inbox-uid)
                     (equal node-type "project")
                     (equal title "inbox")
                     (equal status "OPEN"))
          (error "Inbox project is invalid or corrupted"))))))

(defun org-tasktree-db-init ()
  "Initialize database file, schema, meta, and Inbox.

This function is idempotent."
  (org-tasktree-db--ensure-parent-dir
   (org-tasktree-db--database-path))
  (org-tasktree-db--with-db db
    (org-tasktree-db--with-transaction db
      (org-tasktree-db--ensure-schema db)
      (org-tasktree-db--ensure-meta db)
      (org-tasktree-db--ensure-inbox db))))

(provide 'org-tasktree-db)
;;; org-tasktree-db.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
