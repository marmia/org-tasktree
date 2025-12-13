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
