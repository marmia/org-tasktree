;;; org-tasktree-init-ert.el --- ERT tests for org-tasktree-init -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; ERT tests covering the main use cases for `org-tasktree-init'.
;; The tests focus on database initialization and idempotency.
;;

;;; Code:

(require 'ert)
(require 'sqlite)
(require 'seq)
(require 'org-tasktree)
(require 'org-tasktree-db)
(require 'org-tasktree-test-helper)

(defun org-tasktree-init-ert--row-nth (row index)
  "Return ROW element at INDEX for lists or vectors."
  (if (vectorp row) (aref row index) (nth index row)))

(defun org-tasktree-init-ert--table-row-count (table)
  "Return row count for TABLE."
  (org-tasktree-db--with-db db
    (org-tasktree-init-ert--row-nth
     (car (sqlite-select
           db
           (format "SELECT COUNT(*) FROM %s;" table)))
     0)))

(defun org-tasktree-init-ert--table-columns (table)
  "Return column names for TABLE."
  (org-tasktree-db--with-db db
    (mapcar
     (lambda (row) (org-tasktree-init-ert--row-nth row 1))
     (sqlite-select db (format "PRAGMA table_info(%s);" table)))))

(defun org-tasktree-init-ert--user-version ()
  "Return PRAGMA user_version value."
  (org-tasktree-db--with-db db
    (org-tasktree-init-ert--row-nth
     (car (sqlite-select db "PRAGMA user_version;"))
     0)))

(defun org-tasktree-init-ert--table-exists-p (name)
  "Return non-nil when table NAME exists."
  (org-tasktree-db--with-db db
    (seq-contains-p
     (mapcar (lambda (row) (org-tasktree-init-ert--row-nth row 0))
             (sqlite-select
              db
              "SELECT name FROM sqlite_master WHERE type='table';"))
     name)))

(ert-deftest org-tasktree-init-ert-normal-create-db ()
  "Normal case: init creates DB/queries and schema tables."
  (org-tasktree-test-helper-reset-db)
  (should (file-exists-p org-tasktree-database-location))
  (should (file-directory-p org-tasktree-query-dir))
  (should (org-tasktree-init-ert--table-exists-p "nodes"))
  (should (org-tasktree-init-ert--table-exists-p "node_tags"))
  (should-not (org-tasktree-init-ert--table-exists-p "meta"))
  (should (= 0 (org-tasktree-init-ert--table-row-count "nodes")))
  (should (= 0 (org-tasktree-init-ert--table-row-count "node_tags")))
  (should (= 2 (org-tasktree-init-ert--user-version)))
  (should
   (equal
    '("id" "uid" "parent_id" "todo_keyword" "title" "priority"
      "scheduled" "deadline" "repeat" "closed_at" "tags" "content"
      "status" "created_at" "updated_at")
    (org-tasktree-init-ert--table-columns "nodes"))))

(ert-deftest org-tasktree-init-ert-normal-idempotent ()
  "Normal case: re-running init keeps schema and data intact."
  (org-tasktree-test-helper-reset-db)
  (let ((before-version (org-tasktree-init-ert--user-version))
        (before-count (org-tasktree-init-ert--table-row-count "nodes")))
    (org-tasktree-init)
    (let ((after-version (org-tasktree-init-ert--user-version))
          (after-count (org-tasktree-init-ert--table-row-count "nodes")))
      (should (= before-version after-version))
      (should (= before-count after-count)))))

(ert-deftest org-tasktree-init-ert-abnormal-invalid-db-path ()
  "Abnormal case: init fails with an invalid database path."
  (let ((org-tasktree-database-location "/aaa.db")
        (org-tasktree-query-dir (expand-file-name "test/queries"
                                                  default-directory)))
    (should-error (org-tasktree-init))))

(ert-deftest org-tasktree-init-ert-abnormal-nil-db-path ()
  "Abnormal case: init fails when database path is nil."
  (let ((org-tasktree-database-location nil)
        (org-tasktree-query-dir (expand-file-name "test/queries"
                                                  default-directory)))
    (should-error (org-tasktree-init))))

(ert-deftest org-tasktree-init-ert-abnormal-invalid-query-dir ()
  "Abnormal case: init fails with an invalid query directory."
  (let ((org-tasktree-database-location (expand-file-name "test/tasktree.db"
                                                          default-directory))
        (org-tasktree-query-dir "/aaa"))
    (should-error (org-tasktree-init))))

(ert-deftest org-tasktree-init-ert-abnormal-nil-query-dir ()
  "Abnormal case: init fails when query directory is nil."
  (let ((org-tasktree-database-location (expand-file-name "test/tasktree.db"
                                                          default-directory))
        (org-tasktree-query-dir nil))
    (should-error (org-tasktree-init))))

(provide 'org-tasktree-init-ert)
;;; org-tasktree-init-ert.el ends here
