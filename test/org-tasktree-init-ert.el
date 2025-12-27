;;; org-tasktree-init-ert.el --- ERT tests for org-tasktree-init -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; ERT tests covering the main use cases for `org-tasktree-init'.
;; The tests focus on database initialization, inbox creation, and idempotency.
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

(defun org-tasktree-init-ert--select-inbox-row ()
  "Return the inbox row from the nodes table."
  (org-tasktree-db--with-db db
    (car (sqlite-select
          db
          (string-join
           '("SELECT id, uid, parent_id, node_type, todo_keyword, title,"
             " status, project_id, phase_id"
             " FROM nodes WHERE id = 1 LIMIT 1;")
           "")))))

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
  "Normal case: init creates DB/queries, schema tables, and inbox row."
  (org-tasktree-test-helper-reset-db)
  (should (file-exists-p org-tasktree-database-location))
  (should (file-directory-p org-tasktree-query-dir))
  (should (org-tasktree-init-ert--table-exists-p "nodes"))
  (should (org-tasktree-init-ert--table-exists-p "node_tags"))
  (should (org-tasktree-init-ert--table-exists-p "meta"))
  (let ((row (org-tasktree-init-ert--select-inbox-row)))
    (should row)
    (should (= 1 (org-tasktree-init-ert--row-nth row 0)))
    (should (equal "00000000-0000-0000-0000-000000000001"
                   (org-tasktree-init-ert--row-nth row 1)))
    (should (null (org-tasktree-init-ert--row-nth row 2)))
    (should (equal "project" (org-tasktree-init-ert--row-nth row 3)))
    (should (equal "PROJ" (org-tasktree-init-ert--row-nth row 4)))
    (should (equal "inbox" (org-tasktree-init-ert--row-nth row 5)))
    (should (equal "OPEN" (org-tasktree-init-ert--row-nth row 6)))
    (should (null (org-tasktree-init-ert--row-nth row 7)))
    (should (null (org-tasktree-init-ert--row-nth row 8)))))

(ert-deftest org-tasktree-init-ert-normal-idempotent ()
  "Normal case: re-running init keeps the inbox row intact."
  (org-tasktree-test-helper-reset-db)
  (let ((before (org-tasktree-init-ert--select-inbox-row)))
    (org-tasktree-init)
    (let ((after (org-tasktree-init-ert--select-inbox-row)))
      (should (equal before after)))))

(ert-deftest org-tasktree-init-ert-normal-default-project-name ()
  "Normal case: init uses `org-tasktree-default-project-name' for inbox title."
  (let ((org-tasktree-default-project-name "test"))
    (org-tasktree-test-helper-reset-db)
    (let ((row (org-tasktree-init-ert--select-inbox-row)))
      (should row)
      (should (equal "test" (org-tasktree-init-ert--row-nth row 5))))))

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
