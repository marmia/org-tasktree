;;; org-tasktree-sync-ert.el --- ERT helpers for org-tasktree sync -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Helper utilities for org-tasktree sync ERT tests.
;; Provides deterministic buffer setup and DB query helpers.
;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'sqlite)
(require 'subr-x)
(require 'org-tasktree-db)
(require 'org-tasktree-model)
(require 'org-tasktree-sync)
(require 'org-tasktree-test-helper)

(defvar repo-root)

(defun org-tasktree-sync-ert--repo-root ()
  "Return repo root for test run."
  (if (and (boundp 'repo-root) (stringp repo-root))
      repo-root
    default-directory))

(defun org-tasktree-sync-ert--test-data-path (name)
  "Return absolute path for test data file NAME."
  (expand-file-name (concat "test/test-data/" name)
                    (org-tasktree-sync-ert--repo-root)))

(defun org-tasktree-sync-ert--with-org-buffer (file fn)
  "Insert FILE into a temporary org buffer and call FN."
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (insert-file-contents (org-tasktree-sync-ert--test-data-path file))
      (org-mode)
      (org-set-regexps-and-options)
      (goto-char (point-min))
      (funcall fn))))

(defun org-tasktree-sync-ert--sync-file (file)
  "Reset DB and sync org data from FILE."
  (org-tasktree-test-helper-reset-db)
  (org-tasktree-sync-ert--with-org-buffer
   file
   (lambda ()
     (org-tasktree-sync-buffer))))

(defun org-tasktree-sync-ert--fetch-node-by-title (title &optional parent-id)
  "Return node for TITLE and optional PARENT-ID.
Signals an error when multiple rows match."
  (org-tasktree-db--with-db db
    (let* ((sql (if parent-id
                    (string-join
                     '("SELECT id, uid, parent_id, todo_keyword, title,"
                       " priority, scheduled, deadline, repeat, closed_at,"
                       " tags, content, status, created_at, updated_at"
                       " FROM nodes WHERE title = ? AND parent_id = ? LIMIT 2;")
                     "")
                  (string-join
                   '("SELECT id, uid, parent_id, todo_keyword, title,"
                     " priority, scheduled, deadline, repeat, closed_at,"
                     " tags, content, status, created_at, updated_at"
                     " FROM nodes WHERE title = ? LIMIT 2;")
                   "")))
           (params (if parent-id
                       (vector title parent-id)
                     (vector title)))
           (rows (sqlite-select db sql params)))
      (when (> (length rows) 1)
        (error "Multiple nodes match title: %s" title))
      (when rows
        (org-tasktree-model-node-from-db-row (car rows))))))

(defun org-tasktree-sync-ert--fetch-node-by-uid (uid)
  "Return node for UID or nil."
  (org-tasktree-db--with-db db
    (let ((row (car (sqlite-select
                     db
                     (string-join
                      '("SELECT id, uid, parent_id, todo_keyword, title,"
                        " priority, scheduled, deadline, repeat, closed_at,"
                        " tags, content, status, created_at, updated_at"
                        " FROM nodes WHERE uid = ? LIMIT 1;")
                      "")
                     (vector uid)))))
      (when row
        (org-tasktree-model-node-from-db-row row)))))

(defun org-tasktree-sync-ert--node-count ()
  "Return total node count in the database."
  (org-tasktree-db--with-db db
    (let ((row (car (sqlite-select db "SELECT count(*) FROM nodes;"))))
      (when row
        (if (vectorp row) (aref row 0) (car row))))))

(defun org-tasktree-sync-ert--fetch-node-tags (node-id)
  "Return sorted tag list for NODE-ID."
  (org-tasktree-db--with-db db
    (let ((rows (sqlite-select
                 db
                 "SELECT tag FROM node_tags WHERE node_id = ?;"
                 (vector node-id))))
      (sort (mapcar (lambda (row)
                      (if (vectorp row) (aref row 0) (car row)))
                    rows)
            #'string<))))

(defun org-tasktree-sync-ert--insert-node (node)
  "Insert NODE and return the saved node."
  (org-tasktree-db-commit-nodes (list node))
  (org-tasktree-sync-ert--fetch-node-by-uid
   (org-tasktree-model-node-uid node)))

(defun org-tasktree-sync-ert--seed-update-tree ()
  "Reset DB and insert baseline update tree.
Returns plist with :aaa :bbb :ccc :ddd :eee :fff :ggg :hhh nodes."
  (org-tasktree-test-helper-reset-db)
  (let* ((aaa (org-tasktree-model-node-create
               :uid "82a4e7b7-207f-5583-8e8c-47503339b07b"
               :todo-keyword nil
               :title "AAA (before update)"
               :priority nil
               :scheduled nil
               :deadline nil
               :repeat nil
               :tags nil
               :content "AAA before update."
               :status "OPEN"
               :parent-id nil))
         (aaa-node (org-tasktree-sync-ert--insert-node aaa))
         (aaa-id (org-tasktree-model-node-id aaa-node))
         (bbb (org-tasktree-model-node-create
               :uid "c01ef21b-3bda-5a2b-9179-20fc145215e9"
               :todo-keyword nil
               :title "BBB (before update)"
               :priority nil
               :scheduled nil
               :deadline nil
               :repeat nil
               :tags nil
               :content "BBB before update."
               :status "OPEN"
               :parent-id aaa-id))
         (bbb-node (org-tasktree-sync-ert--insert-node bbb))
         (bbb-id (org-tasktree-model-node-id bbb-node))
         (ccc (org-tasktree-model-node-create
               :uid "69f0ecdd-d8ee-5970-81b2-4edf5e985240"
               :todo-keyword nil
               :title "CCC (before update)"
               :priority nil
               :scheduled nil
               :deadline nil
               :repeat nil
               :tags nil
               :content "CCC before update."
               :status "OPEN"
               :parent-id bbb-id))
         (ccc-node (org-tasktree-sync-ert--insert-node ccc))
         (ccc-id (org-tasktree-model-node-id ccc-node))
         (ddd (org-tasktree-model-node-create
               :uid "1ccee950-21b1-5ec2-bac7-384c1c9cae6f"
               :todo-keyword "TODO"
               :title "DDD (before update)"
               :priority "C"
               :scheduled "2026-01-10"
               :deadline "2026-01-20"
               :repeat nil
               :tags ":before:ddd:"
               :content "DDD before update."
               :status "OPEN"
               :parent-id ccc-id))
         (ddd-node (org-tasktree-sync-ert--insert-node ddd))
         (ddd-id (org-tasktree-model-node-id ddd-node))
         (eee (org-tasktree-model-node-create
               :uid "7e84806d-9bdc-584c-a678-88140ad824b0"
               :todo-keyword nil
               :title "EEE (before update)"
               :priority nil
               :scheduled nil
               :deadline nil
               :repeat nil
               :tags nil
               :content "EEE before update."
               :status "OPEN"
               :parent-id ddd-id))
         (eee-node (org-tasktree-sync-ert--insert-node eee))
         (fff (org-tasktree-model-node-create
               :uid "ca706ec4-dc8e-568c-a157-012e585b741d"
               :todo-keyword nil
               :title "FFF (before update)"
               :priority nil
               :scheduled nil
               :deadline nil
               :repeat nil
               :tags nil
               :content "FFF before update."
               :status "OPEN"
               :parent-id nil))
         (fff-node (org-tasktree-sync-ert--insert-node fff))
         (fff-id (org-tasktree-model-node-id fff-node))
         (ggg (org-tasktree-model-node-create
               :uid "608a8d33-54fe-55dd-b877-a4944b2be2ed"
               :todo-keyword nil
               :title "GGG (before update)"
               :priority nil
               :scheduled nil
               :deadline nil
               :repeat nil
               :tags nil
               :content "GGG before update."
               :status "OPEN"
               :parent-id fff-id))
         (ggg-node (org-tasktree-sync-ert--insert-node ggg))
         (ggg-id (org-tasktree-model-node-id ggg-node))
         (hhh (org-tasktree-model-node-create
               :uid "1438dc0e-da7d-5a82-9233-59d1ac453018"
               :todo-keyword nil
               :title "HHH (before update)"
               :priority nil
               :scheduled nil
               :deadline nil
               :repeat nil
               :tags nil
               :content "HHH before update."
               :status "OPEN"
               :parent-id ggg-id))
         (hhh-node (org-tasktree-sync-ert--insert-node hhh)))
    (list :aaa aaa-node
          :bbb bbb-node
          :ccc ccc-node
          :ddd ddd-node
          :eee eee-node
          :fff fff-node
          :ggg ggg-node
          :hhh hhh-node)))

(defun org-tasktree-sync-ert--sync-file-without-reset (file)
  "Sync org data from FILE without resetting DB."
  (org-tasktree-sync-ert--with-org-buffer
   file
   (lambda ()
     (org-tasktree-sync-buffer))))

(defun org-tasktree-sync-ert--assert-nonempty-string (value label)
  "Assert VALUE is a non-empty string for LABEL."
  (should (stringp value))
  (should (not (string-empty-p value)))
  (should (stringp label)))

(cl-defun org-tasktree-sync-ert--assert-node
    (node &key title todo-keyword priority scheduled deadline repeat tags
          content status parent-id expect-nil)
  "Assert NODE fields against expected values.
Optional keyword arguments include TITLE, TODO-KEYWORD, PRIORITY,
SCHEDULED, DEADLINE, REPEAT, TAGS, CONTENT, STATUS, PARENT-ID, and
EXPECT-NIL."
  (should (org-tasktree-model-node-p node))
  (org-tasktree-sync-ert--assert-nonempty-string
   (org-tasktree-model-node-uid node)
   "uid")
  (when title
    (should (equal title (org-tasktree-model-node-title node))))
  (when todo-keyword
    (should (equal todo-keyword (org-tasktree-model-node-todo-keyword node))))
  (when priority
    (should (equal priority (org-tasktree-model-node-priority node))))
  (when scheduled
    (should (equal scheduled (org-tasktree-model-node-scheduled node))))
  (when deadline
    (should (equal deadline (org-tasktree-model-node-deadline node))))
  (when repeat
    (should (equal repeat (org-tasktree-model-node-repeat node))))
  (when tags
    (should (equal tags (org-tasktree-model-node-tags node))))
  (when content
    (should (equal content (org-tasktree-model-node-content node))))
  (when status
    (should (equal status (org-tasktree-model-node-status node))))
  (when parent-id
    (should (equal parent-id (org-tasktree-model-node-parent-id node))))
  (dolist (field expect-nil)
    (pcase field
      ((or :priority 'priority)
       (should (null (org-tasktree-model-node-priority node))))
      ((or :scheduled 'scheduled)
       (should (null (org-tasktree-model-node-scheduled node))))
      ((or :deadline 'deadline)
       (should (null (org-tasktree-model-node-deadline node))))
      ((or :repeat 'repeat)
       (should (null (org-tasktree-model-node-repeat node))))
      ((or :tags 'tags)
       (should (null (org-tasktree-model-node-tags node))))
      ((or :content 'content)
       (should (null (org-tasktree-model-node-content node))))
      ((or :parent-id 'parent-id)
       (should (null (org-tasktree-model-node-parent-id node))))
      ((or :todo-keyword 'todo-keyword)
       (should (null (org-tasktree-model-node-todo-keyword node))))
      (field
       (error "Unknown expected nil field: %S" field)))))

(provide 'org-tasktree-sync-ert)
;;; org-tasktree-sync-ert.el ends here
