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

(defconst org-tasktree-sync-ert--uid-aaa
  "82a4e7b7-207f-5583-8e8c-47503339b07b")
(defconst org-tasktree-sync-ert--uid-bbb
  "c01ef21b-3bda-5a2b-9179-20fc145215e9")
(defconst org-tasktree-sync-ert--uid-ccc
  "69f0ecdd-d8ee-5970-81b2-4edf5e985240")
(defconst org-tasktree-sync-ert--uid-ddd
  "1ccee950-21b1-5ec2-bac7-384c1c9cae6f")
(defconst org-tasktree-sync-ert--uid-eee
  "7e84806d-9bdc-584c-a678-88140ad824b0")
(defconst org-tasktree-sync-ert--uid-fff
  "ca706ec4-dc8e-568c-a157-012e585b741d")
(defconst org-tasktree-sync-ert--uid-ggg
  "608a8d33-54fe-55dd-b877-a4944b2be2ed")
(defconst org-tasktree-sync-ert--uid-hhh
  "1438dc0e-da7d-5a82-9233-59d1ac453018")

(defconst org-tasktree-sync-ert--update-seed-spec
  (list
   (list :key :aaa
         :uid org-tasktree-sync-ert--uid-aaa
         :title "AAA (before update)"
         :content "AAA before update.")
   (list :key :bbb
         :uid org-tasktree-sync-ert--uid-bbb
         :title "BBB (before update)"
         :content "BBB before update."
         :parent :aaa)
   (list :key :ccc
         :uid org-tasktree-sync-ert--uid-ccc
         :title "CCC (before update)"
         :content "CCC before update."
         :parent :bbb)
   (list :key :ddd
         :uid org-tasktree-sync-ert--uid-ddd
         :todo-keyword "TODO"
         :title "DDD (before update)"
         :priority "C"
         :scheduled "2026-01-10"
         :deadline "2026-01-20"
         :tags ":before:ddd:"
         :content "DDD before update."
         :parent :ccc)
   (list :key :eee
         :uid org-tasktree-sync-ert--uid-eee
         :title "EEE (before update)"
         :content "EEE before update."
         :parent :ddd)
   (list :key :fff
         :uid org-tasktree-sync-ert--uid-fff
         :title "FFF (before update)"
         :content "FFF before update.")
   (list :key :ggg
         :uid org-tasktree-sync-ert--uid-ggg
         :title "GGG (before update)"
         :content "GGG before update."
         :parent :fff)
   (list :key :hhh
         :uid org-tasktree-sync-ert--uid-hhh
         :title "HHH (before update)"
         :content "HHH before update."
         :parent :ggg))
  "Seed data spec for update tests.")

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

(defun org-tasktree-sync-ert--tags-string (tags)
  "Return normalized tag string for TAGS list."
  (org-tasktree-model-tags->org-string tags))

(defun org-tasktree-sync-ert--assert-node-tags (node expected-tags)
  "Assert NODE has EXPECTED-TAGS in node_tags."
  (let* ((node-id (org-tasktree-model-node-id node))
         (tags (org-tasktree-sync-ert--fetch-node-tags node-id))
         (expected (sort (or expected-tags '()) #'string<)))
    (should (equal expected tags))))

(defun org-tasktree-sync-ert--insert-node (node)
  "Insert NODE and return the saved node."
  (org-tasktree-db-commit-nodes (list node))
  (org-tasktree-sync-ert--fetch-node-by-uid
   (org-tasktree-model-node-uid node)))

(defun org-tasktree-sync-ert--seed-update-tree ()
  "Reset DB and insert baseline update tree.
Returns plist with :aaa :bbb :ccc :ddd :eee :fff :ggg :hhh nodes."
  (org-tasktree-test-helper-reset-db)
  (let ((nodes (make-hash-table :test 'eq)))
    (dolist (spec org-tasktree-sync-ert--update-seed-spec)
      (let* ((key (plist-get spec :key))
             (parent-key (plist-get spec :parent))
             (parent-node (and parent-key (gethash parent-key nodes)))
             (parent-id (and parent-node (org-tasktree-model-node-id parent-node)))
             (node (org-tasktree-model-node-create
                    :uid (plist-get spec :uid)
                    :todo-keyword (plist-get spec :todo-keyword)
                    :title (plist-get spec :title)
                    :priority (plist-get spec :priority)
                    :scheduled (plist-get spec :scheduled)
                    :deadline (plist-get spec :deadline)
                    :repeat (plist-get spec :repeat)
                    :tags (plist-get spec :tags)
                    :content (plist-get spec :content)
                    :status (or (plist-get spec :status) "OPEN")
                    :parent-id parent-id))
             (saved (org-tasktree-sync-ert--insert-node node)))
        (puthash key saved nodes)))
    (let (result)
      (dolist (spec org-tasktree-sync-ert--update-seed-spec)
        (let ((key (plist-get spec :key)))
          (setq result (plist-put result key (gethash key nodes)))))
      result)))

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
