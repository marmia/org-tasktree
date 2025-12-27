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
    (let ((org-todo-keywords '((sequence "PROJ" "PHASE" "TODO" "|" "DONE"))))
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

(defun org-tasktree-sync-ert--fetch-node (title node-type)
  "Return node for TITLE and NODE-TYPE or nil."
  (org-tasktree-db--with-db db
    (let ((row (car (sqlite-select
                     db
                     (string-join
                      '("SELECT id, uid, parent_id, node_type, todo_keyword,"
                        " title, level, priority, scheduled, deadline, repeat,"
                        " closed_at, tags, content, status, project_id,"
                        " phase_id, created_at, updated_at"
                        " FROM nodes WHERE title = ? AND node_type = ? LIMIT 1;")
                      "")
                     (vector title node-type)))))
      (when row
        (org-tasktree-model-node-from-db-row row)))))

(defun org-tasktree-sync-ert--fetch-node-by-uid (uid)
  "Return node for UID or nil."
  (org-tasktree-db--with-db db
    (let ((row (car (sqlite-select
                     db
                     (string-join
                      '("SELECT id, uid, parent_id, node_type, todo_keyword,"
                        " title, level, priority, scheduled, deadline, repeat,"
                        " closed_at, tags, content, status, project_id,"
                        " phase_id, created_at, updated_at"
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
Returns plist with :project, :phase, :group, and :task nodes."
  (org-tasktree-test-helper-reset-db)
  (let* ((project (org-tasktree-model-node-create
                   :uid "00000000-0000-0000-0000-upd000000001"
                   :node-type "project"
                   :todo-keyword "PROJ"
                   :title "proj1 (before update)"
                   :level 1
                   :priority "A"
                   :scheduled "2026-01-10"
                   :deadline "2026-01-20"
                   :repeat nil
                   :tags ":unit-test:upd:project:"
                   :content "This is a project node."
                   :status "OPEN"
                   :parent-id nil
                   :project-id nil
                   :phase-id nil))
         (phase (org-tasktree-model-node-create
                 :uid "00000000-0000-0000-0000-upd000000002"
                 :node-type "phase"
                 :todo-keyword "PHASE"
                 :title "phase (before update)"
                 :level 2
                 :priority "A"
                 :scheduled "2026-01-10"
                 :deadline "2026-01-20"
                 :repeat nil
                 :tags ":unit-test:upd:phase:"
                 :content "This is a phase node."
                 :status "OPEN"
                 :parent-id :keep
                 :project-id :keep
                 :phase-id nil))
         (group (org-tasktree-model-node-create
                 :uid "00000000-0000-0000-0000-upd000000003"
                 :node-type "group"
                 :todo-keyword nil
                 :title "group (before update)"
                 :level 3
                 :priority "A"
                 :scheduled "2026-01-10"
                 :deadline "2026-01-20"
                 :repeat nil
                 :tags ":unit-test:upd:group:"
                 :content "This is a group node."
                 :status "OPEN"
                 :parent-id :keep
                 :project-id :keep
                 :phase-id :keep))
         (task (org-tasktree-model-node-create
                :uid "00000000-0000-0000-0000-upd000000004"
                :node-type "task"
                :todo-keyword "TODO"
                :title "task (before update)"
                :level 4
                :priority "A"
                :scheduled "2026-01-10"
                :deadline "2026-01-20"
                :repeat "+1d"
                :tags ":unit-test:upd:task:"
                :content "This is a task node."
                :status "OPEN"
                :parent-id :keep
                :project-id :keep
                :phase-id :keep)))
    (let* ((project-node (org-tasktree-sync-ert--insert-node project))
           (project-id (org-tasktree-model-node-id project-node)))
      (setf (org-tasktree-model-node-parent-id phase) project-id)
      (setf (org-tasktree-model-node-project-id phase) project-id)
      (let* ((phase-node (org-tasktree-sync-ert--insert-node phase))
             (phase-id (org-tasktree-model-node-id phase-node)))
        (setf (org-tasktree-model-node-parent-id group) phase-id)
        (setf (org-tasktree-model-node-project-id group) project-id)
        (setf (org-tasktree-model-node-phase-id group) phase-id)
        (let* ((group-node (org-tasktree-sync-ert--insert-node group))
               (group-id (org-tasktree-model-node-id group-node)))
          (setf (org-tasktree-model-node-parent-id task) group-id)
          (setf (org-tasktree-model-node-project-id task) project-id)
          (setf (org-tasktree-model-node-phase-id task) phase-id)
          (let ((task-node (org-tasktree-sync-ert--insert-node task)))
            (list :project project-node
                  :phase phase-node
                  :group group-node
                  :task task-node)))))))

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
    (node &key title node-type todo-keyword level priority scheduled deadline
          repeat tags content status parent-id project-id phase-id expect-nil)
  "Assert NODE fields against expected values.
Optional keyword arguments include TITLE, NODE-TYPE, TODO-KEYWORD, LEVEL,
PRIORITY, SCHEDULED, DEADLINE, REPEAT, TAGS, CONTENT, STATUS, PARENT-ID,
PROJECT-ID, PHASE-ID, and EXPECT-NIL."
  (should (org-tasktree-model-node-p node))
  (org-tasktree-sync-ert--assert-nonempty-string
   (org-tasktree-model-node-uid node)
   "uid")
  (when title
    (should (equal title (org-tasktree-model-node-title node))))
  (when node-type
    (should (equal node-type (org-tasktree-model-node-node-type node))))
  (when todo-keyword
    (should (equal todo-keyword (org-tasktree-model-node-todo-keyword node))))
  (when level
    (should (= level (org-tasktree-model-node-level node))))
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
  (when project-id
    (should (equal project-id (org-tasktree-model-node-project-id node))))
  (when phase-id
    (should (equal phase-id (org-tasktree-model-node-phase-id node))))
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
      ((or :project-id 'project-id)
       (should (null (org-tasktree-model-node-project-id node))))
      ((or :phase-id 'phase-id)
       (should (null (org-tasktree-model-node-phase-id node))))
      ((or :todo-keyword 'todo-keyword)
       (should (null (org-tasktree-model-node-todo-keyword node))))
      (field
       (error "Unknown expected nil field: %S" field)))))

(provide 'org-tasktree-sync-ert)
;;; org-tasktree-sync-ert.el ends here
