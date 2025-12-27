;;; org-tasktree-find-node-ert.el --- ERT helpers for org-tasktree-find-node -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Helper utilities and stubs for org-tasktree find-node ERT test.
;; This file provides deterministic test data builders and stub helpers
;; to isolate validation logic from UI interactions.
;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'sqlite)
(require 'org-tasktree)
(require 'org-tasktree-db)
(require 'org-tasktree-model)
(require 'org-tasktree-test-helper)
(require 'org-tasktree-ui)
(require 'org-tasktree-ui-minibuffer)

(defvar org-tasktree-find-node-ert--captured-cands nil
  "Holds completion candidates captured by stubbed `completing-read'.")

(defmacro org-tasktree-find-node-ert-with-completing-read (choice &rest body)
  "Run BODY with `completing-read' stubbed to return CHOICE.
Captured candidates are stored in `org-tasktree-find-node-ert--captured-cands'."
  (declare (indent 1))
  `(let (org-tasktree-find-node-ert--captured-cands)
     (cl-letf (((symbol-function 'org-tasktree-ui-minibuffer--completing-read)
                (lambda (_prompt cands &rest _args)
                  (setq org-tasktree-find-node-ert--captured-cands cands)
                  ,choice)))
       ,@body)))

(defmacro org-tasktree-find-node-ert-with-widget-values (values &rest body)
  "Run BODY with widget values stubbed by VALUES alist.
VALUES must be an alist of (KEY . VALUE) pairs."
  (declare (indent 1))
  `(let ((table (make-hash-table :test 'equal)))
     (dolist (pair ,values)
       (puthash (car pair) (cdr pair) table))
     (cl-letf (((symbol-function 'org-tasktree-ui--widget-value)
                (lambda (key) (gethash key table)))
               ((symbol-function 'org-tasktree-ui--widget-value-raw)
                (lambda (key) (gethash key table))))
       ,@body)))

(cl-defun org-tasktree-find-node-ert--make-node
    (&key uid node-type title level todo-keyword status parent-id
          project-id phase-id tags)
  "Return a basic node with required fields set.
UID is the node identifier, NODE-TYPE is the node type, TITLE is the name,
LEVEL is the outline depth, TODO-KEYWORD is the org keyword, STATUS is the
node status, PARENT-ID is the parent identifier, PROJECT-ID is the owning
project, PHASE-ID is the phase identifier when present, and TAGS are the org
tags."
  (org-tasktree-model-node-create
   :uid uid
   :node-type node-type
   :title title
   :level level
   :todo-keyword todo-keyword
   :status status
   :parent-id parent-id
   :project-id project-id
   :phase-id phase-id
   :tags tags))

(defun org-tasktree-find-node-ert--commit-nodes (nodes)
  "Commit NODES after resetting the DB."
  (org-tasktree-test-helper-reset-db)
  (org-tasktree-db-commit-nodes nodes))

(defun org-tasktree-find-node-ert--fetch-node-by-uid (uid)
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

(defun org-tasktree-find-node-ert--insert-node (node)
  "Insert NODE and return the saved node."
  (org-tasktree-db-commit-nodes (list node))
  (org-tasktree-find-node-ert--fetch-node-by-uid
   (org-tasktree-model-node-uid node)))

(defun org-tasktree-find-node-ert--seed-open-tree ()
  "Seed a sample open tree for find-node test."
  (let* ((project-uid "test-project-1")
         (phase-uid "test-phase-1")
         (group-uid "test-group-1")
         (task-uid "test-task-1")
         (child-uid "test-child-1")
         (project (org-tasktree-find-node-ert--make-node
                   :uid project-uid
                   :node-type "project"
                   :title "project1"
                   :level 1
                   :todo-keyword "PROJ"
                   :status "OPEN"
                   :parent-id nil
                   :project-id nil
                   :phase-id nil))
         (phase (org-tasktree-find-node-ert--make-node
                 :uid phase-uid
                 :node-type "phase"
                 :title "phase1"
                 :level 2
                 :todo-keyword "PHASE"
                 :status "OPEN"
                 :parent-id :keep
                 :project-id :keep
                 :phase-id nil))
         (group (org-tasktree-find-node-ert--make-node
                 :uid group-uid
                 :node-type "group"
                 :title "group1"
                 :level 3
                 :todo-keyword nil
                 :status "OPEN"
                 :parent-id :keep
                 :project-id :keep
                 :phase-id :keep))
         (task (org-tasktree-find-node-ert--make-node
                :uid task-uid
                :node-type "task"
                :title "task1"
                :level 4
                :todo-keyword "TODO"
                :status "OPEN"
                :parent-id :keep
                :project-id :keep
                :phase-id :keep
                :tags ":unit-test:task1:"))
         (child (org-tasktree-find-node-ert--make-node
                 :uid child-uid
                 :node-type "task"
                 :title "child1"
                 :level 5
                 :todo-keyword "TODO"
                 :status "OPEN"
                 :parent-id :keep
                 :project-id :keep
                 :phase-id :keep)))
    (org-tasktree-test-helper-reset-db)
    (let* ((project-node (org-tasktree-find-node-ert--insert-node project))
           (project-id (org-tasktree-model-node-id project-node)))
      (setf (org-tasktree-model-node-parent-id phase) project-id)
      (setf (org-tasktree-model-node-project-id phase) project-id)
      (let* ((phase-node (org-tasktree-find-node-ert--insert-node phase))
             (phase-id (org-tasktree-model-node-id phase-node)))
        (setf (org-tasktree-model-node-parent-id group) phase-id)
        (setf (org-tasktree-model-node-project-id group) project-id)
        (setf (org-tasktree-model-node-phase-id group) phase-id)
        (let* ((group-node (org-tasktree-find-node-ert--insert-node group))
               (group-id (org-tasktree-model-node-id group-node)))
          (setf (org-tasktree-model-node-parent-id task) group-id)
          (setf (org-tasktree-model-node-project-id task) project-id)
          (setf (org-tasktree-model-node-phase-id task) phase-id)
          (let* ((task-node (org-tasktree-find-node-ert--insert-node task))
                 (task-id (org-tasktree-model-node-id task-node)))
            (setf (org-tasktree-model-node-parent-id child) task-id)
            (setf (org-tasktree-model-node-project-id child) project-id)
            (setf (org-tasktree-model-node-phase-id child) phase-id)
            (org-tasktree-find-node-ert--insert-node child)))))))

(cl-defun org-tasktree-find-node-ert--meta-for-task
    (&key uid project-id phase-id parent-id)
  "Return META plist for task editing.
UID is the task identifier, PROJECT-ID is the owning project, PHASE-ID is
the phase identifier when present, and PARENT-ID is the parent identifier."
  (list :type 'task
        :uid uid
        :project-id project-id
        :phase-id phase-id
        :parent-id parent-id))

(cl-defun org-tasktree-find-node-ert--meta-for-phase
    (&key uid project-id project-title)
  "Return META plist for phase editing.
UID is the phase identifier, PROJECT-ID is the owning project, and
PROJECT-TITLE is the project title."
  (list :type 'phase
        :uid uid
        :project-id project-id
        :project-title project-title))

(cl-defun org-tasktree-find-node-ert--meta-for-group
    (&key uid project-id phase-id parent-id project-title phase-title)
  "Return META plist for group editing.
UID is the group identifier, PROJECT-ID is the owning project, PHASE-ID is
the phase identifier when present, PARENT-ID is the parent identifier,
PROJECT-TITLE is the project title, and PHASE-TITLE is the phase title."
  (list :type 'group
        :uid uid
        :project-id project-id
        :phase-id phase-id
        :parent-id parent-id
        :project-title project-title
        :phase-title phase-title))

(cl-defun org-tasktree-find-node-ert--meta-for-project (&key uid)
  "Return META plist for project editing.
UID is the project identifier."
  (list :type 'project
        :uid uid))

(provide 'org-tasktree-find-node-ert)
;;; org-tasktree-find-node-ert.el ends here
