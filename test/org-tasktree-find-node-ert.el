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
    (&key uid title todo-keyword status parent-id tags)
  "Return a basic node with required fields set.
UID is the node identifier, TITLE is the name, TODO-KEYWORD is the org
keyword, STATUS is the node status, PARENT-ID is the parent identifier,
and TAGS are the org tags."
  (org-tasktree-model-node-create
   :uid uid
   :title title
   :todo-keyword todo-keyword
   :status status
   :parent-id parent-id
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
                      '("SELECT id, uid, parent_id, todo_keyword, title,"
                        " priority, scheduled, deadline, repeat, closed_at,"
                        " tags, content, status, created_at, updated_at"
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
  (let* ((project (org-tasktree-find-node-ert--make-node
                   :uid "11111111-1111-4111-8111-111111111111"
                   :title "work"
                   :todo-keyword nil
                   :status "OPEN"
                   :parent-id nil
                   :tags ":project:"))
         (phase (org-tasktree-find-node-ert--make-node
                 :uid "22222222-2222-4222-8222-222222222222"
                 :title "design"
                 :todo-keyword nil
                 :status "OPEN"
                 :parent-id :keep
                 :tags ":phase:"))
         (group (org-tasktree-find-node-ert--make-node
                 :uid "33333333-3333-4333-8333-333333333333"
                 :title "alpha"
                 :todo-keyword nil
                 :status "OPEN"
                 :parent-id :keep
                 :tags ":group:"))
         (task (org-tasktree-find-node-ert--make-node
                :uid "44444444-4444-4444-8444-444444444444"
                :title "task1"
                :todo-keyword "TODO"
                :status "OPEN"
                :parent-id :keep
                :tags ":unit_test:task1:"))
         (child (org-tasktree-find-node-ert--make-node
                 :uid "55555555-5555-4555-8555-555555555555"
                 :title "child1"
                 :todo-keyword "TODO"
                 :status "OPEN"
                 :parent-id :keep
                 :tags nil))
         (caps-project (org-tasktree-find-node-ert--make-node
                        :uid "66666666-6666-4666-8666-666666666666"
                        :title "caps-project"
                        :todo-keyword nil
                        :status "OPEN"
                        :parent-id :keep
                        :tags ":Project:"))
         (mixed-phase-group (org-tasktree-find-node-ert--make-node
                             :uid "77777777-7777-4777-8777-777777777777"
                             :title "mixed-phase-group"
                             :todo-keyword nil
                             :status "OPEN"
                             :parent-id :keep
                             :tags ":phase:group:"))
         (mixed-project-group (org-tasktree-find-node-ert--make-node
                               :uid "88888888-8888-4888-8888-888888888888"
                               :title "mixed-project-group"
                               :todo-keyword nil
                               :status "OPEN"
                               :parent-id :keep
                               :tags ":project:group:")))
    (org-tasktree-test-helper-reset-db)
    (let* ((project-node (org-tasktree-find-node-ert--insert-node project))
           (project-id (org-tasktree-model-node-id project-node)))
      (setf (org-tasktree-model-node-parent-id phase) project-id)
      (let* ((phase-node (org-tasktree-find-node-ert--insert-node phase))
             (phase-id (org-tasktree-model-node-id phase-node)))
        (dolist (node (list caps-project mixed-phase-group mixed-project-group))
          (setf (org-tasktree-model-node-parent-id node) project-id)
          (org-tasktree-find-node-ert--insert-node node))
        (setf (org-tasktree-model-node-parent-id group) phase-id)
        (let* ((group-node (org-tasktree-find-node-ert--insert-node group))
               (group-id (org-tasktree-model-node-id group-node)))
          (setf (org-tasktree-model-node-parent-id task) group-id)
          (let* ((task-node (org-tasktree-find-node-ert--insert-node task))
                 (task-id (org-tasktree-model-node-id task-node)))
            (setf (org-tasktree-model-node-parent-id child) task-id)
            (org-tasktree-find-node-ert--insert-node child)))))))

(cl-defun org-tasktree-find-node-ert--meta-for-node
    (&key uid parent-id)
  "Return META plist for node editing.
UID is the node identifier and PARENT-ID is the parent identifier."
  (list :type 'node
        :uid uid
        :parent-id parent-id))

(provide 'org-tasktree-find-node-ert)
;;; org-tasktree-find-node-ert.el ends here
