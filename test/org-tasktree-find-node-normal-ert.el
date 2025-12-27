;;; org-tasktree-find-node-normal-ert.el --- Normal ERT tests for find-node -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Normal-case ERT tests for `org-tasktree-find-node'.
;; These tests use helper stubs to avoid UI interactions.
;;

;;; Code:

(require 'ert)
(require 'seq)
(require 'sqlite)
(require 'org-tasktree-db)
(require 'org-tasktree-model)
(require 'org-tasktree-query)
(require 'org-tasktree-ui)
(require 'org-tasktree-ui-minibuffer)
(require 'org-tasktree-find-node-ert)

(put 'org-tasktree-find-node-ert-with-completing-read
     'lisp-indent-function
     1)
(put 'org-tasktree-find-node-ert-with-widget-values
     'lisp-indent-function
     1)

(defun org-tasktree-find-node-normal-ert--row-nth (row index)
  "Return ROW element at INDEX for lists or vectors."
  (if (vectorp row) (aref row index) (nth index row)))

(defun org-tasktree-find-node-normal-ert--select-node (title node-type)
  "Return first node row for TITLE and NODE-TYPE."
  (org-tasktree-db--with-db db
    (car (sqlite-select
          db
          (concat
           "SELECT id, uid, parent_id, project_id, phase_id, title, tags "
           "FROM nodes WHERE title=? AND node_type=? LIMIT 1;")
          (vector title node-type)))))

(defun org-tasktree-find-node-normal-ert--insert-done-tree ()
  "Insert a DONE tree under a separate project."
  (let* ((project (org-tasktree-find-node-ert--make-node
                   :uid "done-project-1"
                   :node-type "project"
                   :title "project2"
                   :level 1
                   :todo-keyword "PROJ"
                   :status "DONE"
                   :parent-id nil
                   :project-id nil
                   :phase-id nil))
         (phase (org-tasktree-find-node-ert--make-node
                 :uid "done-phase-1"
                 :node-type "phase"
                 :title "phase2"
                 :level 2
                 :todo-keyword "PHASE"
                 :status "DONE"
                 :parent-id :keep
                 :project-id :keep
                 :phase-id nil))
         (group (org-tasktree-find-node-ert--make-node
                 :uid "done-group-1"
                 :node-type "group"
                 :title "group2"
                 :level 3
                 :todo-keyword nil
                 :status "DONE"
                 :parent-id :keep
                 :project-id :keep
                 :phase-id :keep))
         (task (org-tasktree-find-node-ert--make-node
                :uid "done-task-1"
                :node-type "task"
                :title "task2"
                :level 4
                :todo-keyword "TODO"
                :status "DONE"
                :parent-id :keep
                :project-id :keep
                :phase-id :keep)))
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
          (org-tasktree-find-node-ert--insert-node task))))))

(ert-deftest org-tasktree-find-node-normal-ert-open-tree-candidates ()
  "Normal case: candidates include OPEN nodes only."
  (org-tasktree-find-node-ert--seed-open-tree)
  (org-tasktree-find-node-normal-ert--insert-done-tree)
  (let (org-tasktree-find-node-ert--captured-cands)
    (cl-letf (((symbol-function 'org-tasktree-ui-minibuffer--completing-read)
               (lambda (_prompt cands &rest _args)
                 (setq org-tasktree-find-node-ert--captured-cands cands)
                 (car cands))))
      (org-tasktree-ui-minibuffer-read-node))
    (let ((raw (mapcar #'org-tasktree-ui-minibuffer--candidate-raw
                       org-tasktree-find-node-ert--captured-cands)))
      (should (seq-some (lambda (path) (equal path "inbox")) raw))
      (should (seq-some
               (lambda (path)
                 (string-prefix-p "project1/" path))
               raw))
      (should-not (seq-some
                   (lambda (path)
                     (string-prefix-p "project2/" path))
                   raw)))))

(ert-deftest org-tasktree-find-node-normal-ert-new-node-selection ()
  "Normal case: new node selection returns parent metadata."
  (org-tasktree-find-node-ert--seed-open-tree)
  (let (org-tasktree-find-node-ert--captured-cands)
    (cl-letf (((symbol-function 'org-tasktree-ui-minibuffer--completing-read)
               (lambda (_prompt cands &rest _args)
                 (setq org-tasktree-find-node-ert--captured-cands cands)
                 "inbox/new-task")))
      (let ((result (org-tasktree-ui-minibuffer-read-node)))
        (should (equal (plist-get result :existing) nil))
        (should (equal (plist-get result :title) "new-task"))
        (let ((parent (plist-get result :parent-node)))
          (should parent)
          (should (equal (org-tasktree-model-node-title parent) "inbox")))))))

(ert-deftest org-tasktree-find-node-normal-ert-update-task ()
  "Normal case: submit updates an existing task."
  (org-tasktree-find-node-ert--seed-open-tree)
  (let* ((row (org-tasktree-find-node-normal-ert--select-node
               "task1" "task"))
         (task-id (org-tasktree-find-node-normal-ert--row-nth row 0))
         (task-uid (org-tasktree-find-node-normal-ert--row-nth row 1))
         (parent-id (org-tasktree-find-node-normal-ert--row-nth row 2))
         (project-id (org-tasktree-find-node-normal-ert--row-nth row 3))
         (phase-id (org-tasktree-find-node-normal-ert--row-nth row 4))
         (meta (org-tasktree-find-node-ert--meta-for-task
                :uid task-uid
                :project-id project-id
                :phase-id phase-id
                :parent-id parent-id)))
    (let ((table (make-hash-table :test 'equal)))
      (dolist (pair '((:title . "task1-upd")
                      (:priority . "A")
                      (:scheduled . "")
                      (:deadline . "")
                      (:repeat . "")
                      (:tags . "unit-test:task1-upd")
                      (:content . "updated content")))
        (puthash (car pair) (cdr pair) table))
      (cl-letf (((symbol-function 'org-tasktree-ui--widget-value)
                 (lambda (key) (gethash key table)))
                ((symbol-function 'org-tasktree-ui--widget-value-raw)
                 (lambda (key) (gethash key table))))
        (let ((node (org-tasktree-ui--submit-widget meta)))
          (should (equal (org-tasktree-model-node-id node) task-id))
          (should (equal (org-tasktree-model-node-title node) "task1-upd")))))
    (let* ((updated (org-tasktree-find-node-normal-ert--select-node
                     "task1-upd" "task"))
           (updated-tags (org-tasktree-find-node-normal-ert--row-nth updated 6)))
      (should (equal "task1-upd" (org-tasktree-find-node-normal-ert--row-nth updated 5)))
      (should (equal ":unit-test:task1-upd:" updated-tags)))))

(provide 'org-tasktree-find-node-normal-ert)
;;; org-tasktree-find-node-normal-ert.el ends here
