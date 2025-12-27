;;; org-tasktree-find-node-abnormal-ert.el --- Abnormal ERT tests for find-node -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Abnormal-case ERT tests for `org-tasktree-find-node'.
;; These tests focus on validation and reference integrity errors.
;;

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-tasktree-db)
(require 'org-tasktree-model)
(require 'org-tasktree-query)
(require 'org-tasktree-ui)
(require 'org-tasktree-find-node-ert)

(defun org-tasktree-find-node-abnormal-ert--with-widget-values (values fn)
  "Call FN with widget values from VALUES alist.
VALUES is an alist of (KEY . VALUE).  FN is a function of no arguments."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair values)
      (puthash (car pair) (cdr pair) table))
    (cl-letf (((symbol-function 'org-tasktree-ui--widget-value)
               (lambda (key) (gethash key table)))
              ((symbol-function 'org-tasktree-ui--widget-value-raw)
               (lambda (key) (gethash key table))))
      (funcall fn))))

(defun org-tasktree-find-node-abnormal-ert--seed-project ()
  "Seed a single project and return its id."
  (org-tasktree-test-helper-reset-db)
  (let* ((project (org-tasktree-find-node-ert--make-node
                   :uid "abnormal-project-1"
                   :node-type "project"
                   :title "project1"
                   :level 1
                   :todo-keyword "PROJ"
                   :status "OPEN"
                   :parent-id nil
                   :project-id nil
                   :phase-id nil))
         (saved (org-tasktree-find-node-ert--insert-node project)))
    (org-tasktree-model-node-id saved)))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-node-type ()
  "Abnormal case: invalid node_type should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task" "group"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "foo")
       (:title . "x"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-empty-title ()
  "Abnormal case: empty title should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "task")
       (:title . "  "))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-title ()
  "Abnormal case: title with '/' should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "task")
       (:title . "a/b"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-priority ()
  "Abnormal case: invalid priority should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "task")
       (:title . "task")
       (:priority . "AA"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-scheduled ()
  "Abnormal case: invalid scheduled date should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "task")
       (:title . "task")
       (:scheduled . "2025-02-30"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-deadline ()
  "Abnormal case: invalid deadline date should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "task")
       (:title . "task")
       (:deadline . "2025-02-30"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-schedule-after-deadline ()
  "Abnormal case: scheduled after deadline should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "task")
       (:title . "task")
       (:scheduled . "2025-12-31")
       (:deadline . "2025-12-01"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-repeat ()
  "Abnormal case: invalid repeat should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "task")
       (:title . "task")
       (:repeat . "1d"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-tags ()
  "Abnormal case: invalid tags should signal `user-error'."
  (org-tasktree-find-node-abnormal-ert--seed-project)
  (let ((meta (list :type 'node :node-type-options '("task"))))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:node-type . "task")
       (:title . "task")
       (:tags . "tag1,tag2"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-phase-missing-project ()
  "Abnormal case: phase without project should signal `user-error'."
  (org-tasktree-test-helper-reset-db)
  (let ((meta (org-tasktree-find-node-ert--meta-for-phase
               :project-title "missing-project")))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "phase1"))
     (lambda ()
       (should-error (org-tasktree-ui--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-group-missing-parent ()
  "Abnormal case: group with missing parent should signal `user-error'."
  (let ((project-id (org-tasktree-find-node-abnormal-ert--seed-project)))
    (let ((meta (org-tasktree-find-node-ert--meta-for-group
                 :project-id project-id
                 :parent-id 9999
                 :project-title "project1")))
      (org-tasktree-find-node-abnormal-ert--with-widget-values
       '((:title . "group1"))
       (lambda ()
         (should-error (org-tasktree-ui--submit-widget meta)))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-task-missing-parent ()
  "Abnormal case: task with missing parent should signal `user-error'."
  (let ((project-id (org-tasktree-find-node-abnormal-ert--seed-project)))
    (let ((meta (org-tasktree-find-node-ert--meta-for-task
                 :project-id project-id
                 :parent-id 9999)))
      (org-tasktree-find-node-abnormal-ert--with-widget-values
       '((:title . "task1"))
       (lambda ()
         (should-error (org-tasktree-ui--submit-widget meta)))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-task-invalid-phase ()
  "Abnormal case: task with invalid phase should signal `user-error'."
  (let ((project-id (org-tasktree-find-node-abnormal-ert--seed-project)))
    (let ((meta (org-tasktree-find-node-ert--meta-for-task
                 :project-id project-id
                 :phase-id 9999
                 :parent-id project-id)))
      (org-tasktree-find-node-abnormal-ert--with-widget-values
       '((:title . "task1"))
       (lambda ()
         (should-error (org-tasktree-ui--submit-widget meta)))))))

(provide 'org-tasktree-find-node-abnormal-ert)
;;; org-tasktree-find-node-abnormal-ert.el ends here
