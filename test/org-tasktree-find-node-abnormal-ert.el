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
    (cl-letf (((symbol-function 'org-tasktree-ui-widget--value)
               (lambda (key) (gethash key table)))
              ((symbol-function 'org-tasktree-ui-widget--value-raw)
               (lambda (key) (gethash key table))))
      (funcall fn))))

(defun org-tasktree-find-node-abnormal-ert--seed-project ()
  "Seed a single project and return its id."
  (org-tasktree-test-helper-reset-db)
  (let* ((project (org-tasktree-find-node-ert--make-node
                   :uid "eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee"
                   :title "project1"
                   :todo-keyword nil
                   :status "OPEN"
                   :parent-id nil
                   :tags ":project:"))
         (saved (org-tasktree-find-node-ert--insert-node project)))
    (org-tasktree-model-node-id saved)))

(ert-deftest org-tasktree-find-node-abnormal-ert-empty-title ()
  "Abnormal case: empty title should signal `user-error'."
  (let* ((project-id (org-tasktree-find-node-abnormal-ert--seed-project))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :parent-id project-id)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "  "))
     (lambda ()
       (should-error (org-tasktree-ui-edit--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-title ()
  "Abnormal case: title with '/' should signal `user-error'."
  (let* ((project-id (org-tasktree-find-node-abnormal-ert--seed-project))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :parent-id project-id)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "a/b"))
     (lambda ()
       (should-error (org-tasktree-ui-edit--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-priority ()
  "Abnormal case: invalid priority should signal `user-error'."
  (let* ((project-id (org-tasktree-find-node-abnormal-ert--seed-project))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :parent-id project-id)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "task")
       (:priority . "AA"))
     (lambda ()
       (should-error (org-tasktree-ui-edit--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-scheduled ()
  "Abnormal case: invalid scheduled date should signal `user-error'."
  (let* ((project-id (org-tasktree-find-node-abnormal-ert--seed-project))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :parent-id project-id)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "task")
       (:scheduled . "2025-02-30"))
     (lambda ()
       (should-error (org-tasktree-ui-edit--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-deadline ()
  "Abnormal case: invalid deadline date should signal `user-error'."
  (let* ((project-id (org-tasktree-find-node-abnormal-ert--seed-project))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :parent-id project-id)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "task")
       (:deadline . "2025-02-30"))
     (lambda ()
       (should-error (org-tasktree-ui-edit--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-schedule-after-deadline ()
  "Abnormal case: scheduled after deadline should signal `user-error'."
  (let* ((project-id (org-tasktree-find-node-abnormal-ert--seed-project))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :parent-id project-id)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "task")
       (:scheduled . "2025-12-31")
       (:deadline . "2025-12-01"))
     (lambda ()
       (should-error (org-tasktree-ui-edit--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-repeat ()
  "Abnormal case: invalid repeat should signal `user-error'."
  (let* ((project-id (org-tasktree-find-node-abnormal-ert--seed-project))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :parent-id project-id)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "task")
       (:repeat . "1d"))
     (lambda ()
       (should-error (org-tasktree-ui-edit--submit-widget meta))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-invalid-tags ()
  "Abnormal case: invalid tags should signal `user-error'."
  (let* ((project-id (org-tasktree-find-node-abnormal-ert--seed-project))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :parent-id project-id)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "task")
       (:tags . "tag1,tag2"))
     (lambda ()
       (let ((err (should-error (org-tasktree-ui-edit--submit-widget meta)
                                :type 'user-error)))
         (should (string-match-p
                  "Tags must contain only"
                  (error-message-string err))))))))

(ert-deftest org-tasktree-find-node-abnormal-ert-parent-missing ()
  "Abnormal case: missing parent should signal `user-error'."
  (let ((meta (org-tasktree-find-node-ert--meta-for-node
               :parent-id 9999)))
    (org-tasktree-find-node-abnormal-ert--with-widget-values
     '((:title . "task1"))
     (lambda ()
       (should-error (org-tasktree-ui-edit--submit-widget meta))))))

(provide 'org-tasktree-find-node-abnormal-ert)
;;; org-tasktree-find-node-abnormal-ert.el ends here
