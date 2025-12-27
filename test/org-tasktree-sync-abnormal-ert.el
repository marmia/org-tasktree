;;; org-tasktree-sync-abnormal-ert.el --- Abnormal ERT tests for sync -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Abnormal-case ERT tests for `org-tasktree-sync-*'.
;; These tests ensure invalid org trees and values are rejected.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-sync-ert)
(require 'org-tasktree-model)
(require 'org-tasktree-test-helper)

(defun org-tasktree-sync-abnormal-ert--assert-sync-failure (file)
  "Assert that syncing FILE fails and leave DB intact."
  (org-tasktree-test-helper-reset-db)
  (org-tasktree-sync-ert--with-org-buffer
   file
   (lambda ()
     (should-error (org-tasktree-sync-buffer))))
  (should (= 1 (org-tasktree-sync-ert--node-count))))

(defun org-tasktree-sync-abnormal-ert--assert-node-unchanged (before after)
  "Assert that AFTER matches BEFORE node data."
  (should (org-tasktree-model-node-p before))
  (should (org-tasktree-model-node-p after))
  (should (equal (org-tasktree-model-node-to-plist before)
                 (org-tasktree-model-node-to-plist after))))

(defun org-tasktree-sync-abnormal-ert--assert-upd-sync-failure (file)
  "Assert that updating with FILE fails and keep the DB unchanged."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (project-before (plist-get seed :project))
         (phase-before (plist-get seed :phase))
         (group-before (plist-get seed :group))
         (task-before (plist-get seed :task))
         (count-before (org-tasktree-sync-ert--node-count)))
    (org-tasktree-sync-ert--with-org-buffer
     file
     (lambda ()
       (should-error (org-tasktree-sync-buffer))))
    (should (= count-before (org-tasktree-sync-ert--node-count)))
    (org-tasktree-sync-abnormal-ert--assert-node-unchanged
     project-before
     (org-tasktree-sync-ert--fetch-node-by-uid
      (org-tasktree-model-node-uid project-before)))
    (org-tasktree-sync-abnormal-ert--assert-node-unchanged
     phase-before
     (org-tasktree-sync-ert--fetch-node-by-uid
      (org-tasktree-model-node-uid phase-before)))
    (org-tasktree-sync-abnormal-ert--assert-node-unchanged
     group-before
     (org-tasktree-sync-ert--fetch-node-by-uid
      (org-tasktree-model-node-uid group-before)))
    (org-tasktree-sync-abnormal-ert--assert-node-unchanged
     task-before
     (org-tasktree-sync-ert--fetch-node-by-uid
      (org-tasktree-model-node-uid task-before)))))

(ert-deftest org-tasktree-sync-abnormal-ert-ins1-project-nested ()
  "Test that nested project headings signal an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-01.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins2-phase-nested ()
  "Test that nested phase headings signal an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-02.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins3-phase-before-project ()
  "Test that a phase above a project signals an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-03.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins4-group-before-phase ()
  "Test that a group above a phase signals an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-04.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins5-task-before-group ()
  "Test that a task above a group signals an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-05.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins6-empty-title ()
  "Test that an empty title signals an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-06.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins7-invalid-title ()
  "Test that a title containing '/' signals an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-07.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins8-invalid-priority ()
  "Test that an invalid priority signals an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-08.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins9-invalid-scheduled ()
  "Test that invalid scheduled values signal an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-09.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins10-invalid-deadline ()
  "Test that invalid deadline values signal an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-10.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins11-scheduled-after-deadline ()
  "Test that scheduled after deadline signals an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-11.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins12-invalid-repeat ()
  "Test that invalid repeat values signal an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-12.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-ins13-invalid-tags ()
  "Test that invalid tags signal an error."
  (org-tasktree-sync-abnormal-ert--assert-sync-failure "sync-err-ins-13.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd1-project-nested ()
  "Test that nested project headings signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-01.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd2-phase-nested ()
  "Test that nested phase headings signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-02.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd3-phase-above-project ()
  "Test that a phase above a project signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-03.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd4-group-above-phase ()
  "Test that a group above a phase signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-04.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd5-task-above-group ()
  "Test that a task above a group signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-05.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd6-empty-title ()
  "Test that an empty title signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-06.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd7-invalid-title ()
  "Test that a title containing '/' signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-07.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd8-invalid-priority ()
  "Test that an invalid priority signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-08.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd9-invalid-scheduled ()
  "Test that invalid scheduled values signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-09.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd10-invalid-deadline ()
  "Test that invalid deadline values signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-10.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd11-scheduled-after-deadline ()
  "Test that scheduled after deadline signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-11.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd12-invalid-repeat ()
  "Test that invalid repeat values signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-12.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd13-invalid-tags ()
  "Test that invalid tags signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-13.org"))

(provide 'org-tasktree-sync-abnormal-ert)
;;; org-tasktree-sync-abnormal-ert.el ends here
