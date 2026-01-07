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

(defun org-tasktree-sync-abnormal-ert--assert-node-unchanged (before after)
  "Assert that AFTER matches BEFORE node data."
  (should (org-tasktree-model-node-p before))
  (should (org-tasktree-model-node-p after))
  (should (equal (org-tasktree-model-node-to-plist before)
                 (org-tasktree-model-node-to-plist after))))

(defun org-tasktree-sync-abnormal-ert--assert-upd-sync-failure (file)
  "Assert that updating with FILE fails and keep the DB unchanged."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (nodes-before (list (plist-get seed :aaa)
                             (plist-get seed :bbb)
                             (plist-get seed :ccc)
                             (plist-get seed :ddd)
                             (plist-get seed :eee)
                             (plist-get seed :fff)
                             (plist-get seed :ggg)
                             (plist-get seed :hhh)))
         (count-before (org-tasktree-sync-ert--node-count)))
    (org-tasktree-sync-ert--with-org-buffer
     file
     (lambda ()
       (should-error (org-tasktree-sync-buffer))))
    (should (= count-before (org-tasktree-sync-ert--node-count)))
    (dolist (node-before nodes-before)
      (org-tasktree-sync-abnormal-ert--assert-node-unchanged
       node-before
       (org-tasktree-sync-ert--fetch-node-by-uid
        (org-tasktree-model-node-uid node-before))))))

(ert-deftest org-tasktree-sync-abnormal-ert-upd1-invalid-uid ()
  "Test that invalid UID signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-01.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd2-missing-uid ()
  "Test that missing DB UID signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-02.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd3-empty-title ()
  "Test that empty title signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-03.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd4-invalid-title ()
  "Test that title containing '/' signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-04.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd5-invalid-priority ()
  "Test that invalid priority signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-05.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd6-invalid-scheduled ()
  "Test that invalid scheduled values signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-06.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd7-invalid-deadline ()
  "Test that invalid deadline values signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-07.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd8-scheduled-after-deadline ()
  "Test that scheduled after deadline signals an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-08.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd9-invalid-repeat ()
  "Test that invalid repeat values signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-09.org"))

(ert-deftest org-tasktree-sync-abnormal-ert-upd10-invalid-tags ()
  "Test that invalid tags signal an error on update."
  (org-tasktree-sync-abnormal-ert--assert-upd-sync-failure "sync-err-upd-10.org"))

(provide 'org-tasktree-sync-abnormal-ert)
;;; org-tasktree-sync-abnormal-ert.el ends here
