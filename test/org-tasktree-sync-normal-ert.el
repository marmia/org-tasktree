;;; org-tasktree-sync-normal-ert.el --- Normal ERT tests for sync -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Normal-case ERT tests for `org-tasktree-sync-*'.
;; These tests focus on insert/update scenarios using org buffers.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-model)
(require 'org-tasktree-sync-ert)

(defconst org-tasktree-sync-normal-ert--uid-aaa
  "82a4e7b7-207f-5583-8e8c-47503339b07b")
(defconst org-tasktree-sync-normal-ert--uid-bbb
  "c01ef21b-3bda-5a2b-9179-20fc145215e9")
(defconst org-tasktree-sync-normal-ert--uid-ccc
  "69f0ecdd-d8ee-5970-81b2-4edf5e985240")
(defconst org-tasktree-sync-normal-ert--uid-ddd
  "1ccee950-21b1-5ec2-bac7-384c1c9cae6f")
(defconst org-tasktree-sync-normal-ert--uid-eee
  "7e84806d-9bdc-584c-a678-88140ad824b0")
(defconst org-tasktree-sync-normal-ert--uid-fff
  "ca706ec4-dc8e-568c-a157-012e585b741d")
(defconst org-tasktree-sync-normal-ert--uid-ggg
  "608a8d33-54fe-55dd-b877-a4944b2be2ed")
(defconst org-tasktree-sync-normal-ert--uid-hhh
  "1438dc0e-da7d-5a82-9233-59d1ac453018")

(defun org-tasktree-sync-normal-ert--tags-string (tags)
  "Return normalized tag string for TAGS list."
  (org-tasktree-model-tags->org-string tags))

(defun org-tasktree-sync-normal-ert--assert-node-tags (node expected-tags)
  "Assert NODE has EXPECTED-TAGS in node_tags."
  (let* ((node-id (org-tasktree-model-node-id node))
         (tags (org-tasktree-sync-ert--fetch-node-tags node-id))
         (expected (sort (or expected-tags '()) #'string<)))
    (should (equal expected tags))))

(ert-deftest org-tasktree-sync-normal-ert-ins1-full-path ()
  "Normal case: insert single tree with all attributes."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-01.org")
  (should (= 5 (org-tasktree-sync-ert--node-count)))
  (let* ((proj (org-tasktree-sync-ert--fetch-node-by-title "proj 01"))
         (phase (org-tasktree-sync-ert--fetch-node-by-title "phase 01"))
         (group (org-tasktree-sync-ert--fetch-node-by-title "group 01"))
         (task (org-tasktree-sync-ert--fetch-node-by-title "task 01"))
         (child (org-tasktree-sync-ert--fetch-node-by-title "child 01"))
         (proj-id (org-tasktree-model-node-id proj))
         (phase-id (org-tasktree-model-node-id phase))
         (group-id (org-tasktree-model-node-id group))
         (task-id (org-tasktree-model-node-id task)))
    (should proj)
    (should phase)
    (should group)
    (should task)
    (should child)
    (org-tasktree-sync-ert--assert-node
     proj
     :title "proj 01"
     :tags (org-tasktree-sync-normal-ert--tags-string '("project"))
     :content "proj 01 notes"
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :parent-id))
    (should (null (org-tasktree-model-node-parent-id proj)))
    (org-tasktree-sync-normal-ert--assert-node-tags proj '("project"))
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase 01"
     :tags (org-tasktree-sync-normal-ert--tags-string '("phase"))
     :content "phase 01 notes"
     :status "OPEN"
     :parent-id proj-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags phase '("phase"))
    (org-tasktree-sync-ert--assert-node
     group
     :title "group 01"
     :tags (org-tasktree-sync-normal-ert--tags-string '("group"))
     :content "group 01 notes"
     :status "OPEN"
     :parent-id phase-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags group '("group"))
    (org-tasktree-sync-ert--assert-node
     task
     :title "task 01"
     :todo-keyword "TODO"
     :priority "A"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--tags-string '("task"))
     :content "task 01 notes"
     :status "OPEN"
     :parent-id group-id)
    (org-tasktree-sync-normal-ert--assert-node-tags task '("task"))
    (org-tasktree-sync-ert--assert-node
     child
     :title "child 01"
     :content "child 01 notes"
     :status "OPEN"
     :parent-id task-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-normal-ert--assert-node-tags child nil)))

(ert-deftest org-tasktree-sync-normal-ert-ins2-multi-tree ()
  "Normal case: insert multiple trees."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-02.org")
  (should (= 10 (org-tasktree-sync-ert--node-count)))
  (let* ((proj-a (org-tasktree-sync-ert--fetch-node-by-title "proj 02-1"))
         (phase-a (org-tasktree-sync-ert--fetch-node-by-title "phase 02-1"))
         (task-a1 (org-tasktree-sync-ert--fetch-node-by-title "task 02-1"))
         (task-a2 (org-tasktree-sync-ert--fetch-node-by-title "task 02-2"))
         (proj-b (org-tasktree-sync-ert--fetch-node-by-title "proj 02-2"))
         (group-b1 (org-tasktree-sync-ert--fetch-node-by-title "group 02-1"))
         (task-b1 (org-tasktree-sync-ert--fetch-node-by-title "task 02-3"))
         (group-b2 (org-tasktree-sync-ert--fetch-node-by-title "group 02-2"))
         (task-b2 (org-tasktree-sync-ert--fetch-node-by-title "task 02-4"))
         (task-c (org-tasktree-sync-ert--fetch-node-by-title "task 02-5"))
         (proj-a-id (org-tasktree-model-node-id proj-a))
         (phase-a-id (org-tasktree-model-node-id phase-a))
         (proj-b-id (org-tasktree-model-node-id proj-b))
         (group-b1-id (org-tasktree-model-node-id group-b1))
         (group-b2-id (org-tasktree-model-node-id group-b2)))
    (org-tasktree-sync-ert--assert-node
     proj-a
     :title "proj 02-1"
     :tags (org-tasktree-sync-normal-ert--tags-string '("project"))
     :content "proj 02-1 notes"
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :parent-id))
    (org-tasktree-sync-normal-ert--assert-node-tags proj-a '("project"))
    (org-tasktree-sync-ert--assert-node
     phase-a
     :title "phase 02-1"
     :tags (org-tasktree-sync-normal-ert--tags-string '("phase"))
     :content "phase 02-1 notes"
     :status "OPEN"
     :parent-id proj-a-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags phase-a '("phase"))
    (org-tasktree-sync-ert--assert-node
     task-a1
     :title "task 02-1"
     :todo-keyword "TODO"
     :priority "B"
     :tags (org-tasktree-sync-normal-ert--tags-string '("task" "child"))
     :content "task 02-1 notes"
     :status "OPEN"
     :parent-id phase-a-id
     :expect-nil '(:scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags task-a1 '("task" "child"))
    (org-tasktree-sync-ert--assert-node
     task-a2
     :title "task 02-2"
     :todo-keyword "TODO"
     :priority "B"
     :tags (org-tasktree-sync-normal-ert--tags-string '("task" "child"))
     :content "task 02-2 notes"
     :status "OPEN"
     :parent-id phase-a-id
     :expect-nil '(:scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags task-a2 '("task" "child"))
    (org-tasktree-sync-ert--assert-node
     proj-b
     :title "proj 02-2"
     :tags (org-tasktree-sync-normal-ert--tags-string '("project"))
     :content "proj 02-2 notes"
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :parent-id))
    (org-tasktree-sync-normal-ert--assert-node-tags proj-b '("project"))
    (org-tasktree-sync-ert--assert-node
     group-b1
     :title "group 02-1"
     :tags (org-tasktree-sync-normal-ert--tags-string '("group"))
     :content "group2-1 notes"
     :status "OPEN"
     :parent-id proj-b-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags group-b1 '("group"))
    (org-tasktree-sync-ert--assert-node
     task-b1
     :title "task 02-3"
     :todo-keyword "TODO"
     :priority "B"
     :tags (org-tasktree-sync-normal-ert--tags-string '("task" "child"))
     :content "task2-1 notes"
     :status "OPEN"
     :parent-id group-b1-id
     :expect-nil '(:scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags task-b1 '("task" "child"))
    (org-tasktree-sync-ert--assert-node
     group-b2
     :title "group 02-2"
     :tags (org-tasktree-sync-normal-ert--tags-string '("group"))
     :content "group 02-2 notes"
     :status "OPEN"
     :parent-id proj-b-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags group-b2 '("group"))
    (org-tasktree-sync-ert--assert-node
     task-b2
     :title "task 02-4"
     :todo-keyword "TODO"
     :priority "B"
     :tags (org-tasktree-sync-normal-ert--tags-string '("task" "child"))
     :content "task2-2 notes"
     :status "OPEN"
     :parent-id group-b2-id
     :expect-nil '(:scheduled :deadline :repeat))
    (org-tasktree-sync-normal-ert--assert-node-tags task-b2 '("task" "child"))
    (org-tasktree-sync-ert--assert-node
     task-c
     :title "task 02-5"
     :todo-keyword "TODO"
     :priority "A"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :repeat ".+1d"
     :tags (org-tasktree-sync-normal-ert--tags-string '("task" "single"))
     :content "task 02-5 notes"
     :status "OPEN"
     :expect-nil '(:parent-id))
    (org-tasktree-sync-normal-ert--assert-node-tags task-c '("task" "single"))))

(ert-deftest org-tasktree-sync-normal-ert-ins3-single-node ()
  "Normal case: insert single node."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-03.org")
  (should (= 1 (org-tasktree-sync-ert--node-count)))
  (let ((task (org-tasktree-sync-ert--fetch-node-by-title "task 03")))
    (should task)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task 03"
     :todo-keyword "TODO"
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--tags-string '("task" "single"))
     :content "task 03 notes"
     :status "OPEN"
     :expect-nil '(:parent-id))
    (org-tasktree-sync-normal-ert--assert-node-tags task '("task" "single"))))

(ert-deftest org-tasktree-sync-normal-ert-ins4-content-org-syntax ()
  "Normal case: preserve org syntax in content."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-04.org")
  (should (= 3 (org-tasktree-sync-ert--node-count)))
  (let* ((proj (org-tasktree-sync-ert--fetch-node-by-title "proj 04"))
         (task (org-tasktree-sync-ert--fetch-node-by-title "task 04-1"))
         (child (org-tasktree-sync-ert--fetch-node-by-title "task 04-2"))
         (proj-id (org-tasktree-model-node-id proj))
         (task-id (org-tasktree-model-node-id task))
         (content (and task (org-tasktree-model-node-content task))))
    (should proj)
    (should task)
    (should child)
    (org-tasktree-sync-ert--assert-node
     proj
     :title "proj 04"
     :content "proj 04 notes"
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags :parent-id))
    (org-tasktree-sync-normal-ert--assert-node-tags proj nil)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task 04-1"
     :todo-keyword "TODO"
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--tags-string '("task" "org_syntax"))
     :status "OPEN"
     :parent-id proj-id
     :expect-nil '(:deadline))
    (org-tasktree-sync-normal-ert--assert-node-tags task '("task" "org_syntax"))
    (should (stringp content))
    (should (string-match-p (regexp-quote "task1 items:") content))
    (should (string-match-p (regexp-quote "- [ ] item1") content))
    (should (string-match-p (regexp-quote "[[https://www.youtube.com/][YouTube]]")
                            content))
    (should (string-match-p (regexp-quote "#+begin_src python") content))
    (should (string-match-p (regexp-quote "print(total)") content))
    (should (string-match-p (regexp-quote "#+end_src") content))
    (org-tasktree-sync-ert--assert-node
     child
     :title "task 04-2"
     :todo-keyword "TODO"
     :content "task 04-2 notes"
     :status "OPEN"
     :parent-id task-id
     :expect-nil '(:priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-normal-ert--assert-node-tags child nil)))

(ert-deftest org-tasktree-sync-normal-ert-upd1-full-path ()
  "Normal case: update full path (single tree)."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (aaa-id (org-tasktree-model-node-id (plist-get seed :aaa)))
         (bbb-id (org-tasktree-model-node-id (plist-get seed :bbb)))
         (ccc-id (org-tasktree-model-node-id (plist-get seed :ccc)))
         (ddd-id (org-tasktree-model-node-id (plist-get seed :ddd))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-01.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((aaa (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-aaa))
           (bbb (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-bbb))
           (ccc (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-ccc))
           (ddd (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-ddd))
           (eee (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-eee)))
      (org-tasktree-sync-ert--assert-node
      aaa
      :title "AAA (after upd1)"
      :content "AAA after upd1."
      :status "OPEN"
      :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                    :tags :parent-id))
      (org-tasktree-sync-ert--assert-node
       bbb
       :title "BBB (after upd1)"
       :content "BBB after upd1."
       :status "OPEN"
       :parent-id aaa-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       ccc
       :title "CCC (after upd1)"
       :content "CCC after upd1."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       ddd
       :title "DDD (after upd1)"
       :todo-keyword "TODO"
       :priority "A"
       :scheduled "2026-02-01"
       :deadline "2026-02-10"
       :repeat ".+1d"
       :tags (org-tasktree-sync-normal-ert--tags-string '("after" "ddd"))
       :content "DDD after upd1."
       :status "OPEN"
       :parent-id ccc-id)
      (org-tasktree-sync-normal-ert--assert-node-tags ddd '("after" "ddd"))
      (org-tasktree-sync-ert--assert-node
       eee
       :title "EEE (after upd1)"
       :content "EEE after upd1."
       :status "OPEN"
       :parent-id ddd-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-normal-ert--assert-node-tags eee nil))))

(ert-deftest org-tasktree-sync-normal-ert-upd2-multi-tree ()
  "Normal case: update multiple trees."
  (org-tasktree-sync-ert--seed-update-tree)
  (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-02.org")
  (should (= 8 (org-tasktree-sync-ert--node-count)))
  (let* ((aaa (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-normal-ert--uid-aaa))
         (bbb (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-normal-ert--uid-bbb))
         (ccc (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-normal-ert--uid-ccc))
         (ddd (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-normal-ert--uid-ddd))
         (eee (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-normal-ert--uid-eee))
         (fff (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-normal-ert--uid-fff))
         (ggg (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-normal-ert--uid-ggg))
         (hhh (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-normal-ert--uid-hhh))
         (aaa-id (org-tasktree-model-node-id aaa))
         (bbb-id (org-tasktree-model-node-id bbb))
         (ccc-id (org-tasktree-model-node-id ccc))
         (ddd-id (org-tasktree-model-node-id ddd))
         (fff-id (org-tasktree-model-node-id fff))
         (ggg-id (org-tasktree-model-node-id ggg)))
    (org-tasktree-sync-ert--assert-node
     aaa
     :title "AAA (after upd2)"
     :content "AAA after upd2."
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                   :tags :parent-id))
    (org-tasktree-sync-ert--assert-node
     bbb
     :title "BBB (after upd2)"
     :content "BBB after upd2."
     :status "OPEN"
     :parent-id aaa-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     ccc
     :title "CCC (after upd2)"
     :content "CCC after upd2."
     :status "OPEN"
     :parent-id bbb-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     ddd
     :title "DDD (after upd2)"
     :content "DDD after upd2."
     :status "OPEN"
     :parent-id ccc-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     eee
     :title "EEE (after upd2)"
     :content "EEE after upd2."
     :status "OPEN"
     :parent-id ddd-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     fff
     :title "FFF (after upd2)"
     :content "FFF after upd2."
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                   :tags :parent-id))
    (org-tasktree-sync-ert--assert-node
     ggg
     :title "GGG (after upd2)"
     :content "GGG after upd2."
     :status "OPEN"
     :parent-id fff-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     hhh
     :title "HHH (after upd2)"
     :todo-keyword "TODO"
     :priority "B"
     :content "HHH after upd2."
     :status "OPEN"
     :parent-id ggg-id
     :expect-nil '(:scheduled :deadline :repeat :tags))))

(ert-deftest org-tasktree-sync-normal-ert-upd3-partial-with-parent ()
  "Normal case: partial path update with parent in buffer."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (aaa-id (org-tasktree-model-node-id (plist-get seed :aaa)))
         (bbb-id (org-tasktree-model-node-id (plist-get seed :bbb))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-03.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((aaa (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-aaa))
           (bbb (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-bbb))
           (ccc (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-ccc))
           (ccc-id (org-tasktree-model-node-id ccc)))
      (org-tasktree-sync-ert--assert-node
      aaa
      :title "AAA (after upd3-a)"
      :content "AAA after upd3-a."
      :status "OPEN"
      :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                    :tags :parent-id))
      (org-tasktree-sync-ert--assert-node
       bbb
       :title "BBB (after upd3-a)"
       :content "BBB after upd3-a."
       :status "OPEN"
       :parent-id aaa-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       ccc
       :title "CCC (after upd3-a)"
       :content "CCC after upd3-a."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (should (numberp ccc-id)))))

(ert-deftest org-tasktree-sync-normal-ert-upd4-partial-without-parent ()
  "Normal case: partial path update with parent out of scope."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (aaa-id (org-tasktree-model-node-id (plist-get seed :aaa))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-04.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((bbb (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-bbb))
           (ccc (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-ccc))
           (bbb-id (org-tasktree-model-node-id bbb)))
      (org-tasktree-sync-ert--assert-node
       bbb
       :title "BBB (after upd3-b)"
       :content "BBB after upd3-b."
       :status "OPEN"
       :parent-id aaa-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       ccc
       :title "CCC (after upd3-b)"
       :content "CCC after upd3-b."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(ert-deftest org-tasktree-sync-normal-ert-upd5-restructure-1 ()
  "Normal case: restructure tree 1."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (bbb-id (org-tasktree-model-node-id (plist-get seed :bbb))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-05.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((ddd (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-ddd))
           (eee (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-eee))
           (ddd-id (org-tasktree-model-node-id ddd)))
      (org-tasktree-sync-ert--assert-node
       ddd
       :title "DDD (after upd4)"
       :content "DDD after upd4."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       eee
       :title "EEE (after upd4)"
       :content "EEE after upd4."
       :status "OPEN"
       :parent-id ddd-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(ert-deftest org-tasktree-sync-normal-ert-upd6-restructure-2 ()
  "Normal case: restructure tree 2."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (bbb-id (org-tasktree-model-node-id (plist-get seed :bbb)))
         (ccc-id (org-tasktree-model-node-id (plist-get seed :ccc))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-06.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((ddd (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-ddd))
           (eee (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-eee)))
      (org-tasktree-sync-ert--assert-node
       ddd
       :title "DDD (after upd5)"
       :content "DDD after upd5."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       eee
       :title "EEE (after upd5)"
       :content "EEE after upd5."
       :status "OPEN"
       :parent-id ccc-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(ert-deftest org-tasktree-sync-normal-ert-upd7-restructure-3 ()
  "Normal case: restructure tree 3."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (hhh-id (org-tasktree-model-node-id (plist-get seed :hhh))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-07.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((ddd (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-ddd))
           (eee (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-eee))
           (ddd-id (org-tasktree-model-node-id ddd)))
      (org-tasktree-sync-ert--assert-node
       ddd
       :title "DDD (after upd6)"
       :content "DDD after upd6."
       :status "OPEN"
       :parent-id hhh-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       eee
       :title "EEE (after upd6)"
       :content "EEE after upd6."
       :status "OPEN"
       :parent-id ddd-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(ert-deftest org-tasktree-sync-normal-ert-upd8-add-new-nodes ()
  "Normal case: add new nodes under existing tree."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (hhh-id (org-tasktree-model-node-id (plist-get seed :hhh))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-08.org")
    (should (= 10 (org-tasktree-sync-ert--node-count)))
    (let* ((fff (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-fff))
           (ggg (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-ggg))
           (hhh (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-normal-ert--uid-hhh))
           (iii (org-tasktree-sync-ert--fetch-node-by-title "III (new)" hhh-id))
           (iii-id (org-tasktree-model-node-id iii))
           (jjj (org-tasktree-sync-ert--fetch-node-by-title "JJJ (new)" iii-id)))
      (org-tasktree-sync-ert--assert-node
      fff
      :title "FFF (after upd8)"
      :content "FFF after upd8."
      :status "OPEN"
      :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                    :tags :parent-id))
      (org-tasktree-sync-ert--assert-node
       ggg
       :title "GGG (after upd8)"
       :content "GGG after upd8."
       :status "OPEN"
       :parent-id (org-tasktree-model-node-id fff)
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       hhh
       :title "HHH (after upd8)"
       :content "HHH after upd8."
       :status "OPEN"
       :parent-id (org-tasktree-model-node-id ggg)
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       iii
       :title "III (new)"
       :todo-keyword "TODO"
       :priority "B"
       :scheduled "2026-02-15"
       :content "III new node."
       :status "OPEN"
       :parent-id hhh-id
       :expect-nil '(:deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       jjj
       :title "JJJ (new)"
       :content "JJJ new node."
       :status "OPEN"
       :parent-id iii-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(provide 'org-tasktree-sync-normal-ert)
;;; org-tasktree-sync-normal-ert.el ends here
