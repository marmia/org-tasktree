;;; org-tasktree-sync-normal-ins-ert.el --- Normal insert ERT tests for sync -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Normal insert ERT tests for `org-tasktree-sync-*'.
;; These tests focus on initial insert scenarios using org buffers.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-model)
(require 'org-tasktree-sync-ert)

(ert-deftest org-tasktree-sync-normal-ins-ert1-full-path ()
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
     :tags (org-tasktree-sync-ert--tags-string '("project"))
     :content "proj 01 notes"
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :parent-id))
    (should (null (org-tasktree-model-node-parent-id proj)))
    (org-tasktree-sync-ert--assert-node-tags proj '("project"))
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase 01"
     :tags (org-tasktree-sync-ert--tags-string '("phase"))
     :content "phase 01 notes"
     :status "OPEN"
     :parent-id proj-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags phase '("phase"))
    (org-tasktree-sync-ert--assert-node
     group
     :title "group 01"
     :tags (org-tasktree-sync-ert--tags-string '("group"))
     :content "group 01 notes"
     :status "OPEN"
     :parent-id phase-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags group '("group"))
    (org-tasktree-sync-ert--assert-node
     task
     :title "task 01"
     :todo-keyword "TODO"
     :priority "A"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :repeat "+1d"
     :tags (org-tasktree-sync-ert--tags-string '("task"))
     :content "task 01 notes"
     :status "OPEN"
     :parent-id group-id)
    (org-tasktree-sync-ert--assert-node-tags task '("task"))
    (org-tasktree-sync-ert--assert-node
     child
     :title "child 01"
     :content "child 01 notes"
     :status "OPEN"
     :parent-id task-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node-tags child nil)))

(ert-deftest org-tasktree-sync-normal-ins-ert2-multi-tree ()
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
     :tags (org-tasktree-sync-ert--tags-string '("project"))
     :content "proj 02-1 notes"
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :parent-id))
    (org-tasktree-sync-ert--assert-node-tags proj-a '("project"))
    (org-tasktree-sync-ert--assert-node
     phase-a
     :title "phase 02-1"
     :tags (org-tasktree-sync-ert--tags-string '("phase"))
     :content "phase 02-1 notes"
     :status "OPEN"
     :parent-id proj-a-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags phase-a '("phase"))
    (org-tasktree-sync-ert--assert-node
     task-a1
     :title "task 02-1"
     :todo-keyword "TODO"
     :priority "B"
     :tags (org-tasktree-sync-ert--tags-string '("task" "child"))
     :content "task 02-1 notes"
     :status "OPEN"
     :parent-id phase-a-id
     :expect-nil '(:scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags task-a1 '("task" "child"))
    (org-tasktree-sync-ert--assert-node
     task-a2
     :title "task 02-2"
     :todo-keyword "TODO"
     :priority "B"
     :tags (org-tasktree-sync-ert--tags-string '("task" "child"))
     :content "task 02-2 notes"
     :status "OPEN"
     :parent-id phase-a-id
     :expect-nil '(:scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags task-a2 '("task" "child"))
    (org-tasktree-sync-ert--assert-node
     proj-b
     :title "proj 02-2"
     :tags (org-tasktree-sync-ert--tags-string '("project"))
     :content "proj 02-2 notes"
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :parent-id))
    (org-tasktree-sync-ert--assert-node-tags proj-b '("project"))
    (org-tasktree-sync-ert--assert-node
     group-b1
     :title "group 02-1"
     :tags (org-tasktree-sync-ert--tags-string '("group"))
     :content "group2-1 notes"
     :status "OPEN"
     :parent-id proj-b-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags group-b1 '("group"))
    (org-tasktree-sync-ert--assert-node
     task-b1
     :title "task 02-3"
     :todo-keyword "TODO"
     :priority "B"
     :tags (org-tasktree-sync-ert--tags-string '("task" "child"))
     :content "task2-1 notes"
     :status "OPEN"
     :parent-id group-b1-id
     :expect-nil '(:scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags task-b1 '("task" "child"))
    (org-tasktree-sync-ert--assert-node
     group-b2
     :title "group 02-2"
     :tags (org-tasktree-sync-ert--tags-string '("group"))
     :content "group 02-2 notes"
     :status "OPEN"
     :parent-id proj-b-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags group-b2 '("group"))
    (org-tasktree-sync-ert--assert-node
     task-b2
     :title "task 02-4"
     :todo-keyword "TODO"
     :priority "B"
     :tags (org-tasktree-sync-ert--tags-string '("task" "child"))
     :content "task2-2 notes"
     :status "OPEN"
     :parent-id group-b2-id
     :expect-nil '(:scheduled :deadline :repeat))
    (org-tasktree-sync-ert--assert-node-tags task-b2 '("task" "child"))
    (org-tasktree-sync-ert--assert-node
     task-c
     :title "task 02-5"
     :todo-keyword "TODO"
     :priority "A"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :repeat ".+1d"
     :tags (org-tasktree-sync-ert--tags-string '("task" "single"))
     :content "task 02-5 notes"
     :status "OPEN"
     :expect-nil '(:parent-id))
    (org-tasktree-sync-ert--assert-node-tags task-c '("task" "single"))))

(ert-deftest org-tasktree-sync-normal-ins-ert3-single-node ()
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
     :tags (org-tasktree-sync-ert--tags-string '("task" "single"))
     :content "task 03 notes"
     :status "OPEN"
     :expect-nil '(:parent-id))
    (org-tasktree-sync-ert--assert-node-tags task '("task" "single"))))

(ert-deftest org-tasktree-sync-normal-ins-ert4-content-org-syntax ()
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
    (org-tasktree-sync-ert--assert-node-tags proj nil)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task 04-1"
     :todo-keyword "TODO"
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-ert--tags-string '("task" "org_syntax"))
     :status "OPEN"
     :parent-id proj-id
     :expect-nil '(:deadline))
    (org-tasktree-sync-ert--assert-node-tags task '("task" "org_syntax"))
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
    (org-tasktree-sync-ert--assert-node-tags child nil)))

(provide 'org-tasktree-sync-normal-ins-ert)
;;; org-tasktree-sync-normal-ins-ert.el ends here
