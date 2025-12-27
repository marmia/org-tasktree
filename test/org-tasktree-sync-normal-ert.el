;;; org-tasktree-sync-normal-ert.el --- Normal ERT tests for sync -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Normal-case ERT tests for `org-tasktree-sync-*'.
;; These tests focus on insert scenarios using org buffers.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-model)
(require 'org-tasktree-sync-ert)

(defun org-tasktree-sync-normal-ert--expect-tags ()
  "Return normalized tags string for sync test cases."
  ":sync:test:01:")

(defconst org-tasktree-sync-normal-ert--upd-project-uid
  "00000000-0000-0000-0000-upd000000001")
(defconst org-tasktree-sync-normal-ert--upd-phase-uid
  "00000000-0000-0000-0000-upd000000002")
(defconst org-tasktree-sync-normal-ert--upd-group-uid
  "00000000-0000-0000-0000-upd000000003")
(defconst org-tasktree-sync-normal-ert--upd-task-uid
  "00000000-0000-0000-0000-upd000000004")

(defconst org-tasktree-sync-normal-ert--before-project-tags
  '("project" "unit-test" "upd"))
(defconst org-tasktree-sync-normal-ert--before-phase-tags
  '("phase" "unit-test" "upd"))
(defconst org-tasktree-sync-normal-ert--before-group-tags
  '("group" "unit-test" "upd"))
(defconst org-tasktree-sync-normal-ert--before-task-tags
  '("task" "unit-test" "upd"))

(defconst org-tasktree-sync-normal-ert--after-project-tags
  '("after" "proj" "upd"))
(defconst org-tasktree-sync-normal-ert--after-phase-tags
  '("after" "phase" "upd"))
(defconst org-tasktree-sync-normal-ert--after-group-tags
  '("after" "group" "upd"))
(defconst org-tasktree-sync-normal-ert--after-task-tags
  '("after" "task" "upd"))

(defun org-tasktree-sync-normal-ert--assert-node-tags (node expected-tags)
  "Assert NODE has EXPECTED-TAGS in node_tags."
  (let* ((node-id (org-tasktree-model-node-id node))
         (tags (org-tasktree-sync-ert--fetch-node-tags node-id)))
    (should (equal (sort expected-tags #'string<) tags))))

(cl-defun org-tasktree-sync-normal-ert--assert-before-project (node &key level)
  "Assert NODE matches baseline project values at LEVEL."
  (org-tasktree-sync-ert--assert-node
   node
   :title "proj1 (before update)"
   :node-type "project"
   :todo-keyword "PROJ"
   :level level
   :priority "A"
   :scheduled "2026-01-10"
   :deadline "2026-01-20"
   :tags ":unit-test:upd:project:"
   :content "This is a project node."
   :status "OPEN")
  (should (null (org-tasktree-model-node-parent-id node)))
  (should (null (org-tasktree-model-node-project-id node)))
  (should (null (org-tasktree-model-node-phase-id node)))
  (org-tasktree-sync-normal-ert--assert-node-tags
   node
   org-tasktree-sync-normal-ert--before-project-tags))

(cl-defun org-tasktree-sync-normal-ert--assert-before-phase
    (node &key level parent-id project-id)
  "Assert NODE matches baseline phase values at LEVEL, PARENT-ID, and PROJECT-ID."
  (org-tasktree-sync-ert--assert-node
   node
   :title "phase (before update)"
   :node-type "phase"
   :todo-keyword "PHASE"
   :level level
   :priority "A"
   :scheduled "2026-01-10"
   :deadline "2026-01-20"
   :tags ":unit-test:upd:phase:"
   :content "This is a phase node."
   :status "OPEN"
   :parent-id parent-id
   :project-id project-id)
  (should (null (org-tasktree-model-node-phase-id node)))
  (org-tasktree-sync-normal-ert--assert-node-tags
   node
   org-tasktree-sync-normal-ert--before-phase-tags))

(cl-defun org-tasktree-sync-normal-ert--assert-before-group
    (node &key level parent-id project-id phase-id)
  "Assert NODE matches baseline group values at LEVEL, PARENT-ID, PROJECT-ID, and PHASE-ID."
  (org-tasktree-sync-ert--assert-node
   node
   :title "group (before update)"
   :node-type "group"
   :level level
   :priority "A"
   :scheduled "2026-01-10"
   :deadline "2026-01-20"
   :tags ":unit-test:upd:group:"
   :content "This is a group node."
   :status "OPEN"
   :parent-id parent-id
   :project-id project-id
   :phase-id phase-id)
  (should (null (org-tasktree-model-node-todo-keyword node)))
  (org-tasktree-sync-normal-ert--assert-node-tags
   node
   org-tasktree-sync-normal-ert--before-group-tags))

(cl-defun org-tasktree-sync-normal-ert--assert-before-task
    (node &key level parent-id project-id phase-id)
  "Assert NODE matches baseline task values at LEVEL, PARENT-ID, PROJECT-ID, and PHASE-ID."
  (org-tasktree-sync-ert--assert-node
   node
   :title "task (before update)"
   :node-type "task"
   :todo-keyword "TODO"
   :level level
   :priority "A"
   :scheduled "2026-01-10"
   :deadline "2026-01-20"
   :repeat "+1d"
   :tags ":unit-test:upd:task:"
   :content "This is a task node."
   :status "OPEN"
   :parent-id parent-id
   :project-id project-id
   :phase-id phase-id)
  (org-tasktree-sync-normal-ert--assert-node-tags
   node
   org-tasktree-sync-normal-ert--before-task-tags))

(cl-defun org-tasktree-sync-normal-ert--assert-updated-project (node &key level)
  "Assert NODE matches updated project values at LEVEL."
  (org-tasktree-sync-ert--assert-node
   node
   :title "proj (after update)"
   :node-type "project"
   :todo-keyword "PROJ"
   :level level
   :priority "B"
   :scheduled "2026-02-01"
   :deadline "2026-02-20"
   :tags ":proj:after:upd:"
   :content "proj update contents"
   :status "OPEN")
  (should (null (org-tasktree-model-node-parent-id node)))
  (should (null (org-tasktree-model-node-project-id node)))
  (should (null (org-tasktree-model-node-phase-id node)))
  (org-tasktree-sync-normal-ert--assert-node-tags
   node
   org-tasktree-sync-normal-ert--after-project-tags))

(cl-defun org-tasktree-sync-normal-ert--assert-updated-phase
    (node &key level parent-id project-id)
  "Assert NODE matches updated phase values at LEVEL, PARENT-ID, and PROJECT-ID."
  (org-tasktree-sync-ert--assert-node
   node
   :title "phase (after update)"
   :node-type "phase"
   :todo-keyword "PHASE"
   :level level
   :priority "B"
   :scheduled "2026-02-01"
   :deadline "2026-02-10"
   :tags ":phase:after:upd:"
   :content "phase update contents"
   :status "OPEN"
   :parent-id parent-id
   :project-id project-id)
  (should (null (org-tasktree-model-node-phase-id node)))
  (org-tasktree-sync-normal-ert--assert-node-tags
   node
   org-tasktree-sync-normal-ert--after-phase-tags))

(cl-defun org-tasktree-sync-normal-ert--assert-updated-group
    (node &key level parent-id project-id phase-id)
  "Assert NODE matches updated group values at LEVEL, PARENT-ID, PROJECT-ID, and PHASE-ID."
  (org-tasktree-sync-ert--assert-node
   node
   :title "group (after update)"
   :node-type "group"
   :level level
   :priority "B"
   :scheduled "2026-02-01"
   :deadline "2026-02-05"
   :tags ":group:after:upd:"
   :content "group update contents"
   :status "OPEN"
   :parent-id parent-id
   :project-id project-id
   :phase-id phase-id)
  (should (null (org-tasktree-model-node-todo-keyword node)))
  (org-tasktree-sync-normal-ert--assert-node-tags
   node
   org-tasktree-sync-normal-ert--after-group-tags))

(cl-defun org-tasktree-sync-normal-ert--assert-updated-task
    (node &key level parent-id project-id phase-id)
  "Assert NODE matches updated task values at LEVEL, PARENT-ID, PROJECT-ID, and PHASE-ID."
  (org-tasktree-sync-ert--assert-node
   node
   :title "task (after update)"
   :node-type "task"
   :todo-keyword "TODO"
   :level level
   :priority "B"
   :scheduled "2026-02-01"
   :repeat ".+2d"
   :tags ":task:after:upd:"
   :content "task update contents"
   :status "OPEN"
   :parent-id parent-id
   :project-id project-id
   :phase-id phase-id)
  (should (null (org-tasktree-model-node-deadline node)))
  (org-tasktree-sync-normal-ert--assert-node-tags
   node
   org-tasktree-sync-normal-ert--after-task-tags))

(ert-deftest org-tasktree-sync-normal-ert-ins1-full-path ()
  "Normal case: insert project/phase/group/task."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-01.org")
  (should (= 5 (org-tasktree-sync-ert--node-count)))
  (let* ((project (org-tasktree-sync-ert--fetch-node "proj1" "project"))
         (phase (org-tasktree-sync-ert--fetch-node "phase1" "phase"))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (project-id (org-tasktree-model-node-id project))
         (phase-id (org-tasktree-model-node-id phase))
         (group-id (org-tasktree-model-node-id group)))
    (should project)
    (should phase)
    (should group)
    (should task)
    (org-tasktree-sync-ert--assert-node
     project
     :title "proj1"
     :node-type "project"
     :todo-keyword "PROJ"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-20"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "proj1 notes"
     :status "OPEN")
    (should (null (org-tasktree-model-node-parent-id project)))
    (should (null (org-tasktree-model-node-project-id project)))
    (should (null (org-tasktree-model-node-phase-id project)))
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase1"
     :node-type "phase"
     :todo-keyword "PHASE"
     :level 2
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "phase1 notes"
     :status "OPEN"
     :parent-id project-id
     :project-id project-id)
    (should (null (org-tasktree-model-node-phase-id phase)))
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 3
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id phase-id
     :project-id project-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-todo-keyword group)))
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 4
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id project-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins2-no-phase ()
  "Normal case: insert project/group/task without phase."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-02.org")
  (should (= 4 (org-tasktree-sync-ert--node-count)))
  (let* ((project (org-tasktree-sync-ert--fetch-node "proj1" "project"))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (project-id (org-tasktree-model-node-id project))
         (group-id (org-tasktree-model-node-id group)))
    (should project)
    (should group)
    (should task)
    (org-tasktree-sync-ert--assert-node
     project
     :title "proj1"
     :node-type "project"
     :todo-keyword "PROJ"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-20"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "proj1 notes"
     :status "OPEN")
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 2
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id project-id
     :project-id project-id)
    (should (null (org-tasktree-model-node-phase-id group)))
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 3
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id project-id)
    (should (null (org-tasktree-model-node-phase-id task)))
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins3-no-group ()
  "Normal case: insert project/phase/task without group."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-03.org")
  (should (= 4 (org-tasktree-sync-ert--node-count)))
  (let* ((project (org-tasktree-sync-ert--fetch-node "proj1" "project"))
         (phase (org-tasktree-sync-ert--fetch-node "phase1" "phase"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (project-id (org-tasktree-model-node-id project))
         (phase-id (org-tasktree-model-node-id phase)))
    (should project)
    (should phase)
    (should task)
    (org-tasktree-sync-ert--assert-node
     project
     :title "proj1"
     :node-type "project"
     :todo-keyword "PROJ"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-20"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "proj1 notes"
     :status "OPEN")
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase1"
     :node-type "phase"
     :todo-keyword "PHASE"
     :level 2
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "phase1 notes"
     :status "OPEN"
     :parent-id project-id
     :project-id project-id)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 3
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id phase-id
     :project-id project-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins4-no-task ()
  "Normal case: insert project/phase/group without task."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-04.org")
  (should (= 4 (org-tasktree-sync-ert--node-count)))
  (let* ((project (org-tasktree-sync-ert--fetch-node "proj1" "project"))
         (phase (org-tasktree-sync-ert--fetch-node "phase1" "phase"))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (project-id (org-tasktree-model-node-id project))
         (phase-id (org-tasktree-model-node-id phase)))
    (should project)
    (should phase)
    (should group)
    (org-tasktree-sync-ert--assert-node
     project
     :title "proj1"
     :node-type "project"
     :todo-keyword "PROJ"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-20"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "proj1 notes"
     :status "OPEN")
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase1"
     :node-type "phase"
     :todo-keyword "PHASE"
     :level 2
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "phase1 notes"
     :status "OPEN"
     :parent-id project-id
     :project-id project-id)
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 3
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id phase-id
     :project-id project-id
     :phase-id phase-id)))

(ert-deftest org-tasktree-sync-normal-ert-ins5-nested-group ()
  "Normal case: insert nested groups."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-05.org")
  (should (= 6 (org-tasktree-sync-ert--node-count)))
  (let* ((project (org-tasktree-sync-ert--fetch-node "proj1" "project"))
         (phase (org-tasktree-sync-ert--fetch-node "phase1" "phase"))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (group-child (org-tasktree-sync-ert--fetch-node "group1-1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (project-id (org-tasktree-model-node-id project))
         (phase-id (org-tasktree-model-node-id phase))
         (group-id (org-tasktree-model-node-id group))
         (group-child-id (org-tasktree-model-node-id group-child)))
    (should project)
    (should phase)
    (should group)
    (should group-child)
    (should task)
    (org-tasktree-sync-ert--assert-node
     project
     :title "proj1"
     :node-type "project"
     :todo-keyword "PROJ"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-20"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "proj1 notes"
     :status "OPEN")
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase1"
     :node-type "phase"
     :todo-keyword "PHASE"
     :level 2
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "phase1 notes"
     :status "OPEN"
     :parent-id project-id
     :project-id project-id)
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 3
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id phase-id
     :project-id project-id
     :phase-id phase-id)
    (org-tasktree-sync-ert--assert-node
     group-child
     :title "group1-1"
     :node-type "group"
     :level 4
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id project-id
     :phase-id phase-id)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 5
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-child-id
     :project-id project-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins6-nested-task ()
  "Normal case: insert nested tasks."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-06.org")
  (should (= 6 (org-tasktree-sync-ert--node-count)))
  (let* ((project (org-tasktree-sync-ert--fetch-node "proj1" "project"))
         (phase (org-tasktree-sync-ert--fetch-node "phase1" "phase"))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (task-child (org-tasktree-sync-ert--fetch-node "task1-1" "task"))
         (project-id (org-tasktree-model-node-id project))
         (phase-id (org-tasktree-model-node-id phase))
         (group-id (org-tasktree-model-node-id group))
         (task-id (org-tasktree-model-node-id task)))
    (should project)
    (should phase)
    (should group)
    (should task)
    (should task-child)
    (org-tasktree-sync-ert--assert-node
     project
     :title "proj1"
     :node-type "project"
     :todo-keyword "PROJ"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-20"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "proj1 notes"
     :status "OPEN")
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase1"
     :node-type "phase"
     :todo-keyword "PHASE"
     :level 2
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "phase1 notes"
     :status "OPEN"
     :parent-id project-id
     :project-id project-id)
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 3
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id phase-id
     :project-id project-id
     :phase-id phase-id)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 4
     :priority "A"
     :scheduled "2026-01-01"
     :deadline "2026-01-03"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id project-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-repeat task)))
    (org-tasktree-sync-ert--assert-node
     task-child
     :title "task1-1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 5
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id task-id
     :project-id project-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-deadline task-child)))))

(ert-deftest org-tasktree-sync-normal-ert-ins7-phase-group-task ()
  "Normal case: insert phase/group/task under inbox."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-07.org")
  (should (= 4 (org-tasktree-sync-ert--node-count)))
  (let* ((inbox-id (org-tasktree-db-inbox-id))
         (phase (org-tasktree-sync-ert--fetch-node "phase1" "phase"))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (phase-id (org-tasktree-model-node-id phase))
         (group-id (org-tasktree-model-node-id group)))
    (should phase)
    (should group)
    (should task)
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase1"
     :node-type "phase"
     :todo-keyword "PHASE"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "phase1 notes"
     :status "OPEN"
     :parent-id inbox-id
     :project-id inbox-id)
    (should (null (org-tasktree-model-node-phase-id phase)))
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 2
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id phase-id
     :project-id inbox-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-todo-keyword group)))
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 3
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id inbox-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins8-group-task ()
  "Normal case: insert group/task under inbox."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-08.org")
  (should (= 3 (org-tasktree-sync-ert--node-count)))
  (let* ((inbox-id (org-tasktree-db-inbox-id))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (group-id (org-tasktree-model-node-id group)))
    (should group)
    (should task)
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id inbox-id
     :project-id inbox-id)
    (should (null (org-tasktree-model-node-phase-id group)))
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 2
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id inbox-id)
    (should (null (org-tasktree-model-node-phase-id task)))
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins9-task-only ()
  "Normal case: insert task under inbox."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-09.org")
  (should (= 2 (org-tasktree-sync-ert--node-count)))
  (let* ((inbox-id (org-tasktree-db-inbox-id))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task")))
    (should task)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 1
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id inbox-id
     :project-id inbox-id)
    (should (null (org-tasktree-model-node-phase-id task)))
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins10-nested-group ()
  "Normal case: insert nested groups under inbox."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-10.org")
  (should (= 4 (org-tasktree-sync-ert--node-count)))
  (let* ((inbox-id (org-tasktree-db-inbox-id))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (group-child (org-tasktree-sync-ert--fetch-node "group1-1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (group-id (org-tasktree-model-node-id group))
         (group-child-id (org-tasktree-model-node-id group-child)))
    (should group)
    (should group-child)
    (should task)
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id inbox-id
     :project-id inbox-id)
    (org-tasktree-sync-ert--assert-node
     group-child
     :title "group1-1"
     :node-type "group"
     :level 2
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id inbox-id)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 3
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-child-id
     :project-id inbox-id)
    (should (null (org-tasktree-model-node-phase-id task)))
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins11-nested-task ()
  "Normal case: insert nested tasks under inbox."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-11.org")
  (should (= 4 (org-tasktree-sync-ert--node-count)))
  (let* ((inbox-id (org-tasktree-db-inbox-id))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (task-child (org-tasktree-sync-ert--fetch-node "task1-1" "task"))
         (group-id (org-tasktree-model-node-id group))
         (task-id (org-tasktree-model-node-id task)))
    (should group)
    (should task)
    (should task-child)
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 1
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id inbox-id
     :project-id inbox-id)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 2
     :priority "A"
     :scheduled "2026-01-01"
     :deadline "2026-01-03"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id inbox-id)
    (should (null (org-tasktree-model-node-repeat task)))
    (org-tasktree-sync-ert--assert-node
     task-child
     :title "task1-1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 3
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id task-id
     :project-id inbox-id)
    (should (null (org-tasktree-model-node-phase-id task-child)))
    (should (null (org-tasktree-model-node-deadline task-child)))))

(ert-deftest org-tasktree-sync-normal-ert-ins12-project-with-ancestors ()
  "Normal case: ignore headings above project."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-12.org")
  (should (= 5 (org-tasktree-sync-ert--node-count)))
  (let* ((project (org-tasktree-sync-ert--fetch-node "proj1" "project"))
         (phase (org-tasktree-sync-ert--fetch-node "phase1" "phase"))
         (group (org-tasktree-sync-ert--fetch-node "group1" "group"))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (project-id (org-tasktree-model-node-id project))
         (phase-id (org-tasktree-model-node-id phase))
         (group-id (org-tasktree-model-node-id group)))
    (should project)
    (should phase)
    (should group)
    (should task)
    (org-tasktree-sync-ert--assert-node
     project
     :title "proj1"
     :node-type "project"
     :todo-keyword "PROJ"
     :level 3
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-20"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "proj1 notes"
     :status "OPEN")
    (should (null (org-tasktree-model-node-parent-id project)))
    (should (null (org-tasktree-model-node-project-id project)))
    (should (null (org-tasktree-model-node-phase-id project)))
    (org-tasktree-sync-ert--assert-node
     phase
     :title "phase1"
     :node-type "phase"
     :todo-keyword "PHASE"
     :level 4
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-10"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "phase1 notes"
     :status "OPEN"
     :parent-id project-id
     :project-id project-id)
    (should (null (org-tasktree-model-node-phase-id phase)))
    (org-tasktree-sync-ert--assert-node
     group
     :title "group1"
     :node-type "group"
     :level 5
     :priority "B"
     :scheduled "2026-01-01"
     :deadline "2026-01-05"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "group1 notes"
     :status "OPEN"
     :parent-id phase-id
     :project-id project-id
     :phase-id phase-id)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 6
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :content "task1 notes"
     :status "OPEN"
     :parent-id group-id
     :project-id project-id
     :phase-id phase-id)
    (should (null (org-tasktree-model-node-deadline task)))))

(ert-deftest org-tasktree-sync-normal-ert-ins13-content-org-syntax ()
  "Normal case: preserve org syntax in content."
  (org-tasktree-sync-ert--sync-file "sync-normal-ins-13.org")
  (should (= 2 (org-tasktree-sync-ert--node-count)))
  (let* ((inbox-id (org-tasktree-db-inbox-id))
         (task (org-tasktree-sync-ert--fetch-node "task1" "task"))
         (content (and task (org-tasktree-model-node-content task))))
    (should task)
    (org-tasktree-sync-ert--assert-node
     task
     :title "task1"
     :node-type "task"
     :todo-keyword "TODO"
     :level 1
     :priority "A"
     :scheduled "2026-01-01"
     :repeat "+1d"
     :tags (org-tasktree-sync-normal-ert--expect-tags)
     :status "OPEN"
     :parent-id inbox-id
     :project-id inbox-id)
    (should (stringp content))
    (should (string-match-p (regexp-quote "task1 items:") content))
    (should (string-match-p (regexp-quote "- [ ] item1") content))
    (should (string-match-p (regexp-quote "[[https://www.youtube.com/][YouTube]]")
                            content))
    (should (string-match-p (regexp-quote "#+begin_src python") content))
    (should (string-match-p (regexp-quote "print(total)") content))
    (should (string-match-p (regexp-quote "#+end_src") content))))

(ert-deftest org-tasktree-sync-normal-ert-upd1-full-path ()
  "Normal case: update project/phase/group/task."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (project-before (plist-get seed :project))
         (phase-before (plist-get seed :phase))
         (group-before (plist-get seed :group))
         (task-before (plist-get seed :task)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-01.org")
    (should (= 5 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group)))
      (should (= (org-tasktree-model-node-id project)
                 (org-tasktree-model-node-id project-before)))
      (should (= (org-tasktree-model-node-id phase)
                 (org-tasktree-model-node-id phase-before)))
      (should (= (org-tasktree-model-node-id group)
                 (org-tasktree-model-node-id group-before)))
      (should (= (org-tasktree-model-node-id task)
                 (org-tasktree-model-node-id task-before)))
      (org-tasktree-sync-normal-ert--assert-updated-project
       project
       :level 1)
      (org-tasktree-sync-normal-ert--assert-updated-phase
       phase
       :level 2
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-normal-ert--assert-updated-group
       group
       :level 3
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-updated-task
       task
       :level 4
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id))))

(ert-deftest org-tasktree-sync-normal-ert-upd2-nested-group ()
  "Normal case: update with nested group."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (group-before (plist-get seed :group))
         (task-before (plist-get seed :task)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-02.org")
    (should (= 6 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (group-child (org-tasktree-sync-ert--fetch-node
                         "group-1" "group"))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group))
           (group-child-id (org-tasktree-model-node-id group-child)))
      (should (= (org-tasktree-model-node-id group)
                 (org-tasktree-model-node-id group-before)))
      (should (= (org-tasktree-model-node-id task)
                 (org-tasktree-model-node-id task-before)))
      (org-tasktree-sync-normal-ert--assert-updated-project
       project
       :level 1)
      (org-tasktree-sync-normal-ert--assert-updated-phase
       phase
       :level 2
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-normal-ert--assert-updated-group
       group
       :level 3
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-ert--assert-node
       group-child
       :title "group-1"
       :node-type "group"
       :level 4
       :priority "B"
       :scheduled "2026-02-01"
       :deadline "2026-02-05"
       :tags ":group:after:upd:"
       :content "sub group contents"
       :status "OPEN"
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-node-tags
       group-child
       org-tasktree-sync-normal-ert--after-group-tags)
      (org-tasktree-sync-normal-ert--assert-updated-task
       task
       :level 5
       :parent-id group-child-id
       :project-id project-id
       :phase-id phase-id))))

(ert-deftest org-tasktree-sync-normal-ert-upd3-nested-task ()
  "Normal case: update with nested task."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (task-before (plist-get seed :task)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-03.org")
    (should (= 6 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (task-child (org-tasktree-sync-ert--fetch-node
                        "task-1" "task"))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group))
           (task-id (org-tasktree-model-node-id task)))
      (should (= (org-tasktree-model-node-id task)
                 (org-tasktree-model-node-id task-before)))
      (org-tasktree-sync-normal-ert--assert-updated-project
       project
       :level 1)
      (org-tasktree-sync-normal-ert--assert-updated-phase
       phase
       :level 2
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-normal-ert--assert-updated-group
       group
       :level 3
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-updated-task
       task
       :level 4
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-ert--assert-node
       task-child
       :title "task-1"
       :node-type "task"
       :todo-keyword "TODO"
       :level 5
       :priority "B"
       :scheduled "2026-02-01"
       :repeat ".+2d"
       :tags ":task:after:upd:"
       :content "sub-task contents"
       :status "OPEN"
       :parent-id task-id
       :project-id project-id
       :phase-id phase-id)
      (should (null (org-tasktree-model-node-deadline task-child)))
      (org-tasktree-sync-normal-ert--assert-node-tags
       task-child
       org-tasktree-sync-normal-ert--after-task-tags))))

(ert-deftest org-tasktree-sync-normal-ert-upd4-phase-group-task ()
  "Normal case: update phase/group/task without project."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (project-before (plist-get seed :project)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-04.org")
    (should (= 5 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group)))
      (should (= (org-tasktree-model-node-id project)
                 (org-tasktree-model-node-id project-before)))
      (org-tasktree-sync-normal-ert--assert-before-project
       project
       :level 1)
      (org-tasktree-sync-normal-ert--assert-updated-phase
       phase
       :level 1
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-normal-ert--assert-updated-group
       group
       :level 2
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-updated-task
       task
       :level 3
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id))))

(ert-deftest org-tasktree-sync-normal-ert-upd5-group-task ()
  "Normal case: update group/task without project or phase."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (project-before (plist-get seed :project))
         (phase-before (plist-get seed :phase)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-05.org")
    (should (= 5 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group)))
      (should (= (org-tasktree-model-node-id project)
                 (org-tasktree-model-node-id project-before)))
      (should (= (org-tasktree-model-node-id phase)
                 (org-tasktree-model-node-id phase-before)))
      (org-tasktree-sync-normal-ert--assert-before-project
       project
       :level 1)
      (org-tasktree-sync-normal-ert--assert-before-phase
       phase
       :level 2
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-normal-ert--assert-updated-group
       group
       :level 1
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-updated-task
       task
       :level 2
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id))))

(ert-deftest org-tasktree-sync-normal-ert-upd6-task-only ()
  "Normal case: update task only."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (project-before (plist-get seed :project))
         (phase-before (plist-get seed :phase))
         (group-before (plist-get seed :group)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-06.org")
    (should (= 5 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group)))
      (should (= (org-tasktree-model-node-id project)
                 (org-tasktree-model-node-id project-before)))
      (should (= (org-tasktree-model-node-id phase)
                 (org-tasktree-model-node-id phase-before)))
      (should (= (org-tasktree-model-node-id group)
                 (org-tasktree-model-node-id group-before)))
      (org-tasktree-sync-normal-ert--assert-before-project
       project
       :level 1)
      (org-tasktree-sync-normal-ert--assert-before-phase
       phase
       :level 2
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-normal-ert--assert-before-group
       group
       :level 3
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-updated-task
       task
       :level 1
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id))))

(ert-deftest org-tasktree-sync-normal-ert-upd7-nested-group ()
  "Normal case: update nested group without project or phase."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (project-before (plist-get seed :project))
         (phase-before (plist-get seed :phase))
         (group-before (plist-get seed :group))
         (task-before (plist-get seed :task)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-07.org")
    (should (= 6 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (group-child (org-tasktree-sync-ert--fetch-node
                         "group-1" "group"))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group))
           (group-child-id (org-tasktree-model-node-id group-child)))
      (should (= (org-tasktree-model-node-id project)
                 (org-tasktree-model-node-id project-before)))
      (should (= (org-tasktree-model-node-id phase)
                 (org-tasktree-model-node-id phase-before)))
      (should (= (org-tasktree-model-node-id group)
                 (org-tasktree-model-node-id group-before)))
      (should (= (org-tasktree-model-node-id task)
                 (org-tasktree-model-node-id task-before)))
      (org-tasktree-sync-normal-ert--assert-before-project
       project
       :level 1)
      (org-tasktree-sync-normal-ert--assert-before-phase
       phase
       :level 2
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-ert--assert-node
       group
       :title "group (after update)"
       :node-type "group"
       :level 1
       :priority "B"
       :scheduled "2026-02-01"
       :deadline "2026-02-05"
       :tags ":group:after:upd:"
       :content "group update contents"
       :status "OPEN"
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-node-tags
       group
       org-tasktree-sync-normal-ert--after-group-tags)
      (org-tasktree-sync-ert--assert-node
       group-child
       :title "group-1"
       :node-type "group"
       :level 2
       :priority "B"
       :scheduled "2026-02-01"
       :deadline "2026-02-05"
       :tags ":group:after:upd:"
       :content "sub group contents"
       :status "OPEN"
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-node-tags
       group-child
       org-tasktree-sync-normal-ert--after-group-tags)
      (org-tasktree-sync-ert--assert-node
       task
       :title "task (after update)"
       :node-type "task"
       :todo-keyword "TODO"
       :level 3
       :priority "B"
       :scheduled "2026-02-01"
       :repeat ".+2d"
       :tags ":task:after:upd:"
       :content "task update contents"
       :status "OPEN"
       :parent-id group-child-id
       :project-id project-id
       :phase-id phase-id)
      (should (null (org-tasktree-model-node-deadline task)))
      (org-tasktree-sync-normal-ert--assert-node-tags
       task
       org-tasktree-sync-normal-ert--after-task-tags))))

(ert-deftest org-tasktree-sync-normal-ert-upd8-nested-task ()
  "Normal case: update nested task without project or phase."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (project-before (plist-get seed :project))
         (phase-before (plist-get seed :phase))
         (group-before (plist-get seed :group))
         (task-before (plist-get seed :task)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-08.org")
    (should (= 6 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (task-child (org-tasktree-sync-ert--fetch-node
                        "task-1" "task"))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group))
           (task-id (org-tasktree-model-node-id task)))
      (should (= (org-tasktree-model-node-id project)
                 (org-tasktree-model-node-id project-before)))
      (should (= (org-tasktree-model-node-id phase)
                 (org-tasktree-model-node-id phase-before)))
      (should (= (org-tasktree-model-node-id group)
                 (org-tasktree-model-node-id group-before)))
      (should (= (org-tasktree-model-node-id task)
                 (org-tasktree-model-node-id task-before)))
      (org-tasktree-sync-normal-ert--assert-before-project
       project
       :level 1)
      (org-tasktree-sync-normal-ert--assert-before-phase
       phase
       :level 2
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-normal-ert--assert-before-group
       group
       :level 3
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-ert--assert-node
       task
       :title "task (after update)"
       :node-type "task"
       :todo-keyword "TODO"
       :level 1
       :priority "B"
       :scheduled "2026-02-01"
       :repeat ".+2d"
       :tags ":task:after:upd:"
       :content "task update contents"
       :status "OPEN"
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id)
      (should (null (org-tasktree-model-node-deadline task)))
      (org-tasktree-sync-normal-ert--assert-node-tags
       task
       org-tasktree-sync-normal-ert--after-task-tags)
      (org-tasktree-sync-ert--assert-node
       task-child
       :title "task-1"
       :node-type "task"
       :todo-keyword "TODO"
       :level 2
       :priority "B"
       :scheduled "2026-02-01"
       :repeat ".+2d"
       :tags ":task:after:upd:"
       :content "sub-task contents"
       :status "OPEN"
       :parent-id task-id
       :project-id project-id
       :phase-id phase-id)
      (should (null (org-tasktree-model-node-deadline task-child)))
      (org-tasktree-sync-normal-ert--assert-node-tags
       task-child
       org-tasktree-sync-normal-ert--after-task-tags))))

(ert-deftest org-tasktree-sync-normal-ert-upd9-project-with-ancestors ()
  "Normal case: update project with ancestor headings."
  (let ((seed (org-tasktree-sync-ert--seed-update-tree)))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-09.org")
    (should (= 5 (org-tasktree-sync-ert--node-count)))
    (let* ((project (org-tasktree-sync-ert--fetch-node-by-uid
                     org-tasktree-sync-normal-ert--upd-project-uid))
           (phase (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-phase-uid))
           (group (org-tasktree-sync-ert--fetch-node-by-uid
                   org-tasktree-sync-normal-ert--upd-group-uid))
           (task (org-tasktree-sync-ert--fetch-node-by-uid
                  org-tasktree-sync-normal-ert--upd-task-uid))
           (project-id (org-tasktree-model-node-id project))
           (phase-id (org-tasktree-model-node-id phase))
           (group-id (org-tasktree-model-node-id group)))
      (should seed)
      (org-tasktree-sync-normal-ert--assert-updated-project
       project
       :level 3)
      (org-tasktree-sync-normal-ert--assert-updated-phase
       phase
       :level 4
       :parent-id project-id
       :project-id project-id)
      (org-tasktree-sync-normal-ert--assert-updated-group
       group
       :level 5
       :parent-id phase-id
       :project-id project-id
       :phase-id phase-id)
      (org-tasktree-sync-normal-ert--assert-updated-task
       task
       :level 6
       :parent-id group-id
       :project-id project-id
       :phase-id phase-id))))

(provide 'org-tasktree-sync-normal-ert)
;;; org-tasktree-sync-normal-ert.el ends here
