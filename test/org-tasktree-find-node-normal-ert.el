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

(defun org-tasktree-find-node-normal-ert--select-node (title)
  "Return first node row for TITLE."
  (org-tasktree-db--with-db db
    (car (sqlite-select
          db
          (concat
           "SELECT id, uid, parent_id, title, tags "
           "FROM nodes WHERE title=? LIMIT 1;")
          (vector title)))))

(defun org-tasktree-find-node-normal-ert--candidate-type (cands path)
  "Return candidate type for PATH in CANDS."
  (let ((cand (seq-find
               (lambda (entry)
                 (equal (org-tasktree-ui-minibuffer--candidate-raw entry) path))
               cands)))
    (when cand
      (org-tasktree-ui-minibuffer--candidate-type cand))))

(ert-deftest org-tasktree-find-node-normal-ert-open-tree-candidates ()
  "Normal case: candidates include OPEN nodes only."
  (org-tasktree-find-node-ert--seed-open-tree)
  (org-tasktree-find-node-ert--insert-done-tree)
  (let (org-tasktree-find-node-ert--captured-cands)
    (cl-letf (((symbol-function 'org-tasktree-ui-minibuffer--completing-read)
               (lambda (_prompt cands &rest _args)
                 (setq org-tasktree-find-node-ert--captured-cands cands)
                 (car cands))))
      (org-tasktree-ui-minibuffer-read-node))
    (let ((raw (mapcar #'org-tasktree-ui-minibuffer--candidate-raw
                       org-tasktree-find-node-ert--captured-cands)))
      (should (member "work" raw))
      (should (member "work/design" raw))
      (should (member "work/design/alpha" raw))
      (should (member "work/design/alpha/task1" raw))
      (should (member "work/design/alpha/task1/child1" raw))
      (should (member "domain1" raw))
      (should (member "work/caps-project" raw))
      (should (member "work/caps-domain" raw))
      (should (member "work/mixed-phase-group" raw))
      (should (member "work/mixed-project-group" raw))
      (should (member "work/mixed-domain-project" raw))
      (should-not (member "archive" raw))
      (should-not (seq-some
                   (lambda (path)
                     (string-prefix-p "archive/" path))
                   raw))
      (should (equal 'project
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work")))
      (should (equal 'phase
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/design")))
      (should (equal 'group
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/design/alpha")))
      (should (equal 'task
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/design/alpha/task1")))
      (should (equal 'task
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/design/alpha/task1/child1")))
      (should (equal 'domain
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "domain1")))
      (should (equal 'project
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/caps-project")))
      (should (equal 'domain
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/caps-domain")))
      (should (equal 'phase
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/mixed-phase-group")))
      (should (equal 'project
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/mixed-project-group")))
      (should (equal 'domain
                     (org-tasktree-find-node-normal-ert--candidate-type
                      org-tasktree-find-node-ert--captured-cands
                      "work/mixed-domain-project"))))))

(ert-deftest org-tasktree-find-node-normal-ert-type-from-tags ()
  "Normal case: candidate type follows classification tag priority."
  (should (equal 'domain
                 (org-tasktree-ui-minibuffer--type-from-tags ":domain:")))
  (should (equal 'domain
                 (org-tasktree-ui-minibuffer--type-from-tags ":Domain:")))
  (should (equal 'domain
                 (org-tasktree-ui-minibuffer--type-from-tags
                  ":domain:project:")))
  (should (equal 'project
                 (org-tasktree-ui-minibuffer--type-from-tags
                  ":project:phase:")))
  (should (equal 'phase
                 (org-tasktree-ui-minibuffer--type-from-tags
                  ":phase:group:")))
  (should (equal 'task
                 (org-tasktree-ui-minibuffer--type-from-tags
                  ":unit_test:task1:"))))

(ert-deftest org-tasktree-find-node-normal-ert-unbound-domain-color ()
  "Normal case: domain color has a fallback when its defcustom is unbound."
  (let ((was-bound (boundp 'org-tasktree-ui-minibuffer-completion-color-domain))
        (old-value (and (boundp 'org-tasktree-ui-minibuffer-completion-color-domain)
                        org-tasktree-ui-minibuffer-completion-color-domain)))
    (unwind-protect
        (progn
          (makunbound 'org-tasktree-ui-minibuffer-completion-color-domain)
          (should (equal "#40C97E"
                         (org-tasktree-ui-minibuffer--completion-color
                          'domain)))
          (should (stringp
                   (org-tasktree-ui-minibuffer--make-path-candidate
                    "domain1"
                    'domain
                    nil
                    ":domain:"))))
      (when was-bound
        (setq org-tasktree-ui-minibuffer-completion-color-domain old-value)))))

(ert-deftest org-tasktree-find-node-normal-ert-new-node-selection ()
  "Normal case: new node selection returns parent metadata."
  (org-tasktree-find-node-ert--seed-open-tree)
  (let (org-tasktree-find-node-ert--captured-cands)
    (cl-letf (((symbol-function 'org-tasktree-ui-minibuffer--completing-read)
               (lambda (_prompt cands &rest _args)
                 (setq org-tasktree-find-node-ert--captured-cands cands)
                 "work/new-task")))
      (let ((result (org-tasktree-ui-minibuffer-read-node)))
        (should (equal (plist-get result :existing) nil))
        (should (equal (plist-get result :title) "new-task"))
        (let ((parent (plist-get result :parent-node)))
          (should parent)
          (should (equal (org-tasktree-model-node-title parent) "work")))))))

(ert-deftest org-tasktree-find-node-normal-ert-update-task ()
  "Normal case: submit updates an existing task."
  (org-tasktree-find-node-ert--seed-open-tree)
  (let* ((row (org-tasktree-find-node-normal-ert--select-node "task1"))
         (task-id (org-tasktree-find-node-normal-ert--row-nth row 0))
         (task-uid (org-tasktree-find-node-normal-ert--row-nth row 1))
         (parent-id (org-tasktree-find-node-normal-ert--row-nth row 2))
         (meta (org-tasktree-find-node-ert--meta-for-node
                :uid task-uid
                :parent-id parent-id)))
    (let ((table (make-hash-table :test 'equal)))
      (dolist (pair '((:title . "task1-upd")
                      (:priority . "A")
                      (:scheduled . "")
                      (:deadline . "")
                      (:repeat . "")
                      (:tags . "unit_test:task1_upd")
                      (:content . "updated content")))
        (puthash (car pair) (cdr pair) table))
      (cl-letf (((symbol-function 'org-tasktree-ui-widget--value)
                 (lambda (key) (gethash key table)))
                ((symbol-function 'org-tasktree-ui-widget--value-raw)
                 (lambda (key) (gethash key table))))
        (let ((node (org-tasktree-ui-edit--submit-widget meta)))
          (should (equal (org-tasktree-model-node-id node) task-id))
          (should (equal (org-tasktree-model-node-title node) "task1-upd")))))
    (let* ((updated (org-tasktree-find-node-normal-ert--select-node
                     "task1-upd"))
           (updated-tags (org-tasktree-find-node-normal-ert--row-nth updated 4)))
      (should (equal "task1-upd" (org-tasktree-find-node-normal-ert--row-nth updated 3)))
      (should (equal ":unit_test:task1_upd:" updated-tags)))))

(provide 'org-tasktree-find-node-normal-ert)
;;; org-tasktree-find-node-normal-ert.el ends here
