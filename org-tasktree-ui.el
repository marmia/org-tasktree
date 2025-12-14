;;; org-tasktree-ui.el --- UI helpers for org-tasktree -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; User-facing helpers for hierarchical `completing-read' flows
;; used by the find-* commands.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-tasktree-model)
(require 'org-tasktree-query)

(defconst org-tasktree-ui--path-sep " / "
  "Separator for hierarchical paths in `completing-read' candidates.")

(defun org-tasktree-ui--id-map (nodes)
  "Return hash map of id->node from NODES."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (node nodes table)
      (when (org-tasktree-model-node-id node)
        (puthash (org-tasktree-model-node-id node) node table)))))

(defun org-tasktree-ui--titles-path (node id-map)
  "Return list of titles from root to NODE using ID-MAP."
  (let ((titles '())
        (current node))
    (while current
      (push (org-tasktree-model-node-title current) titles)
      (setq current (gethash
                     (org-tasktree-model-node-parent-id current)
                     id-map)))
    titles))

(defun org-tasktree-ui--format-path (titles)
  "Join TITLES into a display path string."
  (string-join titles org-tasktree-ui--path-sep))

(defun org-tasktree-ui--collect (pred)
  "Collect nodes satisfying PRED with display strings and metadata.
Returns list of (DISPLAY . PLIST)."
  (let* ((nodes (org-tasktree-query-open-tree))
         (id-map (org-tasktree-ui--id-map nodes))
         (result '()))
    (dolist (node nodes)
      (when (funcall pred node)
        (let* ((titles (org-tasktree-ui--titles-path node id-map))
               (display (org-tasktree-ui--format-path titles)))
          (push (cons display
                      (list :node node
                            :titles titles))
                result))))
    (nreverse result)))

(defun org-tasktree-ui-read-project ()
  "Prompt for project via `completing-read'.
Returns plist: (:project-title STRING :project-id ID-or-nil)."
  (let* ((cands (org-tasktree-ui--collect
                 (lambda (n)
                   (equal (org-tasktree-model-node-node-type n)
                          "project"))))
         (choice (completing-read
                  "find project: " (mapcar #'car cands) nil nil)))
    (let* ((found (assoc choice cands))
           (node (plist-get (cdr found) :node)))
      (if node
          (list :project-title (org-tasktree-model-node-title node)
                :project-id (org-tasktree-model-node-id node))
        (list :project-title choice :project-id nil)))))

(defun org-tasktree-ui-read-phase ()
  "Prompt for phase path (project / phase).
Returns plist: (:project-title STR :project-id ID-or-nil
               :phase-title STR :phase-id ID-or-nil)."
  (let* ((cands (org-tasktree-ui--collect
                 (lambda (n)
                   (equal (org-tasktree-model-node-node-type n)
                          "phase"))))
         (choice (completing-read
                  "find phase: " (mapcar #'car cands) nil nil))
         (found (assoc choice cands)))
    (if found
        (let* ((node (plist-get (cdr found) :node))
               (titles (plist-get (cdr found) :titles)))
          (list :project-title (car titles)
                :project-id (org-tasktree-model-node-project-id node)
                :phase-title (cadr titles)
                :phase-id (org-tasktree-model-node-id node)))
      ;; new input: expect "Project / Phase"
      (let* ((parts (split-string choice org-tasktree-ui--path-sep))
             (proj (car parts))
             (phase (cadr parts)))
        (list :project-title proj
              :project-id nil
              :phase-title phase
              :phase-id nil)))))

(defun org-tasktree-ui-read-task ()
  "Prompt for task path (project / phase / [group] / task).
Returns plist with titles and ids when existing;
missing ids mean new."
  (let* ((cands (org-tasktree-ui--collect
                 (lambda (n)
                   (member (org-tasktree-model-node-node-type n)
                           '("task" "group")))))
         (choice (completing-read
                  "find task: " (mapcar #'car cands) nil nil))
         (found (assoc choice cands)))
    (if found
        (let* ((node (plist-get (cdr found) :node))
               (titles (plist-get (cdr found) :titles))
               (project (nth 0 titles))
               (phase (nth 1 titles))
               (task (car (last titles))))
          (list :project-title project
                :project-id (org-tasktree-model-node-project-id node)
                :phase-title phase
                :phase-id (org-tasktree-model-node-phase-id node)
                :task-title task
                :task-id (org-tasktree-model-node-id node)))
      ;; new input: best-effort parse project/phase/task
      (let* ((parts (split-string choice org-tasktree-ui--path-sep))
             (proj (nth 0 parts))
             (phase (nth 1 parts))
             (task (car (last parts))))
        (list :project-title proj
              :project-id nil
              :phase-title phase
              :phase-id nil
              :task-title task
              :task-id nil)))))

(provide 'org-tasktree-ui)
;;; org-tasktree-ui.el ends here
