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

(defvar-local org-tasktree-ui--edit-metadata nil
  "Metadata plist for current edit buffer.")

(defvar org-tasktree-ui-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-edit-cancel)
    map)
  "Keymap for `org-tasktree-ui-edit-mode'.")

(define-derived-mode org-tasktree-ui-edit-mode text-mode
  "org-tasktree-edit"
  "Edit buffer for org-tasktree entities."
  (setq-local buffer-read-only nil)
  (setq-local truncate-lines nil))

(defun org-tasktree-ui-edit-cancel ()
  "Cancel current org-tasktree edit buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (message "org-tasktree: edit cancelled"))

(defun org-tasktree-ui--parse-form-buffer ()
  "Parse current edit buffer into plist.
Assumes key: value lines after hints."
  (let (plist)
    (goto-char (point-min))
    (while (re-search-forward "^\\([a-z_]+\\)\\s-*: \\(.*\\)$" nil t)
      (let ((key (match-string 1))
            (val (string-trim (match-string 2))))
        (setq plist (plist-put plist (intern (concat ":" key)) val))))
    plist))

(defun org-tasktree-ui-edit-accept ()
  "Accept current org-tasktree edit buffer (temporary stub)."
  (interactive)
  (let* ((data (org-tasktree-ui--parse-form-buffer))
         (type (plist-get org-tasktree-ui--edit-metadata :type)))
    (message "org-tasktree: [%s] %S" type data)
    (kill-buffer (current-buffer))))

(defun org-tasktree-ui--render-form (type data)
  "Return form string for TYPE using DATA plist."
  (pcase type
    ('project
     (format (string-join
              '("---"
                "Input hints:"
                "priority     : A or B or C"
                "schedule     : yyyy-mm-dd"
                "deadline     : yyyy-mm-dd"
                "tags         : tag1, tag2, tag3"
                "---"
                "type         : project"
                "uid          : %s"
                "project_name : %s"
                "priority     : %s"
                "schedule     : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (or (plist-get data :uid) "")
             (or (plist-get data :project-title) "")
             (or (plist-get data :priority) "")
             (or (plist-get data :schedule) "")
             (or (plist-get data :deadline) "")
             (or (plist-get data :tags) "")))
    ('phase
     (format (string-join
              '("---"
                "Input hints:"
                "priority     : A or B or C"
                "schedule     : yyyy-mm-dd"
                "deadline     : yyyy-mm-dd"
                "tags         : tag1, tag2, tag3"
                "---"
                "type         : phase"
                "uid          : %s"
                "project_name : %s"
                "phase_name   : %s"
                "priority     : %s"
                "schedule     : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (or (plist-get data :uid) "")
             (or (plist-get data :project-title) "")
             (or (plist-get data :phase-title) "")
             (or (plist-get data :priority) "")
             (or (plist-get data :schedule) "")
             (or (plist-get data :deadline) "")
             (or (plist-get data :tags) "")))
    ('task
     (format (string-join
              '("---"
                "Input hints:"
                "priority     : A or B or C"
                "schedule     : yyyy-mm-dd"
                "deadline     : yyyy-mm-dd"
                "tags         : tag1, tag2, tag3"
                "---"
                "type         : task"
                "uid          : %s"
                "project_name : %s"
                "phase_name   : %s"
                "task_name    : %s"
                "priority     : %s"
                "schedule     : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (or (plist-get data :uid) "")
             (or (plist-get data :project-title) "")
             (or (plist-get data :phase-title) "")
             (or (plist-get data :task-title) "")
             (or (plist-get data :priority) "")
             (or (plist-get data :schedule) "")
             (or (plist-get data :deadline) "")
             (or (plist-get data :tags) "")))
    (_ "")))

(defun org-tasktree-ui--open-edit-buffer (type data)
  "Create and show edit buffer for TYPE with DATA plist."
  (let* ((buf (generate-new-buffer
               (format "*org-tasktree-edit %s*" type)))
         (form (org-tasktree-ui--render-form type data)))
    (with-current-buffer buf
      (org-tasktree-ui-edit-mode)
      (erase-buffer)
      (insert form)
      (goto-char (point-min))
      (setq org-tasktree-ui--edit-metadata
            (list :type type :data data)))
    (pop-to-buffer buf)))

(defun org-tasktree-ui-edit-project (selection)
  "Open project edit buffer using SELECTION plist."
  (org-tasktree-ui--open-edit-buffer 'project selection))

(defun org-tasktree-ui-edit-phase (selection)
  "Open phase edit buffer using SELECTION plist."
  (org-tasktree-ui--open-edit-buffer 'phase selection))

(defun org-tasktree-ui-edit-task (selection)
  "Open task edit buffer using SELECTION plist."
  (org-tasktree-ui--open-edit-buffer 'task selection))

(provide 'org-tasktree-ui)
;;; org-tasktree-ui.el ends here
