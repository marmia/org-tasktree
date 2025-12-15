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
(require 'org-tasktree-db)

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
    (define-key map (kbd "TAB") #'org-tasktree-ui-edit-next-field)
    (define-key map (kbd "<backtab>") #'org-tasktree-ui-edit-previous-field)
    (define-key map (kbd "S-<tab>") #'org-tasktree-ui-edit-previous-field)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-edit-set-schedule)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-edit-set-deadline)
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
  (org-tasktree-ui--quit-edit-buffer)
  (message "org-tasktree: edit cancelled"))

(defun org-tasktree-ui--quit-edit-buffer ()
  "Close current edit buffer and its window."
  (let ((win (get-buffer-window (current-buffer))))
    (if win
        (quit-window 'kill win)
      (kill-buffer (current-buffer)))))

(defun org-tasktree-ui--field-regexp (field)
  "Return regexp capturing value part for FIELD line."
  (format "^\\(%s[[:space:]]*:[[:space:]]\\)\\([^(\n]*?\\)\\( (shortcut-key: [^)]+)\\)?$"
          (regexp-quote field)))

(defun org-tasktree-ui--form-start ()
  "Return buffer position just after the second '---' separator."
  (or (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^---$" nil t)
          (when (re-search-forward "^---$" nil t)
            (forward-line 1)
            (point))))
      (point-min)))

(defun org-tasktree-ui--value-or-placeholder (value)
  "Return VALUE or two-space placeholder when VALUE is empty."
  (if (and (stringp value) (> (length value) 0))
      value
    "  "))

(defun org-tasktree-ui--set-field (field value)
  "Replace FIELD line's value with VALUE, preserving trailing hints."
  (save-excursion
    (goto-char (org-tasktree-ui--form-start))
    (let ((regexp (org-tasktree-ui--field-regexp field)))
      (when (re-search-forward regexp nil t)
        (replace-match
         (concat (match-string 1)
                 (org-tasktree-ui--value-or-placeholder value)
                 (or (match-string 3) "")) t t nil 0)
        (org-tasktree-ui--mark-fields (org-tasktree-ui--fields-order))))))

(defun org-tasktree-ui--mark-fields (fields)
  "Add `org-tasktree-field' property over editable VALUES for FIELDS."
  (save-excursion
    (dolist (field fields)
      (let ((start-pos (org-tasktree-ui--form-start)))
        (when start-pos
          (goto-char start-pos)
          (let ((regexp (org-tasktree-ui--field-regexp field)))
            (when (re-search-forward regexp nil t)
              (let ((start (match-beginning 2))
                    (end (match-end 2)))
                ;; Only add property when the match is valid
                (when (and start end (< start end))
                  (add-text-properties start end `(org-tasktree-field ,field)))))))))))

(defun org-tasktree-ui--goto-field (field)
  "Move point to beginning of FIELD value."
  (let ((pos (text-property-any (point-min) (point-max)
                                'org-tasktree-field field)))
    (when pos (goto-char pos))))

(defun org-tasktree-ui--current-field ()
  "Return current field name at point or nil."
  (get-text-property (point) 'org-tasktree-field))

(defun org-tasktree-ui--fields-order ()
  "Return ordered field list for current edit buffer."
  (plist-get org-tasktree-ui--edit-metadata :fields))

(defun org-tasktree-ui--cycle-field (direction)
  "Move to next/previous field according to DIRECTION (1 or -1)."
  (let* ((fields (org-tasktree-ui--fields-order))
         (current (or (org-tasktree-ui--current-field) (car fields)))
         (idx (or (cl-position current fields :test #'string=) 0))
         (len (length fields))
         (next-idx (mod (+ idx direction) len))
         (target (nth next-idx fields)))
    (org-tasktree-ui--goto-field target)))

(defun org-tasktree-ui-edit-next-field ()
  "Jump to next editable field."
  (interactive)
  (org-tasktree-ui--cycle-field 1))

(defun org-tasktree-ui-edit-previous-field ()
  "Jump to previous editable field."
  (interactive)
  (org-tasktree-ui--cycle-field -1))

(defun org-tasktree-ui--read-date ()
  "Read date via `org-read-date' and normalize to YYYY-MM-DD."
  (org-read-date nil nil nil nil nil))

(defun org-tasktree-ui--set-date-field (field)
  "Set FIELD to date chosen by `org-read-date', default today."
  (let ((date (org-tasktree-ui--read-date)))
    (org-tasktree-ui--set-field field date)
    (org-tasktree-ui--goto-field field)))

(defun org-tasktree-ui-edit-set-schedule ()
  "Prompt date and set scheduled field."
  (interactive)
  (org-tasktree-ui--set-date-field "scheduled"))

(defun org-tasktree-ui-edit-set-deadline ()
  "Prompt date and set deadline field."
  (interactive)
  (org-tasktree-ui--set-date-field "deadline"))

(defun org-tasktree-ui--parse-form-buffer ()
  "Parse current edit buffer into plist.
Assumes key: value lines after the second '---' separator."
  (let (plist)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^---$" nil t)
        (re-search-forward "^---$" nil t))
      (dolist (line (split-string (buffer-substring-no-properties
                                   (point) (point-max)) "\n" t))
        (when (string-match
               "^[[:space:]]*\\([a-zA-Z0-9_]+\\)[[:space:]]*:[[:space:]]*\\(.*\\)$"
               line)
          (let* ((key (match-string 1 line))
                 (val (string-trim (match-string 2 line))))
            (setq plist (plist-put plist (intern (concat ":" key)) val))))))
    plist))

(defun org-tasktree-ui--field (plist key)
  "Return string value for KEY in PLIST or nil when empty."
  (let ((v (plist-get plist key)))
    (unless (and (stringp v) (string-empty-p v))
      v)))

(defun org-tasktree-ui--submit-project (data)
  "Submit project DATA plist to DB and return node."
  (let* ((title (org-tasktree-ui--field data :project_name))
         (uid (or (plist-get org-tasktree-ui--edit-metadata :uid)
                  (org-tasktree-ui--field data :uid)
                  (org-tasktree-db-generate-uid)))
         (priority (org-tasktree-ui--field data :priority))
         (scheduled-raw (org-tasktree-ui--field data :scheduled))
         (deadline-raw (org-tasktree-ui--field data :deadline))
         (tags (org-tasktree-ui--field data :tags)))
    (condition-case err
        (progn
          (unless (and title (not (string-empty-p title)))
            (user-error "Project name is required"))
          (let* ((scheduled (org-tasktree-ui--normalize-date scheduled-raw "scheduled"))
                 (deadline (org-tasktree-ui--normalize-date deadline-raw "deadline"))
                 (node (org-tasktree-model-node-create
                        :uid uid
                        :parent-id nil
                        :node-type "project"
                        :todo-keyword "PROJ"
                        :title title
                        :level 1
                        :priority priority
                        :scheduled scheduled
                        :deadline deadline
                        :tags tags
                        :status "OPEN"
                        :project-id nil
                        :phase-id nil)))
            (org-tasktree-db-commit-nodes (list node))
            node))
      (error
       (message (concat
                 "org-tasktree debug: submit project failed"
                 " title=%S uid=%S scheduled=%S deadline=%S plist=%S")
                title uid scheduled-raw deadline-raw data)
       (signal (car err) (cdr err))))))

(defun org-tasktree-ui-edit-accept ()
  "Accept current org-tasktree edit buffer (temporary stub)."
  (interactive)
  (let* ((data (org-tasktree-ui--parse-form-buffer))
         (meta org-tasktree-ui--edit-metadata)
         (type (plist-get meta :type))
         (uid (plist-get meta :uid)))
    (when uid
      (setq data (plist-put data :uid uid)))
    (pcase type
      ('project
       (let ((node (org-tasktree-ui--submit-project data)))
         (org-tasktree-ui--quit-edit-buffer)
         (message "org-tasktree: project saved uid=%s"
                  (org-tasktree-model-node-uid node))))
      (_
       (message "org-tasktree: Submit not implemented yet")))))

(defun org-tasktree-ui--render-form (type data)
  "Return form string for TYPE using DATA plist."
  (pcase type
    ('project
     (format (string-join
              '("---"
                "Input hints:"
                "TAB/S-TAB : move between fields (priority → scheduled → deadline → tags)"
                "priority  : A or B or C"
                "scheduled : yyyy-mm-dd (shortcut-key: C-c C-s)"
                "deadline  : yyyy-mm-dd (shortcut-key: C-c C-d)"
                "tags      : tag1, tag2, tag3"
                "---"
                "project_name : %s"
                "priority     : %s"
                "scheduled    : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :project-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :priority))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :scheduled))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :deadline))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :tags))))
    ('phase
     (format (string-join
              '("---"
                "Input hints:"
                "TAB/S-TAB : move between fields (priority → scheduled → deadline → tags)"
                "priority     : A or B or C"
                "scheduled    : yyyy-mm-dd (shortcut-key: C-c C-s)"
                "deadline     : yyyy-mm-dd (shortcut-key: C-c C-d)"
                "tags         : tag1, tag2, tag3"
                "---"
                "project_name : %s"
                "phase_name   : %s"
                "priority     : %s"
                "scheduled    : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :project-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :phase-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :priority))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :scheduled))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :deadline))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :tags))))
    ('task
     (format (string-join
              '("---"
                "Input hints:"
                "TAB/S-TAB : move between fields (priority → scheduled → deadline → tags)"
                "priority     : A or B or C"
                "scheduled    : yyyy-mm-dd (shortcut-key: C-c C-s)"
                "deadline     : yyyy-mm-dd (shortcut-key: C-c C-d)"
                "tags         : tag1, tag2, tag3"
                "---"
                "project_name : %s"
                "phase_name   : %s"
                "task_name    : %s"
                "priority     : %s"
                "scheduled    : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :project-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :phase-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :task-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :priority))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :scheduled))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :deadline))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :tags))))
    (_ "")))

(defun org-tasktree-ui--open-edit-buffer (type data)
  "Create and show edit buffer for TYPE with DATA plist."
  (let* ((buf (generate-new-buffer
               (format "*org-tasktree-edit %s*" type)))
         (form (org-tasktree-ui--render-form type data))
         (fields (pcase type
                   ('project '("priority" "scheduled" "deadline" "tags"))
                   ('phase '("priority" "scheduled" "deadline" "tags"))
                   ('task '("priority" "scheduled" "deadline" "tags"))
                   (_ nil))))
    (with-current-buffer buf
      (org-tasktree-ui-edit-mode)
      (erase-buffer)
      (insert form)
      (goto-char (point-min))
      (setq org-tasktree-ui--edit-metadata
            (list :type type
                  :data data
                  :fields fields
                  :uid (plist-get data :uid)))
      (org-tasktree-ui--mark-fields fields)
      (when fields
        (org-tasktree-ui--goto-field (car fields))))
    (pop-to-buffer buf)))

(defun org-tasktree-ui-edit-project (selection)
  "Open project edit buffer using SELECTION plist."
  (let* ((pid (plist-get selection :project-id))
         (node (org-tasktree-query-get-node-by-id pid))
         (data (if node
                   (list :uid (org-tasktree-model-node-uid node)
                         :project-title (org-tasktree-model-node-title node)
                         :priority (org-tasktree-model-node-priority node)
                         :scheduled (org-tasktree-model-node-scheduled node)
                         :deadline (org-tasktree-model-node-deadline node)
                         :tags (org-tasktree-model-node-tags node))
                 selection)))
    (org-tasktree-ui--open-edit-buffer 'project data)))

(defun org-tasktree-ui-edit-phase (selection)
  "Open phase edit buffer using SELECTION plist."
  (org-tasktree-ui--open-edit-buffer 'phase selection))

(defun org-tasktree-ui-edit-task (selection)
  "Open task edit buffer using SELECTION plist."
  (org-tasktree-ui--open-edit-buffer 'task selection))

(defun org-tasktree-ui--normalize-date (val field)
  "Return VAL when it matches YYYY-MM-DD, or nil if empty.
Signal `user-error' for invalid date.
FIELD is used in the error message."
  (let* ((trimmed (and val (string-trim val))))
    (cond
     ((or (null trimmed) (string-empty-p trimmed)) nil)
     ((org-tasktree-model--valid-date-p trimmed) trimmed)
     (t (user-error "%s must be YYYY-MM-DD or empty" field)))))

(provide 'org-tasktree-ui)
;;; org-tasktree-ui.el ends here
