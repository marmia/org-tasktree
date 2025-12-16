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
(require 'org)
(require 'calendar)
(require 'widget)
(require 'wid-edit)
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
Return plist with titles and ids when existing; missing ids mean new."
  (let* ((cands (org-tasktree-ui--collect
                 (lambda (n)
                   (equal (org-tasktree-model-node-node-type n)
                          "task"))))
         (choice (completing-read
                  "find task: " (mapcar #'car cands) nil nil))
         (found (assoc choice cands)))
    (if found
        (let* ((node (plist-get (cdr found) :node))
               (titles (plist-get (cdr found) :titles))
               (project (car titles))
               (phase (nth 1 titles))
               (group (and (> (length titles) 3)
                           (nth (- (length titles) 2) titles)))
               (task (car (last titles))))
          (list :project-title project
                :project-id (org-tasktree-model-node-project-id node)
                :phase-title phase
                :phase-id (org-tasktree-model-node-phase-id node)
                :group-title group
                :task-title task
                :task-id (org-tasktree-model-node-id node)))
      ;; new input: best-effort parse project/phase/task
      (let* ((parts (split-string choice org-tasktree-ui--path-sep))
             (proj (nth 0 parts))
             (phase (nth 1 parts))
             (group (and (> (length parts) 3)
                         (nth (- (length parts) 2) parts)))
             (task (car (last parts))))
        (list :project-title proj
              :project-id nil
              :phase-title phase
              :phase-id nil
              :group-title group
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

(defvar org-tasktree-ui--widget-field-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-field-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-widget-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-widget-edit-cancel)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-widget-edit-set-scheduled)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-widget-edit-set-deadline)
    map)
  "Keymap used inside widget editable fields.")

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
  (let* ((buf (current-buffer))
         (win (get-buffer-window buf t)))
    (when (and win (eq (window-buffer win) buf))
      (quit-window 'kill win))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

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
         data)
    (setq data
          (if node
              (list :type 'project
                    :uid (org-tasktree-model-node-uid node)
                    :title (org-tasktree-model-node-title node)
                    :priority (org-tasktree-model-node-priority node)
                    :scheduled (org-tasktree-model-node-scheduled node)
                    :deadline (org-tasktree-model-node-deadline node)
                    :tags (org-tasktree-model-node-tags node)
                    :path-titles nil
                    :project-title (org-tasktree-model-node-title node)
                    :project-id (org-tasktree-model-node-id node))
            (list :type 'project
                  :uid nil
                  :title (plist-get selection :project-title)
                  :priority nil
                  :scheduled nil
                  :deadline nil
                  :tags nil
                  :path-titles nil
                  :project-title (plist-get selection :project-title)
                  :project-id nil)))
    (org-tasktree-ui--open-widget-edit-buffer data)))

(defun org-tasktree-ui-edit-phase (selection)
  "Open phase edit buffer using SELECTION plist."
  (let* ((phase-id (plist-get selection :phase-id))
         (node (org-tasktree-query-get-node-by-id phase-id))
         (project-title (plist-get selection :project-title))
         (data (if node
                   (list :type 'phase
                         :uid (org-tasktree-model-node-uid node)
                         :title (org-tasktree-model-node-title node)
                         :priority (org-tasktree-model-node-priority node)
                         :scheduled (org-tasktree-model-node-scheduled node)
                         :deadline (org-tasktree-model-node-deadline node)
                         :tags (org-tasktree-model-node-tags node)
                         :path-titles (list project-title)
                         :project-title project-title
                         :project-id (org-tasktree-model-node-project-id node)
                         :phase-id (org-tasktree-model-node-id node))
                 (list :type 'phase
                       :uid nil
                       :title (plist-get selection :phase-title)
                       :priority nil
                       :scheduled nil
                       :deadline nil
                       :tags nil
                       :path-titles (list project-title)
                       :project-title project-title
                       :project-id (plist-get selection :project-id)
                       :phase-id nil))))
    (org-tasktree-ui--open-widget-edit-buffer data)))

(defun org-tasktree-ui-edit-task (selection)
  "Open task edit buffer using SELECTION plist."
  (let* ((task-id (plist-get selection :task-id))
         (node (org-tasktree-query-get-node-by-id task-id))
         (project-title (plist-get selection :project-title))
         (phase-title (plist-get selection :phase-title))
         (group-title (plist-get selection :group-title))
         (path-titles (delq nil (list project-title phase-title group-title)))
         (data (if node
                   (let ((parent-id (org-tasktree-model-node-parent-id node)))
                     (list :type 'task
                           :uid (org-tasktree-model-node-uid node)
                           :title (org-tasktree-model-node-title node)
                           :priority (org-tasktree-model-node-priority node)
                           :scheduled (org-tasktree-model-node-scheduled node)
                           :deadline (org-tasktree-model-node-deadline node)
                           :tags (org-tasktree-model-node-tags node)
                           :path-titles path-titles
                           :project-title project-title
                           :project-id (org-tasktree-model-node-project-id node)
                           :phase-title phase-title
                           :phase-id (org-tasktree-model-node-phase-id node)
                           :group-title group-title
                           :parent-id parent-id
                           :task-id (org-tasktree-model-node-id node)))
                 (list :type 'task
                       :uid nil
                       :title (plist-get selection :task-title)
                       :priority nil
                       :scheduled nil
                       :deadline nil
                       :tags nil
                       :path-titles path-titles
                       :project-title project-title
                       :project-id (plist-get selection :project-id)
                       :phase-title phase-title
                       :phase-id (plist-get selection :phase-id)
                       :group-title group-title
                       :parent-id nil
                       :task-id nil))))
    (org-tasktree-ui--open-widget-edit-buffer data)))

(defun org-tasktree-ui--normalize-date (val field)
  "Return VAL when it matches YYYY-MM-DD, or nil if empty.
Signal `user-error' for invalid date.  FIELD is used in the error message."
  (let ((trimmed (and val (string-trim val))))
    (cond
     ((or (null trimmed) (string-empty-p trimmed)) nil)
     ((org-tasktree-model--valid-date-p trimmed) trimmed)
     (t (user-error "Invalid %s: must be YYYY-MM-DD or empty" field)))))

(defvar-local org-tasktree-ui--widget-widgets nil
  "Plist of widget objects for current edit buffer.")

(defvar org-tasktree-ui-widget-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-widget-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-widget-edit-cancel)
    (define-key map (kbd "TAB") #'widget-forward)
    (define-key map (kbd "<backtab>") #'widget-backward)
    (define-key map (kbd "S-<tab>") #'widget-backward)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-widget-edit-set-scheduled)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-widget-edit-set-deadline)
    map)
  "Keymap for `org-tasktree-ui-widget-edit-mode'.")

(define-derived-mode org-tasktree-ui-widget-edit-mode special-mode
  "org-tasktree-edit"
  "Edit buffer for org-tasktree entities."
  (use-local-map org-tasktree-ui-widget-edit-mode-map)
  (setq truncate-lines t))

(defun org-tasktree-ui-widget-edit-cancel ()
  "Cancel current org-tasktree widget edit buffer."
  (interactive)
  (org-tasktree-ui--quit-edit-buffer)
  (message "org-tasktree: edit cancelled"))

(defun org-tasktree-ui--widget-get (key)
  "Return widget object for KEY."
  (plist-get org-tasktree-ui--widget-widgets key))

(defun org-tasktree-ui--widget-value (key)
  "Return current string value for KEY widget."
  (let ((w (org-tasktree-ui--widget-get key)))
    (when w
      (let ((v (widget-value w)))
        (and (stringp v) (string-trim v))))))

(defun org-tasktree-ui--validate-title (value)
  "Return normalized title string from VALUE or signal `user-error'."
  (let ((title (and value (string-trim value))))
    (unless (and title (not (string-empty-p title)))
      (user-error "Title is required"))
    (when (string-match-p "[[:cntrl:]]" title)
      (user-error "Title must not include control characters"))
    title))

(defun org-tasktree-ui--validate-priority (value)
  "Return normalized priority string from VALUE or nil."
  (let ((p (and value (string-trim value))))
    (cond
     ((or (null p) (string-empty-p p)) nil)
     ((string-match-p "\\`[[:alnum:]]\\'" p) p)
     (t (user-error "Priority must be a single alphanumeric character")))))

(defun org-tasktree-ui--validate-tags (value)
  "Return normalized tags string from VALUE or nil."
  (let ((t0 (and value (string-trim value))))
    (cond
     ((or (null t0) (string-empty-p t0)) nil)
     ((string-match-p
       "\\`\\(?::\\)?[A-Za-z0-9_-]+\\(?::[A-Za-z0-9_-]+\\)*\\(?::\\)?\\'"
       t0)
      (car (org-tasktree-model-normalize-tags t0)))
     (t (user-error
         "Tags must be like tag1:tag2 or :tag1:tag2: using [A-Za-z0-9_-]")))))

(defun org-tasktree-ui--days-in-month (year month)
  "Return number of days for YEAR and MONTH."
  (calendar-last-day-of-month month year))

(defun org-tasktree-ui--ymd-to-days (year month day)
  "Return day count for YEAR MONTH DAY at local midnight."
  (time-to-days (encode-time 0 0 0 day month year)))

(defun org-tasktree-ui--today-ymd ()
  "Return today's (YEAR MONTH DAY) list in local time."
  (let ((d (decode-time (current-time))))
    (list (nth 5 d) (nth 4 d) (nth 3 d))))

(defun org-tasktree-ui--format-ymd (year month day)
  "Return YYYY-MM-DD string for YEAR, MONTH, and DAY."
  (format "%04d-%02d-%02d" year month day))

(defun org-tasktree-ui--resolve-month-day (month day)
  "Resolve MONTH/DAY to nearest future date (including today)."
  (pcase-let* ((`(,ty ,tm ,td) (org-tasktree-ui--today-ymd))
               (today (org-tasktree-ui--ymd-to-days ty tm td))
               (max-years 30)
               (found (catch 'found
                        (dotimes (i max-years)
                          (let ((year (+ ty i)))
                            (when (<= day (org-tasktree-ui--days-in-month year month))
                              (let ((cand (org-tasktree-ui--ymd-to-days year month day)))
                                (when (>= cand today)
                                  (throw 'found (list year month day)))))))
                        nil)))
    (unless found
      (user-error "Invalid date: %02d-%02d" month day))
    found))

(defun org-tasktree-ui--resolve-day-of-month (day)
  "Resolve DAY to nearest future date (including today)."
  (pcase-let* ((`(,ty ,tm ,td) (org-tasktree-ui--today-ymd))
               (today (org-tasktree-ui--ymd-to-days ty tm td))
               (max-months 36)
               (found (catch 'found
                        (dotimes (i max-months)
                          (let* ((m (+ tm i))
                                 (year (+ ty (/ (1- m) 12)))
                                 (month (1+ (mod (1- m) 12))))
                            (when (<= day (org-tasktree-ui--days-in-month year month))
                              (let ((cand (org-tasktree-ui--ymd-to-days year month day)))
                                (when (>= cand today)
                                  (throw 'found (list year month day)))))))
                        nil)))
    (unless found
      (user-error "Invalid date: %d" day))
    found))

(defun org-tasktree-ui--parse-date-input (value field)
  "Parse VALUE for FIELD and return YYYY-MM-DD or nil.
Accepts YYYY-MM-DD, YYYY/MM/DD, MM-DD, MM/DD, and DD forms."
  (let ((s (and value (string-trim value))))
    (cond
     ((or (null s) (string-empty-p s)) nil)
     ((string-match
       "\\`\\([0-9]\\{4\\}\\)[-/]\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\'"
       s)
      (let* ((y (string-to-number (match-string 1 s)))
             (m (string-to-number (match-string 2 s)))
             (d (string-to-number (match-string 3 s))))
        (unless (and (<= 1 m 12)
                     (<= 1 d (org-tasktree-ui--days-in-month y m)))
          (user-error "Invalid %s: date is not valid" field))
        (org-tasktree-ui--format-ymd y m d)))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\'" s)
      (let* ((m (string-to-number (match-string 1 s)))
             (d (string-to-number (match-string 2 s))))
        (unless (<= 1 m 12)
          (user-error "Invalid %s: month is not valid" field))
        (pcase-let ((`(,y ,mm ,dd) (org-tasktree-ui--resolve-month-day m d)))
          (org-tasktree-ui--format-ymd y mm dd))))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)\\'" s)
      (let ((d (string-to-number (match-string 1 s))))
        (unless (<= 1 d 31)
          (user-error "Invalid %s: day is not valid" field))
        (pcase-let ((`(,y ,m ,dd) (org-tasktree-ui--resolve-day-of-month d)))
          (org-tasktree-ui--format-ymd y m dd))))
     (t (user-error "Invalid %s: date format is not supported" field)))))

(defun org-tasktree-ui--validate-schedule-deadline (scheduled deadline)
  "Validate SCHEDULED and DEADLINE ordering."
  (when (and scheduled deadline (string-lessp deadline scheduled))
    (user-error "Deadline must be >= scheduled")))

(defun org-tasktree-ui--db-select-int (sql params)
  "Return first column of first row for SQL with PARAMS, or nil."
  (org-tasktree-db--with-db db
    (let ((rows (sqlite-select db sql params)))
      (when rows
        (let ((row (car rows)))
          (if (vectorp row) (aref row 0) (car row)))))))

(defun org-tasktree-ui--db-project-id (title)
  "Return project id for TITLE or nil."
  (org-tasktree-ui--db-select-int
   (concat
    "SELECT id FROM nodes "
    "WHERE node_type='project' AND status='OPEN' AND title=? "
    "ORDER BY id ASC LIMIT 1;")
   (vector title)))

(defun org-tasktree-ui--db-phase-id (project-id title)
  "Return phase id for PROJECT-ID and TITLE or nil."
  (org-tasktree-ui--db-select-int
   (concat
    "SELECT id FROM nodes "
    "WHERE node_type='phase' AND status='OPEN' AND project_id=? AND title=? "
    "ORDER BY id ASC LIMIT 1;")
   (vector project-id title)))

(defun org-tasktree-ui--db-group-id (project-id phase-id title)
  "Return group id for PROJECT-ID PHASE-ID and TITLE or nil."
  (org-tasktree-ui--db-select-int
   (concat
    "SELECT id FROM nodes "
    "WHERE node_type='group' AND status='OPEN' "
    "AND project_id=? AND phase_id=? AND title=? "
    "ORDER BY id ASC LIMIT 1;")
   (vector project-id phase-id title)))

(defun org-tasktree-ui--submit-widget (meta)
  "Submit widget edit META to DB and return saved node."
  (let* ((type (plist-get meta :type))
         (uid (or (plist-get meta :uid) (org-tasktree-db-generate-uid)))
         (title (org-tasktree-ui--validate-title
                 (org-tasktree-ui--widget-value :title)))
         (priority (org-tasktree-ui--validate-priority
                    (org-tasktree-ui--widget-value :priority)))
         (scheduled (org-tasktree-ui--parse-date-input
                     (org-tasktree-ui--widget-value :scheduled)
                     "scheduled"))
         (deadline (org-tasktree-ui--parse-date-input
                    (org-tasktree-ui--widget-value :deadline)
                    "deadline"))
         (tags (org-tasktree-ui--validate-tags
                (org-tasktree-ui--widget-value :tags))))
    (org-tasktree-ui--validate-schedule-deadline scheduled deadline)
    (pcase type
      ('project
       (let ((node (org-tasktree-model-node-create
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
      ('phase
       (let* ((project-title (plist-get meta :project-title))
              (project-id (or (plist-get meta :project-id)
                              (and project-title
                                   (org-tasktree-ui--db-project-id
                                    project-title)))))
         (unless project-id
           (user-error "Project must exist before creating a phase"))
         (let ((node (org-tasktree-model-node-create
                      :uid uid
                      :parent-id project-id
                      :node-type "phase"
                      :todo-keyword "PHASE"
                      :title title
                      :level 2
                      :priority priority
                      :scheduled scheduled
                      :deadline deadline
                      :tags tags
                      :status "OPEN"
                      :project-id project-id
                      :phase-id nil)))
           (org-tasktree-db-commit-nodes (list node))
           node)))
      ('task
       (let* ((project-title (plist-get meta :project-title))
              (phase-title (plist-get meta :phase-title))
              (group-title (plist-get meta :group-title))
              (project-id (or (plist-get meta :project-id)
                              (and project-title
                                   (org-tasktree-ui--db-project-id
                                    project-title))))
              (phase-id (or (plist-get meta :phase-id)
                            (and project-id phase-title
                                 (org-tasktree-ui--db-phase-id
                                  project-id phase-title))))
              (group-id (and group-title project-id phase-id
                             (org-tasktree-ui--db-group-id
                              project-id phase-id group-title)))
              (parent-id (or (plist-get meta :parent-id)
                             group-id
                             phase-id)))
         (unless project-id
           (user-error "Project must exist before creating a task"))
         (unless phase-id
           (user-error "Phase must exist before creating a task"))
         (when (and group-title (not (numberp group-id)))
           (user-error "Group must exist before creating a task"))
         (let ((node (org-tasktree-model-node-create
                      :uid uid
                      :parent-id parent-id
                      :node-type "task"
                      :todo-keyword "TODO"
                      :title title
                      :level (if group-title 4 3)
                      :priority priority
                      :scheduled scheduled
                      :deadline deadline
                      :tags tags
                      :status "OPEN"
                      :project-id project-id
                      :phase-id phase-id)))
           (org-tasktree-db-commit-nodes (list node))
           node)))
      (_ (user-error "Unknown edit type: %S" type)))))

(defun org-tasktree-ui-widget-edit-accept ()
  "Commit current widget edit buffer to DB."
  (interactive)
  (let* ((meta org-tasktree-ui--edit-metadata)
         (node (org-tasktree-ui--submit-widget meta)))
    (org-tasktree-ui--quit-edit-buffer)
    (message "org-tasktree: saved uid=%s" (org-tasktree-model-node-uid node))))

(defun org-tasktree-ui--widget-insert-field (label key value hint)
  "Insert one editable field for LABEL and return widget.
KEY, VALUE, and HINT configure the created widget."
  (widget-insert (format "%-10s " (concat label ":")))
  (let ((w (widget-create 'editable-field
                          :size 40
                          :format "%v"
                          :keymap org-tasktree-ui--widget-field-keymap
                          :value (or value ""))))
    (when hint
      (widget-insert " " (propertize hint 'face 'shadow)))
    (widget-insert "\n")
    (setq org-tasktree-ui--widget-widgets
          (plist-put org-tasktree-ui--widget-widgets key w))
    w))

(defun org-tasktree-ui--render-widget-form (meta)
  "Render widget edit UI for META into current buffer."
  (let* ((path (plist-get meta :path-titles))
         (path-str (if (and (listp path) path)
                       (string-join path " > ")
                     "(root)")))
    (widget-insert (propertize (format "Path: %s\n\n" path-str)
                               'face 'bold))
    (org-tasktree-ui--widget-insert-field "title" :title
                                          (plist-get meta :title)
                                          "required")
    (org-tasktree-ui--widget-insert-field
     "priority" :priority (plist-get meta :priority)
     "[A-Za-z0-9] (v0.2: org priority settings)")
    (org-tasktree-ui--widget-insert-field
     "scheduled" :scheduled (plist-get meta :scheduled)
     "YYYY-MM-DD | YYYY/MM/DD | MM-DD | MM/DD | DD  (C-c C-s)")
    (org-tasktree-ui--widget-insert-field
     "deadline" :deadline (plist-get meta :deadline)
     "YYYY-MM-DD | YYYY/MM/DD | MM-DD | MM/DD | DD  (C-c C-d)")
    (org-tasktree-ui--widget-insert-field
     "tags" :tags (plist-get meta :tags)
     ":tag1:tag2: | tag1:tag2  ([A-Za-z0-9_-], ':' separated)")
    (widget-insert "\n")
    (widget-insert (propertize "C-c C-c: commit,  C-c C-k: cancel\n"
                               'face 'shadow))))

(defun org-tasktree-ui--set-widget-date (key field)
  "Read date via `org-read-date' and set widget KEY for FIELD."
  (condition-case nil
      (let* ((input (org-read-date nil nil nil nil nil))
             (date (org-tasktree-ui--parse-date-input input field))
             (w (org-tasktree-ui--widget-get key)))
        (when w
          (widget-value-set w (or date ""))
          (widget-setup)))
    (quit nil)))

(defun org-tasktree-ui-widget-edit-set-scheduled ()
  "Prompt date and set scheduled field."
  (interactive)
  (org-tasktree-ui--set-widget-date :scheduled "scheduled"))

(defun org-tasktree-ui-widget-edit-set-deadline ()
  "Prompt date and set deadline field."
  (interactive)
  (org-tasktree-ui--set-widget-date :deadline "deadline"))

(defun org-tasktree-ui--open-widget-edit-buffer (meta)
  "Create and show widget edit buffer for META."
  (let* ((type (plist-get meta :type))
         (buf (generate-new-buffer (format "*org-tasktree-edit %s*" type)))
         win)
    (with-current-buffer buf
      (org-tasktree-ui-widget-edit-mode)
      (setq org-tasktree-ui--edit-metadata meta)
      (setq org-tasktree-ui--widget-widgets nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-tasktree-ui--render-widget-form meta)
        (widget-setup)))
    (setq win (pop-to-buffer buf))
    (when (window-live-p win)
      (with-selected-window win
        (set-window-start win (point-min))
        (goto-char (point-min))
        (widget-forward 1)
        (set-window-start win (point-min))))
    win))

(provide 'org-tasktree-ui)
;;; org-tasktree-ui.el ends here
