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
(require 'org-tasktree-ui-minibuffer)

(define-obsolete-variable-alias
  'org-tasktree-ui-completion-color-task
  'org-tasktree-ui-minibuffer-completion-color-task
  "0.1.0")
(define-obsolete-variable-alias
  'org-tasktree-ui-completion-color-project
  'org-tasktree-ui-minibuffer-completion-color-project
  "0.1.0")
(define-obsolete-variable-alias
  'org-tasktree-ui-completion-color-phase
  'org-tasktree-ui-minibuffer-completion-color-phase
  "0.1.0")
(define-obsolete-variable-alias
  'org-tasktree-ui-completion-color-group
  'org-tasktree-ui-minibuffer-completion-color-group
  "0.1.0")

(defun org-tasktree-ui--node-type= (node type)
  "Return non-nil when NODE is of TYPE string."
  (equal (org-tasktree-model-node-node-type node) type))

(defun org-tasktree-ui--node-type-symbol (node-type)
  "Return symbol for NODE-TYPE string."
  (pcase node-type
    ("project" 'project)
    ("phase" 'phase)
    ("group" 'group)
    ("task" 'task)
    (_ nil)))

(defun org-tasktree-ui--node-type-options (parent-type)
  "Return allowed node types (strings) for PARENT-TYPE.
PARENT-TYPE is a node_type string or nil for top-level."
  (cond
   ((or (null parent-type) (string-empty-p parent-type)) '("project"))
   ((string= parent-type "project") '("phase" "group" "task"))
   ((string= parent-type "phase") '("group" "task"))
   ((string= parent-type "group") '("group" "task"))
   ((string= parent-type "task") '("task"))
   (t '("task"))))

(defun org-tasktree-ui--node-type-default (parent-type)
  "Return default node_type string for PARENT-TYPE."
  (cond
   ((or (null parent-type) (string-empty-p parent-type)) "project")
   ((string= parent-type "project") "phase")
   ((string= parent-type "phase") "group")
   ((string= parent-type "group") "group")
   ((string= parent-type "task") "task")
   (t "task")))

(defun org-tasktree-ui--node-title-by-id (id)
  "Return node title for numeric ID or nil."
  (let ((node (org-tasktree-query-get-node-by-id id)))
    (and node (org-tasktree-model-node-title node))))

(defun org-tasktree-ui-read-node ()
  "Prompt for a node path in minibuffer."
  (org-tasktree-ui-minibuffer-read-node))

(defvar-local org-tasktree-ui--edit-metadata nil
  "Metadata plist for current edit buffer.")



(defvar org-tasktree-ui--widget-field-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-field-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-widget-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-widget-edit-cancel)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-widget-edit-set-scheduled)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-widget-edit-set-deadline)
    map)
  "Keymap used inside widget editable fields.")

(defvar org-tasktree-ui--widget-text-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-text-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-widget-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-widget-edit-cancel)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-widget-edit-set-scheduled)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-widget-edit-set-deadline)
    map)
  "Keymap used inside widget multiline text fields.")

(defconst org-tasktree-ui--content-width
  60
  "Preferred content field width in characters.")

(defun org-tasktree-ui--set-content-margins (window)
  "Set right margin on WINDOW to fit `org-tasktree-ui--content-width'."
  (when (window-live-p window)
    (let* ((body (window-body-width window))
           (margin (max 0 (- body org-tasktree-ui--content-width))))
      (set-window-margins window nil margin))))





(defun org-tasktree-ui--quit-edit-buffer ()
  "Close current edit buffer and its window."
  (let* ((buf (current-buffer))
         (win (get-buffer-window buf t)))
    (when (and win (eq (window-buffer win) buf))
      (quit-window 'kill win))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

































(defun org-tasktree-ui--field (plist key)
  "Return string value for KEY in PLIST or nil when empty."
  (let ((v (plist-get plist key)))
    (unless (and (stringp v) (string-empty-p v))
      v)))









(defun org-tasktree-ui--node-edit-meta-existing (node path-titles)
  "Return widget META for existing NODE using PATH-TITLES."
  (let* ((type-str (org-tasktree-model-node-node-type node))
         (type (org-tasktree-ui--node-type-symbol type-str))
         (parent-path (and (listp path-titles) (butlast path-titles)))
         (project-id (if (string= type-str "project")
                         (org-tasktree-model-node-id node)
                       (org-tasktree-model-node-project-id node)))
         (phase-id (if (string= type-str "phase")
                       (org-tasktree-model-node-id node)
                     (org-tasktree-model-node-phase-id node)))
         (project-title
          (if (string= type-str "project")
              (org-tasktree-model-node-title node)
            (org-tasktree-ui--node-title-by-id project-id)))
         (phase-title
          (and (numberp phase-id)
               (org-tasktree-ui--node-title-by-id phase-id)))
         (parent-id (org-tasktree-model-node-parent-id node)))
    (list :type type
          :node-type type-str
          :node-type-fixed t
          :uid (org-tasktree-model-node-uid node)
          :title (org-tasktree-model-node-title node)
          :priority (org-tasktree-model-node-priority node)
          :scheduled (org-tasktree-model-node-scheduled node)
          :deadline (org-tasktree-model-node-deadline node)
          :repeat (org-tasktree-model-node-repeat node)
          :tags (org-tasktree-model-node-tags node)
          :content (org-tasktree-model-node-content node)
          :path-titles parent-path
          :project-title project-title
          :project-id project-id
          :phase-title phase-title
          :phase-id phase-id
          :parent-id parent-id
          :task-id (and (eq type 'task)
                        (org-tasktree-model-node-id node))
          :group-id (and (eq type 'group)
                         (org-tasktree-model-node-id node)))))

(defun org-tasktree-ui--node-edit-meta-new (title parent-node parent-path-titles)
  "Return widget META for new node with TITLE, PARENT-NODE, and PARENT-PATH-TITLES."
  (let* ((parent-type (and parent-node
                           (org-tasktree-model-node-node-type parent-node)))
         (options (org-tasktree-ui--node-type-options parent-type))
         (default (org-tasktree-ui--node-type-default parent-type))
         (fixed (and options (= (length options) 1)))
         (parent-id (and parent-node (org-tasktree-model-node-id parent-node)))
         (project-id
          (cond
           ((null parent-node) nil)
           ((string= parent-type "project") parent-id)
           (t (org-tasktree-model-node-project-id parent-node))))
         (phase-id
          (cond
           ((string= parent-type "phase") parent-id)
           ((member parent-type '("group" "task"))
            (org-tasktree-model-node-phase-id parent-node))
           (t nil)))
         (project-title
          (cond
           ((null parent-node) nil)
           ((string= parent-type "project")
            (org-tasktree-model-node-title parent-node))
           (t (org-tasktree-ui--node-title-by-id project-id))))
         (phase-title
          (and (numberp phase-id)
               (org-tasktree-ui--node-title-by-id phase-id)))
         (meta-type (if fixed
                        (org-tasktree-ui--node-type-symbol default)
                      'node)))
    (list :type meta-type
          :node-type default
          :node-type-fixed fixed
          :node-type-options (unless fixed options)
          :uid nil
          :title title
          :priority nil
          :scheduled nil
          :deadline nil
          :repeat nil
          :tags nil
          :content nil
          :path-titles parent-path-titles
          :project-title project-title
          :project-id project-id
          :phase-title phase-title
          :phase-id phase-id
          :parent-id parent-id)))

(defun org-tasktree-ui-edit-node (selection)
  "Open node edit buffer using SELECTION plist."
  (let* ((existing (plist-get selection :existing))
         (node (plist-get selection :node))
         (path-titles (plist-get selection :path-titles))
         (title (plist-get selection :title))
         (parent-node (plist-get selection :parent-node))
         (parent-path-titles (plist-get selection :parent-path-titles))
         (meta
          (if existing
              (org-tasktree-ui--node-edit-meta-existing node path-titles)
            (org-tasktree-ui--node-edit-meta-new title parent-node parent-path-titles))))
    (setq meta (plist-put meta :return-to 'find-node))
    (setq meta (plist-put meta :show-repeat t))
    (org-tasktree-ui--open-widget-edit-buffer meta)))



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
  (setq buffer-read-only nil)
  (overwrite-mode 1)
  (setq truncate-lines nil))

(defun org-tasktree-ui--pos (pos)
  "Return the integer buffer position of POS.

POS may be an integer or a marker."
  (cond
   ((integerp pos) pos)
   ((markerp pos) (marker-position pos))
   (t nil)))

(defun org-tasktree-ui--widget-lock-buffer ()
  "Make non-widget text read-only in the current buffer."
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max)
                         '(read-only t rear-nonsticky (read-only)))
    (let ((plist org-tasktree-ui--widget-widgets))
      (while plist
        (let* ((w (cadr plist))
               (from (org-tasktree-ui--pos
                      (or (and w (widget-field-start w))
                          (and w (widget-get w :from)))))
               (to (org-tasktree-ui--pos
                    (or (and w (widget-field-end w))
                        (and w (widget-get w :to))))))
          (when (and from to (< from to))
            (remove-text-properties from to
                                    '(read-only t rear-nonsticky t))))
        (setq plist (cddr plist))))))

(defun org-tasktree-ui-widget-edit-cancel ()
  "Cancel current org-tasktree widget edit buffer."
  (interactive)
  (let ((meta org-tasktree-ui--edit-metadata))
    (org-tasktree-ui--quit-edit-buffer)
    (message "org-tasktree: edit cancelled")
    (when (eq (plist-get meta :return-to) 'find-node)
      (run-at-time
       0
       nil
       (lambda ()
         (let ((sel (org-tasktree-ui-read-node)))
           (org-tasktree-ui-edit-node sel)))))))

(defun org-tasktree-ui--widget-get (key)
  "Return widget object for KEY."
  (plist-get org-tasktree-ui--widget-widgets key))

(defun org-tasktree-ui--widget-value (key)
  "Return current string value for KEY widget."
  (let ((w (org-tasktree-ui--widget-get key)))
    (when w
      (let ((v (widget-value w)))
        (and (stringp v) (string-trim v))))))

(defun org-tasktree-ui--widget-value-raw (key)
  "Return raw string value for KEY widget without trimming."
  (let ((w (org-tasktree-ui--widget-get key)))
    (when w
      (let ((v (widget-value w)))
        (and (stringp v) v)))))

(defun org-tasktree-ui--normalize-content (value)
  "Return VALUE or nil when empty/whitespace."
  (let ((text (or value "")))
    (if (string-match-p "\\S-" text)
        text
      nil)))

(defun org-tasktree-ui--validate-title (value)
  "Return normalized title string from VALUE or signal `user-error'."
  (let ((title (and value (string-trim value))))
    (unless (and title (not (string-empty-p title)))
      (user-error "Title is required"))
    (when (string-match-p "/" title)
      (user-error "Title must not include '/'"))
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

(defun org-tasktree-ui--validate-repeat (value)
  "Return normalized repeat string from VALUE or nil."
  (let ((r0 (and value (string-trim value))))
    (cond
     ((or (null r0) (string-empty-p r0)) nil)
     ((string-match-p
       "\\`\\(?:\\+\\|\\+\\+\\|\\.\\+\\)[0-9]+[dwmy]\\(?:/[0-9]+\\)?\\'"
       r0)
      r0)
     (t (user-error "Repeat must follow org repeat syntax")))))

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

(defun org-tasktree-ui--resolve-node-type (meta)
  "Return node_type symbol for META and validate selection."
  (let* ((options (plist-get meta :node-type-options))
         (fixed (plist-get meta :node-type-fixed))
         (raw (or (org-tasktree-ui--widget-value :node-type)
                  (plist-get meta :node-type)))
         (value (and raw (string-trim raw))))
    (when (and fixed (org-tasktree-ui--widget-value :node-type))
      (user-error "Node type is fixed"))
    (unless (and value (not (string-empty-p value)))
      (user-error "Node type is required"))
    (when (and options (not (member value options)))
      (user-error "Node type must be one of %s" (string-join options ", ")))
    (let ((type (org-tasktree-ui--node-type-symbol value)))
      (unless type
        (user-error "Unknown node type: %s" value))
      type)))

(defun org-tasktree-ui--submit-widget (meta)
  "Submit widget edit META to DB and return saved node."
  (let* ((meta-type (plist-get meta :type))
         (type (if (or (eq meta-type 'node)
                       (plist-get meta :node-type-options))
                   (org-tasktree-ui--resolve-node-type meta)
                 meta-type))
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
         (repeat (org-tasktree-ui--validate-repeat
                  (org-tasktree-ui--widget-value :repeat)))
         (content-raw (org-tasktree-ui--widget-value-raw :content))
         (content (org-tasktree-ui--normalize-content
                   (and (stringp content-raw) content-raw)))
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
                    :repeat repeat
                    :content content
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
                      :repeat repeat
                      :content content
                      :tags tags
                      :status "OPEN"
                      :project-id project-id
                      :phase-id nil)))
           (org-tasktree-db-commit-nodes (list node))
           node)))
      ('group
       (let* ((project-title (plist-get meta :project-title))
              (project-id (or (plist-get meta :project-id)
                              (and project-title
                                   (org-tasktree-ui--db-project-id
                                    project-title))))
              (phase-title (plist-get meta :phase-title))
              (phase-id (or (plist-get meta :phase-id)
                            (and (numberp project-id)
                                 phase-title
                                 (org-tasktree-ui--db-phase-id
                                  project-id
                                  phase-title))))
              (parent-id (or (plist-get meta :parent-id) phase-id)))
         (unless project-id
           (user-error "Project must exist before creating a group"))
         (unless (numberp parent-id)
           (user-error "Group parent must exist"))
         (let* ((parent-node (org-tasktree-query-get-node-by-id parent-id))
                (parent-type (and parent-node
                                  (org-tasktree-model-node-node-type parent-node)))
                (parent-project-id (and parent-node
                                        (org-tasktree-model-node-project-id parent-node)))
                (parent-phase-id (and parent-node
                                      (org-tasktree-model-node-phase-id parent-node)))
                (parent-level (and parent-node
                                   (org-tasktree-model-node-level parent-node))))
           (unless parent-node
             (user-error "Group parent must exist"))
           (unless (member parent-type '("project" "phase" "group"))
             (user-error "Group parent must be project, phase, or group"))
           (when (string= parent-type "project")
             (setq phase-id nil))
           (when (string= parent-type "phase")
             (setq phase-id parent-id))
           (when (string= parent-type "group")
             (setq phase-id parent-phase-id)
             (when (and (numberp parent-project-id)
                        (not (equal parent-project-id project-id)))
               (user-error "Group does not belong to project")))
           (when (and (string= parent-type "phase")
                      (numberp project-id)
                      (not (equal parent-project-id project-id)))
             (user-error "Phase does not belong to project"))
           (let* ((level (cond
                          ((string= parent-type "project") 2)
                          ((string= parent-type "phase") 3)
                          ((and (string= parent-type "group")
                                (numberp parent-level))
                           (1+ parent-level))
                          (t 3)))
                  (node (org-tasktree-model-node-create
                         :uid uid
                         :parent-id parent-id
                         :node-type "group"
                         :todo-keyword nil
                         :title title
                         :level level
                         :priority priority
                         :scheduled scheduled
                         :deadline deadline
                         :repeat repeat
                         :content content
                         :tags tags
                         :status "OPEN"
                         :project-id project-id
                         :phase-id phase-id)))
             (org-tasktree-db-commit-nodes (list node))
             node))))
      ('task
       (let* ((project-title (plist-get meta :project-title))
              (project-id (or (plist-get meta :project-id)
                              (and project-title
                                   (org-tasktree-ui--db-project-id
                                    project-title))))
              (phase-id (plist-get meta :phase-id))
              (parent-id (plist-get meta :parent-id)))
         (unless project-id
           (user-error "Project must exist before creating a task"))
         (unless (numberp parent-id)
           (user-error "Task parent must exist"))
         (let* ((parent-node (org-tasktree-query-get-node-by-id parent-id))
                (parent-type (and parent-node
                                  (org-tasktree-model-node-node-type parent-node)))
                (parent-project-id (and parent-node
                                        (org-tasktree-model-node-project-id parent-node)))
                (parent-phase-id (and parent-node
                                      (org-tasktree-model-node-phase-id parent-node)))
                (parent-level (and parent-node
                                   (org-tasktree-model-node-level parent-node))))
           (unless parent-node
             (user-error "Task parent must exist"))
           (if (numberp phase-id)
               (let ((phase-node (org-tasktree-query-get-node-by-id phase-id)))
                 (unless (and phase-node
                              (org-tasktree-ui--node-type= phase-node "phase")
                              (equal (org-tasktree-model-node-project-id phase-node)
                                     project-id))
                   (user-error "Phase not found: %s" phase-id)))
             (when (not (equal parent-id project-id))
               (unless (or (and (string= parent-type "task")
                                (null parent-phase-id)
                                (equal parent-project-id project-id))
                           (and (string= parent-type "group")
                                (null parent-phase-id)
                                (equal parent-project-id project-id)))
                 (user-error "Task parent must be project, group, or task under project"))))
           (when (numberp phase-id)
             (unless (or (equal parent-id phase-id)
                         (and (string= parent-type "group")
                              (equal parent-project-id project-id)
                              (equal parent-phase-id phase-id))
                         (and (string= parent-type "task")
                              (equal parent-project-id project-id)
                              (equal parent-phase-id phase-id)))
               (user-error "Task parent must be phase, group, or task under phase")))
           (let* ((level
                   (if (and (string= parent-type "task")
                            (numberp parent-level))
                       (1+ parent-level)
                     (cond
                      ((not (numberp phase-id))
                       (if (string= parent-type "group") 3 2))
                      ((equal parent-id phase-id) 3)
                      (t 4))))
                  (node
                   (org-tasktree-model-node-create
                    :uid uid
                    :parent-id parent-id
                    :node-type "task"
                    :todo-keyword "TODO"
                    :title title
                    :level level
                    :priority priority
                    :scheduled scheduled
                    :deadline deadline
                    :repeat repeat
                    :content content
                    :tags tags
                    :status "OPEN"
                    :project-id project-id
                    :phase-id (and (numberp phase-id) phase-id))))
             (org-tasktree-db-commit-nodes (list node))
             node))))
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

(defun org-tasktree-ui--widget-insert-multiline-field (label key value hint)
  "Insert multiline field for LABEL and return widget.
KEY, VALUE, and HINT configure the created widget."
  (widget-insert (format "%s:\n" label))
  (let* ((start (point))
         (w (widget-create 'text
                           :format "%v"
                           :keymap org-tasktree-ui--widget-text-keymap
                           :value (or value ""))))
    (when hint
      (widget-insert "\n")
      (widget-insert (propertize hint 'face 'shadow)))
    (widget-insert "\n")
    (let ((end (point)))
      (when (< start end)
        (put-text-property start end 'line-prefix nil)
        (put-text-property start end 'wrap-prefix nil)))
    (setq org-tasktree-ui--widget-widgets
          (plist-put org-tasktree-ui--widget-widgets key w))
    w))

(defun org-tasktree-ui--render-node-type-field (meta)
  "Insert node_type field for META when configured."
  (let* ((node-type (plist-get meta :node-type))
         (options (plist-get meta :node-type-options))
         (fixed (plist-get meta :node-type-fixed)))
    (cond
     (fixed
      (when (and (stringp node-type) (not (string-empty-p node-type)))
        (widget-insert (format "%-10s %s (fixed)\n" "node_type:" node-type))))
     (options
      (org-tasktree-ui--widget-insert-field
       "node_type" :node-type node-type
       (format "required: %s" (string-join options "|"))))
     (node-type
      (org-tasktree-ui--widget-insert-field
       "node_type" :node-type node-type "required"))
     (t nil))))

(defun org-tasktree-ui--render-widget-form (meta)
  "Render widget edit UI for META into current buffer."
  (let* ((path (plist-get meta :path-titles))
         (path-str (if (and (listp path) path)
                       (string-join path " > ")
                     "(root)")))
    (widget-insert (propertize (format "Path: %s\n\n" path-str)
                               'face 'bold))
    (org-tasktree-ui--render-node-type-field meta)
    (org-tasktree-ui--widget-insert-field "title" :title
                                          (plist-get meta :title)
                                          "required")
    (org-tasktree-ui--widget-insert-field
     "priority" :priority (plist-get meta :priority)
     "[A-Za-z0-9]")
    (org-tasktree-ui--widget-insert-field
     "scheduled" :scheduled (plist-get meta :scheduled)
     "YYYY-MM-DD | MM-DD | DD (C-c C-s)")
    (org-tasktree-ui--widget-insert-field
     "deadline" :deadline (plist-get meta :deadline)
     "YYYY-MM-DD | MM-DD | DD (C-c C-d)")
    (when (plist-get meta :show-repeat)
      (org-tasktree-ui--widget-insert-field
       "repeat" :repeat (plist-get meta :repeat)
       "+1d | ++2w | .+3m | +1y/2"))
    (org-tasktree-ui--widget-insert-field
     "tags" :tags (plist-get meta :tags)
     ":tag1:tag2: | tag1:tag2")
    (org-tasktree-ui--widget-insert-multiline-field
     "content" :content (plist-get meta :content)
     "multiline")
    (widget-insert "\n")
    (widget-insert
     (propertize "C-c C-c: commit,  C-c C-k: cancel,  TAB/S-TAB: move fields\n"
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

(defvar-local org-tasktree-ui--widget-enforce-size-in-progress nil
  "Non-nil while widget field size enforcement is running.")

(defun org-tasktree-ui--widget-enforce-field-size (_beg _end _len)
  "Keep widget editable fields at their fixed display width.

This function is intended for use in `after-change-functions'."
  (unless org-tasktree-ui--widget-enforce-size-in-progress
    (let ((org-tasktree-ui--widget-enforce-size-in-progress t)
          (inhibit-read-only t))
      (save-excursion
        (let ((widgets nil)
              (plist org-tasktree-ui--widget-widgets))
          (while plist
            (push (cadr plist) widgets)
            (setq plist (cddr plist)))
          (dolist (w (nreverse widgets))
            (let* ((from (org-tasktree-ui--pos
                          (or (ignore-errors (widget-field-start w))
                              (widget-get w :from))))
                   (to (org-tasktree-ui--pos
                        (or (ignore-errors (widget-field-end w))
                            (widget-get w :to))))
                   (size (and w (widget-get w :size))))
              (when (and (integerp from)
                         (integerp to)
                         (integerp size)
                         (< from to)
                         (< 0 size))
                (let ((cur (- to from)))
                  (cond
                   ((> cur size)
                    (delete-region (+ from size) to))
                   ((< cur size)
                    (goto-char to)
                    (insert-and-inherit
                     (make-string (- size cur) ? )))))))))))))

(defun org-tasktree-ui--open-widget-edit-buffer (meta)
  "Create and show widget edit buffer for META."
  (let* ((type (plist-get meta :type))
         (buf (generate-new-buffer (format "*org-tasktree-edit %s*" type)))
         win)
    (with-current-buffer buf
      (org-tasktree-ui-widget-edit-mode)
      (setq org-tasktree-ui--edit-metadata meta)
      (setq org-tasktree-ui--widget-widgets nil)
      (setq org-tasktree-ui--widget-enforce-size-in-progress nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-tasktree-ui--render-widget-form meta)
        (widget-setup)
        (org-tasktree-ui--widget-lock-buffer))
      (add-hook 'after-change-functions
                #'org-tasktree-ui--widget-enforce-field-size
                nil
                t)
      (let* ((w (org-tasktree-ui--widget-get :title))
             (from (and w
                        (or (ignore-errors (widget-field-start w))
                            (widget-get w :from))))
             (pos (or (org-tasktree-ui--pos from)
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "^title:[[:space:]]+" nil t)
                          (point)))
                      (point-min))))
        (goto-char pos)))
    (setq win (pop-to-buffer buf))
    (when (window-live-p win)
      (with-selected-window win
        (org-tasktree-ui--set-content-margins win)
        (set-window-start win (point-min))
        (set-window-point win (with-current-buffer buf (point)))))
    win))

(provide 'org-tasktree-ui)
;;; org-tasktree-ui.el ends here
