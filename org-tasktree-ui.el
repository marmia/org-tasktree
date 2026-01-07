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
  "Return non-nil when NODE has tag TYPE."
  (let* ((tags (org-tasktree-model-node-tags node))
         (tag-list (cond
                    ((null tags) nil)
                    ((stringp tags) (cdr (org-tasktree-model-normalize-tags tags)))
                    ((listp tags) tags)
                    (t nil)))
         (target (downcase (or type "")))
         (found nil))
    (dolist (tag tag-list found)
      (let ((raw (cond
                  ((stringp tag) tag)
                  ((symbolp tag) (symbol-name tag))
                  (t nil))))
        (when (and raw (string= (downcase raw) target))
          (setq found t))))))

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

(defun org-tasktree-ui--node-by-uid (uid)
  "Return node by UID or nil."
  (when uid
    (org-tasktree-db--with-db db
      (let ((row (car (sqlite-select
                       db
                       (string-join
                        '("SELECT id, uid, parent_id, todo_keyword, title,"
                          " priority, scheduled, deadline, repeat, closed_at,"
                          " tags, content, status, created_at, updated_at"
                          " FROM nodes WHERE uid = ? LIMIT 1;")
                        "")
                       (vector uid)))))
        (when row
          (org-tasktree-model-node-from-db-row row))))))

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

(defvar org-tasktree-ui--widget-node-type-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-tasktree-ui--widget-field-keymap)
    (define-key map (kbd "/") #'org-tasktree-ui--node-type-complete)
    map)
  "Keymap used inside the node_type widget field.")

(defvar org-tasktree-ui--widget-tags-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-tasktree-ui--widget-field-keymap)
    (define-key map (kbd "/") #'org-tasktree-ui--tags-complete)
    map)
  "Keymap used inside the tags widget field.")

(defun org-tasktree-ui--completion-table-with-metadata (table metadata)
  "Return completion TABLE that supplies METADATA."
  (lambda (string pred action)
    (if (eq action 'metadata)
        (cons 'metadata metadata)
      (complete-with-action action table string pred))))

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
  (let ((parent-path (and (listp path-titles) (butlast path-titles))))
    (list :type 'node
          :uid (org-tasktree-model-node-uid node)
          :title (org-tasktree-model-node-title node)
          :priority (org-tasktree-model-node-priority node)
          :scheduled (org-tasktree-model-node-scheduled node)
          :deadline (org-tasktree-model-node-deadline node)
          :repeat (org-tasktree-model-node-repeat node)
          :tags (org-tasktree-model-node-tags node)
          :content (org-tasktree-model-node-content node)
          :path-titles parent-path
          :parent-id (org-tasktree-model-node-parent-id node))))

(defun org-tasktree-ui--node-edit-meta-new (title parent-node parent-path-titles)
  "Return widget META for new node with TITLE, PARENT-NODE, and PARENT-PATH-TITLES."
  (list :type 'node
        :uid nil
        :title title
        :priority nil
        :scheduled nil
        :deadline nil
        :repeat nil
        :tags nil
        :content nil
        :path-titles parent-path-titles
        :parent-id (and parent-node (org-tasktree-model-node-id parent-node))))

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
  (let* ((uid (or (plist-get meta :uid) (org-tasktree-db-generate-uid)))
         (existing (and (plist-get meta :uid)
                        (org-tasktree-ui--node-by-uid uid)))
         (parent-id (or (plist-get meta :parent-id)
                        (and existing
                             (org-tasktree-model-node-parent-id existing))))
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
                (org-tasktree-ui--widget-value :tags)))
         (todo-keyword (and existing
                            (org-tasktree-model-node-todo-keyword existing)))
         (status (or (and existing (org-tasktree-model-node-status existing))
                     "OPEN")))
    (org-tasktree-ui--validate-schedule-deadline scheduled deadline)
    (when (and parent-id (not (org-tasktree-query-get-node-by-id parent-id)))
      (user-error "Parent must exist: %s" parent-id))
    (let ((node (org-tasktree-model-node-create
                 :uid uid
                 :parent-id parent-id
                 :todo-keyword todo-keyword
                 :title title
                 :priority priority
                 :scheduled scheduled
                 :deadline deadline
                 :repeat repeat
                 :content content
                 :tags tags
                 :status status)))
      (org-tasktree-db-commit-nodes (list node))
      node)))

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

(defun org-tasktree-ui--widget-insert-field-with-keymap
    (label key value hint keymap)
  "Insert one editable field with KEYMAP and return widget.
LABEL, KEY, VALUE, and HINT configure the created widget."
  (widget-insert (format "%-10s " (concat label ":")))
  (let ((w (widget-create 'editable-field
                          :size 40
                          :format "%v"
                          :keymap keymap
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
      (org-tasktree-ui--widget-insert-field-with-keymap
       "node_type" :node-type node-type
       (format "required: %s" (string-join options "|"))
       org-tasktree-ui--widget-node-type-keymap))
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
    (org-tasktree-ui--widget-insert-field-with-keymap
     "tags" :tags (plist-get meta :tags)
     ":tag1:tag2: | tag1:tag2"
     org-tasktree-ui--widget-tags-keymap)
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

(defun org-tasktree-ui--node-type-field-range ()
  "Return cons of node_type field bounds or nil."
  (let ((w (org-tasktree-ui--widget-get :node-type)))
    (when w
      (let ((from (org-tasktree-ui--pos
                   (or (ignore-errors (widget-field-start w))
                       (widget-get w :from))))
            (to (org-tasktree-ui--pos
                 (or (ignore-errors (widget-field-end w))
                     (widget-get w :to)))))
        (when (and (integerp from) (integerp to) (< from to))
          (cons from to))))))

(defun org-tasktree-ui--node-type-capf ()
  "Return completion data for node_type field, or nil."
  (let* ((options (plist-get org-tasktree-ui--edit-metadata
                             :node-type-options))
         (range (and (listp options)
                     (org-tasktree-ui--node-type-field-range))))
    (when (and range
               (>= (point) (car range))
               (<= (point) (cdr range)))
      (let* ((start (car range))
             (end (cdr range))
             (raw (buffer-substring-no-properties start end))
             (trimmed (replace-regexp-in-string "[ \t\n\r]+\\'" "" raw))
             (trimmed-end (+ start (length trimmed)))
             (collection
              (org-tasktree-ui--completion-table-with-metadata
               options
               '((display-sort-function . identity)
                 (cycle-sort-function . identity)))))
        (list start trimmed-end collection :exclusive 'no)))))

(defun org-tasktree-ui--node-type-complete ()
  "Trigger completion for node_type field."
  (interactive)
  (let ((range (org-tasktree-ui--node-type-field-range)))
    (when range
      (let* ((start (car range))
             (end (cdr range))
             (raw (buffer-substring-no-properties start end))
             (trimmed (replace-regexp-in-string "[ \t\n\r]+\\'" "" raw)))
        (goto-char (+ start (length trimmed))))))
  (let ((completion-at-point-functions
         (list #'org-tasktree-ui--node-type-capf))
        (completion-styles '(basic)))
    (completion-at-point)))

(defun org-tasktree-ui--tags-field-range ()
  "Return cons of tags field bounds or nil."
  (let ((w (org-tasktree-ui--widget-get :tags)))
    (when w
      (let ((from (org-tasktree-ui--pos
                   (or (ignore-errors (widget-field-start w))
                       (widget-get w :from))))
            (to (org-tasktree-ui--pos
                 (or (ignore-errors (widget-field-end w))
                     (widget-get w :to)))))
        (when (and (integerp from) (integerp to) (< from to))
          (cons from to))))))

(defun org-tasktree-ui--tags-normalize-field ()
  "Normalize current tags widget value in-place."
  (let ((w (org-tasktree-ui--widget-get :tags)))
    (when w
      (let* ((raw (widget-value w))
             (normalized (car (org-tasktree-model-normalize-tags raw))))
        (widget-value-set w (or normalized ""))
        (widget-setup)))))

(defun org-tasktree-ui--tags-candidates ()
  "Return normalized, sorted tag candidates from the database."
  (let (tags)
    (org-tasktree-db--with-db db
      (let ((rows (sqlite-select
                   db
                   "SELECT DISTINCT tag FROM node_tags ORDER BY tag ASC;"
                   nil)))
        (dolist (row rows)
          (let* ((raw (if (vectorp row) (aref row 0) (car row)))
                 (tag (and (stringp raw) (string-trim raw))))
            (when (and (stringp tag)
                       (not (string-empty-p tag))
                       (string-match-p "\\`[A-Za-z0-9_-]+\\'" tag))
              (push tag tags))))))
    (setq tags (cdr (org-tasktree-model-normalize-tags tags)))
    (sort (copy-sequence tags) #'string<)))

(defun org-tasktree-ui--tags-completion-range ()
  "Return (START . END) for tag completion within the tags field."
  (let ((range (org-tasktree-ui--tags-field-range)))
    (when range
      (let* ((start (car range))
             (end (cdr range))
             (raw (buffer-substring-no-properties start end))
             (trimmed (replace-regexp-in-string "[ \t\n\r]+\\'" "" raw))
             (trimmed-end (+ start (length trimmed)))
             (suffix-start
              (if (string-match "\\(?:^\\|.*:\\)\\([^:]*\\)\\'" trimmed)
                  (+ start (match-beginning 1))
                start)))
        (cons suffix-start trimmed-end)))))

(defun org-tasktree-ui--tags-capf ()
  "Return completion data for tags field, or nil."
  (let* ((field-range (org-tasktree-ui--tags-field-range))
         (in-field (and field-range
                        (>= (point) (car field-range))
                        (<= (point) (cdr field-range))))
         (cands (and in-field (org-tasktree-ui--tags-candidates)))
         (range (and in-field (org-tasktree-ui--tags-completion-range))))
    (when (and range cands)
      (list (car range)
            (cdr range)
            (org-tasktree-ui--completion-table-with-metadata
             cands
             '((display-sort-function . identity)
               (cycle-sort-function . identity)))
            :exclusive 'no
            :exit-function
            (lambda (_string status)
              (when (eq status 'finished)
                (org-tasktree-ui--tags-normalize-field)))))))

(defun org-tasktree-ui--tags-complete ()
  "Trigger completion for tags field."
  (interactive)
  (let ((range (org-tasktree-ui--tags-completion-range)))
    (when range
      (goto-char (cdr range))))
  (let ((completion-at-point-functions
         (list #'org-tasktree-ui--tags-capf))
        (completion-styles '(basic)))
    (completion-at-point)))

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
      (add-hook 'completion-at-point-functions
                #'org-tasktree-ui--node-type-capf
                nil
                t)
      (add-hook 'completion-at-point-functions
                #'org-tasktree-ui--tags-capf
                nil
                t)
      (add-hook 'after-change-functions
                #'org-tasktree-ui--widget-enforce-field-size
                nil
                t)
      (let* ((new-node (null (plist-get meta :uid)))
             (node-type-widget (org-tasktree-ui--widget-get :node-type))
             (target-widget (if (and new-node node-type-widget)
                                node-type-widget
                              (org-tasktree-ui--widget-get :title)))
             (from (and target-widget
                        (or (ignore-errors (widget-field-start target-widget))
                            (widget-get target-widget :from))))
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
