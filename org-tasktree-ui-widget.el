;;; org-tasktree-ui-widget.el --- Widget helpers for org-tasktree -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Package: org-tasktree

;;; Commentary:
;;
;; Widget-based edit buffer helpers for org-tasktree.
;; Provides field rendering, completion, and widget behaviors.
;;
;;; Code:

(require 'subr-x)
(require 'org)
(require 'calendar)
(require 'widget)
(require 'wid-edit)
(require 'org-tasktree-model)
(require 'org-tasktree-db)

(declare-function org-tasktree-ui-edit-accept "org-tasktree-ui-edit")
(declare-function org-tasktree-ui-edit-cancel "org-tasktree-ui-edit")

(defvar-local org-tasktree-ui-widget--edit-metadata nil
  "Metadata plist for current edit buffer.")

(defun org-tasktree-ui-widget--current-meta ()
  "Return metadata plist for the current edit buffer."
  org-tasktree-ui-widget--edit-metadata)

(defvar-local org-tasktree-ui-widget--widgets nil
  "Plist of widget objects for current edit buffer.")

(defvar org-tasktree-ui-widget--field-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-field-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-edit-cancel)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-widget-edit-set-scheduled)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-widget-edit-set-deadline)
    map)
  "Keymap used inside widget editable fields.")

(defvar org-tasktree-ui-widget--tags-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-tasktree-ui-widget--field-keymap)
    (define-key map (kbd "/") #'org-tasktree-ui-widget--tags-complete)
    map)
  "Keymap used inside the tags widget field.")

(defvar org-tasktree-ui-widget--text-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-text-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-edit-cancel)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-widget-edit-set-scheduled)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-widget-edit-set-deadline)
    map)
  "Keymap used inside widget multiline text fields.")

(defconst org-tasktree-ui-widget--content-width
  60
  "Preferred content field width in characters.")

(defun org-tasktree-ui-widget--set-content-margins (window)
  "Set right margin on WINDOW to fit `org-tasktree-ui-widget--content-width'."
  (when (window-live-p window)
    (let* ((body (window-body-width window))
           (margin (max 0 (- body org-tasktree-ui-widget--content-width))))
      (set-window-margins window nil margin))))

(defvar org-tasktree-ui-widget-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-edit-cancel)
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

(defun org-tasktree-ui-widget--pos (pos)
  "Return the integer buffer position of POS.

POS may be an integer or a marker."
  (cond
   ((integerp pos) pos)
   ((markerp pos) (marker-position pos))
   (t nil)))

(defun org-tasktree-ui-widget--lock-buffer ()
  "Make non-widget text read-only in the current buffer."
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max)
                         '(read-only t rear-nonsticky (read-only)))
    (let ((plist org-tasktree-ui-widget--widgets))
      (while plist
        (let* ((w (cadr plist))
               (from (org-tasktree-ui-widget--pos
                      (or (and w (widget-field-start w))
                          (and w (widget-get w :from)))))
               (to (org-tasktree-ui-widget--pos
                    (or (and w (widget-field-end w))
                        (and w (widget-get w :to))))))
          (when (and from to (< from to))
            (remove-text-properties from to
                                    '(read-only t rear-nonsticky t))))
        (setq plist (cddr plist))))))

(defun org-tasktree-ui-widget--get (key)
  "Return widget object for KEY."
  (plist-get org-tasktree-ui-widget--widgets key))

(defun org-tasktree-ui-widget--value (key)
  "Return current string value for KEY widget."
  (let ((w (org-tasktree-ui-widget--get key)))
    (when w
      (let ((v (widget-value w)))
        (and (stringp v) (string-trim v))))))

(defun org-tasktree-ui-widget--value-raw (key)
  "Return raw string value for KEY widget without trimming."
  (let ((w (org-tasktree-ui-widget--get key)))
    (when w
      (let ((v (widget-value w)))
        (and (stringp v) v)))))

(defun org-tasktree-ui-widget--completion-table-with-metadata (table metadata)
  "Return completion TABLE that supplies METADATA."
  (lambda (string pred action)
    (if (eq action 'metadata)
        (cons 'metadata metadata)
      (complete-with-action action table string pred))))

(defun org-tasktree-ui-widget--insert-field (label key value hint)
  "Insert one editable field for LABEL and return widget.
KEY, VALUE, and HINT configure the created widget."
  (widget-insert (format "%-10s " (concat label ":")))
  (let ((w (widget-create 'editable-field
                          :size 40
                          :format "%v"
                          :keymap org-tasktree-ui-widget--field-keymap
                          :value (or value ""))))
    (when hint
      (widget-insert " " (propertize hint 'face 'shadow)))
    (widget-insert "\n")
    (setq org-tasktree-ui-widget--widgets
          (plist-put org-tasktree-ui-widget--widgets key w))
    w))

(defun org-tasktree-ui-widget--insert-field-with-keymap
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
    (setq org-tasktree-ui-widget--widgets
          (plist-put org-tasktree-ui-widget--widgets key w))
    w))

(defun org-tasktree-ui-widget--insert-multiline-field (label key value hint)
  "Insert multiline field for LABEL and return widget.
KEY, VALUE, and HINT configure the created widget."
  (widget-insert (format "%s:\n" label))
  (let* ((start (point))
         (w (widget-create 'text
                           :format "%v"
                           :keymap org-tasktree-ui-widget--text-keymap
                           :value (or value ""))))
    (when hint
      (widget-insert "\n")
      (widget-insert (propertize hint 'face 'shadow)))
    (widget-insert "\n")
    (let ((end (point)))
      (when (< start end)
        (put-text-property start end 'line-prefix nil)
        (put-text-property start end 'wrap-prefix nil)))
    (setq org-tasktree-ui-widget--widgets
          (plist-put org-tasktree-ui-widget--widgets key w))
    w))

(defun org-tasktree-ui-widget--days-in-month (year month)
  "Return number of days for YEAR and MONTH."
  (calendar-last-day-of-month month year))

(defun org-tasktree-ui-widget--ymd-to-days (year month day)
  "Return day count for YEAR MONTH DAY at local midnight."
  (time-to-days (encode-time 0 0 0 day month year)))

(defun org-tasktree-ui-widget--today-ymd ()
  "Return today's (YEAR MONTH DAY) list in local time."
  (let ((d (decode-time (current-time))))
    (list (nth 5 d) (nth 4 d) (nth 3 d))))

(defun org-tasktree-ui-widget--format-ymd (year month day)
  "Return YYYY-MM-DD string for YEAR, MONTH, and DAY."
  (format "%04d-%02d-%02d" year month day))

(defun org-tasktree-ui-widget--resolve-month-day (month day)
  "Resolve MONTH/DAY to nearest future date (including today)."
  (pcase-let* ((`(,ty ,tm ,td) (org-tasktree-ui-widget--today-ymd))
               (today (org-tasktree-ui-widget--ymd-to-days ty tm td))
               (max-years 30)
               (found (catch 'found
                        (dotimes (i max-years)
                          (let ((year (+ ty i)))
                            (when (<= day (org-tasktree-ui-widget--days-in-month year month))
                              (let ((cand (org-tasktree-ui-widget--ymd-to-days year month day)))
                                (when (>= cand today)
                                  (throw 'found (list year month day)))))))
                        nil)))
    (unless found
      (user-error "Invalid date: %02d-%02d" month day))
    found))

(defun org-tasktree-ui-widget--resolve-day-of-month (day)
  "Resolve DAY to nearest future date (including today)."
  (pcase-let* ((`(,ty ,tm ,td) (org-tasktree-ui-widget--today-ymd))
               (today (org-tasktree-ui-widget--ymd-to-days ty tm td))
               (max-months 36)
               (found (catch 'found
                        (dotimes (i max-months)
                          (let* ((m (+ tm i))
                                 (year (+ ty (/ (1- m) 12)))
                                 (month (1+ (mod (1- m) 12))))
                            (when (<= day (org-tasktree-ui-widget--days-in-month year month))
                              (let ((cand (org-tasktree-ui-widget--ymd-to-days year month day)))
                                (when (>= cand today)
                                  (throw 'found (list year month day)))))))
                        nil)))
    (unless found
      (user-error "Invalid date: %d" day))
    found))

(defun org-tasktree-ui-widget--parse-date-input (value field)
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
                     (<= 1 d (org-tasktree-ui-widget--days-in-month y m)))
          (user-error "Invalid %s: date is not valid" field))
        (org-tasktree-ui-widget--format-ymd y m d)))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\'" s)
      (let* ((m (string-to-number (match-string 1 s)))
             (d (string-to-number (match-string 2 s))))
        (unless (<= 1 m 12)
          (user-error "Invalid %s: month is not valid" field))
        (pcase-let ((`(,y ,mm ,dd) (org-tasktree-ui-widget--resolve-month-day m d)))
          (org-tasktree-ui-widget--format-ymd y mm dd))))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)\\'" s)
      (let ((d (string-to-number (match-string 1 s))))
        (unless (<= 1 d 31)
          (user-error "Invalid %s: day is not valid" field))
        (pcase-let ((`(,y ,m ,dd) (org-tasktree-ui-widget--resolve-day-of-month d)))
          (org-tasktree-ui-widget--format-ymd y m dd))))
     (t (user-error "Invalid %s: date format is not supported" field)))))

(defun org-tasktree-ui-widget--render-form (meta)
  "Render widget edit UI for META into current buffer."
  (let* ((path (plist-get meta :path-titles))
         (path-str (if (and (listp path) path)
                       (string-join path " > ")
                     "(root)")))
    (widget-insert (propertize (format "Path: %s\n\n" path-str)
                               'face 'bold))
    (org-tasktree-ui-widget--insert-field "title" :title
                                          (plist-get meta :title)
                                          "required")
    (org-tasktree-ui-widget--insert-field
     "priority" :priority (plist-get meta :priority)
     "[A-Za-z0-9]")
    (org-tasktree-ui-widget--insert-field
     "scheduled" :scheduled (plist-get meta :scheduled)
     "YYYY-MM-DD | MM-DD | DD (C-c C-s)")
    (org-tasktree-ui-widget--insert-field
     "deadline" :deadline (plist-get meta :deadline)
     "YYYY-MM-DD | MM-DD | DD (C-c C-d)")
    (when (plist-get meta :show-repeat)
      (org-tasktree-ui-widget--insert-field
       "repeat" :repeat (plist-get meta :repeat)
       "+1d | ++2w | .+3m | +1y/2"))
    (org-tasktree-ui-widget--insert-field-with-keymap
     "tags" :tags (plist-get meta :tags)
     ":tag1:tag2: | tag1:tag2"
     org-tasktree-ui-widget--tags-keymap)
    (org-tasktree-ui-widget--insert-multiline-field
     "content" :content (plist-get meta :content)
     "multiline")
    (widget-insert "\n")
    (widget-insert
     (propertize "C-c C-c: commit,  C-c C-k: cancel,  TAB/S-TAB: move fields\n"
                 'face 'shadow))))

(defun org-tasktree-ui-widget--set-date (key field)
  "Read date via `org-read-date' and set widget KEY for FIELD."
  (condition-case nil
      (let* ((input (org-read-date nil nil nil nil nil))
             (date (org-tasktree-ui-widget--parse-date-input input field))
             (w (org-tasktree-ui-widget--get key)))
        (when w
          (widget-value-set w (or date ""))
          (widget-setup)))
    (quit nil)))

(defun org-tasktree-ui-widget--tags-field-range ()
  "Return cons of tags field bounds or nil."
  (let ((w (org-tasktree-ui-widget--get :tags)))
    (when w
      (let ((from (org-tasktree-ui-widget--pos
                   (or (ignore-errors (widget-field-start w))
                       (widget-get w :from))))
            (to (org-tasktree-ui-widget--pos
                 (or (ignore-errors (widget-field-end w))
                     (widget-get w :to)))))
        (when (and (integerp from) (integerp to) (< from to))
          (cons from to))))))

(defun org-tasktree-ui-widget--tags-normalize-field ()
  "Normalize current tags widget value in-place."
  (let ((w (org-tasktree-ui-widget--get :tags)))
    (when w
      (let* ((raw (widget-value w))
             (normalized (car (org-tasktree-model-normalize-tags raw))))
        (widget-value-set w (or normalized ""))
        (widget-setup)))))

(defun org-tasktree-ui-widget--tags-candidates ()
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
                       (string-match-p "\\`[A-Za-z0-9_@#%]+\\'" tag))
              (push tag tags))))))
    (setq tags (cdr (org-tasktree-model-normalize-tags tags)))
    (sort (copy-sequence tags) #'string<)))

(defun org-tasktree-ui-widget--tags-completion-range ()
  "Return (START . END) for tag completion within the tags field."
  (let ((range (org-tasktree-ui-widget--tags-field-range)))
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

(defun org-tasktree-ui-widget--tags-capf ()
  "Return completion data for tags field, or nil."
  (let* ((field-range (org-tasktree-ui-widget--tags-field-range))
         (in-field (and field-range
                        (>= (point) (car field-range))
                        (<= (point) (cdr field-range))))
         (cands (and in-field (org-tasktree-ui-widget--tags-candidates)))
         (range (and in-field (org-tasktree-ui-widget--tags-completion-range))))
    (when (and range cands)
      (list (car range)
            (cdr range)
            (org-tasktree-ui-widget--completion-table-with-metadata
             cands
             '((display-sort-function . identity)
               (cycle-sort-function . identity)))
            :exclusive 'no
            :exit-function
            (lambda (_string status)
              (when (eq status 'finished)
                (org-tasktree-ui-widget--tags-normalize-field)))))))

(defun org-tasktree-ui-widget--tags-complete ()
  "Trigger completion for tags field."
  (interactive)
  (let ((range (org-tasktree-ui-widget--tags-completion-range)))
    (when range
      (goto-char (cdr range))))
  (let ((completion-at-point-functions
         (list #'org-tasktree-ui-widget--tags-capf))
        (completion-styles '(basic)))
    (completion-at-point)))

(defun org-tasktree-ui-widget-edit-set-scheduled ()
  "Prompt date and set scheduled field."
  (interactive)
  (org-tasktree-ui-widget--set-date :scheduled "scheduled"))

(defun org-tasktree-ui-widget-edit-set-deadline ()
  "Prompt date and set deadline field."
  (interactive)
  (org-tasktree-ui-widget--set-date :deadline "deadline"))

(defvar-local org-tasktree-ui-widget--enforce-size-in-progress nil
  "Non-nil while widget field size enforcement is running.")

(defun org-tasktree-ui-widget--enforce-field-size (_beg _end _len)
  "Keep widget editable fields at their fixed display width.

This function is intended for use in `after-change-functions'."
  (unless org-tasktree-ui-widget--enforce-size-in-progress
    (let ((org-tasktree-ui-widget--enforce-size-in-progress t)
          (inhibit-read-only t))
      (save-excursion
        (let ((widgets nil)
              (plist org-tasktree-ui-widget--widgets))
          (while plist
            (push (cadr plist) widgets)
            (setq plist (cddr plist)))
          (dolist (w (nreverse widgets))
            (let* ((from (org-tasktree-ui-widget--pos
                          (or (ignore-errors (widget-field-start w))
                              (widget-get w :from))))
                   (to (org-tasktree-ui-widget--pos
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

(defun org-tasktree-ui-widget--open-buffer (meta)
  "Create and show widget edit buffer for META."
  (let* ((type (plist-get meta :type))
         (buf (generate-new-buffer (format "*org-tasktree-edit %s*" type)))
         win)
    (with-current-buffer buf
      (org-tasktree-ui-widget-edit-mode)
      (setq org-tasktree-ui-widget--edit-metadata meta)
      (setq org-tasktree-ui-widget--widgets nil)
      (setq org-tasktree-ui-widget--enforce-size-in-progress nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-tasktree-ui-widget--render-form meta)
        (widget-setup)
        (org-tasktree-ui-widget--lock-buffer))
      (add-hook 'completion-at-point-functions
                #'org-tasktree-ui-widget--tags-capf
                nil
                t)
      (add-hook 'after-change-functions
                #'org-tasktree-ui-widget--enforce-field-size
                nil
                t)
      (let* ((target-widget (org-tasktree-ui-widget--get :title))
             (from (and target-widget
                        (or (ignore-errors (widget-field-start target-widget))
                            (widget-get target-widget :from))))
             (pos (or (org-tasktree-ui-widget--pos from)
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "^title:[[:space:]]+" nil t)
                          (point)))
                      (point-min))))
        (goto-char pos)))
    (setq win (pop-to-buffer buf))
    (when (window-live-p win)
      (with-selected-window win
        (org-tasktree-ui-widget--set-content-margins win)
        (set-window-start win (point-min))
        (set-window-point win (with-current-buffer buf (point)))))
    win))

(provide 'org-tasktree-ui-widget)
;;; org-tasktree-ui-widget.el ends here
