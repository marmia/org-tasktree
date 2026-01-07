;;; org-tasktree-sync.el --- Sync org buffers into org-tasktree -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; Sync org-mode buffers into the SQLite-backed task tree.
;; This module implements `org-tasktree-sync-buffer'.
;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'org-tasktree-db)
(require 'org-tasktree-model)

(defconst org-tasktree-sync--delete-keyword
  "DEL"
  "Org TODO keyword that marks a node for deletion.")

(defun org-tasktree-sync--nonempty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun org-tasktree-sync--extract-date (raw)
  "Extract YYYY-MM-DD from RAW timestamp string."
  (when (and raw
             (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" raw))
    (match-string 1 raw)))

(defun org-tasktree-sync--extract-repeat (raw)
  "Extract repeat string from RAW timestamp string."
  (when (and raw
             (string-match
              "\\(?:\\+\\+\\|\\+\\|\\.\\+\\)[0-9]+[dwmy]\\(?:/[0-9]+\\)?"
              raw))
    (match-string 0 raw)))

(defun org-tasktree-sync--validate-repeat-raw (raw)
  "Validate repeat syntax in RAW or signal `user-error'."
  (when (and raw
             (string-match "\\(?:\\+\\+\\|\\+\\|\\.\\+\\)" raw)
             (null (org-tasktree-sync--extract-repeat raw)))
    (user-error "Repeat must follow org repeat syntax")))

(defun org-tasktree-sync--timestamp-info (timestamp)
  "Return cons of (DATE . REPEAT) for TIMESTAMP element."
  (when timestamp
    (let* ((raw (org-element-property :raw-value timestamp))
           (date (org-tasktree-sync--extract-date raw))
           (repeat (org-tasktree-sync--extract-repeat raw)))
      (cons date repeat))))

(defun org-tasktree-sync--planning-raw (headline)
  "Return plist with :scheduled-raw and :deadline-raw from HEADLINE."
  (let* ((contents (org-element-contents headline))
         (section (seq-find
                   (lambda (el)
                     (eq (org-element-type el) 'section))
                   contents)))
    (when section
      (let* ((begin (org-element-property :begin section))
             (end (org-element-property :end section))
             (text (and begin end
                        (buffer-substring-no-properties begin end)))
             (scheduled-raw nil)
             (deadline-raw nil)
             (start 0))
        (when (and text (string-match-p "\\bSCHEDULED:" text))
          (setq scheduled-raw ""))
        (when (and text (string-match-p "\\bDEADLINE:" text))
          (setq deadline-raw ""))
        (when text
          (while (string-match
                  "\\bSCHEDULED:\\s-*\\(<[^>]*>\\|\\[[^]]*\\]\\)"
                  text
                  start)
            (setq scheduled-raw (match-string 1 text))
            (setq start (match-end 0)))
          (setq start 0)
          (while (string-match
                  "\\bDEADLINE:\\s-*\\(<[^>]*>\\|\\[[^]]*\\]\\)"
                  text
                  start)
            (setq deadline-raw (match-string 1 text))
            (setq start (match-end 0))))
        (list :scheduled-raw scheduled-raw :deadline-raw deadline-raw)))))

(defun org-tasktree-sync--validate-title (value)
  "Return normalized title string from VALUE or signal `user-error'."
  (let ((title (and value (string-trim value))))
    (unless (and title (not (string-empty-p title)))
      (user-error "Title is required"))
    (when (string-match-p "/" title)
      (user-error "Title must not include '/'"))
    (when (string-match-p "[[:cntrl:]]" title)
      (user-error "Title must not include control characters"))
    title))

(defun org-tasktree-sync--validate-priority-cookie (line)
  "Validate priority cookie in LINE or signal `user-error'."
  (when (and line (string-match "\\[#\\([^]]+\\)\\]" line))
    (let ((raw (match-string 1 line)))
      (unless (string-match-p "\\`[[:alnum:]]\\'" raw)
        (user-error "Priority must be a single alphanumeric character")))))

(defun org-tasktree-sync--validate-tags (tags line)
  "Validate TAGS list and LINE suffix or signal `user-error'."
  (when tags
    (dolist (tag tags)
      (unless (string-match-p "\\`[A-Za-z0-9_@#%]+\\'" tag)
        (user-error
         "Tags must contain only [A-Za-z0-9_@#%%] and be ':' separated"))))
  (when (and (null tags) line
             (string-match-p "\\s-+:[^ \t\r\n]+:$" line))
    (user-error
     "Tags must contain only [A-Za-z0-9_@#%%] and be ':' separated")))

(defun org-tasktree-sync--validate-planning (scheduled-raw scheduled
                                                           deadline-raw deadline)
  "Validate planning fields using raw text.
SCHEDULED-RAW and DEADLINE-RAW are raw timestamp strings (or empty when
present).  SCHEDULED and DEADLINE are parsed timestamp elements."
  (when scheduled-raw
    (unless scheduled
      (user-error "Scheduled must be YYYY-MM-DD or nil"))
    (unless (org-tasktree-sync--extract-date scheduled-raw)
      (user-error "Scheduled must be YYYY-MM-DD or nil"))
    (org-tasktree-sync--validate-repeat-raw scheduled-raw))
  (when deadline-raw
    (unless deadline
      (user-error "Deadline must be YYYY-MM-DD or nil"))
    (unless (org-tasktree-sync--extract-date deadline-raw)
      (user-error "Deadline must be YYYY-MM-DD or nil"))))

(defun org-tasktree-sync--strip-properties (text)
  "Remove PROPERTIES drawers from TEXT."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
        (let ((start (match-beginning 0)))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (delete-region start (line-end-position))))))
    (buffer-string)))

(defun org-tasktree-sync--strip-drawers (text)
  "Remove drawers from TEXT."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (re-search-forward "^[ \t]*:[A-Za-z0-9_]+:[ \t]*$" nil t)
        (let ((start (match-beginning 0)))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (delete-region start (line-end-position))))))
    (buffer-string)))

(defun org-tasktree-sync--strip-planning (text)
  "Remove planning lines (SCHEDULED/DEADLINE) from TEXT."
  (replace-regexp-in-string
   "^[ \t]*\\(?:SCHEDULED:\\|DEADLINE:\\).*\\(?:\n\\|\\'\\)"
   ""
   text))

(defun org-tasktree-sync--trim-blank-edges (text)
  "Trim leading/trailing blank lines from TEXT."
  (let ((s (replace-regexp-in-string "\\`[ \t\n]*" "" text)))
    (replace-regexp-in-string "[ \t\n]*\\'" "" s)))

(defun org-tasktree-sync--extract-content (headline)
  "Return content string for HEADLINE or nil."
  (let* ((contents (org-element-contents headline))
         (section (seq-find
                   (lambda (el)
                     (eq (org-element-type el) 'section))
                   contents)))
    (when section
      (let* ((begin (org-element-property :begin section))
             (end (org-element-property :end section))
             (text (and begin end
                        (buffer-substring-no-properties begin end)))
             (text (org-tasktree-sync--strip-properties text))
             (text (org-tasktree-sync--strip-drawers text))
             (text (org-tasktree-sync--strip-planning text))
             (text (org-tasktree-sync--trim-blank-edges text)))
        (when (string-match-p "\\S-" text)
          text)))))

(defun org-tasktree-sync--collect-headlines ()
  "Return list of headline plists in buffer order."
  (org-with-wide-buffer
    (let ((tree (org-element-parse-buffer)))
      (org-element-map tree 'headline
        (lambda (hl)
          (let* ((begin (org-element-property :begin hl))
                 (marker (and begin (copy-marker begin t)))
                 (level (org-element-property :level hl))
                 (todo (org-element-property :todo-keyword hl))
                 (todo (or todo (org-with-point-at marker (org-get-todo-state))))
                 (title (org-element-property :raw-value hl))
                 (line (and marker
                            (org-with-point-at marker
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))))
                 (planning (org-tasktree-sync--planning-raw hl))
                 (scheduled-raw (plist-get planning :scheduled-raw))
                 (deadline-raw (plist-get planning :deadline-raw))
                 (delete-flag (and (stringp line)
                                   (string-match-p
                                    (concat "^\\*+\\s-+"
                                            org-tasktree-sync--delete-keyword
                                            "\\b")
                                    line)))
                 (title (if (and delete-flag
                                 (string-prefix-p
                                  (concat org-tasktree-sync--delete-keyword " ")
                                  title))
                            (string-remove-prefix
                             (concat org-tasktree-sync--delete-keyword " ")
                             title)
                          title))
                 (title (org-tasktree-sync--validate-title title))
                 (priority (org-element-property :priority hl))
                 (tags (org-element-property :tags hl))
                 (tags-raw (and line
                                (string-match
                                 "\\s-+\\(:[^ \t\r\n]+:\\)\\s-*\\'"
                                 line)
                                (match-string 1 line)))
                 (scheduled (org-element-property :scheduled hl))
                 (deadline (org-element-property :deadline hl))
                 (content (org-tasktree-sync--extract-content hl))
                 (uid (and marker
                           (org-with-point-at marker
                             (org-entry-get nil "UID"))))
                 (parent (org-element-property :parent hl))
                 (parent (when parent
                           (while (and parent
                                       (not (eq (org-element-type parent)
                                                'headline)))
                             (setq parent (org-element-property :parent parent)))
                           parent))
                 (parent-uid (and parent
                                  (org-with-point-at
                           (org-element-property :begin parent)
                            (org-entry-get nil "UID")))))
            (org-tasktree-sync--validate-priority-cookie line)
            (org-tasktree-sync--validate-tags tags line)
            (org-tasktree-sync--validate-planning
             scheduled-raw scheduled deadline-raw deadline)
            (list :begin begin
                  :marker marker
                  :level level
                  :todo todo
                  :delete delete-flag
                  :title title
                  :priority (and priority (char-to-string priority))
                  :tags tags
                  :tags-raw tags-raw
                  :scheduled (org-tasktree-sync--timestamp-info scheduled)
                  :deadline (org-tasktree-sync--timestamp-info deadline)
                  :content content
                  :uid uid
                  :parent-uid parent-uid
                  :parent-present (and parent t))))))))

(defun org-tasktree-sync--status-from-todo (todo)
  "Return status string from TODO keyword."
  (if (string= todo "DONE") "DONE" "OPEN"))

(defun org-tasktree-sync--build-items (raw-items cache)
  "Return plist with :items and :delete-uids from RAW-ITEMS and CACHE."
  (let ((items nil)
        (delete-uids nil)
        (full-stack nil)
        (effective-stack nil))
    (dolist (raw raw-items)
      (let* ((level (plist-get raw :level))
             (todo (plist-get raw :todo))
             (uid (plist-get raw :uid))
             (cached (and uid (gethash uid cache)))
             (delete-self (and (plist-get raw :delete) uid)))
        (when (and (org-tasktree-sync--nonempty-string-p uid)
                   (not cached))
          (user-error "UID not found in DB (uid=%s)" uid))
        (while (and full-stack
                    (>= (plist-get (car full-stack) :level) level))
          (pop full-stack))
        (while (and effective-stack
                    (>= (plist-get (car effective-stack) :level) level))
          (pop effective-stack))
        (let* ((parent-full (car full-stack))
               (delete-ancestor (and parent-full
                                     (plist-get parent-full :delete)))
               (delete-flag (or delete-self delete-ancestor))
               (ignore (and (string= todo "DONE")
                            (not (org-tasktree-sync--nonempty-string-p uid)))))
          (push (list :level level :delete delete-flag) full-stack)
          (when delete-self
            (push uid delete-uids))
          (unless (or delete-flag ignore
                      (and (plist-get raw :delete) (not uid)))
            (let* ((parent-uid (plist-get raw :parent-uid))
                   (parent-present (plist-get raw :parent-present))
                   (parent-cached (and parent-uid (gethash parent-uid cache)))
                   (scope-limited (plist-get raw :scope-limited))
                   (parent-placeholder
                    (when (and (null (car effective-stack)) parent-uid)
                      (unless parent-cached
                        (user-error
                         "Parent UID not found in DB (uid=%s)" parent-uid))
                      (list :uid parent-uid)))
                   (status (org-tasktree-sync--status-from-todo todo))
                   (parent (or (car effective-stack) parent-placeholder)))
              (when (and parent-present (null parent-uid)
                         (null parent)
                         (not scope-limited))
                (user-error "Parent UID missing for selected headline"))
              (let ((item (append raw
                                  (list :status status
                                        :parent parent
                                        :existing (and cached t)))))
                (push item items)
                (push item effective-stack)))))))
    (let ((delete-set (delete-dups delete-uids)))
      (list
       :items (nreverse items)
       :delete-uids (nreverse delete-set)))))

(defun org-tasktree-sync--ensure-uid (item)
  "Ensure ITEM has UID, generating and storing when needed."
  (let ((uid (plist-get item :uid)))
    (if (org-tasktree-sync--nonempty-string-p uid)
        uid
      (let ((new-uid (org-tasktree-db-generate-uid))
            (marker (plist-get item :marker)))
        (if marker
            (org-with-point-at marker
              (org-entry-put nil "UID" new-uid))
          (user-error "Cannot assign UID: marker missing"))
        (plist-put item :uid new-uid)
        new-uid))))

(defun org-tasktree-sync--resolve-parent-id (parent uid->id)
  "Return numeric parent id from PARENT item and UID->ID map."
  (when parent
    (let ((parent-uid (plist-get parent :uid)))
      (or (and parent-uid (gethash parent-uid uid->id))
          (user-error "Parent ID not resolved for uid=%s" parent-uid)))))

(defun org-tasktree-sync--item->node (item uid->id)
  "Convert ITEM to `org-tasktree-model-node' using UID->ID."
  (let* ((uid (org-tasktree-sync--ensure-uid item))
         (existing (plist-get item :existing))
         (parent (plist-get item :parent))
         (parent-id (org-tasktree-sync--resolve-parent-id parent uid->id))
         (parent-id (cond
                     (parent-id parent-id)
                     (existing :keep)
                     (t nil)))
         (todo (plist-get item :todo))
         (todo-keyword todo)
         (priority (plist-get item :priority))
         (scheduled-info (plist-get item :scheduled))
         (deadline-info (plist-get item :deadline))
         (scheduled (car scheduled-info))
         (repeat (cdr scheduled-info))
         (deadline (car deadline-info))
         (tags-raw (plist-get item :tags-raw))
         (tags (plist-get item :tags))
         (tags (cond
                (tags-raw tags-raw)
                (tags (org-tasktree-model-tags->org-string tags))
                (t nil)))
         (content (plist-get item :content))
         (status (plist-get item :status)))
    (org-tasktree-model-node-create
     :uid uid
     :parent-id parent-id
     :todo-keyword todo-keyword
     :title (plist-get item :title)
     :priority priority
     :scheduled scheduled
     :deadline deadline
     :repeat repeat
     :closed-at nil
     :tags tags
     :content content
     :status status)))

(defun org-tasktree-sync--sync-raw-items (raw-items)
  "Sync RAW-ITEMS into the database."
  (org-tasktree-db-init)
  (org-tasktree-db--with-db db
    (org-tasktree-db--with-transaction db
      (let* ((uids (delq nil (mapcar (lambda (it) (plist-get it :uid))
                                     raw-items)))
             (parent-uids (delq nil
                                (mapcar (lambda (it) (plist-get it :parent-uid))
                                        raw-items)))
             (all-uids (delete-dups (append uids parent-uids)))
             (cache (org-tasktree-db-fetch-existing-cache db all-uids))
             (build (org-tasktree-sync--build-items raw-items cache))
             (items (plist-get build :items))
             (delete-uids (plist-get build :delete-uids))
             (now (format-time-string "%FT%T%:z" (current-time)))
             (uid->id (make-hash-table :test 'equal))
             (delete-count (org-tasktree-db-count-subtree-by-uids
                            db delete-uids)))
        (dolist (uid uids)
          (when (and (org-tasktree-sync--nonempty-string-p uid)
                     (not (gethash uid cache)))
            (user-error "UID not found in DB (uid=%s)" uid)))
        (maphash
         (lambda (uid data)
           (let ((id (plist-get data :id)))
             (when (numberp id)
               (puthash uid id uid->id))))
         cache)
        (org-tasktree-db-delete-subtree-by-uids db delete-uids)
        (dolist (item items)
          (let* ((node (org-tasktree-sync--item->node item uid->id))
                 (prepared (org-tasktree-db-commit-nodes-with-db
                            db (list node) cache now))
                 (saved (car prepared))
                 (saved-id (org-tasktree-model-node-id saved))
                 (saved-uid (org-tasktree-model-node-uid saved)))
            (when (numberp saved-id)
              (puthash saved-uid saved-id uid->id))))
        (message "org-tasktree: sync completed (updated %d, deleted %d)"
                 (length items)
                 delete-count)))))

(defun org-tasktree-sync--collect-headlines-in-range (beg end)
  "Return headline plists whose begin is within BEG..END."
  (let* ((items (org-tasktree-sync--collect-headlines))
         (in-range
          (seq-filter
           (lambda (item)
             (let ((pos (plist-get item :begin)))
               (and (integerp pos) (<= beg pos) (< pos end))))
           items)))
    (mapcar (lambda (item)
              (plist-put item :scope-limited t))
            in-range)))

(defun org-tasktree-sync-buffer ()
  "Sync current org buffer into SQLite database."
  (interactive)
  (atomic-change-group
    (org-tasktree-sync--sync-raw-items
     (org-tasktree-sync--collect-headlines))))

(defun org-tasktree-sync-region (beg end)
  "Sync headlines in region BEG..END."
  (interactive "r")
  (unless (use-region-p)
    (user-error "Region is not active"))
  (atomic-change-group
    (org-with-wide-buffer
      (let* ((raw-items (org-tasktree-sync--collect-headlines-in-range beg end)))
        (org-tasktree-sync--sync-raw-items raw-items)))))

(defun org-tasktree-sync-subtree ()
  "Sync subtree at point into SQLite database."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Point is not at a heading"))
  (atomic-change-group
    (org-with-wide-buffer
      (save-excursion
        (org-back-to-heading t)
        (let* ((beg (point))
               (end (save-excursion
                      (org-end-of-subtree t t)
                      (point)))
               (raw-items (org-tasktree-sync--collect-headlines-in-range beg end)))
          (org-tasktree-sync--sync-raw-items raw-items))))))

(provide 'org-tasktree-sync)
;;; org-tasktree-sync.el ends here
