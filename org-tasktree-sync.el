;;; org-tasktree-sync.el --- Sync org buffers into org-tasktree -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

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

(defun org-tasktree-sync--timestamp-info (timestamp)
  "Return cons of (DATE . REPEAT) for TIMESTAMP element."
  (when timestamp
    (let* ((raw (org-element-property :raw-value timestamp))
           (date (org-tasktree-model--extract-date raw))
           (repeat (org-tasktree-model--extract-repeat raw)))
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

(defun org-tasktree-sync--headline-line (marker)
  "Return line text at MARKER or nil."
  (when marker
    (org-with-point-at marker
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun org-tasktree-sync--headline-delete-flag (line)
  "Return non-nil when LINE is marked for deletion."
  (and (stringp line)
       (string-match-p
        (concat "^\\*+\\s-+" org-tasktree-sync--delete-keyword "\\b")
        line)))

(defun org-tasktree-sync--headline-title (raw-title delete-flag)
  "Return validated title from RAW-TITLE and DELETE-FLAG."
  (let ((title raw-title))
    (when (and delete-flag
               (string-prefix-p
                (concat org-tasktree-sync--delete-keyword " ")
                title))
      (setq title (string-remove-prefix
                   (concat org-tasktree-sync--delete-keyword " ")
                   title)))
    (org-tasktree-model-validate-title title)))

(defun org-tasktree-sync--headline-tags-raw (line)
  "Return raw tags suffix from LINE or nil."
  (when (and line
             (string-match
              "\\s-+\\(:[^ \t\r\n]+:\\)\\s-*\\'"
              line))
    (match-string 1 line)))

(defun org-tasktree-sync--headline-parent (headline)
  "Return parent headline element of HEADLINE or nil."
  (let ((parent (org-element-property :parent headline)))
    (while (and parent (not (eq (org-element-type parent) 'headline)))
      (setq parent (org-element-property :parent parent)))
    parent))

(defun org-tasktree-sync--headline-parent-uid (parent)
  "Return UID string for PARENT headline element or nil."
  (when parent
    (org-with-point-at
        (org-element-property :begin parent)
      (org-entry-get nil "UID"))))

(defun org-tasktree-sync--headline-priority-cookie (line)
  "Return priority cookie from LINE or nil."
  (when (and line (string-match "\\[#\\([^]]+\\)\\]" line))
    (match-string 1 line)))

(defun org-tasktree-sync--headline->raw (headline)
  "Return raw plist data from HEADLINE."
  (let* ((begin (org-element-property :begin headline))
         (marker (and begin (copy-marker begin t)))
         (level (org-element-property :level headline))
         (todo (org-element-property :todo-keyword headline))
         (todo (or todo (org-with-point-at marker (org-get-todo-state))))
         (raw-title (org-element-property :raw-value headline))
         (line (org-tasktree-sync--headline-line marker))
         (planning (org-tasktree-sync--planning-raw headline))
         (scheduled-raw (plist-get planning :scheduled-raw))
         (deadline-raw (plist-get planning :deadline-raw))
         (delete-flag (org-tasktree-sync--headline-delete-flag line))
         (title (org-tasktree-sync--headline-title raw-title delete-flag))
         (priority (org-element-property :priority headline))
         (tags (org-element-property :tags headline))
         (tags-raw (org-tasktree-sync--headline-tags-raw line))
         (scheduled (org-element-property :scheduled headline))
         (deadline (org-element-property :deadline headline))
         (content (org-tasktree-sync--extract-content headline))
         (uid (and marker
                   (org-with-point-at marker
                     (org-entry-get nil "UID"))))
         (parent (org-tasktree-sync--headline-parent headline))
         (parent-uid (org-tasktree-sync--headline-parent-uid parent))
         (priority-cookie (org-tasktree-sync--headline-priority-cookie line)))
    (when priority-cookie
      (org-tasktree-model-validate-priority priority-cookie))
    (org-tasktree-model-validate-tags-list tags line)
    (org-tasktree-model-validate-planning-raw
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
          :parent-present (and parent t))))

(defun org-tasktree-sync--collect-headlines ()
  "Return list of headline plists in buffer order."
  (org-with-wide-buffer
    (let ((tree (org-element-parse-buffer)))
      (org-element-map tree 'headline #'org-tasktree-sync--headline->raw))))

(defun org-tasktree-sync--status-from-todo (todo)
  "Return status string from TODO keyword."
  (if (string= todo "DONE") "DONE" "OPEN"))

(defun org-tasktree-sync--stack-pop-to-level (stack level)
  "Return STACK popped until it is below LEVEL."
  (while (and stack
              (>= (plist-get (car stack) :level) level))
    (pop stack))
  stack)

(defun org-tasktree-sync--stack-push (stack level delete-flag)
  "Return STACK with LEVEL and DELETE-FLAG pushed."
  (push (list :level level :delete delete-flag) stack)
  stack)

(defun org-tasktree-sync--ignore-raw-item-p (todo uid)
  "Return non-nil when TODO is done and UID is missing."
  (and (string= todo "DONE")
       (not (org-tasktree-sync--nonempty-string-p uid))))

(defun org-tasktree-sync--build-item (raw cache effective-stack cached)
  "Return item plist built from RAW.
CACHE is UID->node hash.  EFFECTIVE-STACK contains included ancestors.
CACHED is non-nil when RAW's UID exists in CACHE."
  (let* ((parent-uid (plist-get raw :parent-uid))
         (parent-present (plist-get raw :parent-present))
         (parent-cached (and parent-uid (gethash parent-uid cache)))
         (scope-limited (plist-get raw :scope-limited))
         (parent-placeholder
          (when (and (null (car effective-stack)) parent-uid)
            (unless parent-cached
              (user-error "Parent UID not found in DB (uid=%s)" parent-uid))
            (list :uid parent-uid)))
         (status (org-tasktree-sync--status-from-todo
                  (plist-get raw :todo)))
         (parent (or (car effective-stack) parent-placeholder)))
    (when (and parent-present (null parent-uid)
               (null parent)
               (not scope-limited))
      (user-error "Parent UID missing for selected headline"))
    (append raw
            (list :status status
                  :parent parent
                  :existing (and cached t)))))

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
        (setq full-stack (org-tasktree-sync--stack-pop-to-level
                          full-stack level))
        (setq effective-stack (org-tasktree-sync--stack-pop-to-level
                               effective-stack level))
        (let* ((parent-full (car full-stack))
               (delete-ancestor (and parent-full
                                     (plist-get parent-full :delete)))
               (delete-flag (or delete-self delete-ancestor))
               (ignore (org-tasktree-sync--ignore-raw-item-p todo uid)))
          (setq full-stack (org-tasktree-sync--stack-push
                            full-stack level delete-flag))
          (when delete-self
            (push uid delete-uids))
          (unless (or delete-flag ignore
                      (and (plist-get raw :delete) (not uid)))
            (let ((item (org-tasktree-sync--build-item
                         raw cache effective-stack cached)))
              (push item items)
              (push item effective-stack))))))
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
  (org-with-wide-buffer
    (let* ((tree (org-element-parse-buffer))
           (items
            (org-element-map tree 'headline
              (lambda (headline)
                (let ((pos (org-element-property :begin headline)))
                  (when (and (integerp pos) (<= beg pos) (< pos end))
                    (org-tasktree-sync--headline->raw headline))))))
           (in-range (seq-filter #'identity items)))
      (mapcar (lambda (item)
                (plist-put item :scope-limited t))
              in-range))))

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
  (atomic-change-group
    (org-with-wide-buffer
      (save-excursion
        (let ((heading-pos
               (condition-case nil
                   (progn
                     (org-back-to-heading t)
                     (point))
                 (error nil))))
          (unless heading-pos
            (user-error "No heading above point"))
          (goto-char heading-pos)
          (let* ((beg (point))
                 (end (save-excursion
                        (org-end-of-subtree t t)
                        (point)))
                 (raw-items (org-tasktree-sync--collect-headlines-in-range beg end)))
            (org-tasktree-sync--sync-raw-items raw-items)))))))

(provide 'org-tasktree-sync)
;;; org-tasktree-sync.el ends here
