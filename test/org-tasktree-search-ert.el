;;; org-tasktree-search-ert.el --- ERT tests for org-tasktree search -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; ERT tests for org-tasktree search commands.
;; These tests verify rendered search buffers and invalid date handling.
;;
;;; Code:

(require 'ert)
(require 'org)
(require 'subr-x)
(require 'org-tasktree)
(require 'org-tasktree-db)
(require 'org-tasktree-model)
(require 'org-tasktree-query)
(require 'org-tasktree-view)
(require 'org-tasktree-test-helper)

(declare-function org-tasktree-query-parser--today "org-tasktree-query-parser")
(declare-function org-tasktree-query-parser--days-from-now
                  "org-tasktree-query-parser")

(defconst org-tasktree-search-ert--base-time
  (encode-time 0 0 0 25 12 2025)
  "Fixed time used for org-tasktree search ERT tests.")

(defun org-tasktree-search-ert--repo-root ()
  "Return repo root for test run."
  (if (and (boundp 'repo-root) (stringp repo-root))
      repo-root
    default-directory))

(defun org-tasktree-search-ert--test-data-path (name)
  "Return absolute path for test data file NAME."
  (expand-file-name (concat "test/test-data/" name)
                    (org-tasktree-search-ert--repo-root)))

(defun org-tasktree-search-ert--format-date (time)
  "Return YYYY-MM-DD string from TIME."
  (format-time-string "%Y-%m-%d" time))

(defun org-tasktree-search-ert--format-date-with-weekday (time)
  "Return YYYY-MM-DD EEE string from TIME in English locale."
  (let ((system-time-locale "C"))
    (format-time-string "%Y-%m-%d %a" time)))

(defun org-tasktree-search-ert--time-days-from (base days)
  "Return time DAYS from BASE."
  (time-add base (days-to-time days)))

(defun org-tasktree-search-ert--weekday-from-date (date)
  "Return English weekday abbreviation for DATE (YYYY-MM-DD)."
  (let* ((year (string-to-number (substring date 0 4)))
         (month (string-to-number (substring date 5 7)))
         (day (string-to-number (substring date 8 10))))
    (let ((system-time-locale "C"))
      (format-time-string "%a" (encode-time 0 0 0 day month year)))))

(defun org-tasktree-search-ert--expand-placeholders (text)
  "Return TEXT with date placeholders replaced."
  (let ((case-fold-search t)
        (regex
         "<\\(TODAY\\|YESTERDAY\\|TOMORROW\\)\\([+-][0-9]+\\)?\\(d\\)?\\([^>]*\\)>")
        (pos 0)
        (parts nil))
    (while (string-match regex text pos)
      (let* ((base (match-string 1 text))
             (delta-str (match-string 2 text))
             (delta (if delta-str (string-to-number delta-str) 0))
             (base-offset (pcase (upcase base)
                            ("TODAY" 0)
                            ("YESTERDAY" -1)
                            ("TOMORROW" 1)))
             (date (org-tasktree-search-ert--format-date
                    (org-tasktree-search-ert--time-days-from
                     org-tasktree-search-ert--base-time
                     (+ base-offset delta))))
             (suffix (or (match-string 4 text) "")))
        (push (substring text pos (match-beginning 0)) parts)
        (push (format "<%s%s>" date suffix) parts)
        (setq pos (match-end 0))))
    (push (substring text pos) parts)
    (apply #'concat (nreverse parts))))

(defun org-tasktree-search-ert--normalize-timestamps (text)
  "Return TEXT with timestamps normalized to include weekday."
  (replace-regexp-in-string
   "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\( [A-Za-z]\\{3\\}\\)?>"
   (lambda (match)
     (if (string-match
          "\\`<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\( [A-Za-z]\\{3\\}\\)?>\\'"
          match)
         (let ((date (match-string 1 match))
               (weekday (match-string 2 match)))
           (if weekday
               match
             (format "<%s %s>" date
                     (org-tasktree-search-ert--weekday-from-date date))))
       match))
   text t t))

(defun org-tasktree-search-ert--expected-string (file)
  "Return normalized expected output string for FILE."
  (let ((text (with-temp-buffer
                (insert-file-contents (org-tasktree-search-ert--test-data-path
                                       file))
                (buffer-string))))
    (setq text (org-tasktree-search-ert--expand-placeholders text))
    (setq text (org-tasktree-search-ert--normalize-timestamps text))
    (string-trim-right text)))

(defun org-tasktree-search-ert--buffer-string (title)
  "Return normalized buffer string for search TITLE."
  (let* ((buffer-name (format "%s%s*"
                              org-tasktree-view--buffer-prefix
                              title))
         (buffer (get-buffer buffer-name)))
    (should buffer)
    (with-current-buffer buffer
      (string-trim-right
       (org-tasktree-search-ert--normalize-timestamps
        (buffer-substring-no-properties (point-min) (point-max)))))))

(defun org-tasktree-search-ert--cleanup-buffer (title)
  "Kill search result buffer for TITLE when it exists."
  (let* ((buffer-name (format "%s%s*"
                              org-tasktree-view--buffer-prefix
                              title))
         (buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer))))

(defun org-tasktree-search-ert--fetch-node-by-uid (uid)
  "Return node for UID or nil."
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
        (org-tasktree-model-node-from-db-row row)))))

(defun org-tasktree-search-ert--insert-node (node)
  "Insert NODE and return the saved node."
  (org-tasktree-db-commit-nodes (list node))
  (org-tasktree-search-ert--fetch-node-by-uid
   (org-tasktree-model-node-uid node)))

(defun org-tasktree-search-ert--make-node (spec parent-id)
  "Return node from SPEC and PARENT-ID."
  (org-tasktree-model-node-create
   :uid (plist-get spec :uid)
   :todo-keyword (plist-get spec :todo-keyword)
   :title (plist-get spec :title)
   :priority (plist-get spec :priority)
   :scheduled (plist-get spec :scheduled)
   :deadline (plist-get spec :deadline)
   :repeat (plist-get spec :repeat)
   :closed-at (plist-get spec :closed-at)
   :tags (plist-get spec :tags)
   :content (plist-get spec :content)
   :status (or (plist-get spec :status) "OPEN")
   :parent-id parent-id))

(defun org-tasktree-search-ert--seed-nodes (specs)
  "Insert SPECS and return hash of keyed nodes."
  (let ((nodes (make-hash-table :test 'eq)))
    (dolist (spec specs)
      (let* ((key (plist-get spec :key))
             (parent-key (plist-get spec :parent))
             (parent-node (and parent-key (gethash parent-key nodes)))
             (parent-id (and parent-node (org-tasktree-model-node-id parent-node)))
             (node (org-tasktree-search-ert--make-node spec parent-id))
             (saved (org-tasktree-search-ert--insert-node node)))
        (puthash key saved nodes)))
    nodes))

(defun org-tasktree-search-ert--seed-normal-data ()
  "Seed DB with search test data."
  (org-tasktree-test-helper-reset-db)
  (let* ((today (org-tasktree-query-parser--today))
         (yesterday (org-tasktree-query-parser--days-from-now -1))
         (tomorrow (org-tasktree-query-parser--days-from-now 1))
         (specs
          (list
           (list :key :project
                 :uid "5d4937b3-1fe0-50cb-a885-b85873e6bcaf"
                 :title "proj1"
                 :priority "A"
                 :content "This is a project node.")
           (list :key :phase
                 :uid "1e7c4080-66a8-5243-bc6f-31116a2524ca"
                 :title "phase1"
                 :content "This is a phase."
                 :parent :project)
           (list :key :group
                 :uid "6c966182-ae9c-5470-b549-e10cf191c651"
                 :title "group1"
                 :content "This is a group."
                 :parent :phase)
           (list :key :task-done
                 :uid "84d9b1fa-88a4-5b0e-861a-7476087ed2f6"
                 :todo-keyword "DONE"
                 :title "task-done"
                 :content "This is a done task."
                 :status "DONE"
                 :parent :group)
           (list :key :task-today
                 :uid "e3085041-8060-537f-bda7-1a9da956b8a7"
                 :todo-keyword "TODO"
                 :title "task-today"
                 :priority "A"
                 :scheduled today
                 :deadline "2026-01-20"
                 :content "This is a today task."
                 :parent :project)
           (list :key :task-yesterday
                 :uid "2172d110-82a5-569a-889d-2141e9600991"
                 :todo-keyword "TODO"
                 :title "task-yesterday"
                 :priority "A"
                 :scheduled yesterday
                 :deadline "2026-01-20"
                 :content "This is a yesterday task."
                 :parent :project)
           (list :key :task-overdue
                 :uid "61d1d63d-864d-5fc3-91a2-bdcabdb78cf1"
                 :todo-keyword "TODO"
                 :title "task-overdue"
                 :priority "A"
                 :scheduled yesterday
                 :deadline yesterday
                 :content "This is a overdue task."
                 :parent :project)
           (list :key :task-tomorrow
                 :uid "bff290e4-2ed6-520f-99ce-56ec6325d203"
                 :todo-keyword "TODO"
                 :title "task-tomorrow"
                 :priority "A"
                 :scheduled tomorrow
                 :deadline "2026-01-20"
                 :content "This is a tomorrow task."
                 :parent :project)
           (list :key :task-unscheduled
                 :uid "9e5e1b66-a262-58dc-9b72-7ee052d5ca27"
                 :todo-keyword "TODO"
                 :title "task-unscheduled"
                 :priority "A"
                 :scheduled nil
                 :deadline "2026-01-20"
                 :content "This is a unscheduled task."
                 :parent :project))))
    (org-tasktree-search-ert--seed-nodes specs)))

(defun org-tasktree-search-ert--seed-invalid-date-data ()
  "Seed DB with invalid date test data."
  (org-tasktree-test-helper-reset-db)
  (org-tasktree-db--with-db db
    ;; NOTE: Use raw SQL to bypass model validation for invalid dates.
    (sqlite-execute
     db
     (string-join
      '("INSERT INTO nodes("
        "  id, uid, parent_id, todo_keyword, title, priority, scheduled,"
        "  deadline, repeat, closed_at, tags, content, status, created_at,"
        "  updated_at"
        ") VALUES("
        "  ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
      "\n")
     (vector 2
             "5d4937b3-1fe0-50cb-a885-b85873e6bcaf"
             nil
             nil
             "proj1"
             "A"
             nil
             nil
             nil
             nil
             nil
             "This is a project node."
             "OPEN"
             "2025-12-25T00:00:00+09:00"
             "2025-12-25T00:00:00+09:00"))
    ;; NOTE: Invalid scheduled/deadline values for error paths.
    (sqlite-execute
     db
     (string-join
      '("INSERT INTO nodes("
        "  id, uid, parent_id, todo_keyword, title, priority, scheduled,"
        "  deadline, repeat, closed_at, tags, content, status, created_at,"
        "  updated_at"
        ") VALUES("
        "  ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
      "\n")
     (vector 3
             "e3085041-8060-537f-bda7-1a9da956b8a7"
             2
             "TODO"
             "task-today"
             "A"
             "2025-12-200"
             "2026-01-200"
             nil
             nil
             nil
             "This is a today task."
             "OPEN"
             "2025-12-25T00:00:00+09:00"
             "2025-12-25T00:00:00+09:00"))))

(defun org-tasktree-search-ert--assert-search-output (title file)
  "Assert search buffer TITLE matches expected FILE."
  (let ((actual (org-tasktree-search-ert--buffer-string title))
        (expected (org-tasktree-search-ert--expected-string file)))
    (unwind-protect
        (should (equal expected actual))
      (org-tasktree-search-ert--cleanup-buffer title))))

(provide 'org-tasktree-search-ert)
;;; org-tasktree-search-ert.el ends here
