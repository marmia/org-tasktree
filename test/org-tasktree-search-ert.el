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
(require 'rx)
(require 'subr-x)
(require 'org-tasktree)
(require 'org-tasktree-db)
(require 'org-tasktree-model)
(require 'org-tasktree-query)
(require 'org-tasktree-view)
(require 'org-tasktree-test-helper)
(require 'org-tasktree-ui-minibuffer)

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

(defun org-tasktree-search-ert--replace-sql-now (text)
  "Return TEXT with SQLite now() dates replaced by the base time."
  (let ((regex
         (rx "date('now','localtime'"
             (? ",'" (group (or "+" "-") (+ digit)) " day" (? "s") "'")
             ")"))
        (pos 0)
        (parts nil))
    (while (string-match regex text pos)
      (let* ((delta-str (match-string 1 text))
             (days (if delta-str (string-to-number delta-str) 0))
             (date (org-tasktree-search-ert--format-date
                    (org-tasktree-search-ert--time-days-from
                     org-tasktree-search-ert--base-time days))))
        (push (substring text pos (match-beginning 0)) parts)
        (push (format "'%s'" date) parts)
        (setq pos (match-end 0))))
    (push (substring text pos) parts)
    (apply #'concat (nreverse parts))))

(defun org-tasktree-search-ert--exec-sql-file (file)
  "Execute SQL statements loaded from FILE."
  (let* ((text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (text (org-tasktree-search-ert--replace-sql-now text))
         (stmts (seq-filter
                 (lambda (s) (string-match-p "\\S-" s))
                 (mapcar #'string-trim
                         (split-string text ";" t)))))
    (org-tasktree-db--with-db db
      (dolist (stmt stmts)
        (sqlite-execute db stmt)))))

(defun org-tasktree-search-ert--by-query-sql-path ()
  "Return absolute path for by-query SQL seed file."
  (expand-file-name "test/by-query-testdata.sql"
                    (org-tasktree-search-ert--repo-root)))

(defun org-tasktree-search-ert--by-query-file-path (name)
  "Return absolute path for query file NAME."
  (expand-file-name (concat "test/test-data/query/" name)
                    (org-tasktree-search-ert--repo-root)))

(defun org-tasktree-search-ert--seed-by-query-data ()
  "Seed DB with by-query test data."
  (org-tasktree-test-helper-reset-db)
  (org-tasktree-search-ert--exec-sql-file
   (org-tasktree-search-ert--by-query-sql-path)))

(defun org-tasktree-search-ert--install-query-file (name)
  "Copy query file NAME into `org-tasktree-query-dir'."
  (let* ((src (org-tasktree-search-ert--by-query-file-path name))
         (dest-dir (org-tasktree-test-helper--query-dir))
         (dest (expand-file-name name dest-dir)))
    (make-directory dest-dir t)
    (copy-file src dest t)))

(defun org-tasktree-search-ert--query-title (name)
  "Return result buffer title for query file NAME."
  (file-name-base name))

(defmacro org-tasktree-search-ert-with-query-selection (choice &rest body)
  "Run BODY with query selection stubbed to return CHOICE."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'org-tasktree-ui-minibuffer--completing-read)
              (lambda (_prompt _cands &rest _args)
                ,choice)))
     ,@body))

(defun org-tasktree-search-ert--fetch-node-by-uid (uid)
  "Return node for UID or nil."
  (org-tasktree-db--with-db db
    (let ((row (car (sqlite-select
                     db
                     (string-join
                      '("SELECT id, uid, parent_id, node_type, todo_keyword,"
                        " title, level, priority, scheduled, deadline, repeat,"
                        " closed_at, tags, content, status, project_id,"
                        " phase_id, created_at, updated_at"
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

(defun org-tasktree-search-ert--seed-normal-data ()
  "Seed DB with search test data."
  (org-tasktree-test-helper-reset-db)
  (let* ((project (org-tasktree-model-node-create
                   :uid "00000000-0000-0000-0000-project00001"
                   :node-type "project"
                   :todo-keyword "PROJ"
                   :title "proj1"
                   :level 1
                   :priority "A"
                   :scheduled nil
                   :deadline nil
                   :repeat nil
                   :closed-at nil
                   :tags nil
                   :content "This is a project node."
                   :status "OPEN"
                   :parent-id nil
                   :project-id nil
                   :phase-id nil))
         (project-node (org-tasktree-search-ert--insert-node project))
         (project-id (org-tasktree-model-node-id project-node))
         (today (org-tasktree-query--today))
         (yesterday (org-tasktree-query--days-from-now -1))
         (tomorrow (org-tasktree-query--days-from-now 1))
         (task-today (org-tasktree-model-node-create
                      :uid "00000000-0000-0000-0000-today0000002"
                      :node-type "task"
                      :todo-keyword "TODO"
                      :title "task-today"
                      :level 2
                      :priority "A"
                      :scheduled today
                      :deadline "2026-01-20"
                      :repeat nil
                      :closed-at nil
                      :tags nil
                      :content "This is a today task."
                      :status "OPEN"
                      :parent-id project-id
                      :project-id project-id
                      :phase-id nil))
         (task-yesterday (org-tasktree-model-node-create
                          :uid "00000000-0000-0000-0000-yesterday003"
                          :node-type "task"
                          :todo-keyword "TODO"
                          :title "task-yesterday"
                          :level 2
                          :priority "A"
                          :scheduled yesterday
                          :deadline "2026-01-20"
                          :repeat nil
                          :closed-at nil
                          :tags nil
                          :content "This is a yesterday task."
                          :status "OPEN"
                          :parent-id project-id
                          :project-id project-id
                          :phase-id nil))
         (task-overdue (org-tasktree-model-node-create
                        :uid "00000000-0000-0000-0000-overdue00004"
                        :node-type "task"
                        :todo-keyword "TODO"
                        :title "task-overdue"
                        :level 2
                        :priority "A"
                        :scheduled yesterday
                        :deadline yesterday
                        :repeat nil
                        :closed-at nil
                        :tags nil
                        :content "This is a overdue task."
                        :status "OPEN"
                        :parent-id project-id
                        :project-id project-id
                        :phase-id nil))
         (task-tomorrow (org-tasktree-model-node-create
                         :uid "00000000-0000-0000-0000-tomorrow0005"
                         :node-type "task"
                         :todo-keyword "TODO"
                         :title "task-tomorrow"
                         :level 2
                         :priority "A"
                         :scheduled tomorrow
                         :deadline "2026-01-20"
                         :repeat nil
                         :closed-at nil
                         :tags nil
                         :content "This is a tomorrow task."
                         :status "OPEN"
                         :parent-id project-id
                         :project-id project-id
                         :phase-id nil))
         (task-unscheduled (org-tasktree-model-node-create
                            :uid "00000000-0000-0000-0000-unsche000006"
                            :node-type "task"
                            :todo-keyword "TODO"
                            :title "task-unscheduled"
                            :level 2
                            :priority "A"
                            :scheduled nil
                            :deadline "2026-01-20"
                            :repeat nil
                            :closed-at nil
                            :tags nil
                            :content "This is a unscheduled task."
                            :status "OPEN"
                            :parent-id project-id
                            :project-id project-id
                            :phase-id nil)))
    (org-tasktree-db-commit-nodes
     (list task-today
           task-yesterday
           task-overdue
           task-tomorrow
           task-unscheduled))))

(defun org-tasktree-search-ert--seed-invalid-date-data ()
  "Seed DB with invalid date test data."
  (org-tasktree-test-helper-reset-db)
  (org-tasktree-db--with-db db
    (sqlite-execute
     db
     (string-join
      '("INSERT INTO nodes("
        "  id, uid, parent_id, node_type, todo_keyword, title, level,"
        "  priority, scheduled, deadline, repeat, closed_at, tags,"
        "  content, status, project_id, phase_id, created_at, updated_at"
        ") VALUES("
        "  ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
      "\n")
     (vector 2
             "00000000-0000-0000-0000-project00001"
             nil
             "project"
             "PROJ"
             "proj1"
             1
             "A"
             nil
             nil
             nil
             nil
             nil
             "This is a project node."
             "OPEN"
             nil
             nil
             "2025-12-25T00:00:00+09:00"
             "2025-12-25T00:00:00+09:00"))
    (sqlite-execute
     db
     (string-join
      '("INSERT INTO nodes("
        "  id, uid, parent_id, node_type, todo_keyword, title, level,"
        "  priority, scheduled, deadline, repeat, closed_at, tags,"
        "  content, status, project_id, phase_id, created_at, updated_at"
        ") VALUES("
        "  ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
      "\n")
     (vector 3
             "00000000-0000-0000-0000-today0000002"
             2
             "task"
             "TODO"
             "task-today"
             2
             "A"
             "2025-12-200"
             "2026-01-200"
             nil
             nil
             nil
             "This is a today task."
             "OPEN"
             2
             nil
             "2025-12-25T00:00:00+09:00"
             "2025-12-25T00:00:00+09:00"))))

(defun org-tasktree-search-ert--assert-search-output (title file)
  "Assert search buffer TITLE matches expected FILE."
  (let ((actual (org-tasktree-search-ert--buffer-string title))
        (expected (org-tasktree-search-ert--expected-string file)))
    (unwind-protect
        (should (equal expected actual))
      (org-tasktree-search-ert--cleanup-buffer title))))

(ert-deftest org-tasktree-search-ert-normal-today ()
  "Normal case: search tasks scheduled today."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-today-task))
    (org-tasktree-search-ert--assert-search-output
     "Today"
     "search-normal-01.org")))

(ert-deftest org-tasktree-search-ert-normal-before-today ()
  "Normal case: search tasks scheduled on or before today."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-before-today-task))
    (org-tasktree-search-ert--assert-search-output
     "Before today"
     "search-normal-02.org")))

(ert-deftest org-tasktree-search-ert-normal-overdue ()
  "Normal case: search overdue tasks."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-overdue-task))
    (org-tasktree-search-ert--assert-search-output
     "Overdue"
     "search-normal-03.org")))

(ert-deftest org-tasktree-search-ert-normal-next-7day ()
  "Normal case: search tasks scheduled in the next seven days."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-next-7day-task))
    (org-tasktree-search-ert--assert-search-output
     "Next 7 days"
     "search-normal-04.org")))

(ert-deftest org-tasktree-search-ert-normal-unscheduled ()
  "Normal case: search unscheduled tasks."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-unscheduled-task))
    (org-tasktree-search-ert--assert-search-output
     "Unscheduled"
     "search-normal-05.org")))

(ert-deftest org-tasktree-search-ert-error-today ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-today-task))))

(ert-deftest org-tasktree-search-ert-error-before-today ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-before-today-task))))

(ert-deftest org-tasktree-search-ert-error-overdue ()
  "Abnormal case: invalid deadline values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-overdue-task))))

(ert-deftest org-tasktree-search-ert-error-next-7day ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-next-7day-task))))

(ert-deftest org-tasktree-search-ert-error-unscheduled ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-unscheduled-task))))

(ert-deftest org-tasktree-search-ert-normal-by-query-01 ()
  "Normal case: search by query with all fields specified."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-01.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-01.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-01.yml")
     "by-query-normal-01.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-02 ()
  "Normal case: search by query for project nodes only."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-02.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-02.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-02.yml")
     "by-query-normal-02.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-03 ()
  "Normal case: search by query for project/phase nodes."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-03.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-03.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-03.yml")
     "by-query-normal-03.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-04 ()
  "Normal case: search by query for intermediate nodes (ancestors)."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-04.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-04.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-04.yml")
     "by-query-normal-04.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-05 ()
  "Normal case: search by query for intermediate nodes (descendants)."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-05.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-05.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-05.yml")
     "by-query-normal-05.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-06 ()
  "Normal case: search by query for intermediate nodes (ancestors/descendants)."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-06.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-06.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-06.yml")
     "by-query-normal-06.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-07 ()
  "Normal case: search by query for intermediate nodes only."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-07.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-07.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-07.yml")
     "by-query-normal-07.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-08 ()
  "Normal case: search by query for leaf nodes (ancestors)."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-08.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-08.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-08.yml")
     "by-query-normal-08.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-09 ()
  "Normal case: search by query with not operator."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-09.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-09.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-09.yml")
     "by-query-normal-09.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-10 ()
  "Normal case: search by query with or operator."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-10.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-10.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-10.yml")
     "by-query-normal-10.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-11 ()
  "Normal case: search by query scheduled fixed date."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-11.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-11.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-11.yml")
     "by-query-normal-11.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-12 ()
  "Normal case: search by query scheduled >=."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-12.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-12.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-12.yml")
     "by-query-normal-12.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-13 ()
  "Normal case: search by query scheduled <=."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-13.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-13.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-13.yml")
     "by-query-normal-13.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-14 ()
  "Normal case: search by query scheduled and range."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-14.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-14.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-14.yml")
     "by-query-normal-14.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-15 ()
  "Normal case: search by query scheduled with relative date."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-15.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-15.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-15.yml")
     "by-query-normal-15.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-16 ()
  "Normal case: search by query created_at fixed date."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-16.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-16.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-16.yml")
     "by-query-normal-16.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-17 ()
  "Normal case: search by query created_at >=."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-17.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-17.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-17.yml")
     "by-query-normal-17.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-18 ()
  "Normal case: search by query created_at <=."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-18.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-18.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-18.yml")
     "by-query-normal-18.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-19 ()
  "Normal case: search by query created_at range."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-19.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-19.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-19.yml")
     "by-query-normal-19.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-20 ()
  "Normal case: search by query created_at relative date."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-20.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-20.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-20.yml")
     "by-query-normal-20.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-21 ()
  "Normal case: search by query with missing keys."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-21.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-21.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-21.yml")
     "by-query-normal-21.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-22 ()
  "Normal case: search by query with unknown keys."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-22.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-22.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-22.yml")
     "by-query-normal-22.org")))

(ert-deftest org-tasktree-search-ert-normal-by-query-23 ()
  "Normal case: search by query for all nodes."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-23.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-23.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-23.yml")
     "by-query-normal-23.org")))

(ert-deftest org-tasktree-search-ert-error-by-query-01 ()
  "Abnormal case: invalid query format raises `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-err-01.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-err-01.yml"
      (should-error
       (save-window-excursion
         (org-tasktree-search-by-query))
       :type 'user-error))))

(ert-deftest org-tasktree-search-ert-normal-by-query-empty ()
  "Normal case: empty query returns all nodes."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-err-02.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-err-02.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-err-02.yml")
     "by-query-normal-23.org")))

(ert-deftest org-tasktree-search-ert-error-by-query-03 ()
  "Abnormal case: invalid scheduled date raises `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-err-03.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-err-03.yml"
      (should-error
       (save-window-excursion
         (org-tasktree-search-by-query))
       :type 'user-error))))

(ert-deftest org-tasktree-search-ert-error-by-query-04 ()
  "Abnormal case: invalid scheduled format raises `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-err-04.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-err-04.yml"
      (should-error
       (save-window-excursion
         (org-tasktree-search-by-query))
       :type 'user-error))))

(ert-deftest org-tasktree-search-ert-error-by-query-05 ()
  "Abnormal case: empty values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-err-05.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-err-05.yml"
      (should-error
       (save-window-excursion
         (org-tasktree-search-by-query))
       :type 'user-error))))

(provide 'org-tasktree-search-ert)
;;; org-tasktree-search-ert.el ends here
