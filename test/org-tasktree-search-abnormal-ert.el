;;; org-tasktree-search-abnormal-ert.el --- Abnormal ERT tests for search -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Abnormal-case ERT tests for org-tasktree search commands.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-search-ert)

(ert-deftest org-tasktree-search-abnormal-ert-error-today ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-today-task)
                  :type 'user-error)))

(ert-deftest org-tasktree-search-abnormal-ert-error-before-today ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-before-today-task)
                  :type 'user-error)))

(ert-deftest org-tasktree-search-abnormal-ert-error-overdue ()
  "Abnormal case: invalid deadline values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-overdue-task)
                  :type 'user-error)))

(ert-deftest org-tasktree-search-abnormal-ert-error-next-7day ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-next-7day-task)
                  :type 'user-error)))

(ert-deftest org-tasktree-search-abnormal-ert-error-unscheduled ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-unscheduled-task)
                  :type 'user-error)))

(ert-deftest org-tasktree-search-abnormal-ert-error-all ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-all)
                  :type 'user-error)))

(ert-deftest org-tasktree-search-abnormal-ert-error-open ()
  "Abnormal case: invalid scheduled values raise `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should-error (org-tasktree-search-open)
                  :type 'user-error)))

(ert-deftest org-tasktree-search-abnormal-ert-open-tree-allows-invalid-dates ()
  "Abnormal case: open-tree completion allows invalid dates."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-invalid-date-data)
    (should (= 2 (length (org-tasktree-query-open-tree))))))

(provide 'org-tasktree-search-abnormal-ert)
;;; org-tasktree-search-abnormal-ert.el ends here
