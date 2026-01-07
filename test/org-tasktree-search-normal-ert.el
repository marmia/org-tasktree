;;; org-tasktree-search-normal-ert.el --- Normal ERT tests for search -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Normal-case ERT tests for org-tasktree search commands.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-search-ert)

(ert-deftest org-tasktree-search-normal-ert-today ()
  "Normal case: search tasks scheduled today."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-today-task))
    (org-tasktree-search-ert--assert-search-output
     "Today"
     "search-normal-01.org")))

(ert-deftest org-tasktree-search-normal-ert-before-today ()
  "Normal case: search tasks scheduled on or before today."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-before-today-task))
    (org-tasktree-search-ert--assert-search-output
     "Before today"
     "search-normal-02.org")))

(ert-deftest org-tasktree-search-normal-ert-overdue ()
  "Normal case: search overdue tasks."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-overdue-task))
    (org-tasktree-search-ert--assert-search-output
     "Overdue"
     "search-normal-03.org")))

(ert-deftest org-tasktree-search-normal-ert-next-7day ()
  "Normal case: search tasks scheduled in the next seven days."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-next-7day-task))
    (org-tasktree-search-ert--assert-search-output
     "Next 7 days"
     "search-normal-04.org")))

(ert-deftest org-tasktree-search-normal-ert-unscheduled ()
  "Normal case: search unscheduled tasks."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-unscheduled-task))
    (org-tasktree-search-ert--assert-search-output
     "Unscheduled"
     "search-normal-05.org")))

(ert-deftest org-tasktree-search-normal-ert-all ()
  "Normal case: search all nodes."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-normal-data)
    (save-window-excursion
      (org-tasktree-search-all))
    (org-tasktree-search-ert--assert-search-output
     "All"
     "search-normal-06.org")))

(provide 'org-tasktree-search-normal-ert)
;;; org-tasktree-search-normal-ert.el ends here
