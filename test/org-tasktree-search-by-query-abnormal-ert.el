;;; org-tasktree-search-by-query-abnormal-ert.el --- Abnormal ERT tests for by-query -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Abnormal-case ERT tests for org-tasktree search-by-query.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-search-by-query-ert)

(ert-deftest org-tasktree-search-by-query-abnormal-ert-error-01 ()
  "Abnormal case: invalid query format raises `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-by-query-ert--seed-data)
    (org-tasktree-search-by-query-ert--install-query-file "by-query-err-01.yml")
    (org-tasktree-search-by-query-ert-with-selection "by-query-err-01.yml"
      (should-error
       (save-window-excursion
         (org-tasktree-search-by-query))
       :type 'user-error))))

(ert-deftest org-tasktree-search-by-query-abnormal-ert-error-02 ()
  "Abnormal case: invalid date raises `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-by-query-ert--seed-data)
    (org-tasktree-search-by-query-ert--install-query-file "by-query-err-02.yml")
    (org-tasktree-search-by-query-ert-with-selection "by-query-err-02.yml"
      (should-error
       (save-window-excursion
         (org-tasktree-search-by-query))
       :type 'user-error))))

(ert-deftest org-tasktree-search-by-query-abnormal-ert-error-03 ()
  "Abnormal case: invalid scheduled date raises `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-by-query-ert--seed-data)
    (org-tasktree-search-by-query-ert--install-query-file "by-query-err-03.yml")
    (org-tasktree-search-by-query-ert-with-selection "by-query-err-03.yml"
      (should-error
       (save-window-excursion
         (org-tasktree-search-by-query))
       :type 'user-error))))

(ert-deftest org-tasktree-search-by-query-abnormal-ert-error-04 ()
  "Abnormal case: invalid scheduled format raises `user-error'."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-by-query-ert--seed-data)
    (org-tasktree-search-by-query-ert--install-query-file "by-query-err-04.yml")
    (org-tasktree-search-by-query-ert-with-selection "by-query-err-04.yml"
      (should-error
       (save-window-excursion
         (org-tasktree-search-by-query))
       :type 'user-error))))

(provide 'org-tasktree-search-by-query-abnormal-ert)
;;; org-tasktree-search-by-query-abnormal-ert.el ends here
