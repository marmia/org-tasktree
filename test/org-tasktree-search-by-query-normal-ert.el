;;; org-tasktree-search-by-query-normal-ert.el --- Normal ERT tests for by-query -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Normal-case ERT tests for org-tasktree search-by-query.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-search-ert)

(ert-deftest org-tasktree-search-by-query-normal-ert-01 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-02 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-03 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-04 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-05 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-06 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-07 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-08 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-09 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-10 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-11 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-12 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-13 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-14 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-15 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-16 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-17 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-18 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-19 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-20 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-21 ()
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

(ert-deftest org-tasktree-search-by-query-normal-ert-22 ()
  "Normal case: empty query returns all nodes."
  (org-tasktree-test-helper-with-fixed-time org-tasktree-search-ert--base-time
    (org-tasktree-search-ert--seed-by-query-data)
    (org-tasktree-search-ert--install-query-file "by-query-normal-22.yml")
    (org-tasktree-search-ert-with-query-selection "by-query-normal-22.yml"
      (save-window-excursion
        (org-tasktree-search-by-query)))
    (org-tasktree-search-ert--assert-search-output
     (org-tasktree-search-ert--query-title "by-query-normal-22.yml")
     "by-query-normal-21.org")))

(provide 'org-tasktree-search-by-query-normal-ert)
;;; org-tasktree-search-by-query-normal-ert.el ends here
