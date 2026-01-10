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
(require 'org-tasktree-search-by-query-ert)

(defconst org-tasktree-search-by-query-normal-ert--cases
  (list
   (list :name "01"
         :query "by-query-normal-01.yml"
         :expected "by-query-normal-01.org")
   (list :name "02"
         :query "by-query-normal-02.yml"
         :expected "by-query-normal-02.org")
   (list :name "03"
         :query "by-query-normal-03.yml"
         :expected "by-query-normal-03.org")
   (list :name "04"
         :query "by-query-normal-04.yml"
         :expected "by-query-normal-04.org")
   (list :name "05"
         :query "by-query-normal-05.yml"
         :expected "by-query-normal-05.org")
   (list :name "06"
         :query "by-query-normal-06.yml"
         :expected "by-query-normal-06.org")
   (list :name "07"
         :query "by-query-normal-07.yml"
         :expected "by-query-normal-07.org")
   (list :name "08"
         :query "by-query-normal-08.yml"
         :expected "by-query-normal-08.org")
   (list :name "09"
         :query "by-query-normal-09.yml"
         :expected "by-query-normal-09.org")
   (list :name "10"
         :query "by-query-normal-10.yml"
         :expected "by-query-normal-10.org")
   (list :name "11"
         :query "by-query-normal-11.yml"
         :expected "by-query-normal-11.org")
   (list :name "12"
         :query "by-query-normal-12.yml"
         :expected "by-query-normal-12.org")
   (list :name "13"
         :query "by-query-normal-13.yml"
         :expected "by-query-normal-13.org")
   (list :name "14"
         :query "by-query-normal-14.yml"
         :expected "by-query-normal-14.org")
   (list :name "15"
         :query "by-query-normal-15.yml"
         :expected "by-query-normal-15.org")
   (list :name "16"
         :query "by-query-normal-16.yml"
         :expected "by-query-normal-16.org")
   (list :name "17"
         :query "by-query-normal-17.yml"
         :expected "by-query-normal-17.org")
   (list :name "18"
         :query "by-query-normal-18.yml"
         :expected "by-query-normal-18.org")
   (list :name "19"
         :query "by-query-normal-19.yml"
         :expected "by-query-normal-19.org")
   (list :name "20"
         :query "by-query-normal-20.yml"
         :expected "by-query-normal-20.org")
   (list :name "21"
         :query "by-query-normal-21.yml"
         :expected "by-query-normal-21.org")
   (list :name "22"
         :query "by-query-normal-22.yml"
         :expected "by-query-normal-21.org")
   (list :name "23"
         :query "by-query-normal-23.yml"
         :message "org-tasktree: no results"))
  "Test cases for org-tasktree search-by-query normal paths.")

(ert-deftest org-tasktree-search-by-query-normal-ert-cases ()
  "Normal cases: search-by-query returns expected output."
  (dolist (case org-tasktree-search-by-query-normal-ert--cases)
    (ert-info ((format "case=%s query=%s"
                       (plist-get case :name)
                       (plist-get case :query)))
      (org-tasktree-search-by-query-ert-run-case case))))

(provide 'org-tasktree-search-by-query-normal-ert)
;;; org-tasktree-search-by-query-normal-ert.el ends here
