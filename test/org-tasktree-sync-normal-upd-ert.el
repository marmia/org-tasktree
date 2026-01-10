;;; org-tasktree-sync-normal-upd-ert.el --- Normal update ERT tests for sync -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:
;;
;; Normal update ERT tests for `org-tasktree-sync-*'.
;; These tests focus on update scenarios using seeded DB rows.
;;

;;; Code:

(require 'ert)
(require 'org-tasktree-model)
(require 'org-tasktree-sync-ert)

(ert-deftest org-tasktree-sync-normal-upd-ert1-full-path ()
  "Normal case: update full path (single tree)."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (aaa-id (org-tasktree-model-node-id (plist-get seed :aaa)))
         (bbb-id (org-tasktree-model-node-id (plist-get seed :bbb)))
         (ccc-id (org-tasktree-model-node-id (plist-get seed :ccc)))
         (ddd-id (org-tasktree-model-node-id (plist-get seed :ddd))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-01.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((aaa (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-aaa))
           (bbb (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-bbb))
           (ccc (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-ccc))
           (ddd (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-ddd))
           (eee (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-eee)))
      (org-tasktree-sync-ert--assert-node
       aaa
       :title "AAA (after upd1)"
       :content "AAA after upd1."
       :status "OPEN"
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                     :tags :parent-id))
      (org-tasktree-sync-ert--assert-node
       bbb
       :title "BBB (after upd1)"
       :content "BBB after upd1."
       :status "OPEN"
       :parent-id aaa-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       ccc
       :title "CCC (after upd1)"
       :content "CCC after upd1."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       ddd
       :title "DDD (after upd1)"
       :todo-keyword "TODO"
       :priority "A"
       :scheduled "2026-02-01"
       :deadline "2026-02-10"
       :repeat ".+1d"
       :tags (org-tasktree-sync-ert--tags-string '("after" "ddd"))
       :content "DDD after upd1."
       :status "OPEN"
       :parent-id ccc-id)
      (org-tasktree-sync-ert--assert-node-tags ddd '("after" "ddd"))
      (org-tasktree-sync-ert--assert-node
       eee
       :title "EEE (after upd1)"
       :content "EEE after upd1."
       :status "OPEN"
       :parent-id ddd-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node-tags eee nil))))

(ert-deftest org-tasktree-sync-normal-upd-ert2-multi-tree ()
  "Normal case: update multiple trees."
  (org-tasktree-sync-ert--seed-update-tree)
  (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-02.org")
  (should (= 8 (org-tasktree-sync-ert--node-count)))
  (let* ((aaa (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-ert--uid-aaa))
         (bbb (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-ert--uid-bbb))
         (ccc (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-ert--uid-ccc))
         (ddd (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-ert--uid-ddd))
         (eee (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-ert--uid-eee))
         (fff (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-ert--uid-fff))
         (ggg (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-ert--uid-ggg))
         (hhh (org-tasktree-sync-ert--fetch-node-by-uid
               org-tasktree-sync-ert--uid-hhh))
         (aaa-id (org-tasktree-model-node-id aaa))
         (bbb-id (org-tasktree-model-node-id bbb))
         (ccc-id (org-tasktree-model-node-id ccc))
         (ddd-id (org-tasktree-model-node-id ddd))
         (fff-id (org-tasktree-model-node-id fff))
         (ggg-id (org-tasktree-model-node-id ggg)))
    (org-tasktree-sync-ert--assert-node
     aaa
     :title "AAA (after upd2)"
     :content "AAA after upd2."
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                   :tags :parent-id))
    (org-tasktree-sync-ert--assert-node
     bbb
     :title "BBB (after upd2)"
     :content "BBB after upd2."
     :status "OPEN"
     :parent-id aaa-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     ccc
     :title "CCC (after upd2)"
     :content "CCC after upd2."
     :status "OPEN"
     :parent-id bbb-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     ddd
     :title "DDD (after upd2)"
     :content "DDD after upd2."
     :status "OPEN"
     :parent-id ccc-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     eee
     :title "EEE (after upd2)"
     :content "EEE after upd2."
     :status "OPEN"
     :parent-id ddd-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     fff
     :title "FFF (after upd2)"
     :content "FFF after upd2."
     :status "OPEN"
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                   :tags :parent-id))
    (org-tasktree-sync-ert--assert-node
     ggg
     :title "GGG (after upd2)"
     :content "GGG after upd2."
     :status "OPEN"
     :parent-id fff-id
     :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
    (org-tasktree-sync-ert--assert-node
     hhh
     :title "HHH (after upd2)"
     :todo-keyword "TODO"
     :priority "B"
     :content "HHH after upd2."
     :status "OPEN"
     :parent-id ggg-id
     :expect-nil '(:scheduled :deadline :repeat :tags))))

(ert-deftest org-tasktree-sync-normal-upd-ert3-partial-with-parent ()
  "Normal case: partial path update with parent in buffer."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (aaa-id (org-tasktree-model-node-id (plist-get seed :aaa)))
         (bbb-id (org-tasktree-model-node-id (plist-get seed :bbb))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-03.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((aaa (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-aaa))
           (bbb (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-bbb))
           (ccc (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-ccc))
           (ccc-id (org-tasktree-model-node-id ccc)))
      (org-tasktree-sync-ert--assert-node
       aaa
       :title "AAA (after upd3-a)"
       :content "AAA after upd3-a."
       :status "OPEN"
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                     :tags :parent-id))
      (org-tasktree-sync-ert--assert-node
       bbb
       :title "BBB (after upd3-a)"
       :content "BBB after upd3-a."
       :status "OPEN"
       :parent-id aaa-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       ccc
       :title "CCC (after upd3-a)"
       :content "CCC after upd3-a."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (should (numberp ccc-id)))))

(ert-deftest org-tasktree-sync-normal-upd-ert4-partial-without-parent ()
  "Normal case: partial path update with parent out of scope."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (aaa-id (org-tasktree-model-node-id (plist-get seed :aaa))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-04.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((bbb (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-bbb))
           (ccc (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-ccc))
           (bbb-id (org-tasktree-model-node-id bbb)))
      (org-tasktree-sync-ert--assert-node
       bbb
       :title "BBB (after upd3-b)"
       :content "BBB after upd3-b."
       :status "OPEN"
       :parent-id aaa-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       ccc
       :title "CCC (after upd3-b)"
       :content "CCC after upd3-b."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(ert-deftest org-tasktree-sync-normal-upd-ert5-restructure-1 ()
  "Normal case: restructure tree 1."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (bbb-id (org-tasktree-model-node-id (plist-get seed :bbb))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-05.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((ddd (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-ddd))
           (eee (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-eee))
           (ddd-id (org-tasktree-model-node-id ddd)))
      (org-tasktree-sync-ert--assert-node
       ddd
       :title "DDD (after upd4)"
       :content "DDD after upd4."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       eee
       :title "EEE (after upd4)"
       :content "EEE after upd4."
       :status "OPEN"
       :parent-id ddd-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(ert-deftest org-tasktree-sync-normal-upd-ert6-restructure-2 ()
  "Normal case: restructure tree 2."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (bbb-id (org-tasktree-model-node-id (plist-get seed :bbb)))
         (ccc-id (org-tasktree-model-node-id (plist-get seed :ccc))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-06.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((ddd (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-ddd))
           (eee (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-eee)))
      (org-tasktree-sync-ert--assert-node
       ddd
       :title "DDD (after upd5)"
       :content "DDD after upd5."
       :status "OPEN"
       :parent-id bbb-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       eee
       :title "EEE (after upd5)"
       :content "EEE after upd5."
       :status "OPEN"
       :parent-id ccc-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(ert-deftest org-tasktree-sync-normal-upd-ert7-restructure-3 ()
  "Normal case: restructure tree 3."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (hhh-id (org-tasktree-model-node-id (plist-get seed :hhh))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-07.org")
    (should (= 8 (org-tasktree-sync-ert--node-count)))
    (let* ((ddd (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-ddd))
           (eee (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-eee))
           (ddd-id (org-tasktree-model-node-id ddd)))
      (org-tasktree-sync-ert--assert-node
       ddd
       :title "DDD (after upd6)"
       :content "DDD after upd6."
       :status "OPEN"
       :parent-id hhh-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       eee
       :title "EEE (after upd6)"
       :content "EEE after upd6."
       :status "OPEN"
       :parent-id ddd-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(ert-deftest org-tasktree-sync-normal-upd-ert8-add-new-nodes ()
  "Normal case: add new nodes under existing tree."
  (let* ((seed (org-tasktree-sync-ert--seed-update-tree))
         (hhh-id (org-tasktree-model-node-id (plist-get seed :hhh))))
    (org-tasktree-sync-ert--sync-file-without-reset "sync-normal-upd-08.org")
    (should (= 10 (org-tasktree-sync-ert--node-count)))
    (let* ((fff (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-fff))
           (ggg (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-ggg))
           (hhh (org-tasktree-sync-ert--fetch-node-by-uid
                 org-tasktree-sync-ert--uid-hhh))
           (iii (org-tasktree-sync-ert--fetch-node-by-title "III (new)" hhh-id))
           (iii-id (org-tasktree-model-node-id iii))
           (jjj (org-tasktree-sync-ert--fetch-node-by-title "JJJ (new)" iii-id)))
      (org-tasktree-sync-ert--assert-node
       fff
       :title "FFF (after upd8)"
       :content "FFF after upd8."
       :status "OPEN"
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat
                     :tags :parent-id))
      (org-tasktree-sync-ert--assert-node
       ggg
       :title "GGG (after upd8)"
       :content "GGG after upd8."
       :status "OPEN"
       :parent-id (org-tasktree-model-node-id fff)
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       hhh
       :title "HHH (after upd8)"
       :content "HHH after upd8."
       :status "OPEN"
       :parent-id (org-tasktree-model-node-id ggg)
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       iii
       :title "III (new)"
       :todo-keyword "TODO"
       :priority "B"
       :scheduled "2026-02-15"
       :content "III new node."
       :status "OPEN"
       :parent-id hhh-id
       :expect-nil '(:deadline :repeat :tags))
      (org-tasktree-sync-ert--assert-node
       jjj
       :title "JJJ (new)"
       :content "JJJ new node."
       :status "OPEN"
       :parent-id iii-id
       :expect-nil '(:todo-keyword :priority :scheduled :deadline :repeat :tags)))))

(provide 'org-tasktree-sync-normal-upd-ert)
;;; org-tasktree-sync-normal-upd-ert.el ends here
