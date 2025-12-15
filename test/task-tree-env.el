;;; task-tree-env.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Masahiro Ishida
;;
;; Author: Masahiro Ishida <marmia@MasahironoMacBook-Pro.local>
;; Maintainer: Masahiro Ishida <marmia@MasahironoMacBook-Pro.local>
;; Created: December 14, 2025
;; Modified: December 14, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/marmia/aaa
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(add-to-list 'load-path "/Users/marmia/Projects/develop/org-tasktree/")
(require 'org-tasktree)
(setq org-tasktree-database-location "/Users/marmia/Projects/develop/org-tasktree/.org-tasktree-tmp/tasktree.db")
(setq org-tasktree-query-dir "/Users/marmia/Projects/develop/org-tasktree/.org-tasktree-tmp/queries")

;; org-tasktree-search-today-task
;; org-tasktree-search-before-today-task
;; org-tasktree-search-overdue-task
;; org-tasktree-search-next-7day-task

;; org-tasktree-find-project
;; org-tasktree-find-phase
;; org-tasktree-find-task

(provide 'task-tree-env)
;;; task-tree-env.el ends here

