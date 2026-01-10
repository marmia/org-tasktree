;;; org-tasktree-ui.el --- UI helpers for org-tasktree -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; URL: https://github.com/marmia/org-tasktree
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; User-facing helpers for hierarchical `completing-read' flows
;; used by the find-* commands.
;;
;;; Code:

(require 'org-tasktree-ui-minibuffer)
(require 'org-tasktree-ui-edit)

(define-obsolete-variable-alias
  'org-tasktree-ui-completion-color-task
  'org-tasktree-ui-minibuffer-completion-color-task
  "0.1.0")

(define-obsolete-variable-alias
  'org-tasktree-ui-completion-color-project
  'org-tasktree-ui-minibuffer-completion-color-project
  "0.1.0")
(define-obsolete-variable-alias
  'org-tasktree-ui-completion-color-phase
  'org-tasktree-ui-minibuffer-completion-color-phase
  "0.1.0")
(define-obsolete-variable-alias
  'org-tasktree-ui-completion-color-group
  'org-tasktree-ui-minibuffer-completion-color-group
  "0.1.0")

(defun org-tasktree-ui-read-node ()
  "Prompt for a node path in minibuffer."
  (org-tasktree-ui-minibuffer-read-node))

(provide 'org-tasktree-ui)
;;; org-tasktree-ui.el ends here
