;;; org-tasktree-ui-minibuffer.el --- Minibuffer navigation for org-tasktree -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;; Package: org-tasktree-ui-minibuffer
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; Minibuffer navigation helpers for org-tasktree find-* commands.
;; Provides completion candidates, navigation state handling, and prompts.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-tasktree-model)
(require 'org-tasktree-query)

(defconst org-tasktree-ui-minibuffer--path-sep " > "
  "Separator for hierarchical paths in `completing-read' candidates.")

(defcustom org-tasktree-ui-minibuffer-completion-color-task "white"
  "Foreground color for task candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-minibuffer-completion-color-project "DeepSkyBlue"
  "Foreground color for project candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-minibuffer-completion-color-phase "MediumSeaGreen"
  "Foreground color for phase candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-minibuffer-completion-color-group "DarkOrange"
  "Foreground color for group candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defvar org-tasktree-ui-minibuffer--last-exit-kind nil
  "Exit kind of the last org-tasktree minibuffer session.")

(defvar-local org-tasktree-ui-minibuffer--minibuffer-backspace-original nil
  "Original Backspace command in the minibuffer.")

(defvar-local org-tasktree-ui-minibuffer--minibuffer-backspace-up-enabled nil
  "Non-nil means Backspace goes up when minibuffer is empty.")

(defvar-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-enabled nil
  "Non-nil means a fully typed directory candidate auto-confirms.")

(defvar-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-candidates nil
  "List of directory candidate strings eligible for minibuffer auto-confirm.")

(defvar-local org-tasktree-ui-minibuffer--minibuffer-original-local-map nil
  "Original minibuffer local map before org-tasktree keymap wrapping.")

(defvar-local org-tasktree-ui-minibuffer--minibuffer-validate-fn nil
  "Validation function for org-tasktree minibuffer input.")

(defun org-tasktree-ui-minibuffer--completion-color (type)
  "Return configured completion color for TYPE symbol."
  (pcase type
    ('task org-tasktree-ui-minibuffer-completion-color-task)
    ('project org-tasktree-ui-minibuffer-completion-color-project)
    ('phase org-tasktree-ui-minibuffer-completion-color-phase)
    ('group org-tasktree-ui-minibuffer-completion-color-group)
    (_ nil)))

(defun org-tasktree-ui-minibuffer--normalize-tags (tags)
  "Return downcased tag list derived from TAGS."
  (let (result)
    (cond
     ((stringp tags)
      (dolist (tag (split-string tags ":" t))
        (let ((trimmed (string-trim tag)))
          (unless (string-empty-p trimmed)
            (push (downcase trimmed) result)))))
     ((listp tags)
      (dolist (tag tags)
        (let* ((raw (cond
                     ((stringp tag) tag)
                     ((symbolp tag) (symbol-name tag))
                     (t nil)))
               (trimmed (and raw (string-trim raw))))
          (when (and trimmed (not (string-empty-p trimmed)))
            (push (downcase trimmed) result))))))
    (nreverse result)))

(defun org-tasktree-ui-minibuffer--type-from-tags (tags)
  "Return candidate type symbol inferred from TAGS."
  (let ((tag-list (org-tasktree-ui-minibuffer--normalize-tags tags)))
    (cond
     ((member "project" tag-list) 'project)
     ((member "phase" tag-list) 'phase)
     ((member "group" tag-list) 'group)
     (t 'task))))

(defun org-tasktree-ui-minibuffer--make-completion-candidate (title type)
  "Return a propertized completion candidate for TITLE and TYPE.

TYPE is one of the symbols `project', `phase', `group', or `task'."
  (let* ((suffix (pcase type ((or 'project 'phase 'group) "/") (_ "")))
         (display (concat title suffix))
         (color (org-tasktree-ui-minibuffer--completion-color type))
         (face (and (stringp color) (not (string-empty-p color))
                    `(:foreground ,color))))
    (propertize display
                'face face
                'org-tasktree-ui-minibuffer--raw title
                'org-tasktree-ui-minibuffer--candidate-type type)))

(defun org-tasktree-ui-minibuffer--make-path-candidate (path type)
  "Return a propertized completion candidate for PATH and TYPE."
  (let* ((color (org-tasktree-ui-minibuffer--completion-color type))
         (face (and (stringp color) (not (string-empty-p color))
                    `(:foreground ,color))))
    (propertize path
                'face face
                'org-tasktree-ui-minibuffer--raw path
                'org-tasktree-ui-minibuffer--candidate-type type)))

(defun org-tasktree-ui-minibuffer--candidate-raw (candidate)
  "Return the raw title string for completion CANDIDATE."
  (or (get-text-property 0 'org-tasktree-ui-minibuffer--raw candidate)
      (if (string-suffix-p "/" candidate)
          (string-remove-suffix "/" candidate)
        candidate)))

(defun org-tasktree-ui-minibuffer--candidate-type (candidate)
  "Return the candidate type symbol for completion CANDIDATE, or nil."
  (get-text-property 0 'org-tasktree-ui-minibuffer--candidate-type candidate))

(defun org-tasktree-ui-minibuffer--titles (nodes)
  "Return list of titles from NODES."
  (mapcar #'org-tasktree-model-node-title nodes))

(defun org-tasktree-ui-minibuffer--sorted-strings (strings)
  "Return a sorted copy of STRINGS."
  (sort (copy-sequence strings) #'string<))

(defun org-tasktree-ui-minibuffer--sorted-titles (nodes)
  "Return sorted list of titles from NODES."
  (org-tasktree-ui-minibuffer--sorted-strings (org-tasktree-ui-minibuffer--titles nodes)))

(defun org-tasktree-ui-minibuffer--minibuffer-backspace ()
  "Handle Backspace in org-tasktree minibuffer."
  (interactive)
  (if (and org-tasktree-ui-minibuffer--minibuffer-backspace-up-enabled
           (string-empty-p (minibuffer-contents-no-properties)))
      (throw 'org-tasktree-ui-minibuffer--minibuffer-up :up)
    (call-interactively
     (or org-tasktree-ui-minibuffer--minibuffer-backspace-original
         #'delete-backward-char))))

(defun org-tasktree-ui-minibuffer--minibuffer-shift-enter ()
  "Handle Shift+Enter in org-tasktree minibuffer."
  (interactive)
  (setq org-tasktree-ui-minibuffer--last-exit-kind :shift-enter)
  (org-tasktree-ui-minibuffer--minibuffer-accept))

(defun org-tasktree-ui-minibuffer--vertico-selected-candidate ()
  "Return selected candidate string when Vertico is active, or nil."
  (when (and (featurep 'vertico)
             (boundp 'vertico--candidates)
             (boundp 'vertico--index))
    (let* ((index vertico--index)
           (cands vertico--candidates)
           (cand (cond
                  ((and (integerp index) (>= index 0) (vectorp cands)
                        (< index (length cands)))
                   (aref cands index))
                  ((and (integerp index) (>= index 0) (listp cands))
                   (nth index cands))
                  (t nil))))
      (when (stringp cand)
        (substring-no-properties cand)))))

(defun org-tasktree-ui-minibuffer--minibuffer-accept ()
  "Handle RET in org-tasktree minibuffer."
  (interactive)
  (let ((fn org-tasktree-ui-minibuffer--minibuffer-validate-fn))
    (if (not fn)
        (exit-minibuffer)
      (condition-case err
          (progn
            (when (string-empty-p (minibuffer-contents-no-properties))
              (let ((candidate (org-tasktree-ui-minibuffer--vertico-selected-candidate)))
                (when (and candidate (not (string-empty-p candidate)))
                  (delete-minibuffer-contents)
                  (insert candidate))))
            (funcall fn (minibuffer-contents-no-properties))
            (exit-minibuffer))
        (user-error
         (minibuffer-message "%s" (error-message-string err)))))))

(defun org-tasktree-ui-minibuffer--minibuffer-cleanup ()
  "Cleanup hooks and buffer-local state for org-tasktree minibuffer sessions."
  (remove-hook 'post-command-hook #'org-tasktree-ui-minibuffer--minibuffer-maybe-auto-enter t)
  (remove-hook 'minibuffer-exit-hook #'org-tasktree-ui-minibuffer--minibuffer-cleanup t)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-enabled nil)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-candidates nil)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-backspace-up-enabled nil)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-backspace-original nil)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-validate-fn nil)
  (when (keymapp org-tasktree-ui-minibuffer--minibuffer-original-local-map)
    (use-local-map org-tasktree-ui-minibuffer--minibuffer-original-local-map))
  (setq-local org-tasktree-ui-minibuffer--minibuffer-original-local-map nil))

(defun org-tasktree-ui-minibuffer--minibuffer-maybe-auto-enter ()
  "Auto-confirm when minibuffer input matches a directory candidate exactly."
  (when (and org-tasktree-ui-minibuffer--minibuffer-auto-enter-enabled
             (minibufferp)
             (consp org-tasktree-ui-minibuffer--minibuffer-auto-enter-candidates))
    (let ((input (minibuffer-contents-no-properties))
          (buf (current-buffer)))
      (when (member input org-tasktree-ui-minibuffer--minibuffer-auto-enter-candidates)
        (run-at-time
         0
         nil
         (lambda ()
           (when (and (buffer-live-p buf) (minibufferp (buffer-name buf)))
             (with-current-buffer buf
               (exit-minibuffer)))))))))

(defun org-tasktree-ui-minibuffer--completing-read
    (prompt cands &optional require-match allow-backspace-up allow-auto-enter validate-fn)
  "Read a completion from CANDS with PROMPT.

When ALLOW-BACKSPACE-UP is non-nil, pressing Backspace in an empty minibuffer
returns the symbol `:up'.

When ALLOW-AUTO-ENTER is non-nil, typing an exact directory candidate (ending
with \"/\") auto-confirms and proceeds without requiring RET.

When VALIDATE-FN is non-nil, it is called with minibuffer input before exit.
Signal `user-error' to keep minibuffer open and show the error."
  (catch 'org-tasktree-ui-minibuffer--minibuffer-up
    (minibuffer-with-setup-hook
        (lambda ()
          ;; Defer keymap wrapping so other completion frameworks (e.g. Vertico)
          ;; have already installed their minibuffer keymap bindings.
          (let ((buf (current-buffer))
                (allow allow-backspace-up)
                (auto allow-auto-enter))
            (run-at-time
             0
             nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq-local org-tasktree-ui-minibuffer--minibuffer-backspace-up-enabled
                               (and allow t))
                   (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-enabled
                               (and auto t))
                   (setq-local org-tasktree-ui-minibuffer--minibuffer-original-local-map
                               (current-local-map))
                   (setq-local org-tasktree-ui-minibuffer--minibuffer-validate-fn
                               validate-fn)
                   (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-candidates
                               (when auto
                                 (let (result)
                                   (dolist (cand cands (nreverse result))
                                     (let ((s (substring-no-properties cand)))
                                       (when (and (string-suffix-p "/" s)
                                                  (memq (org-tasktree-ui-minibuffer--candidate-type
                                                         cand)
                                                        '(project phase group task)))
                                         (push s result)))))))
                   (when org-tasktree-ui-minibuffer--minibuffer-auto-enter-enabled
                     (add-hook 'post-command-hook
                               #'org-tasktree-ui-minibuffer--minibuffer-maybe-auto-enter
                               nil
                               t))
                   (add-hook 'minibuffer-exit-hook
                             #'org-tasktree-ui-minibuffer--minibuffer-cleanup
                             nil
                             t)
                   (setq-local org-tasktree-ui-minibuffer--minibuffer-backspace-original
                               (or (key-binding (kbd "DEL") t)
                                   (key-binding (kbd "<backspace>") t)
                                   #'delete-backward-char))
                   (let ((map (make-sparse-keymap))
                         (backspace #'org-tasktree-ui-minibuffer--minibuffer-backspace)
                         (shift-enter #'org-tasktree-ui-minibuffer--minibuffer-shift-enter)
                         (accept #'org-tasktree-ui-minibuffer--minibuffer-accept))
                     (set-keymap-parent map (current-local-map))
                     (define-key map (kbd "DEL") backspace)
                     (define-key map (kbd "<backspace>") backspace)
                     (define-key map (kbd "S-<return>") shift-enter)
                     (define-key map (kbd "S-RET") shift-enter)
                     (when validate-fn
                       (define-key map (kbd "RET") accept)
                       (define-key map (kbd "<return>") accept)
                       (define-key map (kbd "C-m") accept)
                       (define-key map (kbd "C-j") accept))
                     (use-local-map map))))))))
      (completing-read prompt cands nil require-match))))

(defun org-tasktree-ui-minibuffer--id-map (nodes)
  "Return hash map of id->node from NODES."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (node nodes table)
      (when (org-tasktree-model-node-id node)
        (puthash (org-tasktree-model-node-id node) node table)))))

(defun org-tasktree-ui-minibuffer--titles-path (node id-map)
  "Return list of titles from root to NODE using ID-MAP."
  (let ((titles '())
        (current node))
    (while current
      (push (org-tasktree-model-node-title current) titles)
      (setq current (gethash
                     (org-tasktree-model-node-parent-id current)
                     id-map)))
    titles))

(defun org-tasktree-ui-minibuffer--format-path (titles)
  "Join TITLES into a display path string."
  (string-join titles org-tasktree-ui-minibuffer--path-sep))

(defun org-tasktree-ui-minibuffer--format-path-slash (titles)
  "Join TITLES into a slash-separated path."
  (string-join titles "/"))

(defun org-tasktree-ui-minibuffer--normalize-node-path (input)
  "Normalize INPUT string as node path.
Strip trailing slashes and signal `user-error' when invalid."
  (let ((trimmed (string-trim (or input ""))))
    (when (string-empty-p trimmed)
      (user-error "Node path is required"))
    (when (string-prefix-p "/" trimmed)
      (user-error "Node path must not start with '/'"))
    (while (string-suffix-p "/" trimmed)
      (setq trimmed (substring trimmed 0 -1)))
    (when (string-empty-p trimmed)
      (user-error "Node path is required"))
    (when (string-match-p "//" trimmed)
      (user-error "Node path must not contain empty segments"))
    trimmed))

(defun org-tasktree-ui-minibuffer-read-node ()
  "Prompt for node path via `completing-read'.
Return plist describing an existing or new node selection."
  (let* ((nodes (org-tasktree-query-open-tree))
         (id-map (org-tasktree-ui-minibuffer--id-map nodes))
         (entries nil)
         (path-table (make-hash-table :test 'equal)))
    (dolist (node nodes)
      (let* ((titles (org-tasktree-ui-minibuffer--titles-path node id-map))
             (path (org-tasktree-ui-minibuffer--format-path-slash titles))
             (entry (list :path path :node node :titles titles)))
        (push entry entries)
        (puthash path entry path-table)))
    (setq entries
          (sort entries
                (lambda (a b)
                  (string< (plist-get a :path)
                           (plist-get b :path)))))
    (let* ((display-cands
            (sort
             (mapcar
              (lambda (entry)
                (let* ((node (plist-get entry :node))
                       (type (org-tasktree-ui-minibuffer--type-from-tags
                              (org-tasktree-model-node-tags node))))
                  (org-tasktree-ui-minibuffer--make-path-candidate
                   (plist-get entry :path)
                   type)))
              entries)
             (lambda (a b)
               (string< (substring-no-properties a)
                        (substring-no-properties b)))))
           (validate-fn
            (lambda (input)
              (let* ((path (org-tasktree-ui-minibuffer--normalize-node-path input))
                     (existing (gethash path path-table)))
                (unless existing
                  (let* ((segments (split-string path "/" t))
                         (new-title (car (last segments)))
                         (parent-segments (butlast segments))
                         (parent-path (org-tasktree-ui-minibuffer--format-path-slash
                                       parent-segments))
                         (parent-entry (and parent-segments
                                            (gethash parent-path path-table))))
                    (when (string-empty-p (or new-title ""))
                      (user-error "Node title is required"))
                    (when (and parent-segments (not parent-entry))
                      (user-error "Parent path not found: %s" parent-path)))))))
           (choice (let ((completion-extra-properties
                          '(:display-sort-function identity :cycle-sort-function identity)))
                     (org-tasktree-ui-minibuffer--completing-read
                      "find node: "
                      display-cands
                      nil
                      nil
                      nil
                      validate-fn)))
           (raw (org-tasktree-ui-minibuffer--candidate-raw choice))
           (path (org-tasktree-ui-minibuffer--normalize-node-path raw))
           (existing (gethash path path-table)))
      (if existing
          (list :existing t
                :node (plist-get existing :node)
                :path-titles (plist-get existing :titles)
                :path path)
        (let* ((segments (split-string path "/" t))
               (new-title (car (last segments)))
               (parent-segments (butlast segments))
               (parent-path (org-tasktree-ui-minibuffer--format-path-slash
                             parent-segments))
               (parent-entry (and parent-segments
                                  (gethash parent-path path-table))))
          (list :existing nil
                :title new-title
                :parent-node (and parent-entry (plist-get parent-entry :node))
                :parent-path-titles (and parent-entry
                                         (plist-get parent-entry :titles))
                :path path))))))

(provide 'org-tasktree-ui-minibuffer)
;;; org-tasktree-ui-minibuffer.el ends here
