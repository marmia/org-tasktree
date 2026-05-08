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

(defcustom org-tasktree-ui-minibuffer-completion-color-domain "#40C97E"
  "Foreground color for domain candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-minibuffer-completion-color-project "#66BFFF"
  "Foreground color for project candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-minibuffer-completion-color-phase "#F08AC2"
  "Foreground color for phase candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-minibuffer-completion-color-group "#FFB366"
  "Foreground color for group candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defvar org-tasktree-ui-minibuffer--last-exit-kind nil
  "Exit kind of the last org-tasktree minibuffer session.")

(defvar org-tasktree-ui-minibuffer--marginalia-path-table nil
  "Hash table mapping paths to entries for Marginalia annotations.")

(defvar org-tasktree-ui-minibuffer--marginalia-column-widths nil
  "Plist storing column widths for Marginalia annotations.")

(defconst org-tasktree-ui-minibuffer--match-suffix-separator "\t"
  "Separator used to append match-only text to completion candidates.")

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

(defvar org-tasktree-ui-minibuffer--marginalia-registered nil
  "Non-nil when the Marginalia annotator is registered for org-tasktree.")

(defun org-tasktree-ui-minibuffer--completion-color-value (symbol default)
  "Return completion color SYMBOL value, or DEFAULT when SYMBOL is unbound."
  (if (boundp symbol)
      (symbol-value symbol)
    default))

(defun org-tasktree-ui-minibuffer--maybe-register-marginalia ()
  "Register Marginalia annotator for `org-tasktree-find-node'."
  (when (and (not org-tasktree-ui-minibuffer--marginalia-registered)
             (or (boundp 'marginalia-annotators)
                 (boundp 'marginalia-annotator-registry)))
    (when (boundp 'marginalia-annotators)
      (unless (assq 'org-tasktree-node marginalia-annotators)
        (add-to-list 'marginalia-annotators
                     '(org-tasktree-node
                       org-tasktree-ui-minibuffer--marginalia-annotate))))
    (when (boundp 'marginalia-annotator-registry)
      (add-to-list 'marginalia-annotator-registry
                   '(org-tasktree-node
                     . org-tasktree-ui-minibuffer--marginalia-annotate)))
    (when (boundp 'marginalia-command-categories)
      (add-to-list 'marginalia-command-categories
                   '(org-tasktree-find-node . org-tasktree-node)))
    (setq org-tasktree-ui-minibuffer--marginalia-registered t)))

(defun org-tasktree-ui-minibuffer--completion-color (type)
  "Return configured completion color for TYPE symbol."
  (pcase type
    ('task
     (org-tasktree-ui-minibuffer--completion-color-value
      'org-tasktree-ui-minibuffer-completion-color-task "white"))
    ('domain
     (org-tasktree-ui-minibuffer--completion-color-value
      'org-tasktree-ui-minibuffer-completion-color-domain "#40C97E"))
    ('project
     (org-tasktree-ui-minibuffer--completion-color-value
      'org-tasktree-ui-minibuffer-completion-color-project "#66BFFF"))
    ('phase
     (org-tasktree-ui-minibuffer--completion-color-value
      'org-tasktree-ui-minibuffer-completion-color-phase "#F08AC2"))
    ('group
     (org-tasktree-ui-minibuffer--completion-color-value
      'org-tasktree-ui-minibuffer-completion-color-group "#FFB366"))
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
     ((member "domain" tag-list) 'domain)
     ((member "project" tag-list) 'project)
     ((member "phase" tag-list) 'phase)
     ((member "group" tag-list) 'group)
     (t 'task))))

(defun org-tasktree-ui-minibuffer--make-completion-candidate
    (title type &optional todo-keyword tags)
  "Return a propertized completion candidate for TITLE and TYPE.

TYPE is one of the symbols `domain', `project', `phase', `group', or `task'.
TODO-KEYWORD and TAGS are stored as candidate metadata."
  (let* ((suffix (pcase type ((or 'domain 'project 'phase 'group) "/") (_ "")))
         (display (concat title suffix))
         (color (org-tasktree-ui-minibuffer--completion-color type))
         (face (and (stringp color) (not (string-empty-p color))
                    `(:foreground ,color))))
    (propertize display
                'face face
                'org-tasktree-ui-minibuffer--raw title
                'org-tasktree-ui-minibuffer--candidate-type type
                'org-tasktree-ui-minibuffer--todo-keyword todo-keyword
                'org-tasktree-ui-minibuffer--tags tags)))

(defun org-tasktree-ui-minibuffer--make-path-candidate
    (path type &optional todo-keyword tags)
  "Return a propertized completion candidate for PATH and TYPE.

TODO-KEYWORD and TAGS are stored as candidate metadata."
  (let* ((color (org-tasktree-ui-minibuffer--completion-color type))
         (face (and (stringp color) (not (string-empty-p color))
                    `(:foreground ,color)))
         (suffix (org-tasktree-ui-minibuffer--match-suffix
                  todo-keyword
                  tags))
         (match (if suffix (concat path suffix) path))
         (display (if face (propertize path 'face face) path)))
    (propertize match
                'display display
                'face face
                'org-tasktree-ui-minibuffer--raw path
                'org-tasktree-ui-minibuffer--candidate-type type
                'org-tasktree-ui-minibuffer--todo-keyword todo-keyword
                'org-tasktree-ui-minibuffer--tags tags)))

(defun org-tasktree-ui-minibuffer--candidate-raw (candidate)
  "Return the raw title string for completion CANDIDATE."
  (or (get-text-property 0 'org-tasktree-ui-minibuffer--raw candidate)
      (if (string-suffix-p "/" candidate)
          (string-remove-suffix "/" candidate)
        candidate)))

(defun org-tasktree-ui-minibuffer--match-suffix (todo tags)
  "Return match-only suffix for TODO and TAGS, or nil."
  (let ((todo-text (and (stringp todo) (not (string-empty-p todo)) todo))
        (tags-text (and (stringp tags) (not (string-empty-p tags)) tags)))
    (when (or todo-text tags-text)
      (concat org-tasktree-ui-minibuffer--match-suffix-separator
              (string-join (delq nil (list todo-text tags-text)) " ")))))

(defun org-tasktree-ui-minibuffer--strip-match-suffix (text)
  "Return TEXT without match-only suffix."
  (let ((pos (and (stringp text)
                  (string-match-p
                   org-tasktree-ui-minibuffer--match-suffix-separator text))))
    (if (and pos (>= pos 0))
        (substring text 0 pos)
      text)))

(defun org-tasktree-ui-minibuffer--candidate-type (candidate)
  "Return the candidate type symbol for completion CANDIDATE, or nil."
  (get-text-property 0 'org-tasktree-ui-minibuffer--candidate-type candidate))

(defun org-tasktree-ui-minibuffer--marginalia-entry (candidate)
  "Return path entry for CANDIDATE in Marginalia."
  (let ((table org-tasktree-ui-minibuffer--marginalia-path-table))
    (when (hash-table-p table)
      (gethash (org-tasktree-ui-minibuffer--candidate-raw candidate) table))))

(defun org-tasktree-ui-minibuffer--annotation-raw (candidate)
  "Return raw annotation string for CANDIDATE."
  (let* ((entry (org-tasktree-ui-minibuffer--marginalia-entry candidate))
         (node (plist-get entry :node))
         (todo (and node (org-tasktree-model-node-todo-keyword node)))
         (tags (and node (org-tasktree-model-node-tags node)))
         (todo-text (and (stringp todo) (not (string-empty-p todo)) todo))
         (tags-text (and (stringp tags) (not (string-empty-p tags)) tags))
         (parts (delq nil (list todo-text tags-text))))
    (if parts
        (string-join parts " ")
      "")))

(defun org-tasktree-ui-minibuffer--marginalia-column-width (key)
  "Return column width for KEY in Marginalia annotations."
  (let ((width (plist-get org-tasktree-ui-minibuffer--marginalia-column-widths
                          key)))
    (when (and (integerp width) (> width 0))
      width)))

(defun org-tasktree-ui-minibuffer--marginalia-pad (value width)
  "Return VALUE padded to WIDTH for Marginalia display."
  (let ((text (or value "")))
    (if (and (integerp width) (> width 0))
        (format (format "%%-%ds" width) text)
      text)))

(defun org-tasktree-ui-minibuffer--marginalia-annotate (candidate)
  "Return Marginalia annotation string for CANDIDATE."
  (let* ((entry (org-tasktree-ui-minibuffer--marginalia-entry candidate))
         (node (plist-get entry :node))
         (todo (and node (org-tasktree-model-node-todo-keyword node)))
         (tags (and node (org-tasktree-model-node-tags node))))
    (if (and (string-empty-p (or todo "")) (string-empty-p (or tags "")))
        ""
      (let* ((todo-width (org-tasktree-ui-minibuffer--marginalia-column-width
                          :todo))
             (todo-text (org-tasktree-ui-minibuffer--marginalia-pad
                         todo
                         todo-width))
             (tags-text (or tags ""))
             (sep (if (boundp 'marginalia-separator) marginalia-separator "  "))
             (align (propertize " " 'marginalia--align t)))
        (concat align
                sep
                todo-text
                (if (string-empty-p tags-text) "" (concat sep tags-text)))))))

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
        (org-tasktree-ui-minibuffer--strip-match-suffix
         (substring-no-properties cand))))))

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

(defun org-tasktree-ui-minibuffer--auto-enter-candidates (cands)
  "Return auto-enter candidate list derived from CANDS."
  (let (result)
    (dolist (cand cands (nreverse result))
      (let ((text (org-tasktree-ui-minibuffer--strip-match-suffix
                   (substring-no-properties cand))))
        (when (and (string-suffix-p "/" text)
                   (memq (org-tasktree-ui-minibuffer--candidate-type cand)
                         '(domain project phase group task)))
          (push text result))))))

(defun org-tasktree-ui-minibuffer--install-minibuffer-keymap (validate-fn)
  "Install org-tasktree keymap for the active minibuffer.
When VALIDATE-FN is non-nil, bind RET to the custom accept handler."
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
    (use-local-map map)))

(defun org-tasktree-ui-minibuffer--setup-minibuffer
    (allow-backspace-up allow-auto-enter validate-fn cands)
  "Setup minibuffer state for org-tasktree completions.
ALLOW-BACKSPACE-UP and ALLOW-AUTO-ENTER control behavior, while
VALIDATE-FN is the optional validation function and CANDS are candidates."
  (setq-local org-tasktree-ui-minibuffer--minibuffer-backspace-up-enabled
              (and allow-backspace-up t))
  (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-enabled
              (and allow-auto-enter t))
  (setq-local org-tasktree-ui-minibuffer--minibuffer-original-local-map
              (current-local-map))
  (setq-local org-tasktree-ui-minibuffer--minibuffer-validate-fn
              validate-fn)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-candidates
              (when allow-auto-enter
                (org-tasktree-ui-minibuffer--auto-enter-candidates cands)))
  (when org-tasktree-ui-minibuffer--minibuffer-auto-enter-enabled
    (add-hook 'post-command-hook
              #'org-tasktree-ui-minibuffer--minibuffer-maybe-auto-enter
              nil
              t))
  (add-hook 'minibuffer-exit-hook
            #'org-tasktree-ui-minibuffer--minibuffer-cleanup
            nil
            t)
  (org-tasktree-ui-minibuffer--install-minibuffer-keymap validate-fn))

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
                (auto allow-auto-enter)
                (candidates cands))
            (run-at-time
             0
             nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (org-tasktree-ui-minibuffer--setup-minibuffer
                    allow auto validate-fn candidates)))))))
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
  (let* ((raw (org-tasktree-ui-minibuffer--strip-match-suffix (or input "")))
         (trimmed (string-trim raw)))
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

(defun org-tasktree-ui-minibuffer--path-entry (node id-map)
  "Return path entry plist for NODE using ID-MAP."
  (let* ((titles (org-tasktree-ui-minibuffer--titles-path node id-map))
         (path (org-tasktree-ui-minibuffer--format-path-slash titles)))
    (list :path path :node node :titles titles)))

(defun org-tasktree-ui-minibuffer--collect-path-entries (nodes)
  "Return (ENTRIES . PATH-TABLE) built from NODES."
  (let* ((id-map (org-tasktree-ui-minibuffer--id-map nodes))
         (entries nil)
         (path-table (make-hash-table :test 'equal)))
    (dolist (node nodes)
      (let ((entry (org-tasktree-ui-minibuffer--path-entry node id-map)))
        (push entry entries)
        (puthash (plist-get entry :path) entry path-table)))
    (setq entries
          (sort entries
                (lambda (a b)
                  (string< (plist-get a :path)
                           (plist-get b :path)))))
    (cons entries path-table)))

(defun org-tasktree-ui-minibuffer--column-widths (entries)
  "Return plist of column widths for ENTRIES."
  (let ((todo-width 0)
        (tags-width 0))
    (dolist (entry entries)
      (let* ((node (plist-get entry :node))
             (todo (org-tasktree-model-node-todo-keyword node))
             (tags (org-tasktree-model-node-tags node)))
        (setq todo-width (max todo-width (string-width (or todo ""))))
        (setq tags-width (max tags-width (string-width (or tags ""))))))
    (list :todo todo-width :tags tags-width)))

(defun org-tasktree-ui-minibuffer--display-candidates (entries)
  "Return sorted display candidates for ENTRIES."
  (sort
   (mapcar
    (lambda (entry)
      (let* ((node (plist-get entry :node))
             (todo (org-tasktree-model-node-todo-keyword node))
             (tags (org-tasktree-model-node-tags node))
             (type (org-tasktree-ui-minibuffer--type-from-tags
                    (org-tasktree-model-node-tags node))))
        (org-tasktree-ui-minibuffer--make-path-candidate
         (plist-get entry :path)
         type
         todo
         tags)))
    entries)
   (lambda (a b)
     (string<
      (org-tasktree-ui-minibuffer--strip-match-suffix
       (substring-no-properties a))
      (org-tasktree-ui-minibuffer--strip-match-suffix
       (substring-no-properties b))))))

(defun org-tasktree-ui-minibuffer--validate-node-path (input path-table)
  "Validate INPUT path against PATH-TABLE."
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
          (user-error "Parent path not found: %s" parent-path))))))

(defun org-tasktree-ui-minibuffer--read-node-choice
    (display-cands path-table widths)
  "Return user choice from DISPLAY-CANDS using PATH-TABLE and WIDTHS."
  (let ((completion-extra-properties
         '(:display-sort-function identity
           :cycle-sort-function identity
           :category org-tasktree-node
           :annotation-function org-tasktree-ui-minibuffer--annotation-raw)))
    (let ((org-tasktree-ui-minibuffer--marginalia-path-table path-table)
          (org-tasktree-ui-minibuffer--marginalia-column-widths widths))
      (org-tasktree-ui-minibuffer--completing-read
       "find node: "
       display-cands
       nil
       nil
       nil
       (lambda (input)
         (org-tasktree-ui-minibuffer--validate-node-path input path-table))))))

(defun org-tasktree-ui-minibuffer-read-node ()
  "Prompt for node path via `completing-read'.
Return plist describing an existing or new node selection."
  (org-tasktree-ui-minibuffer--maybe-register-marginalia)
  (let* ((nodes (org-tasktree-query-open-tree))
         (entry-data (org-tasktree-ui-minibuffer--collect-path-entries nodes))
         (entries (car entry-data))
         (path-table (cdr entry-data))
         (widths (org-tasktree-ui-minibuffer--column-widths entries))
         (display-cands (org-tasktree-ui-minibuffer--display-candidates entries))
         (choice (org-tasktree-ui-minibuffer--read-node-choice
                  display-cands
                  path-table
                  widths))
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
                :path path)))))

(provide 'org-tasktree-ui-minibuffer)
;;; org-tasktree-ui-minibuffer.el ends here
