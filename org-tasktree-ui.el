;;; org-tasktree-ui.el --- UI helpers for org-tasktree -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/marmia/org-tasktree
;; Version: 0.1.0

;;; Commentary:
;;
;; User-facing helpers for hierarchical `completing-read' flows
;; used by the find-* commands.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'calendar)
(require 'widget)
(require 'wid-edit)
(require 'org-tasktree-model)
(require 'org-tasktree-query)
(require 'org-tasktree-db)

(defconst org-tasktree-ui--path-sep " > "
  "Separator for hierarchical paths in `completing-read' candidates.")

(defcustom org-tasktree-ui-completion-color-task "white"
  "Foreground color for task candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-completion-color-project "DeepSkyBlue"
  "Foreground color for project candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-completion-color-phase "MediumSeaGreen"
  "Foreground color for phase candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defcustom org-tasktree-ui-completion-color-group "DarkOrange"
  "Foreground color for group candidates in minibuffer completion."
  :type 'color
  :group 'org-tasktree)

(defun org-tasktree-ui--completion-color (type)
  "Return configured completion color for TYPE symbol."
  (pcase type
    ('task org-tasktree-ui-completion-color-task)
    ('project org-tasktree-ui-completion-color-project)
    ('phase org-tasktree-ui-completion-color-phase)
    ('group org-tasktree-ui-completion-color-group)
    (_ nil)))

(defun org-tasktree-ui--make-completion-candidate (title type)
  "Return a propertized completion candidate for TITLE and TYPE.

TYPE is one of the symbols `project', `phase', `group', or `task'."
  (let* ((suffix (pcase type ((or 'project 'phase 'group) "/") (_ "")))
         (display (concat title suffix))
         (color (org-tasktree-ui--completion-color type))
         (face (and (stringp color) (not (string-empty-p color))
                    `(:foreground ,color))))
    (propertize display
                'face face
                'org-tasktree-ui--raw title
                'org-tasktree-ui--candidate-type type)))

(defun org-tasktree-ui--candidate-raw (candidate)
  "Return the raw title string for completion CANDIDATE."
  (or (get-text-property 0 'org-tasktree-ui--raw candidate)
      (if (string-suffix-p "/" candidate)
          (string-remove-suffix "/" candidate)
        candidate)))

(defun org-tasktree-ui--candidate-type (candidate)
  "Return the candidate type symbol for completion CANDIDATE, or nil."
  (get-text-property 0 'org-tasktree-ui--candidate-type candidate))

(defun org-tasktree-ui--prompt-path (prefix)
  "Return minibuffer prompt string using PREFIX path.

PREFIX is either nil or a list of titles."
  (if (and (listp prefix) prefix)
      (concat (string-join prefix org-tasktree-ui--path-sep)
              org-tasktree-ui--path-sep)
    ""))

(defun org-tasktree-ui--node-type= (node type)
  "Return non-nil when NODE is of TYPE string."
  (equal (org-tasktree-model-node-node-type node) type))

(defun org-tasktree-ui--nodes-of-type (nodes type)
  "Return NODES filtered by TYPE string."
  (let (result)
    (dolist (node nodes (nreverse result))
      (when (org-tasktree-ui--node-type= node type)
        (push node result)))))

(defun org-tasktree-ui--titles (nodes)
  "Return list of titles from NODES."
  (mapcar #'org-tasktree-model-node-title nodes))

(defun org-tasktree-ui--sorted-strings (strings)
  "Return a sorted copy of STRINGS."
  (sort (copy-sequence strings) #'string<))

(defun org-tasktree-ui--sorted-titles (nodes)
  "Return sorted list of titles from NODES."
  (org-tasktree-ui--sorted-strings (org-tasktree-ui--titles nodes)))

(defun org-tasktree-ui--node-by-title (nodes type title)
  "Return first node in NODES matching TYPE and TITLE, or nil."
  (catch 'found
    (dolist (node nodes nil)
      (when (and (org-tasktree-ui--node-type= node type)
                 (equal (org-tasktree-model-node-title node) title))
        (throw 'found node)))))

(defun org-tasktree-ui--read-required (prompt cands empty-message &optional allow-auto-enter)
  "Read from CANDS with PROMPT.  Signal `user-error' when CANDS is empty."
  (unless cands
    (user-error "%s" empty-message))
  (org-tasktree-ui--candidate-raw
   (org-tasktree-ui--completing-read prompt cands t nil allow-auto-enter)))

(defvar-local org-tasktree-ui--minibuffer-backspace-original nil
  "Original Backspace command in the minibuffer.")

(defvar-local org-tasktree-ui--minibuffer-backspace-up-enabled nil
  "Non-nil means Backspace goes up when minibuffer is empty.")

(defvar-local org-tasktree-ui--minibuffer-auto-enter-enabled nil
  "Non-nil means a fully typed directory candidate auto-confirms.")

(defvar-local org-tasktree-ui--minibuffer-auto-enter-candidates nil
  "List of directory candidate strings eligible for minibuffer auto-confirm.")

(defvar-local org-tasktree-ui--minibuffer-original-local-map nil
  "Original minibuffer local map before org-tasktree keymap wrapping.")

(defun org-tasktree-ui--minibuffer-backspace ()
  "Handle Backspace in org-tasktree minibuffer."
  (interactive)
  (if (and org-tasktree-ui--minibuffer-backspace-up-enabled
           (string-empty-p (minibuffer-contents-no-properties)))
      (throw 'org-tasktree-ui--minibuffer-up :up)
    (call-interactively
     (or org-tasktree-ui--minibuffer-backspace-original
         #'delete-backward-char))))

(defun org-tasktree-ui--minibuffer-cleanup ()
  "Cleanup hooks and buffer-local state for org-tasktree minibuffer sessions."
  (remove-hook 'post-command-hook #'org-tasktree-ui--minibuffer-maybe-auto-enter t)
  (remove-hook 'minibuffer-exit-hook #'org-tasktree-ui--minibuffer-cleanup t)
  (setq-local org-tasktree-ui--minibuffer-auto-enter-enabled nil)
  (setq-local org-tasktree-ui--minibuffer-auto-enter-candidates nil)
  (setq-local org-tasktree-ui--minibuffer-backspace-up-enabled nil)
  (setq-local org-tasktree-ui--minibuffer-backspace-original nil)
  (when (keymapp org-tasktree-ui--minibuffer-original-local-map)
    (use-local-map org-tasktree-ui--minibuffer-original-local-map))
  (setq-local org-tasktree-ui--minibuffer-original-local-map nil))

(defun org-tasktree-ui--minibuffer-maybe-auto-enter ()
  "Auto-confirm when minibuffer input matches a directory candidate exactly."
  (when (and org-tasktree-ui--minibuffer-auto-enter-enabled
             (minibufferp)
             (consp org-tasktree-ui--minibuffer-auto-enter-candidates))
    (let ((input (minibuffer-contents-no-properties))
          (buf (current-buffer)))
      (when (member input org-tasktree-ui--minibuffer-auto-enter-candidates)
        (run-at-time
         0
         nil
         (lambda ()
           (when (and (buffer-live-p buf) (minibufferp (buffer-name buf)))
             (with-current-buffer buf
               (exit-minibuffer)))))))))

(defun org-tasktree-ui--completing-read
    (prompt cands &optional require-match allow-backspace-up allow-auto-enter)
  "Read a completion from CANDS with PROMPT.

When ALLOW-BACKSPACE-UP is non-nil, pressing Backspace in an empty minibuffer
returns the symbol `:up'.

When ALLOW-AUTO-ENTER is non-nil, typing an exact directory candidate (ending
with \"/\") auto-confirms and proceeds without requiring RET."
  (catch 'org-tasktree-ui--minibuffer-up
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
                   (setq-local org-tasktree-ui--minibuffer-backspace-up-enabled
                               (and allow t))
                   (setq-local org-tasktree-ui--minibuffer-auto-enter-enabled
                               (and auto t))
                   (setq-local org-tasktree-ui--minibuffer-original-local-map
                               (current-local-map))
                   (setq-local org-tasktree-ui--minibuffer-auto-enter-candidates
                               (when auto
                                 (let (result)
                                   (dolist (cand cands (nreverse result))
                                     (let ((s (substring-no-properties cand)))
                                       (when (string-suffix-p "/" s)
                                         (push s result)))))))
                   (when org-tasktree-ui--minibuffer-auto-enter-enabled
                     (add-hook 'post-command-hook
                               #'org-tasktree-ui--minibuffer-maybe-auto-enter
                               nil
                               t))
                   (add-hook 'minibuffer-exit-hook
                             #'org-tasktree-ui--minibuffer-cleanup
                             nil
                             t)
                   (setq-local org-tasktree-ui--minibuffer-backspace-original
                               (or (key-binding (kbd "DEL") t)
                                   (key-binding (kbd "<backspace>") t)
                                   #'delete-backward-char))
                   (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map (current-local-map))
                     (define-key map (kbd "DEL") #'org-tasktree-ui--minibuffer-backspace)
                     (define-key map (kbd "<backspace>") #'org-tasktree-ui--minibuffer-backspace)
                     (use-local-map map))))))))
      (completing-read prompt cands nil require-match))))

(defun org-tasktree-ui--id-map (nodes)
  "Return hash map of id->node from NODES."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (node nodes table)
      (when (org-tasktree-model-node-id node)
        (puthash (org-tasktree-model-node-id node) node table)))))

(defun org-tasktree-ui--titles-path (node id-map)
  "Return list of titles from root to NODE using ID-MAP."
  (let ((titles '())
        (current node))
    (while current
      (push (org-tasktree-model-node-title current) titles)
      (setq current (gethash
                     (org-tasktree-model-node-parent-id current)
                     id-map)))
    titles))

(defun org-tasktree-ui--format-path (titles)
  "Join TITLES into a display path string."
  (string-join titles org-tasktree-ui--path-sep))

(defun org-tasktree-ui--collect (pred)
  "Collect nodes satisfying PRED with display strings and metadata.
Returns list of (DISPLAY . PLIST)."
  (let* ((nodes (org-tasktree-query-open-tree))
         (id-map (org-tasktree-ui--id-map nodes))
         (result '()))
    (dolist (node nodes)
      (when (funcall pred node)
        (let* ((titles (org-tasktree-ui--titles-path node id-map))
               (display (org-tasktree-ui--format-path titles)))
          (push (cons display
                      (list :node node
                            :titles titles))
                result))))
    (nreverse result)))

(defun org-tasktree-ui-read-project ()
  "Prompt for project via `completing-read'.
Returns plist: (:project-title STRING :project-id ID-or-nil)."
  (let* ((cands (org-tasktree-ui--collect
                 (lambda (n)
                   (equal (org-tasktree-model-node-node-type n)
                          "project"))))
         (display-cands
          (mapcar (lambda (pair)
                    (org-tasktree-ui--make-completion-candidate
                     (car pair) 'project))
                  cands))
         (choice (org-tasktree-ui--completing-read
                  "find project: "
                  display-cands
                  nil
                  nil))
         (raw (string-trim (org-tasktree-ui--candidate-raw choice))))
    (let* ((found (assoc raw cands))
           (node (plist-get (cdr found) :node)))
      (if node
          (list :project-title (org-tasktree-model-node-title node)
                :project-id (org-tasktree-model-node-id node))
        (list :project-title raw :project-id nil)))))

(defun org-tasktree-ui-read-phase ()
  "Prompt for a phase by navigating project -> phase.
Return plist with titles and ids when existing; missing ids mean new."
  (let* ((nodes (org-tasktree-query-open-tree))
         (projects (org-tasktree-ui--nodes-of-type nodes "project"))
         (project-cands
          (mapcar (lambda (title)
                    (org-tasktree-ui--make-completion-candidate title 'project))
                  (org-tasktree-ui--sorted-titles projects))))
    (cl-block done
      (while t
        (let* ((project-title
                (org-tasktree-ui--read-required
                 "find node: "
                 project-cands
                 "No projects found.  Create a project first."
                 t))
               (project-node
                (or (org-tasktree-ui--node-by-title nodes "project" project-title)
                    (user-error "Project not found: %s" project-title)))
               (project-id (org-tasktree-model-node-id project-node))
               (phases
                (let (result)
                  (dolist (node nodes (nreverse result))
                    (when (and (org-tasktree-ui--node-type= node "phase")
                               (equal (org-tasktree-model-node-project-id node)
                                      project-id))
                      (push node result)))))
               (phase-cands
                (mapcar (lambda (title)
                          (org-tasktree-ui--make-completion-candidate title 'phase))
                        (org-tasktree-ui--sorted-titles phases)))
               (phase-input
                (org-tasktree-ui--completing-read
                 (concat "find node: "
                         (org-tasktree-ui--prompt-path (list project-title)))
                 phase-cands
                 nil
                 t)))
          (unless (eq phase-input :up)
            (let ((phase-title
                   (string-trim (org-tasktree-ui--candidate-raw phase-input))))
              (when (string-empty-p phase-title)
                (user-error "Phase title is required"))
              (let* ((phase-node
                      (org-tasktree-ui--node-by-title phases "phase" phase-title))
                     (phase-id (and phase-node (org-tasktree-model-node-id phase-node))))
                (cl-return-from done
                  (list :project-title project-title
                        :project-id project-id
                        :phase-title phase-title
                        :phase-id phase-id))))))))))

(defun org-tasktree-ui-read-task ()
  "Prompt for a task by navigating project -> phase? -> group? -> task.

Project must exist.  Phase and group must exist when selected.

If the phase input does not match an existing phase title, treat it as a task
title under the selected project.

If the group input does not match an existing group title, treat it as a task
title under the selected phase."
  (let* ((nodes (org-tasktree-query-open-tree))
         (projects (org-tasktree-ui--nodes-of-type nodes "project"))
         (project-cands
          (mapcar (lambda (title)
                    (org-tasktree-ui--make-completion-candidate title 'project))
                  (org-tasktree-ui--sorted-titles projects)))
         (state :project)
         (result nil)
         project-title
         project-id
         phase-title
         phase-id
         group-title
         group-id)
    (cl-labels
        ((collect-nodes (type pred)
           (let (acc)
             (dolist (node nodes (nreverse acc))
               (when (and (org-tasktree-ui--node-type= node type)
                          (funcall pred node))
                 (push node acc)))))
         (collect-phases (project-id-1)
           (collect-nodes
            "phase"
            (lambda (node)
              (equal (org-tasktree-model-node-project-id node) project-id-1))))
         (collect-groups (project-id-1 phase-id-1)
           (collect-nodes
            "group"
            (lambda (node)
              (and (equal (org-tasktree-model-node-project-id node) project-id-1)
                   (equal (org-tasktree-model-node-phase-id node) phase-id-1)))))
         (collect-tasks (project-id-1 phase-id-1 parent-id-1)
           (collect-nodes
            "task"
            (lambda (node)
              (and (equal (org-tasktree-model-node-project-id node) project-id-1)
                   (equal (org-tasktree-model-node-phase-id node) phase-id-1)
                   (equal (org-tasktree-model-node-parent-id node) parent-id-1)))))
         (read-task-title (titles tasks)
           (let ((input
                  (org-tasktree-ui--completing-read
                   (concat "find node: " (org-tasktree-ui--prompt-path titles))
                   (mapcar (lambda (title)
                             (org-tasktree-ui--make-completion-candidate title 'task))
                           (org-tasktree-ui--sorted-titles tasks))
                   nil
                   t)))
             (if (eq input :up)
                 :up
               (string-trim (org-tasktree-ui--candidate-raw input))))))
      (while (null result)
        (pcase state
          (:project
           (setq project-title
                 (org-tasktree-ui--read-required
                  "find node: "
                  project-cands
                  "No projects found.  Create a project first."
                  t))
           (let ((project-node
                  (or (org-tasktree-ui--node-by-title nodes "project" project-title)
                      (user-error "Project not found: %s" project-title))))
             (setq project-id (org-tasktree-model-node-id project-node)))
           (setq phase-title nil
                 phase-id nil
                 group-title nil
                 group-id nil
                 state :phase-or-task))

          (:phase-or-task
           (let* ((phases (collect-phases project-id))
                  (phase-titles (org-tasktree-ui--sorted-titles phases))
                  (project-tasks (collect-tasks project-id nil project-id))
                  (cands
                   (org-tasktree-ui--sorted-strings
                    (append
                     (mapcar (lambda (title)
                               (org-tasktree-ui--make-completion-candidate title 'phase))
                             phase-titles)
                     (mapcar (lambda (title)
                               (org-tasktree-ui--make-completion-candidate title 'task))
                             (org-tasktree-ui--sorted-titles project-tasks)))))
                  (input
                   (org-tasktree-ui--completing-read
                    (concat "find node: "
                            (org-tasktree-ui--prompt-path (list project-title)))
                    cands
                    nil
                    t
                    t)))
             (if (eq input :up)
                 (setq state :project)
               (let* ((raw (string-trim (org-tasktree-ui--candidate-raw input)))
                      (input-type (org-tasktree-ui--candidate-type input)))
                 (cond
                  ((string-empty-p raw)
                   (setq state :project-task))
                  ((or (eq input-type 'phase) (member raw phase-titles))
                   (setq phase-title raw
                         phase-id (org-tasktree-model-node-id
                                   (or (org-tasktree-ui--node-by-title phases "phase" raw)
                                       (user-error "Phase not found: %s" raw)))
                         group-title nil
                         group-id nil
                         state :group-or-task))
                  (t
                   (when (string-empty-p raw)
                     (user-error "Task title is required"))
                   (let* ((task-node (org-tasktree-ui--node-by-title project-tasks "task" raw))
                          (task-id (and task-node (org-tasktree-model-node-id task-node))))
                     (setq result
                           (list :project-title project-title
                                 :project-id project-id
                                 :phase-title nil
                                 :phase-id nil
                                 :group-title nil
                                 :parent-id project-id
                                 :task-title raw
                                 :task-id task-id)))))))))

          (:project-task
           (let* ((tasks (collect-tasks project-id nil project-id))
                  (title (read-task-title (list project-title) tasks)))
             (if (eq title :up)
                 (setq state :phase-or-task)
               (when (string-empty-p title)
                 (user-error "Task title is required"))
               (let* ((task-node (org-tasktree-ui--node-by-title tasks "task" title))
                      (task-id (and task-node (org-tasktree-model-node-id task-node))))
                 (setq result
                       (list :project-title project-title
                             :project-id project-id
                             :phase-title nil
                             :phase-id nil
                             :group-title nil
                             :parent-id project-id
                             :task-title title
                             :task-id task-id))))))

          (:group-or-task
           (let* ((groups (collect-groups project-id phase-id))
                  (group-titles (org-tasktree-ui--sorted-titles groups))
                  (phase-tasks (collect-tasks project-id phase-id phase-id))
                  (cands
                   (org-tasktree-ui--sorted-strings
                    (append
                     (mapcar (lambda (title)
                               (org-tasktree-ui--make-completion-candidate title 'group))
                             group-titles)
                     (mapcar (lambda (title)
                               (org-tasktree-ui--make-completion-candidate title 'task))
                             (org-tasktree-ui--sorted-titles phase-tasks)))))
                  (input
                   (org-tasktree-ui--completing-read
                    (concat "find task group: "
                            (org-tasktree-ui--prompt-path
                             (list project-title phase-title)))
                    cands
                    nil
                    t
                    t)))
             (if (eq input :up)
                 (setq group-title nil
                       group-id nil
                       state :phase-or-task)
               (let* ((raw (string-trim (org-tasktree-ui--candidate-raw input)))
                      (input-type (org-tasktree-ui--candidate-type input)))
                 (cond
                  ((eq input-type 'task)
                   (when (string-empty-p raw)
                     (user-error "Task title is required"))
                   (let* ((task-node (org-tasktree-ui--node-by-title phase-tasks "task" raw))
                          (task-id (and task-node (org-tasktree-model-node-id task-node))))
                     (setq result
                           (list :project-title project-title
                                 :project-id project-id
                                 :phase-title phase-title
                                 :phase-id phase-id
                                 :group-title nil
                                 :parent-id phase-id
                                 :task-title raw
                                 :task-id task-id))))
                  ((string-empty-p raw)
                   (setq state :phase-task))
                  ((or (eq input-type 'group) (member raw group-titles))
                   (setq group-title raw
                         group-id (org-tasktree-model-node-id
                                   (or (org-tasktree-ui--node-by-title groups "group" raw)
                                       (user-error "Group not found: %s" raw)))
                         state :group-task))
                  (t
                   (when (string-empty-p raw)
                     (user-error "Task title is required"))
                   (let* ((task-node (org-tasktree-ui--node-by-title phase-tasks "task" raw))
                          (task-id (and task-node (org-tasktree-model-node-id task-node))))
                     (setq result
                           (list :project-title project-title
                                 :project-id project-id
                                 :phase-title phase-title
                                 :phase-id phase-id
                                 :group-title nil
                                 :parent-id phase-id
                                 :task-title raw
                                 :task-id task-id)))))))))

          (:phase-task
           (let* ((tasks (collect-tasks project-id phase-id phase-id))
                  (title (read-task-title (list project-title phase-title) tasks)))
             (if (eq title :up)
                 (setq state :group-or-task)
               (when (string-empty-p title)
                 (user-error "Task title is required"))
               (let* ((task-node (org-tasktree-ui--node-by-title tasks "task" title))
                      (task-id (and task-node (org-tasktree-model-node-id task-node))))
                 (setq result
                       (list :project-title project-title
                             :project-id project-id
                             :phase-title phase-title
                             :phase-id phase-id
                             :group-title nil
                             :parent-id phase-id
                             :task-title title
                             :task-id task-id))))))

          (:group-task
           (let* ((tasks (collect-tasks project-id phase-id group-id))
                  (title (read-task-title (list project-title phase-title group-title) tasks)))
             (if (eq title :up)
                 (setq state :group-or-task)
               (when (string-empty-p title)
                 (user-error "Task title is required"))
               (let* ((task-node (org-tasktree-ui--node-by-title tasks "task" title))
                      (task-id (and task-node (org-tasktree-model-node-id task-node))))
                 (setq result
                       (list :project-title project-title
                             :project-id project-id
                             :phase-title phase-title
                             :phase-id phase-id
                             :group-title group-title
                             :parent-id group-id
                             :task-title title
                             :task-id task-id))))))

          (_
           (error "Unknown state: %S" state))))
      result)))
(defvar-local org-tasktree-ui--edit-metadata nil
  "Metadata plist for current edit buffer.")

(defvar org-tasktree-ui-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-edit-cancel)
    (define-key map (kbd "TAB") #'org-tasktree-ui-edit-next-field)
    (define-key map (kbd "<backtab>") #'org-tasktree-ui-edit-previous-field)
    (define-key map (kbd "S-<tab>") #'org-tasktree-ui-edit-previous-field)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-edit-set-schedule)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-edit-set-deadline)
    map)
  "Keymap for `org-tasktree-ui-edit-mode'.")

(defvar org-tasktree-ui--widget-field-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-field-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-widget-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-widget-edit-cancel)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-widget-edit-set-scheduled)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-widget-edit-set-deadline)
    map)
  "Keymap used inside widget editable fields.")

(define-derived-mode org-tasktree-ui-edit-mode text-mode
  "org-tasktree-edit"
  "Edit buffer for org-tasktree entities."
  (setq-local buffer-read-only nil)
  (setq-local truncate-lines nil))

(defun org-tasktree-ui-edit-cancel ()
  "Cancel current org-tasktree edit buffer."
  (interactive)
  (org-tasktree-ui--quit-edit-buffer)
  (message "org-tasktree: edit cancelled"))

(defun org-tasktree-ui--quit-edit-buffer ()
  "Close current edit buffer and its window."
  (let* ((buf (current-buffer))
         (win (get-buffer-window buf t)))
    (when (and win (eq (window-buffer win) buf))
      (quit-window 'kill win))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun org-tasktree-ui--field-regexp (field)
  "Return regexp capturing value part for FIELD line."
  (format "^\\(%s[[:space:]]*:[[:space:]]\\)\\([^(\n]*?\\)\\( (shortcut-key: [^)]+)\\)?$"
          (regexp-quote field)))

(defun org-tasktree-ui--form-start ()
  "Return buffer position just after the second '---' separator."
  (or (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^---$" nil t)
          (when (re-search-forward "^---$" nil t)
            (forward-line 1)
            (point))))
      (point-min)))

(defun org-tasktree-ui--value-or-placeholder (value)
  "Return VALUE or two-space placeholder when VALUE is empty."
  (if (and (stringp value) (> (length value) 0))
      value
    "  "))

(defun org-tasktree-ui--set-field (field value)
  "Replace FIELD line's value with VALUE, preserving trailing hints."
  (save-excursion
    (goto-char (org-tasktree-ui--form-start))
    (let ((regexp (org-tasktree-ui--field-regexp field)))
      (when (re-search-forward regexp nil t)
        (replace-match
         (concat (match-string 1)
                 (org-tasktree-ui--value-or-placeholder value)
                 (or (match-string 3) "")) t t nil 0)
        (org-tasktree-ui--mark-fields (org-tasktree-ui--fields-order))))))

(defun org-tasktree-ui--mark-fields (fields)
  "Add `org-tasktree-field' property over editable VALUES for FIELDS."
  (save-excursion
    (dolist (field fields)
      (let ((start-pos (org-tasktree-ui--form-start)))
        (when start-pos
          (goto-char start-pos)
          (let ((regexp (org-tasktree-ui--field-regexp field)))
            (when (re-search-forward regexp nil t)
              (let ((start (match-beginning 2))
                    (end (match-end 2)))
                ;; Only add property when the match is valid
                (when (and start end (< start end))
                  (add-text-properties start end `(org-tasktree-field ,field)))))))))))

(defun org-tasktree-ui--goto-field (field)
  "Move point to beginning of FIELD value."
  (let ((pos (text-property-any (point-min) (point-max)
                                'org-tasktree-field field)))
    (when pos (goto-char pos))))

(defun org-tasktree-ui--current-field ()
  "Return current field name at point or nil."
  (get-text-property (point) 'org-tasktree-field))

(defun org-tasktree-ui--fields-order ()
  "Return ordered field list for current edit buffer."
  (plist-get org-tasktree-ui--edit-metadata :fields))

(defun org-tasktree-ui--cycle-field (direction)
  "Move to next/previous field according to DIRECTION (1 or -1)."
  (let* ((fields (org-tasktree-ui--fields-order))
         (current (or (org-tasktree-ui--current-field) (car fields)))
         (idx (or (cl-position current fields :test #'string=) 0))
         (len (length fields))
         (next-idx (mod (+ idx direction) len))
         (target (nth next-idx fields)))
    (org-tasktree-ui--goto-field target)))

(defun org-tasktree-ui-edit-next-field ()
  "Jump to next editable field."
  (interactive)
  (org-tasktree-ui--cycle-field 1))

(defun org-tasktree-ui-edit-previous-field ()
  "Jump to previous editable field."
  (interactive)
  (org-tasktree-ui--cycle-field -1))

(defun org-tasktree-ui--read-date ()
  "Read date via `org-read-date' and normalize to YYYY-MM-DD."
  (org-read-date nil nil nil nil nil))

(defun org-tasktree-ui--set-date-field (field)
  "Set FIELD to date chosen by `org-read-date', default today."
  (let ((date (org-tasktree-ui--read-date)))
    (org-tasktree-ui--set-field field date)
    (org-tasktree-ui--goto-field field)))

(defun org-tasktree-ui-edit-set-schedule ()
  "Prompt date and set scheduled field."
  (interactive)
  (org-tasktree-ui--set-date-field "scheduled"))

(defun org-tasktree-ui-edit-set-deadline ()
  "Prompt date and set deadline field."
  (interactive)
  (org-tasktree-ui--set-date-field "deadline"))

(defun org-tasktree-ui--parse-form-buffer ()
  "Parse current edit buffer into plist.
Assumes key: value lines after the second '---' separator."
  (let (plist)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^---$" nil t)
        (re-search-forward "^---$" nil t))
      (dolist (line (split-string (buffer-substring-no-properties
                                   (point) (point-max)) "\n" t))
        (when (string-match
               "^[[:space:]]*\\([a-zA-Z0-9_]+\\)[[:space:]]*:[[:space:]]*\\(.*\\)$"
               line)
          (let* ((key (match-string 1 line))
                 (val (string-trim (match-string 2 line))))
            (setq plist (plist-put plist (intern (concat ":" key)) val))))))
    plist))

(defun org-tasktree-ui--field (plist key)
  "Return string value for KEY in PLIST or nil when empty."
  (let ((v (plist-get plist key)))
    (unless (and (stringp v) (string-empty-p v))
      v)))

(defun org-tasktree-ui--submit-project (data)
  "Submit project DATA plist to DB and return node."
  (let* ((title (org-tasktree-ui--field data :project_name))
         (uid (or (plist-get org-tasktree-ui--edit-metadata :uid)
                  (org-tasktree-ui--field data :uid)
                  (org-tasktree-db-generate-uid)))
         (priority (org-tasktree-ui--field data :priority))
         (scheduled-raw (org-tasktree-ui--field data :scheduled))
         (deadline-raw (org-tasktree-ui--field data :deadline))
         (tags (org-tasktree-ui--field data :tags)))
    (condition-case err
        (progn
          (unless (and title (not (string-empty-p title)))
            (user-error "Project name is required"))
          (let* ((scheduled (org-tasktree-ui--normalize-date scheduled-raw "scheduled"))
                 (deadline (org-tasktree-ui--normalize-date deadline-raw "deadline"))
                 (node (org-tasktree-model-node-create
                        :uid uid
                        :parent-id nil
                        :node-type "project"
                        :todo-keyword "PROJ"
                        :title title
                        :level 1
                        :priority priority
                        :scheduled scheduled
                        :deadline deadline
                        :tags tags
                        :status "OPEN"
                        :project-id nil
                        :phase-id nil)))
            (org-tasktree-db-commit-nodes (list node))
            node))
      (error
       (message (concat
                 "org-tasktree debug: submit project failed"
                 " title=%S uid=%S scheduled=%S deadline=%S plist=%S")
                title uid scheduled-raw deadline-raw data)
       (signal (car err) (cdr err))))))

(defun org-tasktree-ui-edit-accept ()
  "Accept current org-tasktree edit buffer (temporary stub)."
  (interactive)
  (let* ((data (org-tasktree-ui--parse-form-buffer))
         (meta org-tasktree-ui--edit-metadata)
         (type (plist-get meta :type))
         (uid (plist-get meta :uid)))
    (when uid
      (setq data (plist-put data :uid uid)))
    (pcase type
      ('project
       (let ((node (org-tasktree-ui--submit-project data)))
         (org-tasktree-ui--quit-edit-buffer)
         (message "org-tasktree: project saved uid=%s"
                  (org-tasktree-model-node-uid node))))
      (_
       (message "org-tasktree: Submit not implemented yet")))))

(defun org-tasktree-ui--render-form (type data)
  "Return form string for TYPE using DATA plist."
  (pcase type
    ('project
     (format (string-join
              '("---"
                "Input hints:"
                "TAB/S-TAB : move between fields (priority → scheduled → deadline → tags)"
                "priority  : A or B or C"
                "scheduled : yyyy-mm-dd (shortcut-key: C-c C-s)"
                "deadline  : yyyy-mm-dd (shortcut-key: C-c C-d)"
                "tags      : tag1, tag2, tag3"
                "---"
                "project_name : %s"
                "priority     : %s"
                "scheduled    : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :project-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :priority))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :scheduled))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :deadline))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :tags))))
    ('phase
     (format (string-join
              '("---"
                "Input hints:"
                "TAB/S-TAB : move between fields (priority → scheduled → deadline → tags)"
                "priority     : A or B or C"
                "scheduled    : yyyy-mm-dd (shortcut-key: C-c C-s)"
                "deadline     : yyyy-mm-dd (shortcut-key: C-c C-d)"
                "tags         : tag1, tag2, tag3"
                "---"
                "project_name : %s"
                "phase_name   : %s"
                "priority     : %s"
                "scheduled    : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :project-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :phase-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :priority))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :scheduled))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :deadline))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :tags))))
    ('task
     (format (string-join
              '("---"
                "Input hints:"
                "TAB/S-TAB : move between fields (priority → scheduled → deadline → tags)"
                "priority     : A or B or C"
                "scheduled    : yyyy-mm-dd (shortcut-key: C-c C-s)"
                "deadline     : yyyy-mm-dd (shortcut-key: C-c C-d)"
                "tags         : tag1, tag2, tag3"
                "---"
                "project_name : %s"
                "phase_name   : %s"
                "task_name    : %s"
                "priority     : %s"
                "scheduled    : %s"
                "deadline     : %s"
                "tags         : %s\n")
              "\n")
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :project-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :phase-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :task-title))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :priority))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :scheduled))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :deadline))
             (org-tasktree-ui--value-or-placeholder
              (plist-get data :tags))))
    (_ "")))

(defun org-tasktree-ui--open-edit-buffer (type data)
  "Create and show edit buffer for TYPE with DATA plist."
  (let* ((buf (generate-new-buffer
               (format "*org-tasktree-edit %s*" type)))
         (form (org-tasktree-ui--render-form type data))
         (fields (pcase type
                   ('project '("priority" "scheduled" "deadline" "tags"))
                   ('phase '("priority" "scheduled" "deadline" "tags"))
                   ('task '("priority" "scheduled" "deadline" "tags"))
                   (_ nil))))
    (with-current-buffer buf
      (org-tasktree-ui-edit-mode)
      (erase-buffer)
      (insert form)
      (goto-char (point-min))
      (setq org-tasktree-ui--edit-metadata
            (list :type type
                  :data data
                  :fields fields
                  :uid (plist-get data :uid)))
      (org-tasktree-ui--mark-fields fields)
      (when fields
        (org-tasktree-ui--goto-field (car fields))))
    (pop-to-buffer buf)))

(defun org-tasktree-ui-edit-project (selection)
  "Open project edit buffer using SELECTION plist."
  (let* ((pid (plist-get selection :project-id))
         (node (org-tasktree-query-get-node-by-id pid))
         data)
    (setq data
          (if node
              (list :type 'project
                    :uid (org-tasktree-model-node-uid node)
                    :title (org-tasktree-model-node-title node)
                    :priority (org-tasktree-model-node-priority node)
                    :scheduled (org-tasktree-model-node-scheduled node)
                    :deadline (org-tasktree-model-node-deadline node)
                    :tags (org-tasktree-model-node-tags node)
                    :path-titles nil
                    :project-title (org-tasktree-model-node-title node)
                    :project-id (org-tasktree-model-node-id node))
            (list :type 'project
                  :uid nil
                  :title (plist-get selection :project-title)
                  :priority nil
                  :scheduled nil
                  :deadline nil
                  :tags nil
                  :path-titles nil
                  :project-title (plist-get selection :project-title)
                  :project-id nil)))
    (org-tasktree-ui--open-widget-edit-buffer data)))

(defun org-tasktree-ui-edit-phase (selection)
  "Open phase edit buffer using SELECTION plist."
  (let* ((phase-id (plist-get selection :phase-id))
         (node (org-tasktree-query-get-node-by-id phase-id))
         (project-title (plist-get selection :project-title))
         (data (if node
                   (list :type 'phase
                         :uid (org-tasktree-model-node-uid node)
                         :title (org-tasktree-model-node-title node)
                         :priority (org-tasktree-model-node-priority node)
                         :scheduled (org-tasktree-model-node-scheduled node)
                         :deadline (org-tasktree-model-node-deadline node)
                         :tags (org-tasktree-model-node-tags node)
                         :path-titles (list project-title)
                         :project-title project-title
                         :project-id (org-tasktree-model-node-project-id node)
                         :phase-id (org-tasktree-model-node-id node))
                 (list :type 'phase
                       :uid nil
                       :title (plist-get selection :phase-title)
                       :priority nil
                       :scheduled nil
                       :deadline nil
                       :tags nil
                       :path-titles (list project-title)
                       :project-title project-title
                       :project-id (plist-get selection :project-id)
                       :phase-id nil))))
    (org-tasktree-ui--open-widget-edit-buffer data)))

(defun org-tasktree-ui-edit-task (selection)
  "Open task edit buffer using SELECTION plist."
  (let* ((task-id (plist-get selection :task-id))
         (node (org-tasktree-query-get-node-by-id task-id))
         (project-title (plist-get selection :project-title))
         (phase-title (plist-get selection :phase-title))
         (group-title (plist-get selection :group-title))
         (path-titles (delq nil (list project-title phase-title group-title)))
         (data (if node
                   (let ((parent-id (org-tasktree-model-node-parent-id node)))
                     (list :type 'task
                           :uid (org-tasktree-model-node-uid node)
                           :title (org-tasktree-model-node-title node)
                           :priority (org-tasktree-model-node-priority node)
                           :scheduled (org-tasktree-model-node-scheduled node)
                           :deadline (org-tasktree-model-node-deadline node)
                           :tags (org-tasktree-model-node-tags node)
                           :path-titles path-titles
                           :project-title project-title
                           :project-id (org-tasktree-model-node-project-id node)
                           :phase-title phase-title
                           :phase-id (org-tasktree-model-node-phase-id node)
                           :group-title group-title
                           :parent-id parent-id
                           :task-id (org-tasktree-model-node-id node)))
                 (list :type 'task
                       :uid nil
                       :title (plist-get selection :task-title)
                       :priority nil
                       :scheduled nil
                       :deadline nil
                       :tags nil
                       :path-titles path-titles
                       :project-title project-title
                       :project-id (plist-get selection :project-id)
                       :phase-title phase-title
                       :phase-id (plist-get selection :phase-id)
                       :group-title group-title
                       :parent-id (plist-get selection :parent-id)
                       :task-id nil))))
    (org-tasktree-ui--open-widget-edit-buffer data)))

(defun org-tasktree-ui--normalize-date (val field)
  "Return VAL when it matches YYYY-MM-DD, or nil if empty.
Signal `user-error' for invalid date.  FIELD is used in the error message."
  (let ((trimmed (and val (string-trim val))))
    (cond
     ((or (null trimmed) (string-empty-p trimmed)) nil)
     ((org-tasktree-model--valid-date-p trimmed) trimmed)
     (t (user-error "Invalid %s: must be YYYY-MM-DD or empty" field)))))

(defvar-local org-tasktree-ui--widget-widgets nil
  "Plist of widget objects for current edit buffer.")

(defvar org-tasktree-ui-widget-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-c C-c") #'org-tasktree-ui-widget-edit-accept)
    (define-key map (kbd "C-c C-k") #'org-tasktree-ui-widget-edit-cancel)
    (define-key map (kbd "TAB") #'widget-forward)
    (define-key map (kbd "<backtab>") #'widget-backward)
    (define-key map (kbd "S-<tab>") #'widget-backward)
    (define-key map (kbd "C-c C-s") #'org-tasktree-ui-widget-edit-set-scheduled)
    (define-key map (kbd "C-c C-d") #'org-tasktree-ui-widget-edit-set-deadline)
    map)
  "Keymap for `org-tasktree-ui-widget-edit-mode'.")

(define-derived-mode org-tasktree-ui-widget-edit-mode special-mode
  "org-tasktree-edit"
  "Edit buffer for org-tasktree entities."
  (use-local-map org-tasktree-ui-widget-edit-mode-map)
  (setq buffer-read-only nil)
  (overwrite-mode 1)
  (setq truncate-lines t))

(defun org-tasktree-ui--pos (pos)
  "Return the integer buffer position of POS.

POS may be an integer or a marker."
  (cond
   ((integerp pos) pos)
   ((markerp pos) (marker-position pos))
   (t nil)))

(defun org-tasktree-ui--widget-lock-buffer ()
  "Make non-widget text read-only in the current buffer."
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max)
                         '(read-only t rear-nonsticky (read-only)))
    (let ((plist org-tasktree-ui--widget-widgets))
      (while plist
        (let* ((w (cadr plist))
               (from (org-tasktree-ui--pos
                      (or (and w (widget-field-start w))
                          (and w (widget-get w :from)))))
               (to (org-tasktree-ui--pos
                    (or (and w (widget-field-end w))
                        (and w (widget-get w :to))))))
          (when (and from to (< from to))
            (remove-text-properties from to
                                    '(read-only t rear-nonsticky t))))
        (setq plist (cddr plist))))))

(defun org-tasktree-ui-widget-edit-cancel ()
  "Cancel current org-tasktree widget edit buffer."
  (interactive)
  (org-tasktree-ui--quit-edit-buffer)
  (message "org-tasktree: edit cancelled"))

(defun org-tasktree-ui--widget-get (key)
  "Return widget object for KEY."
  (plist-get org-tasktree-ui--widget-widgets key))

(defun org-tasktree-ui--widget-value (key)
  "Return current string value for KEY widget."
  (let ((w (org-tasktree-ui--widget-get key)))
    (when w
      (let ((v (widget-value w)))
        (and (stringp v) (string-trim v))))))

(defun org-tasktree-ui--validate-title (value)
  "Return normalized title string from VALUE or signal `user-error'."
  (let ((title (and value (string-trim value))))
    (unless (and title (not (string-empty-p title)))
      (user-error "Title is required"))
    (when (string-match-p "[[:cntrl:]]" title)
      (user-error "Title must not include control characters"))
    title))

(defun org-tasktree-ui--validate-priority (value)
  "Return normalized priority string from VALUE or nil."
  (let ((p (and value (string-trim value))))
    (cond
     ((or (null p) (string-empty-p p)) nil)
     ((string-match-p "\\`[[:alnum:]]\\'" p) p)
     (t (user-error "Priority must be a single alphanumeric character")))))

(defun org-tasktree-ui--validate-tags (value)
  "Return normalized tags string from VALUE or nil."
  (let ((t0 (and value (string-trim value))))
    (cond
     ((or (null t0) (string-empty-p t0)) nil)
     ((string-match-p
       "\\`\\(?::\\)?[A-Za-z0-9_-]+\\(?::[A-Za-z0-9_-]+\\)*\\(?::\\)?\\'"
       t0)
      (car (org-tasktree-model-normalize-tags t0)))
     (t (user-error
         "Tags must be like tag1:tag2 or :tag1:tag2: using [A-Za-z0-9_-]")))))

(defun org-tasktree-ui--days-in-month (year month)
  "Return number of days for YEAR and MONTH."
  (calendar-last-day-of-month month year))

(defun org-tasktree-ui--ymd-to-days (year month day)
  "Return day count for YEAR MONTH DAY at local midnight."
  (time-to-days (encode-time 0 0 0 day month year)))

(defun org-tasktree-ui--today-ymd ()
  "Return today's (YEAR MONTH DAY) list in local time."
  (let ((d (decode-time (current-time))))
    (list (nth 5 d) (nth 4 d) (nth 3 d))))

(defun org-tasktree-ui--format-ymd (year month day)
  "Return YYYY-MM-DD string for YEAR, MONTH, and DAY."
  (format "%04d-%02d-%02d" year month day))

(defun org-tasktree-ui--resolve-month-day (month day)
  "Resolve MONTH/DAY to nearest future date (including today)."
  (pcase-let* ((`(,ty ,tm ,td) (org-tasktree-ui--today-ymd))
               (today (org-tasktree-ui--ymd-to-days ty tm td))
               (max-years 30)
               (found (catch 'found
                        (dotimes (i max-years)
                          (let ((year (+ ty i)))
                            (when (<= day (org-tasktree-ui--days-in-month year month))
                              (let ((cand (org-tasktree-ui--ymd-to-days year month day)))
                                (when (>= cand today)
                                  (throw 'found (list year month day)))))))
                        nil)))
    (unless found
      (user-error "Invalid date: %02d-%02d" month day))
    found))

(defun org-tasktree-ui--resolve-day-of-month (day)
  "Resolve DAY to nearest future date (including today)."
  (pcase-let* ((`(,ty ,tm ,td) (org-tasktree-ui--today-ymd))
               (today (org-tasktree-ui--ymd-to-days ty tm td))
               (max-months 36)
               (found (catch 'found
                        (dotimes (i max-months)
                          (let* ((m (+ tm i))
                                 (year (+ ty (/ (1- m) 12)))
                                 (month (1+ (mod (1- m) 12))))
                            (when (<= day (org-tasktree-ui--days-in-month year month))
                              (let ((cand (org-tasktree-ui--ymd-to-days year month day)))
                                (when (>= cand today)
                                  (throw 'found (list year month day)))))))
                        nil)))
    (unless found
      (user-error "Invalid date: %d" day))
    found))

(defun org-tasktree-ui--parse-date-input (value field)
  "Parse VALUE for FIELD and return YYYY-MM-DD or nil.
Accepts YYYY-MM-DD, YYYY/MM/DD, MM-DD, MM/DD, and DD forms."
  (let ((s (and value (string-trim value))))
    (cond
     ((or (null s) (string-empty-p s)) nil)
     ((string-match
       "\\`\\([0-9]\\{4\\}\\)[-/]\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\'"
       s)
      (let* ((y (string-to-number (match-string 1 s)))
             (m (string-to-number (match-string 2 s)))
             (d (string-to-number (match-string 3 s))))
        (unless (and (<= 1 m 12)
                     (<= 1 d (org-tasktree-ui--days-in-month y m)))
          (user-error "Invalid %s: date is not valid" field))
        (org-tasktree-ui--format-ymd y m d)))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\'" s)
      (let* ((m (string-to-number (match-string 1 s)))
             (d (string-to-number (match-string 2 s))))
        (unless (<= 1 m 12)
          (user-error "Invalid %s: month is not valid" field))
        (pcase-let ((`(,y ,mm ,dd) (org-tasktree-ui--resolve-month-day m d)))
          (org-tasktree-ui--format-ymd y mm dd))))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)\\'" s)
      (let ((d (string-to-number (match-string 1 s))))
        (unless (<= 1 d 31)
          (user-error "Invalid %s: day is not valid" field))
        (pcase-let ((`(,y ,m ,dd) (org-tasktree-ui--resolve-day-of-month d)))
          (org-tasktree-ui--format-ymd y m dd))))
     (t (user-error "Invalid %s: date format is not supported" field)))))

(defun org-tasktree-ui--validate-schedule-deadline (scheduled deadline)
  "Validate SCHEDULED and DEADLINE ordering."
  (when (and scheduled deadline (string-lessp deadline scheduled))
    (user-error "Deadline must be >= scheduled")))

(defun org-tasktree-ui--db-select-int (sql params)
  "Return first column of first row for SQL with PARAMS, or nil."
  (org-tasktree-db--with-db db
    (let ((rows (sqlite-select db sql params)))
      (when rows
        (let ((row (car rows)))
          (if (vectorp row) (aref row 0) (car row)))))))

(defun org-tasktree-ui--db-project-id (title)
  "Return project id for TITLE or nil."
  (org-tasktree-ui--db-select-int
   (concat
    "SELECT id FROM nodes "
    "WHERE node_type='project' AND status='OPEN' AND title=? "
    "ORDER BY id ASC LIMIT 1;")
   (vector title)))

(defun org-tasktree-ui--db-phase-id (project-id title)
  "Return phase id for PROJECT-ID and TITLE or nil."
  (org-tasktree-ui--db-select-int
   (concat
    "SELECT id FROM nodes "
    "WHERE node_type='phase' AND status='OPEN' AND project_id=? AND title=? "
    "ORDER BY id ASC LIMIT 1;")
   (vector project-id title)))

(defun org-tasktree-ui--db-group-id (project-id phase-id title)
  "Return group id for PROJECT-ID PHASE-ID and TITLE or nil."
  (org-tasktree-ui--db-select-int
   (concat
    "SELECT id FROM nodes "
    "WHERE node_type='group' AND status='OPEN' "
    "AND project_id=? AND phase_id=? AND title=? "
    "ORDER BY id ASC LIMIT 1;")
   (vector project-id phase-id title)))

(defun org-tasktree-ui--submit-widget (meta)
  "Submit widget edit META to DB and return saved node."
  (let* ((type (plist-get meta :type))
         (uid (or (plist-get meta :uid) (org-tasktree-db-generate-uid)))
         (title (org-tasktree-ui--validate-title
                 (org-tasktree-ui--widget-value :title)))
         (priority (org-tasktree-ui--validate-priority
                    (org-tasktree-ui--widget-value :priority)))
         (scheduled (org-tasktree-ui--parse-date-input
                     (org-tasktree-ui--widget-value :scheduled)
                     "scheduled"))
         (deadline (org-tasktree-ui--parse-date-input
                    (org-tasktree-ui--widget-value :deadline)
                    "deadline"))
         (tags (org-tasktree-ui--validate-tags
                (org-tasktree-ui--widget-value :tags))))
    (org-tasktree-ui--validate-schedule-deadline scheduled deadline)
    (pcase type
      ('project
       (let ((node (org-tasktree-model-node-create
                    :uid uid
                    :parent-id nil
                    :node-type "project"
                    :todo-keyword "PROJ"
                    :title title
                    :level 1
                    :priority priority
                    :scheduled scheduled
                    :deadline deadline
                    :tags tags
                    :status "OPEN"
                    :project-id nil
                    :phase-id nil)))
         (org-tasktree-db-commit-nodes (list node))
         node))
      ('phase
       (let* ((project-title (plist-get meta :project-title))
              (project-id (or (plist-get meta :project-id)
                              (and project-title
                                   (org-tasktree-ui--db-project-id
                                    project-title)))))
         (unless project-id
           (user-error "Project must exist before creating a phase"))
         (let ((node (org-tasktree-model-node-create
                      :uid uid
                      :parent-id project-id
                      :node-type "phase"
                      :todo-keyword "PHASE"
                      :title title
                      :level 2
                      :priority priority
                      :scheduled scheduled
                      :deadline deadline
                      :tags tags
                      :status "OPEN"
                      :project-id project-id
                      :phase-id nil)))
           (org-tasktree-db-commit-nodes (list node))
           node)))
      ('task
       (let* ((project-title (plist-get meta :project-title))
              (project-id (or (plist-get meta :project-id)
                              (and project-title
                                   (org-tasktree-ui--db-project-id
                                    project-title))))
              (phase-id (plist-get meta :phase-id))
              (parent-id (plist-get meta :parent-id)))
         (unless project-id
           (user-error "Project must exist before creating a task"))
         (unless (numberp parent-id)
           (user-error "Task parent must exist"))
         (when (and (null phase-id) (not (equal parent-id project-id)))
           (user-error "Task parent must be project when phase is not selected"))
         (when (and (numberp phase-id) (equal parent-id project-id))
           (user-error "Task parent must not be project when phase is selected"))
         (when (numberp phase-id)
           (let ((phase-node (org-tasktree-query-get-node-by-id phase-id)))
             (unless (and phase-node
                          (org-tasktree-ui--node-type= phase-node "phase")
                          (equal (org-tasktree-model-node-project-id phase-node)
                                 project-id))
               (user-error "Phase not found: %s" phase-id))))
         (when (and (numberp phase-id) (not (equal parent-id phase-id)))
           (let ((parent-node (org-tasktree-query-get-node-by-id parent-id)))
             (unless (and parent-node
                          (org-tasktree-ui--node-type= parent-node "group")
                          (equal (org-tasktree-model-node-project-id parent-node)
                                 project-id)
                          (equal (org-tasktree-model-node-phase-id parent-node)
                                 phase-id))
               (user-error "Group not found: %s" parent-id))))
         (let* ((level
                 (cond
                  ((not (numberp phase-id)) 2)
                  ((equal parent-id phase-id) 3)
                  (t 4)))
                (node
                 (org-tasktree-model-node-create
                  :uid uid
                  :parent-id parent-id
                  :node-type "task"
                  :todo-keyword "TODO"
                  :title title
                  :level level
                  :priority priority
                  :scheduled scheduled
                  :deadline deadline
                  :tags tags
                  :status "OPEN"
                  :project-id project-id
                  :phase-id (and (numberp phase-id) phase-id))))
           (org-tasktree-db-commit-nodes (list node))
           node)))
      (_ (user-error "Unknown edit type: %S" type)))))

(defun org-tasktree-ui-widget-edit-accept ()
  "Commit current widget edit buffer to DB."
  (interactive)
  (let* ((meta org-tasktree-ui--edit-metadata)
         (node (org-tasktree-ui--submit-widget meta)))
    (org-tasktree-ui--quit-edit-buffer)
    (message "org-tasktree: saved uid=%s" (org-tasktree-model-node-uid node))))

(defun org-tasktree-ui--widget-insert-field (label key value hint)
  "Insert one editable field for LABEL and return widget.
KEY, VALUE, and HINT configure the created widget."
  (widget-insert (format "%-10s " (concat label ":")))
  (let ((w (widget-create 'editable-field
                          :size 40
                          :format "%v"
                          :keymap org-tasktree-ui--widget-field-keymap
                          :value (or value ""))))
    (when hint
      (widget-insert " " (propertize hint 'face 'shadow)))
    (widget-insert "\n")
    (setq org-tasktree-ui--widget-widgets
          (plist-put org-tasktree-ui--widget-widgets key w))
    w))

(defun org-tasktree-ui--render-widget-form (meta)
  "Render widget edit UI for META into current buffer."
  (let* ((path (plist-get meta :path-titles))
         (path-str (if (and (listp path) path)
                       (string-join path " > ")
                     "(root)")))
    (widget-insert (propertize (format "Path: %s\n\n" path-str)
                               'face 'bold))
    (org-tasktree-ui--widget-insert-field "title" :title
                                          (plist-get meta :title)
                                          "required")
    (org-tasktree-ui--widget-insert-field
     "priority" :priority (plist-get meta :priority)
     "[A-Za-z0-9]")
    (org-tasktree-ui--widget-insert-field
     "scheduled" :scheduled (plist-get meta :scheduled)
     "YYYY-MM-DD | YYYY/MM/DD | MM-DD | MM/DD | DD  (C-c C-s)")
    (org-tasktree-ui--widget-insert-field
     "deadline" :deadline (plist-get meta :deadline)
     "YYYY-MM-DD | YYYY/MM/DD | MM-DD | MM/DD | DD  (C-c C-d)")
    (org-tasktree-ui--widget-insert-field
     "tags" :tags (plist-get meta :tags)
     ":tag1:tag2: | tag1:tag2  ([A-Za-z0-9_-], ':' separated)")
    (widget-insert "\n")
    (widget-insert
     (propertize "C-c C-c: commit,  C-c C-k: cancel,  TAB/S-TAB: move fields\n"
                 'face 'shadow))))

(defun org-tasktree-ui--set-widget-date (key field)
  "Read date via `org-read-date' and set widget KEY for FIELD."
  (condition-case nil
      (let* ((input (org-read-date nil nil nil nil nil))
             (date (org-tasktree-ui--parse-date-input input field))
             (w (org-tasktree-ui--widget-get key)))
        (when w
          (widget-value-set w (or date ""))
          (widget-setup)))
    (quit nil)))

(defun org-tasktree-ui-widget-edit-set-scheduled ()
  "Prompt date and set scheduled field."
  (interactive)
  (org-tasktree-ui--set-widget-date :scheduled "scheduled"))

(defun org-tasktree-ui-widget-edit-set-deadline ()
  "Prompt date and set deadline field."
  (interactive)
  (org-tasktree-ui--set-widget-date :deadline "deadline"))

(defvar-local org-tasktree-ui--widget-enforce-size-in-progress nil
  "Non-nil while widget field size enforcement is running.")

(defun org-tasktree-ui--widget-enforce-field-size (_beg _end _len)
  "Keep widget editable fields at their fixed display width.

This function is intended for use in `after-change-functions'."
  (unless org-tasktree-ui--widget-enforce-size-in-progress
    (let ((org-tasktree-ui--widget-enforce-size-in-progress t)
          (inhibit-read-only t))
      (save-excursion
        (let ((widgets nil)
              (plist org-tasktree-ui--widget-widgets))
          (while plist
            (push (cadr plist) widgets)
            (setq plist (cddr plist)))
          (dolist (w (nreverse widgets))
            (let* ((from (org-tasktree-ui--pos
                          (or (ignore-errors (widget-field-start w))
                              (widget-get w :from))))
                   (to (org-tasktree-ui--pos
                        (or (ignore-errors (widget-field-end w))
                            (widget-get w :to))))
                   (size (and w (widget-get w :size))))
              (when (and (integerp from)
                         (integerp to)
                         (integerp size)
                         (< from to)
                         (< 0 size))
                (let ((cur (- to from)))
                  (cond
                   ((> cur size)
                    (delete-region (+ from size) to))
                   ((< cur size)
                    (goto-char to)
                    (insert-and-inherit
                     (make-string (- size cur) ? )))))))))))))

(defun org-tasktree-ui--open-widget-edit-buffer (meta)
  "Create and show widget edit buffer for META."
  (let* ((type (plist-get meta :type))
         (buf (generate-new-buffer (format "*org-tasktree-edit %s*" type)))
         win)
    (with-current-buffer buf
      (org-tasktree-ui-widget-edit-mode)
      (setq org-tasktree-ui--edit-metadata meta)
      (setq org-tasktree-ui--widget-widgets nil)
      (setq org-tasktree-ui--widget-enforce-size-in-progress nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-tasktree-ui--render-widget-form meta)
        (widget-setup)
        (org-tasktree-ui--widget-lock-buffer))
      (add-hook 'after-change-functions
                #'org-tasktree-ui--widget-enforce-field-size
                nil
                t)
      (let* ((w (org-tasktree-ui--widget-get :title))
             (from (and w
                        (or (ignore-errors (widget-field-start w))
                            (widget-get w :from))))
             (pos (or (org-tasktree-ui--pos from)
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "^title:[[:space:]]+" nil t)
                          (point)))
                      (point-min))))
        (goto-char pos)))
    (setq win (pop-to-buffer buf))
    (when (window-live-p win)
      (with-selected-window win
        (set-window-start win (point-min))
        (set-window-point win (with-current-buffer buf (point)))))
    win))

(provide 'org-tasktree-ui)
;;; org-tasktree-ui.el ends here
