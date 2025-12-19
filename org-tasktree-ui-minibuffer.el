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

(defun org-tasktree-ui-minibuffer--completion-color (type)
  "Return configured completion color for TYPE symbol."
  (pcase type
    ('task org-tasktree-ui-minibuffer-completion-color-task)
    ('project org-tasktree-ui-minibuffer-completion-color-project)
    ('phase org-tasktree-ui-minibuffer-completion-color-phase)
    ('group org-tasktree-ui-minibuffer-completion-color-group)
    (_ nil)))

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

(defun org-tasktree-ui-minibuffer--candidate-raw (candidate)
  "Return the raw title string for completion CANDIDATE."
  (or (get-text-property 0 'org-tasktree-ui-minibuffer--raw candidate)
      (if (string-suffix-p "/" candidate)
          (string-remove-suffix "/" candidate)
        candidate)))

(defun org-tasktree-ui-minibuffer--candidate-type (candidate)
  "Return the candidate type symbol for completion CANDIDATE, or nil."
  (get-text-property 0 'org-tasktree-ui-minibuffer--candidate-type candidate))

(defun org-tasktree-ui-minibuffer--prompt-path (prefix)
  "Return minibuffer prompt string using PREFIX path.

PREFIX is either nil or a list of titles."
  (if (and (listp prefix) prefix)
      (concat (string-join prefix org-tasktree-ui-minibuffer--path-sep)
              org-tasktree-ui-minibuffer--path-sep)
    ""))

(defun org-tasktree-ui-minibuffer--node-type= (node type)
  "Return non-nil when NODE is of TYPE string."
  (equal (org-tasktree-model-node-node-type node) type))

(defun org-tasktree-ui-minibuffer--nodes-of-type (nodes type)
  "Return NODES filtered by TYPE string."
  (let (result)
    (dolist (node nodes (nreverse result))
      (when (org-tasktree-ui-minibuffer--node-type= node type)
        (push node result)))))

(defun org-tasktree-ui-minibuffer--titles (nodes)
  "Return list of titles from NODES."
  (mapcar #'org-tasktree-model-node-title nodes))

(defun org-tasktree-ui-minibuffer--sorted-strings (strings)
  "Return a sorted copy of STRINGS."
  (sort (copy-sequence strings) #'string<))

(defun org-tasktree-ui-minibuffer--sorted-titles (nodes)
  "Return sorted list of titles from NODES."
  (org-tasktree-ui-minibuffer--sorted-strings (org-tasktree-ui-minibuffer--titles nodes)))

(defun org-tasktree-ui-minibuffer--node-by-title (nodes type title)
  "Return first node in NODES matching TYPE and TITLE, or nil."
  (catch 'found
    (dolist (node nodes nil)
      (when (and (org-tasktree-ui-minibuffer--node-type= node type)
                 (equal (org-tasktree-model-node-title node) title))
        (throw 'found node)))))

(defun org-tasktree-ui-minibuffer--nav-children (nodes type &rest filters)
  "Return list of nodes of TYPE in NODES matching FILTERS.

FILTERS is a plist that may include :project-id, :phase-id, or :parent-id."
  (let* ((project-set (plist-member filters :project-id))
         (project-id (plist-get filters :project-id))
         (phase-set (plist-member filters :phase-id))
         (phase-id (plist-get filters :phase-id))
         (parent-set (plist-member filters :parent-id))
         (parent-id (plist-get filters :parent-id))
         result)
    (dolist (node nodes (nreverse result))
      (when (and (org-tasktree-ui-minibuffer--node-type= node type)
                 (or (not project-set)
                     (equal (org-tasktree-model-node-project-id node) project-id))
                 (or (not phase-set)
                     (equal (org-tasktree-model-node-phase-id node) phase-id))
                 (or (not parent-set)
                     (equal (org-tasktree-model-node-parent-id node) parent-id)))
        (push node result)))))

(defun org-tasktree-ui-minibuffer--nav-candidates (titles type)
  "Return sorted completion candidates for TITLES of TYPE."
  (mapcar (lambda (title)
            (org-tasktree-ui-minibuffer--make-completion-candidate title type))
          (org-tasktree-ui-minibuffer--sorted-strings titles)))

(defun org-tasktree-ui-minibuffer--nav-candidates-from-nodes (nodes type)
  "Return sorted completion candidates for NODES of TYPE."
  (org-tasktree-ui-minibuffer--nav-candidates (org-tasktree-ui-minibuffer--titles nodes) type))

(defun org-tasktree-ui-minibuffer--nav-merge-candidates (&rest candidate-lists)
  "Return sorted completion candidates from CANDIDATE-LISTS."
  (org-tasktree-ui-minibuffer--sorted-strings (apply #'append candidate-lists)))

(defun org-tasktree-ui-minibuffer--nav-read-input
    (prompt cands require-match allow-backspace-up allow-auto-enter)
  "Read navigation input using PROMPT and CANDS.

REQUIRE-MATCH, ALLOW-BACKSPACE-UP, and ALLOW-AUTO-ENTER are passed to
`org-tasktree-ui-minibuffer--completing-read'.

Return :up or a plist with :raw, :type, and :candidate."
  (let ((input (org-tasktree-ui-minibuffer--completing-read
                prompt cands require-match allow-backspace-up allow-auto-enter)))
    (if (eq input :up)
        :up
      (list :raw (string-trim (org-tasktree-ui-minibuffer--candidate-raw input))
            :type (org-tasktree-ui-minibuffer--candidate-type input)
            :candidate input))))

(defun org-tasktree-ui-minibuffer--nav-read (initial-state step-fn)
  "Run navigation state machine starting from INITIAL-STATE using STEP-FN.

STEP-FN receives the current state and must return a plist that includes
either :state NEXT-STATE, :result RESULT, or :stay t."
  (let ((state initial-state)
        result)
    (while (null result)
      (let ((action (funcall step-fn state)))
        (cond
         ((plist-member action :result)
          (setq result (plist-get action :result)))
         ((plist-member action :state)
          (setq state (plist-get action :state)))
         ((plist-member action :stay)
          nil)
         (t
          (error "Invalid navigation action: %S" action)))))
    result))

(defun org-tasktree-ui-minibuffer--read-required
    (prompt cands empty-message &optional allow-auto-enter)
  "Read from CANDS with PROMPT.  Signal `user-error' when CANDS is empty.

EMPTY-MESSAGE is shown when CANDS is empty.  ALLOW-AUTO-ENTER enables
automatic confirmation for directory candidates."
  (unless cands
    (user-error "%s" empty-message))
  (let ((input (org-tasktree-ui-minibuffer--nav-read-input
                prompt cands t nil allow-auto-enter)))
    (when (eq input :up)
      (user-error "Unexpected navigation state"))
    (plist-get input :raw)))

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

(defun org-tasktree-ui-minibuffer--minibuffer-backspace ()
  "Handle Backspace in org-tasktree minibuffer."
  (interactive)
  (if (and org-tasktree-ui-minibuffer--minibuffer-backspace-up-enabled
           (string-empty-p (minibuffer-contents-no-properties)))
      (throw 'org-tasktree-ui-minibuffer--minibuffer-up :up)
    (call-interactively
     (or org-tasktree-ui-minibuffer--minibuffer-backspace-original
         #'delete-backward-char))))

(defun org-tasktree-ui-minibuffer--minibuffer-cleanup ()
  "Cleanup hooks and buffer-local state for org-tasktree minibuffer sessions."
  (remove-hook 'post-command-hook #'org-tasktree-ui-minibuffer--minibuffer-maybe-auto-enter t)
  (remove-hook 'minibuffer-exit-hook #'org-tasktree-ui-minibuffer--minibuffer-cleanup t)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-enabled nil)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-candidates nil)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-backspace-up-enabled nil)
  (setq-local org-tasktree-ui-minibuffer--minibuffer-backspace-original nil)
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
    (prompt cands &optional require-match allow-backspace-up allow-auto-enter)
  "Read a completion from CANDS with PROMPT.

When ALLOW-BACKSPACE-UP is non-nil, pressing Backspace in an empty minibuffer
returns the symbol `:up'.

When ALLOW-AUTO-ENTER is non-nil, typing an exact directory candidate (ending
with \"/\") auto-confirms and proceeds without requiring RET."
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
                   (setq-local org-tasktree-ui-minibuffer--minibuffer-auto-enter-candidates
                               (when auto
                                 (let (result)
                                   (dolist (cand cands (nreverse result))
                                     (let ((s (substring-no-properties cand)))
                                       (when (string-suffix-p "/" s)
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
                   (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map (current-local-map))
                     (define-key
                      map
                      (kbd "DEL")
                      #'org-tasktree-ui-minibuffer--minibuffer-backspace)
                     (define-key
                      map
                      (kbd "<backspace>")
                      #'org-tasktree-ui-minibuffer--minibuffer-backspace)
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

(defun org-tasktree-ui-minibuffer--collect (pred)
  "Collect nodes satisfying PRED with display strings and metadata.
Returns list of (DISPLAY . PLIST)."
  (let* ((nodes (org-tasktree-query-open-tree))
         (id-map (org-tasktree-ui-minibuffer--id-map nodes))
         (result '()))
    (dolist (node nodes)
      (when (funcall pred node)
        (let* ((titles (org-tasktree-ui-minibuffer--titles-path node id-map))
               (display (org-tasktree-ui-minibuffer--format-path titles)))
          (push (cons display
                      (list :node node
                            :titles titles))
                result))))
    (nreverse result)))

(defun org-tasktree-ui-minibuffer-read-project ()
  "Prompt for project via `completing-read'.
Returns plist: (:project-title STRING :project-id ID-or-nil)."
  (let* ((cands (org-tasktree-ui-minibuffer--collect
                 (lambda (n)
                   (equal (org-tasktree-model-node-node-type n)
                          "project"))))
         (display-cands
          (mapcar (lambda (pair)
                    (org-tasktree-ui-minibuffer--make-completion-candidate
                     (car pair) 'project))
                  cands))
         (choice (org-tasktree-ui-minibuffer--completing-read
                  "find project: "
                  display-cands
                  nil
                  nil))
         (raw (string-trim (org-tasktree-ui-minibuffer--candidate-raw choice))))
    (let* ((found (assoc raw cands))
           (node (plist-get (cdr found) :node)))
      (if node
          (list :project-title (org-tasktree-model-node-title node)
                :project-id (org-tasktree-model-node-id node))
        (list :project-title raw :project-id nil)))))

(defun org-tasktree-ui-minibuffer-read-phase ()
  "Prompt for a phase by navigating project -> phase.
Return plist with titles and ids when existing; missing ids mean new."
  (let* ((nodes (org-tasktree-query-open-tree))
         (projects (org-tasktree-ui-minibuffer--nav-children nodes "project"))
         (project-cands
          (org-tasktree-ui-minibuffer--nav-candidates-from-nodes projects 'project))
         project-title
         project-id)
    (org-tasktree-ui-minibuffer--nav-read
     :project
     (lambda (state)
       (pcase state
         (:project
          (setq project-title
                (org-tasktree-ui-minibuffer--read-required
                 "find node: "
                 project-cands
                 "No projects found.  Create a project first."
                 t))
          (let ((project-node
                 (or (org-tasktree-ui-minibuffer--node-by-title nodes "project" project-title)
                     (user-error "Project not found: %s" project-title))))
            (setq project-id (org-tasktree-model-node-id project-node)))
          (list :state :phase))
         (:phase
          (let* ((phases (org-tasktree-ui-minibuffer--nav-children
                          nodes "phase" :project-id project-id))
                 (phase-cands
                  (org-tasktree-ui-minibuffer--nav-candidates-from-nodes phases 'phase))
                 (input
                  (org-tasktree-ui-minibuffer--nav-read-input
                   (concat "find node: "
                           (org-tasktree-ui-minibuffer--prompt-path (list project-title)))
                   phase-cands
                   nil
                   t
                   nil)))
            (if (eq input :up)
                (list :state :project)
              (let ((phase-title (plist-get input :raw)))
                (when (string-empty-p phase-title)
                  (user-error "Phase title is required"))
                (let* ((phase-node
                        (org-tasktree-ui-minibuffer--node-by-title phases "phase" phase-title))
                       (phase-id (and phase-node
                                      (org-tasktree-model-node-id phase-node))))
                  (list :result
                        (list :project-title project-title
                              :project-id project-id
                              :phase-title phase-title
                              :phase-id phase-id)))))))
         (_
          (error "Unknown state: %S" state)))))))

(defun org-tasktree-ui-minibuffer-read-task ()
  "Prompt for a task by navigating project -> phase? -> group? -> task.

Project must exist.  Phase and group must exist when selected.

If the phase input does not match an existing phase title, treat it as a task
title under the selected project.

If the group input does not match an existing group title, treat it as a task
title under the selected phase."
  (let* ((nodes (org-tasktree-query-open-tree))
         (projects (org-tasktree-ui-minibuffer--nav-children nodes "project"))
         (project-cands
          (org-tasktree-ui-minibuffer--nav-candidates-from-nodes projects 'project))
         project-title
         project-id
         phase-title
         phase-id
         group-title
         group-id)
    (cl-labels
        ((read-task-title (titles tasks)
           (let* ((task-cands
                   (org-tasktree-ui-minibuffer--nav-candidates-from-nodes tasks 'task))
                  (input
                   (org-tasktree-ui-minibuffer--nav-read-input
                    (concat "find node: " (org-tasktree-ui-minibuffer--prompt-path titles))
                    task-cands
                    nil
                    t
                    nil)))
             (if (eq input :up)
                 :up
               (plist-get input :raw)))))
      (org-tasktree-ui-minibuffer--nav-read
       :project
       (lambda (state)
         (pcase state
           (:project
            (setq project-title
                  (org-tasktree-ui-minibuffer--read-required
                   "find node: "
                   project-cands
                   "No projects found.  Create a project first."
                   t))
            (let ((project-node
                   (or (org-tasktree-ui-minibuffer--node-by-title nodes "project" project-title)
                       (user-error "Project not found: %s" project-title))))
              (setq project-id (org-tasktree-model-node-id project-node)))
            (setq phase-title nil
                  phase-id nil
                  group-title nil
                  group-id nil)
            (list :state :phase-or-task))

           (:phase-or-task
            (let* ((phases (org-tasktree-ui-minibuffer--nav-children
                            nodes "phase" :project-id project-id))
                   (phase-titles (org-tasktree-ui-minibuffer--titles phases))
                   (project-tasks (org-tasktree-ui-minibuffer--nav-children
                                   nodes "task"
                                   :project-id project-id
                                   :phase-id nil
                                   :parent-id project-id))
                   (cands
                    (org-tasktree-ui-minibuffer--nav-merge-candidates
                     (org-tasktree-ui-minibuffer--nav-candidates phase-titles 'phase)
                     (org-tasktree-ui-minibuffer--nav-candidates-from-nodes project-tasks 'task)))
                   (input
                    (org-tasktree-ui-minibuffer--nav-read-input
                     (concat "find node: "
                             (org-tasktree-ui-minibuffer--prompt-path (list project-title)))
                     cands
                     nil
                     t
                     t)))
              (if (eq input :up)
                  (list :state :project)
                (let* ((raw (plist-get input :raw))
                       (input-type (plist-get input :type)))
                  (cond
                   ((string-empty-p raw)
                    (list :state :project-task))
                   ((or (eq input-type 'phase) (member raw phase-titles))
                    (setq phase-title raw
                          phase-id (org-tasktree-model-node-id
                                    (or (org-tasktree-ui-minibuffer--node-by-title
                                         phases "phase" raw)
                                        (user-error "Phase not found: %s" raw)))
                          group-title nil
                          group-id nil)
                    (list :state :group-or-task))
                   (t
                    (when (string-empty-p raw)
                      (user-error "Task title is required"))
                    (let* ((task-node
                            (org-tasktree-ui-minibuffer--node-by-title project-tasks "task" raw))
                           (task-id (and task-node
                                         (org-tasktree-model-node-id task-node))))
                      (list :result
                            (list :project-title project-title
                                  :project-id project-id
                                  :phase-title nil
                                  :phase-id nil
                                  :group-title nil
                                  :parent-id project-id
                                  :task-title raw
                                  :task-id task-id)))))))))

           (:project-task
            (let* ((tasks (org-tasktree-ui-minibuffer--nav-children
                           nodes "task"
                           :project-id project-id
                           :phase-id nil
                           :parent-id project-id))
                   (title (read-task-title (list project-title) tasks)))
              (if (eq title :up)
                  (list :state :phase-or-task)
                (when (string-empty-p title)
                  (user-error "Task title is required"))
                (let* ((task-node (org-tasktree-ui-minibuffer--node-by-title tasks "task" title))
                       (task-id (and task-node
                                     (org-tasktree-model-node-id task-node))))
                  (list :result
                        (list :project-title project-title
                              :project-id project-id
                              :phase-title nil
                              :phase-id nil
                              :group-title nil
                              :parent-id project-id
                              :task-title title
                              :task-id task-id))))))

           (:group-or-task
            (let* ((groups (org-tasktree-ui-minibuffer--nav-children
                            nodes "group"
                            :project-id project-id
                            :phase-id phase-id))
                   (group-titles (org-tasktree-ui-minibuffer--titles groups))
                   (phase-tasks (org-tasktree-ui-minibuffer--nav-children
                                 nodes "task"
                                 :project-id project-id
                                 :phase-id phase-id
                                 :parent-id phase-id))
                   (cands
                    (org-tasktree-ui-minibuffer--nav-merge-candidates
                     (org-tasktree-ui-minibuffer--nav-candidates group-titles 'group)
                     (org-tasktree-ui-minibuffer--nav-candidates-from-nodes phase-tasks 'task)))
                   (input
                    (org-tasktree-ui-minibuffer--nav-read-input
                     (concat "find task group: "
                             (org-tasktree-ui-minibuffer--prompt-path
                              (list project-title phase-title)))
                     cands
                     nil
                     t
                     t)))
              (if (eq input :up)
                  (progn
                    (setq group-title nil
                          group-id nil)
                    (list :state :phase-or-task))
                (let* ((raw (plist-get input :raw))
                       (input-type (plist-get input :type)))
                  (cond
                   ((eq input-type 'task)
                    (when (string-empty-p raw)
                      (user-error "Task title is required"))
                    (let* ((task-node
                            (org-tasktree-ui-minibuffer--node-by-title phase-tasks "task" raw))
                           (task-id (and task-node
                                         (org-tasktree-model-node-id task-node))))
                      (list :result
                            (list :project-title project-title
                                  :project-id project-id
                                  :phase-title phase-title
                                  :phase-id phase-id
                                  :group-title nil
                                  :parent-id phase-id
                                  :task-title raw
                                  :task-id task-id))))
                   ((string-empty-p raw)
                    (list :state :phase-task))
                   ((or (eq input-type 'group) (member raw group-titles))
                    (setq group-title raw
                          group-id (org-tasktree-model-node-id
                                    (or (org-tasktree-ui-minibuffer--node-by-title
                                         groups "group" raw)
                                        (user-error "Group not found: %s" raw))))
                    (list :state :group-task))
                   (t
                    (when (string-empty-p raw)
                      (user-error "Task title is required"))
                    (let* ((task-node
                            (org-tasktree-ui-minibuffer--node-by-title phase-tasks "task" raw))
                           (task-id (and task-node
                                         (org-tasktree-model-node-id task-node))))
                      (list :result
                            (list :project-title project-title
                                  :project-id project-id
                                  :phase-title phase-title
                                  :phase-id phase-id
                                  :group-title nil
                                  :parent-id phase-id
                                  :task-title raw
                                  :task-id task-id)))))))))

           (:phase-task
            (let* ((tasks (org-tasktree-ui-minibuffer--nav-children
                           nodes "task"
                           :project-id project-id
                           :phase-id phase-id
                           :parent-id phase-id))
                   (title (read-task-title (list project-title phase-title) tasks)))
              (if (eq title :up)
                  (list :state :group-or-task)
                (when (string-empty-p title)
                  (user-error "Task title is required"))
                (let* ((task-node (org-tasktree-ui-minibuffer--node-by-title tasks "task" title))
                       (task-id (and task-node
                                     (org-tasktree-model-node-id task-node))))
                  (list :result
                        (list :project-title project-title
                              :project-id project-id
                              :phase-title phase-title
                              :phase-id phase-id
                              :group-title nil
                              :parent-id phase-id
                              :task-title title
                              :task-id task-id))))))

           (:group-task
            (let* ((tasks (org-tasktree-ui-minibuffer--nav-children
                           nodes "task"
                           :project-id project-id
                           :phase-id phase-id
                           :parent-id group-id))
                   (title (read-task-title
                           (list project-title phase-title group-title) tasks)))
              (if (eq title :up)
                  (list :state :group-or-task)
                (when (string-empty-p title)
                  (user-error "Task title is required"))
                (let* ((task-node (org-tasktree-ui-minibuffer--node-by-title tasks "task" title))
                       (task-id (and task-node
                                     (org-tasktree-model-node-id task-node))))
                  (list :result
                        (list :project-title project-title
                              :project-id project-id
                              :phase-title phase-title
                              :phase-id phase-id
                              :group-title group-title
                              :parent-id group-id
                              :task-title title
                              :task-id task-id))))))

           (_
            (error "Unknown state: %S" state))))))))

(provide 'org-tasktree-ui-minibuffer)
;;; org-tasktree-ui-minibuffer.el ends here
