;;; org-conf.el -- Settings for org-mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-journal)
(require 'org-roam)
(require 's)

(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

;; Silence compiler warnings
(declare-function pjones:org-roam-activate "./org-roam-conf.el")
(declare-function whitespace-mode "whitespace")
(declare-function org-bookmark-jump-unhide "org")
(declare-function org-clocking-p "org-clock")
(declare-function dbus-send-signal "dbus")
(declare-function org-clock-sum-current-item "org-clock")
(declare-function org-trello-mode "org-trello")
(declare-function org-bullets-mode "org-bullets")
(defvar whitespace-style)
(defvar org-clock-start-time)
(defvar dbus-path-emacs)
(defvar dbus-interface-emacs)

;; General Org Settings
(custom-set-variables
 ;; Visual Settings:
 '(org-hide-leading-stars t)
 '(org-clock-clocked-in-display (quote both))
 '(org-clock-mode-line-total 'current)
 '(org-show-context-detail (quote ((default . tree))))
 '(org-duration-format (quote h:mm))

 ;; Behavior Settings:
 '(org-log-done 'time)
 '(org-reverse-note-order nil)
 '(org-list-empty-line-terminates-plain-lists nil)
 '(org-blank-before-new-entry (quote ((heading . nil) (plain-list-item . nil))))
 '(org-tags-column 0)
 '(org-use-fast-todo-selection t)
 '(org-use-fast-tag-selection (quote auto))
 '(org-fast-tag-selection-single-key nil)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-M-RET-may-split-line t)
 '(org-clock-into-drawer t)
 '(org-log-into-drawer t)
 '(org-tags-exclude-from-inheritance nil)
 '(org-completion-use-ido t)
 '(org-goto-interface 'outline-path-completion)
 '(org-outline-path-complete-in-steps t)
 '(org-id-link-to-org-use-id t)
 '(org-edit-src-persistent-message nil)
 '(org-src-window-setup (quote current-window))
 '(org-attach-directory "attachments/")
 '(org-attach-commit nil)
 '(org-attach-git-annex-cutoff nil)
 '(org-attach-annex-auto-get nil)
 '(org-attach-method 'ln)
 '(org-attach-store-link-p t)
 '(org-attach-archive-delete t)

 ;; Showing context
 '(org-show-hierarchy-above t)
 '(org-show-following-heading t)
 '(org-show-siblings t)
 '(org-show-entry-below t)

 ;; Following Links
 '(org-file-apps (quote ((auto-mode       . emacs)
                         ("\\.mm\\'"      . default)
                         ("\\.x?html?\\'" . default)
                         ("\\.mp4\\'"     . "vlc %s"))))

 '(org-link-frame-setup
   (quote ((file . find-file))))

 ;; Tags:
 '(org-tag-persistent-alist
   (quote ((:startgroup  . nil)
           ("@computer"  . ?c)
           ("@desk"      . ?d)
           ("@email"     . ?e)
           ("@errand"    . ?E)
           ("@home"      . ?h)
           ("@kindle"    . ?k)
           ("@phone"     . ?p)
           (:endgroup    . nil)
           (:startgroup  . nil)
           ("@online"    . ?o)
           ("@offline"   . ?O)
           (:endgroup    . nil)
           (:startgroup  . nil)
           ("5m"         . ?5)
           ("30m"        . ?3)
           ("1h"         . ?1)
           ("4h"         . ?4)
           (:endgroup    . nil))))

 ;; TODO keywords and faces:
 '(org-todo-keywords
   (quote ((sequence "TODO(t)" "|" "DONE(d)")
           (sequence "|" "NOTES")
           (sequence "NEXT(n)" "WAITING(w)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)"))))

 '(org-todo-keyword-faces
   (quote (("TODO"    . (:foreground "#66cccc" :weight bold))
           ("NEXT"    . (:foreground "#66cccc" :weight bold))
           ("WAITING" . (:foreground "#cc99cc" :weight bold))
           ("BLOCKED" . (:inherit org-agenda-dimmed-todo-face)))))

 ;; Stuff for org-agenda.
 '(org-agenda-files
   (quote ("~/notes/agenda/projects.org"
           "~/notes/agenda/tasks.org"
           "~/notes/agenda/review.org"
           "~/notes/agenda/calendar.org"
           "~/notes/agenda/inbox.org")))

 '(org-agenda-window-setup (quote current-window))
 '(org-agenda-todo-ignore-with-date nil)
 '(org-agenda-todo-ignore-timestamp nil)
 '(org-agenda-todo-ignore-deadlines (quote near))
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-time-leading-zero t)
 '(org-agenda-show-inherited-tags nil)
 '(org-deadline-warning-days 14)
 '(org-agenda-span 'day)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-day nil)

 '(org-stuck-projects
   (quote ("+project+LEVEL=3-notes-TODO=\"DONE\""
           ("NEXT" "WAITING" "BLOCKED") nil "")))

 '(org-agenda-custom-commands
   (quote (("c" "Current Status"
            ((agenda)
             (todo "WAITING"
                   ((org-agenda-todo-ignore-deadlines (quote future))
                    (org-agenda-todo-ignore-scheduled (quote future))))
             (tags-todo "@phone|@email"
                   ((org-agenda-todo-ignore-deadlines (quote all))
                    (org-agenda-todo-ignore-scheduled (quote all))))
             (stuck)
             (tags "+TODO=\"BLOCKED\"")
             (tags "+inbox+LEVEL=2|+orgzly+LEVEL=1")
             (todo "NEXT"
                   ((org-agenda-todo-ignore-deadlines (quote all))
                    (org-agenda-todo-ignore-scheduled (quote all))))))
           ("p" "Project List"
            ((tags "+project+LEVEL=3")))
           ("e" "Tasks by Energy Level"
            ((tags-todo "5m")
             (tags-todo "30m")
             (tags-todo "1h"))
            ((org-agenda-todo-ignore-deadlines nil)))
           ("o" "Offline Tasks"
            ((tags-todo "+@offline")))
           ("T" "Travel Schedule"
            ((tags "+travel+TIMESTAMP>=\"<now>\""))
            ((org-agenda-view-columns-initially t))))))

 ;; Stuff for org-capture and org-refile:
 '(org-default-notes-file "~/notes/agenda/tasks.org")
 '(org-refile-use-outline-path t)
 '(org-refile-allow-creating-parent-nodes t)
 '(org-log-refile (quote time))
 '(org-archive-location "~/notes/agenda/archive/%s::")

 '(org-refile-targets
   (quote (("~/notes/agenda/projects.org" :level . 3)
           ("~/notes/agenda/tasks.org" :level . 1)
           ("~/notes/agenda/review.org" :level . 2)
           ("~/notes/agenda/calendar.org" :level . 1))))

 '(org-capture-templates
   (quote (("i" "Inbox" entry (id "9a053ee1-ade2-43af-af63-ea3e28fcc639")
            (file "~/notes/etc/templates/orgmode/inbox.org"))
           ("t" "Standalone Task" entry (id "f058299a-ab32-4374-9d38-2ef744c3f306")
            (file "~/notes/etc/templates/orgmode/task.org"))
           ("o" "Prep for Upcoming Offline" entry (id "6224680c-d1c7-4e3a-825e-affa4a065345")
            (file "~/notes/etc/templates/orgmode/task.org"))
           ("r" "Respond to Email" entry (id "3ef415be-890a-407b-8a3e-21814810790e")
            (file "~/notes/etc/templates/orgmode/mail.org"))
           ("c" "New Training Class" entry (id "09727f4a-aa01-4429-8408-d40511c19657")
            (file "~/notes/etc/templates/orgmode/training.org")))))

 ;; Plugin Settings:
 '(org-mru-clock-completing-read #'ivy-completing-read))

(defun pjones:org-mode-hook ()
  "Hook to hack `org-mode'."
  ;; Buffer Settings
  (save-place-mode -1)

  ;; Tailor whitespace mode
  (set (make-local-variable 'whitespace-style)
       '(trailing tabs empty))
  (whitespace-mode))

(add-hook 'org-mode-hook 'pjones:org-mode-hook)

(defun pjones:org-agenda-mode-hook ()
  "Hook run after a `org-agenda-mode' buffer is created."
  (hl-line-mode 1))

(add-hook 'org-agenda-mode-hook 'pjones:org-agenda-mode-hook)

(defun pjones:org-hide-others ()
  "Close all headings except the heading at point."
  (interactive)
  (org-back-to-heading)
  (org-overview)
  (org-show-set-visibility 'tree)
  (org-show-entry))

(defun pjones:org-hide-all ()
  "Close all headings, move to bob."
  (interactive)
  (goto-char (point-min))
  (org-cycle '(4)))

(defun pjones:org-smart-insert (respect &optional todo)
  "Insert a heading or plain list item.
If RESPECT is non-nil and the current item is a heading, always insert
after the current subtree content.  If TODO is non-nil, mark it as
to-do or checkbox."
  (interactive "P")
  (let* ((plain (org-at-item-p))
         (org-insert-heading-respect-content (and respect (not plain))))
    (if todo
        (org-insert-todo-heading
         nil (if plain nil '(4)))
      (org-meta-return
       (if plain nil '(4))))
    (evil-insert-state)))

(defun pjones:org-insert-below (&optional todo)
  "Insert a heading or item below the current one.
If TODO is non-nil, mark it as to-do or checkbox."
  (interactive "P")
  (end-of-line)
  (pjones:org-smart-insert t todo))

(defun pjones:org-insert-above (&optional todo)
  "Insert a heading or item above the current one.
If TODO is non-nil, mark it as to-do or checkbox."
  (interactive "P")
  (beginning-of-line)
  (pjones:org-smart-insert nil todo))

(defun pjones:org-goto (&optional alternative-interface)
  "Call `org-goto' after first widening the buffer.
Jumps to the correct heading via `org-goto', then narrows the buffer
again if necessary.  Passes ALTERNATIVE-INTERFACE to `org-goto'."
  (interactive "P")
  (let ((point-size (save-restriction (- (point-max) (point-min))))
        (buff-size  (buffer-size)))
    (widen)
    (org-goto alternative-interface)
    (if (/= buff-size point-size) (org-narrow-to-subtree))))

(defun pjones:org-clock-update-dbus ()
  "Broadcast a D-Bus signal with the latest `org-clock' data.

This exposes the current clock's start time and heading to any process
listening to the correct D-Bus signal.

You can monitor this signal via the following command:

    dbus-monitor type='signal',interface='org.gnu.Emacs.Org.Clock'

Read the code below for the two event names and the signal arguments
they provide."
  (require 'dbus)
  (require 'org-clock)
  (if (org-clocking-p)
      (let ((start-time (floor (float-time org-clock-start-time)))
            (description org-clock-heading))
        (dbus-send-signal
         :session nil dbus-path-emacs
         (concat dbus-interface-emacs ".Org.Clock") "Started"
         start-time description))
    (dbus-send-signal
     :session nil dbus-path-emacs
     (concat dbus-interface-emacs ".Org.Clock") "Stopped")))

(defun pjones:org-effort-sum (&optional skip-done clock-diff)
  "Recursively sum the Effort property.
If SKIP-DONE is non-nil done headings report an effort of 0.  If
CLOCK-DIFF is non-nil, return the difference between the effort and
clocked time."
  (apply '+ (org-map-entries
    (lambda ()
      (if (and skip-done (org-entry-is-done-p)) 0
        (let* ((effort (org-entry-get (point) "Effort"))
               (mins (if effort (org-duration-to-minutes effort) 0)))
          (if (and (> mins 0) clock-diff)
              (- mins (org-clock-sum-current-item))
            mins))))
    t 'tree)))

(defun org-dblock-write:pjones-project-review (params)
  "Create a table for reviewing projects.

PARAMS is a property list of parameters:

`:id' (mandatory) The heading to start at.
`:hlines' (optional number) Draw a line before this heading level."
  (let ((id (or (plist-get params :id) (error "Missing :id")))
        (hlines (or (plist-get params :hlines) 0))
        base-level table)
    (insert "| Task | Effort | Clocked | Remaining |\n")
    (insert "|------|--------|---------|-----------|\n")
    (save-excursion
      (let ((m (org-id-find id 'marker)))
        (unless m
          (error "Cannot find entry with ID \"%s\"" id))
        (with-current-buffer (marker-buffer m)
          (goto-char m)
          (org-show-context)
          (setq base-level (org-current-level))
          (org-map-entries
           (lambda ()
             (let* ((level (- (org-current-level) base-level))
                    (indent (if (> level 0) (concat "\\_" (make-string level ?_))))
                    (title (concat indent " " (org-get-heading)))
                    (clock (org-clock-sum-current-item))
                    (effort (pjones:org-effort-sum))
                    (left (pjones:org-effort-sum t t))
                    plist)
               (setq plist (list
                 :title title
                 :level (org-current-level)
                 :effort (org-duration-from-minutes effort)
                 :clock (org-duration-from-minutes clock)
                 :left (org-duration-from-minutes left)))
               (push plist table)))
           t 'tree))))
      (dolist (row (nreverse table))
        (when (= hlines (plist-get row :level))
          (insert "|-\n"))
        (dolist (attr (list :title :effort :clock :left))
          (insert (concat "| " (plist-get row attr))))
        (insert "|\n"))
      (org-table-align)))

(defun pjones:org-trello-activate ()
  "Maybe activate `org-trello-mode'."
  (let ((name (buffer-file-name (current-buffer))))
    (when (and name (s-matches-p "\\.trello\\.org$" name))
      (org-trello-mode))))

;;; Key Bindings:

  ;; (org-defkey org-mode-map "\C-ce"               'pjones:org-edit-special)
  ;; (org-defkey org-mode-map "\C-c0"               'pjones:org-hide-all)
  ;; (org-defkey org-mode-map "\C-c1"               'pjones:org-hide-others)
  ;; (org-defkey org-mode-map "\C-j"                'pjones:org-list-append)
  ;; (org-defkey org-mode-map "\C-c\C-j"            'pjones:org-goto)
  ;; (org-defkey org-mode-map [(meta return)]       'pjones:org-list-append)
  ;; (org-defkey org-mode-map [(shift meta return)] 'pjones:org-list-append-with-checkbox)


;; (evil-leader/set-key-for-mode 'org-mode
;;   "DEL d" #'org-time-stamp-inactive
;;   "DEL i" #'org-clock-in
;;   "DEL o" #'org-clock-out
;;   "DEL s" #'org-schedule
;;   "DEL t" #'org-todo)
;;
;; (evil-leader/set-key-for-mode 'org-agenda-mode
;;   "w" #'org-save-all-org-buffers
;;   "DEL i" #'org-agenda-clock-in
;;   "DEL o" #'org-agenda-clock-out
;;   "DEL s" #'org-agenda-schedule
;;   "DEL t" #'org-agenda-todo)

;; Taken (and modified) from: https://github.com/Somelauw/evil-org-mode
(evil-define-operator evil-org-> (beg end count)
  "Demote, indent, move column right."
  :type line
  :move-point nil
  (interactive "<r><vc>")
  (when (null count) (setq count 1))
  (cond
   ;; Work with subtrees and headings
   ((org-with-limited-levels
     (or (org-at-heading-p)
         (save-excursion (goto-char beg) (org-at-heading-p))))
    (if (> count 0)
        (org-map-region 'org-do-demote beg end)
      (org-map-region 'org-do-promote beg end)))
   (t
    (condition-case nil
      (if (> count 0) (org-shiftmetaright)
        (org-shiftmetaleft))
      (error
       (if (> count 0) (org-indent-item)
         (org-outdent-item))))))
  (when (evil-visual-state-p)
    (evil-normal-state)
    (evil-visual-restore)))

;; Taken from: https://github.com/Somelauw/evil-org-mode
(evil-define-operator evil-org-< (beg end count)
  "Promote, dedent, move column left."
  :type line
  :move-point nil
  (interactive "<r><vc>")
  (evil-org-> beg end (- (or count 1))))

(defun pjones:org-activate ()
  "Activate/finish something."
  (interactive)
  (cond
   (org-capture-mode
    (org-capture-finalize))
   (t
    (org-ctrl-c-ctrl-c))))

(defun pjones:org-cancel ()
  "Cancel something."
  (interactive)
  (cond
   (org-capture-mode
    (org-capture-kill))
   (t
    (org-kill-note-or-show-branches))))

(evil-set-initial-state 'org-mode 'normal)
(evil-set-initial-state 'org-agenda-mode 'normal)

(evil-define-key 'normal org-mode-map
  ;; Folding:
  "zo" #'outline-show-children
  "zO" #'outline-show-branches
  "zc" #'outline-hide-subtree
  "zC" #'pjones:org-hide-others
  "za" #'org-cycle
  "zr" #'outline-show-all
  "zm" #'pjones:org-hide-all
  "zR" #'org-reveal

  ;; Header Manipulation:
  ">" #'evil-org->
  "<" #'evil-org-<

  ;; Links:
  "gx" #'org-open-at-point)

(evil-define-key 'insert org-mode-map
  "\C-j" #'pjones:org-insert-below
  "\C-k" #'pjones:org-insert-above
  "\C-ci" #'org-roam-insert)

(evil-define-key 'motion org-mode-map
  "gj" #'outline-forward-same-level
  "gk" #'outline-up-heading
  "gJ" #'org-shiftmetadown
  "gK" #'org-shiftmetaup)

(evil-leader/set-key-for-mode 'org-mode
  "m c c" #'pjones:org-activate
  "m c i" #'org-clock-in
  "m c o" #'org-clock-out
  "m k" #'pjones:org-cancel
  "m d !" #'org-time-stamp-inactive
  "m d ." #'org-time-stamp
  "m d d" #'org-deadline
  "m d s" #'org-schedule
  "m g g" #'pjones:org-goto
  "m j" #'pjones:org-insert-below
  "m k" #'pjones:org-insert-above
  "m t" #'org-todo
  "m T" #'org-set-tags-command)

(evil-define-key 'normal org-agenda-mode-map
  "q" #'org-agenda-quit
  "gr" #'org-agenda-redo-all)

(evil-leader/set-key-for-mode 'org-agenda-mode
  "f s" #'org-save-all-org-buffers
  "j j" #'org-journal-new-entry
  "j r" #'org-roam-find-file
  "m c i" #'org-agenda-clock-in
  "m c o" #'org-agenda-clock-out
  "m d s" #'org-agenda-schedule
  "m t" #'org-agenda-todo)

;;; Hooks
(add-hook 'org-agenda-mode-hook #'pjones:org-roam-activate)
(add-hook 'org-mode-hook #'org-bullets-mode)
(add-hook 'org-mode-hook #'pjones:org-roam-activate)
(add-hook 'org-mode-hook #'pjones:org-trello-activate)

(let ((hooks
       '(org-clock-in-hook
         org-clock-out-hook
         org-clock-cancel-hook)))
  (dolist (hook hooks)
    (add-hook hook #'pjones:org-clock-update-dbus)))

;;; org-conf.el ends here

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
