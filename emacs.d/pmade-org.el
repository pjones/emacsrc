;; This file contains my customizations for the awesome Org-Mode
(eval-when-compile
  (load "pmade-loadpath"))

;; Org Invoice
(autoload 'org-invoice-report "org-invoice" nil t)
(autoload 'org-dblock-write:invoice "org-invoice" nil t)
(autoload 'org-eva-submit "org-eva" nil t)
(autoload 'org-export-as-mindmap "org-mindmap" nil t)

(eval-after-load "org-eva"
  '(let ((eva-conf "~/.comm-sync/etc/eva.el"))
     (when (file-exists-p eva-conf) (load-file eva-conf))))

;; Org Basecamp
(require 'org-basecamp)
(setq org-basecamp-mark-todo nil)

;; Org Crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)

;; Org Depend
(require 'org-depend)

(setq
 ;; General Org Settings
 org-log-done t
 org-reverse-note-order t
 org-deadline-warning-days 14
 org-hide-leading-stars t
 org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
 org-empty-line-terminates-plain-lists t
 org-use-fast-todo-selection t
 org-use-fast-tag-selection 'auto
 org-fast-tag-selection-single-key t
 org-special-ctrl-a/e t
 org-special-ctrl-k t
 org-M-RET-may-split-line nil
 org-time-clocksum-format "%02d:%02d"
 org-clock-into-drawer "CLOCK"
 org-log-into-drawer "LOGBOOK"
 org-tags-exclude-from-inheritance nil
 org-completion-use-ido t
 org-goto-interface 'outline-path-completion
 org-outline-path-complete-in-steps nil
 
 ;; Showing context
 org-show-hierarchy-above '((default . t))
 org-show-following-heading '((default . t))
 org-show-siblings '((default . t))
 org-show-entry-below '((default . t))

 ;; The notes file and options
 org-directory "~/Documents/pmade/pmade-inc/planning"
 org-default-notes-file (concat org-directory "/general/business.org")
 pmade-org-active-clients (concat (expand-file-name org-directory) "/clients/active")
 pmade-org-general-files (concat (expand-file-name org-directory) "/general")
 pmade-org-misc-files (expand-file-name "~/.emacs.d/orgfiles")

 ;; Agenda Files and Agenda Settings
 org-agenda-window-setup 'current-window
 org-agenda-restore-windows-after-quit nil
 org-stuck-projects '("+LEVEL=2+project" ("NEXT" "PENDING" "BLOCKED") ("single") nil)
 org-agenda-ndays 1
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-show-all-dates t
 org-agenda-start-on-weekday 1
 org-agenda-todo-ignore-with-date t
 org-agenda-include-diary nil
 org-agenda-time-grid
   '((today) "--------------------" (730 900 1030 1400 1600 1700))
 org-agenda-files 
   (append
    (directory-files pmade-org-active-clients t "\\.org$")
    (directory-files pmade-org-general-files  t "\\.org$")
    (directory-files pmade-org-misc-files     t "\\.org$"))

 ;; Custom Agenda Views
 org-agenda-custom-commands
 '(("d" "Daily Agenda"
    ((agenda ""
       ((org-agenda-todo-keyword-format "")
        (org-agenda-remove-tags t)))
     (tags "LEVEL=2+goals"
       ((org-agenda-remove-tags t)
        (org-agenda-prefix-format "  ")
        (org-agenda-todo-keyword-format "")))
     (tags "TODO=\"NEXT\"-client"
       ((org-agenda-overriding-header "Non-Client Tasks Marked NEXT")
        (org-agenda-sorting-strategy '(tag-up))
        (org-agenda-show-inherited-tags nil)
        (org-agenda-todo-keyword-format "")))
     (tags "TODO=\"NEXT\"+client" 
       ((org-agenda-overriding-header "Client Tasks")
        (org-agenda-sorting-strategy '(category-up))
        (org-agenda-show-inherited-tags t)
        (org-agenda-todo-keyword-format "")))
     (todo "PENDING"
       ((org-agenda-todo-keyword-format "")))
     (stuck ""
       ((org-agenda-remove-tags t)))))
   ("b" "Blocked Items"
    ((todo "BLOCKED")))
   ("e" "Next Item Effort"
    ((todo "NEXT"
       ((org-agenda-sorting-strategy '(effort-up))
        (org-agenda-show-inherited-tags nil)
        (org-agenda-todo-keyword-format ""))))))
 
 ;; Faces
 org-todo-keyword-faces
   '(("NEXT"    . pmade-org-next-face)
     ("PENDING" . pmade-org-pending-face)
     ("READING" . pmade-org-reading-face))
   
 ;; MobileOrg
 org-mobile-directory "/Volumes/pmade/org"
 org-mobile-inbox-for-pull (concat pmade-org-general-files "/from-mobile.org"))

(defun pmade:org-mode-hook ()
  ;; Extra Bindings
  (org-defkey org-mode-map "\C-ci"     'org-invoice-report)
  (org-defkey org-mode-map "\C-\M-f"   'org-metaright)
  (org-defkey org-mode-map "\C-\M-b"   'org-metaleft)
  (org-defkey org-mode-map "\C-\M-S-f" 'org-shiftmetaright)
  (org-defkey org-mode-map "\C-\M-S-b" 'org-shiftmetaleft)
  (org-defkey org-mode-map "\C-\M-p"   'org-metaup)
  (org-defkey org-mode-map "\C-\M-n"   'org-metadown)

  (org-defkey org-mode-map "\C-c0"               'pmade:org-hide-all)
  (org-defkey org-mode-map "\C-c1"               'pmade:org-hide-others)
  (org-defkey org-mode-map "\C-c\C-r"            'pmade:org-reveal)
  (org-defkey org-mode-map "\C-j"                'pmade:org-list-append)
  (org-defkey org-mode-map [(meta return)]       'pmade:org-list-append)
  (org-defkey org-mode-map [(shift meta return)] 'pmade:org-list-append-with-checkbox)

  ;; Buffer Settings
  (setq save-place nil)
  
  ;; Exporting
  (setq 
   org-latex-to-pdf-process (list "latexmk -pdf %f")
   org-export-html-auto-postamble nil
   org-export-with-sub-superscripts nil
   org-export-with-emphasize nil
   org-icalendar-include-todo nil
   org-icalendar-store-UID t
   org-export-html-style-default ""
   org-export-html-style-extra ""
   org-export-html-style (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" pmade-print-css "\"/>")))

(add-hook 'org-mode-hook 'pmade:org-mode-hook)

(add-hook 'org-agenda-mode-hook 
  (lambda () 
    ;; Use line highlighting in the Org Agenda
    (hl-line-mode 1)
    
    ;; Keys
    (define-key org-agenda-keymap " " 'org-agenda-cycle-show)
    (define-key org-agenda-mode-map " " 'org-agenda-cycle-show)
    (define-key org-agenda-mode-map "g" 'pmade:org-agenda-redo)
    (local-set-key "\C-x\C-w" 'pmade:org-write-agenda)))

;; Publishing
(setq org-publish-project-alist
  '(("knowledge-base"
     :style "<link rel=\"stylesheet\" href=\"/stylesheets/basic.css\" type=\"text/css\"/>"
     :base-directory "~/Documents/pmade/knowledge-base"
     :publishing-directory "~/Sites/knowledge-base"
     :recursive t
     :author-info t
     :email-info t
     :creator-info t
     :timestamp t
     :emphasize t
     :sub-superscript t
     :special-strings t
     :tags t
     :todo-keywords t
     :auto-preamble t
     :auto-postamble t
     :auto-sitemap t
     :sitemap-title "Sitemap"
     :sitemap-sort-folders 'last)))

(defun pmade:org-write-agenda ()
  "Write the agenda buffer to a file, and send to pmade.com."
  (interactive)
  (org-write-agenda "~/agenda.html")
  (shell-command "sed -E 's/T:([0-9+-]+)/T:<a href=\"tel:\\1\">\\1<\\/a>/' < ~/agenda.html | ssh -q dracula.pmade.com 'cat > websites/pmade.com/www/private/agenda.html'")
  (delete-file "~/agenda.html"))

(defun pmade:org-reveal (&optional siblings)
  (interactive "P")
  (org-decrypt-entry)
  (org-reveal siblings))

(defun pmade:org-hide-others ()
  "Close all headings except the heading at point."
  (interactive)
  (org-overview)
  (org-reveal))

(defun pmade:org-hide-all ()
  "Close all headings, move to bob."
  (interactive)
  (goto-char (point-min))
  (org-cycle '(4)))

(defun pmade:org-list-append (&optional checkbox)
  "Append a plain list item to the current heading.  If the
current heading already has plain list items, a new one will be
added, otherwise a new plain list will be created.  If checkbox
is set, add a plain list item with a checkbox."
  (interactive "P")
  (when (not (org-insert-item (if checkbox 'checkbox)))
    (org-back-to-heading)
    (org-show-subtree)
    (outline-next-heading)
    (if (eolp) (newline)
      (newline)
      (previous-line))
    (org-indent-line-function)
    (insert (concat "-" (if checkbox " [ ] " " ")))
    (save-excursion
      (org-back-to-heading)
      (org-cycle-hide-drawers 'subtree))))

(defun pmade:org-list-append-with-checkbox ()
  "Calls `pmade:org-list-append' with checkbox set."
  (interactive)
  (pmade:org-list-append t))

(defun pmade:org-agenda-redo ()
  "Update the agenda view after reverting buffers that were auto
generated from external processes."
  (interactive)
  (with-current-buffer "crum.org" (revert-buffer t t))
  (with-current-buffer "library.org" (revert-buffer t t))
  (org-agenda-redo))

(defun pmade:org-time-diff (t1 t2)
  "Returns the difference between t1 and t2.  Expects that times
are formatted as HH:MM and returns them in that format"
  (org-minutes-to-hh:mm-string 
   (- (org-hh:mm-string-to-minutes t1)
      (org-hh:mm-string-to-minutes t2))))

;; Override the default mode line to use Growl instead
(defun pmade:org-clock-growl (msg)
  (let ((fname "/tmp/org-clock.txt"))
    (with-temp-file fname (insert msg))
    (call-process "growlnotify"
                  fname nil nil "-n" "Org" "-s" 
                  "-a" "Emacs" "-d" "modeline"
                  "OrgMode Clock")))
    
(eval-after-load 'org-clock
  '(progn
     (defun org-clock-update-mode-line ()
       (if org-clock-effort (org-clock-notify-once-if-expired)
         (setq org-task-overrun nil))
       (setq global-mode-string (delq 'org-mode-line-string global-mode-string))
       (pmade:org-clock-growl (org-no-properties (org-clock-get-clock-string))))
     (defun pmade:org-clock-out-with-growl ()
       (pmade:org-clock-growl "Stopped"))
     (add-hook 'org-clock-out-hook 'pmade:org-clock-out-with-growl)))
