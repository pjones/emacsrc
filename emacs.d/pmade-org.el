;; This file contains my customizations for the awesome Org-Mode
(eval-when-compile
  (load "pmade-loadpath"))

;; Org Invoice
(autoload 'org-invoice-report "org-invoice" nil t)
(autoload 'org-dblock-write:invoice "org-invoice" nil t)
(autoload 'org-eva-submit "org-eva" nil t)

(eval-after-load "org-eva"
  '(let ((eva-conf "~/develop/pmade/privaterc/emacs/secrets.el"))
     (when (file-exists-p eva-conf) (load-file eva-conf))))

;; Org Basecamp
(require 'org-basecamp)
(setq org-basecamp-mark-todo nil)

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
 org-M-RET-may-split-line t
 org-time-clocksum-format "%02d:%02d"
 org-clock-into-drawer "CLOCK"
 org-log-into-drawer "LOGBOOK"
 org-tags-exclude-from-inheritance nil
 org-completion-use-ido t
 org-goto-interface 'outline-path-completion
 org-outline-path-complete-in-steps t

 ;; Showing context
 org-show-hierarchy-above '((default . t))
 org-show-following-heading '((default . t))
 org-show-siblings '((default . t))
 org-show-entry-below '((default . t))

 ;; Faces
 org-todo-keyword-faces
   '(("NEXT"    . pmade-org-next-face)
     ("PENDING" . pmade-org-pending-face)
     ("READING" . pmade-org-reading-face)))

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
   org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f")
   org-export-html-auto-postamble nil
   org-export-with-sub-superscripts nil
   org-export-with-emphasize nil
   org-icalendar-include-todo nil
   org-icalendar-store-UID t
   org-export-html-style-default ""
   org-export-html-style-extra ""
   org-export-html-style nil))

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
(defun pmade:org-publish-html-or-image (plist filename pub-dir)
  (if (string-match "\\.org$" filename)
      (org-publish-org-to-html plist filename pub-dir)
    (org-publish-attachment plist filename pub-dir)))

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
