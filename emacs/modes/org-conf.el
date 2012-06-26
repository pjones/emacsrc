;;; org-conf.el -- Settings for org-mode.
(eval-when-compile
  (require 'saveplace)
  (require 'org-install)
  (require 'org)
  (require 'org-clock)
  (require 'org-html))

;; General Org Settings
(setq org-log-done t
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
      org-clock-clocked-in-display 'frame-title
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

      ;; Following Links
      org-link-frame-setup
      '((vm   . vm-visit-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl   . wl-other-frame))

      ;; Emphasis
      org-emphasis-alist
      `(("_" bold "<b>" "</b>")
        ("*" italic "<i>" "</i>")
        ("=" underline "<span style=\"text-decoration:underline;\">" "</span>")
        ("`" org-code "<code>" "</code>" verbatim)
        ("~" org-verbatim "<code>" "</code>" verbatim)))

;; Need to tell org-mode to update org-emphasis-alist
(org-set-emph-re 'org-emphasis-alist org-emphasis-alist)

(defun pjones:org-mode-hook ()
  ;; Extra Bindings
  (org-defkey org-mode-map "\C-\M-f"   'org-metaright)
  (org-defkey org-mode-map "\C-\M-b"   'org-metaleft)
  (org-defkey org-mode-map "\C-\M-S-f" 'org-shiftmetaright)
  (org-defkey org-mode-map "\C-\M-S-b" 'org-shiftmetaleft)
  (org-defkey org-mode-map "\C-\M-p"   'org-metaup)
  (org-defkey org-mode-map "\C-\M-n"   'org-metadown)
  (org-defkey org-mode-map "\C-c;"     'flyspell-auto-correct-previous-word)

  (org-defkey org-mode-map "\C-c0"               'pjones:org-hide-all)
  (org-defkey org-mode-map "\C-c1"               'pjones:org-hide-others)
  (org-defkey org-mode-map "\C-j"                'pjones:org-list-append)
  (org-defkey org-mode-map [(meta return)]       'pjones:org-list-append)
  (org-defkey org-mode-map [(shift meta return)] 'pjones:org-list-append-with-checkbox)

  ;; Buffer Settings
  (setq save-place nil)

  ;; Exporting
  (setq org-export-html-link-org-files-as-html nil
        org-export-with-sub-superscripts nil
        org-export-with-emphasize nil
        org-export-html-style-default ""
        org-export-html-style-extra ""
        org-export-html-style nil))

(add-hook 'org-mode-hook 'pjones:org-mode-hook)

(defun pjones:org-hide-others ()
  "Close all headings except the heading at point."
  (interactive)
  (org-overview)
  (org-reveal))

(defun pjones:org-hide-all ()
  "Close all headings, move to bob."
  (interactive)
  (goto-char (point-min))
  (org-cycle '(4)))

(defun pjones:org-list-append (&optional checkbox)
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
      (forward-line -1))
    (org-indent-line-function)
    (insert (concat "-" (if checkbox " [ ] " " ")))
    (save-excursion
      (org-back-to-heading)
      (org-cycle-hide-drawers 'subtree))))

(defun pjones:org-list-append-with-checkbox ()
  "Calls `pjones:org-list-append' with checkbox set."
  (interactive)
  (pjones:org-list-append t))

(defun pjones:org-time-diff (t1 t2)
  "Returns the difference between t1 and t2.  Expects that times
are formatted as HH:MM and returns them in that format"
  (org-minutes-to-hh:mm-string
   (- (org-hh:mm-string-to-minutes t1)
      (org-hh:mm-string-to-minutes t2))))

;; Silence a compiler warning
(declare-function org-bookmark-jump-unhide "org")
