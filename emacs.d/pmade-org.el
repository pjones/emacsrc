;; This file contains my customizations for the awesome Org-Mode

;; Org Invoice
(autoload 'org-invoice-report "org-invoice" nil t)
(autoload 'org-dblock-write:invoice "org-invoice" nil t)
(autoload 'org-eva-submit "org-eva" nil t)

(eval-after-load "org-eva"
  (let ((eva-conf "~/.comm-sync/etc/eva.el"))
    (when (file-exists-p eva-conf) (load-file eva-conf))))

(setq
 ;; General Org Settings
 org-log-done t
 org-reverse-note-order t
 org-deadline-warning-days 14
 org-hide-leading-stars t
 org-use-fast-todo-selection t
 org-use-fast-tag-selection 'auto
 org-fast-tag-selection-single-key t
 org-special-ctrl-a/e t
 org-special-ctrl-k t
 org-M-RET-may-split-line nil
 org-time-clocksum-format "%02d:%02d"
 org-completion-use-ido t
 org-tags-exclude-from-inheritance nil

 ;; The notes file and options
 org-directory "~/Documents/pmade/pmade-inc/planning"
 org-default-notes-file (concat org-directory "/general/business.org")
 pmade-org-active-clients (concat (expand-file-name org-directory) "/clients/active")
 pmade-org-general-files (concat (expand-file-name org-directory) "/general")

 ;; Agenda Files and Agenda Settings
 org-agenda-window-setup 'current-window
 org-agenda-restore-windows-after-quit nil
 org-stuck-projects '("LEVEL=2+project/-DONE" ("NEXT" "PENDING") ("single") "")
 org-agenda-ndays 1
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-show-all-dates t
 org-agenda-start-on-weekday 1
 org-agenda-todo-ignore-with-date t
 org-agenda-files 
   (append
    (directory-files pmade-org-active-clients t "\\.org$")
    (directory-files pmade-org-general-files  t "\\.org$"))

 ;; Custom Agenda Views
 org-agenda-custom-commands
 '(("d" "Daily Agenda"
    ((agenda ""
             ((org-agenda-todo-keyword-format "")
              (org-agenda-remove-tags t)))
     (tags "LEVEL=2+affirmation"
           ((org-agenda-remove-tags t)
            (org-agenda-prefix-format "")
           (org-agenda-todo-keyword-format "")))
     (todo "NEXT" 
           ((org-agenda-sorting-strategy '(tag-up))
            (org-agenda-show-inherited-tags nil)
            (org-agenda-todo-keyword-format "")))
     (todo "PENDING"
           ((org-agenda-todo-keyword-format "")))
     (stuck ""
            ((org-agenda-remove-tags t))))))
 
 ;; Faces
 org-todo-keyword-faces
   '(("NEXT"    . pmade-org-next-face)
     ("PENDING" . pmade-org-pending-face)
     ("READING" . pmade-org-reading-face)))

(add-hook 'org-mode-hook
  (lambda ()
    ;; Extra Bindings
    (define-key org-mode-map "\C-ci"     'org-invoice-report)
    (define-key org-mode-map "\C-j"      'org-meta-return)
    (define-key org-mode-map "\C-\M-f"   'org-metaright)
    (define-key org-mode-map "\C-\M-b"   'org-metaleft)
    (define-key org-mode-map "\C-\M-S-f" 'org-shiftmetaright)
    (define-key org-mode-map "\C-\M-S-b" 'org-shiftmetaleft)
    (define-key org-mode-map "\C-\M-p"   'org-metaup)
    (define-key org-mode-map "\C-\M-n"   'org-metadown)

    ;; Buffer Settings
    (setq save-place nil)
    
    ;; Exporting
    (setq org-export-html-auto-postamble nil
          org-export-with-sub-superscripts nil
          org-export-with-emphasize nil
          org-icalendar-include-todo nil
          org-icalendar-store-UID t
          org-export-html-style-default ""
          org-export-html-style-extra ""
          org-export-html-style (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" pmade-print-css "\"/>"))))

(add-hook 'org-agenda-mode-hook 
  (lambda () 
    ;; Use line highlighting in the Org Agenda
    (hl-line-mode 1)
    
    ;; Keys
    (local-set-key "\C-x\C-w" 'pmade:org-write-agenda)))

(defun pmade:org-write-agenda ()
  "Write the agenda buffer to a file, and send to pmade.com."
  (interactive)
  (org-write-agenda "~/agenda.html")
  (shell-command "sed -E 's/T:([0-9+-]+)/T:<a href=\"tel:\\1\">\\1<\\/a>/' < ~/agenda.html | ssh -q pmade.com 'cat > /opt/sites/pmade.com/www/private/agenda.html'")
  (delete-file "~/agenda.html"))

(defun pmade:org-remove-redundant-heading-markers ()
  "Called from an export buffer, removes leading stars so that
the first heading in the export has only one star."
  (let ((reduce-by 0)
        (remove-regex "^"))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (search-forward-regexp "^\\*")
        (beginning-of-line)
        (setq reduce-by (- (org-outline-level) 1))
        (when (> reduce-by 0)
          (setq remove-regex (concat remove-regex (regexp-quote (make-string reduce-by ?*))))
          (while (re-search-forward remove-regex nil t)
            (replace-match "" nil nil)
            (forward-line)))))))

(add-hook 'org-export-preprocess-hook 'pmade:org-remove-redundant-heading-markers)
