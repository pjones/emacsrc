;; This file contains my customizations for the awesome Org-Mode

;; Attempt to load org-mode from a local installation, but if that
;; fails, ignore the error since an older org-mode comes with emacs.
(require 'org-install nil t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq
 org-log-done t
 org-reverse-note-order t
 org-agenda-ndays 14
 org-deadline-warning-days 14
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-show-all-dates t
 org-agenda-start-on-weekday 1
 org-hide-leading-stars t
 org-use-fast-todo-selection t
 org-special-ctrl-a/e t
 org-special-ctrl-k t
 org-M-RET-may-split-line nil
 org-time-clocksum-format "%02d:%02d"
 org-agenda-window-setup 'current-window
 org-export-html-auto-postamble nil
 org-export-with-sub-superscripts nil
 org-export-with-emphasize nil
 org-icalendar-include-todo nil
 org-icalendar-store-UID t)

;; The notes file and options
(setq
 org-directory "~/Documents/pmade/pmade-inc/planning"
 org-default-notes-file (concat org-directory "/general/business.org"))

;; Agenda Files and Agenda Settings
(setq org-agenda-files
      (append
       (directory-files (concat (expand-file-name org-directory) "/clients/active") t "\\.org$")
       (directory-files (concat (expand-file-name org-directory) "/general") t "\\.org$")))
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; Exporting
(setq
 org-export-html-style-default ""
 org-export-html-style-extra ""
 org-export-html-style (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" pmade-print-css "\"/>"))

(defun pmade:org-remove-redundant-heading-markers ()
  "Called from an export buffer, removes leading stars so that the first heading in the export has only one star."
  (let ((reduce-by 0)
        (remove-regex "^"))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (search-forward-regexp "^\\*")
        (beginning-of-line)
        (when (looking-at "^\\(\\*+\\)[ \t]+")
          (setq reduce-by (- (match-end 1) (point))))
        (when (not (= 0 reduce-by))
          (setq remove-regex (concat remove-regex (regexp-quote (make-string reduce-by ?*))))
          (forward-line 1) ; leave the top heading alone (org must ignore it as well)
          (while (re-search-forward remove-regex nil t)
            (replace-match "" nil nil)
            (forward-line 1)))))))

(add-hook 'org-export-preprocess-hook 'pmade:org-remove-redundant-heading-markers)
