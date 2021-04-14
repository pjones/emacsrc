;;; completion.el -- Configuration for completion, abbreviations, and shortcuts.
;;
;;; Commentary:
;;
;;; Code:

(require 'company)
(require 'company-try-hard)
(require 'yasnippet)

;; I don't want to type "yes".
(defalias 'yes-or-no-p 'y-or-n-p)

;; What to do with the tab key.
(defun pjones:indent-or-complete (&optional arg)
  "Indent or complete.
If the character before point is a space character then indent the
current line.  Otherwise run the completion command.  ARG is passed to
`indent-for-tab-command'."
  (interactive "P")
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (let ((tab-always-indent t)
          (n (save-excursion (beginning-of-line) (point))))
      (if (or (bolp) (looking-back "\\s-" n)) (indent-for-tab-command arg)
        (if (yas-maybe-expand-abbrev-key-filter t)
            (yas-expand)
          (company-try-hard))))))

;; In buffer completion:
(add-hook 'after-init-hook 'global-company-mode)

;; Settings for older code still using hippie expand:
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        try-expand-line))

;;; completion.el ends here
