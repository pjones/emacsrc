;;; completion.el -- Configuration for completion, abbreviations, and shortcuts.
;;
;;; Commentary:
;;
;;; Code:

(require 'company)
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
  (if (region-active-p) (indent-region (region-beginning) (region-end))
    (let ((tab-always-indent t)
          (max-backtrack (save-excursion (beginning-of-line) (point))))
      (cond
       ;; Force indenting the entire expression:
       (arg
        (indent-for-tab-command arg))
       ;; Maybe indent the line or match indentation:
       ((or (bolp) (looking-back "\\s-" max-backtrack))
          (if (looking-at-p "\\s-*$")
              ;; Blank line, copy indentation from above:
              (indent-to
               (save-excursion
                 (forward-line -1)
                 (back-to-indentation)
                 (current-indentation)))
            ;; Indent the current line:
            (indent-for-tab-command arg)))
       ;; Try snippet completion:
       ((yas-maybe-expand-abbrev-key-filter t)
        (yas-expand))
       ;; Fallback to completion:
       (t
        (company-complete))))))

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
