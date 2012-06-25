;;; simple-conf.el -- Functions and settings for simple.el (mostly for
;;; prog-mode since it's defined in simple.el).
(eval-when-compile (require 'saveplace))

(defun pjones:comment-bar (&optional without-newline)
  "Create a comment bar based on the current mode."
  (interactive "P")
  (insert (concat comment-start " "))
  (insert-char ?# (- 80 (length comment-start) (current-column)))
  (if without-newline (beginning-of-line) (newline))
  (indent-according-to-mode))

(defun pjones:prog-mode-hook ()
  (setq save-place t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (local-set-key (kbd "C-c TAB") 'pjones:comment-bar)
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'pjones:prog-mode-hook)
