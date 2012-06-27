;;; code.El -- Settings and functions for programming modes
(eval-when-compile (require 'saveplace))

;; Create some faces
(defface pjones:fixme-face
  '((t (:inherit 'font-lock-warning-face)))
  "Face to style FIXME and TODO with."
  :group 'faces)

(defun pjones:comment-bar (&optional without-newline)
  "Create a comment bar based on the current mode."
  (interactive "P")
  (let ((start (if (string= comment-start "# ") "#" comment-start))
        (end comment-end))
    (insert start)
    (insert-char ?# (- 80 (length start) (length end) (current-column)))
    (insert end)
    (if without-newline (beginning-of-line) (newline))
    (indent-according-to-mode)))

(defun pjones:prog-mode-hook ()
  "Settings and bindings for programming modes."
  (setq save-place t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (local-set-key (kbd "C-c TAB") 'pjones:comment-bar)
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (font-lock-add-keywords nil '(("\\<\\(FIXME:\\|TODO:\\|NOTE:\\)"
                                 1 pjones:fixme-face t))))

(defun pjones:add-programming-hook (mode-hook)
  (add-hook mode-hook 'show-paren-mode)
  (add-hook mode-hook 'whitespace-mode)
  (add-hook mode-hook 'auto-fill-mode)
  (add-hook mode-hook 'flyspell-prog-mode)
  (add-hook mode-hook 'electric-pair-mode)
  (add-hook mode-hook 'pjones:prog-mode-hook)
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))
