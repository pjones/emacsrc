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
  (let ((char (if (string= comment-start "-- ") ?- ?#))
        (end comment-end)
        (col (current-column))
        (start (cond
                ((string= comment-start "# ")  "#")
                ((string= comment-start "-- ") "-")
                (t comment-start))))
    (insert start)
    (insert-char char (- 80 (length start) (length end) col))
    (insert end)
    (if without-newline (beginning-of-line)
      (call-interactively (key-binding (kbd "RET"))))))

(defun pjones:prog-mode-hook ()
  "Settings and bindings for programming modes."
  (setq save-place t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (local-set-key (kbd "C-c TAB") 'pjones:comment-bar)
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (show-paren-mode)
  (whitespace-mode)
  (auto-fill-mode)
  (flyspell-prog-mode)
  (electric-pair-mode)
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)
  (font-lock-add-keywords nil '(("\\<\\(FIXME:\\|TODO:\\|NOTE:\\)"
                                 1 'pjones:fixme-face t))))

(defun pjones:add-programming-hook (mode-hook)
  (add-hook mode-hook 'pjones:prog-mode-hook))
