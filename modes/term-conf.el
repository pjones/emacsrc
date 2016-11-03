;;; term-conf.el -- Settings for term-mode.

(eval-when-compile
  (require 'term))

;; Settings for term-mode:
(custom-set-variables
 '(term-input-autoexpand t)
 '(term-input-ignoredups t)
 '(term-scroll-to-bottom-on-output nil))

(defvar pjones:term-last-position nil
  "Last position of point before switching term sub-modes.")

(defun pjones:term-line-mode ()
  "Go to term-line-mode."
  (interactive)
  (setq pjones:term-last-position (point))
  (term-line-mode))

(defun pjones:term-char-mode ()
  "Return to raw/character mode."
  (interactive)
  (goto-char (or pjones:term-last-position (point-max)))
  (term-char-mode))

(defun pjones:remove-dead-term (&rest args)
  "Clean up after a dead terminal."
  (kill-buffer))

(defun pjones:term-mode-hook ()
  "Hook run after a terminal starts"

  ;; Some variables that term-mode doesn't initialize but uses :(
  (setq term-ansi-at-dir default-directory
        term-ansi-at-host (system-name)
        term-ansi-at-user (user-real-login-name))

  ;; Variables for configuration:
  (setq term-prompt-regexp "❯ "
        mode-line-format '("  %b    [term" mode-line-process "]"))

  ;; Keep things Emacs-like:
  (term-set-escape-char ?\C-x)

  (let ((map term-raw-escape-map))
    (define-key map (kbd "C-c") nil)
    (define-key map (kbd "C-j") nil)
    (define-key map (kbd "C-k") nil)
    (define-key map (kbd "C-q") nil)
    (define-key map (kbd "C-x") nil))

  (let ((map term-raw-map))
    (define-key map (kbd "C-c") nil)
    (define-key map (kbd "C-c C-l") 'pjones:term-line-mode)
    (define-key map (kbd "C-c C-r") 'pjones:term-char-mode)
    (define-key map (kbd "C-y")     'term-paste)))

(advice-add 'pjones:remove-dead-term :after 'term-handle-exit)
(add-hook 'term-mode-hook 'pjones:term-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
