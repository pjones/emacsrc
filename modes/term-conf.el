;;; term-conf.el -- Settings for term-mode.
;;; Commentary:
;;; Code:
(require 'term)

;; Settings for term-mode:
(custom-set-variables
 '(term-input-autoexpand nil)
 '(term-input-ignoredups nil)
 '(term-scroll-to-bottom-on-output nil)
 '(term-scroll-show-maximum-output nil)

 ;; WARNING: setting this to `t' triggers a bug where the shell will
 ;; barf text all over the screen.
 '(term-suppress-hard-newline nil))

(defun pjones:term-line-mode ()
  "Go to term-line-mode."
  (interactive)
  (term-line-mode))

(defun pjones:term-char-mode ()
  "Return to raw/character mode."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (term-char-mode)
      (goto-char (process-mark proc)))))

(defmacro pjones:term-send-char (chars)
  "Create a function to send CHARS to the terminal."
  `(lambda ()
     (interactive)
     (seq-doseq (char ,chars)
       (term-send-raw-string (make-string 1 char)))))

(defun pjones:term-insert-directory ()
  "Prompt for a directory and insert it into the term buffer."
  (interactive)
  (let ((dir (read-directory-name "Dir: ")))
    (term-send-raw-string dir)))

(defun pjones:remove-dead-term (&rest args)
  "Clean up after a dead terminal.
Ignores ARGS."
  (quit-window t))

(defun pjones:term-mode-hook ()
  "Hook run after starting a new terminal."
  ;; Some variables that term-mode doesn't initialize but uses :(
  (setq term-ansi-at-dir default-directory
        term-ansi-at-host (system-name)
        term-ansi-at-user (user-real-login-name)
        term-line-mode-buffer-read-only t)

  ;; Keep things Emacs-like:
  (term-set-escape-char ?\C-x)
  (toggle-truncate-lines -1))

(advice-add 'term-handle-exit :after 'pjones:remove-dead-term)
(add-hook 'term-mode-hook 'pjones:term-mode-hook)

;;; term-conf.el ends here
