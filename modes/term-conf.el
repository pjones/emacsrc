;;; term-conf.el -- Settings for term-mode.
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'term))

;; Dependencies:
(require 'projectile)
(require 'elscreen)

;; Remove some warnings:
(defvar pjones:z-map nil "Defined in keys.el.")

;; Settings for term-mode:
(custom-set-variables
 '(term-input-autoexpand nil)
 '(term-input-ignoredups nil)
 '(term-scroll-to-bottom-on-output nil)
 '(term-scroll-show-maximum-output nil)

 ;; WARNING: setting this to `t' triggers a bug where the shell will
 ;; barf text all over the screen.
 '(term-suppress-hard-newline nil))

(defun pjones:term-rename ()
  "Rename a terminal using the current project name."
  (interactive)
  (let ((suffix (if (projectile-project-p) (projectile-project-name)
                  (elscreen-get-screen-nickname
                   (elscreen-get-current-screen)))))
    (rename-buffer (generate-new-buffer-name
                     (concat "term:" suffix)))))

(defun pjones:term-line-mode ()
  "Go to term-line-mode."
  (interactive)
  (term-line-mode))

(defun pjones:term-char-mode ()
  "Return to raw/character mode."
  (interactive)
  (term-char-mode)
  (let ((proc (get-buffer-process (current-buffer))))
    (goto-char (process-mark proc))))

(defun pjones:term-send-control-c ()
  "Send ^c to the terminal."
  (interactive)
  (term-send-raw-string (make-string 1 ?\C-c)))

(defun pjones:remove-dead-term (&rest args)
  "Clean up after a dead terminal.

Ignores ARGS."
  (let ((buffer (current-buffer)))
    (kill-buffer buffer)))

;; FIXME: this doesn't really work. Write a new version based off the
;; same idea in EXWM.
(defun pjones:term-quoted-insert (count)
  "Read next input character and send it directly to the terminal.

Sends the next key COUNT times."
  (interactive "*p")
  (let ((char (read-char)))
    (term-send-raw-string (make-string count char))))

(defun pjones:term-mode-hook ()
  "Hook run after starting a new terminal."

  ;; Some variables that term-mode doesn't initialize but uses :(
  (setq term-ansi-at-dir default-directory
        term-ansi-at-host (system-name)
        term-ansi-at-user (user-real-login-name)
        term-prompt-regexp "^[^❯]+❯ *")

  ;; Clean mode-line:
  (setq mode-line-format
        '("" mode-line-front-space
          (:eval (pjones:mode-line-status))
          "   " mode-line-buffer-identification
          "     [term" mode-line-process "]"))

  ;; Keep things Emacs-like:
  (term-set-escape-char ?\C-x)
  (toggle-truncate-lines -1)

  (let ((map term-raw-escape-map))
    (define-key map (kbd "C-c") nil)
    (define-key map (kbd "C-j") nil)
    (define-key map (kbd "C-k") nil)
    (define-key map (kbd "C-q") nil)
    (define-key map (kbd "C-x") nil))

  (let ((map term-mode-map))
    (define-key map (kbd "C-c") nil)
    (define-key map (kbd "C-c C-k") 'pjones:term-char-mode)
    (define-key map (kbd "RET")     'pjones:term-char-mode))

  (let ((map term-raw-map))
    (define-key map (kbd "C-c") nil)
    (define-key map (kbd "C-c C-k") 'pjones:term-line-mode)
    (define-key map (kbd "C-c C-c") 'pjones:term-send-control-c)
    (define-key map (kbd "C-c C-r") 'pjones:term-rename)
    (define-key map (kbd "C-c M-x") 'helm-M-x)
    (define-key map (kbd "C-c C-q") 'pjones:term-quoted-insert)
    (define-key map (kbd "C-c C-u") 'universal-argument)
    (define-key map (kbd "C-y")     'term-paste)
    (define-key map (kbd "C-z")     pjones:z-map)))

(advice-add 'term-handle-exit :after 'pjones:remove-dead-term)
(add-hook 'term-mode-hook 'pjones:term-mode-hook)

;;; term-conf.el ends here
