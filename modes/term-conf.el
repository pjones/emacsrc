;;; term-conf.el -- Settings for term-mode.
;;; Commentary:
;;; Code:
(require 'term)
(require 'projectile)
(require 'evil)

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

;; FIXME: this doesn't really work. Write a new version based off the
;; same idea in EXWM.  Also, this isn't working with Evil (or maybe
;; it's default Emacs).  I'm getting an error about the terminal being
;; read only.
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
        term-line-mode-buffer-read-only t)

  ;; Keep things Emacs-like:
  (term-set-escape-char ?\C-x)
  (toggle-truncate-lines -1)

  ;; But use Evil too:
  (add-hook 'evil-insert-state-entry-hook #'pjones:term-char-mode)
  (add-hook 'evil-insert-state-exit-hook  #'pjones:term-line-mode)

  (evil-define-key 'insert 'term-raw-map
    ;; NOTE: Make C-o work like it should.  I suppose this might break
    ;; some terminal applications but I don't care:
    (kbd "C-o")     #'evil-execute-in-normal-state
    (kbd "C-c C-d") #'pjones:term-insert-directory)

  (evil-define-key 'normal 'term-mode-map
    (kbd "C-c C-k")  #'evil-insert
    (kbd "<return>") #'evil-insert)

  (let ((map term-raw-map))
    ;; Sending special keys to the terminal:
    (define-key map (kbd "C-c")     nil)
    (define-key map (kbd "C-c C-c") (pjones:term-send-char [?\C-c]))
    (define-key map (kbd "C-c C-d") #'pjones:term-insert-directory)
    (define-key map (kbd "C-c C-e") (pjones:term-send-char [?\C-x ?\C-e]))
    (define-key map (kbd "C-c C-o") (pjones:term-send-char [?\C-o]))
    (define-key map (kbd "C-c C-x") (pjones:term-send-char [?\C-x]))
    (define-key map (kbd "C-c C-z") (pjones:term-send-char [?\C-z]))
    (define-key map (kbd "C-c M-x") (pjones:term-send-char [escape ?x]))
    (define-key map (kbd "C-c C-q") #'pjones:term-quoted-insert)

    ;; Other functions:
    (define-key map (kbd "C-c C-k") #'evil-normal-state)
    (define-key map (kbd "M-x")     nil)))

(advice-add 'term-handle-exit :after 'pjones:remove-dead-term)
(add-hook 'term-mode-hook 'pjones:term-mode-hook)

;;; term-conf.el ends here
