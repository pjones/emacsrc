;;; options.el -- Emacs settings not tied to any one mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'server)
(require 'epa)
(require 'doom-modeline)
(require 'auth-source-pass)
(require 'flycheck)

(eval-when-compile
  (require 'gnus))

;; Personal information
(setq user-full-name "Peter Jones"
      user-mail-address "pjones@devalot.com")

;; Things to turn on for all modes
(defun pjones:add-basic-mode-hook (mode-hook)
  "A helper function to add minor modes to another mode's MODE-HOOK."
  (add-hook mode-hook 'font-lock-mode)
  (add-hook mode-hook 'auto-fill-mode)
  (add-hook mode-hook 'flyspell-mode)
  (add-hook mode-hook 'line-number-mode)
  (add-hook mode-hook 'column-number-mode))

(pjones:add-basic-mode-hook 'text-mode-hook)
(pjones:add-basic-mode-hook 'prog-mode-hook)

;; Variables defined in Emacs' C source
(custom-set-variables
 '(inhibit-startup-message t)
 '(initial-scratch-message nil)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(visible-bell nil)
 '(enable-recursive-minibuffers t)
 '(echo-keystrokes 0.1)
 '(disabled-command-function nil)
 '(truncate-partial-width-windows nil)
 '(mark-even-if-inactive t)
 '(inhibit-eol-conversion t)
 '(hscroll-margin 2)
 '(hscroll-step 1)
 '(scroll-conservatively 101)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position nil)
 '(auto-window-vscroll nil)
 '(x-underline-at-descent-line t))

;; Default variables that become buffer/frame local.
(setq-default
 cursor-in-non-selected-windows 'hollow ; Self-explanatory
 indicate-buffer-boundaries 'left       ; Fringe stuff
 indicate-empty-lines t                 ; Ditto
 require-final-newline t                ; Always end files with \n
 indent-tabs-mode nil                   ; Don't use tabs
 truncate-lines t                       ; Don't wrap lines
 display-line-numbers-width 3)          ; Faster default.

;; Settings not worth their own file in the modes directory:
(custom-set-variables
 '(epa-file-encrypt-to "204284CB")    ; Default GPG key to use
 '(auth-sources '(password-store))    ; Use pass(1) for passwords.
 '(compilation-scroll-output 'first-error)
 `(custom-file "/dev/null")) ; Don't load transient config!

;; Settings that must be set before a mode file is loaded:
(setq
 gnus-home-directory "~/.cache/emacs"
 gnus-directory "~/.cache/emacs/gnus")

;; Settings from simple.el:
(custom-set-variables
 '(async-shell-command-buffer 'new-buffer)
 '(kill-do-not-save-duplicates t)
 '(set-mark-command-repeat-pop t)
 '(next-line-add-newlines nil))

(defun pjones:frame-title-file-name ()
  "How to format frame titles."
  (let* ((home (expand-file-name "~"))
         (end (length home))
         (start (and buffer-file-name (substring buffer-file-name 0 end)))
         (under-home (and start (string= home start)))
         (server (and server-name (concat " [" server-name "]")))
         (file (cond (under-home
                      (concat "~/" (file-relative-name buffer-file-name "~")))
                     (buffer-file-name buffer-file-name)
                     (dired-directory
                      (if (listp dired-directory) (car dired-directory) dired-directory)))))
    (concat "Emacs: " (buffer-name) " " file server)))

(defun pjones:configure-new-frame (&optional frame)
  "Hook to configure new frame FRAME."
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (if (and (fboundp mode) (symbol-value mode))
        (funcall mode -1)))
  (blink-cursor-mode)
  (require 'fringe)
  (fringe-mode 10)
  (if (fboundp 'doom-modeline-mode) (doom-modeline-mode))
  ;; Reset the `title' frame parameter as it may have been set on the
  ;; command line when the frame name was set.
  (set-frame-parameter frame 'title nil)
  (set-frame-parameter frame 'name  nil))

(add-to-list 'default-frame-alist '(cursor-type  . hbar))
(add-to-list 'default-frame-alist '(font . "Hermit:pixelsize=14:weight=normal"))
(setq frame-title-format '(:eval (pjones:frame-title-file-name)))

(defun pjones:find-file-hook ()
  "Hook called after a file is loaded into a buffer."
  ;; Encrypted files should start as read-only:
  (require 'epa-hook)
  (when (and buffer-file-name
             (string-match epa-file-name-regexp buffer-file-name))
    (read-only-mode 1)))

;; Add all of the hooks from above.
(add-hook 'after-make-frame-functions #'pjones:configure-new-frame)
(add-hook 'after-init-hook #'pjones:configure-new-frame)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'find-file-hook 'pjones:find-file-hook)

;; Control how Emacs makes buffer names unique.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(provide 'options)
;;; options.el ends here
