;;; options.el -- Emacs settings not tied to any one mode.
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'server))

;; Personal information
(setq user-full-name "Peter Jones"
      user-mail-address "pjones@devalot.com")

;; Things to turn on for all modes
(defun pjones:add-basic-mode-hook (mode-hook)
  "A helper function to add minor modes to another mode's hooks."
  (add-hook mode-hook 'font-lock-mode)
  (add-hook mode-hook 'auto-fill-mode)
  (add-hook mode-hook 'flyspell-mode)
  (add-hook mode-hook 'line-number-mode)
  (add-hook mode-hook 'column-number-mode))

(pjones:add-basic-mode-hook 'text-mode-hook)
(pjones:add-basic-mode-hook 'prog-mode-hook)

;; Variables defined in Emacs' C source
(setq max-lisp-eval-depth 1200           ; My theme is too deep
      inhibit-startup-message t          ; I've seen it already
      initial-scratch-message nil        ; Ditto
      make-backup-files nil              ; Don't make backup files
      mouse-yank-at-point t              ; Don't move point when mouse pasting
      ring-bell-function (lambda ())     ; Kill those damn bells
      visible-bell nil                   ; No visual bell
      enable-recursive-minibuffers t     ; Allow multiple mini buffers
      echo-keystrokes 0.1                ; Show unfinished keystrokes early
      disabled-command-function nil      ; Disable novice user protection
      truncate-partial-width-windows nil ; When windows don't fill the frame
      mark-even-if-inactive t            ; Use the mark without a region
      next-line-add-newlines t           ; Create new lines by moving down
      inhibit-eol-conversion t)          ; Force manual line conversions.

;; Default variables that become buffer/frame local.
(setq-default cursor-in-non-selected-windows 'hbar  ; Self-explanatory
              indicate-buffer-boundaries 'left      ; Fringe stuff
              indicate-empty-lines t                ; Ditto
              require-final-newline t               ; Always end files with \n
              indent-tabs-mode nil                  ; Don't use tabs
              truncate-lines t)                     ; Don't wrap lines

;; Settings not worth their own file in the modes directory:
(setq epa-file-encrypt-to "204284CB"    ; Default GPG key to use
      custom-file (concat user-emacs-directory "custom.el" )) ; To keep Emacs happy

(defun pjones:frame-title-file-name ()
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
  "Hook to configure a new frame."
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (if (fboundp mode) (funcall mode -1)))
  (blink-cursor-mode)
  (require 'fringe)
  (fringe-mode 10)
  ;; Reset the `title' frame parameter as it may have been set on the
  ;; command line when the frame name was set.
  (set-frame-parameter frame 'title nil)
  (set-frame-parameter frame 'name  nil))

(add-to-list 'default-frame-alist '(cursor-type  . bar))
(add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono-9"))
(setq frame-title-format '(:eval (pjones:frame-title-file-name)))

(defun pjones:find-file-hook ()
  "Hook called after a file is loaded into a buffer."
  ;; Encrypted files should start as read-only:
  (require 'epa-hook)
  (when (and buffer-file-name
             (string-match epa-file-name-regexp buffer-file-name))
    (read-only-mode 1)))

;; Add all of the hooks from above.
(add-hook 'after-init-hook 'pjones:configure-new-frame)
(add-hook 'after-make-frame-functions 'pjones:configure-new-frame)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'find-file-hook 'pjones:find-file-hook)

;; Control how Emacs makes buffer names unique.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(provide 'options)
;;; options.el ends here
