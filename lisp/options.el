;;; options.el -- Emacs settings not tied to any one mode.

;; Personal information
(setq user-full-name "Peter Jones"
      user-mail-address "pjones@pmade.com")

;; Things to turn on for all modes
(add-hook 'fundamental-mode-hook 'font-lock-mode)
(add-hook 'fundamental-mode-hook 'auto-fill-mode)
(add-hook 'fundamental-mode-hook 'flyspell-mode)
(add-hook 'fundamental-mode-hook 'column-number-mode)
(add-hook 'fundamental-mode-hook 'show-paren-mode)

;; Variables defined in Emacs' C source
(setq inhibit-startup-message t          ; I've seen it already
      initial-scratch-message nil        ; Ditto
      mouse-yank-at-point t              ; Don't move point when mouse pasting
      ring-bell-function (lambda ())     ; Kill those damn bells
      visible-bell nil                   ; No visual bell
      enable-recursive-minibuffers t     ; Allow multiple mini buffers
      echo-keystrokes 0.1                ; Show unfinished keystrokes early
      disabled-command-function nil      ; Disable novice user protection
      truncate-partial-width-windows nil ; When windows don't fill the frame
      mark-even-if-inactive t            ; Use the mark without a region
      frame-title-format "%b")           ; Set the contents of the frame title

;; Default variables that become buffer/frame local.
(setq-default cursor-in-non-selected-windows 'hbar  ; Self-explanatory
              indicate-buffer-boundaries 'left      ; Fringe stuff
              indicate-empty-lines t                ; Ditto
              require-final-newline t               ; Always end files with \n
              indent-tabs-mode nil                  ; Don't use tabs
              truncate-lines)                       ; Don't wrap lines

;; Settings not worth their own file in the modes directory:
(setq epa-file-encrypt-to "D4426FFA"    ; Default GPG key to use
      custom-file "~/.emacs.d/pjones/lisp/custom.el") ; To keep Emacs happy

;; Frame setup
(setq default-frame-alist
      '((cursor-type  . bar)
        (cursor-color . "yellow")))

(defun pjones:configure-new-frame (&optional frame)
  "Hook to configure a new frame."
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (if (fboundp mode) (funcall mode -1)))
  (blink-cursor-mode)
  (require 'fringe)
  (fringe-mode 10))

(add-hook 'after-init-hook 'pjones:configure-new-frame)
(add-hook 'after-make-frame-functions 'pjones:configure-new-frame)

;; Libraries used throughout my Emacs session
(require 'saveplace)                    ; Saves your location in files
(require 'dired-x)                      ; Extra features for dired-mode
(require 'org-install)                  ; Load external org-mode
