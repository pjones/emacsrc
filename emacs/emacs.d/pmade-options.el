;;; This file contains general settings and options for emacs, to make
;;; it work more like what I expect

;; Personal Settings
(setq
 user-full-name "Peter Jones"
 user-mail-address "pjones@pmade.com")

;; Interface Options (remove some interface elements, such as the toolbar, scroll bar, and menu bar.
;; Frame setup
(setq default-frame-alist 
 '((cursor-type  . bar)
   (cursor-color . "yellow")))

(defun pmade-configure-new-frame (frame)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (setq-default 
   cursor-in-non-selected-windows 'hbar
   indicate-buffer-boundaries 'left
   indicate-empty-lines t)
  (blink-cursor-mode)
  (require 'fringe)
  (fringe-mode 10))

(add-hook 'after-make-frame-functions 'pmade-configure-new-frame)

;; General options
(setq
 inhibit-startup-message t        ; I've seen it already
 initial-scratch-message nil      ; Ditto
 ring-bell-function (lambda ())   ; kill those damn bells
 visible-bell nil                 ; no visual bell
 column-number-mode t             ; display column number in mode line
 enable-recursive-minibuffers t   ; allow multiple minibuffers
 echo-keystrokes 0.1              ; show unfinished keystrokes early
 disabled-command-hook nil)       ; Enable commands disabled by default for novice users

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; Font lock mode (syntax coloring)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; White-space and Special Characters
(set-default 'require-final-newline t)
(setq next-line-add-newlines t)
(setq-default indent-tabs-mode nil)
(setq default-truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq paren-priority 'both)
(setq paren-sexp-mode nil)
(setq paren-match-face 'show-paren-match-face)
(setq mark-even-if-inactive t)
(show-paren-mode t)
(transient-mark-mode t)

;; Auto-save, backups, and version control
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control 'never            ; version numbers on backups
      backup-by-copying t
      vc-handled-backends nil)          ; disable SCCS integration

;; URL browsing
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "conkopen.sh")
