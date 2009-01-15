;;; This file contains general settings and options for emacs, to make
;;; it work more like what I expect

;; Personal Settings
(setq
 user-full-name "Peter Jones"
 user-mail-address "pjones@pmade.com"
 pmade-com-src "~/Documents/pmade/pmade.com"
 pmade-com-dst (concat pmade-com-src "/published")
 pmade-print-css "http://pmade.com/static/stylesheets/print.css")

;; Interface Options (remove some interface elements, such as the toolbar, scroll bar, and menu bar.
;; The menu bar is only removed from non-GUI versions of emacs
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and terminal-frame (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; General options
(setq
 inhibit-startup-message t        ; I've seen it already
 ring-bell-function (lambda ())   ; kill those damn bells
 visible-bell nil                 ; no visual bell
 column-number-mode t             ; display column number in mode line
 enable-recursive-minibuffers t   ; allow multiple minibuffers
 echo-keystrokes 0.1              ; show unfinished keystrokes early
 disabled-command-hook nil)       ; Enable commands disabled by default for novice users

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; Frame setup
(setq
 default-frame-alist '((cursor-type  . bar) (cursor-color . "yellow")))

;; Window Fringes
(require 'fringe)
(setq default-indicate-buffer-boundaries 'left) ; Indicate the top and bottom of a buffer
(setq default-indicate-empty-lines t)           ; Display an indicator for lines beyond the buffer EOF
(fringe-mode 'default)

;; Font lock mode (syntax coloring)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; White-space and Special Characters
(set-default 'require-final-newline t)
(setq next-line-add-newlines t)
(setq-default indent-tabs-mode nil)
(setq default-truncate-lines t)
(setq paren-priority 'both)
(setq paren-sexp-mode nil)
(setq paren-match-face 'show-paren-match-face)
(setq mark-even-if-inactive t)
(show-paren-mode t)
(transient-mark-mode t)

;; Use numbered backups, but keep them from cluttering up working directories
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq backup-by-copying t)
