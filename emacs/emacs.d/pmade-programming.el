;; Correctly configure the hippie-expand function
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill))

;; Newline that puts you in the correct place
(defun pmade-newline ()
  "Emulate newline-and-indent for modes that don't have it"
  (interactive)
  (newline)
  (indent-according-to-mode))

(defun pmade-pound-comment-bar (&optional without-newline)
  "Create 80 pound signs"
  (interactive "P")
  (insert-char ?# (- 80 (current-column)))
  (if without-newline (beginning-of-line) (newline))
  (indent-according-to-mode))

(defun pmade-c-comment-bar (&optional without-newline)
  "Create a comment separator bar"
  (interactive "P")
  (insert-char ?/ 2)
  (insert-char ?= (- 78 (current-column)))
  (if without-newline (beginning-of-line) (newline))
  (indent-according-to-mode))

(defun pmade-latex-comment-bar (&optional without-newline)
  "Create a comment separator bar for LaTeX"
  (interactive "P")
  (insert-char ?% 1)
  (insert " ")
  (insert-char ?= (- 78 (current-column)))
  (if without-newline (beginning-of-line) (newline))
  (indent-according-to-mode))

;; Code that should be called for each programming mode
(defun pmade-programming-mode-hook ()
  (require 'align)
  (require 'enclose)
  (require 'wrap-region)
  (setq show-trailing-whitespace nil save-place t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill)
  (flyspell-prog-mode)
  (hs-minor-mode 1)
  (enclose-mode t)
  (wrap-region-mode t)
  ;(idea-complement-mode 1)
  (font-lock-add-keywords nil '(("\\<\\(FIXME:\\|TODO:\\|NOTE:\\)" 1 pmade-fixme-face t)))
  (local-set-key "\C-cf"  'hs-toggle-hiding)
  (local-set-key "\C-m"   'pmade-newline)
  (local-set-key "\t"     'pmade-smart-tab))

(defun create-and-use-tags ()
  (interactive)
  (shell-command "find . \( -name .svn -type d -prune \) -o -type f | exctags -e -L -"))

;; Emacs and Lisp
(add-hook 'emacs-lisp-mode-hook 'pmade-programming-mode-hook)
(add-hook 'lisp-mode-hook       'pmade-programming-mode-hook)

;; YAML Files
(autoload 'yaml-mode "yaml-mode" "YAML" t)
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook 'pmade-programming-mode-hook)

;; C Programming
(add-hook 'c-mode-hook
  (lambda ()
    (c-set-style "bsd")
    (setq c-basic-offset 4)
    (pmade-programming-mode-hook)
    (local-set-key "\C-c\t" 'pmade-c-comment-bar)))

;; Objective-C
(add-hook 'objc-mode-hook
  (lambda ()
    (require 'hideshow)
    (add-to-list 'hs-special-modes-alist 
      '(objc-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning))
    (pmade-programming-mode-hook)
    (when (string-match "\\.m$" buffer-file-name) (hs-hide-all))
    (load "emacs-xcode")
    (local-set-key "\C-ch" 'xcode/toggle-header-and-source)
    (local-set-key "\C-c\C-c" 'xcode/build-compile)
    (local-set-key "\C-c\t" 'pmade-c-comment-bar)))

;; CSS, HTML
(autoload 'css-mode "css-mode" "CSS" t)
(add-hook 'css-mode-hook 'pmade-programming-mode-hook)
(add-hook 'html-mode-hook 'pmade-programming-mode-hook)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(setq css-indent-offset 2 sgml-basic-offset 2)

;; JavaScript
(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(add-hook 'espresso-mode-hook 'pmade-programming-mode-hook)
(setq espresso-indent-level 2 espresso-flat-functions t)

;; Shell scripting
(setq sh-basic-offset 2)
(add-hook 'sh-mode-hook (lambda ()
  (pmade-programming-mode-hook)
  (local-set-key "\C-c\t" 'pmade-pound-comment-bar)))

;; SQL Files
(add-hook 'sql-mode-hook 'pmade-programming-mode-hook)

;; Make #! scripts executable after saving them
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; I keep Ruby programming stuff in a separate file
(load "~/.emacs.d/pmade/pmade-ruby")

;; LaTeX
(load "auctex.el" nil t t)

(add-hook 'LaTeX-mode-hook
  (lambda ()
    (local-set-key "\C-c\t" 'pmade-latex-comment-bar)))

;; Great Subversion Integration
(setq svn-restore-windows t) ;; needed only for dsvn
(setq svn-status-verbose nil)
(autoload 'svn-status "psvn" "Run `svn status'." t)
(autoload 'svn-update "psvn" "Run `svn update'." t)

;; Other file name to mode mappings
(add-to-list 'auto-mode-alist '("errors\\.out$" . compilation-mode))
