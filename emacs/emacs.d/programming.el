;; Load in a few files that are great for programming
(require 'find-recursive)

;; Correctly configure the hippie-expand function
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
	try-complete-file-name
	try-expand-dabbrev))

;; '(try-expand-dabbrev
;; try-expand-dabbrev-all-buffers
;; try-expand-dabbrev-from-kill
;; try-complete-file-name-partially
;; try-complete-file-name
;; try-complete-lisp-symbol-partially
;; try-complete-lisp-symbol
;; try-expand-whole-kill))

;; Newline that puts you in the correct place
(defun pmade-newline ()
  "Emulate indent-then-newline-and-indent for modes that don't have it"
  (interactive)
  (indent-according-to-mode)
  (newline)
  (indent-according-to-mode))

;; Code that should be called for each programming mode
(defun pmade-programming-mode-hook ()
  (setq show-trailing-whitespace t)
  (local-set-key "\C-m" 'pmade-newline)
  (local-set-key "\t"   'pmade-smart-tab)
  (flyspell-prog-mode))

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
(defun pmade-c-mode-hook ()
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (pmade-programming-mode-hook))
(add-hook 'c-mode-hook 'pmade-c-mode-hook)

;; CSS, HTML
(autoload 'css-mode "css-mode" "CSS" t)
(add-hook 'css-mode-hook 'pmade-programming-mode-hook)
(add-hook 'html-mode-hook 'pmade-programming-mode-hook)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(setq css-indent-offset 2 sgml-basic-offset 2)

;; Javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Shell scripting
(setq sh-basic-offset 2)

;; Make #! scripts executable after saving them
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; I keep Ruby programming stuff in a separate file
(load "~/.emacs.d/pmade/ruby.el")

;; Great Subversion Integration
(setq svn-restore-windows t)
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

;; Git Integration
(autoload 'git-status "git" "Run `git status'." t)
