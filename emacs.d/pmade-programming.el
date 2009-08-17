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

;; Code that should be called for each programming mode
(defun pmade-programming-mode-hook ()
  (setq show-trailing-whitespace nil
        save-place t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill)
  (flyspell-prog-mode)
  (font-lock-add-keywords nil '(("\\<\\(FIXME:\\|TODO:\\)" 1 pmade-fixme-face t)))
  (local-set-key "\C-m" 'pmade-newline)
  (local-set-key "\t"   'pmade-smart-tab))

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
    (pmade-programming-mode-hook)))

;; CSS, HTML
(autoload 'css-mode "css-mode" "CSS" t)
(add-hook 'css-mode-hook 'pmade-programming-mode-hook)
(add-hook 'html-mode-hook 'pmade-programming-mode-hook)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(setq css-indent-offset 2 sgml-basic-offset 2)

;; Javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'pmade-programming-mode-hook)
(setq js2-auto-indent-flag nil
      js2-bounce-indent-flag nil
      js2-rebind-eol-bol-keys nil
      js2-mode-escape-quotes nil
      js2-basic-offset 2)

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

;; Great Subversion Integration
(setq svn-restore-windows t) ;; needed only for dsvn
(setq svn-status-verbose nil)
(autoload 'svn-status "psvn" "Run `svn status'." t)
(autoload 'svn-update "psvn" "Run `svn update'." t)
