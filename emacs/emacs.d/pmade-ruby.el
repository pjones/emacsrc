;; Ruby Programming
(eval-when-compile
  (load-file "pmade-loadpath.el"))

(defun pmade-ruby-mode-hook ()
  "Correctly setup a Ruby buffer"
  (require 'ruby-electric)
  (ruby-electric-mode t)
  (inf-ruby-keys)
  (pmade-programming-mode-hook)
  (define-key ruby-mode-map "\C-c\t" 'pmade-ruby-comment-bar)
  (require 'align)
  (setq align-mode-rules-list
        '((ruby-comma
           (regexp . ",\\(\\s-*\\)")
           (group  . 1)
           (repeat . t))
          (ruby-hash
           (regexp . "\\(\\s-*\\)=>")
           (group  . 1)
           (repeat . t)))))

(defun pmade-ruby-comment-bar (&optional without-newline)
  "Create 80 pound signs"
  (interactive "P")
  (insert-char ?# 80)
  (if without-newline (beginning-of-line) (newline))
  (indent-according-to-mode))

;; Trigger ruby-mode
(add-to-list 'load-path "~/.emacs.d/packages/ruby")
(add-to-list 'auto-mode-alist '("\\.rjs$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$" . ruby-mode))
(setq ruby-deep-indent-paren '(?\( t))

;; manually set some keys because for some reason, local-set-key
;; doesn't work for ruby-mode in terminal emacs :(
(add-hook 'ruby-mode-hook
          (lambda ()
            (pmade-ruby-mode-hook)
            (define-key ruby-mode-map "\t"   'pmade-smart-tab)
            (define-key ruby-mode-map "\C-m" 'pmade-newline)))

;; Getting Ruby Documentation via RI
(setq ri-ruby-script (concat (getenv "HOME") "/.emacs.d/packages/ri-emacs.rb"))
(autoload 'ri "ri-ruby" nil t)

;; Running IRB inside Emacs
(add-to-list 'interpreter-mode-alist '(("ruby" . ruby-mode)) t)
(setq inferior-ruby-first-prompt-pattern "^>>")
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

;; Send Ruby code through the interpreter
(defun ruby-eval-region (start end)
  "Send the region through the Ruby interpreter"
  (interactive "r")
  (shell-command-on-region start end "ruby"))

(defun ruby-eval-buffer ()
  "Send the entire buffer through the Ruby interpreter"
  (interactive)
  (ruby-eval-region (point-min) (point-max)))

;; Use the XMP filter
(defun ruby-xmp-region (start end)
  "Send the region through Ruby's xmp utility, and replace
the region with the output.  Requires the rcodetools Ruby gem."
  (interactive "r")
  (shell-command-on-region start end "xmpfilter" t))

(defun ruby-xmp-buffer ()
  "Send the entire buffer through Ruby's xmp utility"
  (interactive)
  (ruby-xmp-region (point-min) (point-max)))

;; Ruby on Rails
(add-to-list 'load-path (concat pmade-site-lisp "/rinari"))
(require 'rinari)

(defun pmade-rhtml-mode-hook ()
  "Stuff common across all RHTML buffers"
  (pmade-programming-mode-hook)
  (setq fill-column 120))

(add-to-list 'load-path (concat pmade-site-lisp "/rhtml"))
(autoload 'rhtml-mode "rhtml-mode" "RHTML" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html\.erb$" . rhtml-mode))
(add-hook 'rhtml-mode-hook 'pmade-rhtml-mode-hook)
