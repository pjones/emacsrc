;;; ruby-conf.el -- Settings for ruby-mode.
(eval-when-compile
  (require 'align)
  (require 'ruby-mode))

(defun pjones:ruby-mode-hook ()
  (setq ruby-deep-indent-paren nil
        align-mode-rules-list
        '((ruby-comma
           (regexp . ",\\(\\s-*\\)")
           (group  . 1)
           (repeat . t))
          (ruby-hash
           (regexp . "\\(\\s-*\\)=>")
           (group  . 1)
           (repeat . t))
          (ruby-eq
           (regexp . "\\(\\s-*\\)=")
           (group  . 1)
           (repeat . nil)))))

(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'pjones:ruby-mode-hook)

;; Getting Ruby Documentation via RI
;; (setq ri-ruby-script
;;       (concat user-emacs-directory "/share/emacs/site-lisp/ri-emacs.rb"))
;; (autoload 'ri "ri-ruby" nil t)
