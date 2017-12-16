;;; eshell-conf.el -- Settings for Eshell
(eval-when-compile
  (require 'eshell))

(require 'powerline)
(require 'esh-mode)

(custom-set-variables
 '(eshell-prompt-regexp "^❯ ")
 '(eshell-review-quick-commands nil)
 '(eshell-where-to-jump (quote after))
 '(eshell-smart-space-goes-to-end t))

(defun pjones:eshell-prompt-function ()
  (let* ((separator-left (intern (format "powerline-%s-%s"
                                         (powerline-current-separator)
                                         (car powerline-default-separator-dir))))
         (face0 'powerline-active0)
         (face1 'powerline-active1)
         (face2 'powerline-active2)
         (dir (abbreviate-file-name (eshell/pwd)))
         (host (concat (user-login-name) "@" system-name))
         (bar (list (powerline-raw host face0 'l)
                    (funcall separator-left face0 face1)
                    (powerline-raw dir  face1 'l)
                    (funcall separator-left face1 'default))))
    (concat (powerline-render bar)
            "\n❯ ")))

(defun pjones:eshell-mode-hook ()
  "Customize Eshell."
  (require 'em-smart)
  (setq eshell-prompt-function 'pjones:eshell-prompt-function)
  (eshell-smart-initialize))

(defvar eshell-mode-hook "Why isn't this defined?" nil)
(add-to-list 'eshell-mode-hook #'pjones:eshell-mode-hook)
