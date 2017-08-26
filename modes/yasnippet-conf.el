;;; yasnippet-conf.el -- Settings for yasnippet.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'yasnippet))

(custom-set-variables
 `(yas-snippet-dirs ,(concat user-emacs-directory "pjones/snippets")))
