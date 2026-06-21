;;; savehist-conf.el -- Settings for `savehist' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'savehist)

(custom-set-variables
 '(savehist-file (concat user-emacs-directory "savehist")))

;; Additional variables to keep track of:
(dolist (var '(corfu-history register-alist))
  (add-to-list 'savehist-additional-variables var))

;;; savehist-conf.el ends here
