;;; shell-conf.el -- Settings for `shell' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

(require 'shell)
(require 'evil)

(pjones:evil-override-mode shell-mode)

;;; shell-conf.el ends here
