;;; rg-conf.el -- Settings for `rg'
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'rg)

(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

(pjones:evil-override-mode rg-mode)

;;; rg-conf.el ends here
