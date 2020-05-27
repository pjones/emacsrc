;;; help-mode-conf.el -- Help mode settings
;;
;;; Commentary:
;;
;;; Code:
(require 'help-mode)
(require 'evil)

(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

(pjones:evil-override-mode help-mode
  (kbd "<tab>") #'forward-button
  (kbd "<backtab>") #'backward-button
  "H" #'help-go-back
  "L" #'help-go-forward)

;;; help-mode-conf.el ends here
