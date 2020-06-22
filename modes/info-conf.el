;;; info-conf.el -- Settings for `info'
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'info)

(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

(pjones:evil-override-mode Info-mode
  "[[" #'Info-backward-node
  "]]" #'Info-forward-node
  "/" #'isearch-forward
  "?" #'isearch-backward
  "H" #'Info-history-back
  "L" #'Info-history-forward
  "n" #'isearch-repeat-forward
  "N" #'isearch-repeat-backward)

(evil-leader/set-key-for-mode 'Info-mode-map
  "m g" #'Info-goto-node
  "m h" #'Info-help)

;;; info-conf.el ends here
