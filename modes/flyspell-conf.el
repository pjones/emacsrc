;;; flyspell-conf.el -- Configuration for flyspell
;;
;;; Commentary:
;;
;;; Code:
(require 'flyspell)
(require 'flyspell-correct)

(define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)

;;; flyspell-conf.el ends here
