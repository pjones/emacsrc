;;; flyspell-conf.el -- Configuration for flyspell
;;
;;; Commentary:
;;
;;; Code:

(require 'flyspell)
(require 'flyspell-correct-avy-menu)

(defun pjones:flyspell-correct ()
  "Correct work without moving point."
  (interactive)
  (save-excursion (flyspell-correct-wrapper)))

(define-key flyspell-mode-map (kbd "C-;") #'pjones:flyspell-correct)
(define-key flyspell-mode-map (kbd "C-.") nil)

;;; flyspell-conf.el ends here
