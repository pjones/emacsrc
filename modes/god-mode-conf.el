;;; god-mode-conf.el -- Settings for god-mode.
(eval-when-compile
  (require 'god-mode))

(defun pjones:god-update-cursor ()
  "Change the cursor to indicate `god-mode'."
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)

(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(add-hook 'god-mode-enabled-hook 'pjones:god-update-cursor)
(add-hook 'god-mode-disabled-hook 'pjones:god-update-cursor)
