;;; god-mode-conf.el -- Settings for god-mode.
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'god-mode))

;; Dependencies:
(require 'exwm)

;; Helper functions:
(defun pjones:god-update-cursor ()
  "Change the cursor to indicate `god-mode'."
  (setq cursor-type (if god-local-mode 'box t)))

(defun pjones:god-mode-enabled ()
  "Respond to `god-mode' turning on."
  (pjones:god-update-cursor)
  (setq exwm-input-line-mode-passthrough t))

(defun pjones:god-mode-disabled ()
  "Respond to `god-mode' turning off."
  (pjones:god-update-cursor)
  (setq exwm-input-line-mode-passthrough nil))

;; Extra key bindings:
(define-key god-local-mode-map (kbd ".") #'repeat)

;; Search mode:
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>")     #'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)

;; Settings:
(custom-set-variables
 '(god-exempt-major-modes nil)
 '(god-exempt-predicates nil))

;; Hooks:
(add-hook 'god-mode-enabled-hook  #'pjones:god-mode-enabled)
(add-hook 'god-mode-disabled-hook #'pjones:god-mode-disabled)

;;; god-mode-conf.el ends here
