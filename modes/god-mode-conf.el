;;; god-mode-conf.el -- Settings for god-mode.
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'god-mode))

;; Dependencies:
(require 'exwm)

;; Variables:
(defvar pjones:god-orig-cursor-type
  (default-value 'cursor-type)
  "The original cursor type.")

(defvar pjones:god-orig-cursor-bg
  (face-attribute 'cursor :background)
  "The original cursor background color.")

(defvar pjones:god-mode-on nil
  "Help track whether God mode is on or not.")

;; Helper functions:
(defun pjones:god-mode-enabled ()
  "Respond to `god-mode' turning on."
  ;; Settings for each buffer:
  (setq exwm-input-line-mode-passthrough t)
  ;; Settings to apply only once:
  (unless pjones:god-mode-on
    (setq pjones:god-mode-on t
          pjones:god-orig-cursor-type (default-value 'cursor-type)
          pjones:god-orig-cursor-bg (face-attribute 'cursor :background))
    (setq-default cursor-type 'box)
    (set-face-attribute 'cursor nil :background "red")))

(defun pjones:god-mode-disabled ()
  "Respond to `god-mode' turning off."
  ;; Settings to apply for each buffer:
  (setq exwm-input-line-mode-passthrough nil)
  ;; Settings to apply only once:
  (when pjones:god-mode-on
    (setq pjones:god-mode-on nil)
    (setq-default cursor-type pjones:god-orig-cursor-type)
    (set-face-attribute 'cursor nil :background pjones:god-orig-cursor-bg)))

;; Extra key bindings:
(define-key god-local-mode-map (kbd "i") 'god-mode-all)
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
