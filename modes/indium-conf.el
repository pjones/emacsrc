;;; indium-conf.el -- Settings for `indium' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

;; FIXME:
;; Loading this file leads to something trying to touch $HOME which
;; fails during a nix-build.
;; (require 'indium)

(declare-function indium-eval "indium-interaction")
(defvar indium-interaction-mode-map)
(defvar indium-repl-mode-map)

(defun pjones:indium-eval-buffer ()
  "Eval the entire buffer."
  (interactive)
  (indium-eval (buffer-substring-no-properties (point-min) (point-max))))

(defun pjones:indium-eval-buffer-and-close ()
  "Eval the last expression and close indium-scratch."
  (interactive)
  (pjones:indium-eval-buffer)
  (delete-window))

(defun pjones:indium-repl-mode-hook ()
  "Configure indium-repl-mode."
  (let ((map indium-repl-mode-map))
    (define-key map (kbd "C-c C-l") 'indium-repl-clear-output)
    (define-key map (kbd "C-c C-i") 'indium-repl-inspect)
    (define-key map (kbd "C-c C-s") 'indium-scratch)))

(defun pjones:indium-interaction-mode-hook ()
  "Configure indium-interaction-mode."
  (let ((map indium-interaction-mode-map))
    (define-key map (kbd "C-c C-c") 'pjones:indium-eval-buffer-and-close)
    (define-key map (kbd "C-c e b") 'pjones:indium-eval-buffer)
    (define-key map (kbd "C-c e l") 'indium-eval-last-node)
    (define-key map (kbd "C-c e n") 'indium-inspect-last-node)
    (define-key map (kbd "C-x C-s") 'pjones:indium-eval-buffer)))

;; Install Hooks:
(add-hook 'indium-repl-mode-hook 'pjones:indium-repl-mode-hook)
(add-hook 'indium-interaction-mode-hook 'pjones:indium-interaction-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; indium-conf.el ends here
