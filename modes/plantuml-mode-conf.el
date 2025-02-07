;;; plantuml-mode-conf.el -- Settings for `plantuml-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'plantuml-mode)

(defun pjones:plantuml-preview ()
  "Wrapper for `plantuml-preview'.
Forces `plantuml-preview' to reuse the same window."
  (interactive)
  (let* ((buffer (get-buffer plantuml-preview-buffer))
         (window (and buffer (get-buffer-window buffer t))))
    (when window
      (display-buffer-override-next-command
       (lambda (_buffer alist)
         (window--display-buffer buffer window 'reuse alist)
         (cons window 'reuse))))
    (call-interactively #'plantuml-preview)))

(custom-set-variables
 '(plantuml-indent-level 2)
 '(plantuml-default-exec-mode 'executable))

(define-key plantuml-mode-map (kbd "C-c C-c") #'pjones:plantuml-preview)

;;; plantuml-mode-conf.el ends here
