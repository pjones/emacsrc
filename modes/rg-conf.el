;;; rg-conf.el -- Settings for `rg' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'compile)
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

(defun pjones:rg-compilation-auto-jump ()
  "Emulate `compilation-auto-jump-to-first-error'."
  (when (and (> rg-hit-count 0) compilation-auto-jump-to-first-error)
    (goto-char (point-min))
    (compilation-next-error 1)
    (compile-goto-error)))

(add-hook 'rg-filter-hook #'pjones:rg-compilation-auto-jump)

;;; rg-conf.el ends here
