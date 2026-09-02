;;; nextflow-mode-conf.el -- Settings for `nextflow-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'nextflow-mode)
(require 'nextflow-ts-mode)
(require 'reformatter)

(reformatter-define nextflow-format
  :program "nextflow"
  :args (list "lint" "-format" "-output" "json" input-file)
  :stdin nil
  :stdout nil
  :input-file (reformatter-temp-file-in-current-directory "nf")
  :group 'nextflow)

(defun pjones:nextflow-mode-hook ()
  "Hook for `nextflow-mode-hook'."
  (if (eq 'nextflow-mode major-mode)
      (nextflow-ts-mode)
    (nextflow-format-on-save-mode)))

;; Nextflow Language Server currently doesn't work with eglot :(
;; https://github.com/nextflow-io/language-server/issues/118
;; (add-to-list
;;  'eglot-server-programs
;;  '((nextflow-mode nextflow-ts-mode) .
;;      ("nextflow-language-server")))

(add-to-list 'nextflow-mode-hook #'pjones:nextflow-mode-hook)
(add-to-list 'nextflow-ts-mode-hook #'pjones:nextflow-mode-hook)

;;; nextflow-mode-conf.el ends here
