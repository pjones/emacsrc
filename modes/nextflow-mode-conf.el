;;; nextflow-mode-conf.el -- Settings for `nextflow-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'eglot)
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
  (if (eq major-mode 'nextflow-mode)
      (nextflow-ts-mode)
    (let* ((project (project-current))
           (dir (or (and project (project-root project))
                    default-directory)))
      (dir-locals-set-directory-class dir 'nextflow-project)
      (eglot-ensure)
      (eglot-semantic-tokens-mode -1)
      (nextflow-format-on-save-mode))))

;; These variable need to be set for nextflow to work correctly:
(let ((variables '((eglot-workspace-configuration
                 . (:nextflow
                    (:debug :json-false
                            :files (:exclude [".git" ".nf-test" "work"])
                            :formatting (:harshilAlignment :json-false)
                            :suppressFutureWarnings t))))))
  (dir-locals-set-class-variables
   'nextflow-project
   `((nextflow-mode . ,variables)
     (nextflow-ts-mode . ,variables))))

;; Ensure eglot can find the LSP server:
(add-to-list 'eglot-server-programs
             '(((nextflow-ts-mode :language-id "nextflow")
                (nextflow-mode :language-id "nextflow"))
               . ("nextflow-language-server")))

(add-to-list 'nextflow-mode-hook #'pjones:nextflow-mode-hook)
(add-to-list 'nextflow-ts-mode-hook #'pjones:nextflow-mode-hook)

;;; nextflow-mode-conf.el ends here
