;;; python-conf.el -- Settings for `python' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'python)
(require 'reformatter)

(declare-function indent-bars-mode "indent-bars")

(custom-set-variables
 '(python-shell-prompt-detect-failure-warning nil) ; Never works.
 '(python-shell-completion-native-enable nil) ; Doesn't work.
 '(python-indent-offset 4)

 ;; I have no idea why, but Python 3.14 plus my configuration causes
 ;; it to start a fancy REPL which breaks org-babel.  This hack turns
 ;; that off:
 '(python-shell-process-environment '("PYTHON_BASIC_REPL=1")))

(reformatter-define python-format
  :program "ruff"
  :args '("format" "-")
  :group 'python)

(defun pjones:python-mode-hook ()
  "Hook function for `python-mode'."
  (setq prettify-symbols-alist
        (assoc-delete-all
         "and" (assoc-delete-all "or" prettify-symbols-alist)))
  (when (fboundp 'pjones:prog-mode-hook)
    (pjones:prog-mode-hook))
  (python-format-on-save-mode)
  (indent-bars-mode)
  (eglot-ensure))

(add-hook 'python-mode-hook #'pjones:python-mode-hook)
(add-hook 'python-ts-mode-hook #'pjones:python-mode-hook)

;;; python-conf.el ends here
