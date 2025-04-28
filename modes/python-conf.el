;;; python-conf.el -- Settings for `python' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'python)
(require 'reformatter)

(custom-set-variables
 '(python-shell-prompt-detect-failure-warning nil) ; Never works.
 '(python-shell-completion-native-enable nil) ; Doesn't work.
 '(python-indent-offset 4))

(reformatter-define python-format
  :program "black"
  :args '("-")
  :group 'python)

(defun pjones:python-mode-hook ()
  "Hook function for `python-mode'."
  (when (fboundp 'pjones:prog-mode-hook)
    (pjones:prog-mode-hook))
  (when (buffer-file-name)
    (python-format-on-save-mode)
    (eglot-ensure))
  (setq prettify-symbols-alist
        (assoc-delete-all
         "and" (assoc-delete-all "or" prettify-symbols-alist))))

(add-hook 'inferior-python-mode-hook #'pjones:python-mode-hook)
(add-hook 'python-mode-hook #'pjones:python-mode-hook)
(add-hook 'python-ts-mode-hook #'pjones:python-mode-hook)

;;; python-conf.el ends here
