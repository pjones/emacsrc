;;; python-conf.el -- Settings for `python' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'python)
(require 'reformatter)

(custom-set-variables
 '(python-indent-offset 4))

(reformatter-define python-format
  :program "black"
  :args '("-")
  :group 'python)

(defun pjones:python-mode-hook ()
  "Hook function for `python-mode'."
  (setq prettify-symbols-alist
        (assoc-delete-all "and"
         (assoc-delete-all "or"
           prettify-symbols-alist))))

(add-hook 'python-mode-hook #'python-format-on-save-mode)
(add-hook 'python-mode-hook #'pjones:python-mode-hook)
(add-hook 'inferior-python-mode-hook #'pjones:python-mode-hook)

;;; python-conf.el ends here
