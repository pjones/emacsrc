;;; css-mode-conf.el -- Settings for `css-mode'
;;
;;; Commentary:
;;
;;; Code:

(require 'css-mode)
(require 'rainbow-mode)

(custom-set-variables
 '(css-indent-offset 2))

(add-hook 'css-mode-hook #'rainbow-mode)
(when (fboundp 'pjones:prog-mode-hook)
  (add-hook 'css-mode-hook #'pjones:prog-mode-hook))

;;; css-mode-conf.el ends here
