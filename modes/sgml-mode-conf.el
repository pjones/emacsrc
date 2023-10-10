;;; sgml-mode-conf.el -- Settings for `sgml-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(when (fboundp 'pjones:prog-mode-hook)
  (add-hook 'sgml-mode-hook #'pjones:prog-mode-hook))

;;; sgml-mode-conf.el ends here
