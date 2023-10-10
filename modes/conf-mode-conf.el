;;; conf-mode-conf.el -- Settings for `conf-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'conf-mode)

(when (fboundp 'pjones:prog-mode-hook)
  (add-hook 'conf-mode-hook #'pjones:prog-mode-hook))

;;; conf-mode-conf.el ends here
