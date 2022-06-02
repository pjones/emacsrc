;;; emms-conf.el -- Settings for `emms' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'emms)
(require 'emms-setup)

(emms-all)
(emms-default-players)

(custom-set-variables
  '(emms-source-file-default-directory "~/documents/music/"))

;;; emms-conf.el ends here
