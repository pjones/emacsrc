;;; google-translate-smooth-ui-conf.el -- Settings for `google-translate-smooth-ui' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'google-translate)
(require 'google-translate-smooth-ui)

(custom-set-variables
 '(google-translate-output-destination nil)
 '(google-translate-pop-up-buffer-set-focus t)
 '(google-translate-listen-program "mpv")
 '(google-translate-translation-directions-alist
   '(("de" . "en")
     ("en" . "de"))))

;;; google-translate-smooth-ui-conf.el ends here
