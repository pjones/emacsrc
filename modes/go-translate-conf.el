;;; go-translate-conf.el -- Settings for `go-translate' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'go-translate)

(custom-set-variables
 '(gt-langs '(en de))
 '(gt-default-translator
   (gt-translator
    :taker   (gt-taker :text 'buffer :pick 'paragraph)
    :engines (list (gt-google-engine))
    :render  (gt-buffer-render)))
 '(gt-buffer-render-window-config
   '((display-buffer-reuse-window display-buffer-in-direction)
     (direction . below))))

;;; go-translate-conf.el ends here
