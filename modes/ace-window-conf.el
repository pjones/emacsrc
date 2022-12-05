;;; ace-window-conf.el -- Settings for `ace-window' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'ace-window)

(custom-set-variables
 '(aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
 '(aw-scope 'frame)
 '(aw-leading-char-style 'path))

(custom-set-faces
 '(aw-leading-char-face ((t (:inherit isearch :weight bold)))))

;;; ace-window-conf.el ends here
