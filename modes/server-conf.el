;;; server-conf.el -- Settings for `server' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'server)

(declare-function pjones:display-buffer-in-non-popup-frame "../lisp/functions")

(custom-set-variables
  '(server-window #'pjones:display-buffer-in-non-popup-frame))

;;; server-conf.el ends here
