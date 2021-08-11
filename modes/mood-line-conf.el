;;; mood-line-conf.el -- Settings for `mood-line' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'mood-line)

(eval-when-compile
  (require 'pdf-macs))

(declare-function pdf-cache-number-of-pages 'pdf-cache)
(declare-function image-mode-window-get 'image-mode)

(defun pjones:mood-line-segment-position (func &rest args)
  "Advice `mood-line-segment-position' to include PDF page number.

FUNC is the original version of the function and ARGS are any
arguments that need to be passed along to it."
  (pcase major-mode
    ;; Display current page and total number of pages:
    ('pdf-view-mode
     (format "%s/%s"
             (pdf-view-current-page)
             (pdf-cache-number-of-pages)))
    ;; Fall through to the original function:
    (_ (apply func args))))

(advice-add
 'mood-line-segment-position
 :around #'pjones:mood-line-segment-position)

;;; mood-line-conf.el ends here
