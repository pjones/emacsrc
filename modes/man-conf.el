;;; man-conf.el -- Settings for `man' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'ansi-osc)
(require 'man)

(defun pjones:man-cleanup ()
  "Clean up man pages."
  (save-excursion
    ;; https://www.mail-archive.com/bug-groff@gnu.org/msg13845.html
    (ansi-osc-filter-region (point-min) (point-max))))

(custom-set-variables
 '(Man-notify-method 'pushy))

(add-hook 'Man-cooked-hook #'pjones:man-cleanup)

;;; man-conf.el ends here
