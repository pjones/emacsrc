;;; puni-conf.el -- Settings for `puni' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'puni)

(let ((mode puni-mode-map))
  (define-key mode (kbd "C-w") nil))

;;; puni-conf.el ends here
