;;; doom-themes-conf.el -- Settings for `doom-themes' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'doom-themes)

(defun pjones:doom-themes-fixups (theme)
  "Fix really bad faces set by doom themes.
THEME is the name of the theme just set."
  (when (string-match-p (rx bol "doom-") (symbol-name theme))
    (custom-set-faces
     `(isearch ((t (:inherit nil
                    :background ,(doom-color 'cyan)
                    :foreground ,(doom-color 'bg)
                    :weigth bold
                    :box (:line-width 3 :color ,(doom-color 'cyan))))))
     `(lazy-highlight ((t (:inherit nil
                           :background ,(doom-color 'dark-cyan)
                           :foreground ,(doom-color 'fg)
                           :weigth bold)))))))

(add-hook 'enable-theme-functions #'pjones:doom-themes-fixups)

;;; doom-themes-conf.el ends here
