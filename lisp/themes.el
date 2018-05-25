;;; themes.el -- Theme configuration.
;;
;;; Commentary:
;;
;;    Code to activate my preferred themes.
;;
;;; Code:

;; Load default theme and update settings:
;; https://github.com/alezost/alect-themes
(require 'alect-themes)

(custom-set-variables
 '(alect-overriding-faces
   (quote ((mode-line ((t :foreground fg+1
                          :background bg-1
                          :box (:line-width 2 :color fg+1 :style nil))))
           (font-lock-string-face ((t :foreground yellow+2)))
           (font-lock-comment-face ((t :foreground "#9396c4")))
           (font-lock-comment-delimiter-face ((t :foreground bg
                                                 :weight bold)))))))

(alect-set-color 'dark 'bg-1 "#333333")
(load-theme 'alect-dark t)

;; Stolen from: https://github.com/alezost/emacs-config
(defun pjones:load-theme (theme)
  "Load THEME after unloading all other themes first."
  (interactive (list (intern (completing-read
                              "Load custom theme: "
                              (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'themes)
;;; themes.el ends here
