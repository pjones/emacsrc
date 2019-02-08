;;; themes.el -- Theme configuration.
;;
;;; Commentary:
;;
;;    Code to activate my preferred themes.
;;
;;; Code:
(if (display-graphic-p)
    (load-theme 'doom-city-lights t)
  (load-theme 'doom-one t))

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
