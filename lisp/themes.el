;;; themes.el -- Theme configuration.
;;
;;; Commentary:
;;
;;    Code to activate my preferred themes.
;;
;;; Code:
(load-theme 'doom-dracula t)

;; Stolen from: https://github.com/alezost/emacs-config
(defun pjones:load-theme (theme)
  "Load THEME after unloading all other themes first."
  (interactive
   (list (intern (completing-read
                  "Load custom theme: "
                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (if (fboundp 'pjones:spaceline-update)
      (pjones:spaceline-update)))

(provide 'themes)
;;; themes.el ends here
