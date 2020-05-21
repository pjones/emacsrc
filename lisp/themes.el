;;; themes.el -- Theme configuration.
;;
;;; Commentary:
;;
;;    Code to activate my preferred themes.
;;
;;; Code:
(defvar pjones:after-theme-change-hook nil
  "Hook run after changing themes.")

;; Stolen from: https://github.com/alezost/emacs-config
(defun pjones:load-theme (theme)
  "Load THEME after unloading all other themes first."
  (interactive
   (list (intern (completing-read
                  "Load custom theme: "
                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (run-hooks 'pjones:after-theme-change-hook))

;;; Override some annoying faces.
(custom-set-faces
 '(flymake-warning ((t (:underline nil))))
 '(flymake-error ((t (:underline nil)))))

(add-hook 'after-init-hook (lambda () (pjones:load-theme 'doom-dracula)))

;;; themes.el ends here
