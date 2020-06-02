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
 '(markdown-header-face-1 ((t (:inherit org-level-1 :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit org-level-2 :height 1.7))))
 '(markdown-header-face-3 ((t (:inherit org-level-3 :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit org-level-4 :height 1.1))))
 '(markdown-header-face-5 ((t (:inherit org-level-5))))
 '(markdown-header-face-6 ((t (:inherit org-level-6))))
 '(markdown-header-delimiter-face ((t (:inherit org-done))))
 '(flymake-warning ((t (:underline nil))))
 '(flymake-error ((t (:underline nil)))))

(add-hook 'after-init-hook (lambda () (pjones:load-theme 'doom-dracula)))

;;; themes.el ends here
