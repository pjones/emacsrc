;;; themes.el -- Theme configuration.
;;
;;; Commentary:
;;
;;    Code to activate my preferred themes.
;;
;;; Code:
(require 'color-theme-sanityinc-tomorrow)

(defvar pjones:preferred-themes
  '( sanityinc-tomorrow-eighties
     sanityinc-tomorrow-day )
  "List of preferred themes, in order of priority.")

;; Enable all themes to avoid prompts.
(dolist (theme pjones:preferred-themes)
  (load-theme theme t nil))

;; Activate the default theme.
(enable-theme (car pjones:preferred-themes))

(provide 'themes)
;;; themes.el ends here
