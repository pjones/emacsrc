;;; highlight-indent-guides-conf.el -- Settings for indent highlighting
;;
;;; Commentary:
;;
;;; Code:
(require 'highlight-indent-guides)

(custom-set-variables
  '(highlight-indent-guides-method 'character)
  '(highlight-indent-guides-character 9615)
  '(highlight-indent-guides-auto-character-face-perc 10))

(defun pjones:highlight-indent-guides-mode-hook ()
  "Hook for `highlight-indent-guides-mode'."
  (when (and highlight-indent-guides-mode (display-graphic-p))
    (highlight-indent-guides-auto-set-faces)))

(add-hook
 'highlight-indent-guides-mode-hook
 #'pjones:highlight-indent-guides-mode-hook)

(add-hook
 'pjones:after-theme-change-hook
 #'pjones:highlight-indent-guides-mode-hook)

;;;  highlight-indent-guides-conf.el ends here
