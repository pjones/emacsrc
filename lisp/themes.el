;;; themes.el -- Theme configuration.
;;
;;; Commentary:
;;
;;    Code to activate my preferred themes.
;;
;;; Code:
(defvar pjones:after-theme-change-hook nil
  "Hook run after changing themes.")

(defvar pjones:current-theme nil
  "The currently active theme.")

(custom-set-variables
 '(doom-themes-treemacs-theme "doom-colors")
 '(doom-themes-treemacs-enable-variable-pitch nil))

;; Stolen from: https://github.com/alezost/emacs-config
(defun pjones:load-theme (theme)
  "Load THEME after unloading all other themes first."
  (interactive
   (list (intern (completing-read
                  "Load custom theme: "
                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (condition-case nil
      (load-theme theme t)
    (error nil))
  (setq pjones:current-theme theme)
  (run-hooks 'pjones:after-theme-change-hook))

(defun pjones:theme-next (&optional prev)
  "Switch to the next theme.
If PREV is non-nil go to the previous theme."
  (interactive "P")
  (let* ((themes (custom-available-themes))
         (last (1- (length themes)))
         (n (or (seq-position themes pjones:current-theme) last))
         (m (if prev (1- n) (1+ n))))
    (cond
     ((> m last) (pjones:load-theme (car themes)))
     ((< m 0) (pjones:load-theme (nth last themes)))
     (t (pjones:load-theme (nth m themes))))
    (message "Theme %s" pjones:current-theme)))

(defun pjones:theme-prev ()
  "Switch to the previous theme."
  (interactive)
  (pjones:theme-next t))

;;; Override some annoying faces.
(add-hook 'after-init-hook
  (defun pjones:set-initial-theme ()
    (pjones:load-theme 'doom-dark+)))

(add-hook 'pjones:after-theme-change-hook
  (defun pjones:set-up-doom ()
    (when (s-prefix-p "doom-" (symbol-name pjones:current-theme))
      (doom-themes-visual-bell-config)
      (doom-themes-treemacs-config)
      (doom-themes-org-config))))

;;; themes.el ends here
