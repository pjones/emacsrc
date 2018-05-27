;;; exwm-conf.el -- Setting for EXWM.
;;
;; https://github.com/ch11ng/exwm
(eval-when-compile
  (require 'exwm))

;; Load optional EXWM features:
(require 'exwm-randr)

(defun pjones:exwm-manage-finish-hook ()
  "Hook run when a new X window is managed by EXWM."
  (setq mode-line-format
        '("  " mode-line-buffer-identification
          " (" mode-name mode-line-process
          ") " exwm-title))
  ;; Per-application settings:
  (cond
   ((string= exwm-class-name "Surf") t)))

(defun pjones:exwm-update-class-hook ()
  "Hook run when a window's class name changed."
  (exwm-workspace-rename-buffer exwm-class-name))

(custom-set-variables
   ;; Workspace settings:
  '(exwm-workspace-number 2)
  '(exwm-workspace-show-all-buffers nil)

  ;; Floating windows:
  '(exwm-floating-border-width 3)
  '(exwm-floating-border-color "#ff52bb")

  ;; RandR settings:
  '(exwm-randr-workspace-output-plist '(0 "eDP1" 1 "DP1"))

  ;; Global key bindings:
  '(exwm-input-prefix-keys
    (list ?\C-x ?\C-u ?\C-h ?\C-z ?\M-x ?\M-& ?\M-:))

  `(exwm-input-global-keys
    (quote ((,(kbd "s-SPC")      . helm-elscreen)
            (,(kbd "<s-return>") . pjones:start-term)
            (,(kbd "s-r")        . exwm-reset)
            (,(kbd "s-t")        . elscreen-toggle)
            (,(kbd "s-w")        . exwm-workspace-switch)
            (,(kbd "s-x")        . helm-run-external-command))))

  ;; Simulated key presses to X Windows:
  '(exwm-input-simulation-keys
    '(([?\C-b] . [left])
      ([?\C-f] . [right])
      ([?\C-p] . [up])
      ([?\C-n] . [down])
      ([?\C-a] . [home])
      ([?\C-e] . [end])
      ([?\M-v] . [prior])
      ([?\C-v] . [next])
      ([?\C-d] . [delete])
      ([?\C-k] . [S-end delete])
      ([?\C-w] . [?\C-x])
      ([?\M-w] . [?\C-c])
      ([?\C-y] . [?\C-v]))))

;; Activate optional features:
(exwm-randr-enable)

;; Insert some hooks:
(add-hook 'exwm-update-class-hook  #'pjones:exwm-update-class-hook)
(add-hook 'exwm-manage-finish-hook #'pjones:exwm-manage-finish-hook)
