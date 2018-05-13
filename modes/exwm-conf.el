;;; exwm-conf.el -- Setting for EXWM.
;;
;; https://github.com/ch11ng/exwm
(eval-when-compile
  (require 'exwm))

;; Load optional EXWM features:
(require 'exwm-randr)
(require 'exwm-systemtray)

(defun pjones:exwm-update-class-hook ()
  "Update shit after a class name changes."
  (exwm-workspace-rename-buffer exwm-class-name))

(custom-set-variables
   ;; Move minibuffer to the top of the screen and auto hide:
   '(exwm-workspace-minibuffer-position 'top)

   ;; Number of workspaces:
  '(exwm-workspace-number 9)

  ;; RandR settings:
  '(exwm-randr-workspace-output-plist '(0 "eDP1"))

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
      ([?\C-k] . [S-end delete]))))

(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

;; Activate optional features:
(exwm-randr-enable)
(exwm-systemtray-enable)

;; Insert some hooks:
(add-hook 'exwm-update-class-hook #'pjones:exwm-update-class-hook)
