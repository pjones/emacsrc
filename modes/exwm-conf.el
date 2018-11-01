;;; exwm-conf.el -- Setting for EXWM.
;;
;;; Commentary:
;;
;; Functions and settings for EXWM:
;;   https://github.com/ch11ng/exwm
;;
;;; Code:
(eval-when-compile
  (require 'exwm))

;; Load optional EXWM features:
(require 'cl-lib)
(require 'exwm-nw)
(require 'exwm-randr)

;###############################################################################
;;
;;; Custom EXWM functions:
;;
;###############################################################################
(defun pjones:exwm-manage-finish-hook ()
  "Hook run when a new X window is managed by EXWM."
  (setq mode-line-format
        '("" mode-line-front-space
          (:eval (pjones:mode-line-status))
          "   " mode-line-buffer-identification
          " ("  mode-name mode-line-process
          ") "  exwm-title)))

(defun pjones:exwm-update-class-hook ()
  "Hook run when a window's class name changed."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun pjones:exwm-workspace-switch-hook ()
  "Hook run when changing workspaces.")

;###############################################################################
;;
;;; Settings:
;;
;###############################################################################
(cond ;; Settings that depend on the machine:
 ((string= (system-name) "medusa")
  (custom-set-variables
   '(exwm-workspace-number 3)
   '(exwm-randr-workspace-output-plist '(1 "DVI-0" 2 "DVI-1"))))
 ((string= (system-name) "elphaba")
  (custom-set-variables
   '(exwm-workspace-number 2)
   '(exwm-randr-workspace-output-plist '(1 "DP1"))))
 (t
  (custom-set-variables
   '(exwm-workspace-number 1))))

(custom-set-variables ;; Common settings:
   ;; Workspace settings:
  '(exwm-workspace-show-all-buffers nil)
  '(exwm-layout-show-all-buffers nil)
  '(exwm-workspace-minibuffer-position 'bottom)
  '(exwm-workspace-display-echo-area-timeout 1)
  '(exwm-workspace-warp-cursor t)

  ;; Floating windows:
  '(exwm-manage-force-tiling t)
  '(exwm-floating-border-width 3)
  '(exwm-floating-border-color "#ff52bb")

  ;; Global key bindings:
  '(exwm-input-prefix-keys
    (list ?\C-x ?\C-u ?\C-h ?\C-z ?\M-x ?\M-& ?\M-:))

  `(exwm-input-global-keys
    (quote ((,(kbd "s-r")        . exwm-reset)
            (,(kbd "s-s")        . other-frame)
            (,(kbd "s-z")        . exwm-workspace-switch)
            (,(kbd "<escape>")   . god-mode-all))))

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
      ([?\C-g] . [escape])
      ([?\C-w] . [?\C-x])
      ([?\M-w] . [?\C-c])
      ([?\C-y] . [?\C-v])
      ([?\C-/] . [?\C-z]))))

(define-key exwm-workspace--switch-map (kbd "s-z")     #'exwm-nw-goto-previous)
(define-key exwm-workspace--switch-map (kbd "C-z C-z") #'exwm-nw-goto-previous)
(define-key exwm-workspace--switch-map (kbd "C-c C-b") #'exwm-nw-move-left)
(define-key exwm-workspace--switch-map (kbd "C-c C-f") #'exwm-nw-move-right)
(define-key exwm-workspace--switch-map (kbd "C-c C-n") #'exwm-nw-set-name)
(define-key exwm-workspace--switch-map (kbd "C-s")     #'exwm-nw-find-workspace)
(define-key exwm-workspace--switch-map (kbd "C-u")     #'universal-argument)

;###############################################################################
;;
;;; Activate optional features:
;;
;###############################################################################
(exwm-randr-enable)
(exwm-nw-mode)

;###############################################################################
;;
;;; Insert some hooks:
;;
;###############################################################################
(add-hook 'exwm-update-class-hook     #'pjones:exwm-update-class-hook)
(add-hook 'exwm-manage-finish-hook    #'pjones:exwm-manage-finish-hook)
(add-hook 'exwm-workspace-switch-hook #'pjones:exwm-workspace-switch-hook)
(add-hook 'exwm-init-hook              'exwm-workspace-attach-minibuffer)

;;; exwm-conf.el ends here
