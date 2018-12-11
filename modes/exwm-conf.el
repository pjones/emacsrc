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
  "Hook run when a new X window is managed by EXWM.")

(defun pjones:exwm-update-class-hook ()
  "Hook run when a window's class name changed."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun pjones:exwm-workspace-switch-hook ()
  "Hook run when changing workspaces.")

(defmacro pjones:exwm-switch-to (n)
  "Create a function that will switch to workspace N."
  `(lambda () (interactive) (exwm-workspace-switch-create ,n)))

;###############################################################################
;;
;;; Settings:
;;
;###############################################################################
(custom-set-variables ;; Common settings:
   ;; Workspace settings:
  '(exwm-workspace-number 10)
  '(exwm-randr-workspace-output-plist nil)
  '(exwm-workspace-show-all-buffers nil)
  '(exwm-layout-show-all-buffers nil)
  '(exwm-workspace-minibuffer-position nil)
  '(exwm-workspace-display-echo-area-timeout 1)
  '(exwm-workspace-warp-cursor t)

  ;; Floating windows:
  '(exwm-manage-force-tiling t)
  '(exwm-floating-border-width 3)
  '(exwm-floating-border-color "#ff52bb")

  ;; Global key bindings:
  '(exwm-input-prefix-keys
    (list ?\C-w ?\C-x ?\C-u ?\C-h ?\C-c ?\M-x ?\M-& ?\M-:))

  `(exwm-input-global-keys
    (quote ((,(kbd "s-r")        . exwm-reset)
            (,(kbd "s-o")        . other-frame)
            (,(kbd "s-z")        . exwm-workspace-switch)
            (,(kbd "s-p")        . exwm-nw-goto-previous)

            ;; Switch workspaces with the super key:
            (,(kbd "s-;") . ,(pjones:exwm-switch-to 0))
            (,(kbd "s-a") . ,(pjones:exwm-switch-to 1))
            (,(kbd "s-s") . ,(pjones:exwm-switch-to 2))
            (,(kbd "s-d") . ,(pjones:exwm-switch-to 3))
            (,(kbd "s-f") . ,(pjones:exwm-switch-to 4))
            (,(kbd "s-g") . ,(pjones:exwm-switch-to 5))
            (,(kbd "s-h") . ,(pjones:exwm-switch-to 6))
            (,(kbd "s-j") . ,(pjones:exwm-switch-to 7))
            (,(kbd "s-k") . ,(pjones:exwm-switch-to 8))
            (,(kbd "s-l") . ,(pjones:exwm-switch-to 9)))))

  ;; Simulated key presses to X Windows:
  '(exwm-input-simulation-keys nil))

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
