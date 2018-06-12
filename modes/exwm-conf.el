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
(require 'exwm-randr)
(require 'dash)
(require 'helm)
(require 'helm-buffers)

;###############################################################################
;;
;;; Helm Integration:
;;
;###############################################################################
(defvar pjones:exwm-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-buffer-switch-other-window))
  "Key bindings for `pjones:exwm-helm-title-source'.")

(defvar pjones:exwm-helm-title-source
  (helm-make-source "Window Titles (EXWM)" 'helm-source
      :candidates #'pjones:exwm-buffers
      :candidate-transformer #'pjones:exwm-buffers-transformer
      :action '(("Switch to buffer(s)" . helm-buffer-switch-buffers)
                ("Switch to buffer(s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window))
      :persistent-action 'helm-buffers-list-persistent-action
      :keymap 'pjones:exwm-helm-map))

(defun pjones:exwm-buffers ()
  "List of buffers whose major mode is `exwm-mode'."
  (-filter (lambda (buffer)
             (with-current-buffer buffer
               (derived-mode-p 'exwm-mode)))
           (helm-buffer-list)))

(defun pjones:exwm-buffers-transformer (buffers)
  "Return Helm candidate string for BUFFERS."
  (-map (lambda (buffer)
          (with-current-buffer buffer
            (cons (concat
                   (helm-substring-by-width
                    exwm-class-name helm-buffer-max-length)
                   (propertize
                    exwm-title 'face 'helm-buffer-process))
                  (current-buffer))))
        buffers))

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
          ") "  exwm-title))
  ;; Per-application settings:
  (cond
   ((string= exwm-class-name "Surf") t)))

(defun pjones:exwm-update-class-hook ()
  "Hook run when a window's class name changed."
  (exwm-workspace-rename-buffer exwm-class-name))

;###############################################################################
;;
;;; Settings:
;;
;###############################################################################
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

  ;; Helm helper:
  '(helm-mini-default-sources
    (quote (helm-source-buffers-list
            pjones:exwm-helm-title-source
            helm-source-recentf)))

  `(exwm-input-global-keys
    (quote ((,(kbd "s-r")        . exwm-reset)
            (,(kbd "s-s")        . other-frame)
            (,(kbd "s-w")        . exwm-workspace-switch)
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
      ([?\C-y] . [?\C-v]))))

;###############################################################################
;;
;;; Activate optional features:
;;
;###############################################################################
(exwm-randr-enable)

;###############################################################################
;;
;;; Insert some hooks:
;;
;###############################################################################
(add-hook 'exwm-update-class-hook  #'pjones:exwm-update-class-hook)
(add-hook 'exwm-manage-finish-hook #'pjones:exwm-manage-finish-hook)

;;; exwm-conf.el ends here
