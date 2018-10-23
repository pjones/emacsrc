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
(require 'helm)
(require 'helm-buffers)

;###############################################################################
;;
;;; Helm Integration:
;;
;###############################################################################
(defun pjones:helm-limited-string (str)
  "Truncate or pad string STR."
  (if (> (string-width str) helm-buffer-max-length)
      (helm-substring-by-width
       str helm-buffer-max-length helm-buffers-end-truncated-string)
    (concat str
            (make-string
             (- (+ helm-buffer-max-length
                   (length
                    helm-buffers-end-truncated-string))
                (string-width str))
             ? ))))

(defun pjones:helm-highlight-exwm-buffers (buffers _source)
  "Return Helm candidate string for BUFFERS."
  (cl-loop for i in buffers
           for buffer = (get-buffer i)
           for name = (pjones:helm-limited-string (buffer-name buffer))
           for class = (with-current-buffer buffer (pjones:helm-limited-string exwm-class-name))
           for title = (with-current-buffer buffer exwm-title)
           collect (cons (concat
                          (propertize name 'face 'helm-non-file-buffer)
                          helm-buffers-column-separator
                          (propertize class 'face 'helm-buffer-size)
                          helm-buffers-column-separator
                          (propertize title 'face 'helm-buffer-process))
                         buffer)))

(defun pjones:buffers-sans-exwm (buffers _source)
  "Filter BUFFERS so it doesn't include any EXWM windows."
  (cl-loop for i in buffers
           unless (with-current-buffer i (derived-mode-p 'exwm-mode))
           collect i))

(defun pjones:buffers-only-exwm (buffers _source)
  "Filter BUFFERS so it only includes EXWM windows."
  (cl-loop for i in buffers
           if (with-current-buffer i (derived-mode-p 'exwm-mode))
           collect i))

(defvar pjones:helm-source-buffers-list-sans-exwm
  (let ((source (helm-make-source "Buffers" 'helm-source-buffers)))
    (helm-attrset 'filtered-candidate-transformer
                  '(pjones:buffers-sans-exwm
                    helm-skip-boring-buffers
                    helm-buffers-sort-transformer
                    helm-highlight-buffers)
                  source)
    source)
  "Helm source of buffers without EXWM windows.")

(defvar pjones:helm-source-buffers-list-only-exwm
  (let ((source (helm-make-source "EXWM Windows" 'helm-source-buffers)))
    (helm-attrset 'filtered-candidate-transformer
                  '(pjones:buffers-only-exwm
                    helm-buffers-sort-transformer
                    pjones:helm-highlight-exwm-buffers)
                  source)
    source)
  "Helm source of buffers that only includes EXWM windows.")

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
  '(exwm-workspace-show-all-buffers t)
  '(exwm-layout-show-all-buffers t)
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

  ;; Helm helper:
  '(helm-mini-default-sources
    (quote (pjones:helm-source-buffers-list-sans-exwm
            pjones:helm-source-buffers-list-only-exwm
            helm-source-recentf)))

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

;;; exwm-conf.el ends here
