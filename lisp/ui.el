;;; ui.el -- User Interface and Theme Configuration.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Settings and functions that control the user interface.
;;
;;; Code:

(require 'dbus)

(declare-function consult-theme "consult")

(defvar pjones:dark-theme 'ef-maris-dark
  "Default dark theme.")

(defvar pjones:light-theme 'ef-maris-light
  "Default light theme.")

(defvar pjones:after-theme-change-hook nil
  "Hook run after changing themes.")

(defvar pjones:current-theme nil
  "The currently active theme.")

(defvar pjones:frame-alpha-background 90
  "Default frame alpha background value.")

(custom-set-variables
 '(calc-kill-line-numbering nil)
 '(compilation-scroll-output 'first-error)
 '(describe-bindings-outline t)
 '(echo-keystrokes 0.1)
 '(enable-recursive-minibuffers t)
 '(ffap-require-prefix t)
 '(font-lock-maximum-decoration t)
 '(goto-line-history-local t)
 '(inhibit-startup-message t)
 '(initial-scratch-message nil)
 '(kill-buffer-delete-auto-save-files t)
 '(mark-even-if-inactive t)
 '(mouse-autoselect-window nil)
 '(mouse-yank-at-point t)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(save-interprogram-paste-before-kill t)
 '(save-place-file (concat user-emacs-directory "places"))
 '(show-paren-context-when-offscreen 'overlay)
 '(show-paren-when-point-inside-paren t)
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-bar-new-tab-choice "*scratch*")
 '(tab-bar-show 1) ; Only show the tab bar when needed.
 '(tab-bar-tab-hints t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets)
 '(use-short-answers t)
 '(visible-bell nil)
 '(winum-auto-setup-mode-line nil)
 '(x-mouse-click-focus-ignore-position nil)
 '(x-underline-at-descent-line t)

 ;; Settings for various themes:
 '(catppuccin-enlarge-headings nil)
 '(catppuccin-highlight-matches t)
 '(dracula-enlarge-headings nil))

;; Default variables that become buffer/frame local.
(setq-default
 cursor-in-non-selected-windows 'hollow ; Self-explanatory
 indicate-buffer-boundaries 'left       ; Fringe stuff
 indicate-empty-lines t                 ; Ditto
 require-final-newline t                ; Always end files with \n
 indent-tabs-mode nil                   ; Don't use tabs
 truncate-lines t                       ; Don't wrap lines
 display-line-numbers-width 3)          ; Faster default.

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
    (error nil)))

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

(defun pjones:theme-from-dbus (value)
  "Change the theme based on a D-Bus property.

VALUE should be an integer or an arbitrarily nested list that
contains an integer.  When VALUE is equal to 2 then a light theme
will be selected, otherwise a dark theme will be selected."
  (pjones:load-theme
   (if (= 2 (car (flatten-list value)))
       pjones:light-theme
     pjones:dark-theme)))

(defun pjones:theme-fixups (&rest _)
  "Fix some UI elements after a changing themes."
  ;; Step 1: Emulate spacious-padding for the mode line:
  (let* ((base (custom-face-attributes-get 'mode-line nil))
         (faces '(mode-line
                  mode-line-active
                  mode-line-inactive
                  mode-line-highlight))
         (or-base (lambda (spec attr)
                    (let ((val (plist-get spec attr)))
                      (or (and (stringp val) val)
                          (plist-get base attr))))))
    (dolist (face faces)
      (let ((spec (custom-face-attributes-get face nil)))
        (custom-set-faces
         `(,face ((t (:background ,(funcall or-base spec :background)
                      :foreground ,(funcall or-base spec :foreground)
                      :box (:line-width 6
                            :color ,(funcall or-base spec :background)
                            :style nil))))))))))

(add-hook 'enable-theme-functions #'pjones:theme-fixups)

;; Run my theme hooks:
(advice-add
 #'load-theme :after
 (lambda (theme &rest _)
   (setq pjones:current-theme theme)
   (run-hooks 'pjones:after-theme-change-hook)))

(advice-add
 #'enable-theme :after
 (lambda (theme)
   (setq pjones:current-theme theme)
   (run-hooks 'pjones:after-theme-change-hook)))

(defun pjones:frame-toggle-alpha nil
  "Toggle alpha transparency for the current frame."
  (interactive)
  (set-frame-parameter
   nil 'alpha-background
   (if (eq (frame-parameter nil 'alpha-background) pjones:frame-alpha-background)
       nil pjones:frame-alpha-background)))

(defun pjones:frame-title-file-name ()
  "How to format frame titles."
  (let* ((home (expand-file-name "~"))
         (end (length home))
         (start (and buffer-file-name (substring buffer-file-name 0 end)))
         (under-home (and start (string= home start)))
         (file (cond (under-home
                      (concat "~/" (file-relative-name buffer-file-name "~")))
                     (buffer-file-name buffer-file-name)
                     (dired-directory
                      (if (listp dired-directory) (car dired-directory) dired-directory)))))
    (concat "Emacs: "
            (or file (buffer-name))
            (format " [%s]" (frame-parameter (selected-frame) 'window-id)))))

(defun pjones:configure-new-frame (&optional frame)
  "Hook to configure new frame FRAME."
  ;; Remove the "name" parameter which may have been set by one of my
  ;; buffer management functions.  Without this the "name" parameter
  ;; will be used as the frame title which isn't what I want.
  (set-frame-parameter frame 'name nil)
  (let ((font-fixed "Hermit")
        (font-variable "IBM Plex Serif"))
    (dolist (mode '(menu-bar-mode
                    tab-bar-mode
                    tool-bar-mode
                    scroll-bar-mode))
      (if (and (fboundp mode) (symbol-value mode))
          (funcall mode -1)))
    (blink-cursor-mode)
    (require 'fringe)
    (fringe-mode 10)
    (when (find-font (font-spec :name font-fixed))
      (set-face-attribute 'default nil :family font-fixed :height 100)
      (set-face-attribute 'fixed-pitch nil :family font-fixed))
    (when (find-font (font-spec :name font-variable))
      (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif"))))

(add-to-list 'default-frame-alist '(cursor-type  . bar))
(setq frame-title-format '(:eval (pjones:frame-title-file-name)))

(defun pjones:mode-line-buffer ()
  "Return a `mode-line-format' component for the buffer name."
  (propertize "%b" 'face
              (if (and (buffer-file-name) (buffer-modified-p)) 'error
                'mode-line-buffer-id)))

;; Mode line format:
(setq-default
 mode-line-buffer-identification nil

 mode-line-format
 '(;; Show window numbers if there are enough windows:
   (:eval (if (and (fboundp 'winum--get-window-vector)
                             (> (length (winum--get-window-vector)) 2))
                        (format winum-format (winum-get-number-string))))

   ;; Meow state:
   (:eval (meow-indicator))

   ;; Buffer name, colored when modified:
   (:eval (or mode-line-buffer-identification (pjones:mode-line-buffer))) " "

   ;; Buffer position and size:
   mode-line-position

   ;; Now the right side:
   mode-line-format-right-align

   ;; Misc (and global) mode info:
   mode-line-misc-info

   ;; Major and minor modes:
   mode-line-modes " "))

;; Hooks:
(add-hook 'after-init-hook
  (defun pjones:set-initial-theme ()
    "Set the theme on start up."
    ;; Make a dbus method call to find the current color scheme:
    ;;
    ;; dbus-send --session --print-reply --dest=org.freedesktop.portal.Desktop \
    ;;   /org/freedesktop/portal/desktop org.freedesktop.portal.Settings.Read \
    ;;   string:org.freedesktop.appearance string:color-scheme
    (dbus-call-method-asynchronously
     :session "org.freedesktop.portal.Desktop"
     "/org/freedesktop/portal/desktop"
     "org.freedesktop.portal.Settings"
     "Read"
     #'pjones:theme-from-dbus
     "org.freedesktop.appearance"
     "color-scheme")
    ;; Register to be notified when the theme changes in the future:
    (dbus-register-signal
     :session "org.freedesktop.portal.Desktop"
     "/org/freedesktop/portal/desktop"
     "org.freedesktop.portal.Settings"
     "SettingChanged"
     (lambda (path var value)
       (when (and (string-equal path "org.freedesktop.appearance")
                  (string-equal var "color-scheme"))
         (pjones:theme-from-dbus value))))))

(add-hook 'after-init-hook #'pjones:configure-new-frame)
(add-hook 'after-make-frame-functions #'pjones:configure-new-frame)

;;; ui.el ends here
