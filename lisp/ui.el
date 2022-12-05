;;; ui.el -- User Interface and Theme Configuration.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Settings and functions that control the user interface.
;;
;;; Code:

(declare-function consult-theme "consult")

(defvar pjones:after-theme-change-hook nil
  "Hook run after changing themes.")

(defvar pjones:current-theme nil
  "The currently active theme.")

(custom-set-variables
 '(compilation-scroll-output 'first-error)
 '(describe-bindings-outline t)
 '(echo-keystrokes 0.1)
 '(enable-recursive-minibuffers t)
 '(focus-follows-mouse t)
 '(goto-line-history-local t)
 '(inhibit-startup-message t)
 '(initial-scratch-message nil)
 '(kill-buffer-delete-auto-save-files t)
 '(mark-even-if-inactive t)
 '(mouse-autoselect-window nil)
 '(mouse-yank-at-point t)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(show-paren-when-point-inside-paren t)
 '(tab-bar-close-button-show nil)
 '(tab-bar-new-button-show nil)
 '(tab-bar-new-tab-choice nil) ; Duplicate the current tab
 '(tab-bar-show nil) ; Only show the tab bar on request.
 '(tab-bar-tab-hints t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets)
 '(use-short-answers t)
 '(visible-bell nil)
 '(winum-auto-setup-mode-line nil)
 '(x-mouse-click-focus-ignore-position nil)
 '(x-underline-at-descent-line t))

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

(defun pjones:face-clone (dest src)
  "Clone the SRC face to DEST face, resetting inheritance."
  (set-face-attribute dest nil :inherit nil)
  (dolist (attr face-attribute-name-alist)
    (set-face-attribute
     dest nil (car attr)
     (face-attribute src (car attr)))))

;; Override some annoying faces from doom-themes:
(add-hook 'pjones:after-theme-change-hook
  (defun pjones:theme-fix-bad-faces ()
    ;; Force isearch and lazy-highlight faces to be as different as
    ;; possible so I can tell the difference quickly.
    (require 'cus-edit) ; Load the custom-button faces:
    (pjones:face-clone 'isearch 'custom-button-mouse)
    (pjones:face-clone 'lazy-highlight 'custom-button)))

;; Run my theme hooks after consult-theme:
(advice-add
 #'consult-theme :after
 (lambda (&rest _)
   (run-hooks 'pjones:after-theme-change-hook)))

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
    (concat "Emacs: " (or file (buffer-name)))))

(defun pjones:configure-new-frame (&optional _frame)
  "Hook to configure new frame FRAME."
  (let ((font-fixed "Hermit-10")
        (font-variable "Noto Serif-10"))
    (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
      (if (and (fboundp mode) (symbol-value mode))
          (funcall mode -1)))
    (blink-cursor-mode)
    (require 'fringe)
    (fringe-mode 10)
    (when (find-font (font-spec :name font-fixed))
      (custom-set-faces
       `(default ((t (:font ,font-fixed))))
       `(fixed-pitch ((t (:font ,font-fixed))))))
    (when (find-font (font-spec :name font-variable))
      (custom-set-faces
       `(variable-pitch ((t (:font ,font-variable))))))))

(add-to-list 'default-frame-alist '(cursor-type  . bar))
(setq frame-title-format '(:eval (pjones:frame-title-file-name)))

(defun pjones:find-file-hook ()
  "Hook called after a file is loaded into a buffer."
  ;; Encrypted files should start as read-only:
  (require 'epa-hook)
  (when (and buffer-file-name
             (string-match epa-file-name-regexp buffer-file-name))
    (read-only-mode 1)))

;; Adapted from: https://gitlab.com/jessieh/mood-line
(defun pjones:mode-line-align-right (construct)
  "Right-align a mode line construct.
CONSTRUCT should be a list, representing a mode line construct.
An appropriately wide string is prepend to the list, such that
the construct itself is pushed to the right side of the window.
The mode construct list is then returned."
  (let ((reserve (length (format-mode-line construct))))
    (cons
     (propertize " " 'display `((space :align-to (- right (- 0 right-margin) ,reserve))))
     construct)))

(defun pjones:mode-line-buffer ()
  "Return a `mode-line-format' component for the buffer name."
  (propertize "%b" 'face
              (if (or (derived-mode-p 'comint-mode)
                      (memq major-mode '(vterm-mode
                                         lisp-interaction-mode
                                         org-agenda-mode))
                      (not (buffer-modified-p)))
                  'mode-line-buffer-id
                'error)))

;; Mode line format:
(setq-default
 mode-line-buffer-identification nil

 mode-line-format
 '(" "
   ;; Show window numbers if there are enough windows:
   (:eval (if (and (fboundp 'winum--get-window-vector)
                             (> (length (winum--get-window-vector)) 2))
                        (format winum-format (winum-get-number-string))))
   ;; Buffer name, colored when modified:
   (:eval (or mode-line-buffer-identification (pjones:mode-line-buffer))) " "
   ;; Buffer position and size:
   mode-line-position
   (:eval (pjones:mode-line-align-right
           '(" "
             ;; Misc (and global) mode info:
             mode-line-misc-info
             ;; Major and minor modes:
             mode-line-modes " ")))))

;; Hooks:
(add-hook 'after-init-hook
  (defun pjones:set-initial-theme ()
    (pjones:load-theme 'doom-tokyo-night)))

(add-hook 'after-init-hook #'pjones:configure-new-frame)
(add-hook 'after-make-frame-functions #'pjones:configure-new-frame)
(add-hook 'find-file-hook 'pjones:find-file-hook)

;;; ui.el ends here
