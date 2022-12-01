;;; embark-conf.el -- Settings for `embark' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'embark)
(require 'embark-consult)
(require 'which-key)

(defun pjones:embark-indicator (map)
  "Show the embark MAP via `which-key'."
  (let ((which-key-side-window-location 'bottom))
    (which-key--show-keymap "Embark" map nil nil 'no-paging))
  #'which-key--hide-popup-ignore-command)

;; Partially stolen from:
;; https://protesilaos.com/dotemacs/
(defun pjones:embark-collect-fit-window ()
  "Hook to resize the `embark-collect-completions' window."
  (when-let* ((buffer (current-buffer))
              (name (buffer-name buffer))
              (window (get-buffer-window buffer)))
    (when (string-match-p "Embark Collect \\(Live\\|Completions\\)" name)
      (fit-window-to-buffer window (floor (frame-height) 2) 1))))

;; https://github.com/oantolin/embark/wiki/Additional-Configuration
(defun pjones:embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

;; https://github.com/oantolin/embark/wiki/Additional-Configuration
(defun pjones:embark-hide-which-key-indicator (fn &rest args)
  "Hide the `which-key' indicator.
FN and ARGS are the advised function and original arguments."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'pjones:embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'pjones:embark-hide-which-key-indicator)

(custom-set-variables
 '(embark-indicators '(pjones:embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
 '(embark-prompter #'embark-keymap-prompter)
 '(embark-collect-view 'grid)
 '(embark-action-indicator #'pjones:embark-indicator)
 '(embark-become-indicator #'pjones:embark-indicator)
 '(embark-collect-initial-view-alist '((t . grid))))

(add-hook
 'embark-collect-post-revert-hook
 #'pjones:embark-collect-fit-window)

;;; embark-conf.el ends here
