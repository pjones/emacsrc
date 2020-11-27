;;; shackle-conf.el -- Settings for shackle. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
;;
;; https://depp.brause.cc/shackle/
(require 'shackle)

(defun pjones:shackle-make-window (buffer alist plist)
  "Make a window for BUFFER.

If the :same-mode key in PLIST is non-nil, try to reuse a window that is
already showing a buffer with the same mode as BUFFER.

Otherwise split the current window.  ALIST is passed to display
functions."
  (or (and (plist-get plist :same-mode)
           (display-buffer-reuse-mode-window buffer alist))
      (display-buffer-below-selected buffer alist)))

(defun pjones:shackle-make-frame (buffer alist plist)
  "Make a frame for BUFFER.
Follow the rules in `pjones:shackle-make-window' for PLIST and ALIST."
  (let ((params (list
                 (cons 'name "popup")
                 (cons 'unsplittable (plist-get plist :dedicated)))))
    (push (cons 'reusable-frames 'visible) alist)
    (push (cons 'pop-up-frame-parameters params) alist)
    (unless (plist-get plist :select)
      (push (cons 'inhibit-switch-frame t) alist))
    (or (and (plist-get plist :same-mode)
             (display-buffer-reuse-mode-window buffer alist))
        (display-buffer-reuse-window buffer alist)
        (display-buffer-pop-up-frame buffer alist)
        (pjones:shackle-make-window buffer alist plist))))

(defun pjones:shackle-split (buffer alist plist)
  "Split the current window to display BUFFER.
This calls `pjones:shackle-make-window' to create a window and then
decides if the window should be selected by checking the :select key
in PLIST.  ALIST is passed to display functions."
  (let* ((current (selected-window))
         (window (if (plist-get plist :frame)
                     (pjones:shackle-make-frame buffer alist plist)
                   (pjones:shackle-make-window buffer alist plist))))
    (when window
      (set-window-dedicated-p window (plist-get plist :dedicated))
      (select-window (if (plist-get plist :select) window current)))))

;; Settings:
(custom-set-variables
 '(shackle-inhibit-window-quit-on-same-windows t)
 '(shackle-default-rule (quote (:same t)))

 '(shackle-rules
   '(;; Modes that should get their own windows, but remain inactive:
     ((magit-diff-mode)
      :select nil
      :same nil)

     ;; Windows that should split the entire frame:
     (("\\*Completions\\*"
       "\\*Deletions\\*"
       "\\*Flycheck error"
       "\\*flymake "
       "\\*evil-owl\\*"
       "\\*eglot-help"
       "\\*eldoc\\*"
       calendar-mode)
      :regexp t
      :size 0.3
      :align 'below
      :select nil)

     ;; Windows that should split the current window and get focus.  If
     ;; there's a window already showing a buffer with the same mode then
     ;; reuse that buffer instead of creating a new one.
     (("\\*magit-.*popup"
       "\\*transient"
       "\\*eww buffers\\*"
       "\\*Backtrace\\*"
       "\\*JS scratch\\*"
       help-mode
       pdf-outline-buffer-mode)
      :regexp t
      :select t
      :same-mode t
      :custom pjones:shackle-split)

     ;; Windows that should split the current window but *not* get focus:
     (("magit-diff: "
       "\\*HTTP Response.*")
      :regexp t
      :select nil
      :same-mode t
      :custom pjones:shackle-split)

     ;; Modes that force a new (raised and focused) frame:
     ((term-mode
       haskell-interactive-mode)
      :select t
      :dedicated t
      :frame t
      :custom pjones:shackle-split)

     ;; Modes that share a frame that is never raised.
     ((compilation-mode
       grep-mode
       rg-mode)
      :select nil
      :same-mode t
      :dedicated t
      :frame t
      :custom pjones:shackle-split))))

;;; shackle-conf.el ends here
