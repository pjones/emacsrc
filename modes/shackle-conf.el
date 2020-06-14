;;; shackle-conf.el -- Settings for shackle.
;;
;;; Commentary:
;;
;;; Code:
;;
;; https://github.com/wasamasa/shackle
(require 'shackle)

(defun pjones:shackle-upper-right (buffer alist plist)
  "Split the right-most window and put BUFFER there.
ALIST is passed to display functions.  PLIST is ignored."
  (let ((current (window-at (frame-width) 0)))
    (when current
      (let ((new (split-window current nil 'above)))
        (when new
          ;; This makes it so quitting the buffer closes the window:
          (window--display-buffer buffer new 'window alist nil)
          new)))))

(defun pjones:shackle-make-window (buffer alist plist)
  "Make a window for BUFFER.
If the :same-mode key in PLIST is non-nil, try to reuse a window that is
already showing a buffer with the same mode as BUFFER.  If the
:upper-right key is set, place buffer in the upper-right corner.
Otherwise split the current window.  ALIST is passed to display
functions."
  (or (and (plist-get plist :same-mode)
           (display-buffer-reuse-mode-window buffer alist))
      (and (plist-get plist :upper-right)
           (pjones:shackle-upper-right buffer alist plist))
      (display-buffer-below-selected buffer alist)))

(defun pjones:shackle-make-frame (buffer alist plist)
  "Make a frame for BUFFER.
Follow the rules in `pjones:shackle-make-window' for PLIST and ALIST."
  (add-to-list 'alist (cons 'reusable-frames 'visible))
  (add-to-list 'alist (cons 'pop-up-frame-parameters
                            (list (cons 'name "popup"))))
  (unless (plist-get plist :select)
    (add-to-list 'alist (cons 'inhibit-switch-frame t)))
  (or (and (plist-get plist :same-mode)
           (display-buffer-reuse-mode-window buffer alist))
      (display-buffer-reuse-window buffer alist)
      (display-buffer-pop-up-frame buffer alist)
      (pjones:shackle-make-window buffer alist plist)))

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
       "\\*evil-owl\\*"
       "\\*eglot-help"
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

     ;; Modes that force a new frame:
     ((term-mode
       haskell-interactive-mode)
      :select t
      :dedicated t
      :frame t
      :custom pjones:shackle-split)

     ;; Windows that should split the current window but *not* get focus:
     (("magit-diff: "
       "\\*HTTP Response.*")
      :regexp t
      :select nil
      :same-mode t
      :custom pjones:shackle-split)

     ;; A very special rule, windows that should be placed in the upper
     ;; right-hand corner of the frame but not take the focus:
     ((compilation-mode
       grep-mode)
      :select nil
      :same-mode t
      :frame t
      :custom pjones:shackle-split))))

;;; shackle-conf.el ends here
