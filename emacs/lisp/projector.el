;;; projector.el -- Functions for projecting.
(eval-when-compile
  (require 'highline))

(defvar pjones:projector-font-big
  "-unknown-DejaVu Sans Mono-normal-normal-normal-*-32-*-*-*-m-0-iso10646-1"
  "A large font to use for projectors.")

(defun pjones:projector-font ()
  "Switch the current frame to a big font."
  (interactive)
  (set-frame-font pjones:projector-font-big))

(defun pjones:projector-highline ()
  "Toggle highline mode from outside Emacs."
  (with-current-buffer (window-buffer (selected-window))
    (call-interactively 'highline-mode)))

(defun pjones:projector-next-line ()
  "Move to the next line from outside Emacs."
  (with-current-buffer (window-buffer (selected-window))
    (call-interactively 'next-line)
    (if highline-mode (highline-highlight-current-line))))

(defun pjones:projector-prev-line ()
  "Move to the previous line from outside Emacs."
  (with-current-buffer (window-buffer (selected-window))
    (call-interactively 'previous-line)
    (if highline-mode (highline-highlight-current-line))))

(defun pjones:projector-recenter ()
  "Places the current line at the top of the window from outside
Emacs."
  (with-current-buffer (window-buffer (selected-window))
    (recenter 0)
    (if highline-mode (highline-highlight-current-line))))

(defun pjones:projector-other-window ()
  "Move to the other window from outside Emacs."
  (select-window (next-window nil 0 'visible))
  (if highline-mode (highline-highlight-current-line)))
