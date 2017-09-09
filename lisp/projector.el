;;; projector.el -- A minor mode and some functions for presentations.
;;
;;; Commentary:
;;
;;
;;; Code:
(eval-when-compile
  (require 'highline))

(defvar pjones:projector-font-big
  "Dejavu Sans Mono-18"
  "A large font to use for projectors.")

(defvar pjones:projector-font-prev
  "Dejavu Sans Mono-9"
  "Intial font used.")

(defvar pjones:projector-scratch-mode-line nil
  "The `mode-line-format' in *scratch* before we fuck it up.")

(defun pjones:projector-font (&optional font)
  "Switch the current frame to FONT or a big font."
  (interactive)
  (set-frame-font (or font pjones:projector-font-big)))

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


(define-minor-mode projector-mode
  "Global minor mode that makes things look nice on a projector."
  :global t
  (if projector-mode
      (progn (setq pjones:projector-font-prev
                   (or (alist-get 'fonts default-frame-alist)
                       pjones:projector-font-prev))
             (with-current-buffer "*scratch*"
               (setq pjones:projector-scratch-mode-line mode-line-format
                     mode-line-format nil))
             (pjones:projector-font)
             (global-linum-mode 1))
    (with-current-buffer "*scratch*"
      (setq mode-line-format pjones:projector-scratch-mode-line))
    (pjones:projector-font pjones:projector-font-prev)
    (global-linum-mode -1)))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
(provide 'projector)
;;; projector.el ends here
