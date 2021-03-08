;;; flymake-conf.el -- Settings for `flymake'
;;
;;; Commentary:
;;
;;; Code:
(require 'flymake)

(eval-when-compile
  (require 'subr-x))

(custom-set-faces
 '(flymake-error ((t (:underline nil))))
 '(flymake-warning ((t (:underline nil)))))

(defvar pjones:flymake-message-buffer "*flymake message*"
  "Buffer name used to display flymake messages.")

(defvar pjones:flymake-last-jump-marker (make-marker)
  "The last place that `pjones:flymake-goto-next-error' took us.")

(defvar pjones:flymake-message-distance 10
  "How far point can travel away from a flymake error.
If point goes past this distance the flymake window will be closed.")

(defun pjones:flymake-goto-next-error nil
  "Jump to and display the next flymake error."
  (interactive)
  (set-marker pjones:flymake-last-jump-marker nil)
  (flymake-goto-next-error)
  (let ((err (get-char-property (point) 'flymake-diagnostic))
        (max-mini-window-height 0.15))
    (when err
      (set-marker pjones:flymake-last-jump-marker (point))
      (display-message-or-buffer
       (concat (flymake--diag-text err) "\n\n")
       pjones:flymake-message-buffer))))

(defun pjones:flymake-maybe-bury-buffer nil
  "If point has moved away from an error, close the flymake window."
  (when-let ((flymake-buf (get-buffer pjones:flymake-message-buffer)))
    (let ((flymake-win (get-buffer-window flymake-buf))
          (error-buf (marker-buffer pjones:flymake-last-jump-marker))
          (pos (marker-position pjones:flymake-last-jump-marker)))
      (when (or (not (eq (current-buffer) error-buf))
                (> (abs (- (point) pos)) pjones:flymake-message-distance))
        (if flymake-win (delete-window flymake-win))
        (bury-buffer flymake-buf)
        (set-marker pjones:flymake-last-jump-marker nil)))))

(defun pjones:flymake-mode-hook nil
  "Respond to a change in `flymake-mode' status."
  (when flymake-mode
    (add-hook 'post-command-hook #'pjones:flymake-maybe-bury-buffer nil t)))

(add-hook 'flymake-mode-hook #'pjones:flymake-mode-hook)

;;; flymake-conf.el ends here
