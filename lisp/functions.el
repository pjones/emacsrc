;;; functions.el -- Non-interactive functions
;;; Commentary:
;;; Code:
(defun pjones:urgency-hint (frame status)
  (let* ((wm-hints (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints (if status (logior flags #x00000100) (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t))
  (x-send-client-message ; This is for Wayland EWHM changes.
     frame 0 frame "_NET_WM_STATE" 32
     '(1 "_NET_WM_STATE_DEMANDS_ATTENTION" 0)))

(defun pjones:plasmoid-update ()
  "Update a KDE plasmoid that is blocked reading from a FIFO."
  (let ((inhibit-message t)
        (file (concat user-emacs-directory "plasmoid"))
        (desktop (exwm-nw--format exwm-workspace-current-index))
        (eyebrowse (let* ((slot (eyebrowse--get 'current-slot))
                          (cfgs (eyebrowse--get 'window-configs)))
                     (eyebrowse-format-slot (assoc slot cfgs)))))
    (with-temp-buffer
      (insert (concat desktop " | " eyebrowse "\n"))
      (write-region nil nil file t))))

(defun pjones:define-keys-from-hydra (keymap heads)
  "Define keys in KEYMAP from HEADS."
  (dolist (head heads)
    (define-key keymap (kbd (car head)) (cadr head))))

(provide 'functions)
;;; functions.el ends here
