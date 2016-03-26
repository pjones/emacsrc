;;; functions.el -- Non-interactive functions
;;; Commentary:
;;; Code:

;; Silence a compiler warning
;; (declare-function org-clock-get-clock-string "org")
;; (declare-function festival-say-region "festival")

;; (defun pjones:org-clock-time ()
;;   "Returns a formatted org clock time if currently clocked in."
;;   (if (and (fboundp 'org-clocking-p) (org-clocking-p))
;;       (substring-no-properties (org-clock-get-clock-string)) ""))
;; (defalias 'pmade:org-clock-time 'pjones:org-clock-time)

(defun pjones:urgency-hint (frame status)
  (let* ((wm-hints (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
         (flags (car wm-hints)))
    (setcar wm-hints (if status (logior flags #x00000100) (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t))
  (x-send-client-message ; This is for Wayland EWHM changes.
     frame 0 frame "_NET_WM_STATE" 32
     '(1 "_NET_WM_STATE_DEMANDS_ATTENTION" 0)))

;; (defun pjones:text-to-speech-para ()
;;   "Read the current paragraph."
;;   (interactive)
;;   (save-excursion
;;     (let* ((r-end (progn (forward-paragraph) (point)))
;;            (r-start (progn (backward-paragraph) (point))))
;;       (festival-say-region r-start r-end))))

(defun pjones:define-keys-from-hydra (keymap heads)
  "Define keys in KEYMAP from HEADS."
  (dolist (head heads)
    (define-key keymap (kbd (car head)) (cadr head))))

(provide 'functions)
;;; functions.el ends here
