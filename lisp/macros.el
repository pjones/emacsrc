;;; macros.el -- Custom Lisp macros.
;;
;;; Commentary:
;;
;;; Code:
(defmacro pjones:jump-to-buffer (name)
  "Generate a command to jump to buffer NAME."
  `(defun ,(intern (concat "pjones:jump-to-buffer-" name)) ()
    (interactive)
    (let ((buf (get-buffer ,name)))
      (when buf (switch-to-buffer buf)))))

(defmacro pjones:evil-override-mode (mode &rest bindings)
  "Override standard evil bindings for the given MODE.
MODE-map should be a mode's keymap that would normally conflict with
evil bindings.  Use BINDINGS to move those keys to alternatives."
  (declare (indent defun))
  (let ((keymap (intern (concat (symbol-name mode) "-map"))))
    `(progn
       (evil-make-overriding-map ,keymap 'normal)
       (evil-set-initial-state (quote ,mode) 'normal)
       (evil-define-key 'normal ,keymap
         "h" (lookup-key evil-motion-state-map "h")
         "j" (lookup-key evil-motion-state-map "j")
         "k" (lookup-key evil-motion-state-map "k")
         "l" (lookup-key evil-motion-state-map "l")
         "n" (lookup-key evil-motion-state-map "n")
         "N" (lookup-key evil-motion-state-map "N")
         "G" (lookup-key evil-motion-state-map "G")
         "gg" (lookup-key evil-motion-state-map "gg")
         ":" (lookup-key evil-motion-state-map ":")
         "/" (lookup-key evil-motion-state-map "/")
         "?" (lookup-key evil-motion-state-map "?")
         ,@bindings))))

;;; macros.el ends here
