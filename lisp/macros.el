;;; macros.el -- Custom Lisp macros.
;;
;;; Commentary:
;;
;;; Code:
(defmacro pjones:jump-to-buffer (name &optional command)
  "Generate a command to jump to buffer NAME.
If buffer NAME doesn't exist, COMMAND can be used to create it."
  `(defun ,(intern (concat "pjones:jump-to-buffer-" name)) ()
    (interactive)
    (let ((buf (get-buffer ,name)))
      (if buf (switch-to-buffer buf)
        ,(if command `(,command) 'nil)))))

(defmacro pjones:evil-override-mode (mode &rest bindings)
  "Override standard evil bindings for the given MODE.
MODE-map should be a mode's keymap that would normally conflict with
evil bindings.  Use BINDINGS to move those keys to alternatives."
  (declare (indent defun))
  (let ((keymap (intern (concat (symbol-name mode) "-map"))))
    `(progn
       (evil-make-overriding-map ,keymap 'normal t)
       (evil-set-initial-state (quote ,mode) 'normal)
       (evil-define-key 'normal ,keymap
         "\C-v" (lookup-key evil-motion-state-map "\C-v")
         "G" (lookup-key evil-motion-state-map "G")
         "gg" (lookup-key evil-motion-state-map "gg")
         "h" (lookup-key evil-motion-state-map "h")
         "j" (lookup-key evil-motion-state-map "j")
         "k" (lookup-key evil-motion-state-map "k")
         "l" (lookup-key evil-motion-state-map "l")
         "N" (lookup-key evil-motion-state-map "N")
         "n" (lookup-key evil-motion-state-map "n")
         "V" (lookup-key evil-motion-state-map "V")
         "v" (lookup-key evil-motion-state-map "v")
         "Y" (lookup-key evil-normal-state-map "Y")
         "y" (lookup-key evil-normal-state-map "y")
         ":" (lookup-key evil-motion-state-map ":")
         "/" (lookup-key evil-motion-state-map "/")
         "?" (lookup-key evil-motion-state-map "?")
         ,@bindings))))

;;; macros.el ends here
