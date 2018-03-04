;;; ido-conf.el -- Settings for ido-mode.
(eval-when-compile
(require 'ido))

;; ido customization:
(custom-set-variables
  '(ido-auto-merge-work-directories-length nil)
  '(ido-create-new-buffer 'always)
  '(ido-enable-flex-matching t)
  '(ido-enable-prefix nil)
  '(ido-max-prospects 10)
  '(ido-use-filename-at-point 'guess)
  '(ido-use-virtual-buffers t))

(defun pjones:ido-setup-hook ()
  "Override some weird ido stuff using the set up hook."
  (flx-ido-mode 1)
  (ido-everywhere)
  (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir))

;; Insert my set up hook.
(add-hook 'ido-setup-hook 'pjones:ido-setup-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
