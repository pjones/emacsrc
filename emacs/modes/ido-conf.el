;;; ido-conf.el -- Settings for ido-mode.
(eval-when-compile (require 'ido))

(defun pjones:ido-setup-hook ()
  "Override some weird ido stuff using the set up hook."
  (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir))

;; Insert my set up hook.
(add-hook 'ido-setup-hook 'pjones:ido-setup-hook)

;; ido settings
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-max-prospects 10)
