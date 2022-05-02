;;; options.el -- Emacs settings not tied to any one mode.
;;
;;; Commentary:
;;
;;; Code:

;; Personal information
(setq user-full-name "Peter J. Jones"
      user-mail-address "pjones@devalot.com")

;; Settings not worth their own file in the modes directory:
(custom-set-variables
 '(auth-sources '(password-store))    ; Use pass(1) for passwords.
 '(custom-file (concat user-emacs-directory "custom-init.el"))
 '(disabled-command-function nil)
 '(epa-file-encrypt-to "B3CDF5E4B27790430DACE07F526722D1204284CB")
 '(inhibit-eol-conversion t)
 '(make-backup-files nil)
 '(next-line-add-newlines t)
 '(vc-follow-symlinks t)) ; Don't warn prompt me about symlinks!

(add-hook 'after-init-hook #'auth-source-pass-enable)

;;; options.el ends here
