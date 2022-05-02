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
 '(custom-file (concat user-emacs-directory "custom-init.el"))
 '(disabled-command-function nil)
 '(epa-file-encrypt-to "B3CDF5E4B27790430DACE07F526722D1204284CB")
 '(inhibit-eol-conversion t)
 '(make-backup-files nil)
 '(next-line-add-newlines t)
 '(vc-follow-symlinks t)) ; Don't warn prompt me about symlinks!

;;; options.el ends here
