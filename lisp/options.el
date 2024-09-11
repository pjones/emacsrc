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
 '(epa-file-encrypt-to "4D0CD0756F1B8B9D3DCD0CAAE1CF584F79D0D3DC")
 '(inhibit-eol-conversion t)
 '(make-backup-files nil)
 '(server-client-instructions nil)
 '(vc-follow-symlinks t)) ; Don't warn prompt me about symlinks!

;;; options.el ends here
