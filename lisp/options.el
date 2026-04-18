;;; options.el -- Emacs settings not tied to any one mode. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

;; Personal information
(setq user-full-name "Peter J. Jones"
      user-mail-address (string-join '("peter" "jonesbunch.com") "@"))

;; Settings not worth their own file in the modes directory:
(custom-set-variables
 '(comint-terminfo-terminal "xterm-256color")
 '(custom-file (concat user-emacs-directory "custom-init.el"))
 '(disabled-command-function nil)
 '(epa-file-encrypt-to "4D0CD0756F1B8B9D3DCD0CAAE1CF584F79D0D3DC")
 '(epa-keys-select-method 'minibuffer)
 '(history-delete-duplicates t)
 '(inhibit-eol-conversion t)
 '(mail-user-agent 'mu4e-user-agent)
 '(make-backup-files nil)
 '(server-client-instructions nil)
 '(trusted-content '("~/src/rc/"))
 '(vc-follow-symlinks t)) ; Don't warn prompt me about symlinks!

;; Mark some file variables as safe:
(setopt safe-local-variable-values
        '((eval add-hook 'after-save-hook #'org-babel-tangle nil t)))

;; Purely for performance:
;;
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/
(setq-default
 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right)

(setopt
 bidi-inhibit-bpa t
 redisplay-skip-fontification-on-input t
 read-process-output-max (* 4 1024 1024))

;;; options.el ends here
