;;; gnus.el -- Settings that need to configured before Gnus can run.
(eval-when-compile
  (require 'gnus)
  (require 'gnus-start)
  (require 'gnus-agent)
  (require 'nnfolder)
  (require 'nndraft))

;; Bring in an external Gnus, but first we need to set some location
;; variables otherwise Gnus will load and create the wrong
;; directories.
(setq mail-default-directory (expand-file-name "~/.gnus.d/")
      gnus-startup-file (concat mail-default-directory "newsrc")
      gnus-directory (concat mail-default-directory "news")
      gnus-agent-directory (concat mail-default-directory "agent")
      message-directory (concat mail-default-directory "mail")
      nnfolder-directory (concat mail-default-directory "mail")
      nndraft-directory (concat mail-default-directory "mail"))
