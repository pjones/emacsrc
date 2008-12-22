(eval-when-compile
  (load-file "pmade-loadpath.el"))

;; Load in my passwords
(setq pmade-comm-sync "~/.comm-sync")
(load (concat pmade-comm-sync "/etc/erc/erc.el"))

;; Basic IRC Settings
(setq erc-user-full-name "Peter Jones")
(setq erc-email-userid "pjones@pmade.com")
(setq erc-nick "pmade")

;; ERC Time stamps
(setq erc-timestamp-only-if-changed-flag nil)
(setq erc-timestamp-format "[%H:%M:%S] ")
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left)

;; Auto-fill (static size so log files look decent)
(setq erc-fill-column 78)
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-static-center 15)

;; Ignore messages from the server that are not channel activity
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude '("&bitlbee" "#emacs" "#ruby" "#applescript"))

;; Auto join the given channels
(setq erc-autojoin-channels-alist
      '(("freenode" "#gnus" "#erc" "#emacs")
        ("macrumors" "#macrumors")))

;; Logging
(setq erc-log-channels-directory (concat pmade-comm-sync "/log/erc"))
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil)
(setq erc-log-write-after-send t)
(setq erc-log-write-after-insert t)
(setq erc-enable-logging 'pmade-log-enabled)
(setq pmade-log-exclude '("#macrumors"))

;; Some other settings
(setq erc-prompt 'pmade-erc-prompt)
(setq erc-max-buffer-size 20000)
(setq erc-track-showcount t)
(setq erc-join-buffer 'buffer)
(setq erc-auto-query 'bury)             ; Private messages go to a hidden buffer
(setq erc-query-display 'buffer)        ; Reuse current buffer when sending private messages
(setq erc-keywords '("pmade" "peter"))
(setq erc-kill-queries-on-quit nil)

;; Identify with IRC servers
(defun pmade-erc-after-connect (server nick)
  (cond
   ((string-match "localhost" server) (erc-message "PRIVMSG" (concat "&bitlbee identify "     bitlbee-password)))
   ((string-match "freenode"  server) (erc-message "PRIVMSG" (concat "NickServ identify "     freenode-password)))
   ((string-match "pmade"     server) (erc-message "PRIVMSG" (concat "userserv login pjones " pmade-irc-pjones-password)))))

;; Setup ERC buffers
(defun pmade-erc-hook ()
  "Correctly configure ERC buffers"
  (auto-fill-mode 0)
  (setq truncate-lines nil)
  (local-set-key "\C-c\C-o" 'pmade-erc-open-last-link))

;; Mac Rumors
(defun macrumors-irc ()
  "Connect to the Mac Rumors IRC server"
  (interactive)
  (erc :server "irc.macrumorslive.com"))

;; Better Prompt
(defun pmade-erc-prompt ()
  (if (and (boundp 'erc-default-recipients) (erc-default-target))
      (erc-propertize (concat "[ " (erc-default-target) " ]") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
    (erc-propertize (concat "[ ERC ]") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))

;; Control which buffers get logged
(defun pmade-log-enabled (buffer)
  (cond
   ((not (erc-log-all-but-server-buffers buffer)) nil)
   ((member (buffer-name buffer) erc-track-exclude) nil)
   ((member (buffer-name buffer) pmade-log-exclude) nil)
   (t t)))

;; Open links easier
(defun pmade-erc-open-last-link (arg)
  (interactive "*p")
  (save-excursion
    (search-backward "http" nil nil arg)
    (erc-button-press-button)))

;; Bring ERC in
(add-to-list 'load-path (concat pmade-site-lisp "/erc"))
(require 'erc)

;; Load in some ERC extra modules (you must download these separately)
(require 'erc-highlight-nicknames)

;; Add some modules
(add-to-list 'erc-modules 'spelling)
(add-to-list 'erc-modules 'scrolltobottom)
(add-to-list 'erc-modules 'truncate)
(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'highlight-nicknames)
(erc-update-modules)

;; Hook in
(add-hook 'erc-mode-hook 'pmade-erc-hook)
(add-hook 'erc-after-connect 'pmade-erc-after-connect)

;; Start a local bitlbee server
(require 'bitlbee)
(setq bitlbee-user-directory "~/.comm-sync/etc/bitlbee")
(setq bitlbee-executable "/opt/local/sbin/bitlbee")
(bitlbee-start)
(sleep-for 1) ;; Give bitlbee a chance to bind to the local port
