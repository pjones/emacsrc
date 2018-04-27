;;; rcirc-conf.el -- Settings for rcirc.
(eval-when-compile
  (require 'cl)
  (require 'rcirc))

;; Silence compiler warnings
(declare-function rcirc-omit-mode "rcirc")
(declare-function rcirc-track-minor-mode "rcirc")
(declare-function rcirc-nick "rcirc")
(declare-function rcirc-server-name "rcirc")

(defvar pjones-irc-password nil
  "My password for connecting to ZNC.")

;; Load passwords from a private file
(load "~/keys/emacs/secrets.el")

(setq rcirc-default-nick "pmade"
      rcirc-default-user-name "pmade"
      rcirc-default-full-name "Peter Jones"
      rcirc-multiline-major-mode 'markdown-mode
      rcirc-fill-flag t)

;; List of servers to connect to.
::
;; N.B. The first server is the one used when the pjones:irc function
;; is called with a prefix argument.
(setq rcirc-server-alist
      `(("bitlbee.pmade.com"
        :nick "pjones" :user-name "pjones/bitlbee"
        :port 6697 :encryption tls
        :password ,pjones-irc-password)
        ("freenode.pmade.com"
         :nick "pjones" :user-name "pjones/freenode"
         :port 6697 :encryption tls
         :password ,pjones-irc-password)))

;; Other interesting Freenode channels:
;;
;; #hakyll -- Hakyll static site generator
;; #gnus   -- Gnus news/mail reader
;; #debian -- Obvious
;; #mpd    -- Music player daemon

(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
      rcirc-buffer-maximum-lines 500)

(defvar pjones:rcirc-buffers
  '("#derailed@irc.freenode.net"
    "#xmonad@irc.freenode.net"
    "#conkeror@irc.freenode.net"
    "#emacs@irc.freenode.net"
    "&bitlbee@localhost"
    "#haskell@irc.freenode.net")
  "A list of rcirc buffer names in the order in which they should
be placed into the current set of windows.")

(defvar pjones:rcirc-low-traffic-channels
  '("#haskell-mobile@irc.freenode.net"
    "#hakyll@irc.freenode.net"
    "#derailed@irc.freenode.net"
    "#boulderhs@irc.freenode.net")
  "A list of IRC channels that have low enough traffic that it's
okay to send me notifications of activity.")

(defun pjones:rcirc-windows ()
  "Split the current frame into several windows and place the
buffers listed in `pjones:rcirc-buffers' in each of the resulting
windows, in the correct order."
  (interactive)
  (let ((wins (window-list nil nil (frame-first-window))))
    (when (= 1 (length wins))
      (split-window-right)
      (dolist (w (window-list))
        (select-window w)
        (dotimes (i 2) (split-window-below)))
      (balance-windows))
      (setq wins (window-list nil nil (frame-first-window)))
      (cl-mapcar 'set-window-buffer wins pjones:rcirc-buffers)))

(defun pjones:rcirc-cmd-all (input)
  "See the docs for rcirc-cmd-all."
  (let ((buffers (mapcar 'process-buffer (rcirc-process-list))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "/" input)
        (rcirc-send-input)))))

(defun-rcirc-command all (input)
  "Run the arguments as a command for all connections.
Example use: /all away food or /all quit zzzz."
  (interactive "s")
  (pjones:rcirc-cmd-all input))

(defun pjones:rcirc-quit ()
  "Quit all rcirc connections."
  (interactive)
  (let ((buffers (mapcar 'process-buffer (rcirc-process-list))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (rcirc-cmd-quit "bye.")))))

(defun pjones:rcirc-macrumors ()
  "Connect to the macrumors IRC server."
  (interactive)
  (rcirc-connect "irc.macrumorslive.com" nil nil nil nil "#macrumors"))

(defun pjones:rcirc-update-fill-column (&optional window)
  "Update `rcirc-fill-column' based on the width of WINDOW, or
the current window if WINDOW is nil."
  (with-current-buffer (window-buffer window)
    (if (eq major-mode 'rcirc-mode)
        (setq rcirc-fill-column (- (window-width window) 2)))))

(defun pjones:rcirc-update-fill-column-all-windows (&optional frame)
  "Call `pjones:rcirc-update-fill-column' for all windows."
  (walk-windows 'pjones:rcirc-update-fill-column 'no-minibuf frame))

(defun pjones:rcirc-hook ()
  (require 'rcirc-color)
  (when (and (string-match "#" (buffer-name))
             (not (string-match "developers\\|twitter" (buffer-name)))
             (not (member (buffer-name) pjones:rcirc-low-traffic-channels)))
    (setq rcirc-ignore-buffer-activity-flag t)
    (rcirc-omit-mode))
  (define-key rcirc-mode-map (kbd "C-c C-o") 'rcirc-browse-url)
  (set (make-local-variable 'scroll-conservatively) 8192)
  (set (make-local-variable 'next-line-add-newlines) nil)
  (setq mode-line-format '("  %b " global-mode-string)
        wrap-prefix "  "
        rcirc-prompt "❯ ")
  (pjones:rcirc-update-fill-column)
  (flyspell-mode)
  (visual-line-mode)
  (rcirc-update-prompt)
  (rcirc-track-minor-mode))

(defun pjones:rcirc-activity-string ()
  (when (string= "[]" rcirc-activity-string)
    (setq rcirc-activity-string "")))

(defun pjones:rcirc-notify (sender text)
  "Display an activity notification."
  (pjones:urgency-hint (selected-frame) t))

(defun pjones:rcirc-print-hook (proc sender response target text)
  "Hook called when a new IRC message is received.  If the
message is either a PRIVMSG or mentions your nick, use the
notification function.  This only happens if the target buffer is
not currently displayed in a window."
  (let ((buf (rcirc-get-buffer proc target)))
    (when (and
           (or (and (string= response "PRIVMSG")
                    (not (string= (rcirc-nick proc) sender))
                    (not (rcirc-channel-p target)))
               (and (string-match (rcirc-nick proc) text)
                    (not (string= (rcirc-nick proc) sender))
                    (not (string= (rcirc-server-name proc) sender)))))
;           (not (get-buffer-window buf)))
      (pjones:rcirc-notify sender text))))

(add-hook 'rcirc-mode-hook 'pjones:rcirc-hook)
(add-hook 'rcirc-update-activity-string-hook 'pjones:rcirc-activity-string)
(add-hook 'rcirc-print-hooks 'pjones:rcirc-print-hook)
(add-hook 'window-size-change-functions 'pjones:rcirc-update-fill-column-all-windows)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions noruntime)
;; End:
