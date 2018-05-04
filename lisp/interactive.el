;;; interactive.el -- Interactive functions.
(eval-when-compile
  (require 'cl)                         ; for plusp (need to replace it)
  (require 'etags)
  (require 'flyspell)
  (require 'ispell)
  (require 'linum)
  (require 'server)
  (require 'subword)
  (require 'term))

(defun pjones:maybe-save-buffers-kill-terminal (&optional arg)
  "Save me from myself.  I somehow keep hitting C-x C-c when I
don't want to."
  (interactive)
  (if (yes-or-no-p "Really close this terminal? ")
      (save-buffers-kill-terminal arg)))

(defun pjones:kill-region-or-backward-kill-word (arg)
  "Replacement for `kill-region'.  If there is a region with
`transient-mark-mode' active, it will be removed and placed in
the kill ring in a similar manner to `kill-region'.  If there
isn't a region, the word before point will be deleted (without
placing it in the kill ring)."
  (interactive "p")
  (let ((forward
    (if (and (boundp 'subword-mode)
             (or subword-mode global-subword-mode))
        'subword-forward
      'forward-word)))
    (if (or (not transient-mark-mode) (and transient-mark-mode mark-active))
        (kill-region (region-beginning) (region-end))
      (delete-region (point) (progn (funcall forward (- arg))
                                    (point))))))

(defun pjones:switch-to-previous-buffer ()
  "Switch back to the last buffer shown in this window."
  (interactive)
  (let ((ido-process-ignore-lists t)
        (ido-ignored-list nil))
    (switch-to-buffer (car (ido-make-buffer-list nil)))))

(defun pjones:open-line-above (stay)
  "Open a line above point and move there if STAY is nil."
  (interactive "P")
  (let ((already-bol (bolp)))
    (move-beginning-of-line nil)
    (open-line 1)
    (if stay (move-end-of-line 2))
    (when (not already-bol)
      (indent-according-to-mode))))

(defun pjones:open-line-below ()
  "Open a line below the point, and move there"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun pjones:move-beginning-of-line ()
  "Move back to indentation or the first column."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    (when (= start (point)) (move-beginning-of-line 1))))

(defun pjones:start-mail ()
  "Start an instance of mu4e."
  (interactive)
  (require 'mu4e)
  (mu4e))

(defun pjones:start-irc (&optional local-only)
  "Start IRC client.  With an argument only start a connection to
the local bitlbee instance."
  (interactive "P")
  (require 'circe) ; Loads in my circe-conf.el file
  (if local-only (circe-maybe-connect "bitlbee")
    (circe-maybe-connect "bitlbee")
    (circe-maybe-connect "freenode")))

(defun pjones:start-term ()
  "Start a new terminal buffer."
  (interactive)
  (term "zsh")
  (rename-buffer (generate-new-buffer-name "term")))

(defun pjones:start-http ()
  "Create a new buffer running `http-mode'."
  (interactive)
  (let ((buf (get-buffer-create "*http*")))
    (with-current-buffer buf (http-mode))
    (switch-to-buffer buf)))

(defun pjones:pwgen (&optional word)
  "Generate and insert a password."
  (interactive "P")
  (let* ((opts (if word "20 1" "-cnsyB 15 1"))
         (pw (replace-regexp-in-string "\n" ""
              (shell-command-to-string (concat "pwgen " opts)))))
    (insert pw)))

(defvar pjones:last-dictionary nil
  "The last non-English dictionary used by `pjones:toggle-dictionary'.")

(defun pjones:auto-correct-previous-word ()
  "Use flyspell to automatically correct the previous word
without going past the current line."
  (interactive)
  (let ((start (save-excursion
                 (beginning-of-line)
                 (point)))
        (end (save-excursion
               (end-of-line)
               (point))))
    (save-restriction
      ;; Keep flyspell from trying to correct a previous word outside
      ;; of the region.  It does this when there is no incorrect word
      ;; within the current region :(
      (when (and flyspell-auto-correct-previous-pos
                 (or (> flyspell-auto-correct-previous-pos end)
                     (< flyspell-auto-correct-previous-pos start)))
        (setq flyspell-auto-correct-previous-pos nil))
      (narrow-to-region start end)
      (flyspell-auto-correct-previous-word (point)))))

(defun pjones:toggle-dictionary (&optional reset)
  "Switch between English and another language easily.  Switches
the current input method and dictionary.  With a prefix argument
prompts for the foreign language to use."
  (interactive "P")
  (let* ((en "english")
         ;;         Human        Dict       Input Method
         (langs '(("Italian" . ("italian"  "italian-postfix"))
                  ("French"  . ("francais" "french-postfix"))))
         (names (mapcar 'car langs))
         (last (if (or reset (not pjones:last-dictionary))
                   (ido-completing-read "Lang: " names) pjones:last-dictionary))
         (cur-dict (or (and reset en) ispell-current-dictionary en))
         (new-dict (if (string= cur-dict en) last en)))
    (setq pjones:last-dictionary last)
    (ispell-change-dictionary (cadr (assoc new-dict langs)))
    (activate-input-method (car (cddr (assoc new-dict langs))))))

(defun pjones:insert-italian-name ()
  "Helper function for when I'm writing Italian dialog."
  (interactive)
  (let* ((names '("Alessandra" "Piera" "Carlotta" "Stefano"
                  "Valentina" "Paolo"))
         (pick (ido-completing-read "Name: " names)))
    (insert (concat "*" pick ":* "))))

(defun pjones:journal ()
  "Open an entry in the journal for today."
  (interactive)
  (let* ((base (expand-file-name "~/documents/journal/"))
         (path (downcase (format-time-string "%Y/%m/%d-%A.md")))
         (full (concat base path))
         (dir (file-name-directory full)))
    (unless (file-directory-p dir) (make-directory dir t))
    (find-file full)
    (when (= (buffer-size) 0)
      (insert (format-time-string "%% %A, %B %d, %Y\n\n"))
      (insert-file-contents (concat base "template.md")))))

(defun pjones:bookmark (&optional set)
  "Quicker access to Emacs bookmarks.  Prompts for a bookmark and
then jumps to that bookmark.  If a prefix argument is given, set
a bookmark instead."
  (interactive "P")
  (if set (call-interactively 'bookmark-set)
    (call-interactively 'bookmark-jump)))

(defun pjones:inc-file ()
  "Given that the current file name is a number, increment that
number and open a file with the incremented number as a name."
  (interactive)
  (let* ((base (file-name-nondirectory buffer-file-name))
         (dir  (file-name-directory buffer-file-name))
         (name (file-name-sans-extension base))
         (ext  (file-name-extension base t))
         (num  (string-to-number name))
         (fmt  (concat "%0" (number-to-string (length name)) "d"))
         (new  (format fmt (1+ num))))
    (find-file (concat dir new ext))))

(defun pjones:window-config (&optional set)
  "Simple shortcut for remembering a window configuration and
then jumping back to it later.  With a prefix argument, save the
current window configuration.  Otherwise restore the window
configuration."
  (interactive "P")
  (if set (window-configuration-to-register ?.)
    (jump-to-register ?.)))

(defun pjones:push-tag-mark (&optional jump)
  "Pushes the current location of point onto the tags mark ring
so you can pop back later with `M-,'.  When JUMP is non-nil jump
to the previous tag mark.  This allows you to jump back and forth
between two points."
  (interactive "P")
  (require 'etags)
  (let ((mark (point-marker)))
    (if jump (pop-tag-mark))
    (xref-push-marker-stack mark)))

(defun pjones:jump-back-and-forth (&optional record)
  "Jump between two points.  With a prefix argument, set the
destination for the next jump to point."
  (interactive "P")
  (if record (point-to-register ?~)
    (let ((old (get-register ?~)))
      (point-to-register ?~)
      (switch-to-buffer (marker-buffer old))
      (goto-char old))))

(defun pjones:register-get-set (register start end &optional set)
  "General purpose register function.

Inserts or jumps to REGISTER, unless SET is non-nil.  If SET is
non-nil then either set a register or record the current point.
If the mark is active the use START and END to record the current
mark in REGISTER.  Otherwise record the current point in
REGISTER."
  (interactive (list (register-read-with-preview "Register: ")
                     (and mark-active (region-beginning))
                     (and mark-active (region-end))
                     current-prefix-arg))
    (cond
     ((and set mark-active)
      (set-register register (filter-buffer-substring start end)))
     (set
      (set-register register (point-marker)))
     (t
      (let ((val (get-register register)))
        (if (markerp val) (progn (pjones:push-tag-mark)
                                 (switch-to-buffer (marker-buffer val)))
          (insert-register register t))))))

(defun pjones:kill-file-name (&optional full-path)
  "Create a new kill containing the base name of the buffer's
file.  With a prefix argument kill the entire path for the file."
  (interactive "P")
  (let* ((path (buffer-file-name))
         (name (file-name-nondirectory path)))
    (kill-new (if full-path path name))))

(defun pjones:agenda ()
  "Start org-agenda with my custom agenda view"
  (interactive)
  (require 'org)
  (org-agenda nil "c"))

;; Stolen from: http://whattheemacsd.com//key-bindings.el-01.html
(defun pjones:goto-line-with-feedback ()
  "Show line numbers temporarily while prompting for the line
number input."
  (interactive)
  (require 'linum)
  (let ((showing-line-numbers linum-mode))
    (unwind-protect (progn
      (unless showing-line-numbers (linum-mode 1))
      (call-interactively 'goto-line))
      (unless showing-line-numbers (linum-mode -1)))))

(defun pjones:zap-to-quote (&optional backward)
  "Delete characters up to next/previous quote based on BACKWARD.

If BACKWARD is non-nil delete backward instead of forward."
  (interactive "P")
  (let* ((re "\\('\\|\"\\)")
         (start (point))
         (end (save-excursion
                (if backward (progn (search-backward-regexp re nil t)
                                    (forward-char 1))
                  (search-forward-regexp re nil t)
                  (backward-char 1))
                (point))))
    (if backward (kill-region end start)
      (kill-region start end))))

(defun pjones:uuid ()
  "Create a UUID, add it to the kill ring, and insert it into the
current buffer after point."
  (interactive)
  (let ((uuid (replace-regexp-in-string "[\n-]" "" (shell-command-to-string "uuid"))))
    (kill-new uuid)
    (insert uuid)))

(defun pjones:toggle-theme ()
  "Switch between my dark theme and my light theme."
  (interactive)
  (enable-theme (cadr custom-enabled-themes)))

(defun pjones:switch-window-then-delete ()
  "Interactively delete a window."
  (interactive)
  (let ((switch-window-threshold 1))
    (switch-window-then-delete)))

(defhydra hydra-launch (:hint nil :color blue)
  "
^OrgMode^      ^Indium^        ^Other^
-----------------------------------------
 _a_: agenda    _j c_: chrome   _m_: mail
 _c_: capture   _j n_: node     _i_: irc
 _s_: store     ^ ^             _h_: http
"
  ("a"   pjones:agenda)
  ("c"   org-capture)
  ("s"   org-store-link)
  ("j c" pjones:indium-start-chrome)
  ("j n" pjones:indium-start-node)
  ("m"   pjones:start-mail)
  ("i"   pjones:start-irc)
  ("h"   pjones:start-http))

(defhydra hydra-window-ops (:hint nil :color blue)
  "
^Windows^        ^Config^          ^Sidebar^    ^Theme^
^^^^^^^^^^^-------------------------------------------------
 _t_: transpose   _u_: undo         _d_: dired   _T_: switch
 _r_: rotate      _U_: redo
 ^ ^              _j_: save/restore
"
  ("T" pjones:toggle-theme)
  ("U" winner-redo)
  ("d" dired-sidebar-toggle-sidebar)
  ("j" pjones:window-config)
  ("r" rotate-layout)
  ("t" rotate-window)
  ("u" winner-undo))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
