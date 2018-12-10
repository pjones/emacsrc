;;; interactive.el -- Interactive functions.
(eval-when-compile
  (require 'cl)                         ; for plusp (need to replace it)
  (require 'etags)
  (require 'flyspell)
  (require 'ispell)
  (require 'server)
  (require 'subword)
  (require 'term))

;; Dependencies:
(require 'hydra)

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

(defun pjones:exchange-point-and-mark (&optional arg)
  "Exchange point and mark without changing if the region is active.

If the region is active, keep it active.  If the region is
inactive, keep it inactive.  When ARG is non-nil, negate this
behavior."
  (interactive "P")
  (exchange-point-and-mark
   (if (region-active-p) arg
     (not arg))))

(defun pjones:switch-to-previous-buffer ()
  "Switch back to the last buffer shown in this window."
  (interactive)
  (let ((ido-process-ignore-lists t)
        (ido-ignored-list nil))
    (switch-to-buffer (car (ido-make-buffer-list nil)))))

(defun pjones:open-line-above (stay)
  "Open a line above point and move there if STAY is nil."
  (interactive "P")
  (let ((already-bol (bolp))
        (loc (set-marker (make-marker) (point))))
    (move-beginning-of-line nil)
    (open-line 1)
    (if stay (goto-char (marker-position loc))
      (when (not already-bol)
        (indent-according-to-mode)))))

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
  "Start IRC clients.

When LOCAL-ONLY is non-nil, only connect to Bitlbee."
  (interactive "P")
  (require 'erc) ; Loads in my erc-conf.el file
  (pjones:erc-bitlbee)
  (unless local-only (pjones:erc-freenode)))

(defun pjones:start-term (&optional dont-ask)
  "Start a new terminal buffer in the current project.

If DONT-ASK is non-nil, don't prompt for a project."
  (interactive "P")
  (let ((default-directory (pjones:projectile-project-root dont-ask)))
    (term "tmux")
    (pjones:term-rename)))

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
  (let ((default-directory "~/notes"))
    (org-agenda nil "c")))

(defun pjones:uuid ()
  "Create a UUID, add it to the kill ring, and insert it into the
current buffer after point."
  (interactive)
  (let ((uuid (replace-regexp-in-string "[\n-]" "" (shell-command-to-string "uuid"))))
    (kill-new uuid)
    (insert uuid)))

(defun pjones:lock-screen ()
  "Lock the screen."
  (interactive)
  (start-process "lock-screen" nil
                 "qdbus" "org.freedesktop.ScreenSaver"
                 "/ScreenSaver"
                 "Lock"))

(defun pjones:switch-window-then-delete ()
  "Interactively delete a window."
  (interactive)
  (let ((switch-window-threshold 1))
    (switch-window-then-delete)))

(defhydra hydra-window-ops (:hint nil :color blue)
  "
^Windows^           ^Config^             ^Themes/Fonts^
^^^------------------------------------------------------------
 _t_: transpose      _u_: undo            _T_: switch theme
 _r_: rotate         _U_: redo            _p_: frame scale up
 _d_: dired sidbar   _j_: save/restore    _n_: frame scale down
 ^ ^                 ^ ^                  _P_: buffer scale up
 ^ ^                 ^ ^                  _N_: buffer scale down
"
  ("T" pjones:load-theme)
  ("U" winner-redo)
  ("d" dired-sidebar-toggle-sidebar)
  ("j" pjones:window-config)
  ("r" rotate-layout)
  ("t" rotate-window)
  ("u" winner-undo)
  ("p" default-text-scale-increase :color red)
  ("n" default-text-scale-decrease :color red)
  ("P" text-scale-increase :color red)
  ("N" text-scale-decrease :color red))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
