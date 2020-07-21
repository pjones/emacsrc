;;; interactive.el -- Interactive functions.
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'cl)) ; for plusp (need to replace it)
(require 'flycheck)
(require 'flymake)

(declare-function pjones:erc-freenode "../modes/erc-conf.el")
(declare-function pjones:erc-bitlbee "../modes/erc-conf.el")
(declare-function pjones:projectile-project-root "../modes/projectile-conf.el")

(defun pjones:maybe-save-buffers-kill-terminal (&optional arg)
  "Save me from myself.  I somehow keep hitting C-x C-c when I
don't want to."
  (interactive)
  (if (yes-or-no-p "Really close this terminal? ")
      (save-buffers-kill-terminal arg)))

(defun pjones:switch-to-previous-buffer ()
  "Switch back to the last buffer shown in this window."
  (interactive)
  (require 'ido)
  (let ((previous-place (evil-alternate-buffer)))
    (if previous-place
        (progn
          (switch-to-buffer (car previous-place))
          (goto-char (car (last previous-place))))
      (let ((ido-process-ignore-lists t)
            (ido-ignored-list nil))
        (switch-to-buffer (car (ido-make-buffer-list nil)))))))

(defun pjones:rename-current-file (newname)
  "Rename the current file to NEWNAME."
  (interactive "F")
  (let ((oldname (buffer-file-name)))
    (when oldname
      (dired-rename-file oldname newname nil))))

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

(defun pjones:backward-delete-char (&optional count)
  "Delete a character backwards.
If the all of the characters before point are space characters then
delete COUNT levels of indentation."
  (interactive "p")
  (let ((sofar (save-excursion (beginning-of-line) (point))))
    (if (or (bolp) (looking-back "^\\s-$" sofar))
        (evil-shift-left-line count)
      (evil-delete-backward-char-and-join count))))

(defun pjones:start-mail ()
  "Start an instance of mu4e."
  (interactive)
  (require 'gnus)
  (gnus))

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
  (require 'term)
  (let* ((default-directory (pjones:projectile-project-root dont-ask))
         (short (directory-file-name (file-relative-name default-directory "~/")))
         (name (generate-new-buffer-name (concat "term:" short)))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (term-mode)
      (term-exec buffer name (getenv "SHELL") nil nil)
      (term-char-mode))
    (pop-to-buffer buffer)))

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

(defun pjones:kill-file-name (&optional full-path)
  "Create a new kill containing the base name of the buffer's
file.  With a prefix argument kill the entire path for the file."
  (interactive "P")
  (let* ((path (buffer-file-name))
         (name (file-name-nondirectory path)))
    (kill-new (if full-path path name))))

(defun pjones:kill-whole-buffer ()
  "Kill the entire buffer contents."
  (interactive)
  (kill-new
   (buffer-substring-no-properties
    (point-min)
    (point-max))))

(defun pjones:kill-directory-name ()
  "Put the current directory name into the kill ring."
  (interactive)
  (kill-new
   (concat "~/" (file-relative-name default-directory "~"))))

(defun pjones:agenda ()
  "Start org-agenda with my custom agenda view"
  (interactive)
  (require 'org)
  (let ((default-directory "~/notes/"))
    (org-agenda nil "c")))

(defun pjones:uuid ()
  "Create a UUID, add it to the kill ring, and insert it into the
current buffer after point."
  (interactive)
  (let ((uuid (replace-regexp-in-string "[\n-]" "" (shell-command-to-string "uuid"))))
    (kill-new uuid)
    (insert uuid)))

(defun pjones:switch-window-then-delete ()
  "Interactively delete a window."
  (interactive)
  (let ((switch-window-threshold 1))
    (switch-window-then-delete)))

(defun pjones:open-temp-buffer ()
  "Open/create a temporary buffer for writing."
  (interactive)
  (let ((buf (get-buffer-create "*write*")))
    (with-current-buffer buf
      (markdown-mode)
      (pjones:markdown-visual-line))
    (pop-to-buffer buf)))

(defun pjones:fly-next-error ()
  "Go to the next fly(check|make) error."
  (interactive)
  (cond
   (flycheck-mode
    (flycheck-next-error))
   (flymake-mode
    (flymake-goto-next-error)
    (let ((err (get-char-property (point) 'flymake-diagnostic)))
      (when err
        (display-message-or-buffer
         (concat (flymake--diag-text err) "\n\n")
         "*flymake message*" 'not-this-window))))))

;; Adapted from: https://github.com/hlissner/doom-emacs/
(defun pjones:next-file (n)
  "Return the name of the Nth next file."
  (unless buffer-file-name
    (user-error "Must be called from a file-visiting buffer"))
  (let* ((directory (file-name-directory buffer-file-name))
         (filename (file-name-nondirectory buffer-file-name))
         (files (directory-files
                 (file-name-directory buffer-file-name) nil
                 (rx bol (not (in ?.)))))
         (index (cl-position filename files :test #'file-equal-p)))
    (when (null index)
      (user-error "Couldn't find this file in current directory"))
    (let ((index (+ index n)))
      (cond ((>= index (length files))
             (user-error "No files after this one"))
            ((< index 0)
             (user-error "No files before this one"))
            ((expand-file-name (nth index files) directory))))))

(defun pjones:find-file-next (count)
  "Call `find-file' with COUNT next file."
  (interactive "p")
  (find-file (pjones:next-file count)))

(defun pjones:find-file-prev (count)
  "Call `find-file' with COUNT previous file."
  (interactive "p")
  (find-file (pjones:next-file (- count))))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
