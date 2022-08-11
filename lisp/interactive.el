;;; interactive.el -- Interactive functions.
;;
;;; Commentary:
;;
;;; Code:

(require 'flycheck)
(require 'flymake)

(declare-function pjones:erc-freenode "../modes/erc-conf.el")
(declare-function pjones:erc-bitlbee "../modes/erc-conf.el")
(declare-function pjones:projectile-project-root "../modes/projectile-conf.el")

(declare-function puni-kill-active-region "puni")
(declare-function puni-kill-line "puni")
(declare-function vterm "vterm")

(defvar puni-mode)

(defun pjones:maybe-save-buffers-kill-terminal (&optional arg)
  "A function to save me from myself.
I somehow keep hitting \\[kill-emacs] when I don't want to.
ARG is passed to `save-buffers-kill-terminal'"
  (interactive)
  (if (yes-or-no-p "Really close this terminal? ")
      (save-buffers-kill-terminal arg)))

(defun pjones:kill-region-or-backward-kill-word (arg)
  "Replacement for `kill-region' to kill ARG preceding words.
If there is a region with \\[transient-mark-mode] active, it will
be removed and placed in the kill ring in a similar manner to
`kill-region'.  If there isn't a region, the word before point
will be deleted (without placing it in the kill ring)."
  (interactive "p")
  (require 'subword)
  (let ((forward
         (if (and (boundp 'subword-mode)
                  (or subword-mode global-subword-mode))
             'subword-forward
           'forward-word)))
    (if (use-region-p)
        (if (= 0 arg) (puni-kill-active-region)
          (kill-region (region-beginning) (region-end)))
      (delete-region (point) (progn
                               (funcall forward (- arg))
                               (point))))))

(defun pjones:kill-line (arg)
  "Kill from point to the end of the line.
If point is at the end of a line, kill the entire line.  ARG is
passed on to `kill-line'."
  (interactive "P")
  (if (or arg (not (looking-at-p "\\s-*$")))
      (if puni-mode
          (progn
            (puni-kill-line arg)
            (indent-according-to-mode))
        (kill-line arg))
    (kill-whole-line)))

(defun pjones:rename-current-file (newname)
  "Rename the current file to NEWNAME."
  (interactive "F")
  (let ((oldname (buffer-file-name)))
    (when oldname
      (dired-rename-file oldname newname nil))))

(defun pjones:open-line-above (below)
  "Open a line above point and move there.
If BELOW is non-nil, open a line below point instead."
  (interactive "P")
  (let ((already-bol (bolp)))
    (if below (progn
                (move-end-of-line nil)
                (newline))
      (move-beginning-of-line nil)
      (open-line 1))
    (when (not already-bol)
      (indent-according-to-mode))))

(defun pjones:start-irc (&optional local-only)
  "Start IRC clients.

When LOCAL-ONLY is non-nil, only connect to Bitlbee."
  (interactive "P")
  (require 'erc) ; Loads in my erc-conf.el file
  (pjones:erc-bitlbee)
  (unless local-only (pjones:erc-freenode)))

(defun pjones:start-term ()
  "Start a new terminal buffer."
  (interactive)
  (if (file-remote-p default-directory)
      (let ((default-directory (expand-file-name "~/")))
        (call-interactively #'vterm))
    (call-interactively #'vterm)))

(defun pjones:start-http ()
  "Create a new buffer running `http-mode'."
  (interactive)
  (let ((buf (get-buffer-create "*http*")))
    (with-current-buffer buf (http-mode))
    (switch-to-buffer buf)))

(defun pjones:pwgen (&optional word)
  "Generate and insert a password.
If WORD is non-nil then generate a simple password."
  (interactive "P")
  (let* ((opts (if word "20 1" "-cnsyB 15 1"))
         (pw (replace-regexp-in-string "\n" ""
              (shell-command-to-string (concat "pwgen " opts)))))
    (insert pw)))

(defun pjones:kill-file-name (&optional full-path)
  "Kill the base name of the buffer's file.
When FULL-PATH is non-nil kill the entire path for the file."
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
  "Start `org-agenda' with my custom agenda view."
  (interactive)
  (require 'org)
  (let ((default-directory "~/notes/"))
    (org-agenda nil "c")))

(defun pjones:uuid ()
  "Create a UUID, add it to the kill ring, and insert it after point."
  (interactive)
  (let* ((raw (shell-command-to-string "uuid"))
         (uuid (replace-regexp-in-string "[\n-]" "" raw)))
    (kill-new uuid)
    (insert uuid)))

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
    (pjones:flymake-goto-next-error))))

(defun pjones:rectangle-number-lines ()
  "Call `rectangle-number-lines' with prefix argument set."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'rectangle-number-lines)))

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

(defun pjones:sort-lines (beg end)
  "Sort the region from BEG to END."
  (interactive "r")
  (let ((sort-fold-case t))
    (save-excursion
      (sort-lines nil beg end))))

(defun pjones:exchange-point-and-mark (&optional arg)
  "Exchange point and mark without alerting region state.
If the region is active, keep it active.  If the region is
inactive, keep it inactive.  When ARG is non-nil, negate this
behavior."
  (interactive "P")
  (exchange-point-and-mark
   (if (region-active-p) arg
     (not arg))))

(defun pjones:duplicate-region-or-line (&optional count)
  "Duplicate the current line, or region if active.
The line/region will be duplicated COUNT times."
  (interactive "p")
  (let ((indent
         (save-excursion
           (if (region-active-p) (goto-char (region-beginning)))
           (back-to-indentation)
           (current-column)))
        (col (current-column))
        (text
         (if (region-active-p)
             (buffer-substring-no-properties
              (region-beginning)
              (region-end))
           (save-excursion
             (back-to-indentation)
             (buffer-substring-no-properties
              (point)
              (progn (end-of-line) (point)))))))
    (if (region-active-p)
      (progn
        (goto-char (region-end))
        (deactivate-mark))
      (end-of-line))
    (dotimes (n count)
      (newline)
      (indent-to indent)
      (insert text))
    (move-to-column col)))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; interactive.el ends here
