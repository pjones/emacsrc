;;; vterm-conf.el -- Settings for `vterm'
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'ivy)
(require 'vterm)

(custom-set-variables
 '(vterm-max-scrollback 10000)
 '(vterm-kill-buffer-on-exit nil)
 '(vterm-buffer-name-string "vterm %s"))

(defvar-local pjones:vterm-kill-buffer-on-exit t
  "Smarter version of `vterm-kill-buffer-on-exit'.")
(put 'pjones:vterm-kill-buffer-on-exit 'permanent-local t)

(defun pjones:vterm-insert-mode ()
  "Put `vterm' back into insert mode."
  (interactive)
  (vterm-copy-mode -1))

(defun pjones:vterm-normal-mode ()
  "Put `vterm' into copy mode."
  (interactive)
  (vterm-copy-mode))

(defun pjones:vterm-insert-directory ()
  "Prompt for a directory and insert it into the vterm buffer."
  (interactive)
  (let ((dir (read-directory-name "Insert Dir Name: ")))
    (vterm-send-string dir t)))

(defun pjones:vterm-switch-buffer ()
  "Switch to another `vterm' buffer."
  (interactive)
  (let* ((cur (current-buffer))
         (ivy-use-virtual-buffers nil)
         (ivy-ignore-buffers
          (list
           (lambda (name)
             (when-let ((buf (get-buffer name)))
               (with-current-buffer buf
                 (or (eq cur buf)
                     (not (eq 'vterm-mode major-mode)))))))))
    (ivy-switch-buffer)))

(defmacro pjones:vterm-send-key (key)
  "Send KEY to `vterm'."
  `(lambda ()
     (interactive)
     (vterm-send ,key)))

(defun pjones:vterm (cmd &optional keep)
  "Open a terminal running CMD.
If KEEP is non-nil then don't close the buffer when the command
completes."
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (with-current-buffer buffer
      (setq pjones:vterm-kill-buffer-on-exit (not keep))
      (let ((vterm-shell cmd)) (vterm-mode)))
    (pop-to-buffer-same-window buffer)))

(defun pjones:vterm-exit-functions (buffer &rest _args)
  "Maybe kill the recently closed vterm BUFFER."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'pjones:vterm-kill-buffer-on-exit buffer))
    (let ((win (get-buffer-window buffer)))
      (if (and win (= 1 (length (window-list (window-frame win) nil))))
          (progn
            (delete-frame (window-frame win))
            (kill-buffer buffer))
        (with-current-buffer buffer
          (kill-buffer-and-window))))))

(defun pjones:vterm-mode-hook ()
  "Configure a new `vterm' buffer."
  (setq-local evil-move-cursor-back nil)

  ;; Switch between normal and copy mode when Evil changes:
  (make-local-variable 'evil-insert-state-entry-hook)
  (make-local-variable 'evil-insert-state-exit-hook)
  (add-hook 'evil-insert-state-entry-hook #'pjones:vterm-insert-mode)
  (add-hook 'evil-insert-state-exit-hook  #'pjones:vterm-normal-mode)

  ;; Use a smaller font in terminals by default:
  (text-scale-decrease 1))

(evil-set-initial-state 'vterm-mode 'insert)
(evil-set-initial-state 'vterm-copy-mode 'normal)

(evil-define-key 'insert vterm-mode-map
  (kbd "C-a") #'vterm--self-insert
  (kbd "C-b") #'vterm--self-insert
  (kbd "C-d") #'vterm--self-insert
  (kbd "C-e") #'vterm--self-insert
  (kbd "C-f") #'vterm--self-insert
  (kbd "C-j") (pjones:vterm-send-key "C-n")
  (kbd "C-k") (pjones:vterm-send-key "C-p")
  (kbd "C-l") #'vterm--self-insert
  (kbd "C-n") #'vterm--self-insert
  (kbd "C-p") #'vterm--self-insert
  (kbd "C-q") #'vterm--self-insert
  (kbd "C-r") #'vterm--self-insert
  (kbd "C-s") #'vterm--self-insert
  (kbd "C-t") #'vterm--self-insert
  (kbd "C-u") #'vterm--self-insert
  (kbd "C-v") #'vterm--self-insert
  (kbd "C-w") #'vterm--self-insert
  (kbd "C-y") #'vterm--self-insert
  (kbd "C-z") #'vterm--self-insert
  (kbd "C-/") #'pjones:vterm-insert-directory
  (kbd "<backspace>") #'vterm-send-backspace
  (kbd "<delete>") 'vterm-send-delete
  ;; Keys I've stolen, let them shine through after C-c:
  (kbd "C-c j") (pjones:vterm-send-key "C-j")
  (kbd "C-c k") (pjones:vterm-send-key "C-k")
  (kbd "C-c u") (pjones:vterm-send-key "C-u")
  (kbd "C-c /") (pjones:vterm-send-key "C-/")
  (kbd "C-c <escape>") (pjones:vterm-send-key "ESC"))

(evil-define-key 'normal vterm-copy-mode-map
  "gt" #'pjones:vterm-switch-buffer
  "p" #'vterm-yank ;; FIXME: probably needs to go into insert mode to work?
  "u" #'vterm-undo)

(evil-define-key 'motion vterm-copy-mode-map
  "[[" #'vterm-previous-prompt
  "]]" #'vterm-next-prompt)

(dolist (mode '(vterm-mode vterm-copy-mode))
  (evil-leader/set-key-for-mode mode
    "b b" #'switch-to-buffer-other-frame
    "f f" #'find-file-other-frame))

(add-hook 'vterm-mode-hook #'pjones:vterm-mode-hook)
(add-hook 'vterm-exit-functions #'pjones:vterm-exit-functions)

;;; vterm-conf.el ends here
