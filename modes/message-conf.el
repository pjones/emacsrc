;;; message-conf.el --- composing mail and news messages
;;; Commentary:
;;; Code:
(require 'company)
(require 'mml)
(require 'message)
(require 'hydra)

(defhydra hydra-message (:hint nil :color blue)
  "
^Actions^          ^Fields^
---------------------------------
 _b_: body         _f t_: to
 _i_: sig          _f o_: from
 _c_: send         _f b_: bcc
 _s_: strip        _f c_: cc
 _a_: attach       _f s_: subj
 _e_: encrypt
"
  ("b"   message-goto-body)
  ("i"   message-goto-signature)
  ("c"   message-send-and-exit)
  ("s"   message-kill-to-signature)
  ("a"   mml-attach-file)
  ("e"   mml-secure-message-sign-encrypt)
  ("f t" message-goto-to)
  ("f o" message-goto-from)
  ("f b" message-goto-bcc)
  ("f c" message-goto-cc)
  ("f s" message-goto-subject))

(defun pjones:message-mode-hook ()
  "Configure message mode to my liking."
  ;; Configure completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell)

  ;; Key bindings:
  (local-set-key (kbd "C-c h")   #'hydra-message/body)
  (local-set-key (kbd "C-c C-s") #'message-kill-to-signature)
  (local-set-key (kbd "C-c C-e") #'mml-secure-message-sign-encrypt))

(add-hook 'message-mode-hook #'pjones:message-mode-hook)

(provide 'message-conf)
;;; message-conf.el ends here
