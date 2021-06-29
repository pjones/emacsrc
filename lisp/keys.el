;;; keys.el -- Global key bindings.
;;
;;; Commentary:
;;
;; Key bindings follow the style used in Spacemacs.
;;
;;; Code:
(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "macros"))
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "interactive")))

;; Loading `link-hint' will also load my settings and custom functions
;; for it.
(autoload 'pjones:link-hint-open-link "link-hint")

;; Autoloads for neuron-mode:
(autoload 'pjones:rg-zettel-dir "neuron-mode")
(autoload 'pjones:zettel-need-to-do "neuron-mode")
(autoload 'pjones:zettel-open-inbox "neuron-mode")
(dolist (f '(neuron-new-zettel
             neuron-edit-zettel
             neuron-open-daily-notes
             neuron-select-zettelkasten
             neuron-open-zettel
             neuron-open-index))
  (autoload f "neuron-mode"))

(defun pjones:global-set-keys (key def &rest bindings)
  "Call `global-set-key' for KEY and DEF and each pair in BINDINGS."
  (while key
    (global-set-key key def)
    (setq key (pop bindings)
          def (pop bindings))))

(pjones:global-set-keys
 (kbd "C-c a") (pjones:jump-to-buffer "*Org Agenda*" pjones:agenda)
 (kbd "C-c C") #'full-calc
 (kbd "C-c c") #'quick-calc
 (kbd "C-c d") #'eldoc-doc-buffer
 (kbd "C-c E") #'embark-act
 (kbd "C-c f") (pjones:jump-to-buffer "*flymake message*")
 (kbd "C-c h") #'pjones:start-http
 (kbd "C-c l o") #'link-hint-open-link
 (kbd "C-c l w") #'link-hint-copy-link
 (kbd "C-c M") #'magit-file-dispatch
 (kbd "C-c m") #'magit-status
 (kbd "C-c M-W") #'pjones:kill-directory-name
 (kbd "C-c M-w") #'pjones:kill-file-name
 (kbd "C-c n") #'pjones:start-mail
 (kbd "C-c p g") #'pjones:pwgen
 (kbd "C-c p l") #'passmm-list-passwords
 (kbd "C-c p p") #'passmm-completing-read
 (kbd "C-c R") #'pjones:rename-current-file
 (kbd "C-c r") #'revert-buffer
 (kbd "C-c s") #'pjones:sort-lines
 (kbd "C-c t") #'pjones:open-temp-buffer
 (kbd "C-c w /") #'winner-undo
 (kbd "C-c w R") #'resize-window
 (kbd "C-c w r") #'rotate-layout
 (kbd "C-c w s") #'ace-swap-window
 (kbd "C-c w S") #'window-toggle-side-windows
 (kbd "C-c w u") #'winner-undo
 (kbd "C-c z d") #'neuron-open-daily-notes
 (kbd "C-c z f") #'neuron-edit-zettel
 (kbd "C-c z s") #'neuron-select-zettelkasten
 (kbd "C-c z z") #'neuron-new-zettel

 ;; Additional searching/grepping bindings:
 (kbd "M-s d") #'rg
 (kbd "M-s f") #'consult-find
 (kbd "M-s G") #'consult-git-grep
 (kbd "M-s g") #'consult-grep
 (kbd "M-s h g") #'highlight-indent-guides-mode
 (kbd "M-s h L") #'hl-line-mode
 (kbd "M-s k") #'consult-keep-lines
 (kbd "M-s l") #'consult-locate
 (kbd "M-s m") #'consult-multi-occur
 (kbd "M-s M-s") #'consult-line
 (kbd "M-s p") #'rg-project
 (kbd "M-s r") #'consult-ripgrep
 (kbd "M-s s") #'consult-line
 (kbd "M-s u") #'consult-focus-lines
 (kbd "M-s z") #'pjones:rg-zettel-dir

 ;; Additional go-to bindings:
 (kbd "M-g @") #'consult-global-mark
 (kbd "M-g i") #'consult-imenu
 (kbd "M-g m") #'consult-mark
 (kbd "M-g N") #'pjones:fly-next-error

 ;; Bindings that are not under C-c:
 (kbd "C-+") #'text-scale-adjust
 (kbd "C--") #'text-scale-adjust
 (kbd "C-.") #'goto-last-point
 (kbd "C-_") (lambda () (interactive) (text-scale-set 0))
 (kbd "C-`") #'consult-register-load
 (kbd "C-M-`") #'consult-register
 (kbd "C-M-SPC") #'er/expand-region
 (kbd "C-M-z") #'zap-to-char
 (kbd "C-x C-k @") #'consult-kmacro
 (kbd "C-z") #'repeat
 (kbd "M-'") #'mode-line-other-buffer
 (kbd "M-`") #'consult-register-store
 (kbd "M-RET") #'delete-blank-lines

 ;; Overriding default key bindings
 [remap apropos-command] #'consult-apropos
 [remap bookmark-jump] #'consult-bookmark
 [remap delete-window] #'ace-delete-window
 [remap exchange-point-and-mark] #'pjones:exchange-point-and-mark
 [remap indent-for-tab-command] #'pjones:indent-or-complete
 [remap kill-region] #'pjones:kill-region-or-backward-kill-word
 [remap list-buffers] #'ibuffer
 [remap move-beginning-of-line] #'pjones:move-beginning-of-line
 [remap open-line] #'pjones:open-line-above
 [remap other-window] #'ace-select-window
 [remap rectangle-number-lines] #'pjones:rectangle-number-lines
 [remap save-buffers-kill-emacs] #'pjones:maybe-save-buffers-kill-terminal
 [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
 [remap switch-to-buffer-other-window] #'consult-buffer-other-window
 [remap switch-to-buffer] #'consult-buffer
 [remap yank-pop] #'consult-yank-pop
 [remap zap-to-char] #'zap-up-to-char)

;;; keys.el ends here
