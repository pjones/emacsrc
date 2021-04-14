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
 ;; Miscellaneous Commands:
 (kbd "C-c s") #'pjones:sort-lines

 ;; Buffer Commands:
 (kbd "C-c b a") (pjones:jump-to-buffer "*Org Agenda*" pjones:agenda)
 (kbd "C-c b e") #'eldoc-doc-buffer
 (kbd "C-c b f") (pjones:jump-to-buffer "*flymake message*")
 (kbd "C-c b m") (pjones:jump-to-buffer "*Messages*")
 (kbd "C-c b r") #'revert-buffer
 (kbd "C-c b s") (pjones:jump-to-buffer "*scratch*")
 (kbd "C-c b t") #'pjones:open-temp-buffer
 (kbd "C-c b w") #'read-only-mode

 ;; Evaluation Keys:
 (kbd "C-c e b") #'eval-buffer
 (kbd "C-c e f") #'eval-defun
 (kbd "C-c e l") #'eval-last-sexp
 (kbd "C-c e r") #'eval-region

 ;; File Commands:
 (kbd "C-c f f") #'find-dired
 (kbd "C-c f m") #'magit-file-dispatch
 (kbd "C-c f n") #'pjones:find-file-next
 (kbd "C-c f p") #'pjones:find-file-prev
 (kbd "C-c f R") #'pjones:rename-current-file
 (kbd "C-c f s") #'save-buffer
 (kbd "C-c f w") #'pjones:kill-file-name
 (kbd "C-c f W") #'pjones:kill-directory-name

 ;; Application Launcher and Link Following:
 (kbd "C-c a C") #'full-calc
 (kbd "C-c a c") #'quick-calc
 (kbd "C-c a e") #'embark-act
 (kbd "C-c a h") #'pjones:start-http
 (kbd "C-c a m") #'pjones:start-mail
 (kbd "C-c l o") #'link-hint-open-link
 (kbd "C-c l w") #'link-hint-copy-link
 (kbd "C-c m") #'magit-status

 ;; Passwords:
 (kbd "C-c p g") #'pjones:pwgen
 (kbd "C-c p l") #'passmm-list-passwords
 (kbd "C-c p p") #'passmm-completing-read

 ;; Toggles:
 (kbd "C-c t H") #'highlight-regexp
 (kbd "C-c t h") #'hl-line-mode
 (kbd "C-c t i") #'highlight-indent-guides-mode
 (kbd "C-c t l") #'toggle-truncate-lines
 (kbd "C-c t n") #'display-line-numbers-mode

 ;; Window Commands:
 (kbd "C-+") #'text-scale-adjust
 (kbd "C--") #'text-scale-adjust
 (kbd "C-_") (lambda () (interactive) (text-scale-set 0))
 (kbd "C-c 0") #'winum-select-window-0-or-10
 (kbd "C-c 1") #'winum-select-window-1
 (kbd "C-c 2") #'winum-select-window-2
 (kbd "C-c 3") #'winum-select-window-3
 (kbd "C-c 4") #'winum-select-window-4
 (kbd "C-c 5") #'winum-select-window-5
 (kbd "C-c 6") #'winum-select-window-6
 (kbd "C-c 7") #'winum-select-window-7
 (kbd "C-c 8") #'winum-select-window-8
 (kbd "C-c 9") #'winum-select-window-9
 (kbd "C-c w /") #'winner-undo
 (kbd "C-c w =") #'balance-windows
 (kbd "C-c w n") #'winum-select-window-by-number
 (kbd "C-c w r") #'rotate-layout
 (kbd "C-c w s") #'ace-swap-window
 (kbd "C-c w S") #'window-toggle-side-windows
 (kbd "C-c w R") #'resize-window

 ;; Zettelkasten:
 (kbd "C-c z d") #'neuron-open-daily-notes
 (kbd "C-c z f") #'neuron-edit-zettel
 (kbd "C-c z s") #'neuron-select-zettelkasten
 (kbd "C-c z z") #'neuron-new-zettel

 ;; Additional searching/grepping bindings:
 (kbd "M-s z") #'pjones:rg-zettel-dir
 (kbd "M-s f") #'consult-find
 (kbd "M-s G") #'consult-git-grep
 (kbd "M-s g") #'consult-grep
 (kbd "M-s k") #'consult-keep-lines
 (kbd "M-s l") #'consult-line
 (kbd "M-s L") #'consult-locate
 (kbd "M-s m") #'consult-multi-occur
 (kbd "M-s r") #'consult-ripgrep
 (kbd "M-s u") #'consult-focus-lines
 (kbd "M-s p") #'rg-project
 (kbd "M-s d") #'rg

 ;; Additional go-to bindings:
 (kbd "M-g @") #'consult-global-mark
 (kbd "M-g i") #'consult-imenu
 (kbd "M-g m") #'consult-mark
 (kbd "M-g N") #'pjones:fly-next-error

 ;; Bindings that are not under C-c:
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
 [remap other-window] #'ace-select-window
 [remap delete-window] #'ace-delete-window
 [remap bookmark-jump] #'consult-bookmark
 [remap yank-pop] #'consult-yank-pop
 [remap apropos-command] #'consult-apropos
 [remap switch-to-buffer] #'consult-buffer
 [remap switch-to-buffer-other-window] #'consult-buffer-other-window
 [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
 [remap goto-line] #'consult-goto-line
 [remap exchange-point-and-mark] #'pjones:exchange-point-and-mark
 [remap indent-for-tab-command] #'pjones:indent-or-complete
 [remap isearch-backward] #'isearch-backward-regexp
 [remap isearch-forward] #'isearch-forward-regexp
 [remap kill-region] #'pjones:kill-region-or-backward-kill-word
 [remap move-beginning-of-line] #'pjones:move-beginning-of-line
 [remap open-line] #'pjones:open-line-above
 [remap rectangle-number-lines] #'pjones:rectangle-number-lines
 [remap save-buffers-kill-emacs] #'pjones:maybe-save-buffers-kill-terminal
 [remap zap-to-char] #'zap-up-to-char)

;;; keys.el ends here
