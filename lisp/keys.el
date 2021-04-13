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

;; Miscellaneous Commands:
(global-set-key (kbd "C-c s") #'pjones:sort-lines)
(global-set-key (kbd "C-c +") #'er/expand-region)
(global-set-key (kbd "C-c -") #'er/contract-region)

;; Buffer Commands:
(global-set-key (kbd "C-c b a") (pjones:jump-to-buffer "*Org Agenda*" pjones:agenda))
(global-set-key (kbd "C-c b e") #'eldoc-doc-buffer)
(global-set-key (kbd "C-c b f") (pjones:jump-to-buffer "*flymake message*"))
(global-set-key (kbd "C-c b m") (pjones:jump-to-buffer "*Messages*"))
(global-set-key (kbd "C-c b r") #'revert-buffer)
(global-set-key (kbd "C-c b s") (pjones:jump-to-buffer "*scratch*"))
(global-set-key (kbd "C-c b t") #'pjones:open-temp-buffer)
(global-set-key (kbd "C-c b w") #'read-only-mode)

;; Evaluation Keys:
(global-set-key (kbd "C-c e b") #'eval-buffer)
(global-set-key (kbd "C-c e f") #'eval-defun)
(global-set-key (kbd "C-c e l") #'eval-last-sexp)
(global-set-key (kbd "C-c e r") #'eval-region)

;; File Commands:
(global-set-key (kbd "C-c f b") #'bookmark-jump)
(global-set-key (kbd "C-c f d") #'dired-jump)
(global-set-key (kbd "C-c f f") #'find-dired)
(global-set-key (kbd "C-c f m") #'magit-file-dispatch)
(global-set-key (kbd "C-c f n") #'pjones:find-file-next)
(global-set-key (kbd "C-c f p") #'pjones:find-file-prev)
(global-set-key (kbd "C-c f R") #'pjones:rename-current-file)
(global-set-key (kbd "C-c f s") #'save-buffer)
(global-set-key (kbd "C-c f w") #'pjones:kill-file-name)
(global-set-key (kbd "C-c f W") #'pjones:kill-directory-name)

;; Application Launcher and Link Following:
(global-set-key (kbd "C-c a C") #'full-calc)
(global-set-key (kbd "C-c a c") #'quick-calc)
(global-set-key (kbd "C-c a e") #'embark-act)
(global-set-key (kbd "C-c a h") #'pjones:start-http)
(global-set-key (kbd "C-c a m") #'pjones:start-mail)
(global-set-key (kbd "C-c C-c") #'pjones:projectile-compile-project)
(global-set-key (kbd "C-c i") #'imenus)
(global-set-key (kbd "C-c l o") #'link-hint-open-link)
(global-set-key (kbd "C-c l w") #'link-hint-copy-link)
(global-set-key (kbd "C-c m") #'magit-status)

;; Grep Commands:
(global-set-key (kbd "C-c g f") #'rg)
(global-set-key (kbd "C-c g g") #'rg-project)
(global-set-key (kbd "C-c g z") #'pjones:rg-zettel-dir)

;; Passwords and Projects:
(global-set-key (kbd "C-c p d") #'pjones:projectile-dired)
(global-set-key (kbd "C-c p f") #'pjones:projectile-find-file)
(global-set-key (kbd "C-c p g") #'pjones:pwgen)
(global-set-key (kbd "C-c p l") #'passmm-list-passwords)
(global-set-key (kbd "C-c p p") #'passmm-completing-read)
(global-set-key (kbd "C-c p s") #'projectile-switch-project)
(global-set-key (kbd "C-c p t") #'projectile-test-project)

;; Toggles:
(global-set-key (kbd "C-c t H") #'highlight-regexp)
(global-set-key (kbd "C-c t h") #'hl-line-mode)
(global-set-key (kbd "C-c t i") #'highlight-indent-guides-mode)
(global-set-key (kbd "C-c t l") #'toggle-truncate-lines)
(global-set-key (kbd "C-c t n") #'display-line-numbers-mode)

;; Window Commands:
(global-set-key (kbd "C-+") #'text-scale-adjust)
(global-set-key (kbd "C--") #'text-scale-adjust)
(global-set-key (kbd "C-_") (lambda () (interactive) (text-scale-set 0)))
(global-set-key (kbd "C-c 0") #'winum-select-window-0-or-10)
(global-set-key (kbd "C-c 1") #'winum-select-window-1)
(global-set-key (kbd "C-c 2") #'winum-select-window-2)
(global-set-key (kbd "C-c 3") #'winum-select-window-3)
(global-set-key (kbd "C-c 4") #'winum-select-window-4)
(global-set-key (kbd "C-c 5") #'winum-select-window-5)
(global-set-key (kbd "C-c 6") #'winum-select-window-6)
(global-set-key (kbd "C-c 7") #'winum-select-window-7)
(global-set-key (kbd "C-c 8") #'winum-select-window-8)
(global-set-key (kbd "C-c 9") #'winum-select-window-9)
(global-set-key (kbd "C-c w /") #'winner-undo)
(global-set-key (kbd "C-c w 0") #'delete-window)
(global-set-key (kbd "C-c w 1") #'delete-other-windows)
(global-set-key (kbd "C-c w =") #'balance-windows)
(global-set-key (kbd "C-c w k") #'pjones:switch-window-then-delete)
(global-set-key (kbd "C-c w n") #'winum-select-window-by-number)
(global-set-key (kbd "C-c w o") #'switch-window)
(global-set-key (kbd "C-c w r") #'rotate-layout)
(global-set-key (kbd "C-c w s") #'ace-swap-window)
(global-set-key (kbd "C-c w S") #'window-toggle-side-windows)
(global-set-key (kbd "C-c w R") #'resize-window)

;; Zettelkasten:
(global-set-key (kbd "C-c z d") #'neuron-open-daily-notes)
(global-set-key (kbd "C-c z f") #'neuron-edit-zettel)
(global-set-key (kbd "C-c z s") #'neuron-select-zettelkasten)
(global-set-key (kbd "C-c z z") #'neuron-new-zettel)

;; Overriding default key bindings
(global-set-key (kbd "C-a") #'pjones:move-beginning-of-line)
(global-set-key (kbd "C-M-r") #'isearch-backward)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-z") #'zap-to-char)
(global-set-key (kbd "C-o") #'pjones:open-line-above)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-w") #'pjones:kill-region-or-backward-kill-word)
(global-set-key (kbd "C-x C-c") #'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-x") #'pjones:exchange-point-and-mark)
(global-set-key (kbd "M-'") #'mode-line-other-buffer)
(global-set-key (kbd "M-g 0") #'end-of-buffer)
(global-set-key (kbd "M-g 1") #'beginning-of-buffer)
(global-set-key (kbd "M-g N") #'pjones:fly-next-error)
(global-set-key (kbd "M-RET") #'delete-blank-lines)
(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "TAB") #'pjones:indent-or-complete)

;;; keys.el ends here
