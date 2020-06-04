;;; keys.el -- Global key bindings.
;;
;;; Commentary:
;;
;; Key bindings follow the style used in Spacemacs.
;;
;;; Code:
(require 'smartrep)

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

;; Evil and evil-leader:
;; These need to be set before loading Evil.
(custom-set-variables
 '(evil-leader/leader "SPC"))

(require 'evil)
(require 'evil-leader)

;; Loading `link-hint' will also load my settings and custom functions
;; for it.
(autoload 'pjones:link-hint-open-link "link-hint")

;; Autoloads for neuron-mode:
(dolist (f '(neuron-new-zettel
             neuron-open-daily-notes
             neuron-select-zettelkasten
             neuron-open-zettel
             neuron-open-index))
  (autoload f "neuron-mode"))

;; Maps that need to be shared:
(defvar pjones:zoom-map (make-sparse-keymap))
(defvar pjones:window-map (make-sparse-keymap))

(evil-leader/set-key
  ;; Menus:
  "SPC" #'counsel-M-x
  ":" #'eval-expression

  ;; Buffer Commnads:
  "b a" (pjones:jump-to-buffer "*Org Agenda*" pjones:agenda)
  "b b" #'ivy-switch-buffer
  "b B" #'ibuffer
  "b d" #'kill-this-buffer
  "b f" #'counsel-find-file
  "b l" #'pjones:switch-to-previous-buffer
  "b m" (pjones:jump-to-buffer "*Messages*")
  "b n" #'next-buffer
  "b p" #'previous-buffer
  "b r" #'revert-buffer
  "b s" (pjones:jump-to-buffer "*scratch*")
  "b t" #'treemacs-select-window
  "b v" #'find-alternate-file
  "b y" #'pjones:kill-file-name
  "b Y" #'pjones:kill-whole-buffer
  "b w" #'read-only-mode

  ;; Evaluation Keys:
  "e b" #'eval-buffer
  "e f" #'eval-defun
  "e l" #'eval-last-sexp
  "e r" #'eval-region

  ;; File Commands:
  "f b" #'counsel-bookmark
  "f f" #'counsel-find-file
  "f F" #'find-dired
  "f d" #'dired-jump
  "f g" #'rg-project
  "f G" #'rg
  "f o" #'occur
  "f R" #'pjones:rename-current-file
  "f s" #'save-buffer
  "f S" #'evil-write-all
  "f t" #'treemacs-select-window
  "f w" #'write-file
  "f y" #'pjones:kill-file-name

  ;; Go Commands:
  "g a h" #'pjones:start-http
  "g a i" #'pjones:start-irc
  "g a m" #'pjones:start-mail
  "g a t" #'pjones:start-term
  "g c" #'quick-calc
  "g C" #'full-calc
  "g e" #'next-error
  "g f" #'pjones:fly-next-error
  "g i" #'counsel-imenu
  "g m b" #'magit-blame
  "g m f" #'magit-log-buffer-file
  "g m m" #'magit-status
  "g o a" #'pjones:agenda
  "g o c" #'org-capture
  "g o i" #'org-mru-clock-in
  "g o l" #'org-store-link
  "g o L" #'org-id-store-link
  "g o o" #'org-clock-out
  "g x" #'pjones:link-hint-open-link

  ;; Help:
  "h" help-map
  "h m" #'which-key-show-major-mode

  ;; Jumping:
  "j c" #'evil-avy-goto-char
  "j l" #'evil-avy-goto-line
  "j p" #'avy-pop-mark
  "j w" #'evil-avy-goto-word-or-subword-1

  ;; Mode-specific Commands:
  ;; m c: Main command
  ;; m k: Cancel main command
  "m c" #'pjones:projectile-compile-project

  ;; Passwords and Projects
  "p d" #'pjones:projectile-dired
  "p f" #'pjones:projectile-find-file
  "p g" #'pjones:pwgen
  "p l" #'passmm-list-passwords
  "p p" #'passmm-completing-read

  ;; Toggles:
  "t /" #'evil-ex-nohighlight
  "t e" #'toggle-debug-on-error
  "t h" #'hl-line-mode
  "t H" #'highlight-regexp
  "t i" #'highlight-indent-guides-mode
  "t l" #'toggle-truncate-lines
  "t n" #'display-line-numbers-mode

  ;; Universal Argument:
  "u" #'universal-argument

  ;; Window Commands:
  "0" #'winum-select-window-0-or-10
  "1" #'winum-select-window-1
  "2" #'winum-select-window-2
  "3" #'winum-select-window-3
  "4" #'winum-select-window-4
  "5" #'winum-select-window-5
  "6" #'winum-select-window-6
  "7" #'winum-select-window-7
  "8" #'winum-select-window-8
  "9" #'winum-select-window-9
  "w" pjones:window-map
  "w1" #'delete-other-windows
  "w=" #'balance-windows
  "wd" #'delete-window
  "wD" #'pjones:switch-window-then-delete
  "wn" #'winum-select-window-by-number
  "wo" #'switch-window
  "wu" #'winner-undo
  ;; Resizing commands below:

  ;; Text Commands:
  "x c" #'pjones:comment-bar
  "x s" #'pjones:evil-sort

  ;; Zoom Commands: (others: zw zf)
  "z" pjones:zoom-map

  ;; Zettelkasten:
  "zd" #'neuron-open-daily-notes
  "zz" #'neuron-new-zettel
  "ze" #'neuron-edit-zettel
  "zs" #'neuron-select-zettelkasten)

;; Window Resizing:
(smartrep-define-key pjones:window-map "r"
  '(("j" . enlarge-window)
    ("k" . shrink-window)
    ("h" . enlarge-window-horizontally)
    ("l" . shrink-window-horizontally)))

;; Zooming windows:
(smartrep-define-key pjones:zoom-map "w"
  '(("=" . text-scale-increase)
    ("+" . text-scale-increase)
    ("-" . text-scale-decrease)
    ("0" . text-scale-set)))

;; Zooming frames:
(smartrep-define-key pjones:zoom-map "f"
  '(("=" . default-text-scale-increase)
    ("+" . default-text-scale-increase)
    ("-" . default-text-scale-decrease)
    ("0" . default-text-scale-reset)))


;; Overriding default key bindings
(global-set-key (kbd "C-w")       #'pjones:kill-region-or-backward-kill-word)
(global-set-key (kbd "C-x C-c")   #'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "TAB")       #'pjones:indent-or-complete)

;; Turn on Evil!
(global-evil-leader-mode)
(evil-mode)

;;; keys.el ends here
