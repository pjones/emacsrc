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

(require 'evil)
(require 'evil-leader)

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

;; Maps that need to be shared:
(defvar pjones:window-map (make-sparse-keymap))

;; Key bindings that fall under the leader (space) key:
(evil-leader/set-key
  ;; Menus:
  "DEL" #'execute-extended-command
  "SPC" #'consult-buffer
  ":" #'eval-expression

  ;; Buffer Commnads:
  "b a" (pjones:jump-to-buffer "*Org Agenda*" pjones:agenda)
  "b B" #'ibuffer
  "b b" #'consult-buffer
  "b d" #'kill-this-buffer
  "b e" #'eldoc-doc-buffer
  "b f" #'find-file
  "b l" #'pjones:switch-to-previous-buffer
  "b m" (pjones:jump-to-buffer "*Messages*")
  "b r" #'revert-buffer
  "b s" (pjones:jump-to-buffer "*scratch*")
  "b t" #'pjones:open-temp-buffer
  "b w" #'read-only-mode

  ;; Evaluation Keys:
  "e b" #'eval-buffer
  "e f" #'eval-defun
  "e l" #'eval-last-sexp
  "e r" #'eval-region

  ;; File Commands:
  "f d" #'dired-jump
  "f f" #'find-file
  "f F" #'find-dired
  "f m" #'magit-file-dispatch
  "f o" #'occur
  "f R" #'pjones:rename-current-file
  "f S" #'evil-write-all
  "f s" #'save-buffer
  "f v" #'find-alternate-file
  "f w" #'write-file

  ;; Go/Grep Commands:
  "g a h" #'pjones:start-http
  "g a i" #'pjones:start-irc
  "g C" #'full-calc
  "g c" #'quick-calc
  "g e" #'embark-act
  "g f" #'rg
  "g g" #'rg-project
  "g m" #'magit-status
  "g o a" #'pjones:agenda
  "g o c" #'org-capture
  "g o i" #'org-mru-clock-in
  "g o L" #'org-id-store-link
  "g o l" #'org-store-link
  "g o o" #'org-clock-out
  "g r" #'consult-ripgrep
  "g t" #'pjones:zettel-need-to-do
  "g x" #'pjones:link-hint-open-link
  "g z" #'pjones:rg-zettel-dir

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
  "p t" #'projectile-test-project

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
  "w0" #'delete-window
  "w1" #'delete-other-windows
  "w=" #'balance-windows
  "w'" #'evil-window-mru
  "wd" #'pjones:switch-window-then-delete
  "wn" #'winum-select-window-by-number
  "wo" #'switch-window
  "ws" #'window-toggle-side-windows
  "wt" #'treemacs-select-window
  "wu" #'winner-undo
  ;; "wz" Zoom window
  ;; "wZ" Zoom all windows

  ;; Text Commands:
  "x c" #'pjones:comment-bar
  "x s" #'pjones:evil-sort

  ;; Yanking (copy) Commands:
  "y b" #'pjones:kill-whole-buffer
  "y d" #'pjones:kill-directory-name
  "y f" #'pjones:kill-file-name
  "y x" #'link-hint-copy-link

  ;; Zettelkasten:
  "z d" #'neuron-open-daily-notes
  "z f" #'neuron-edit-zettel
  "z i" #'pjones:zettel-open-inbox
  "z s" #'neuron-select-zettelkasten
  "z z" #'neuron-new-zettel)

;; Window Resizing:
(smartrep-define-key pjones:window-map "r"
  '(("j" . enlarge-window)
    ("k" . shrink-window)
    ("h" . enlarge-window-horizontally)
    ("l" . shrink-window-horizontally)))

;; Zooming windows:
(smartrep-define-key pjones:window-map "z"
  '(("=" . text-scale-increase)
    ("+" . text-scale-increase)
    ("-" . text-scale-decrease)
    ("k" . text-scale-increase)
    ("j" . text-scale-decrease)
    ("0" . text-scale-set)))

;; Zooming frames:
(smartrep-define-key pjones:window-map "Z"
  '(("=" . default-text-scale-increase)
    ("+" . default-text-scale-increase)
    ("-" . default-text-scale-decrease)
    ("k" . default-text-scale-increase)
    ("j" . default-text-scale-decrease)
    ("0" . default-text-scale-reset)))

;; Overriding default key bindings
(global-set-key (kbd "C-x C-c") #'pjones:maybe-save-buffers-kill-terminal)
(global-set-key (kbd "TAB") #'pjones:indent-or-complete)

(evil-define-operator pjones:evil-sort (beg end)
  "Sort text."
  :move-point nil
  :type line
  (let ((sort-fold-case t))
    (save-excursion
      (condition-case nil
          (sort-lines nil beg end)
        (error nil)))))

(evil-define-motion pjones:evil-first-non-blank ()
  "Move to first character, or beginning of line."
  :type exclusive
  (evil-narrow-to-line (pjones:move-beginning-of-line)))

;; Additional key bindings:
(evil-define-key 'insert minibuffer-local-map
  (kbd "<escape>") #'abort-recursive-edit)

(evil-define-key 'normal global-map
  "[b" #'previous-buffer
  "[f" #'pjones:find-file-prev
  "[t" #'pjones:theme-prev
  "[z" #'text-scale-decrease
  "]b" #'next-buffer
  "]c" #'pjones:fly-next-error
  "]f" #'pjones:find-file-next
  "]t" #'pjones:theme-next
  "]z" #'text-scale-increase
  (kbd "g <return>") #'delete-blank-lines
  "g " #'just-one-space
  "g'" #'pjones:switch-to-previous-buffer
  "gG" #'consult-goto-line
  "gm" #'consult-imenu
  "gs" #'pjones:evil-sort
  "gS" #'evil-surround-edit
  "z'" #'evil-window-mru
  "zn" #'widen)

(evil-define-key 'visual global-map
  "s" #'evil-surround-region
  "S" #'evil-Surround-region
  "zn" #'narrow-to-region)

;; Hybrid evil/Emacs bindings:
(evil-define-key 'insert global-map
  (kbd "<backspace>") #'pjones:backward-delete-char
  (kbd "C-a") #'pjones:move-beginning-of-line
  (kbd "C-e") #'end-of-line
  (kbd "C-k") #'kill-line)

(evil-define-key 'motion global-map
  "^" #'pjones:evil-first-non-blank
  "[e" #'previous-error
  "]e" #'next-error
  "gl" #'evil-avy-goto-line
  "gM" #'evil-middle-of-visual-line)

(dolist (state '(normal visual motion))
  (evil-define-key state global-map
    ;; Swap ' and `
    "'" #'evil-goto-mark
    "`" #'evil-goto-mark-line
    ;; Second leader key:
    (kbd "DEL") evil-leader--default-map))

;; Turn on Evil!
(global-evil-leader-mode)
(evil-mode)

;;; keys.el ends here
