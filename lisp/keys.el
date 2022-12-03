;;; keys.el -- Global key bindings.
;;
;;; Commentary:
;;
;; Key bindings follow the style used in Spacemacs.
;;
;;; Code:
(defmacro pjones:jump-to-buffer (name &optional command)
  "Generate a command to jump to buffer NAME.
If buffer NAME doesn't exist, COMMAND can be used to create it."
  `(defun ,(intern (concat "pjones:jump-to-buffer-" name)) ()
    (interactive)
    (let ((buf (get-buffer ,name)))
      (if buf (display-buffer buf)
        ,(if command `(,command) 'nil)))))

(defmacro pjones:in-home-dir (func)
  "Generate an interactive function to call FUNC from within $HOME.
The original intent of this macro is to clobber `default-directory'
for some interactive commands so that tramp doesn't try to make a new
connection."
  `(lambda ()
     (interactive)
     (let ((default-directory (expand-file-name "~")))
       (call-interactively ,func))))

;; Loading `link-hint' will also load my settings and custom functions
;; for it.
(autoload 'pjones:link-hint-open-link "link-hint")

;; Make the linter happy:
(declare-function ace-delete-window "ace-window")
(declare-function ace-select-window "ace-window")
(declare-function ace-swap-window "ace-window")
(declare-function consult-apropos "consult")
(declare-function consult-bookmark "consult")
(declare-function consult-buffer "consult")
(declare-function consult-buffer-other-frame "consult")
(declare-function consult-buffer-other-window "consult")
(declare-function consult-find "consult")
(declare-function consult-focus-lines "consult")
(declare-function consult-git-grep "consult")
(declare-function consult-global-mark "consult")
(declare-function consult-grep "consult")
(declare-function consult-imenu "consult-imenu")
(declare-function consult-keep-lines "consult")
(declare-function consult-kmacro "consult")
(declare-function consult-line "consult")
(declare-function consult-locate "consult")
(declare-function consult-mark "consult")
(declare-function consult-multi-occur "consult")
(declare-function consult-org-roam-mode "consult-org-roam")
(declare-function consult-org-roam-search "consult-org-roam")
(declare-function consult-outline "consult")
(declare-function consult-register "consult-register")
(declare-function consult-register-load "consult-register")
(declare-function consult-register-store "consult-register")
(declare-function consult-ripgrep "consult")
(declare-function consult-yank-pop "consult")
(declare-function embark-act "embark")
(declare-function er/expand-region "expand-region")
(declare-function goto-last-change "goto-chg")
(declare-function highlight-indent-guides-mode "highlight-indent-guides")
(declare-function link-hint-copy-link "link-hint")
(declare-function link-hint-open-link "link-hint")
(declare-function magit-file-dispatch "magit-files")
(declare-function magit-status "magit-status")
(declare-function org-roam-capture "org-roam")
(declare-function org-roam-capture "org-roam")
(declare-function org-roam-dailies-capture-date "org-roam")
(declare-function org-roam-dailies-goto-date "org-roam")
(declare-function org-roam-node-find "org-roam")
(declare-function org-roam-node-insert "org-roam")
(declare-function passmm-completing-read "passmm")
(declare-function passmm-list-passwords "passmm")
(declare-function pjones:agenda "./interactive")
(declare-function pjones:duplicate-region-or-line "./interactive")
(declare-function pjones:exchange-point-and-mark "./interactive")
(declare-function pjones:fly-next-error "./interactive")
(declare-function pjones:indent-or-complete "./completion")
(declare-function pjones:kill-directory-name "./interactive")
(declare-function pjones:kill-file-name "./interactive")
(declare-function pjones:kill-line "./interactive")
(declare-function pjones:kill-region-or-backward-kill-word "./interactive")
(declare-function pjones:maybe-save-buffers-kill-terminal "./interactive")
(declare-function pjones:open-line-above "./interactive")
(declare-function pjones:open-temp-buffer "./interactive")
(declare-function pjones:pwgen "./interactive")
(declare-function pjones:rectangle-number-lines "./interactive")
(declare-function pjones:rename-current-file "./interactive")
(declare-function pjones:sort-lines "./interactive")
(declare-function pjones:start-http "./interactive")
(declare-function pjones:start-term "./interactive")
(declare-function resize-window "resize-window")
(declare-function rg "rg")
(declare-function rg-project "rg")
(declare-function rotate-layout "rotate")
(declare-function which-key-show-top-level "which-key")
(declare-function winner-undo "winner")
(declare-function winum-select-window-0-or-10 "winum")
(declare-function winum-select-window-1 "winum")
(declare-function winum-select-window-2 "winum")
(declare-function winum-select-window-3 "winum")
(declare-function winum-select-window-4 "winum")
(declare-function winum-select-window-5 "winum")
(declare-function winum-select-window-6 "winum")
(declare-function winum-select-window-7 "winum")
(declare-function winum-select-window-8 "winum")
(declare-function winum-select-window-9 "winum")

(defun pjones:global-set-keys (key def &rest bindings)
  "Call `global-set-key' for KEY and DEF and each pair in BINDINGS."
  (while key
    (global-set-key key def)
    (setq key (pop bindings)
          def (pop bindings))))

(defvar pjones:zettle-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'org-roam-dailies-capture-date)
    (define-key map (kbd "D") #'org-roam-dailies-goto-date)
    (define-key map (kbd "f") #'org-roam-node-find)
    (define-key map (kbd "i") #'org-roam-node-insert)
    (define-key map (kbd "z") #'org-roam-capture)

    ;; This is a bit annoying:
    (define-key map (kbd "s")
      (lambda ()
        (interactive)
        (consult-org-roam-mode)
        (consult-org-roam-search)))

    map)
  "Key bindings for note taking.")

(pjones:global-set-keys
 (kbd "C-c ?") #'which-key-show-top-level
 (kbd "C-c a") (pjones:jump-to-buffer "*Org Agenda*" pjones:agenda)
 (kbd "C-c b") #'eldoc-doc-buffer
 (kbd "C-c C") #'full-calc
 (kbd "C-c c") #'quick-calc
 (kbd "C-c d") #'pjones:duplicate-region-or-line
 (kbd "C-c e") #'embark-act
 (kbd "C-c f") (pjones:jump-to-buffer "*flymake message*")
 (kbd "C-c h") #'pjones:start-http
 (kbd "C-c k") #'pjones:kill-line
 (kbd "C-c l o") #'link-hint-open-link
 (kbd "C-c l w") #'link-hint-copy-link
 (kbd "C-c M") #'magit-file-dispatch
 (kbd "C-c m") #'magit-status
 (kbd "C-c M-W") #'pjones:kill-directory-name
 (kbd "C-c M-w") #'pjones:kill-file-name
 (kbd "C-c p g") #'pjones:pwgen
 (kbd "C-c p l") #'passmm-list-passwords
 (kbd "C-c p s") #'pjones:start-term
 (kbd "C-c p p") #'passmm-completing-read
 (kbd "C-c R") #'pjones:rename-current-file
 (kbd "C-c r") #'revert-buffer-quick
 (kbd "C-c s") #'pjones:sort-lines
 (kbd "C-c t") #'pjones:open-temp-buffer

 ;; Additional window commands:
 (kbd "C-x w /") #'winner-undo
 (kbd "C-x w d") #'ace-delete-window
 (kbd "C-x w R") #'resize-window
 (kbd "C-x w r") #'rotate-layout
 (kbd "C-x w s") #'ace-swap-window
 (kbd "C-x w S") #'window-toggle-side-windows
 (kbd "C-x w u") #'winner-undo

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

 ;; Additional go-to bindings:
 (kbd "M-g @") #'consult-global-mark
 (kbd "M-g f") #'pjones:fly-next-error
 (kbd "M-g h") #'consult-outline
 (kbd "M-g i") #'consult-imenu
 (kbd "M-g m") #'consult-mark

 ;; Bindings that are not under C-c:
 (kbd "C-'") #'goto-last-change
 (kbd "C-+") #'text-scale-adjust
 (kbd "C--") #'text-scale-adjust
 (kbd "C-_") (lambda () (interactive) (text-scale-set 0))
 (kbd "C-`") #'consult-register-load
 (kbd "C-M-`") #'consult-register
 (kbd "C-M-SPC") #'er/expand-region
 (kbd "C-M-z") #'zap-to-char
 (kbd "C-x C-k @") #'consult-kmacro
 (kbd "C-z") pjones:zettle-map
 (kbd "M-'") #'mode-line-other-buffer
 (kbd "M-/") #'dabbrev-completion
 (kbd "M-<backspace>") (lambda () (interactive) (kill-buffer))
 (kbd "M-`") #'consult-register-store
 (kbd "M-RET") #'delete-blank-lines

 ;; Window movement keys:
 (kbd "C-0") #'winum-select-window-0-or-10
 (kbd "C-1") #'winum-select-window-1
 (kbd "C-2") #'winum-select-window-2
 (kbd "C-3") #'winum-select-window-3
 (kbd "C-4") #'winum-select-window-4
 (kbd "C-5") #'winum-select-window-5
 (kbd "C-6") #'winum-select-window-6
 (kbd "C-7") #'winum-select-window-7
 (kbd "C-8") #'winum-select-window-8
 (kbd "C-9") #'winum-select-window-9

 ;; Overriding default key bindings
 [remap apropos-command] #'consult-apropos
 [remap bookmark-jump] #'consult-bookmark
 [remap exchange-point-and-mark] #'pjones:exchange-point-and-mark
 [remap indent-for-tab-command] #'pjones:indent-or-complete
 [remap isearch-forward-symbol-at-point] #'isearch-forward-thing-at-point
 [remap kill-line] #'pjones:kill-line
 [remap kill-region] #'pjones:kill-region-or-backward-kill-word
 [remap list-buffers] #'ibuffer
 [remap open-line] #'pjones:open-line-above
 [remap other-window] #'ace-select-window
 [remap rectangle-number-lines] #'pjones:rectangle-number-lines
 [remap save-buffers-kill-emacs] #'pjones:maybe-save-buffers-kill-terminal
 [remap switch-to-buffer-other-frame] (pjones:in-home-dir #'consult-buffer-other-frame)
 [remap switch-to-buffer-other-window] (pjones:in-home-dir #'consult-buffer-other-window)
 [remap switch-to-buffer] (pjones:in-home-dir #'consult-buffer)
 [remap yank-pop] #'consult-yank-pop
 [remap zap-to-char] #'zap-up-to-char)

;;; keys.el ends here
