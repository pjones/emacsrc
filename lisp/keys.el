;;; keys.el -- Global key bindings. -*- lexical-binding: t -*-
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
     (let ((default-directory (expand-file-name "~"))
           (this-command ,func))
       (call-interactively ,func))))

(defmacro pjones:load-call (lib func)
  "Generate and return an interactive function.
It will load the library given in LIB and then interactively call
FUNC."
  `(lambda ()
     (interactive)
     (require ,lib)
     (call-interactively ,func)))

;; Make the linter happy:
(declare-function ace-delete-window "ace-window")
(declare-function ace-select-window "ace-window")
(declare-function ace-swap-window "ace-window")
(declare-function ace-window "ace-window")
(declare-function avy-goto-char-timer "avy")
(declare-function avy-goto-line "avy")
(declare-function consult-apropos "consult")
(declare-function consult-bookmark "consult")
(declare-function consult-buffer "consult")
(declare-function consult-buffer-other-frame "consult")
(declare-function consult-buffer-other-window "consult")
(declare-function consult-find "consult")
(declare-function consult-flymake "consult-flymake")
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
(declare-function consult-org-agenda "consult-org-agenda")
(declare-function consult-org-roam-mode "consult-org-roam")
(declare-function consult-org-roam-search "consult-org-roam")
(declare-function consult-outline "consult")
(declare-function consult-recoll "consult-recoll")
(declare-function consult-register "consult-register")
(declare-function consult-register-load "consult-register")
(declare-function consult-register-store "consult-register")
(declare-function consult-ripgrep "consult")
(declare-function consult-yank-pop "consult")
(declare-function devdocs-lookup "devdocs")
(declare-function embark-act "embark")
(declare-function emms "emms")
(declare-function emms-add-directory "emms")
(declare-function emms-add-playlist "emms")
(declare-function emms-insert-playlist "emms")
(declare-function emms-smart-browse "emms")
(declare-function flymake-goto-next-error "flymake")
(declare-function flymake-goto-prev-error "flymake")
(declare-function flymake-show-buffer-diagnostics "flymake")
(declare-function flymake-show-project-diagnostics "flymake")
(declare-function google-translate-smooth-translate "google-translate")
(declare-function goto-last-change "goto-chg")
(declare-function jinx-correct "jinx")
(declare-function link-hint-avy-act "link-hint")
(declare-function magit-file-dispatch "magit-files")
(declare-function magit-status "magit-status")
(declare-function mu4e "mu4e")
(declare-function org-roam-capture "org-roam")
(declare-function org-roam-capture "org-roam")
(declare-function org-roam-dailies-capture-date "org-roam")
(declare-function org-roam-dailies-capture-today "org-roam")
(declare-function org-roam-dailies-goto-date "org-roam")
(declare-function org-roam-node-find "org-roam")
(declare-function org-roam-node-insert "org-roam")
(declare-function org-store-link "ol")
(declare-function pass "pass")
(declare-function pjones:agenda "./interactive")
(declare-function pjones:emms-play-stream "emms")
(declare-function pjones:exchange-point-and-mark "./interactive")
(declare-function pjones:frame-toggle-alpha "./interactive")
(declare-function pjones:indent-or-complete "./completion")
(declare-function pjones:jump-to-marker "./interactive")
(declare-function pjones:kill-directory-name "./interactive")
(declare-function pjones:kill-file-name "./interactive")
(declare-function pjones:kill-line "./interactive")
(declare-function pjones:kill-region-or-backward-kill-word "./interactive")
(declare-function pjones:maybe-save-buffers-kill-terminal "./interactive")
(declare-function pjones:meow-insert-select "meow")
(declare-function pjones:meow-sort "../modes/meow-conf")
(declare-function pjones:move-beginning-of-line "./interactive")
(declare-function pjones:open-line-above "./interactive")
(declare-function pjones:open-temp-buffer "./interactive")
(declare-function pjones:password-goto "./interactive")
(declare-function pjones:pwgen "./interactive")
(declare-function pjones:rectangle-number-lines "./interactive")
(declare-function pjones:set-register-buffer "./interactive")
(declare-function pjones:sort-lines "./interactive")
(declare-function pjones:start-http "./interactive")
(declare-function pjones:start-term "./interactive")
(declare-function pjones:toggle-prev-buffer "./interactive")
(declare-function pjones:window-to-frame "./interactive")
(declare-function puni-barf-backward "puni")
(declare-function puni-barf-forward "puni")
(declare-function puni-raise "puni")
(declare-function puni-slurp-backward "puni")
(declare-function puni-slurp-forward "puni")
(declare-function resize-window "resize-window")
(declare-function rg "rg")
(declare-function rg-project "rg")
(declare-function rotate-layout "rotate")
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
    (define-key map (kbd "C-z") #'org-roam-capture)
    (define-key map (kbd "f") #'org-roam-node-find)
    (define-key map (kbd "i") #'org-roam-node-insert)
    (define-key map (kbd "SPC") #'org-roam-dailies-capture-date)
    (define-key map (kbd "z") #'org-roam-capture)

    ;; Jump directly to creating a daily note for today:
    (define-key map (kbd "d") '("org-roam-dailies-today" .
      (lambda ()
        "Create a new daily entry for today."
        (interactive)
        (require 'org-roam)
        (org-roam-dailies-capture-today nil "d"))))

    ;; Jump directly to creating a German lesson note:
    (define-key map (kbd "g") '("org-roam-german-today" .
      (lambda ()
        "Create a new daily entry for today."
        (interactive)
        (require 'org-roam)
        (org-roam-dailies-capture-today nil "g"))))

    ;; This is a bit annoying:
    (define-key map (kbd "s") '("org-roam-search" .
      (lambda ()
        "Grep through org-roam."
        (interactive)
        (require 'org-roam)
        (consult-org-roam-mode)
        (consult-org-roam-search))))

    map)
  "Key bindings for note taking.")

;; Additional help commands (under C-h).  The couple of extra keys
;; under C- are so meow will expose them without using the spacebar.
(let ((map help-map))
  (keymap-set map "D" #'apropos-documentation)
  (keymap-set map "d" #'devdocs-lookup)
  (keymap-set map "C-d" #'devdocs-lookup)
  (keymap-set map "C-f" #'describe-function)
  (keymap-set map "m" #'man)
  (keymap-set map "C-m" #'man)
  (keymap-set map "r" #'info-display-manual)
  (keymap-set map "R" #'info-emacs-manual))

(pjones:global-set-keys
 ;; Keys under C-c:
 ;;
 ;; Reserved keys:
 ;;
 ;;   - k: Custom bindings for the current major mode.
 ;;
 ;;(kbd "C-c '") #'separedit
 (kbd "C-c :") #'eval-expression
 (kbd "C-c a a") (pjones:jump-to-buffer "*Org Agenda*" pjones:agenda)
 (kbd "C-c a m") #'mu4e
 (kbd "C-c A") #'pjones:frame-toggle-alpha
 (kbd "C-c b t") #'pjones:open-temp-buffer
 (kbd "C-c c") #'quick-calc
 (kbd "C-c d") #'duplicate-dwim
 (kbd "C-c e") #'embark-act
 (kbd "C-c f f") (pjones:jump-to-buffer "*flymake message*")
 (kbd "C-c f R") #'rename-visited-file
 (kbd "C-c g") #'google-translate-smooth-translate
 (kbd "C-c h h") #'pjones:start-http
 (kbd "C-c h j") #'webjump
 (kbd "C-c j") #'pjones:jump-to-marker
 (kbd "C-c k") nil ; Reserved for major mode customzation.
 (kbd "C-c l") #'link-hint-avy-act
 (kbd "C-c L") #'org-store-link
 (kbd "C-c m b") #'emms-smart-browse
 (kbd "C-c m d") #'emms-add-directory
 (kbd "C-c m e") (pjones:load-call 'emms-playlist-mode #'emms)
 (kbd "C-c m i") #'emms-insert-playlist
 (kbd "C-c m p") #'emms-add-playlist
 (kbd "C-c m SPC") (pjones:load-call 'emms #'pjones:emms-play-stream)
 (kbd "C-c M-W") #'pjones:kill-directory-name
 (kbd "C-c M-w") #'pjones:kill-file-name
 (kbd "C-c o") #'ace-window
 (kbd "C-c p g") #'pjones:pwgen
 (kbd "C-c p l") #'pass
 (kbd "C-c p p") #'pjones:password-goto
 (kbd "C-c Q") #'full-calc
 (kbd "C-c r") #'revert-buffer-quick
 (kbd "C-c RET") #'pjones:start-term
 (kbd "C-c SPC") #'org-capture
 (kbd "C-c t B") #'puni-barf-backward
 (kbd "C-c t b") #'puni-barf-forward
 (kbd "C-c t c") #'capitalize-dwim
 (kbd "C-c t d") #'delete-blank-lines
 (kbd "C-c t l") #'downcase-dwim
 (kbd "C-c t P") #'puni-slurp-backward
 (kbd "C-c t p") #'puni-slurp-forward
 (kbd "C-c t r") #'puni-raise
 (kbd "C-c t s") #'pjones:meow-sort
 (kbd "C-c t u") #'upcase-dwim

 ;; Additional window commands:
 (kbd "C-x w /") #'winner-undo
 (kbd "C-x w d") #'ace-delete-window
 (kbd "C-x w o") #'delete-other-windows
 (kbd "C-x w p") #'pjones:window-to-frame
 (kbd "C-x w R") #'resize-window
 (kbd "C-x w r") #'rotate-layout
 (kbd "C-x w s") #'ace-swap-window
 (kbd "C-x w S") #'window-toggle-side-windows
 (kbd "C-x w u") #'winner-undo

 ;; Additional register commands:
 (kbd "C-x r B") #'pjones:set-register-buffer
 (kbd "C-x r r") #'consult-register-load
 (kbd "C-x r /") #'consult-register-load
 (kbd "C-x r s") #'consult-register-store

 ;; Additional searching/grepping bindings:
 (kbd "M-s d") #'rg
 (kbd "M-s f") #'consult-find
 (kbd "M-s G") #'consult-git-grep
 (kbd "M-s g") #'consult-grep
 (kbd "M-s h L") #'hl-line-mode
 (kbd "M-s k") #'consult-keep-lines
 (kbd "M-s l") #'consult-locate
 (kbd "M-s m") #'consult-multi-occur
 (kbd "M-s M-s") #'consult-line
 (kbd "M-s p") #'rg-project
 (kbd "M-s r") #'consult-ripgrep
 (kbd "M-s s") #'consult-line
 (kbd "M-s t") #'consult-recoll
 (kbd "M-s u") #'consult-focus-lines

 ;; Additional go-to bindings:
 (kbd "M-g @") #'consult-global-mark
 (kbd "M-g a") #'avy-goto-char-timer
 (kbd "M-g e") #'eldoc-doc-buffer
 (kbd "M-g f b") #'flymake-show-buffer-diagnostics
 (kbd "M-g f c") #'consult-flymake
 (kbd "M-g f B") #'flymake-show-project-diagnostics
 (kbd "M-g f n") #'flymake-goto-next-error
 (kbd "M-g f p") #'flymake-goto-prev-error
 (kbd "M-g h") #'consult-outline
 (kbd "M-g m") #'consult-mark
 (kbd "M-g M-a") #'avy-goto-char-timer
 (kbd "M-g o") #'consult-org-agenda

 ;; Bindings that are not under C-c:
 (kbd "C-'") #'goto-last-change
 (kbd "C-+") #'text-scale-adjust
 (kbd "C--") #'text-scale-adjust
 (kbd "C-.") #'embark-act
 (kbd "C-;") #'jinx-correct
 (kbd "C-_") (lambda () (interactive) (text-scale-set 0))
 (kbd "C-M-SPC") #'pjones:meow-insert-select
 (kbd "C-M-z") #'zap-to-char
 (kbd "C-x C-k @") #'consult-kmacro
 (kbd "C-x C-k S") #'kmacro-end-macro
 (kbd "C-z") pjones:zettle-map
 (kbd "M-'") #'pjones:toggle-prev-buffer
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
 [remap copy-to-register] #'consult-register-store
 [remap exchange-point-and-mark] #'pjones:exchange-point-and-mark
 [remap find-file] #'find-file-at-point
 [remap goto-line] #'avy-goto-line
 [remap imenu] #'consult-imenu
 [remap indent-for-tab-command] #'pjones:indent-or-complete
 [remap insert-register] #'consult-register-load
 [remap isearch-forward-symbol-at-point] #'isearch-forward-thing-at-point
 [remap kill-line] #'pjones:kill-line
 [remap kill-region] #'pjones:kill-region-or-backward-kill-word
 [remap list-buffers] #'ibuffer
 [remap move-beginning-of-line] #'pjones:move-beginning-of-line
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
