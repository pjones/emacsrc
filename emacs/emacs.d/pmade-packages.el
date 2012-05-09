(eval-when-compile
  (load "pmade-loadpath"))

;; Bring in an external Gnus, but first we need to set some location
;; variables otherwise Gnus will load and create the wrong
;; directories.
(setq gnus-startup-file      "~/.gnus.d/newsrc"
      gnus-directory         "~/.gnus.d/news"
      gnus-agent-directory   "~/.gnus.d/agent"
      message-directory      "~/.gnus.d/mail"
      nnfolder-directory     "~/.gnus.d/mail"
      nndraft-directory      "~/.gnus.d/mail"
      mail-default-directory "~/.gnus.d/"
      mail-signature-dir     "~/develop/pmade/privaterc/signatures")
(require 'gnus-load)

;; Emacs nonstandard editing commands
(autoload 'zap-up-to-char "misc" nil t)

;; Use Aspell
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Better buffer switching and file finding
(require 'ido)
(add-hook 'ido-setup-hook
  (lambda ()
    (define-key ido-file-dir-completion-map "\C-n" 'ido-next-work-directory)
    (define-key ido-file-dir-completion-map "\C-p" 'ido-prev-work-directory)
    (define-key ido-file-completion-map     "\C-w" 'ido-delete-backward-word-updir)))
(setq
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-use-filename-at-point nil
 ido-completion-buffer-all-completions t
 ido-max-prospects 10
 ido-auto-merge-work-directories-length 0)
(ido-mode t)

;; Save your place in files you edit
(setq save-place-file "~/.emacs.d/places.el")
(require 'saveplace)

;; Dired-X
(require 'dired-x)

;; Winmove is awesome
(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

;; Getting and fetching pastie.caboo.se
(autoload 'pastie-region "pastie" nil t)
(autoload 'pastie-buffer "pastie" nil t)
(autoload 'pastie-get    "pastie" nil t)

;; Go-to Last Change
(autoload 'goto-last-change "goto-chg" nil t)
(autoload 'goto-last-change-reverse "goto-chg" nil t)

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(setq markdown-command "pandoc -f markdown -t html"
      markdown-follow-wiki-link-on-enter nil)

;; Visual Bookmarks
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)
(setq
 bm-highlight-style 'bm-highlight-only-fringe
 bm-restore-repository-on-load nil)

;; Editing the Emacs Wiki
(defun pmade-oddmuse-hook ()
  "Hook is run after oddmuse is started"
  (unless (string-match "question" oddmuse-post)
    (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post))))

(setq oddmuse-username "PeterJones")
(setq oddmuse-directory "~/.oddmuse")

(defun pmade-oddmuse-edit ()
  "Load oddmuse and run oddmuse-edit."
  (interactive)
  (require 'oddmuse)
  (add-hook 'oddmuse-mode-hook 'pmade-oddmuse-hook)
  (oddmuse-mode-initialize)
  (call-interactively 'oddmuse-edit))

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Magit (Git Interface for Emacs)
(autoload 'magit-status "magit" "Magit Status" t)

;; EasyPG http://www.easypg.org/
;; (require 'epa)
(setq epa-file-encrypt-to "D4426FFA"
      epa-popup-info-window nil)

;; Graphviz Dot Files
(setq graphviz-dot-indent-width 2)

;; ESS for R
(add-to-list 'load-path (concat pmade-site-lisp "/ess/lisp"))
(autoload 'R-mode "ess-site" "ESS R Mode" t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

;; Calendar/Diary
(setq
 diary-file "~/Documents/pmade/pmade-inc/planning/schedule/diary"
 calendar-date-style 'iso)

;; I love Org Mode!
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(eval-after-load 'org '(load "~/.emacs.d/pmade/pmade-org"))

;; Mingus (front-end for MPD)
(autoload 'mingus "mingus" "Mingus" t)
(eval-after-load 'mingus '(load "~/.emacs.d/pmade/pmade-mingus"))

;; Whitespace Mode
(autoload 'whitespace-mode "whitespace" "Whitespace Mode" t)
(setq whitespace-style '(face trailing tabs newline lines-tail)
      whitespace-action '(auto-cleanup))

;; Idea (awesome for coding and window management)
;;(require 'idea nil t)
