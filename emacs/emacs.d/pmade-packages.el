(eval-when-compile
    (load "pmade-loadpath"))

;; Emacs nonstandard editing commands
(autoload 'zap-up-to-char "misc" nil t)

;; Gnus CVS
(when (= 22 emacs-major-version)
  (require 'gnus-load))

;; Use Aspell
(setq ispell-program-name "aspell")

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
 ido-use-filename-at-point t
 ido-completion-buffer-all-completions t
 ido-max-prospects 10
 ido-auto-merge-work-directories-length 0)
(ido-mode t)

;; Tramp kicks ass
(setq tramp-default-method "ssh"
      tramp-verbose 3                   ; For reference if I need to
      tramp-debug-buffer nil            ; debug Tramp
      tramp-auto-save-directory "~/.emacs.d/autosave")
(require 'tramp)

;; Save your place in files you edit
(setq save-place-file "~/.emacs.d/places.el")
(require 'saveplace)

;; Bookmarks
(setq
 bookmark-save-flag 1
 bookmark-default-file "~/.comm-sync/var/emacs/bookmarks.el")

;; Add window numbers
(require 'window-number)
(window-number-mode 1)

;; Getting and fetching pastie.caboo.se
(autoload 'pastie-region "pastie" nil t)
(autoload 'pastie-buffer "pastie" nil t)
(autoload 'pastie-get    "pastie" nil t)

;; Place the time in the title bar
(setq display-time-format "%A, %B %d, %Y  %H:%M")
(require 'title-time)

;; ElScreen
(setq
 elscreen-display-screen-number t
 elscreen-display-tab nil
 elscreen-tab-display-kill-screen t
 elscreen-tab-display-control nil)
(require 'elscreen)

;; Go-to Last Change
(autoload 'goto-last-change "goto-chg" nil t)
(autoload 'goto-last-change-reverse "goto-chg" nil t)

;; Markdown Mode
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Visual Bookmarks
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)
(setq
 bm-highlight-style 'bm-highlight-only-fringe
 bm-repository-file "~/.comm-sync/etc/bm-repo"
 bm-restore-repository-on-load t)

;; Typing Breaks
(setq 
 type-break-query-mode t
 type-break-mode-line-message-mode t
 type-break-demo-functions '(type-break-demo-boring)
 type-break-demo-boring-stats t
 type-break-file-name (expand-file-name "~/.emacs.d/type-break"))
(type-break-mode)

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

;; Magit (Git Interface for Emacs)
(autoload 'magit-status "magit" "Magit Status" t)

;; EasyPG http://www.easypg.org/
(require 'epa-setup)
(setq epa-file-encrypt-to "61C4F407C4EB56B7"
      epa-popup-info-window nil)

;; I love Org Mode!
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(eval-after-load 'org '(load "~/.emacs.d/pmade/pmade-org"))

;; Muse is great for writing documents and articles
(autoload 'muse-mode-choose-mode "muse-mode" "Emacs Muse" t)
(add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode-choose-mode))
(eval-after-load "muse-mode" '(load "~/.emacs.d/pmade/pmade-muse"))
