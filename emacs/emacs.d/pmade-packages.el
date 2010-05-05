(eval-when-compile
    (load "pmade-loadpath"))

;; Emacs nonstandard editing commands
(autoload 'zap-up-to-char "misc" nil t)

;; Gnus CVS
(when (and (= 22 emacs-major-version)
           (file-exists-p (concat pmade-site-lisp "/gnus")))
  (require 'gnus-load))

;; Use Aspell
(setq ispell-extra-args '("--rem-filter=nroff")
      ispell-program-name "/opt/local/bin/aspell")
(setenv "ASPELL_CONF" nil)
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

;; Tramp kicks ass
;; (setq tramp-default-method "ssh"
;;       tramp-verbose 3                   ; For reference if I need to
;;       tramp-debug-buffer nil            ; debug Tramp
;;       tramp-auto-save-directory "~/.emacs.d/autosave")
;; (require 'tramp)

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

;; Also try windmove
(windmove-default-keybindings 'hyper)
(setq windmove-wrap-around t)

;; Getting and fetching pastie.caboo.se
(autoload 'pastie-region "pastie" nil t)
(autoload 'pastie-buffer "pastie" nil t)
(autoload 'pastie-get    "pastie" nil t)

;; escreen
(load "escreen")
(escreen-install)

(defun pmade:escreen-get-active-screen-numbers-with-emphasis ()
  "List the screen numbers in the mode line"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
        (emphased "")
        (screen ""))
    (dolist (s escreens)
      (setq screen (number-to-string s))
      (setq emphased (concat emphased 
        (if (= escreen-current-screen-number s)
            (propertize (concat "[" screen "]") 'face 'highlight)
          screen) " ")))
    (message "active escreens: %s" emphased)))

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

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Magit (Git Interface for Emacs)
(autoload 'magit-status "magit" "Magit Status" t)

;; EasyPG http://www.easypg.org/
(require 'epa)
(setq epa-file-encrypt-to "61C4F407C4EB56B7"
      epa-popup-info-window nil)

;; Graphviz Dot Files
(setq graphviz-dot-indent-width 2)

;; ESS for R
(load (concat pmade-site-lisp "/ess/lisp/ess-site"))

;; I love Org Mode!
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(eval-after-load 'org '(load "~/.emacs.d/pmade/pmade-org"))

;; Muse is great for writing documents and articles
(autoload 'muse-mode-choose-mode "muse-mode" "Emacs Muse" t)
(add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode-choose-mode))
(eval-after-load "muse-mode" '(load "~/.emacs.d/pmade/pmade-muse"))

;; Simple Emacs Spreadsheet
(defun pmade:ses-export-buffer-to-tsv nil
  "Export the current SES buffer to a TSV file.  The file name
  will be derived from the current buffer name."
  (interactive)
  (let* ((fname (or (buffer-file-name) "export"))
         (bcell (get-text-property (point-min) 'intangible))
         (ecell (get-text-property (- (point-max) 2) 'intangible))
         (ses--curcell (cons bcell ecell)))
    (setq fname
          (if (string-match "\\..*$" fname)
              (replace-match ".tsv" nil nil fname)
            (concat fname ".tsv")))
    (ses-export-tab nil)
    (with-temp-file fname (insert (concat (car kill-ring) "\n")))))
  
(add-hook 'ses-mode-hook
  (lambda () 
    (add-hook 'after-save-hook 'pmade:ses-export-buffer-to-tsv t t)))
