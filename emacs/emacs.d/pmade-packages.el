(eval-when-compile
    (load "pmade-loadpath"))

;; Emacs nonstandard editing commands
(autoload 'zap-up-to-char "misc" nil t)

;; Gnus CVS
(require 'gnus-load)

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

;; Save you place in files you edit
(setq-default save-place t)
(setq save-place-file "~/.comm-sync/var/emacs/places.el")
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

;; Twitter
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(eval-after-load "twitter"
  (let ((twitter-conf "~/.comm-sync/etc/twitter/twit.el"))
    (when (file-exists-p twitter-conf) (load-file twitter-conf))))

;; Go-to Last Change
(autoload 'goto-last-change "goto-chg" nil t)
(autoload 'goto-last-change-reverse "goto-chg" nil t)

;; Typing Breaks
(setq 
 type-break-query-mode nil
 type-break-mode-line-message-mode t
 type-break-demo-functions '(type-break-demo-boring)
 type-break-demo-boring-stats t
 type-break-file-name (expand-file-name "~/.comm-sync/etc/type-break"))
(type-break-mode)

;; Editing the Emacs Wiki
(defun pmade-oddmuse-hook ()
  "Hook is run after oddmuse is started"
  (unless (string-match "answer" oddmuse-post)
    (setq oddmuse-post (concat "answer=emacs;" oddmuse-post))))

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
