(eval-when-compile
    (load "pmade-loadpath"))

;; Gnus CVS
(require 'gnus-load)

;; Use Aspell
(setq ispell-program-name "aspell")

;; Better buffer switching and file finding
(setq
 ido-enable-flex-matching t
 ido-completion-buffer-all-completions t
 ido-auto-merge-work-directories-length 0)
(require 'ido)
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
(require 'pastie)

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
(dotimes (i 4) (elscreen-create))
(elscreen-goto 0)

;; Twitter
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(let ((twitter-conf "~/.comm-sync/etc/twitter/twit.el"))
  (when (file-exists-p twitter-conf) (load-file twitter-conf)))

;; Typing Breaks
(setq 
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
