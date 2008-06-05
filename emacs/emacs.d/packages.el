;; Better buffer switching and file finding
(setq ido-completion-buffer-all-completions t)
(setq ido-auto-merge-work-directories-length -1)
(require 'ido)
(ido-mode t)

;; Save you place in files you edit
(setq-default save-place t)
(require 'saveplace)

;; Add window numbers
(require 'window-number)
(window-number-mode 1)

;; Getting and fetching pastie.caboo.se
(require 'pastie)

;; Place the time in the title bar
(setq display-time-format "%A, %B %d, %Y  %H:%M")
(require 'title-time)

;; If ElScreen can be used, do so
(when (file-exists-p (concat pmade-site-lisp "/apel"))
  (add-to-list 'load-path (concat pmade-site-lisp "/apel"))
  (add-to-list 'load-path (concat pmade-site-lisp "/emu"))
  (setq elscreen-display-screen-number nil)
  (setq elscreen-display-tab nil)
  (setq elscreen-tab-display-kill-screen t)
  (setq elscreen-tab-display-control nil)
  (require 'elscreen))

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
