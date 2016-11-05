;;; projects.el -- Configuration for various projects.
(eval-when-compile
  (require 'server)
  (require 'projectile))

;; TODO:
;;
;; 1) Remove projectile
;; 2) Add `compile' command that can be used from the command line
;; 3) Add a `diff' command just like the above
;; 4) Add a `man' command just like above


;; Projectile is used to manage projects:
;; Need to fix the key binding before loading!
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(require 'projectile)

(defun pjones:projectile-mode-hook ()
  (add-to-list 'projectile-project-root-files "GNUmakefile")
  (add-to-list 'projectile-project-root-files "Rakefile")
  (add-to-list 'projectile-project-root-files "package.json")
  (add-to-list 'projectile-project-root-files-bottom-up ".dir-locals.el")
  (setq projectile-switch-project-action 'projectile-dired))

(add-to-list 'projectile-mode-hook 'pjones:projectile-mode-hook)

(if (and (boundp 'server-name) (string= server-name "server"))
    (projectile-global-mode))

;; Project settings (when I can't use a .dir-locals.el file):
(dir-locals-set-class-variables 'cltc-new-project
   '((nil . ((projectile-project-run-cmd  "bundle install")
             (projectile-project-test-cmd "bundle exec rake test")))))

(dir-locals-set-directory-class
   (expand-file-name "~/develop/scors/cltc-claims") 'cltc-new-project)
