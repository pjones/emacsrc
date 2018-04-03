;;; projects.el -- Configuration for various projects.
(require 'projectile)

;; TODO:
;;
;; 2) Add `compile' command that can be used from the command line
;; 3) Add a `diff' command just like the above
;; 4) Add a `man' command just like above


(defun pjones:projectile-mode-hook ()
  (add-to-list 'projectile-project-root-files "GNUmakefile")
  (add-to-list 'projectile-project-root-files "Rakefile")
  (add-to-list 'projectile-project-root-files "package.json")
  (add-to-list 'projectile-project-root-files-bottom-up ".dir-locals.el")
  (setq projectile-switch-project-action 'projectile-dired))

(add-to-list 'projectile-mode-hook 'pjones:projectile-mode-hook)
(projectile-mode)

;; Project settings (when I can't use a .dir-locals.el file):
;; (dir-locals-set-class-variables 'cltc-new-project
;;    '((nil . ((projectile-project-run-cmd  "bundle install")
;;              (projectile-project-test-cmd "bundle exec rake test")))))

;; (dir-locals-set-directory-class
;;    (expand-file-name "~/develop/scors/cltc-claims") 'cltc-new-project)
