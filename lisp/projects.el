;;; projects.el -- Configuration for various projects.
(eval-when-compile
  (require 'projectile))

;; Projectile is used to manage projects:
;; Need to fix the key binding before loading!
(setq projectile-keymap-prefix (kbd "C-c C-p"))
(projectile-global-mode)

;; Which files mark a project:
(add-to-list 'projectile-project-root-files "GNUmakefile")
(add-to-list 'projectile-project-root-files "Rakefile")
(add-to-list 'projectile-project-root-files "package.json")
(add-to-list 'projectile-project-root-files-bottom-up ".dir-locals.el")

(setq projectile-switch-project-action 'projectile-dired)

;; Project settings (when I can't use a .dir-locals.el file):
(dir-locals-set-class-variables 'cltc-new-project
   '((nil . ((projectile-project-run-cmd  "bundle install")
             (projectile-project-test-cmd "bundle exec rake test")))))

(dir-locals-set-directory-class
   (expand-file-name "~/develop/scors/cltc-claims") 'cltc-new-project)

(dir-locals-set-class-variables 'xmonad
  '((nil . ((projectile-project-compilation-cmd "cd xmonad && script/nix-env.sh")
            (projectile-project-run-cmd "cd xmonad && script/install.sh restart")))))

(dir-locals-set-directory-class
 (expand-file-name "~/core/xmonad") 'xmonad)
