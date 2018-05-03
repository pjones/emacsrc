;;; projectile-conf.el -- Settings for projectile.
(eval-when-compile
  (require 'projectile))

(custom-set-variables
 '(projectile-switch-project-action 'projectile-dired)
 '(projectile-mode-line
   '(:eval (format " [%s]" (projectile-project-name)))))

(add-to-list 'projectile-globally-ignored-modes "mu4e-.*-mode")
(add-to-list 'projectile-globally-ignored-modes "org-agenda-mode")

(add-to-list 'projectile-project-root-files "GNUmakefile")
(add-to-list 'projectile-project-root-files "Rakefile")
(add-to-list 'projectile-project-root-files "package.json")
(add-to-list 'projectile-project-root-files-bottom-up ".dir-locals.el")

(projectile-register-project-type
   'edify '("default.nix" "courses" "content")
   "nix-shell --run 'edify build courses/*'")
