;;; projectile-conf.el -- Settings for projectile.
;;
;;; Commentary:
;;
;;; Code:
(require 'ivy)
(require 'project)
(require 'projectile)

(custom-set-variables
 '(projectile-mode-line nil)
 '(projectile-switch-project-action 'projectile-dired)
 '(projectile-completion-system 'ivy))

;; Modes to ignore:
(add-to-list 'projectile-globally-ignored-modes "mu4e-.*-mode")
(add-to-list 'projectile-globally-ignored-modes "org-agenda-mode")

;; Files to consider as root project files:
(add-to-list 'projectile-project-root-files "GNUmakefile")
(add-to-list 'projectile-project-root-files "Rakefile")
(add-to-list 'projectile-project-root-files "package.json")
(add-to-list 'projectile-project-root-files-bottom-up ".dir-locals.el")
(add-to-list 'projectile-project-root-files-bottom-up "Setup.hs")

(projectile-register-project-type
 'edify '("default.nix" "courses" "content")
 :compile "nix-shell --run 'eval \"$buildPhase\"'")

(projectile-register-project-type
 'haskell '("default.nix" "Setup.hs")
 :compile "cabal build && cabal test")

;;; Utility Functions

(defun pjones:projectile-project-root (&optional dont-ask)
  "Return the root directory of the current project.
When a project is not active and DO NT-ASK is nil, prompt the user
to select a project.  If DONT-ASK is non-nil then simply return
the default directory."
  (cond
   ((projectile-project-p)
    (projectile-project-root))
   ((null dont-ask)
    (completing-read
     "Select project: "
     (projectile-relevant-known-projects)))
   (t default-directory)))

(defun pjones:projectile-dired (&optional dont-ask)
  "Open dired for a project.

Similar to `projectile-dired' except if you're not currently in a
project prompt for which project to use.

If DONT-ASK is non-nil, don't prompt for the project and use the
default directory instead."
  (interactive "P")
  (let ((dir (pjones:projectile-project-root dont-ask)))
    (dired dir)))


(defun pjones:projectile-switch-to-buffer ()
  "Call `switch-to-buffer' or `projectile-switch-to-buffer'."
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       'projectile-switch-to-buffer
     'switch-to-buffer)))

(defun pjones:projectile-find-file ()
  "Call `find-file' or `projectile-find-file'."
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       'projectile-find-file
     'find-file)))

;; Taken from: https://github.com/bbatsov/projectile/issues/1282
;; project-find-function which uses projectile methods to find
;; the projectile project associated with a directory.
;; If projectile not loaded, or directory is not in a project,
;; hopefully returns nil.
(defun pjones:projectile-project-find (dir)
  "Find the project that exists in DIR."
  (if (fboundp 'projectile-project-root)
      (let ((root (projectile-project-root dir)))
        (and root (cons 'transient root)))))
(add-to-list 'project-find-functions #'pjones:projectile-project-find)

;;; projectile-conf.el ends here
