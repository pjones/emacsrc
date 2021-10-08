;;; loadpath.el -- Correctly set my load-path variable.
;;
;;; Commentary:
;;
;;; Code:

;; Shouldn't be needed in recent versions of Emacs, but without this
;; line some packages from Nix don't get put into `load-path'.
(package-initialize)

;; Some basic variables used by my other configuration files.
(defvar pjones:lisp-dir
  (file-name-directory load-file-name)
  "The directory where I store my general Emacs configuration files.")

(defvar pjones:lisp-files
  '( "buffers"
     "modes"
     "server"
     "themes"
     "code"
     "options"
     "automode"
     "functions"
     "interactive"
     "completion"
     "keys"
     "suspend"
    )
  "A list of my config files to load in the correct order.")

(defun pjones:load-configuration-files ()
  "Load all of my lisp configuration files."
  (dolist (file pjones:lisp-files)
    (load (concat pjones:lisp-dir file))))
