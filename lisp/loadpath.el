;;; loadpath.el -- Correctly set my load-path variable.

;; Some basic variables used by my other configuration files.
(defvar pjones:lisp-dir
  (file-name-directory load-file-name)
  "The directory where I store my general Emacs configuration files.")

(defvar pjones:modes-dir
  (concat (file-name-directory (directory-file-name pjones:lisp-dir)) "modes/")
  "The directory where I keep mode-specific configuration files.")

(defvar pjones:lisp-files
  '( "themes"
     "mode-line"
     "code"
     "modes"
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
