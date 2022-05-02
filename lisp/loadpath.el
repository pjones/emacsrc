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
  '("options"
     "modes"
     "server"
     "code"
     "automode"
     "functions"
     "suspend")
  "A list of my config files to load in the correct order.")

(defvar pjones:interactive-lisp-files
  '("buffers"
    "ui"
    "interactive"
    "completion"
    "keys")
  "Lisp files that should be loaded in non-batch Emacs.")

(defun pjones:load-configuration-files (&optional files)
  "Load all of my Lisp configuration files.
If FILES is non-nil, load files from that list instead."
  (dolist (file (or files pjones:lisp-files))
    (load (concat pjones:lisp-dir file)))
  ;; Load "interactive" files too:
  (when (and (null files) (not noninteractive))
    (pjones:load-configuration-files pjones:interactive-lisp-files)))

;;; loadpath.el ends here
