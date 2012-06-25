;;; loadpath.el -- Correctly set my load-path variable.

;; Some basic variables used by my other configuration files.
(defvar pjones:lisp-dir (concat user-emacs-directory "pjones/lisp/")
  "The directory where I store my general Emacs configuration files.")

(defvar pjones:modes-dir (concat user-emacs-directory "pjones/modes/")
  "The directory where I keep mode-specific configuration files.")

(defvar pjones:site-lisp (concat user-emacs-directory "share/emacs/site-lisp")
  "The directory where I install extra software packages like org-mode.")

(defvar pjones:lisp-files
  '("modes" "packages" "automode" "interactive" "keys")
  "A list of my config files to load in the correct order.")

;; Add my "site-lisp" directory to load-path.
(add-to-list 'load-path pjones:site-lisp)

;; Add all directories in "site-lisp" to the load-path.
(dolist (d (directory-files pjones:site-lisp t))
  (if (file-directory-p d) (add-to-list 'load-path d)))

(defun pjones:load-configuration-files ()
  "Load all of my lisp configuration files."
  (dolist (file pjones:lisp-files)
    (load (concat pjones:lisp-dir file))))
