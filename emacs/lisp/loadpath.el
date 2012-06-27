;;; loadpath.el -- Correctly set my load-path variable.

;; Some basic variables used by my other configuration files.
(defvar pjones:lisp-dir (concat user-emacs-directory "pjones/lisp/")
  "The directory where I store my general Emacs configuration files.")

(defvar pjones:modes-dir (concat user-emacs-directory "pjones/modes/")
  "The directory where I keep mode-specific configuration files.")

(defvar pjones:site-lisp (concat user-emacs-directory "share/emacs/site-lisp")
  "The directory where I install extra software packages like org-mode.")

(defvar pjones:lisp-files
  '("code" "modes" "packages" "options" "autoload" "automode"
    "functions" "interactive" "completion" "keys" "gnus")
  "A list of my config files to load in the correct order.")

(defvar pjones:theme-files (concat user-emacs-directory "themes")
  "Where custom theme files live.")

(defvar pjones:default-theme 'devalot
  "The name of the default theme to load.")

;; Add my "site-lisp" directory to load-path.
(add-to-list 'load-path pjones:site-lisp)

;; Add all directories in "site-lisp" to the load-path.
(dolist (d (directory-files pjones:site-lisp t))
  (cond
   ((file-directory-p (concat d "/lisp"))
    (add-to-list 'load-path (concat d "/lisp")))
   ((file-directory-p d)
    (add-to-list 'load-path d))))

;; Set up the custom theme path.
(add-to-list 'custom-theme-load-path pjones:theme-files)

(defun pjones:load-configuration-files ()
  "Load all of my lisp configuration files."
  (dolist (file pjones:lisp-files)
    (load (concat pjones:lisp-dir file))))
