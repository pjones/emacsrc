;;; loadpath.el -- Correctly set my load-path variable.

;; Some basic variables used by my other configuration files.
(defvar pjones:lisp-dir (concat user-emacs-directory "pjones/lisp/")
  "The directory where I store my general Emacs configuration files.")

(defvar pjones:modes-dir (concat user-emacs-directory "pjones/modes/")
  "The directory where I keep mode-specific configuration files.")

(defvar pjones:site-lisp (concat user-emacs-directory "share/emacs/site-lisp")
  "The directory where I install extra software packages like org-mode.")

(defvar pjones:lisp-files
  '("packages")
  "A list of my config files to load in the correct order.")

;; Add my "site-lisp" directory to load-path.
(add-to-list 'load-path pjones:site-lisp)

;; Add all directories in "site-lisp" to the load-path.
(dolist (d (directory-files pjones:site-lisp t))
  (if (file-directory-p d) (add-to-list 'load-path d)))

;; Automatically load my mode configuration files after a mode starts.
(dolist (file (directory-files pjones:modes-dir t))
  (let ((basename (file-name-sans-extension (file-name-nondirectory file))))
    (when (string-match "\\(-conf\\)$" basename)
      (eval-after-load (intern (replace-match "" t t basename))
        (load file)))))
