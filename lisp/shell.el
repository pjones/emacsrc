;;; shell.el -- Integration with various shell commands. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'cl-macs)
(require 'dash)
(require 's)

(declare-function json-read-file "json")
(declare-function pjones:vterm "vterm")
(declare-function project-name "project")
(declare-function project-prefixed-buffer-name "project")
(declare-function project-root "project")
(declare-function vterm--self-insert "vterm")
(declare-function vterm--set-title "vterm")
(defvar vterm-kill-buffer-on-exit)

(defvar pjones:shell-mode-map
  (define-keymap
    "q" #'pjones:shell-kill-if-done
    "C-c C-c" #'kill-current-buffer)
  "Keymap for `pjones:shell-mode'.")

(define-minor-mode pjones:shell-mode
  "A minor mode for temporary shells."
  :init-value nil
  :lighter nil)

(defun pjones:shell-kill-if-done ()
  "Kill the current `pjones:shell-mode' buffer if the process is done."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (if (or (not process) (eq 'exit (process-status process)))
        (kill-current-buffer)
      (vterm--self-insert))))

(defun pjones:shell-on-exit (buffer event)
  "Hook called with a shell process exits.
BUFFER is the vterm buffer and EVENT is the exit message."
  (let* ((proc (get-buffer-process buffer))
         (clean (or (and proc (= 0 (process-exit-status proc)))
                    (string-match-p "^finished" event))))
    (if (and vterm-kill-buffer-on-exit (not clean))
        (setq-local vterm-kill-buffer-on-exit nil)
      (message "%s: %s" (buffer-name buffer) (s-trim event)))
    (setq mode-name (s-trim event))))

(cl-defun pjones:shell-command (&key bufname command close project)
  "Execute a shell command like `async-shell-command'.

:BUFNAME should be the name of a buffer that will be attached to the
process.  If :BUFNAME is nil then it is derived from the command
name.

:COMMAND is a shell command to run.

If :PROJECT is non-nil run the command in the project's root directory
and prefix the buffer name with the project name.

If :CLOSE is non-nil then automatically close the window when the
process exits."
  (require 'vterm)
  (let* ((default-directory (if project
                                (project-root (project-current t))
                              default-directory))
         (name (if project
                   (pjones:project-buffer-name (or bufname command))
                 (or bufname (concat "*" command "*"))))
         (buffer (pjones:vterm :name name
                               :keep (not close)
                               :exit #'pjones:shell-on-exit
                               :command (format
                                         "bash -c %s"
                                         (shell-quote-argument command)))))
    (with-current-buffer buffer
      (pjones:shell-mode 1)
      (setq mode-name "running"
            header-line-format command))
    (save-selected-window
      (pop-to-buffer
       buffer '((display-buffer-in-side-window) .
                ((side          . bottom)
                 (window-height . 0.2)))))))

(defun pjones:nix-flake-lock-json (&optional file)
  "Return the \"flake.lock\" JSON data for FILE.
If no \"flake.lock\" file is found, return nil."
  (require 'json)
  (when-let* ((lockfile "flake.lock")
              (dir (locate-dominating-file (or file ".") lockfile)))
    (json-read-file (concat dir lockfile))))

(defun pjones:nix-flake-inputs (json &optional node)
  "Return a list of input names from JSON.
NODE should be the name of an input node, or nil to use the root node."
  (mapcar #'car
          (alist-get
           'inputs
           (alist-get
            (intern (or node "root"))
            (alist-get
             'nodes json)))))

(defun pjones:project-buffer-name (name)
  "Return a string based on NAME that includes the current project name."
  (require 'project)
  (concat "*"
          (if-let ((proj (project-current nil)))
              (project-name proj)
            (file-name-nondirectory
             (directory-file-name default-directory)))
          ": "
          name
          "*"))

(defun pjones:colmena-apply-local (&optional now)
  "Run a local colmena deployment.

When NOW is non-nil, apply it now instead of waiting for the next boot."
  (interactive "P")
  (let* ((command "colmena apply-local")
         (script (string-join
                  (list command "--sudo" (unless now "boot"))
                  " ")))
    (pjones:shell-command :bufname command
                          :command script
                          :project t)))

(defun pjones:nix-flake-check (&optional update)
  "Run \"nix flake check\".
If UPDATE is non-nil then run \"nix flake update\" first."
  (interactive "P")
  (require 'project)
  (let* ((command "nix flake check")
         (script (concat (if update "nix flake update && ") command)))
    (pjones:shell-command :bufname command
                          :command script
                          :project t)))

(defun pjones:nix-flake-update (inputs)
  "Run \"nix flake update\" on the given flake INPUTS."
  (interactive (let ((json (pjones:nix-flake-lock-json)))
                 (list
                  (completing-read-multiple
                   "Flake inputs: "
                   (pjones:nix-flake-inputs json)))))
  (let* ((command "nix flake update")
         (script (string-join (cons command (-uniq inputs)) " ")))
    (pjones:shell-command :bufname command
                          :command script
                          :project t)))

(defun pjones:nix-flake-override (input path &optional nested)
  "Override a flake INPUT to point at PATH.
If NESTED is non-nil then it should be the name of a nested input of
INPUT."
  (interactive (let* ((json (pjones:nix-flake-lock-json))
                      (name (completing-read "Flake input: "
                                             (pjones:nix-flake-inputs json)))
                      (sub (if current-prefix-arg
                               (completing-read
                                "Nested flake input: "
                                (pjones:nix-flake-inputs json name))))
                      (dir (read-directory-name "Path: " nil nil t)))
                 (list name dir sub)))
  (let* ((command "nix flake update")
         (override (if nested (concat input "/" nested)))
         (dir (expand-file-name path))
         (script (string-join (list command input "--override-input"
                                    (or override input) dir)
                              " ")))
    (pjones:shell-command :bufname command
                          :command script
                          :close t
                          :project t)))

;;; shell.el ends here
