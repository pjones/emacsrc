;;; shell.el -- Integration with various shell commands. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'cl-macs)
(require 'dash)
(require 's)

(declare-function comint-output-filter "comint")
(declare-function comint-term-environment "comint")
(declare-function json-read-file "json")
(declare-function project-name "project")
(declare-function project-prefixed-buffer-name "project")
(declare-function project-root "project")

(defvar pjones:shell-mode-map
  (define-keymap
    "g" #'pjones:shell-repeat-if-done
    "q" #'pjones:shell-kill-if-done
    "C-c C-c" #'kill-current-buffer)
  "Keymap for `pjones:shell-mode'.")

(define-minor-mode pjones:shell-mode
  "A minor mode for temporary shells."
  :init-value nil
  :lighter nil)

(defun pjones:shell-is-running ()
  "Return non-nil if the process in this buffer is still running."
  (let ((process (get-buffer-process (current-buffer))))
    (and process (not (eq 'exit (process-status process))))))

(defun pjones:shell-kill-if-done ()
  "Kill the current `pjones:shell-mode' buffer if the process is done."
  (interactive)
  (if (pjones:shell-is-running)
      (call-interactively #'self-insert-command)
    (kill-current-buffer)))

(defun pjones:shell-repeat-if-done ()
  "Rerun the current command if it has finished."
  (interactive)
  (if (pjones:shell-is-running)
      (call-interactively #'self-insert-command)
    (revert-buffer)))

(defun pjones:shell-on-exit (success process event)
  "Hook called with a shell process exits.
PROCESS is the process and EVENT is the exit message.  If SUCCESS is
non-nil and a function, call it if the process exited cleanly."
  (let* ((buffer (process-buffer process))
         (clean (or (and process (= 0 (process-exit-status process)))
                    (string-match-p "^finished" event))))
    (message "%s: %s" (buffer-name buffer) (s-trim event))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (if (and clean (functionp success))
              (funcall success))))))

(cl-defun pjones:shell-command (&key bufname command success close project)
  "Execute a shell command like `async-shell-command'.

:BUFNAME should be the name of a buffer that will be attached to the
process.  If :BUFNAME is nil then it is derived from the command
name.

:COMMAND A list or a string.  If it is a string it will be given to the
shell to run as a shell command.

If :PROJECT is non-nil run the command in the project's root directory
and prefix the buffer name with the project name.

If :CLOSE is non-nil then automatically close the window when the
process exits.

:SUCCESS a function to call if the process exits cleanly.  The function
is called with no arguments and with the process buffer as the current
buffer.  If the buffer is no longer live then this function will not be
called."
  (let ((default-directory (if project
                               (project-root (project-current t))
                             default-directory))
        (name (if project
                  (pjones:project-buffer-name (or bufname command))
                (or bufname (format "*%s*" command)))))
    (pjones:shell-command-in-buffer
     :buffer (get-buffer-create name)
     :command command
     :success (lambda ()
                (if (functionp success)
                    (funcall success))
                (if close (kill-buffer))))))

(cl-defun pjones:shell-command-in-buffer (&key buffer command success)
  "Run COMMAND in an existing BUFFER.
If SUCCESS is non-nil, and a function, call it if the process
exits successfully."
  (require 'comint)
  (when-let ((old-proc (get-buffer-process buffer)))
    (kill-process old-proc))
  (let* ((proc-cmd (if (listp command) command
                     (list shell-file-name shell-command-switch command)))
         (display (string-join proc-cmd " "))
         (process-environment
          (append (and (natnump async-shell-command-width)
                       (list (format "COLUMNS=%d" async-shell-command-width)))
           (comint-term-environment)
           process-environment)))
    (with-current-buffer buffer
      (funcall async-shell-command-mode)
      (pjones:shell-mode 1)
      (setq header-line-format display)
      (setq-local revert-buffer-function
                  (lambda (&rest _)
                    (pjones:shell-command-in-buffer
                     :buffer buffer
                     :command command
                     :success success))))
    (when (make-process
           :name (buffer-name buffer)
           :buffer buffer
           :command proc-cmd
           :connection-type 'pty
           :filter #'comint-output-filter
           :sentinel (apply-partially #'pjones:shell-on-exit success))
      (save-selected-window
        (pop-to-buffer
         buffer '((display-buffer-in-side-window) .
                  ((side          . bottom)
                   (window-height . 0.2))))))))

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
  (let* ((command "nix run .#colmena -- apply-local")
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
