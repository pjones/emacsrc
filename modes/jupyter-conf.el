;;; jupyter-conf.el -- Settings for `jupyter' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;; NOTE: For hacking the `jupyter' package to work with org-babel, see
;; the `pjones:org-babel-execute-src-block-for-jupyter' function in
;; "org-conf.el".
;;
;;; Code:

(require 'inheritenv)
(require 'jupyter)

;; `jupyter-session-with-random-ports' is called when starting a
;; kernel and it uses `with-temp-buffer' that doesn't preserve the
;; process environment and so the path environment variable from direnv
;; is lost.  This should fix it.
(inheritenv-add-advice #'jupyter-session-with-random-ports)

(let ((map jupyter-repl-interaction-mode-map))
  (keymap-set map "C-c C-c" #'jupyter-eval-buffer))

;;; jupyter-conf.el ends here
