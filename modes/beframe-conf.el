;;; beframe-conf.el -- Settings for `beframe' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'beframe)

(defvar consult-buffer-sources)
(declare-function consult--buffer-state "consult")

;;; Take from the beframe manual:
;;;
;;; https://protesilaos.com/emacs/beframe#h:1c2d3d64-aa7b-4585-a418-ccedbb548b38
(with-eval-after-load 'consult
  (defface beframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")

  (defun my-beframe-buffer-names-sorted (&optional frame)
    "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  (defvar beframe-consult-source
    `( :name     "Frame-specific buffers (current frame)"
       :narrow   ?F
       :category buffer
       :face     beframe-buffer
       :history  beframe-history
       :items    ,#'my-beframe-buffer-names-sorted
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'beframe-consult-source))

;;; beframe-conf.el ends here
