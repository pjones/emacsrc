;;; posframe-conf.el -- Settings for `posframe'
;;
;;; Commentary:
;;
;;; Code:
(require 'posframe)

(custom-set-variables
  '(posframe-arghandler #'pjones:posframe-arghandler))

(defun pjones:posframe-arghandler (_buffer name value)
  "Override some posframe arguments.
NAME is the name of the incoming argument and VALUE is its value."
  (cond
   ((eq name :x-pixel-offset) 10)
   ((eq name :y-pixel-offset) 10)
   ((eq name :background-color) (face-background 'fringe nil t))
   ((eq name :internal-border-width) 2)
   ((eq name :internal-border-color) (face-background 'mode-line nil t))
   ((eq name :foreground-color) (face-foreground 'fringe nil t))
   ((eq name :poshandler) #'posframe-poshandler-absolute-x-y)
   ((eq name :position) '(0 . 0))
   ((eq name :left-fringe) (frame-char-width))
   (t value)))

;;; posframe-conf.el ends here
