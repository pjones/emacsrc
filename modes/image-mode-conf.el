;;; image-mode-conf.el -- Settings for image-mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'image-mode)
(require 'cl-macs)

(defvar-local image-dimensions-minor-mode-dimensions nil
  "Buffer-local image dimensions for `image-dimensions-minor-mode'.")

(define-minor-mode image-dimensions-minor-mode
  "Displays the image dimensions in the mode line."
  :init-value nil
  :lighter image-dimensions-minor-mode-dimensions
  (when (not image-dimensions-minor-mode-dimensions)
    (let ((image (image-get-display-property)))
      (when image
        (setq image-dimensions-minor-mode-dimensions
              (cl-destructuring-bind (width . height)
                  (image-size image :pixels)
                (format " (%dx%d)" width height)))))))

(add-hook 'image-mode-hook 'image-dimensions-minor-mode)
(add-hook 'image-mode-hook 'eimp-mode)

;;; image-mode-conf.el ends here
