;;; speedbar.el -- quick access to files and tags in a frame.
(eval-when-compile
  (require 'speedbar))

(setq speedbar-show-unknown-files t)
(speedbar-add-supported-extension ".hs")

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
