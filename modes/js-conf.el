;;; js-conf.el -- Configuration options for js-mode (JavaScript).
;;; Commentary:
;;; Code:
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'js))

;; JavaScript mode settings
(setq js-indent-level 2
      js-flat-functions t)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
