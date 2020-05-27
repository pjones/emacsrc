;;; js-conf.el -- Configuration options for js-mode (JavaScript).
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'js))

;; JavaScript mode settings
(setq js-indent-level 2
      js-flat-functions t)

;; FIXME:
;;   (define-key map (kbd "j c") #'pjones:indium-start-chrome)
;;   (define-key map (kbd "j n") #'pjones:indium-start-node)

;;; js-conf.el ends here

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
