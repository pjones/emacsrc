;;; sh-script-conf.el -- Settings for sh-mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'reformatter)
(require 'sh-script)

(custom-set-variables
 '(sh-basic-offset 2))

(reformatter-define sh-format
  :program "shfmt"
  :args '("-i" "2")
  :group 'sh-mode)

(add-to-list 'sh-mode-hook #'sh-format-on-save-mode)

;;; sh-script-conf.el ends here
