;;; diary-lib-conf.el -- Settings for `diary-lib' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'diary-lib)

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

;;; diary-lib-conf.el ends here
