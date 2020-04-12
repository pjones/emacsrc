;;; yasnippet-conf.el -- Settings for yasnippet
;;
;;; Commentary:
;;
;;; Code:
(require 'yasnippet)

(defvar pjones:snippets-dir
  (concat (file-name-directory (directory-file-name (file-name-directory load-file-name))) "snippets/")
  "The directory where I keep snippet files.")

(add-to-list 'yas-snippet-dirs pjones:snippets-dir)
(yas-reload-all)

(define-key yas-minor-mode-map (kbd "TAB") nil)

;;; yasnippet-conf.el ends here
