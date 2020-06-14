;;; magit-conf.el -- Customizations for magit.
;;
;;; Commentary:
;;
;;; Code:
(require 'magit)
(require 'evil)
(require 'evil-leader)

(eval-when-compile
  (load
   (concat
    (file-name-directory
     (or load-file-name
         byte-compile-current-file
         (buffer-file-name)))
    "../lisp/macros")))

(custom-set-variables
 '(magit-popup-use-prefix-argument 'default)
 '(magit-status-margin '(t age magit-log-margin-width nil 18))
 '(magit-status-show-hashes-in-headers t)
 '(magit-section-initial-visibility-alist
   '(([unpushed status] . show))))

(pjones:evil-override-mode magit-mode)

(pjones:evil-override-mode magit-status-mode
  "j" #'magit-section-forward
  "k" #'magit-section-backward
  "J" #'magit-status-jump ; "j"
  "K" #'magit-discard ; "k"
  "H" #'magit-dispatch ; "h"
  "L" #'magit-log ; "l"
  "gr" #'magit-refresh
  "gR" #'magit-refresh-all)

(evil-leader/set-key-for-mode 'magit-status-mode-map
  "m L" #'magit-log-refresh
  "m :" #'magit-git-command
  "m SPC" #'magit-diff-show-or-scroll-up
  "y m" #'magit-copy-buffer-revision)

(pjones:evil-override-mode magit-log-mode
  "H" #'magit-dispatch ; "h"
  "gr" #'magit-refresh
  "gR" #'magit-refresh-all)

(pjones:evil-override-mode magit-revision-mode
  "H" #'magit-dispatch ; "h"
  "gr" #'magit-refresh
  "gR" #'magit-refresh-all
  "v" (lookup-key evil-motion-state-local-map "v")
  "V" (lookup-key evil-motion-state-local-map "V")
  "\C-v" (lookup-key evil-motion-state-local-map "\C-v")
  "r" #'magit-reverse
  "R" #'magit-revert)

;;; magit-conf.el ends here
