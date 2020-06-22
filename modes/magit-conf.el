;;; magit-conf.el -- Customizations for magit.
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'git-rebase)
(require 'magit)
(require 'with-editor)

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
  "]]" #'magit-section-forward
  "[[" #'magit-section-backward
  "J" #'magit-status-jump ; "j"
  "K" #'magit-discard ; "k"
  "H" #'magit-dispatch ; "h"
  "L" #'magit-log ; "l"
  "gr" #'magit-refresh
  "gR" #'magit-refresh-all)

(evil-leader/set-key-for-mode 'magit-status-mode-map
  "m :" #'magit-git-command
  "m L" #'magit-log-refresh
  "m r" #'magit-reverse
  "m R" #'magit-show-refs
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
  "r" #'magit-reverse
  "R" #'magit-revert)

(pjones:evil-override-mode magit-diff-mode
  "[[" #'magit-section-backward
  "]]" #'magit-section-forward
  "H" #'magit-dispatch ; "h"
  "K" #'magit-discard) ; "k"

(pjones:evil-override-mode magit-log-select-mode
  (kbd "RET") #'magit-log-select-pick
  [escape] #'magit-log-select-quit)

(pjones:evil-override-mode git-rebase-mode
  "L" #'git-rebase-label
  "gk" #'git-rebase-move-line-up
  "gj" #'git-rebase-move-line-down)

(evil-leader/set-key-for-mode 'git-rebase-mode-map
  "m c" #'with-editor-finish
  "m k" #'with-editor-cancel)

;;; magit-conf.el ends here
