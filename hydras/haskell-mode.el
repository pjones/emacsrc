;;; haskell-mode.el ;; Hydras for haskell-mode

;;; Commentary:

;;; Code:
(require 'hydra)

(defhydra pjones:hydras:haskell-mode (:hint nil)
  "
^Imports^          ^GHCi^              ^Insert/Edit^          ^Run
^^^^^^^^^-------------------------------------------------------------------------
_C-c C-i_: jump    _C-c C-;_: info     _C-c C-0_: cost center  _C-c c_: compile
_C-c C-l_: return  _C-c C-r_: reload   _C-c C-n_: kill module
_C-c C-s_: sort    _C-c C-t_: type     _C-c C-e_: edit cabal
"
  ("C-c C-;" dante-info)
  ("C-c C-0" haskell-mode-toggle-scc-at-point :color blue)
  ("C-c C-e" pjones:haskell-edit-cabal-file :color blue)
  ("C-c C-i" haskell-navigate-imports)
  ("C-c C-l" haskell-navigate-imports-return :color blue)
  ("C-c C-n" pjones:haskell-module-name-to-kill-ring :color blue)
  ("C-c C-r" dante-restart)
  ("C-c C-s" pjones:haskell-sort-imports)
  ("C-c C-t" dante-type-at :color blue))

;;; haskell-mode ends here
