;;; elscreen-conf.el -- Settings for elscreen.el
(eval-when-compile
  (require 'elscreen))

(defhydra hydra-elscreen (:hint nil :color blue)
  "
^Screens^         ^Jumping^
---------------------------------
  _c_: create     _t_: toggle
  _C_: clone      _n_: next
  _r_: rename     _p_: previous
  _m_: message    _s_: select
  _S_: swap
"
  ("c" elscreen-create)
  ("C" elscreen-clone)
  ("r" elscreen-screen-nickname)
  ("m" elscreen-last-message)
  ("S" elscreen-swap)
  ("t" elscreen-toggle)
  ("n" elscreen-next)
  ("p" elscreen-previous)
  ("s" elscreen-select-and-goto))

(custom-set-variables
 `(elscreen-prefix-key ,(kbd "s-z"))
 '(elscreen-default-buffer-initial-message nil)
 '(elscreen-display-tab nil)
 '(elscreen-tab-display-control nil))

;; elscreen loads too late so we need to help it boot:
(elscreen-make-frame-confs (selected-frame))
