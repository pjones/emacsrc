;;; dired.el --- Hydras for `dired-mode'.

;;; Commentary:
;;
;; My hydras for dired-mode.

;;; Code:
(require 'dired)
(require 'dired-aux)
(require 'dired-filter)
(require 'dired-narrow)
(require 'dired-x)
(require 'hydra)
(require 'noccur)

;; External functions:
(declare-function pjones:dired-mark-all-files "../modes/dired-conf.el")

(defhydra pjones:hydras:dired-mode (:hint nil) "
 ^Narrowing^        ^Marking^           ^Operations^
-----------------------------------------------------------
  _/ n_: name        _M n_: name         _o_: noccur marked
  _/ r_: regexp      _M r_: regexp       _q_: query replace
  _/ ._: ext         _M ._: ext          _C_: copy
  _/ f_: files       _M f_: files        _d_: delete
  _/ d_: dirs        _M d_: dirs         _R_: rename
  _/ s_: symlinks    _M s_: symlinks     _T_: touch
  _/ m_: mode        _M m_: mode         _S_: symlink
  _/ p_: pop one     _M u_: none         _F_: find
  _/ /_: pop all     _M a_: all
"
  ("/ n" dired-filter-by-name)
  ("/ r" dired-filter-by-regexp)
  ("/ ." dired-filter-by-extension)
  ("/ f" dired-filter-by-file)
  ("/ d" dired-filter-by-directory)
  ("/ s" dired-filter-by-symlink)
  ("/ m" dired-filter-by-mode)
  ("/ p" dired-filter-pop)
  ("/ /" dired-filter-pop-all)
  ("M n" dired-filter-mark-by-name)
  ("M r" dired-filter-mark-by-regexp)
  ("M ." dired-filter-mark-by-extension)
  ("M f" dired-filter-mark-by-file)
  ("M d" dired-filter-mark-by-directory)
  ("M s" dired-filter-mark-by-symlink)
  ("M m" dired-filter-mark-by-mode)
  ("M u" dired-unmark-all-marks)
  ("M a" pjones:dired-mark-all-files)
  ("o" noccur-dired)
  ("q" dired-do-query-replace-regexp)
  ("C" dired-do-copy)
  ("d" dired-flag-file-deletion)
  ("R" dired-do-rename)
  ("T" dired-do-touch)
  ("S" dired-do-relsymlink)
  ("F" find-dired))

;;; dired.el ends here
