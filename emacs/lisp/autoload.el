;;; autoload.el -- Automatically load some libraries when their
;;; functions are called.  Mostly for stuff in my third-party
;;; directory.

;; Go-to Last Change
(autoload 'goto-last-change "goto-chg" nil t)
(autoload 'goto-last-change-reverse "goto-chg" nil t)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode" nil t)

;; Visual Bookmarks
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

;; Highline (http://emacswiki.org/emacs/HighlineMode)
(autoload 'highline-mode "highline" "Highline Mode" t)

;; RHTML (.html.erb for Rails)
(autoload 'rhtml-mode "rhtml-mode" "RHTML" t)
