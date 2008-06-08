;;; Gnus Configuration (http://gnus.org/)

;; Load the rest of my Gnus config files
(load "~/.gnus.d/options")
(load "~/.gnus.d/faces")
(load "~/.gnus.d/groups")
(load "~/.gnus.d/summary")
(load "~/.gnus.d/sources")
(load "~/.gnus.d/message")

;; Compile things like format strings
(gnus-compile)
