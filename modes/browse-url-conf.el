;;; browse-url-conf.el -- Settings for browse-url.
(eval-when-compile
  (require 'browse-url))

;; Dependencies.
(require 'exwm)
(require 'async)
(require 'browse-url) ;; If `C-z o' loads this file.

;; FIXME: Turn this into a minor-mode.

(defvar pjones:url-shortcuts
  '("http://duckduckgo.com/?q=%s"
    "http://www.google.com/search?q=%s"
    "http://en.wikipedia.org/w/index.php?title=Special:Search&search=%s&go=Go")
;; FIXME: add hoogle, hackage, imdb, amazon, etc.
  "Custom URLs that will have %s replaced with a search string.")

;; Taken from exwm-surf: Copyright (C) 2017 craven@gmx.net
(defun exwm-surf-set-prop (prop winid value)
  "Set property PROP on X window WINID to VALUE."
  (start-process-shell-command "xprop" nil (format "xprop -id %s -f %s 8s -set %s \"%s\"" winid prop prop value)))

;; Taken from exwm-surf: Copyright (C) 2017 craven@gmx.net
(defun exwm-surf-get-prop (prop winid)
  "Read property PROP from X window WINID."
  (let ((text (shell-command-to-string (format "xprop -notype -id %s %s" winid prop))))
    (string-match "\"\\(.*\\)\"" text)
    (match-string 1 text)))

(defun pjones:browse-url-browser-function (url &optional new-window &rest args)
  "Open URL in a external browser.

If NEW-WINDOW is non-nil then always open a new browsers window.
Other arguments in ARGS are ignored."
  (interactive)
  (if (and (derived-mode-p 'exwm-mode) (not new-window))
      (cond
       ((string= exwm-class-name "Surf")
        (exwm-surf-set-prop "_SURF_GO" (exwm--buffer->id (current-buffer)) url))
       (t (async-start-process "surf" "surf" nil url)))
  (async-start-process "firefox" "firefox" nil "--new-window" url)))

(defun pjones:extract-url-from-surf ()
  "Extract the URL from the Surf window.

Assumes the current window is a Surf window."
  (let ((winid (if (derived-mode-p 'exwm-mode)
                    (exwm--buffer->id (current-buffer))
                  (error "Not an exwm window"))))
    (exwm-surf-get-prop "_SURF_URI" winid)))

(defun pjones:browse-url-prompt ()
  "Prompt for a URL.

If the current window is a browser, extract a URL from it."
  (let ((prompt "URL: ")
        (default (when (derived-mode-p 'exwm-mode)
                   (cond
                    ((string= exwm-class-name "Surf")
                     (pjones:extract-url-from-surf))))))
    (if default (list (read-string prompt default)
                      (not (eq (null browse-url-new-window-flag)
                               (null current-prefix-arg))))
      (browse-url-interactive-arg prompt))))

(defun pjones:browse-url (url &rest args)
  "Ask a browser to display URL.

If URL is nil, prompt for it.  ARGS are passed to the real
`browse-url' unchanged."
  (interactive (pjones:browse-url-prompt))
  (browse-url url (car args)))

(defun pjones:browse-url-shortcut (new-window)
  "Browse a URL after picking a shortcut.

Prompts for a shortcut and search string.  If NEW-WINDOW is
non-nil then display the URL in a new window."
  (interactive "P")
  (let* ((shortcut (completing-read "Shortcut: " pjones:url-shortcuts))
         (url (if (string-match-p "%s" shortcut)
                  (format shortcut (read-string "Search: "))
                shortcut)))
    (browse-url url new-window)))

(setq browse-url-browser-function #'pjones:browse-url-browser-function)
