;;; automode.el -- Set up the auto-mode-alist variable

;; CSS
(add-to-list 'auto-mode-alist '("\\.css\\.scss" . css-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js$"       . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.erb$" . js2-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.txt$"            . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$"             . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.password-store/" . markdown-mode))

;; Org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.rake$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$"      . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$"    . ruby-mode))

;; RHTML
(add-to-list 'auto-mode-alist '("\\.rhtml$"      . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; ZSH
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;; Files that trigger complication-mode
(add-to-list 'auto-mode-alist '("errors\\.out$" . compilation-mode))
