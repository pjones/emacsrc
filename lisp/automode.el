;;; automode.el -- Set up the auto-mode-alist variable
;;
;;; Commentary:
;;
;;; Code:

;; ePub
;; https://github.com/wasamasa/nov.el
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.css\\.scss" . css-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js$"       . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$"      . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.erb$" . js2-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.password-store/.*\\.gpg$" . markdown-mode))

;; Mermaid: https://mermaidjs.github.io/
(add-to-list 'auto-mode-alist '("\\.mermaid$" . mermaid-mode))

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

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; ZSH
(add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))

;; Images
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; Files that trigger complication-mode
(add-to-list 'auto-mode-alist '("errors\\.out$" . compilation-mode))

;;; automode.el ends here
