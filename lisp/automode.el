;;; automode.el -- Set up the auto-mode-alist variable -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(declare-function pjones:sqlite-mode-open-file "../modes/sqlite-mode-conf")

(defun pjones:sqlite-mode-magic ()
  "File handler for SQLite files."
  (require 'sqlite-mode)
  (pjones:sqlite-mode-open-file))

;; ePub
;; https://github.com/wasamasa/nov.el
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.css\\.scss" . css-mode))

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js$"       . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$"      . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\.erb$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jq$"       . jq-mode))

;; Lua
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-ts-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

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

;; SQLite
(add-to-list 'magic-mode-alist
             '("SQLite format 3\x00" . pjones:sqlite-mode-magic))

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
