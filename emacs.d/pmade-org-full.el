;; Specific settings for the org-mode files I use to run my business

;; The notes file and options
(setq
 org-directory          "~/Documents/pmade/planning/"
 org-default-notes-file (concat org-directory "clients/notes.org")
 org-reverse-note-order t               ; put new notes on top
 org-archive-location  "archive/%s::")

;; Agenda Files
(setq org-agenda-files
      (append
       (directory-files (concat (expand-file-name org-directory) "/clients") t "\\.org$")
       (directory-files (concat (expand-file-name org-directory) "/tasks") t "\\.org$")))

;; Exporting
(setq org-export-html-style (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" pmade-print-css "\"/>"))

;; Org and Remember Mode
(require 'remember)
(org-remember-insinuate)

(setq org-remember-templates
      (list
       (list "Client Tasks"   ?t "** TODO %?\n   %i" (concat org-directory "clients/pmade.org")  "Tasks")
       (list "General Tasks"  ?g "** TODO %?\n   %i" (concat org-directory "tasks/general.org")  "Tasks")
       (list "Devenv Tasks"   ?d "** TODO %?\n   %i" (concat org-directory "tasks/devenv.org")   "Tasks")))

