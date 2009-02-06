;; Muse Mode
(require 'muse-html)
(require 'muse-latex)
(require 'muse-barecms)

;; Muse Projects
(setq pmade:test-src-path "~/Develop/pmade/barecms/test/muse"
      pmade:test-dst-path "~/Develop/tmp/barecms-test"
      pmade:sc-src-path   "~/Develop/pmade/sc-trunk/doc"
      pmade:sc-dst-path   "~/Develop/tmp/sc-docs")

(setq muse-project-alist
      `(("pmade.com"
         (,@(muse-project-alist-dirs pmade-com-src) :default "index" :set (muse-barecms-url "http://pmade.com"))
         ,@(muse-project-alist-styles pmade-com-src pmade-com-dst "barecms" :include "\\.muse$" :exclude "/snippets/"))
        ("muse-barecms-test"
         (,@(muse-project-alist-dirs pmade:test-src-path) :default "index" :set (muse-barecms-url "http://localhost:3000" muse-barecms-base-path "/test/"))
         ,@(muse-project-alist-styles pmade:test-src-path pmade:test-dst-path "barecms" :include "\\.muse$"))
        ("labs/sva"
         (,@(muse-project-alist-dirs pmade:sc-src-path) :default "index" :set (muse-barecms-url "http://pmade.com" muse-barecms-base-path "/labs/sva/"))
         ,@(muse-project-alist-styles pmade:sc-src-path pmade:sc-dst-path "barecms" :include "\\.muse$"))))

(setq pmade-muse-html-header
      "<html>
       <head><title><lisp>(muse-publishing-directive \"title\")</lisp></title>
       <link href=\"<lisp>pmade-print-css</lisp>\" media=\"all\" rel=\"stylesheet\" type=\"text/css\"/></head>
       <body><lisp>muse-barecms-header</lisp>")

(setq 
 muse-latex-header "~/Develop/pmade/rc/latex/muse.tex"
 muse-completing-read-function 'ido-completing-read
 muse-colors-autogen-headings nil
 muse-html-table-attributes " class=\"muse-table\" cellpadding=\"0\" cellspacing=\"0\"")

(muse-derive-style "printable-xhtml" "xhtml"
                   :header 'pmade-muse-html-header)
