;; Muse Mode
(add-to-list 'load-path (concat pmade-site-lisp "/muse"))
(setq muse-colors-autogen-headings nil)
(setq muse-html-table-attributes " class=\"muse-table\" cellpadding=\"0\" cellspacing=\"0\"")

(require 'muse-mode)
(require 'muse-html)
(require 'muse-barecms)

;; Muse Projects
(setq muse-project-alist
      `(("pmade.com" 
         (,@(muse-project-alist-dirs pmade-com-src) :default "index" :set (muse-barecms-url "http://pmade.com"))
         ,@(muse-project-alist-styles pmade-com-src pmade-com-dst "barecms" :include "\\.muse$" :exclude "/snippets/"))))

(setq pmade-muse-html-header
      "<html>
       <head><title><lisp>(muse-publishing-directive \"title\")</lisp></title>
       <link href=\"<lisp>pmade-print-css</lisp>\" media=\"all\" rel=\"stylesheet\" type=\"text/css\"/></head>
       <body><lisp>muse-barecms-header</lisp>")

(setq muse-latex-header
  "\\documentclass{article}
\\usepackage{lmodern}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage[pdftex]{graphicx}

\\def\\museincludegraphics{%
  \\begingroup
  \\catcode`\\|=0
  \\catcode`\\\\=12
  \\catcode`\\#=12
  \\includegraphics[width=0.75\\textwidth]
}

\\begin{document}

\\title{<lisp>(muse-publish-escape-specials-in-string
  (muse-publishing-directive \"title\") 'document)</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\maketitle

<lisp>(and muse-publish-generate-contents
           (not muse-latex-permit-contents-tag)
           \"\\\\tableofcontents\n\\\\newpage\")</lisp>\n\n")
  
(muse-derive-style "printable-xhtml" "xhtml"
                   :header 'pmade-muse-html-header)
