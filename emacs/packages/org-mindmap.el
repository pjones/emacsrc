;;; org-mindmap.el --- Turn OrgMode Headings Into Graphviz (DOT) Mindmaps
;;
;; Copyright (C) 2009-2010 pmade inc. (Peter Jones pjones@pmade.com)
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; Commentary:
;;
;; M-x org-export-as-mindmap
;;
;; Generates a Graphviz DOT file starting with the current node and
;; working downwards.
;;
;; You can control the style of the node (e.g. shape, color, etc.)
;; using the org-mindmap-node-styles customization or by setting the
;; DOT_NODE_STYLE property on a heading.
;;
;; For now, you must manually convert the DOT file to something more
;; useful.  On my system there are three ways to do that:
;;
;;  1. Use Graphviz: twopi -Tpng generated.dot > generated.png
;;
;;  2. Use dot2tex: http://www.fauskes.net/code/dot2tex/
;;
;;  3. Open the file in OmniGraffle: 
;;     http://www.omnigroup.com/applications/omnigraffle/
(eval-when-compile
  (require 'org))

(defgroup org-mindmap nil
  "OrgMode Mindmap Generator"
  :tag "Org-Mindmap" :group 'org)

(defcustom org-mindmap-header
  "digraph mindmap {\n  ranksep=3;\n  ratio=auto;\n"
  "Text inserted at the beginning of the DOT file"
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-footer
  "}\n"
  "Text inserted at the end of the DOT file"
  :type 'string :group 'org-mindmap)

(defcustom org-mindmap-node-styles
  '("shape=\"polygon\",style=\"filled\",color=\"green\""
    "shape=\"ellipse\",style=\"filled\",color=\"grey\""
    "shape=\"plaintext\"")
  "A list of styles to place on the DOT nodes as they are
generated.  The first style is placed on the root node, the
second style is placed on first level of children and so forth
and so on.  The last style is used for all nodes that don't have
a style.

You can also set a node style by using the DOT_NODE_STYLE
property on a heading.  Children will inherit their parent
heading's node style.  This allows you to style each sub-tree
using the same style, which sometimes looks very nice."
  :type 'string :group 'org-mindmap)

(defvar org-mindmap-export-buffer nil
  "The buffer that is receiving export information")

(defvar org-mindmap-start-level 0
  "The heading level of the root node")

(defun org-mindmap-walk-tree (&optional parent)
  (let* ((title (org-no-properties (org-get-heading t)))
         (level (org-outline-level))
         (id    (or (org-entry-get nil "ID") (org-id-new nil)))
         (style (or (org-entry-get nil "DOT_NODE_STYLE" t)
                    (nth (- level org-mindmap-start-level) org-mindmap-node-styles)
                    (car (last org-mindmap-node-styles)))))
    (with-current-buffer org-mindmap-export-buffer
      (insert (concat "  \"" id "\" [label=\"" title "\""))
      (if style (insert (concat "," style)))
      (insert "];\n")
      (if parent (insert (concat "  \"" parent "\" -> \"" id "\";\n"))))
    (while (and (outline-next-heading) (= (org-outline-level) (1+ level)))
      (org-mindmap-walk-tree id))
    (when (and (org-at-heading-p) (< (org-outline-level) (1+ level)))
      (outline-previous-heading))))

(defun org-export-as-mindmap ()
  "Export the current heading (as a sub-tree) as a DOT file"
  (interactive)
  (let ((file-name (or buffer-file-name "mindmap"))
        (org-mindmap-start-level (org-outline-level)))
    (setq file-name
          (if (string-match "\\..*$" file-name)
              (replace-match ".dot" nil nil file-name)
            (concat file-name ".dot")))
    (setq org-mindmap-export-buffer 
          (generate-new-buffer (generate-new-buffer-name "*org-mindmap*")))
    (with-current-buffer org-mindmap-export-buffer
      (insert org-mindmap-header))
    (save-excursion
      (org-back-to-heading)
      (save-restriction
        (org-narrow-to-subtree)
        (org-mindmap-walk-tree)))
    (with-current-buffer org-mindmap-export-buffer
      (insert org-mindmap-footer)
      (write-file file-name t)
      (switch-to-buffer (current-buffer)))))

(provide 'org-mindmap)
