;;; devalot-light-theme.el --- Solarized-based custom theme for faces.
;;
;; Copyright (C) 2007-2015 Peter Jones <pjones@pmade.com>
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
;;; Commentary:
;;
;; The majority of the colors in this theme come from the Solarized
;; palette, which is released under a MIT license:
;; https://github.com/altercation/solarized.
;;
;;; Code:
(require 'devalot-theme)
(deftheme devalot-light "A theme loosely based on solarized.")

;; Create the faces if they don't already exist.
(put 'devalot-light 'theme-immediate t)

;;; Notes
;; * To see all faces in effect: list-faces-display
;; * To see all colors: list-colors-display
(defvar devalot-colors-light
  '((yellow       "#b58900")
    (yellow-high  "#b5a924")
    (orange       "#cb4b16")
    (orange-high  "#cb683f")
    (red          "#dc322f")
    (red-high     "#dc5c5a")
    (magenta      "#d33682")
    (magenta-high "#d36198")
    (violet       "#6c71c4")
    (violet-high  "#9396c4")
    (blue         "#268bd2")
    (blue-high    "#509cd2")
    (cyan         "#2aa198")
    (cyan-high    "#4aa19a")
    (green        "#859900")
    (green-high   "#88b324")
    (bg-normal    "#fdf6e3")
    (bg-off       "#073642")
    (bg-high      "#fff8e5")
    (bg-low       "#eee8d5")
    (bg-inverse   "#222222")
    (fg-normal    "#657b83")
    (fg-low       "#586e75")
    (fg-high      "#93a1a1")
    (fg-inverse   "#222222")
    (class        ((class color) (min-colors 89))))
  "Colors for the light version of the Devalot theme.")

(let ((colors (devalot-colors-apply 'devalot-light devalot-colors-light)))
  (apply 'custom-theme-set-faces 'devalot-light colors))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'devalot-light)
;;; -*- coding: utf-8; lexical-binding:t -*-
;;; devalot-light-theme.el ends here
