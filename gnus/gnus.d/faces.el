;;; My Gnus Faces

;; Create the faces
(make-face 'my-gnus-summary-selected)
(make-face 'my-gnus-face-5)
(make-face 'my-gnus-face-6)
(make-face 'my-gnus-face-7)
(make-face 'my-gnus-face-8)
(make-face 'my-gnus-face-9)

;; Set their values
(custom-set-faces
 '(my-gnus-summary-selected ((t (:background "#444444"))))
 '(my-gnus-face-5 ((t (:foreground "#B150E7"))))
 '(my-gnus-face-6 ((t (:foreground "#A5F26E"))))
 '(my-gnus-face-7 ((t (:foreground "#D6B163"))))
 '(my-gnus-face-8 ((t (:foreground "#CC7832"))))
 '(my-gnus-face-9 ((t (:foreground "#888888" :family "Arial Unicode MS"))))
 '(gnus-header-name ((t (:foreground "#8CCC5F" :bold t))))
 '(gnus-header-from ((t (:foreground "#D6B163"))))
 '(gnus-header-subject ((t (:foreground "#CC7832" :height 1.3 :inherit variable-pitch)))))

;; Make Gnus use them
(setq
 gnus-summary-selected-face 'my-gnus-summary-selected
 gnus-face-5                'my-gnus-face-5
 gnus-face-6                'my-gnus-face-6
 gnus-face-7                'my-gnus-face-7
 gnus-face-8                'my-gnus-face-8
 gnus-face-9                'my-gnus-face-9)
