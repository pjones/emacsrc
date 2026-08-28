;;; define-word-conf.el -- Settings for `define-word' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'define-word)

(custom-set-variables
 '(define-word-services
   '((wordnik "https://wordnik.com/words/%s" define-word--parse-wordnik)
     (openthesaurus "https://www.openthesaurus.de/synonyme/%s" define-word--parse-openthesaurus)
     (webster "https://webstersdictionary1828.com/Dictionary/%s" define-word--parse-webster))))

;;; define-word-conf.el ends here
