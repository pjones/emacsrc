;;; indent-bars-conf.el -- Settings for `indent-bars' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'indent-bars)

(custom-set-variables
 '(indent-bars-treesit-support t)
 '(indent-bars-treesit-ignore-blank-lines-types '("module"))
 '(indent-bars-treesit-wrap
   '((c argument_list
        init_declarator
        parameter_list
        parenthesized_expression)
     (cpp argument_list
          binary_expression
          init_declarator
          parameter_list
          parenthesized_expression)
     (python argument_list
	     dictionary
	     list
	     parenthesized_expression
             dictionary_comprehension
             list_comprehension
             parameters
             subscript))))

;;; indent-bars-conf.el ends here
