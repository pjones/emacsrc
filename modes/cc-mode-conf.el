;;; cc-mode-conf.el -- Settings for `cc-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'cc-mode)
(require 'eglot)
(require 'reformatter)

(declare-function electric-pair-default-inhibit "elec-pair")
(declare-function indent-bars-mode "indent-bars")
(defvar electric-pair-pairs)

(reformatter-define cc-format
  :program "clang-format"
  :group 'cc-mode)

(defun pjones:cc-electric-pair-inhibit-predicate (char)
  "Return non-nil to prevent insertion of a matching delimiter.
CHAR is the character just inserted."
  (pcase char
    ;; Hack to sometimes use < as an electric delimiter in C++.  To
    ;; make this work you also need a few other things:
    ;;
    ;;  - The `pjones:cc-electric-pair-syntax-info' and
    ;;  `pjones:cc-mode-angle-brackets' functions from below.
    ;;
    ;;  - Run the `pjones:cc-mode-angle-brackets' function from within
    ;;  your `c++-mode-hook' hook function.
    (?< (let* ((end (point))
               (beg (save-excursion
                      (move-beginning-of-line 1)
                      (point)))
               (str (buffer-substring beg end)))
          (and (not (string-match-p (rx bol "#include <" eol) str))
               (not (string-match-p (rx  "template <" eol) str))
               (string-match-p (rx (or (= 2 ?<)
                                       (seq (syntax open-parenthesis) ?<)
                                       (seq blank ?<))
                                   eol)
                               str))))
    (_ (electric-pair-default-inhibit char))))

(defun pjones:cc-electric-pair-syntax-info (orig &rest args)
  "Fix `electric-pair-syntax-info' handling of angle brackets.
ARGS are passed on to the original function ORIG."
  (let ((result (apply orig args)))
    (if (and (equal (nth 0 result) ?\()
             (equal (nth 1 result) ?>))
        (list (nth 0 result)
              (nth 1 result)
              nil ; Force running the inhibit hook.
              (nth 3 result))
      result)))

(defun pjones:cc-mode-angle-brackets ()
  "Get \"<\" and \">\" working with electric pair mode in C++."
  (let ((pairs (append '((?< . ?>)) electric-pair-pairs)))
    (setq-local electric-pair-pairs pairs
                electric-pair-inhibit-predicate
                #'pjones:cc-electric-pair-inhibit-predicate))
  (advice-add 'electric-pair-syntax-info
              :around #'pjones:cc-electric-pair-syntax-info))

(custom-set-variables
 '(c-default-style "bsd")
 '(c-ts-mode-indent-style 'bsd))

(let ((offset 2))
  (setopt c-basic-offset offset
          c-basic-indent offset
          c-ts-mode-indent-offset offset))

(defun pjones:c-mode-hook ()
  "Set up C-like modes."
  (cc-format-on-save-mode)
  (eglot-ensure))

(dolist (hook '(c-mode-hook
                c-ts-mode-hook
                c++-mode-hook
                c++-ts-mode-hook
                objc-mode-hook))
  (add-hook hook #'pjones:c-mode-hook)
  (add-hook hook #'pjones:cc-mode-angle-brackets))

;;; cc-mode-conf.el ends here
