;;; assertions.el -- Tests for the emacsrc package.  -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'ert)
(require 'sh-script)

(ert-deftest emacsrc-customize ()
  "Verify that the variable`custom-file' is not loaded by default."
  (should (not (equal sh-basic-offset 100))))

(ert-deftest enchant-configured-correctly ()
  "Enchant can see nuspell dictionaries."
  (dolist (dict '("en_US" "de_DE"))
    (should (= 0 (call-process "enchant-lsmod-2" nil t nil "-lang" dict)))))

;; (ert-deftest "emacsrc-fail" ()
;;   "Always fail.  Useful for verifying this actually works."
;;   (should nil))

;;; assertions.el ends here
