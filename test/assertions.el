;;; assertions.el -- Tests for the emacsrc package.  -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'ert)
(require 'sh-script)

(defvar emacsrc-test-top nil
  "Path to the this test package.  Set by the test runner.")

(ert-deftest emacsrc-customize ()
  "Verify that the variable`custom-file' is not loaded by default."
  (should (not (equal sh-basic-offset 100))))

(ert-deftest enchant-configured-correctly ()
  "Enchant can see nuspell dictionaries."
  (dolist (dict '("en_US" "de_DE"))
    (should (= 0 (call-process "enchant-lsmod-2" nil t nil "-lang" dict)))))

(ert-deftest can-open-an-org-file ()
  "Ensure my `org-mode' configuration still works."
  (declare-function pjones:org-mode-hook "org-conf.el")
  (should (not (fboundp #'pjones:org-mode-hook)))
  (find-file (concat emacsrc-test-top "share/files/test.org"))
  (should (eq 'org-mode major-mode))
  (should (fboundp #'pjones:org-mode-hook)))

;; (ert-deftest "emacsrc-fail" ()
;;   "Always fail.  Useful for verifying this actually works."
;;   (should nil))

;;; assertions.el ends here
