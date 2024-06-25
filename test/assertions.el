;;; assertions.el -- Tests for the emacsrc package.
;;
;;; Commentary:
;;
;;; Code:
(require 'ert)
(require 'sh-script)

(ert-deftest emacsrc-customize ()
  "Verify that the `custom-file' is not loaded by default."
  (should (not (equal sh-basic-offset 100))))

(ert-deftest enchant-configured-correctly ()
  "Enchant can see nuspell dictionaries."
  (should (= 0 (call-process "enchant-lsmod-2" nil t nil "-lang" "en_US"))))

;;; assertions.el ends here
