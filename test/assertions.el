;;; assertions.el -- Tests for the emacsrc package.
;;
;;; Commentary:
;;
;;; Code:
(require 'ert)
(require 'sh-script)

(ert-deftest emacsrc-customize ()
  "Verify that the custom-file is not loaded by default."
  (should (not (equal sh-basic-offset 100))))

;;; assertions.el ends here
