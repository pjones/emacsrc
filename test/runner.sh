#!/usr/bin/env bash

set -eu
set -o pipefail

# Send all output to a file:
exec &> >(tee -a "$HOME/log") 2>&1
set -x

# Initial assertions:
test -e ~/.config/emacs/init.el
test -n "${XDG_RUNTIME_DIR}"
test -S "$XDG_RUNTIME_DIR/emacs/server"

# Verify that the `e' script can connect to the daemon:
test "$(e -d -- --eval nil)" = "nil"

# Create a customize file for the tests:
custom_file=$(e -d -- --eval custom-file)
mkdir -p "$(dirname "$custom_file")"

cat <<EOF >"$custom_file"
(custom-set-variables
 '(sh-basic-offset 100))
EOF

# Run the tests:
tests=$(realpath "$(dirname "$0")/../share/assertions.el")

emacs \
  --batch \
  --funcall package-initialize \
  --load ~/.config/emacs/init.el \
  --load ert \
  --load "$tests" \
  --funcall ert-run-tests-batch-and-exit

# Done.
touch "$HOME/PASSED"
