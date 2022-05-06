#!/usr/bin/env bash

set -eu
set -o pipefail

# Send all output to a file:
exec &> >(tee -a "$HOME/log") 2>&1
set -x

# Initial assertions:
test -e ~/.config/emacs/init.el
test -n "${XDG_RUNTIME_DIR}"

# Start an Emacs daemon so we can connect to it:
emacs --bg-daemon=test </dev/null
test -S "$XDG_RUNTIME_DIR/emacs/test"

# Verify that the `e' script can connect to the daemon:
test "$(e -ds test -- --eval nil)" = "nil"

# Create a customize file for the tests:
custom_file=$(e -ds test -- --eval custom-file)
mkdir -p "$(dirname "$custom_file")"

cat <<EOF >"$custom_file"
(custom-set-variables
 '(sh-basic-offset 100))
EOF

# Kill the daemon process:
e -ds test -- --eval '(kill-emacs)'

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
