#!/bin/sh

# This script is needed when some stupid software (Firefox) will open
# execute commands without arguments.  It invokes emacsclient with
# flags to open a new frame, and wait for the file to be done being
# edited.
exec e -c -w -- "$@"
