#!/bin/sh

################################################################################
# Number of idle seconds before the display is turned off.
OFF_TIMEOUT_SECS=300

################################################################################
# Change display sleep settings.
xset dpms $OFF_TIMEOUT_SECS $OFF_TIMEOUT_SECS $OFF_TIMEOUT_SECS
