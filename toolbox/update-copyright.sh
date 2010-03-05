#!/bin/sh
################################################################################
#
# Update copyright years for all files recursively.
#
# Example:
#
# If you have files that look like this:
#
#   Copyright (C) 1969-1970 Foo Bar
#
# Then:
#
#   update-copyright.sh Foo
#
# Will edit your file (assume it's 2010):
#
#   Copyright (C) 1969-2010 Foo Bar
#
################################################################################

# Refuse to run unless we're under revision control
if [ \( ! -d .git \) -a \( ! -d .svn \) ]; then
  echo "ERROR: not under revision control!"
  exit 1;
fi

if [ $# -ne 1 ]; then
  echo "Usage: update-copyright.sh regex"
  echo "Regex should match your name/company after the year"
  exit 1
fi

suffix=$1
year=`date +%Y`

find . \
  \( -name .svn -type d -prune \) -o \
  \( -name .git -type d -prune \) -o \
  -type f -print0 | xargs -0 \
  sed -E -i '' \
  -e "s/(Copyright[[:space:]]+\([cC]\)[[:space:]][0-9]+))[[:space:]]$suffix/\1-$year $suffix/" \
  -e "s/(Copyright[[:space:]]+\([cC]\)[[:space:]][0-9]+)-[0-9]+[[:space:]]$suffix/\1-$year $suffix/"
