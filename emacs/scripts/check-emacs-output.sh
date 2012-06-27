#!/bin/sh

clean_output () {
  egrep -v '^(Wrote|Loading)' | \
    egrep -v '^[[:space:]]*$' | \
    egrep -v '^In end of data:' | \
    egrep -v 'org-bookmark-jump-unhide' | \
    egrep -v '^[[:space:]]*be defined at runtime.'
}

clean_output | wc -l
