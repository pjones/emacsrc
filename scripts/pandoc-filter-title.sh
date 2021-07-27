#!/usr/bin/env bash

# This script is a pandoc filter that reads a JSON document from
# standard input and produces an updated document on standard output.
#
# The updated document will have its metadata title field set to the
# first heading in the document.

set -eu
set -o pipefail

read -r -d '' query <<'EOF' || :
.meta.title = {
  "t": "MetaInlines",
  "c": [
    {
      "t": "Str",
      "c": .blocks | map(select(.t=="Header")) | .[0].c[2][0].c
    }
  ]
}
EOF

jq "$query"
