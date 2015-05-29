#!/bin/sh -e

################################################################################
# This is a simple wrapper around cabal to build Haskell projects.
echo $NIX_GHC

################################################################################
# Keep cabal from being affected by local configuration.
export HOME="$(mktemp -d)"

################################################################################
# Some flags that can be overridden.
CABAL_FLAGS=""
TEST_DIR=test
SRC_DIR=src
VENDOR_DIR=vendor

if [ -d "$TEST_DIR" ]; then
  CABAL_FLAGS="$CABAL_FLAGS --enable-tests"
fi

################################################################################
# Create a sandbox just in case cabal needs to install something.
if [ ! -d .cabal-sandbox ]; then
  cabal sandbox init

  # Register any local packages.
  for dir in `ls "$VENDOR_DIR"`; do
    if [ -r $VENDOR_DIR/$dir/$dir.cabal ]; then
      cabal sandbox add-source $VENDOR_DIR/$dir
    fi
  done

  cabal install --only-dependencies $CABAL_FLAGS
fi

################################################################################
# Build the project.
cabal configure -fmaintainer $CABAL_FLAGS
cabal build

################################################################################
# Optionally run the tests.
[ -d "$TEST_DIR" ] && cabal test

################################################################################
# Scan the source code for suggestions.
hlint lint src
[ -d "$TEST_DIR" ] && hlint lint $TEST_DIR
