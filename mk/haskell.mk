################################################################################
# Make sure that cabal-dev is installed.
all: $(HOME)/.cabal/bin/cabal-dev

################################################################################
$(HOME)/.cabal/bin/cabal-dev:
	cabal install cabal-dev
