################################################################################
include ../mk/init.mk
include ../mk/files.mk
include ../mk/haskell.mk

################################################################################
BIN_FILES = pandoc hasktags
SRC_BIN_FILES = $(addprefix cabal-dev/bin/,$(BIN_FILES))
DST_BIN_FILES = $(addprefix $(HOME)/bin/,$(BIN_FILES))

################################################################################
$(eval $(call PMADE_INSTALL_FILE,dot.ghci,~/.ghci))

################################################################################
all: $(DST_BIN_FILES)

################################################################################
$(DST_BIN_FILES): $(SRC_BIN_FILES)
	cp -p $< $@

################################################################################
$(SRC_BIN_FILES): pjones.cabal
	cabal-dev install
