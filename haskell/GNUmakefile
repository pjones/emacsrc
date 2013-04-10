################################################################################
include ../mk/init.mk
include ../mk/files.mk
include ../mk/haskell.mk

################################################################################
BIN_FILES = pandoc hasktags
DST_BIN_FILES = $(addprefix $(HOME)/bin/,$(BIN_FILES))

################################################################################
all: $(DST_BIN_FILES)

################################################################################
$(eval $(call PMADE_INSTALL_FILE,dot.ghci,~/.ghci))

################################################################################
$(foreach f,$(BIN_FILES),\
  $(eval $(call PMADE_INSTALL_BIN,cabal-dev/bin/$(f),$(HOME)/bin/$(f))))

################################################################################
$(SRC_BIN_FILES): pjones.cabal
	cabal-dev install
