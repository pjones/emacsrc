SRC	 ?= $(wildcard *.el)
COMPILED  = $(patsubst %.el,$(DEST)/%.elc,$(SRC))
PMADE_LOAD_PATH ?= $(CURDIR)/../emacs.d/pmade-loadpath

EMACS = emacs
EMACS_FLAGS = -q --no-site-file --batch
EMACS_FLAGS += --eval "(add-to-list 'load-path \".\")"
EMACS_FLAGS += -l $(PMADE_LOAD_PATH)
EMACS_FLAGS += -f batch-byte-compile

.PHONEY: all

all: $(DEST) $(COMPILED) $(addprefix $(DEST)/,$(TO_REMOVE))
	@:

uninstall:
	rm -f $(COMPILED)

.SUFFIXES: .el .elc

$(DEST)/%.elc: %.el
	@ echo emacs compile $<
	@ $(EMACS) $(EMACS_FLAGS) $<
	@ mv $(<:.el=.elc) $@

$(DEST):
	mkdir -p $@

$(addprefix $(DEST)/,$(TO_REMOVE))::
	@ rm -f $@
