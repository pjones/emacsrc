##############################################################################
SRC	 ?= $(wildcard *.el)
COMPILED  = $(patsubst %.el,$(DEST)/%.elc,$(SRC))
PMADE_LOAD_PATH ?= $(CURDIR)/../lisp/loadpath.el

##############################################################################
EMACS = emacs
EMACS_FLAGS = -q --no-site-file --batch
EMACS_FLAGS += --eval "(add-to-list 'load-path \".\")"
EMACS_FLAGS += -l $(PMADE_LOAD_PATH)
EMACS_FLAGS += -f batch-byte-compile

##############################################################################
EMACS_OUTPUT_FILE = emacs.out

##############################################################################
.PHONEY: all

##############################################################################
all: $(DEST) $(COMPILED) $(addprefix $(DEST)/,$(TO_REMOVE))
	@:

##############################################################################
uninstall:
	rm -f $(COMPILED)

##############################################################################
.SUFFIXES: .el .elc

##############################################################################
$(DEST)/%.elc: %.el
	@ echo emacs compile $<
	@ $(EMACS) $(EMACS_FLAGS) $< > $(EMACS_OUTPUT_FILE) 2>&1; exit
ifeq ($(IGNORE_EMACS_WARNINGS),)
	@ if [ `egrep -v '^(Wrote|Loading)' $(EMACS_OUTPUT_FILE)|wc -l` -ne 0 ]; then \
	    cat $(EMACS_OUTPUT_FILE); exit 1; \
	  fi
endif
	@ rm $(EMACS_OUTPUT_FILE)
	@ mv $(<:.el=.elc) $@

##############################################################################
$(DEST):
	mkdir -p $@

##############################################################################
$(addprefix $(DEST)/,$(TO_REMOVE))::
	@ rm -f $@
