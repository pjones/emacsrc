##############################################################################
SRC	 ?= $(wildcard *.el)
COMPILED  = $(patsubst %.el,$(DEST)/%.elc,$(SRC))
PMADE_LOAD_PATH ?= $(CURDIR)/../lisp/loadpath.el
IGNORE_EMACS_WARNINGS ?= YES

##############################################################################
EMACS = emacs
EMACS_FLAGS = -Q --quick --batch
EMACS_FLAGS += --eval "(add-to-list 'load-path \".\")"
EMACS_FLAGS += --eval "(setq max-lisp-eval-depth 1200)"
EMACS_FLAGS += -l $(PMADE_LOAD_PATH)
EMACS_FLAGS += -f package-initialize
EMACS_FLAGS += -f batch-byte-compile

##############################################################################
CHECK_EMACS_OUTPUT ?= $(dir $(PMADE_LOAD_PATH))/../scripts/check-emacs-output.sh
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
	@ mkdir -p $(HOME)/.emacs.d/share/emacs/site-lisp
	@ $(EMACS) $(EMACS_FLAGS) $< > $(EMACS_OUTPUT_FILE) 2>&1; exit
ifeq ($(IGNORE_EMACS_WARNINGS),)
	@ if [ `$(CHECK_EMACS_OUTPUT) < $(EMACS_OUTPUT_FILE)` -ne 0 ]; then \
	    cat $(EMACS_OUTPUT_FILE); exit 1; \
	  fi
else
	@ cat $(EMACS_OUTPUT_FILE)
endif
	@ rm $(EMACS_OUTPUT_FILE)
	@ mv $(<:.el=.elc) $@
	@ if [ "$(EMACS_INSTALL_PACKAGES)" = "$<" ]; then \
	    echo emacs packages install; \
	    $(EMACS) -Q --batch -l $(PMADE_LOAD_PATH) -l $< -f "pjones:install-packages"; \
	  fi

##############################################################################
$(DEST):
	mkdir -p $@

##############################################################################
$(addprefix $(DEST)/,$(TO_REMOVE))::
	@ rm -f $@
