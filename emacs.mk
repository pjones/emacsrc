SRC	 ?= $(wildcard *.el)
COMPILED  = $(patsubst %.el,$(DEST)/%.elc,$(SRC))

EMACS = emacs
EMACS_FLAGS = -q --no-site-file --batch
EMACS_FLAGS += --eval "(add-to-list 'load-path \".\")"
EMACS_FLAGS += --eval "(load \"~/.emacs.d/pmade/pmade-loadpath\")"
EMACS_FLAGS += -f batch-byte-compile

all: $(DEST) $(COMPILED) $(addprefix $(DEST)/,$(TO_REMOVE)) 

uninstall:
	rm -f $(COMPILED)

.SUFFIXES: .el .elc

$(DEST)/%.elc: %.el
	@ echo emacs compile $<
	@ $(EMACS) $(EMACS_FLAGS) $< #2> /dev/null
	@ mv $(<:.el=.elc) $@

$(DEST):
	mkdir -p $@

$(addprefix $(DEST)/,$(TO_REMOVE))::
	@ rm -f $@
