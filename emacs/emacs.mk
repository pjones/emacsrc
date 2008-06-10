SRC	 ?= $(wildcard *.el)
COMPILED  = $(patsubst %.el,$(DEST)/%.elc,$(SRC))

EMACS = emacs
EMACS_FLAGS = -q --no-site-file --batch
EMACS_FLAGS += --eval "(add-to-list 'load-path \".\")"
EMACS_FLAGS += --eval "(add-to-list 'load-path \"~/.emacs.d/packages\")"
EMACS_FLAGS += --eval "(add-to-list 'load-path \"~/Local/share/emacs/site-lisp/muse\")"
EMACS_FLAGS += --eval "(add-to-list 'load-path \"~/Local/share/emacs/site-lisp/apel\")"
EMACS_FLAGS += --eval "(add-to-list 'load-path \"~/Local/share/emacs/site-lisp/emu\")"
EMACS_FLAGS += --eval "(add-to-list 'load-path \"~/Local/share/emacs/site-lisp/rails\")"
EMACS_FLAGS += -f batch-byte-compile

all: $(DEST) $(COMPILED)

uninstall:
	rm -f $(COMPILED)

.SUFFIXES: .el .elc

$(DEST)/%.elc: %.el
	@ echo emacs compile $<
	@ $(EMACS) $(EMACS_FLAGS) $< #2> /dev/null
	@ mv $(<:.el=.elc) $@

$(DEST):
	mkdir $@
