DEST = ${HOME}/.emacs.d/share/emacs/site-lisp
IGNORE_EMACS_WARNINGS = YES
include ../emacs.mk

all: $(DEST)/ri-emacs.rb

$(DEST)/ri-emacs.rb: ri-emacs.rb
	cp -p $< $@
