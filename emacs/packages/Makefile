DEST = ${HOME}/.emacs.d/packages
TO_REMOVE  = find-recursive.elc
TO_REMOVE += git.elc
include ../emacs.mk

all: $(DEST)/ri-emacs.rb

$(DEST)/ri-emacs.rb: ri-emacs.rb
	cp -p $< $@
