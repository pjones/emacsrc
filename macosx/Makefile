BINDINGS_FILE = $(HOME)/Library/KeyBindings/DefaultKeyBinding.dict

.PHONEY: all

all: $(BINDINGS_FILE)
	@ $(MAKE) -C scripts/iTunes all

$(BINDINGS_FILE): bindings.dict
	@ mkdir -p `dirname $@`
	install -m 644 -o $(USER) $< $@
