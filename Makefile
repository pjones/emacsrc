LOADPATHEL ?= lisp/loadpath.el
PREFIX ?= out
USER_DIR ?= $(PREFIX)/emacs.d

################################################################################
.PHONEY: all install clean

################################################################################
all:

################################################################################
install: all

################################################################################
clean:
	@rm -rf $(PREFIX)

################################################################################
# $1: Source file
# $2: Install prefix
# $3: Mode
define INSTALL_FILE
install: $(2)/$(1)

$(2)/$(1): $(1)
	@echo "install $$@"
	@mkdir -p $(2)/$(dir $(1))
	@install --mode=$(3) $$< $$@
endef

################################################################################
# $1: Source file
define COMPILE_LISP
all: $(USER_DIR)/$(basename $(1)).elc
$(call INSTALL_FILE,$(1),$(USER_DIR),0444)
endef

################################################################################
%.elc: %.el
	@echo "compile" "$(shell sed -E 's|/nix/store/[^/]+/||' <<<"$<")"
	@emacs --quick --batch \
          --load "$(LOADPATHEL)" \
          --funcall batch-byte-compile \
          "$<"

################################################################################
$(foreach f,\
  $(shell find dot.emacs.el lisp modes -type f -name '*.el'),\
  $(eval $(call COMPILE_LISP,$(f))))

$(foreach f,\
  $(shell find bin -type f),\
  $(eval $(call INSTALL_FILE,$(f),$(PREFIX),0555)))

$(foreach f,\
  $(shell find scripts -type f),\
  $(eval $(call INSTALL_FILE,$(f),$(USER_DIR),0555)))

$(foreach f,\
  $(shell find snippets -type f),\
  $(eval $(call INSTALL_FILE,$(f),$(USER_DIR),0444)))

$(foreach f,\
  $(shell find share -type f),\
  $(eval $(call INSTALL_FILE,$(f),$(PREFIX),0444)))
