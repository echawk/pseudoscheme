# Our libguile-3.0.so.1 (docs/libguile.md): the C half of the bridge to
# Guile mode.  Needs the Boehm collector (libgc) and an SBCL to run the
# generator.  `make` builds it in BUILD (default: ../../build/libguile).
#
#   make                   api.c and ids.h from the table, then the library
#   make extract GUILE_SRC=DIR   scheme.lisp again, from Guile's manual

BUILD ?= ../../build/libguile
SBCL ?= sbcl
CC ?= cc
CFLAGS ?= -O2 -g
LIB = $(BUILD)/libguile-3.0.so.1

all: $(LIB)

$(BUILD)/api.c: table.lisp scheme.lisp gen.lisp exports-3.0.11.txt
	mkdir -p $(BUILD)
	$(SBCL) --script gen.lisp $(BUILD)

$(LIB): $(BUILD)/api.c core.c data.c psg.h libguile.map
	$(CC) $(CFLAGS) -fPIC -shared -I. -I$(BUILD) -o $@ core.c data.c $(BUILD)/api.c \
	  -Wl,-soname,libguile-3.0.so.1 -Wl,--version-script=libguile.map -lgc -lpthread
	ln -sf libguile-3.0.so.1 $(BUILD)/libguile-3.0.so

extract:
	$(SBCL) --script extract-manual.lisp $(GUILE_SRC)/doc/ref exports-3.0.11.txt > scheme.lisp

clean:
	rm -rf $(BUILD)

.PHONY: all extract clean
