# Build bin/pseudoscheme, the command-line Scheme.
#
#   make              build with SBCL
#   make LISP=ccl     ... or another Common Lisp: sbcl, ccl, ecl, clisp
#   make test         run the smoke tests in contrib/cli/test.sh
#
# psyntax's expander recurses deeply, so the image is built with a big
# control stack; on SBCL the saved executable keeps that setting (UIOP
# saves runtime options for executables).

LISP ?= sbcl
ROOT := $(abspath ../..)

ifeq ($(LISP),sbcl)
  RUN = sbcl --dynamic-space-size 2GB --control-stack-size 256MB --non-interactive --no-userinit --load
else ifeq ($(LISP),ccl)
  RUN = ccl --no-init --batch --load
else ifeq ($(LISP),ecl)
  RUN = ecl --norc --load
else ifeq ($(LISP),clisp)
  RUN = clisp -norc
else
  $(error unknown LISP=$(LISP): use sbcl, ccl, ecl or clisp)
endif

all: $(ROOT)/bin/pseudoscheme

$(ROOT)/bin/pseudoscheme: FORCE
	mkdir -p $(ROOT)/bin
	$(RUN) build.lisp

test: $(ROOT)/bin/pseudoscheme
	sh ./test.sh $(ROOT)/bin/pseudoscheme

FORCE:
.PHONY: all test FORCE
