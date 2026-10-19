# Build bin/quasischeme, the command-line Scheme.
#
#   make              build with SBCL
#   make LISP=ccl     ... or another Common Lisp: sbcl, ccl, ecl, clisp
#   make test         run the smoke tests in contrib/cli/test.sh
#
# psyntax's expander recurses deeply, so the image is built with a big
# control stack; on SBCL the saved executable keeps that setting (UIOP
# saves runtime options for executables).  The heap limit is generous
# (8GB, reserved, not allocated): SBCL's copying collector needs room
# beyond the live data, and allocation-heavy programs (the paraffins and
# mperm benchmarks) exhaust 2GB.
#
# SRFI 22's script interpreters, bin/scheme-r5rs and the rest, and SRFI
# 138's bin/compile-r7rs are links to bin/quasischeme, which looks at
# the name it's run as.

LISP ?= sbcl
ROOT := $(abspath ../..)
# SBCL's heap limit, kept by the saved executable: more for big loads
# (Guix's package modules: tests/guix/run.sh builds with HEAP=11GB)
HEAP ?= 8GB

ifeq ($(LISP),sbcl)
  RUN = sbcl --dynamic-space-size $(HEAP) --control-stack-size 256MB --non-interactive --no-userinit --load
else ifeq ($(LISP),ccl)
  RUN = ccl --no-init --batch --load
else ifeq ($(LISP),ecl)
  RUN = ecl --norc --load
else ifeq ($(LISP),clisp)
  RUN = clisp -norc
else
  $(error unknown LISP=$(LISP): use sbcl, ccl, ecl or clisp)
endif

SCRIPT_INTERPRETERS = scheme-r4rs scheme-r5rs scheme-ieee-1178-1990 scheme-srfi-0 scheme-r6rs scheme-r7rs scheme-script
# chez: Chez Scheme's own command line (cli.lisp, PARSE-CHEZ-ARGUMENTS)

all: $(ROOT)/bin/quasischeme

$(ROOT)/bin/quasischeme: FORCE
	mkdir -p $(ROOT)/bin
	rm -f $(ROOT)/bin/quasischeme.new  # else ASDF thinks it's up to date after a Makefile-only change
	$(RUN) build.lisp
	mv -f $(ROOT)/bin/quasischeme.new $(ROOT)/bin/quasischeme  # atomically, for programs running the old one
	for name in $(SCRIPT_INTERPRETERS) compile-r7rs chez; do ln -sf quasischeme $(ROOT)/bin/$$name; done

test: $(ROOT)/bin/quasischeme
	sh ./test.sh $(ROOT)/bin/quasischeme

FORCE:
.PHONY: all test FORCE
