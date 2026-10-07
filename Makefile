# Pseudoscheme.  The Lisp side is built with ASDF (pseudoscheme.asd);
# this is for what isn't: running the tests, and regenerating the
# checked-in generated files from source (see boot/README.md).

SBCL ?= sbcl

.PHONY: bootstrap bootstrap-all bootstrap-pso bootstrap-psyntax bootstrap-check clean \
	test test-escape test-cli test-srfi test-all precompile-srfi

# Tests.  `make test` runs every suite in the default mode (full
# continuations, src/continuations.lisp); `make test-escape` the
# standards' suites with escape-only continuations (--continuations=escape);
# `make test-cli` builds and tests the command
# line; `make test-srfi` builds it and runs the bundled SRFIs' tests
# (src/srfi/tests/, tests/run-srfi-tests.sh; SRFIS="1 13" for some);
# `make test-all` all four.  Each suite prints its tally, and the
# make stops at the first one that fails to finish.  The compiled-library
# cache is off, so a stale cache can't hide anything.
SCHEME_RUN = PSEUDOSCHEME_LIBRARY_CACHE=0 $(SBCL) --dynamic-space-size 4GB \
	--control-stack-size 500MB --script

SUITES = syntax-case library interop srfi-system r5rs r7rs r6rs continuation

test:
	@# one run first, so ASDF compiles whatever is stale once
	@$(SCHEME_RUN) tests/run-syntax-case-tests.lisp | tail -1
	@for t in $(filter-out syntax-case,$(SUITES)); do \
	  $(SCHEME_RUN) tests/run-$$t-tests.lisp > /tmp/pseudoscheme-test-$$t.log 2>&1; \
	  grep -hE "tests passed|of [0-9]+ tests" /tmp/pseudoscheme-test-$$t.log \
	    || { echo "$$t: no result; see /tmp/pseudoscheme-test-$$t.log"; exit 1; }; \
	done

test-escape:
	@for t in r5rs r7rs r6rs; do \
	  $(SCHEME_RUN) tests/run-$$t-tests.lisp --continuations=escape > /tmp/pseudoscheme-test-escape-$$t.log 2>&1; \
	  grep -hE "tests passed|of [0-9]+ tests" /tmp/pseudoscheme-test-escape-$$t.log \
	    | sed 's/^/escape-only continuations: /' \
	    || { echo "$$t: no result; see /tmp/pseudoscheme-test-escape-$$t.log"; exit 1; }; \
	done

test-cli:
	$(MAKE) -C contrib/cli test

test-srfi:
	@echo "Building bin/pseudoscheme (log: /tmp/pseudoscheme-cli-build.log) ..."
	@$(MAKE) -C contrib/cli > /tmp/pseudoscheme-cli-build.log 2>&1 \
	  || { tail -20 /tmp/pseudoscheme-cli-build.log; exit 1; }
	@sh tests/run-srfi-tests.sh bin/pseudoscheme $(SRFIS)

test-all: test test-escape test-cli test-srfi

# Expand and compile every bundled SRFI library into the library cache
# (~/.cache/pseudoscheme/libraries, or $$PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY),
# for both continuation modes, so that programs load them compiled from
# the start.  (bin/pseudoscheme --precompile-srfi does one mode.)
precompile-srfi:
	@echo "Building bin/pseudoscheme (log: /tmp/pseudoscheme-cli-build.log) ..."
	@$(MAKE) -C contrib/cli > /tmp/pseudoscheme-cli-build.log 2>&1 \
	  || { tail -20 /tmp/pseudoscheme-cli-build.log; exit 1; }
	@echo "Full continuations (the default):"
	@bin/pseudoscheme --precompile-srfi
	@echo "Escape-only continuations (--continuations=escape):"
	@bin/pseudoscheme --continuations=escape --precompile-srfi

# Everything, in order:
#  1. the .pso files (and spack.lisp), in some other Scheme: $(SCHEME)
#     if set (e.g. make bootstrap SCHEME=guile), else the first found;
#  2. then, with Pseudoscheme loaded from those, psyntax's image, from a
#     seed another Scheme builds from psyntax's sources: SEED=chez (the
#     default) with Chez Scheme, which has R6RS natively, or SEED=stage0
#     with any R5RS or R7RS-small Scheme through boot/stage0/
#     (STAGE0_HOST=chibi, the default, scheme48 or gauche).
SEED ?= chez

bootstrap: bootstrap-pso bootstrap-psyntax

# The same, with every Scheme installed for step 1, which must agree.
bootstrap-all:
	boot/bootstrap.sh --all
	SBCL=$(SBCL) boot/psyntax.sh --seed=$(SEED)

# src/*.pso and src/spack.lisp
bootstrap-pso:
	boot/bootstrap.sh

# vendor/psyntax/psyntax-pseudoscheme.pp
bootstrap-psyntax:
	SBCL=$(SBCL) boot/psyntax.sh --seed=$(SEED)

# Make everything without installing it, and check: the .pso files
# against what Pseudoscheme's own translator (loaded from src/) writes;
# the psyntax image, rebuilt from Chez's seed and from stage0's, each
# until it reproduces itself, against what's checked in.  The two seeds
# come from unrelated expanders, so their agreeing is evidence that
# neither put anything into the image that the sources don't say.
bootstrap-check:
	SBCL=$(SBCL) boot/bootstrap.sh --no-install --check
	SBCL=$(SBCL) boot/psyntax.sh --no-install --seed=chez
	cmp boot/build/psyntax/psyntax-pseudoscheme.pp vendor/psyntax/psyntax-pseudoscheme.pp
	SBCL=$(SBCL) boot/psyntax.sh --no-install --seed=stage0
	cmp boot/build/psyntax/psyntax-pseudoscheme.pp vendor/psyntax/psyntax-pseudoscheme.pp

clean:
	rm -rf boot/build
