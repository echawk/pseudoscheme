# Pseudoscheme.  The Lisp side is built with ASDF (pseudoscheme.asd);
# this is for what isn't.

SBCL ?= sbcl

.PHONY: bootstrap bootstrap-all bootstrap-check clean

# Regenerate the translator's .pso files (and closed.pso, read.pso,
# write.pso, spack.lisp) from their .scm sources, in some other Scheme:
# $(SCHEME) if set (e.g. make bootstrap SCHEME=guile), else the first
# one installed.  See boot/README.md.
bootstrap:
	boot/bootstrap.sh

# The same with every Scheme installed, checking they agree.
bootstrap-all:
	boot/bootstrap.sh --all

# Bootstrap without installing, and check the result against what
# Pseudoscheme's own translator (as loaded from src/) produces.  After
# a `make bootstrap`, this is a fixpoint test.
bootstrap-check:
	SBCL=$(SBCL) boot/bootstrap.sh --no-install --check

clean:
	rm -rf boot/build
