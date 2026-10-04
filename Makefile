# Pseudoscheme.  The Lisp side is built with ASDF (pseudoscheme.asd);
# this is for what isn't: regenerating the checked-in generated files
# from source.  See boot/README.md.

SBCL ?= sbcl

.PHONY: bootstrap bootstrap-all bootstrap-pso bootstrap-psyntax bootstrap-check clean

# Everything, in order:
#  1. the .pso files (and spack.lisp), in some other Scheme: $(SCHEME)
#     if set (e.g. make bootstrap SCHEME=guile), else the first found;
#  2. then, with Pseudoscheme loaded from those, psyntax's image, seeded
#     by an image Chez Scheme builds from psyntax's sources.
bootstrap: bootstrap-pso bootstrap-psyntax

# The same, with every Scheme installed for step 1, which must agree.
bootstrap-all:
	boot/bootstrap.sh --all
	SBCL=$(SBCL) boot/psyntax.sh

# src/*.pso and src/spack.lisp
bootstrap-pso:
	boot/bootstrap.sh

# vendor/psyntax/psyntax-pseudoscheme.pp
bootstrap-psyntax:
	SBCL=$(SBCL) boot/psyntax.sh

# Make everything without installing it, and check: the .pso files
# against what Pseudoscheme's own translator (loaded from src/) writes,
# the psyntax image by rebuilding it until it reproduces itself.
bootstrap-check:
	SBCL=$(SBCL) boot/bootstrap.sh --no-install --check
	SBCL=$(SBCL) boot/psyntax.sh --no-install
	cmp boot/build/psyntax/psyntax-pseudoscheme.pp vendor/psyntax/psyntax-pseudoscheme.pp

clean:
	rm -rf boot/build
