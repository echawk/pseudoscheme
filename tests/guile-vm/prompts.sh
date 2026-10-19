#!/bin/sh
# Composable continuations through the VM (docs/guile-vm.md): prompts.scm
# on the installed Guile and here, the results compared.
# Usage: sh tests/guile-vm/prompts.sh
here=$(cd "$(dirname "$0")" && pwd)
out=${TMPDIR:-/tmp}/quasischeme-guile-vm-prompts
guile --no-auto-compile "$here/prompts.scm" > "$out.guile" 2>&1 || exit 1
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script "$here/eval.lisp" "$here/prompts.scm" \
  2>&1 | grep -v '^;\|^WARNING' > "$out.here"
if diff "$out.guile" "$out.here"; then
  echo "prompts: $(wc -l < "$out.guile" | tr -d ' ') cases as Guile's"
else
  echo "prompts: differs from Guile"; exit 1
fi
