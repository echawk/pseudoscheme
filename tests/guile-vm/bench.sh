#!/bin/sh
# Guile's VM backends compared (docs/guile-vm.md): bench.scm compiled by
# the installed guile, then run with each backend.
# Usage: sh tests/guile-vm/bench.sh [backend ...]
here=$(cd "$(dirname "$0")" && pwd)
out=${TMPDIR:-/tmp}/pseudoscheme-guile-vm-bench.go
guile --no-auto-compile -c "(compile-file \"$here/bench.scm\" #:output-file \"$out\")" >/dev/null || exit 1
for backend in ${@:-interpret jit aot vop}; do
  PSEUDOSCHEME_GUILE_VM_BACKEND=$backend sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
    --script "$here/bench.lisp" "$out" 2>&1 | grep -v '^;'
done
run_guile () {
  guile --no-auto-compile -c "(use-modules (ice-9 format)) (for-each (lambda (c) (let loop ((i 0) (best #f)) (if (< i 3) (let ((t0 (get-internal-real-time))) (apply (cadr c) (cddr c)) (let ((ms (exact->inexact (/ (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))) (loop (+ i 1) (if best (min best ms) ms)))) (format #t \"  ~18a ~8,1f ms~%\" (car c) best)))) (load-compiled \"$out\"))"
}
echo "guile, its VM:"
GUILE_JIT_THRESHOLD=-1 run_guile
echo "guile, its JIT:"
run_guile
