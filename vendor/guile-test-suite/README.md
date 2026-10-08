# Guile's test suite

`test-suite/` (the `(test-suite lib)` module) and `tests/` are GNU Guile
3.0.11's test suite, from `guile-3.0.11.tar.xz`'s `test-suite/`
directory, unmodified. They are Guile's: copyright the Free Software
Foundation and others, under the GNU Lesser General Public License,
version 3 or later (`COPYING.LESSER`; some files say GPL, `COPYING`).
`README.guile` is the suite's own README.

They measure the Guile mode (src/guile/, docs/guile.md):
`make test-guile` runs each `tests/*.test` file in its own process
(tests/run-guile-tests.sh, tests/run-guile-test.lisp) and tallies
Guile's own result kinds (pass, fail, xfail, unresolved, ...).
