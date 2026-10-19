# The older Revised Reports

`bin/quasischeme` has a language mode for each report before R5RS
whose text is available: each is an environment with exactly that
report's names (plus what the report lists as optional, where that was
practical), reading case-folded, as the reports say identifiers are.

| flag | report | kind |
|---|---|---|
| `--r4rs` | R4RS, 1991 | a subset of R5RS |
| `--ieee` (`--ieee-1178`) | IEEE 1178-1990 | R4RS less a few procedures |
| `--r3rs` | R3RS, 1986 | R4RS less a few, `nil`, `t`, and the empty list is false |
| `--r2rs` | R2RS, "the Revised Revised Report", AI Memo 848, 1985 | `#!true`, `#!false`, `#!null`, `1+`, `named-lambda`, ... |
| `--rrs` | The Revised Report, AI Memo 452, 1978 | MacLISP's Scheme: `labels`, `aset'`, `fluidbind`, `catch`, `prog`, `macro` ... |

`scheme-r4rs` and `scheme-ieee-1178-1990` (SRFI 22's script
interpreters, links to `bin/quasischeme`) are `--r4rs` and `--ieee`.
Everything else works as it does in R5RS mode: `-e`, `-p`, files,
the REPL (`r4rs> `), `load` (which loads at the report's top level).
R0RS, AI Memo 349 (1975), is not done: it is MacLISP-dependent (see
the end).

Each report is a library, `(quasischeme reports r4rs)` and so on,
built by `src/reports/reports.lisp` from R5RS's bindings
(`(scheme r5rs)`), the helpers of `src/reports/reports.sls`, and the
report's name list; the Revised Report's is `src/reports/rrs.sls`.
The top level is an interaction library whose unbound names come from
that library, so a name a report doesn't have is unbound
(`(values 1 2)` in `--r4rs` is an error). The lists are from the
reports' texts (R4RS and R3RS from the HTML renderings of the texts,
R2RS and the Revised Report from the scans at dspace.mit.edu, AI
memos 848 and 452), checked against the SchemeXref pages the project
list points at; the SchemeXref lists leave out the `c[ad]*r`
compositions, which the reports have, and so they are here.
IEEE 1178's text is not freely available: its list is SchemeXref's,
which is R4RS without `with-input-from-file`, `with-output-to-file`,
`load`, `transcript-on` and `transcript-off`.

## What each has, and how it differs

**R4RS** is a subset of R5RS: no `dynamic-wind`, `values`,
`call-with-values`, `eval`, `scheme-report-environment`,
`null-environment`, `interaction-environment`, or `define-syntax`,
`let-syntax`, `letrec-syntax`, `syntax-rules`; no `=>` in `cond`.
`else` is kept. The empty list is true. `transcript-on` and
`transcript-off` (the report's optional procedures) are accepted and
record nothing. Syntax is R5RS's expander (psyntax), so, for example,
internal definitions anywhere in a body work, as they do in R5RS.

**IEEE 1178-1990**: R4RS without the procedures listed above.

**R3RS** differs from R4RS by what it lacks (`list?`, `string`,
`peek-char`; its numeric procedures here still take any number of
arguments), and has
`last-pair`, `nil` (`#f`) and `t` (`#t`). The empty list counts as
false: `if`, `cond`, `and`, `or`, `do` and `not` are replaced by
versions that treat `()` as false (`(if '() 1 2)` is 2); `case`,
`memq` and friends are as ever. `number->string` takes a format and
`string->number` an exactness and a radix (symbols `E`/`EXACT`,
`I`/`INEXACT`, `B`/`O`/`D`/`X` or the full words, in either order; a
bare number is a radix, as in R4RS), and `rationalize` takes one
argument too. The formats are not made: `(number->string n '(fix 2))`
is `(number->string n)`.

**R2RS** has Part II's catalog as it is. Beyond R3RS it has:
`#!true`, `#!false`, `#!null` (read as `#t`, `#f`, `()`; the reader
directive is global, but nothing else uses those names), `nil` (the
empty list), `t`, `1+`, `-1+`, `=?`, `<?`, `>?`, `<=?`, `>=?`,
`append!`, `last-pair`, `string-null?`, `substring-fill!`,
`substring-move-left!`, `substring-move-right!`, `object-hash`,
`object-unhash` (strongly held), `rec`, `named-lambda`, `sequence`,
curried `define` (`(define ((f a) b) ...)`) and, optional in the
report, `=>` in `cond` and named `let`. It lacks `delay`, `force`,
`boolean?`, `procedure?`, `list?`, `peek-char`, `numerator`,
`denominator`, as the report does. The empty list is false.

**The Revised Report (1978)** is a different language from the others:
a MacLISP-hosted Scheme with `NIL` as both the empty list and false.
Its names: `lambda`, `if`, `quote`, `labels`, `define` (all three
spellings), `aset'`, `fluidbind`, `fluid`, `fluidset'`, `static`,
`catch`, `block`, `let`, `do`, `iterate`, `test`, `cond` (with `=>`),
`or`, `and`, `amapcar`, `amaplist`, `amapc`, `prog` (with `go` and
`return`), `macro`, `schmac`, `procp`, `enclose`, `create!process`,
`start!process`, `stop!process`, `terminate`, `**process**`, and, for
"all the usual MacLISP SUBRs", the ones its examples use: `car cdr
cons caar cadr cdar cddr caddr cdddr cadddr atom null eq equal rplaca
rplacd list append reverse nreverse length memq assq mapcar mapc
maplist + - * // = < > <= >= abs minus 1+ -1+ zerop minusp plusp max
min sqrt +$ -$ *$ //$ gensym print terpri error read gcd not t nil`.
Notes:

- `ASET'` reads as `(aset 'x e)` (the reader quotes the next datum,
  as MacLISP's does), so `aset` is the form; `fluidset'` likewise.
  `(define name (arg ...) form ...)` and `(define (name arg ...) form
  ...)` are the report's alternative spellings. `(macro name pattern
  body)` takes a lambda list for `pattern` and a Scheme body that
  returns the new form (not hygienic); `schmac`'s body is MacLISP in
  the report, and Scheme here. A macro must be defined by an earlier
  top-level form than its uses.
- `fluidbind` keeps its bindings in an association list shadowed for
  the dynamic extent of the body (`dynamic-wind`). `catch`'s escape
  object is a continuation.
- `amapcar`, `amaplist` and `amapc` are procedures here (the report
  makes them syntax, to take any number of lists); they stop at the
  shortest list.
- Processes are coroutines: `start!process` runs the process until it
  stops, terminates or starts another, and the starter is runnable
  meanwhile; when the last terminates, control returns to the top
  level. There is no preemption and no synchronization.
- `procp` returns `#t` (not the arity). Predicates return `#t` and
  `#f`, not `T` and `NIL`; `nil` is `()`, and `()` and `#f` are both
  false in `if`, `cond`, `and`, `or`, `do`, `test` and `not`; what is
  unspecified (a failed one-armed `if`) is `()`.
- `enclose` closes the lambda expression in the report's top level plus
  the a-list's bindings.
- There is no `else`, `letrec`, `set!`, `let*`, `case`, vectors,
  characters or strings, as the report has none.

## Caveats

- Printing is R5RS's: `#t`, `#f`, `()`, not `#!true` or `NIL`.
- `#!null` becomes `()` in quoted data and `(quote ())` in code; one
  read at run time with `read` is a marker, not `()`.
- The numeric printing formats of R2RS and R3RS (`(fix 2)`, `(sci 5 2)`,
  `(rat)`, ...) are accepted and ignored. R2RS numbers are as R5RS's
  tower (exact rationals, flonums, no `#s`/`#l` precisions).
- `transcript-on` and `transcript-off` record nothing.
- The libraries are R5RS's underneath: an R4RS program that depends on
  an R4RS "error" case (it is permitted to do anything) may meet R5RS's
  behavior. Errors for unbound names are the ordinary unbound-variable
  error.
- Tests: `make test-reports` (`REPORTS="r4rs r2rs"` for some) runs
  `tests/reports/<report>.scm`: about a thousand checks, written for
  this project from the examples in the reports' texts (the Guile
  r4rs test is not used), each also checking that names the report
  lacks are unbound. `tests/run-reports-tests.lisp` explains the format.

## R0RS

AI Memo 349 (1975) was not read for this work. It is out of scope
because it describes Scheme as a MacLISP program: the Revised Report's
notes say `ASET` (an evaluated variable name) was replaced by `ASET'` and
`EVALUATE` removed, so a mode would be the Revised Report's with those
two and MacLISP's data and primitives, which would need a decision about
how much of MacLISP to provide.
