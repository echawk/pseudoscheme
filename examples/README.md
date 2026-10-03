# Examples

| file | what it shows | run with |
|---|---|---|
| `scheme-uses-lisp.scm` | An R7RS program using `COMMON-LISP`: sequence functions, keyword arguments, predicates, multiple values, hash tables with `lisp-set!`, special variables with `lisp-let`, Lisp errors as Scheme conditions | `bin/pseudoscheme examples/scheme-uses-lisp.scm` |
| `scheme-uses-lisp-libraries.scm` | Alexandria and CL-PPCRE from Scheme, with the systems loaded automatically on import | `bin/pseudoscheme --quicklisp examples/scheme-uses-lisp-libraries.scm` |
| `lisp-uses-scheme.lisp` | Lisp using Scheme: SRFIs and a Scheme-defined library as Lisp packages, `r7rs:scheme`, `r7rs:procedure`, `expand`/`translate`, errors, and R6RS/R5RS | `sbcl --dynamic-space-size 4GB --control-stack-size 500MB --load examples/lisp-uses-scheme.lisp` |
| `mixed-system/` | An ASDF system of Scheme and Lisp: an R7RS library (which itself uses SRFI 1 and CL's `sort`), used by Lisp code as the package `STATS` | see below |

For the Lisp examples, ASDF has to find Pseudoscheme. Put the checkout
on the source registry, or push it onto `asdf:*central-registry*`.

```lisp
;; mixed-system
(push #p"/path/to/pseudoscheme-asdf/" asdf:*central-registry*)
(push #p"/path/to/pseudoscheme-asdf/examples/mixed-system/" asdf:*central-registry*)
(asdf:load-system :mixed-demo)
(mixed-demo:report '(2 4 4 4 5 5 7 9))
;; n = 8, mean = 5, median = 9/2, standard deviation = 2.000
```

`docs/interop.md` describes the bridge in full.
