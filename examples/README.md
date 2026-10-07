# Examples

| file | what it shows | run with |
|---|---|---|
| `scheme-uses-lisp.scm` | An R7RS program using `COMMON-LISP`: sequence functions, keyword arguments, predicates, multiple values, hash tables with `lisp-set!`, special variables with `lisp-let`, Lisp macros (`loop`, `destructuring-bind`, `handler-case`), CLOS classes defined from Scheme, `(lisp ...)` forms, Lisp errors as Scheme conditions | `bin/pseudoscheme examples/scheme-uses-lisp.scm` |
| `scheme-uses-lisp-libraries.scm` | Alexandria and CL-PPCRE from Scheme, with the systems loaded automatically on import | `bin/pseudoscheme --quicklisp examples/scheme-uses-lisp-libraries.scm` |
| `scheme-uses-ironclad.scm` | Cryptography with Ironclad: digests, HMAC, PBKDF2, AES, Ed25519 signatures, on Scheme bytevectors | `bin/pseudoscheme --quicklisp examples/scheme-uses-ironclad.scm` |
| `lisp-uses-scheme.lisp` | Lisp using Scheme: `r7rs:import` with import sets, Scheme macros (`cut`, `and-let*`) as Lisp macros, `r7rs:define`, a library written in the Lisp file, `r7rs:scheme`, `expand`/`translate`, errors, and R6RS/R5RS | `sbcl --dynamic-space-size 4GB --control-stack-size 500MB --load examples/lisp-uses-scheme.lisp` |
| `mixed-system/` | An ASDF system of Scheme and Lisp: an R7RS library (which itself uses SRFI 1 and CL's `sort`), used by Lisp code as the package `STATS` | see below |

For the Lisp examples, ASDF has to find Pseudoscheme. Put the checkout
on the source registry, or push it onto `asdf:*central-registry*`.

```lisp
;; mixed-system
(push #p"/path/to/pseudoscheme-asdf/" asdf:*central-registry*)
(push #p"/path/to/pseudoscheme-asdf/examples/mixed-system/" asdf:*central-registry*)
(asdf:load-system :mixed-demo)
(mixed-demo:report '(2 4 4 4 5 5 7 9))
;; prints
;;   n = 8
;;   mean = 5
;;   median = 9/2
;;   standard deviation = 2.000
;; and returns the summary as an alist
```

`docs/interop.md` describes the bridge in full.
