;;; -*- Mode: Lisp -*-

;;;; Quasischeme
;;;; System Definitions

(cl:defpackage :quasischeme-system
  (:use :common-lisp :asdf))

(cl:in-package :quasischeme-system)

(defclass pso-file (cl-source-file) ())

(defmethod source-file-type ((component pso-file) system)
  (declare (ignore component system))
  "qso")

;;; psyntax's expanded image (vendor/psyntax/psyntax-quasischeme.pp),
;;; compiled: the compile-op translates it to Lisp in a psyntax host and
;;; compiles that (QSX::COMPILE-IMAGE); the load-op only records the
;;; fasl, which QSX::BOOT loads into each host it creates.
(defclass psyntax-image (source-file)
  ((type :initform "pp")))

(defmethod output-files ((o compile-op) (c psyntax-image))
  (list (make-pathname :type (uiop:compile-file-type) :defaults (component-pathname c))))

(defmethod perform ((o compile-op) (c psyntax-image))
  (uiop:symbol-call "QUASISCHEME-PSYNTAX" "COMPILE-IMAGE"
		    (component-pathname c) (output-file o c)))

(defmethod perform ((o load-op) (c psyntax-image))
  (setf (symbol-value (uiop:find-symbol* "*IMAGE-FASL*" "QUASISCHEME-PSYNTAX"))
	(first (input-files o c))))

;;; Quasischeme is split into a core run-time (rts), the self-hosted
;;; Scheme-to-Common-Lisp translator (translator), and an evaluator/REPL
;;; layer built on both.  Language-standard surfaces (r5rs, r6rs, r7rs)
;;; are secondary systems built on top of this shared core; see
;;; :quasischeme/r5rs below.

(defsystem :quasischeme/rts
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:float-features		; IEEE infinities, NaNs and traps
	       :trivial-garbage)		; the reader's weak table of ports
  :components
    (
     (:file "pack")
     (:file "spack" :depends-on ("pack"))
     (:file "core" :depends-on ("pack"))
     (:file "numbers" :depends-on ("core"))
     (pso-file "closed" :depends-on ("spack" "core" "numbers"))
     (:file "rts" :depends-on ("pack" "spack" "core" "numbers"))
     (:file "readwrite" :depends-on ("pack" "core"))
     (:file "quasi" :depends-on ("pack" "core"))	; SRFIs 107, 108, 109
     #+(or) (pso-file "read" :depends-on ("spack" "core"))
     #+(or) (pso-file "write" :depends-on ("spack" "core"))
     ))

(defsystem :quasischeme/translator
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme/rts)
  :serial t                        ;[No time to figure out the deps... --TRC]
  :components
  (
   (pso-file "p-record")                ; record package
   (pso-file "p-utils")                 ; tables and fluids
   (pso-file "list")                    ; list utilities
   (pso-file "classes")                 ; expression classes
   (pso-file "form")                    ; expression stuff used by classifier
   (pso-file "classify")                ; expression classifier
   (pso-file "node")                    ; budding node abstraction
   (pso-file "module")                  ; signatures and structures
   (pso-file "ssig")                    ; Scheme signature
   (pso-file "alpha")                   ; front end
   (pso-file "rules")                   ; the (syntax-rules ...) macro
   (pso-file "derive")                  ; derived expression types
   (pso-file "strategy")                ; LETREC strategy anaylzer
   (pso-file "version")
   (pso-file "schemify")                ; degenerate back end

   ;; Common Lisp back end
   (pso-file "emit")                    ; code emission utilities
   (pso-file "generate")                ; CL code generator
   (pso-file "builtin")                 ; CL info about scheme built-ins
   (pso-file "translate")               ; phase coordination and file transducer
   (pso-file "reify")                   ; miscellaneous
   ))

(defsystem :quasischeme/evaluator
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme/rts :quasischeme/translator)
  :components ((:file "eval")))

(defsystem :quasischeme
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on
    (:quasischeme/rts :quasischeme/translator :quasischeme/evaluator))

;;; A proper, self-hosted Scheme reader/writer (adapted from
;;; Scheme48's), as an explicit alternative to the default CL-reader
;;; bridge (qs:scheme-read-using-commonlisp-reader, installed by
;;; readwrite.lisp as part of :quasischeme/rts). Loading this system
;;; switches qs:*scheme-read*/*scheme-write*/*scheme-display* to it --
;;; the right choice for evaluating or loading ordinary R5RS/R6RS/R7RS
;;; Scheme source: unlike the CL-reader bridge, it correctly handles
;;; the `...' ellipsis identifier, standard string escapes (\n \t...),
;;; #| |# block comments and #; datum comments, more named characters,
;;; and case-preserving SYMBOL->STRING/WRITE (read.scm/write.scm have
;;; the details and rationale).
;;;
;;; It is NOT used for retranslating the translator's own .scm sources
;;; (see :quasischeme/bootstrap) -- those rely throughout on bare CL
;;; package-qualified symbols like QS-LISP:SETF, and on the CL-reader
;;; bridge's exact case-folding being in effect, matching how the
;;; classifier's own keyword tables were originally built. Re-pointing
;;; the translator itself at this reader would be a much bigger step
;;; than loading ordinary Scheme source with it; QUASISCHEME-BOOTSTRAP
;;; always forces the CL-reader bridge back on for that regardless of
;;; what this system has set globally (see bootstrap.lisp).
(defsystem :quasischeme/reader
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme/rts)
  :components
  ((pso-file "read")
   (pso-file "write")))

;;; The R5RS-conformant surface. REVISED^4-SCHEME-INTERFACE (ssig.scm)
;;; already includes R5RS's additions over R4RS (VALUES, DYNAMIC-WIND,
;;; EVAL, string ports, ...) in the one shared runtime structure, so
;;; this system is otherwise just the core stack under its R5RS name;
;;; tests/run-r5rs-tests.lisp (chibi's R5RS suite) is what actually
;;; gates this system's R5RS claim -- see its header comment and the
;;; project README for remaining known gaps.
(defsystem :quasischeme/r5rs
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme/rts :quasischeme/translator
	       :quasischeme/evaluator :quasischeme/reader))

;;; Building and filling program environments from Lisp; see
;;; src/environments.lisp.
(defsystem :quasischeme/environments
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme/rts :quasischeme/translator
	       :quasischeme/evaluator :quasischeme/reader)
  :components ((:file "environments")))

;;; The foreign-function layer the Schemes share, on CFFI: Chez's FFI
;;; and Guile's (system foreign) are made of it.  See src/ffi.lisp.
(defsystem :quasischeme/cffi
  :version "1.0"
  :pathname #p"src/"
  :depends-on (:quasischeme/rts :cffi)
  :components ((:file "ffi")))

;;; R6RS, with psyntax (vendor/psyntax/) as the front end: psyntax does
;;; all expansion and provides every standard library's namespace; the
;;; files of src/r6rs/ are the primitives those libraries refer to.  See
;;; src/psyntax.lisp.
(defsystem :quasischeme/r6rs
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme/r7rs-runtime
	       :quasischeme/cffi		; (chezscheme)'s FFI
	       :cl-unicode			; (rnrs unicode)
	       :trivial-gray-streams		; (rnrs io ports)
	       :bordeaux-threads		; (chezscheme)'s threads
	       #+sbcl :sb-posix)		; (chezscheme)'s getpid, putenv
  :components ((:module "r6rs"
		:components ((:file "rts")
			     (:file "lists" :depends-on ("rts"))
			     (:file "hashtables" :depends-on ("lists"))
			     (:file "guardians" :depends-on ("rts"))
			     (:file "records" :depends-on ("lists"))
			     (:file "conditions" :depends-on ("records"))
			     (:file "arithmetic" :depends-on ("conditions"))
			     (:file "bytevectors" :depends-on ("conditions"))
			     (:file "numeric-vectors" :depends-on ("conditions"))
			     (:file "enums" :depends-on ("conditions"))
			     (:file "ports" :depends-on ("bytevectors" "enums"))
			     (:file "r7rs-compat" :depends-on ("ports"))))
	       ;; Chez Scheme's (chezscheme): host procedures here, the
	       ;; library and Chez's top level in quasischeme/r7rs
	       ;; (src/chez/chez.lisp); docs/chez.md
	       (:module "chez"
		:depends-on ("r6rs" "psyntax")
		:components ((:file "host")
			     (:file "ffi" :depends-on ("host"))
			     (:file "ftype" :depends-on ("host"))
			     (:static-file "chezscheme.scm") (:static-file "ffi.scm") (:static-file "lists.scm")
			     (:static-file "syntax.scm") (:static-file "extend-syntax.sls")
			     (:static-file "ftype.scm") (:static-file "ftype-helpers.sls") (:static-file "system.scm") (:static-file "more.scm")
			     (:static-file "numbers.scm") (:static-file "hashtables.scm")
			     (:static-file "threads.scm") (:static-file "conditions.scm")
			     (:static-file "top-level.scm")))
	       (:module "compat"
		:depends-on ("r6rs" "psyntax" "chez")
		:components ((:static-file "ikarus.scm")))
	       (:file "psyntax" :depends-on ("r6rs"))
	       (:file "continuations" :depends-on ("psyntax"))
	       (:file "cp0" :depends-on ("psyntax" "continuations"))
	       (:file "cptypes" :depends-on ("cp0"))
	       (:file "cpdx" :depends-on ("cp0" "cptypes"))
	       (:file "library-cache" :depends-on ("psyntax" "continuations" "cptypes" "cpdx"))
	       (:file "compiled-files" :depends-on ("psyntax" "continuations" "cptypes" "cpdx"))
	       (psyntax-image "psyntax-quasischeme"
		:pathname "../vendor/psyntax/psyntax-quasischeme"
		:depends-on ("r6rs" "chez" "compat" "psyntax" "continuations" "cptypes" "cpdx" "library-cache"))))

;;; The Common Lisp face: packages R5RS, R6RS and R7RS (EVAL, LOAD,
;;; REPL, EXPAND, USE-LIBRARY, ...), and the bridge between the
;;; languages: (cl <package>) libraries for Scheme, Scheme libraries as
;;; Lisp packages.  See src/api.lisp, src/interop.lisp, docs/interop.md.
;;; The systems r5rs, r6rs and r7rs (r7rs.asd etc.) are shorthands.
(defsystem :quasischeme/api
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme/r7rs
	       :trivial-cltl2)		; lexical environments, for Scheme macros in Lisp
  :components ((:file "interop")
	       (:static-file "interop/lisp.sls")
	       (:file "api" :depends-on ("interop"))))

;;; Scheme source files as components of ASDF systems: :r7rs-file,
;;; :r6rs-file, :r5rs-file, :r7rs-library, :r6rs-library.  See
;;; src/asdf.lisp.
(defsystem :quasischeme/asdf
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme/api)
  :components ((:file "asdf")))

;;; The R7RS procedures (and the native R7RS environment they're built
;;; in, which the psyntax host copies).  See src/r7rs/*.
(defsystem :quasischeme/r7rs-runtime
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/r7rs/"
  :depends-on (:quasischeme/environments)
  :components ((:file "exports")
	       (:file "rts" :depends-on ("exports"))
	       (:file "r7rs" :depends-on ("exports" "rts"))))

;;; R7RS-small on psyntax: define-library, the (scheme ...) libraries,
;;; programs and the REPL.  See src/r7rs/front.lisp.
(defsystem :quasischeme/r7rs
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/r7rs/"
  :depends-on (:quasischeme/r6rs)
  :components ((:static-file "syntax.sls")
	       (:static-file "../control.sls")
	       (:file "front")
	       (:module "chez" :pathname "../chez/"
		:depends-on ("front")
		:components ((:file "chez") (:file "compile" :depends-on ("chez"))))
	       (:module "reports" :pathname "../reports/"
		:depends-on ("front" "chez")
		:components ((:file "reports")
			     (:static-file "reports.sls") (:static-file "rrs.sls")))
	       (:module "guile" :pathname "../guile/"
		:depends-on ("front")
		:components ((:file "guile")
			     (:static-file "guile.scm")))))

;;; Regenerates translator.files' .qso bootstrap artifacts from their
;;; .scm sources using the already-loaded translator. See bootstrap.lisp.
(defsystem :quasischeme/bootstrap
  :version "1.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:quasischeme)
  :components ((:file "bootstrap")))

;;; Guile: Guile's own boot-9, psyntax and modules, loaded from a Guile
;;; installation, on libguile's primitives written in Lisp and a
;;; Tree-IL compiler.  See src/guile/boot.lisp, docs/guile.md.
(defsystem :quasischeme/guile
  :version "1.0"
  :pathname #p"src/guile/"
  :depends-on (:quasischeme/api :quasischeme/cffi
	       :trivial-gray-streams)		; ports (src/guile/ports.lisp)
  :components ((:file "reader")
	       (:file "runtime" :depends-on ("reader"))
	       (:file "compile" :depends-on ("runtime"))
	       (:file "boot" :depends-on ("compile"))
	       (:file "primitives" :depends-on ("boot"))
	       (:file "ports" :depends-on ("primitives"))
	       (:file "threads" :depends-on ("primitives"))
	       (:file "regex" :depends-on ("primitives"))
	       (:file "foreign" :depends-on ("primitives"))
	       (:file "sockets" :depends-on ("ports"))
	       (:file "goops" :depends-on ("foreign" "ports"))
	       (:file "arrays" :depends-on ("primitives"))
	       (:file "cache" :depends-on ("boot"))
	       (:file "i18n" :depends-on ("primitives"))
	       (:file "write" :depends-on ("goops" "ports" "regex" "arrays"))
	       (:file "elisp" :depends-on ("primitives" "compile" "boot"))
	       ;; Guile's VM (docs/guile-vm.md)
	       (:file "vm-elf" :depends-on ("runtime"))
	       (:file "vm-ops" :depends-on ("primitives"))
	       (:file "vm" :depends-on ("vm-elf" "vm-ops" "goops"))
	       (:file "vm-jit" :depends-on ("vm"))
	       (:file "vm-vops" :depends-on ("vm-jit"))
	       (:file "stacks" :depends-on ("vm"))
	       (:static-file "root-primitives.txt")))

;;; Our libguile (docs/libguile.md): the Lisp half of a libguile-3.0.so.1
;;; that C programs and extensions built against GNU Guile 3.0 use.  The
;;; C half is built apart: make -C src/libguile.
(defsystem :quasischeme/libguile
  :version "1.0"
  :pathname #p"src/libguile/"
  :depends-on (:quasischeme/guile)
  :serial t
  :components ((:file "table")
	       (:file "scheme")
	       (:file "missing")
	       (:file "abi")
	       (:file "bridge")
	       (:file "impl")
	       (:file "api")
	       (:static-file "build-core.lisp")
	       (:static-file "core.c")
	       (:static-file "data.c")
	       (:static-file "psg.h")
	       (:static-file "libguile.map")
	       (:static-file "exports-3.0.11.txt")))

;;; --racket (docs/racket.md): three strategies, selected at run time.
;;; Skeleton: --racket=NAME is accepted and each strategy reports what
;;; isn't implemented yet.
(defsystem :quasischeme/racket
  :version "1.0"
  :pathname #p"src/racket/"
  :depends-on (:quasischeme/api :cl-ppcre #+sbcl :sb-introspect #+sbcl :sb-posix)
  :serial t
  :components ((:file "racket")
	       (:file "instances")
	       (:file "cache")
	       ;; B's runtime, and C's overrides
	       (:module "native"
		:serial t
		:components ((:file "native") (:file "structs") (:file "procedures")
			     (:file "control") (:file "hash") (:file "exn") (:file "basics") (:file "paths") (:file "equal") (:file "ports") (:file "port-ops") (:file "printer") (:file "regexp") (:file "more") (:file "data") (:file "handlers") (:file "foreign") (:file "extfl") (:file "network") (:file "chaperone") (:file "threads")
			     (:file "rktio")))
	       (:file "reader")
	       ;; A: Racket CS's layers on --chez
	       (:module "cs"
		:serial t
		:components ((:file "fork") (:static-file "fork.scm") (:file "layers")
			     (:file "hook") (:file "boot")))
	       ;; B: our linklet compiler
	       (:module "linklet"
		:serial t
		:components ((:file "api") (:file "compiler") (:file "sources") (:file "regexp-layer") (:file "boot") (:file "repl")))
	       ;; C: A plus native overrides
	       (:module "hybrid" :components ((:file "overrides")))
	       (:file "strategies")))

;;; The REPL every mode shares: a line editor, meta-commands, ,language;
;;; Guile's own REPL for --guile (src/repl/).
(defsystem :quasischeme/repl
  :version "1.0"
  :pathname #p"src/repl/"
  :depends-on (:quasischeme/api :quasischeme/guile :quasischeme/racket
	       #+sbcl :sb-posix #+sbcl :sb-introspect)
  :serial t
  :components ((:file "terminal")
	       (:file "highlight")
	       (:file "editor")
	       (:file "repl")
	       (:file "commands")
	       (:file "languages")
	       (:file "guile")
	       (:file "racket")
	       (:file "ide")))
