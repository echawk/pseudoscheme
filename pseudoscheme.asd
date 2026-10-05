;;; -*- Mode: Lisp -*-

;;;; Pseudoscheme
;;;; System Definitions

(cl:defpackage :pseudoscheme-system
  (:use :common-lisp :asdf))

(cl:in-package :pseudoscheme-system)

(defclass pso-file (cl-source-file) ())

(defmethod source-file-type ((component pso-file) system)
  (declare (ignore component system))
  "pso")

;;; psyntax's expanded image (vendor/psyntax/psyntax-pseudoscheme.pp),
;;; compiled: the compile-op translates it to Lisp in a psyntax host and
;;; compiles that (PSX::COMPILE-IMAGE); the load-op only records the
;;; fasl, which PSX::BOOT loads into each host it creates.
(defclass psyntax-image (source-file)
  ((type :initform "pp")))

(defmethod output-files ((o compile-op) (c psyntax-image))
  (list (make-pathname :type (uiop:compile-file-type) :defaults (component-pathname c))))

(defmethod perform ((o compile-op) (c psyntax-image))
  (uiop:symbol-call "PSEUDOSCHEME-PSYNTAX" "COMPILE-IMAGE"
		    (component-pathname c) (output-file o c)))

(defmethod perform ((o load-op) (c psyntax-image))
  (setf (symbol-value (uiop:find-symbol* "*IMAGE-FASL*" "PSEUDOSCHEME-PSYNTAX"))
	(first (input-files o c))))

;;; Pseudoscheme is split into a core run-time (rts), the self-hosted
;;; Scheme-to-Common-Lisp translator (translator), and an evaluator/REPL
;;; layer built on both.  Language-standard surfaces (r5rs, r6rs, r7rs)
;;; are secondary systems built on top of this shared core; see
;;; :pseudoscheme/r5rs below.

(defsystem :pseudoscheme/rts
  :version "3.0"
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
     #+(or) (pso-file "read" :depends-on ("spack" "core"))
     #+(or) (pso-file "write" :depends-on ("spack" "core"))
     ))

(defsystem :pseudoscheme/translator
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme/rts)
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

(defsystem :pseudoscheme/evaluator
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme/rts :pseudoscheme/translator)
  :components ((:file "eval")))

(defsystem :pseudoscheme
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on
    (:pseudoscheme/rts :pseudoscheme/translator :pseudoscheme/evaluator))

;;; A proper, self-hosted Scheme reader/writer (adapted from
;;; Scheme48's), as an explicit alternative to the default CL-reader
;;; bridge (ps:scheme-read-using-commonlisp-reader, installed by
;;; readwrite.lisp as part of :pseudoscheme/rts). Loading this system
;;; switches ps:*scheme-read*/*scheme-write*/*scheme-display* to it --
;;; the right choice for evaluating or loading ordinary R5RS/R6RS/R7RS
;;; Scheme source: unlike the CL-reader bridge, it correctly handles
;;; the `...' ellipsis identifier, standard string escapes (\n \t...),
;;; #| |# block comments and #; datum comments, more named characters,
;;; and case-preserving SYMBOL->STRING/WRITE (read.scm/write.scm have
;;; the details and rationale).
;;;
;;; It is NOT used for retranslating the translator's own .scm sources
;;; (see :pseudoscheme/bootstrap) -- those rely throughout on bare CL
;;; package-qualified symbols like PS-LISP:SETF, and on the CL-reader
;;; bridge's exact case-folding being in effect, matching how the
;;; classifier's own keyword tables were originally built. Re-pointing
;;; the translator itself at this reader would be a much bigger step
;;; than loading ordinary Scheme source with it; PSEUDOSCHEME-BOOTSTRAP
;;; always forces the CL-reader bridge back on for that regardless of
;;; what this system has set globally (see bootstrap.lisp).
(defsystem :pseudoscheme/reader
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme/rts)
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
(defsystem :pseudoscheme/r5rs
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme/rts :pseudoscheme/translator
	       :pseudoscheme/evaluator :pseudoscheme/reader))

;;; Building and filling program environments from Lisp; see
;;; src/environments.lisp.
(defsystem :pseudoscheme/environments
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme/rts :pseudoscheme/translator
	       :pseudoscheme/evaluator :pseudoscheme/reader)
  :components ((:file "environments")))

;;; R6RS, with psyntax (vendor/psyntax/) as the front end: psyntax does
;;; all expansion and provides every standard library's namespace; the
;;; files of src/r6rs/ are the primitives those libraries refer to.  See
;;; src/psyntax.lisp.
(defsystem :pseudoscheme/r6rs
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme/r7rs-runtime
	       :cl-unicode			; (rnrs unicode)
	       :trivial-gray-streams)		; (rnrs io ports)
  :components ((:module "r6rs"
		:components ((:file "rts")
			     (:file "lists" :depends-on ("rts"))
			     (:file "hashtables" :depends-on ("lists"))
			     (:file "records" :depends-on ("lists"))
			     (:file "conditions" :depends-on ("records"))
			     (:file "arithmetic" :depends-on ("conditions"))
			     (:file "bytevectors" :depends-on ("conditions"))
			     (:file "numeric-vectors" :depends-on ("conditions"))
			     (:file "enums" :depends-on ("conditions"))
			     (:file "ports" :depends-on ("bytevectors"))
			     (:file "r7rs-compat" :depends-on ("ports"))))
	       (:module "compat"
		:depends-on ("r6rs" "psyntax")
		:components ((:file "chezscheme-host")
			     (:static-file "chezscheme.scm")
			     (:static-file "ikarus.scm")))
	       (:file "psyntax" :depends-on ("r6rs"))
	       (:file "continuations" :depends-on ("psyntax"))
	       (:file "library-cache" :depends-on ("psyntax" "continuations"))
	       (psyntax-image "psyntax-pseudoscheme"
		:pathname "../vendor/psyntax/psyntax-pseudoscheme"
		:depends-on ("r6rs" "compat" "psyntax" "continuations" "library-cache"))))

;;; The Common Lisp face: packages R5RS, R6RS and R7RS (EVAL, LOAD,
;;; REPL, EXPAND, USE-LIBRARY, ...), and the bridge between the
;;; languages: (cl <package>) libraries for Scheme, Scheme libraries as
;;; Lisp packages.  See src/api.lisp, src/interop.lisp, docs/interop.md.
;;; The systems r5rs, r6rs and r7rs (r7rs.asd etc.) are shorthands.
(defsystem :pseudoscheme/api
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme/r7rs
	       :trivial-cltl2)		; lexical environments, for Scheme macros in Lisp
  :components ((:file "interop")
	       (:static-file "interop/lisp.sls")
	       (:file "api" :depends-on ("interop"))))

;;; Scheme source files as components of ASDF systems: :r7rs-file,
;;; :r6rs-file, :r5rs-file, :r7rs-library, :r6rs-library.  See
;;; src/asdf.lisp.
(defsystem :pseudoscheme/asdf
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme/api)
  :components ((:file "asdf")))

;;; The R7RS procedures (and the native R7RS environment they're built
;;; in, which the psyntax host copies).  See src/r7rs/*.
(defsystem :pseudoscheme/r7rs-runtime
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/r7rs/"
  :depends-on (:pseudoscheme/environments)
  :components ((:file "exports")
	       (:file "rts" :depends-on ("exports"))
	       (:file "r7rs" :depends-on ("exports" "rts"))))

;;; R7RS-small on psyntax: define-library, the (scheme ...) libraries,
;;; programs and the REPL.  See src/r7rs/front.lisp.
(defsystem :pseudoscheme/r7rs
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/r7rs/"
  :depends-on (:pseudoscheme/r6rs)
  :components ((:static-file "syntax.sls")
	       (:file "front")))

;;; Regenerates translator.files' .pso bootstrap artifacts from their
;;; .scm sources using the already-loaded translator. See bootstrap.lisp.
(defsystem :pseudoscheme/bootstrap
  :version "3.0"
  :author "Jonathan Rees"
  :pathname #p"src/"
  :depends-on (:pseudoscheme)
  :components ((:file "bootstrap")))
