;;; SRFI 242: the CFG language.  Marc Nieper-Wißkirchen's reference
;;; implementation, for Chez Scheme (MIT licence; reference/srfi-242/,
;;; with its LICENSE), as R7RS libraries.  Its R6RS libraries are
;;; converted mechanically (the bodies unchanged) into private ones:
;;;
;;;   (srfi box), (srfi define-who), (srfi formals), (srfi list-case),
;;;   (srfi make-identifier-hashtable), (srfi renamer), (srfi with-implicit),
;;;   (srfi cfg) and (srfi cfg ast|compile|derived|expand|infer-types|
;;;   parse|primitive)  ->  (srfi private srfi-242-...), e.g.
;;;   (srfi private srfi-242-cfg-parse);
;;;   (srfi :213), (srfi :213 define-property)  ->  (srfi private
;;;   srfi-242-define-property), written for Pseudoscheme.
;;;
;;; The implementation needs SRFI 213's identifier properties (Chez's
;;; define-property, and transformers that return a procedure to be
;;; called with a lookup procedure).  psyntax has neither, so (srfi
;;; private srfi-242-define-property) emulates them, with a table of
;;; properties at expansion time and a define-syntax that calls a
;;; procedure a transformer returns with the lookup procedure; the
;;; converted libraries take define-syntax from it.  See that library
;;; for where the emulation falls short of SRFI 213.
;;;
;;; (srfi 242) and (srfi 242 cfg) are the SRFI's libraries.
(define-library (srfi 242)
  (export cfg call execute finally halt label* labels
          bind permute
          define-cfg-label define-cfg-label*
          define-cfg-syntax define-cfg-syntax*)
  (import (srfi 242 cfg)))
