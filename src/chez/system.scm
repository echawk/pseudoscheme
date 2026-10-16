;;; -*- Mode: Scheme -*-
;;;; Part of the body of Pseudoscheme's (chezscheme) library: Chez's
;;;; system procedures, the $-names that a program reaches as #%$name
;;;; ($primitive, top-level.scm) and that (chezscheme) doesn't export, and
;;;; the other names Racket CS's rumble layer and the schemified layers
;;;; use that upstream Chez has (docs/racket.md, "The Chez fork's
;;;; additions").  Host procedures are in host.lisp (chez:sys-...).

;;; Application: the procedures themselves; in Chez the names tell its
;;; optimizer (no inlining, no return, one value)
(define $app
  (case-lambda
    ((f) (f))
    ((f a) (f a))
    ((f a b) (f a b))
    ((f a b c) (f a b c))
    ((f . args) (apply f args))))
(define $app/no-inline $app)
(define $app/no-return $app)
(define $app/value $app)
(define ($value x) x)

;;; Records
(define $record? %chez:sys-record?)
(define $record-type-field-count %chez:sys-record-type-field-count)
(define $record-cas! %chez:sys-record-cas!)
(define $sealed-record? %chez:sys-sealed-record?)
(define $sealed-record-instance? %chez:sys-sealed-record?)
(define $record-type-act-sealed? %chez:sys-record-type-act-sealed?)
(define $record-type-act-sealed! %chez:sys-record-type-act-sealed!)

;;; Stencil vectors, which the system's own (the HAMTs of rumble) are of
;;; a kind of their own: the $-names work on both kinds, those with
;;; system in them on the system's
(define $stencil-vector? %chez:sys-stencil-vector?)
(define $system-stencil-vector? %chez:sys-system-stencil-vector?)
(define $system-stencil-vector %chez:sys-system-stencil-vector)
(define $stencil-vector-mask %chez:sys-stencil-vector-mask)
(define $stencil-vector-length %chez:sys-stencil-vector-length)
(define $stencil-vector-ref %chez:sys-stencil-vector-ref)
(define $stencil-vector-set! %chez:sys-stencil-vector-set!)
(define $system-stencil-vector-update %chez:sys-system-stencil-vector-update)
(define $system-stencil-vector-truncate! %chez:sys-system-stencil-vector-truncate!)
(define ($stencil-vector-fill-set! v i x) (%chez:sys-stencil-vector-set! v i x))
(define ($make-stencil-vector len mask)
  (assertion-violation '$make-stencil-vector "should only be used as inlined with GC disabled"))
(define ($make-system-stencil-vector len mask)
  (assertion-violation '$make-system-stencil-vector "should only be used as inlined with GC disabled"))
(define ($make-empty-stencil-vector mask)
  (apply stencil-vector mask (make-list (fxpopcount mask) 0)))
(define ($stencil-vector-do-update v remove add . vals)
  (apply stencil-vector-update v remove add vals))
(define ($system-stencil-vector-do-update v remove add . vals)
  (apply %chez:sys-system-stencil-vector-update v remove add vals))

;;; Immutable objects, and the checks inline operations make
(define $vector-set-immutable! %chez:sys-vector-set-immutable!)
(define $string-set-immutable! %chez:sys-string-set-immutable!)
(define $bytevector-set-immutable! %chez:sys-bytevector-set-immutable!)
(define $vector-ref-check? %chez:sys-vector-ref-check?)
(define $vector-set!-check? %chez:sys-vector-set!-check?)

;;; Numbers, time, memory, threads
(define $flhash %chez:sys-flhash)
(define $quotient-remainder %chez:sys-quotient-remainder)
(define ($gc-cpu-time)
  (let* ((s (vector-ref (%chez:gc-statistics) 1))
         (sec (floor s)))
    (make-time 'time-duration (floor (* (- s sec) 1000000000)) sec)))
(define ($generation x) 0)
(define-syntax $active-threads
  (identifier-syntax (%chez:sys-active-threads)))
(define ($target-machine) (machine-type))
(define (current-memory-bytes) (bytes-allocated))
(define maximum-memory-bytes-seen 0)
(define (maximum-memory-bytes)
  (set! maximum-memory-bytes-seen (max maximum-memory-bytes-seen (current-memory-bytes)))
  maximum-memory-bytes-seen)
(define (reset-maximum-memory-bytes!)
  (set! maximum-memory-bytes-seen (current-memory-bytes)))
(define (bytes-deallocated) (%chez:sys-bytes-deallocated))
(define (bytes-finalized) 0)
(define initial-bytes-allocated-at-start (bytes-allocated))
(define (initial-bytes-allocated) initial-bytes-allocated-at-start)
(define enable-object-backreferences (make-parameter #f ->boolean))
(define (object-backreferences) #f)
(define (thread-preserve-ownership!) (if #f #f))
(define initial-thread (list 'initial-thread))
(define (get-initial-thread) initial-thread)

;;; Procedures and code: a procedure is its own code object here
(define ($closure-code f)
  (unless (procedure? f) (assertion-violation '$closure-code "not a procedure" f))
  f)
(define $code-name %chez:sys-code-name)
(define ($code? x) (procedure? x))
(define ($code-info c) #f)
(define ($closure-length f) 0)
(define ($closure-ref f i) (assertion-violation '$closure-ref "invalid index" i))
(define ($procedure-realm f) #f)
(define (procedure-known-single-valued? p) #f)
(define (keep-live x) (if #f #f))
(define (black-box x) x)
(define (assert-unreachable) (assertion-violation 'assert-unreachable "unreachable code reached"))
(define-syntax pariah
  (syntax-rules () ((_ e1 e2 ...) (begin e1 e2 ...))))
(define-syntax memory-order-acquire
  (syntax-rules () ((_) (if #f #f))))
(define-syntax memory-order-release
  (syntax-rules () ((_) (if #f #f))))
(define current-generate-id
  (make-parameter (lambda (sym) (gensym (symbol->string sym)))))
(define vector-copy! %chez:vector-copy!)

;;; Continuations.  Chez's attachments are the values the continuation's
;;; frames carry; here they are continuation marks of one key
;;; (host.lisp).  A continuation can't be taken apart here: the frames
;;; are the transformation's, not Chez's, so the procedures on its parts
;;; give the empty continuation.
(define $null-continuation (list '$null-continuation))
(define ($continuation? x) #f)
(define ($continuation-link k) $null-continuation)
(define ($split-continuation k offset) (if #f #f))
(define ($continuation-return-code k) #f)
(define ($continuation-stack-return-code k offset) #f)
(define ($continuation-return-offset k) 0)
(define ($continuation-stack-return-offset k offset) 0)
(define ($continuation-return-frame-words k) 0)
(define ($continuation-stack-return-frame-words k offset) 0)
(define ($continuation-stack-clength k) 0)
(define $continuation-attachments %chez:sys-continuation-attachments)
(define $current-attachments %chez:sys-current-attachments)
(define $call-setting-continuation-attachment %chez:sys-call-setting-attachment)
(define $call-getting-continuation-attachment %chez:sys-call-getting-attachment)
(define $call-consuming-continuation-attachment %chez:sys-call-consuming-attachment)
(define $current-winders %chez:sys-current-winders)

(define (check-call-in who k p)
  (unless (or (procedure? k) (eq? k $null-continuation))
    (assertion-violation who "not a continuation" k))
  (unless (procedure? p)
    (assertion-violation who "not a procedure" p)))

;; ($call-in-continuation k [attachments] p): p's values go to k, with
;; the attachments the continuation had, or those given
(define $call-in-continuation
  (case-lambda
    ((k p)
     (check-call-in '$call-in-continuation k p)
     ($current-attachments ($continuation-attachments k))
     (if (eq? k $null-continuation) (p) (call-with-values p k)))
    ((k as p)
     (check-call-in '$call-in-continuation k p)
     (unless (list? as)
       (assertion-violation '$call-in-continuation "not a list of attachments" as))
     ($current-attachments as)
     (if (eq? k $null-continuation) (p) (call-with-values p k)))))

;; (call-in-continuation k [marks] p): p's values go to k.  (Chez calls
;; p in k's context, after k's winders have run: here p runs first.)
(define call-in-continuation
  (case-lambda
    ((k p)
     (check-call-in 'call-in-continuation k p)
     (call-with-values p k))
    ((k marks p)
     (check-call-in 'call-in-continuation k p)
     (unless (continuation-marks? marks)
       (assertion-violation 'call-in-continuation "not a continuation-marks object" marks))
     (call-with-values p k))))

;;; Exception states: the stack of handlers
(define-record-type (exception-state make-exception-state exception-state?)
  (nongenerative)
  (opaque #t)
  (sealed #t)
  (fields (immutable stack exception-state-stack)))
(define create-exception-state
  (case-lambda
    (() (make-exception-state '()))
    ((p)
     (unless (procedure? p) (assertion-violation 'create-exception-state "not a procedure" p))
     (make-exception-state (list p)))))
(define current-exception-state
  (case-lambda
    (() (make-exception-state (%chez:sys-handler-stack)))
    ((x)
     (unless (exception-state? x)
       (assertion-violation 'current-exception-state "not an exception state" x))
     (%chez:sys-handler-stack (exception-state-stack x)))))
;; what an exception nobody handles is given to; not consulted here, where
;; the top level reports it
(define base-exception-handler
  (make-parameter (lambda (c) (raise c))
    (lambda (p)
      (unless (procedure? p) (assertion-violation 'base-exception-handler "not a procedure" p))
      p)))

;;; Chez's conditions: format conditions (those errorf and the system's own
;;; errors make) and a continuation's
(define-condition-type &format &condition make-format-condition format-condition?)
(define-condition-type &continuation &condition
  make-continuation-condition continuation-condition?
  (k condition-continuation))

;;; Dates
(define (check-date who d)
  (unless (date? d) (assertion-violation who "not a date" d)))
(define (date-year-day d)
  (check-date 'date-year-day d)
  (let ((y (date-year d)) (m (date-month d)))
    (+ (vector-ref '#(0 31 59 90 120 151 181 212 243 273 304 334) (- m 1))
       (- (date-day d) 1)
       (if (and (> m 2) (or (and (zero? (mod y 4)) (not (zero? (mod y 100)))) (zero? (mod y 400)))) 1 0))))
(define (date-dst? d) (check-date 'date-dst? d) #f)
(define (date-zone-name d) (check-date 'date-zone-name d) #f)

;;; Printer parameters that don't change what is written here (but
;;; print-positive-exponent-sign, which the writer reads)
(define print-subnormal-precision (make-parameter #t ->boolean))
(define print-positive-exponent-sign (make-parameter #f ->boolean))
;; the writer (host.lisp) reads these
(define printer-parameters-registered
  (%chez:set-printer-parameters! print-graph print-length print-level print-radix print-gensym
                                 print-char-name print-vector-length print-unicode
                                 print-extended-identifiers print-positive-exponent-sign))

;;; Pseudo-random generators (host.lisp: Chez's own generator)
(define make-pseudo-random-generator %chez:make-pseudo-random-generator)
(define pseudo-random-generator? %chez:pseudo-random-generator?)
(define pseudo-random-generator-seed! %chez:pseudo-random-generator-seed!)
(define pseudo-random-generator-next! %chez:pseudo-random-generator-next!)
(define pseudo-random-generator->vector %chez:pseudo-random-generator->vector)
(define (vector->pseudo-random-generator v)
  (%chez:vector->pseudo-random-generator #f v))
(define (vector->pseudo-random-generator! g v)
  (unless (pseudo-random-generator? g)
    (assertion-violation 'vector->pseudo-random-generator! "not a pseudo-random generator" g))
  (%chez:vector->pseudo-random-generator g v))
