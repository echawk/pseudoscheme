;;; (srfi private srfi-147-implementation): SRFI 147's sample
;;; implementation (Marc Nieper-Wißkirchen's; MIT licence, in
;;; reference/srfi-147/), adapted to psyntax.  The code is that of
;;; reference/srfi-147/implementation.scm and er-macro-transformer.scm
;;; with these changes:
;;;
;;;   - expand-transformer passes more transformer specs straight to the
;;;     native binding form, as it did scheme-syntax-rules and
;;;     scheme-er-macro-transformer: those headed by lambda, case-lambda,
;;;     the let family, if, cond and case (so syntax-case transformers
;;;     work), and by SRFI 211's transformer makers and R6RS's
;;;     identifier-syntax and make-variable-transformer.  Any other
;;;     macro use is expanded as a custom macro transformer, as in the
;;;     sample; so a transformer spec can't be a procedure call.
;;;   - scheme-er-macro-transformer is SRFI 211's er-macro-transformer
;;;     ((srfi private srfi-211-transformers), on syntax-case); the
;;;     sample uses Chibi's.  er-macro-transformer.scm's macro is
;;;     rewritten with syntax-case, so that its transformer renames
;;;     symbols where the er-macro-transformer keyword was written (the
;;;     syntax-rules version would rename them in this library), and
;;;     Chibi's (syntax-quote :c) became (syntax :c).
;;;   - scheme-syntax-rules works around a psyntax bug with a custom
;;;     ellipsis and ... as a literal: see (srfi private
;;;     srfi-147-syntax-rules).
;;;   - The library declarations follow the sample's
;;;     (srfi 147 implementation) library's else branch.
(define-library (srfi private srfi-147-implementation)
  (export define-syntax let-syntax letrec-syntax syntax-rules
          er-macro-transformer)
  (import (rename (except (scheme base) let-syntax letrec-syntax)
                  (syntax-rules native-syntax-rules)
                  (define-syntax scheme-define-syntax))
          (prefix (only (scheme base) let-syntax letrec-syntax) scheme-)
          (scheme case-lambda)
          (scheme cxr)
          (only (rnrs base) identifier-syntax)
          (only (rnrs syntax-case) make-variable-transformer)
          (rename (only (srfi private srfi-211-transformers) er-macro-transformer)
                  (er-macro-transformer scheme-er-macro-transformer))
          (only (srfi private srfi-211-transformers) ir-macro-transformer
                sc-macro-transformer rsc-macro-transformer lisp-transformer
                make-er-transformer)
          (only (rnrs syntax-case) syntax-case syntax syntax-violation)
          (rename (srfi private srfi-147-syntax-rules)
                  (syntax-rules scheme-syntax-rules)))
  (begin
(scheme-define-syntax :c
  (scheme-syntax-rules ()))

(scheme-define-syntax expand-transformer
  (scheme-syntax-rules (scheme-syntax-rules native-syntax-rules syntax-error begin
			scheme-er-macro-transformer ir-macro-transformer sc-macro-transformer rsc-macro-transformer lisp-transformer identifier-syntax make-variable-transformer make-er-transformer lambda case-lambda let let* letrec letrec* let-values let*-values if cond case)
    ((expand-transformer (k ...) (scheme-syntax-rules . args))
     (k ... (scheme-syntax-rules . args)))
    ((expand-transformer (k ...) (native-syntax-rules . args))
     (k ... (native-syntax-rules . args)))
    ((expand-transformer (k ...) (scheme-er-macro-transformer . args))
     (k ... (scheme-er-macro-transformer . args)))
    ((expand-transformer (k ...) (ir-macro-transformer . args))
     (k ... (ir-macro-transformer . args)))
    ((expand-transformer (k ...) (sc-macro-transformer . args))
     (k ... (sc-macro-transformer . args)))
    ((expand-transformer (k ...) (rsc-macro-transformer . args))
     (k ... (rsc-macro-transformer . args)))
    ((expand-transformer (k ...) (lisp-transformer . args))
     (k ... (lisp-transformer . args)))
    ((expand-transformer (k ...) (identifier-syntax . args))
     (k ... (identifier-syntax . args)))
    ((expand-transformer (k ...) (make-variable-transformer . args))
     (k ... (make-variable-transformer . args)))
    ((expand-transformer (k ...) (make-er-transformer . args))
     (k ... (make-er-transformer . args)))
    ((expand-transformer (k ...) (lambda . args))
     (k ... (lambda . args)))
    ((expand-transformer (k ...) (case-lambda . args))
     (k ... (case-lambda . args)))
    ((expand-transformer (k ...) (let . args))
     (k ... (let . args)))
    ((expand-transformer (k ...) (let* . args))
     (k ... (let* . args)))
    ((expand-transformer (k ...) (letrec . args))
     (k ... (letrec . args)))
    ((expand-transformer (k ...) (letrec* . args))
     (k ... (letrec* . args)))
    ((expand-transformer (k ...) (let-values . args))
     (k ... (let-values . args)))
    ((expand-transformer (k ...) (let*-values . args))
     (k ... (let*-values . args)))
    ((expand-transformer (k ...) (if . args))
     (k ... (if . args)))
    ((expand-transformer (k ...) (cond . args))
     (k ... (cond . args)))
    ((expand-transformer (k ...) (case . args))
     (k ... (case . args)))
    ((expand-transformer (k ...) (syntax-error . args))
     (syntax-error . args))
    ((expand-transformer (k ...) (begin definition ... transformer-spec))
     (begin definition
	    ...
	    (expand-transformer (k ...) transformer-spec)))   
    ((expand-transformer (k ...) (keyword . args))
     (keyword (:c expand-transformer (k ...)) . args))
    ((expand-transformer (k ...) keyword)
     (k ... (scheme-syntax-rules ()
	      ((_ . args) (keyword . args)))))))

(scheme-define-syntax define-syntax
  (scheme-syntax-rules ()
    ((define-syntax name transformer-spec)
     (expand-transformer (scheme-define-syntax name) transformer-spec))
    ((define-syntax . _)
     (syntax-error "invalid define-syntax syntax"))))

(scheme-define-syntax let-syntax
  (scheme-syntax-rules ()
    ((let-syntax ((keyword transformer-spec) ...) body1 body2 ...)
     (let ()
       (let-syntax-aux (keyword ...) (transformer-spec ...) () (body1 body2 ...))))
    ((let-syntax . _)
     (syntax-error "invalid let-syntax syntax"))))

(scheme-define-syntax let-syntax-aux
  (scheme-syntax-rules ()
    ((let-syntax-aux (keyword ...) () (transformer-spec ...) body*)
     (scheme-let-syntax ((keyword transformer-spec) ...) . body*))
    ((let-syntax-aux keyword* (transformer-spec1 transformer-spec2 ...) transformer-spec* body*)
     (expand-transformer (let-syntax-aux keyword*
					    (transformer-spec2 ...)
					    transformer-spec*
					    body*)
			 transformer-spec1))
    ((let-syntax-aux keyword*
		     (transformer-spec2 ...)
		     (transformer-spec ...)
		     body*
		     transformer-spec1)
     (let-syntax-aux keyword*
		     (transformer-spec2 ...)
		     (transformer-spec ... transformer-spec1)
		     body*))))

(scheme-define-syntax letrec-syntax
  (scheme-syntax-rules ()
    ((letrec-syntax ((keyword transformer-spec) ...) body1 body2 ...)
     (let ()
       (letrec-syntax-aux (keyword ...) (transformer-spec ...) () (body1 body2 ...))))
    ((letrec-syntax . _)
     (syntax-error "invalid letrec-syntax syntax"))))

(scheme-define-syntax letrec-syntax-aux
  (scheme-syntax-rules ()
    ((letrec-syntax-aux (keyword ...) () (transformer-spec ...) body*)
     (begin
       (define-syntax keyword transformer-spec)
       ...
       (let () . body*)))
    ((letrec-syntax-aux keyword*
			(transformer-spec1 transformer-spec2 ...)
			transformer-spec*
			body*)
     (expand-transformer (letrec-syntax-aux keyword*
					       (transformer-spec2 ...)
					       transformer-spec*
					       body*)
			 transformer-spec1))
    ((letrec-syntax-aux keyword*
			(transformer-spec2 ...)
			(transformer-spec ...)
			body*
			transformer-spec1)
     (letrec-syntax-aux keyword*
			(transformer-spec2 ...)
			(transformer-spec ... transformer-spec1)
			body*))))

(scheme-define-syntax syntax-rules
  (scheme-syntax-rules (:c)
    ((syntax-rules (:c k ...) . args)
     (syntax-rules-aux "state0" :c (k ...) . args))
    ((syntax-rules . _)
     (syntax-error "invalid syntax-rules syntax"))))

(scheme-define-syntax syntax-rules-aux
  (scheme-syntax-rules ()
    ((syntax-rules-aux "state0" :c k* (literal* ...) . rule*)
     (syntax-rules-aux "state1" :c k* (... ...) ((literal* ... :c)) rule* () rule*))

    ((syntax-rules-aux "state0" :c k* ellipsis (literal* ...) . rule*)
     (syntax-rules-aux "state1" :c k* ellipsis (ellipsis (literal* ... :c))
       rule* () rule*))
   
    ((syntax-rules-aux "state1" :c (k ...) e (l ...) () (rule1* ...) rule2*)
     (k ... (scheme-syntax-rules l ... rule1* ... . rule2*)))

    ((syntax-rules-aux "state1" :c k* ::: l*
       (((_ . pattern) template) . rule1*) (rule2 ...) rule3*)
     (syntax-rules-aux "state1" :c k* ::: l* rule1*
       (rule2
	...
	((_ (:c c :::) . pattern)
	 (c ::: template)))
       rule3*))
    ((syntax-rules-aux . _)
     (syntax-error "invalid syntax-rules syntax"))))

(scheme-define-syntax er-macro-transformer
  (lambda (x)
    (syntax-case x (:c)
      ((kw (:c k ...) transformer)
       (syntax
        (k ...
           (make-er-transformer
            (syntax kw)
            (lambda (expr rename compare)
              (if (and (pair? (cdr expr))
                       (pair? (cadr expr))
                       (compare (syntax :c) (caadr expr)))
                  `(,@(cdadr expr) ,(transformer (cons (car expr) (cddr expr)) rename compare))
                  (transformer expr rename compare)))))))
      (_
       (syntax-violation 'er-macro-transformer "invalid er-macro-transformer syntax" x)))))))
