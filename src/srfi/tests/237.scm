;;; Tests for SRFI 237: the sample implementation's test suite
;;; (reference/srfi-237/tests.sps, with its (example dictionary)
;;; library, reference/srfi-237/dictionary.sls), as SRFI 64 tests,
;;; plus a few more.  Most definitions are internal ones, in several
;;; procedures: Pseudoscheme compiles many definitions and calls in one
;;; body slowly (see ../57.sld).

(define-library (srfi-237-test dictionary)
  (export dictionary dictionary? dictionary-ref
          dictionary-from-hashtable make-dictionary-from-hashtable
          dictionary-from-alist make-dictionary-from-alist)
  (import (rnrs base)
          (rnrs hashtables)
          (srfi 237))
  (begin
    (define-record-type dictionary
      (nongenerative dictionary-e6a703a4-5469-4f6e-8cbb-19d0f66de601)
      (opaque #t)
      (fields ht)
      (protocol
       (lambda (p)
         (lambda args
           (assert #f)))))

    (define dictionary-ref
      (lambda (dict key default)
        (hashtable-ref (dictionary-ht dict) key default)))

    (define-record-name (dictionary-from-hashtable dictionary)
      (protocol
       (lambda (p)
         (lambda (ht)
           (assert (hashtable? ht))
           (p ht)))))

    (define-record-name (dictionary-from-alist dictionary)
      (protocol
       (lambda (p)
         (lambda (alist)
           (define ht (make-eqv-hashtable))
           (assert (list? alist))
           (for-each
            (lambda (entry)
              (assert (pair? entry))
              (hashtable-set! ht (car entry) (cdr entry)))
            alist)
           (p ht)))))))

(import (except (scheme base) define-record-type)
        (scheme process-context)
        (only (rnrs base) assert)
        (only (rnrs hashtables) make-eqv-hashtable hashtable-set!)
        (srfi 64)
        (srfi 237)
        (srfi-237-test dictionary))

(define (foo-tests)
  (define-record-type foo
    (fields x (mutable y foo-y foo-set-y!))
    (protocol
     (lambda (new)
       (lambda (x)
         (new x #f)))))
  (define-record-type bar
    (parent foo)
    (fields z)
    (protocol
     (lambda (n)
       (lambda (x z)
         ((n x) z)))))
  (test-equal #t (foo? (make-foo 1)))
  (test-equal 2 (foo-x (make-foo 2)))
  (test-equal '(3 4) (let ((foo (make-foo 3)))
                       (foo-set-y! foo 4)
                       (list (foo-x foo) (foo-y foo))))
  (let ((rtd (record-type-descriptor foo))
        (rcd (record-constructor-descriptor foo)))
    (test-equal 'foo (record-type-name rtd))
    (test-assert (not (record-type-parent rtd)))
    (test-assert (record-type-generative? rtd))
    (test-assert (not (record-type-sealed? rtd)))
    (test-assert (not (record-type-opaque? rtd)))
    (test-equal '#(x y) (record-type-field-names rtd))
    (test-assert (not (record-field-mutable? rtd 0)))
    (test-assert (record-field-mutable? rtd 1))
    (test-assert rcd)
    (test-assert "record name evaluates to a record descriptor"
                 (record-descriptor? foo))
    (test-eq rtd (record-descriptor-rtd foo)))
  (test-assert (foo? (make-bar 5 6)))
  (test-equal 5 (foo-x (make-bar 5 6)))
  (test-equal 6 (bar-z (make-bar 5 6)))
  (test-equal 7 (let ((bar (make-bar 5 6)))
                  (foo-set-y! bar 7)
                  (foo-y bar)))
  (test-eq "record-descriptor-parent" foo (record-descriptor-parent bar)))

;;; Inheritance between the layers
(define (layer-tests)
  (define-record-type rec1
    (fields a)
    (protocol
     (lambda (p)
       (lambda (a/2)
         (p (* 2 a/2))))))
  (define rec2
    (make-record-descriptor
     'rec2 rec1 #f #f #f '#((immutable b))
     (lambda (n)
       (lambda (a/2 b)
         ((n a/2) b)))))
  (define make-rec2 (record-constructor rec2))
  (define rec2? (record-predicate rec2))
  (define rec2-b (record-accessor rec2 0))
  (define-record-type rec3
    (parent rec2)
    (fields c)
    (protocol
     (lambda (n)
       (lambda (c)
         ((n c c) c)))))
  ;; A base type made procedurally, with a mutable field (not in tests.sps)
  (define prec
    (make-record-descriptor 'prec #f #f #f #f '#((mutable m)) #f))
  (test-equal '(6 7) (let ((r (make-rec2 3 7))) (list (rec1-a r) (rec2-b r))))
  (test-assert (rec2? (make-rec3 5)))
  (test-equal '(10 5 5) (let ((r (make-rec3 5)))
                          (list (rec1-a r) (rec2-b r) (rec3-c r))))
  (let ((r ((record-constructor prec) 1)))
    ((record-mutator prec 0) r 2)
    (test-equal 2 ((record-accessor prec 0) r))))

;;; Generative clause, different symbolic name, uids
(define (misc-tests)
  (define-record-type gen
    (generative))
  (define-record-type (sname rname))
  (define-record-type urec
    (nongenerative urec-7373d255-44a2-41f1-87e7-bf41a924e390))
  (test-assert (gen? (make-gen)))
  (test-assert (sname? (make-sname)))
  (test-assert (record-descriptor? rname))
  (test-eq 'sname (record-type-name rname))
  (test-eqv (record-descriptor-rtd urec)
            (record-uid->rtd 'urec-7373d255-44a2-41f1-87e7-bf41a924e390))
  (test-eqv (record-descriptor-rtd dictionary)
            (record-uid->rtd 'dictionary-e6a703a4-5469-4f6e-8cbb-19d0f66de601))
  (test-eqv #f (record-uid->rtd 'norecord))
  (test-assert (not (record-type-generative? urec)))
  (test-error (port-read-rtd 'x)))

;;; Multiple constructors
(define (salmon-tests)
  (define-record-type fish
    (fields name))
  (define-record-name (salmon fish)
    (protocol
     (lambda (p)
       (lambda ()
         (p 'salmon)))))
  (define-record-type colored-salmon
    (parent salmon)
    (fields color)
    (protocol
     (lambda (n)
       (lambda (c)
         ((n) c)))))
  (define-record-name (green-salmon colored-salmon)
    (protocol
     (lambda (n)
       (lambda ()
         ((n) 'green)))))
  (define-record-name (blue-salmon colored-salmon)
    (parent fish)
    (protocol
     (lambda (n)
       (lambda ()
         ((n 'salmon) 'blue)))))
  (test-equal 'salmon (fish-name (make-salmon)))
  (test-equal 'green (colored-salmon-color (make-green-salmon)))
  (test-equal 'blue (colored-salmon-color (make-blue-salmon)))
  (test-equal '(salmon c) (let ((s (make-colored-salmon 'c)))
                            (list (fish-name s) (colored-salmon-color s)))))

;;; Dictionary example
(define (dictionary-tests)
  (define-record-type owned-dictionary
    (parent dictionary)
    (fields owner)
    (protocol
     (lambda (n)
       (lambda args
         (assert #f)))))
  (define-record-name (owned-dictionary-from-hashtable owned-dictionary)
    (parent dictionary-from-hashtable)
    (protocol
     (lambda (n)
       (lambda (ht owner)
         ((n ht) owner)))))
  (define-record-name (owned-dictionary-from-alist owned-dictionary)
    (parent dictionary-from-alist)
    (protocol
     (lambda (n)
       (lambda (alist owner)
         ((n alist) owner)))))
  (let ((d (make-owned-dictionary-from-alist '((1 . one)) 'me)))
    (test-assert (dictionary? d))
    (test-assert (owned-dictionary? d))
    (test-equal 'one (dictionary-ref d 1 #f))
    (test-equal 'me (owned-dictionary-owner d)))
  (let ((ht (make-eqv-hashtable)))
    (hashtable-set! ht 2 'two)
    (test-equal 'two
      (dictionary-ref (make-owned-dictionary-from-hashtable ht 'you) 2 #f)))
  (test-error (make-owned-dictionary 1)))

;;; A top-level definition
(define-record-type tl (fields a))

(define (top-level-tests)
  (test-equal 1 (tl-a (make-tl 1))))

(test-begin "srfi-237")
(foo-tests)
(layer-tests)
(misc-tests)
(salmon-tests)
(dictionary-tests)
(top-level-tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-237")
  (exit (if (zero? failures) 0 1)))
