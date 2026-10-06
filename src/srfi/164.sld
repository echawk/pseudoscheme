;;; SRFI 164: enhanced multi-dimensional arrays.  Written for
;;; Pseudoscheme from the SRFI document: the SRFI's only implementation
;;; is Kawa's, in Java.  The design follows the document's description
;;; of Kawa's: an array is a view onto storage through an index mapping,
;;; either affine (a backing vector, an offset and a stride per
;;; dimension, as made by make-array, array, share-array and
;;; array-reshape of a simple array) or procedural (build-array,
;;; array-transform, array-index-share and the other views).
;;;
;;; Gvectors are Scheme vectors: a vector is a simple rank-1 array with
;;; lower bound 0.  The SRFI's ranges are non-normative, and Kawa's range
;;; syntax ([1 <: 3], [<:]) isn't available; SRFI 196 ranges of exact
;;; integers serve instead wherever an index array or a shape dimension
;;; may be given (a range in a shape specifier must have step 1).
;;; array->vector of a simple array whose elements fill its backing
;;; vector returns that vector; of any other array, it returns a rank-1,
;;; zero-based array that is a view, as the SRFI requires, rather than a
;;; Scheme vector.  There is no array literal syntax or printer (SRFI
;;; 163), and equal? does not compare arrays.
;;;
;;; make-array, array, array-ref, array-set!, shape and share-array have
;;; the names and (extended) contracts of SRFI 25's, and clash with SRFI
;;; 25, 47, 63, 179 and 231's.
(define-library (srfi 164)
  (export array? shape ->shape array-shape array-rank array-start
          array-end array-size array make-array build-array index-array
          array-ref array-index-ref array-set! array-copy! array-fill!
          array-transform array-index-share array-reshape share-array
          array-flatten array->vector)
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 196) range? range-length range-ref))
  (begin
    ;; lo, hi: vectors of bounds.  For an affine array, store is the
    ;; backing vector and the element at index (i ...) is at
    ;; offset + sum(stride_k * i_k); getter and setter are #f.  For a
    ;; procedural one, store is #f and getter (and setter, or #f when
    ;; immutable) take an index vector.
    (define-record-type <array>
      (%make-array lo hi store offset strides getter setter mutable?)
      %array?
      (lo %lo)
      (hi %hi)
      (store %store)
      (offset %offset)
      (strides %strides)
      (getter %getter)
      (setter %setter)
      (mutable? %mutable?))

    (define (array? obj) (or (vector? obj) (%array? obj)))

    (define (check-array who a)
      (unless (array? a) (error (string-append who ": not an array") a)))

    ;; --- bounds ---

    (define (lo-of a) (if (vector? a) (vector 0) (%lo a)))
    (define (hi-of a) (if (vector? a) (vector (vector-length a)) (%hi a)))

    (define (array-rank a)
      (check-array "array-rank" a)
      (if (vector? a) 1 (vector-length (%lo a))))

    (define (array-start a k) (vector-ref (lo-of a) k))
    (define (array-end a k) (vector-ref (hi-of a) k))

    (define (bounds-size lo hi)
      (let loop ((k 0) (n 1))
        (if (= k (vector-length lo))
            n
            (loop (+ k 1) (* n (- (vector-ref hi k) (vector-ref lo k)))))))

    (define (array-size a)
      (check-array "array-size" a)
      (bounds-size (lo-of a) (hi-of a)))

    ;; A shape specifier -> (values lo hi).
    (define (shape-bounds spec)
      (define (bad) (error "not a shape specifier" spec))
      (define (check lo hi)
        (let loop ((k 0))
          (when (< k (vector-length lo))
            (let ((b (vector-ref lo k)) (e (vector-ref hi k)))
              (unless (and (exact-integer? b) (exact-integer? e) (<= b e))
                (bad)))
            (loop (+ k 1))))
        (values lo hi))
      (cond
       ((vector? spec)
        (let* ((r (vector-length spec))
               (lo (make-vector r))
               (hi (make-vector r)))
          (do ((k 0 (+ k 1)))
              ((= k r) (check lo hi))
            (let ((d (vector-ref spec k)))
              (cond ((exact-integer? d)
                     (vector-set! lo k 0) (vector-set! hi k d))
                    ((and (list? d) (= (length d) 2))
                     (vector-set! lo k (car d)) (vector-set! hi k (cadr d)))
                    ((range? d)
                     (let ((n (range-length d)))
                       (cond ((zero? n) (bad))
                             ((and (> n 1)
                                   (not (eqv? 1 (- (range-ref d 1) (range-ref d 0)))))
                              (bad))
                             (else
                              (vector-set! lo k (range-ref d 0))
                              (vector-set! hi k (+ (range-ref d 0) n))))))
                    (else (bad)))))))
       ((and (%array? spec)
             (= (vector-length (%lo spec)) 2)
             (eqv? 2 (- (vector-ref (%hi spec) 1) (vector-ref (%lo spec) 1))))
        (let* ((r0 (vector-ref (%lo spec) 0))
               (r (- (vector-ref (%hi spec) 0) r0))
               (c0 (vector-ref (%lo spec) 1))
               (lo (make-vector r))
               (hi (make-vector r)))
          (do ((k 0 (+ k 1)))
              ((= k r) (check lo hi))
            (vector-set! lo k (%ref spec (vector (+ r0 k) c0)))
            (vector-set! hi k (%ref spec (vector (+ r0 k) (+ c0 1)))))))
       (else (bad))))

    ;; Canonical (r 2) shape array from bounds.
    (define (bounds->shape lo hi)
      (let* ((r (vector-length lo))
             (v (make-vector (* 2 r))))
        (do ((k 0 (+ k 1)))
            ((= k r))
          (vector-set! v (* 2 k) (vector-ref lo k))
          (vector-set! v (+ 1 (* 2 k)) (vector-ref hi k)))
        (make-simple (vector 0 0) (vector r 2) v #f)))

    (define (->shape spec)
      (let-values (((lo hi) (shape-bounds spec)))
        (bounds->shape lo hi)))

    (define (shape . bounds)
      (unless (even? (length bounds))
        (error "shape: odd number of bounds" bounds))
      (let loop ((bs bounds) (los '()) (his '()))
        (if (null? bs)
            (let ((lo (list->vector (reverse los)))
                  (hi (list->vector (reverse his))))
              (let-values (((lo hi) (shape-bounds
                                     (vector-map list lo hi))))
                (bounds->shape lo hi)))
            (loop (cddr bs) (cons (car bs) los) (cons (cadr bs) his)))))

    (define (array-shape a)
      (check-array "array-shape" a)
      (bounds->shape (lo-of a) (hi-of a)))

    ;; --- affine arrays ---

    ;; Row-major strides for bounds.
    (define (row-major-strides lo hi)
      (let* ((r (vector-length lo))
             (s (make-vector r 1)))
        (let loop ((k (- r 1)) (n 1))
          (when (>= k 0)
            (vector-set! s k n)
            (loop (- k 1) (* n (- (vector-ref hi k) (vector-ref lo k))))))
        s))

    (define (dot strides index)
      (let loop ((k 0) (n 0))
        (if (= k (vector-length strides))
            n
            (loop (+ k 1) (+ n (* (vector-ref strides k) (vector-ref index k)))))))

    ;; A simple array over STORE, whose element at the lower bounds is at
    ;; position START.
    (define (make-simple lo hi store mutable? . start)
      (let ((strides (row-major-strides lo hi))
            (start (if (pair? start) (car start) 0)))
        (%make-array lo hi store (- start (dot strides lo)) strides #f #f
                     mutable?)))

    (define (affine? a) (or (vector? a) (%store a)))

    ;; Is A affine with its elements contiguous in row-major order?
    ;; Returns the store position of its first element, or #f.
    (define (simple-start a)
      (cond ((vector? a) 0)
            ((%store a)
             (let* ((lo (%lo a)) (hi (%hi a)) (s (%strides a))
                    (rm (row-major-strides lo hi)))
               (let loop ((k 0))
                 (cond ((= k (vector-length lo))
                        (+ (%offset a) (dot s lo)))
                       ((or (<= (- (vector-ref hi k) (vector-ref lo k)) 1)
                            (= (vector-ref s k) (vector-ref rm k)))
                        (loop (+ k 1)))
                       (else #f)))))
            (else #f)))

    ;; --- element access ---

    (define (check-index who a index)
      (let ((lo (lo-of a)) (hi (hi-of a)))
        (unless (= (vector-length index) (vector-length lo))
          (error (string-append who ": wrong number of indexes") index))
        (do ((k 0 (+ k 1)))
            ((= k (vector-length lo)))
          (let ((i (vector-ref index k)))
            (unless (and (exact-integer? i)
                         (<= (vector-ref lo k) i)
                         (< i (vector-ref hi k)))
              (error (string-append who ": index out of bounds") index))))))

    ;; Unchecked access by index vector.
    (define (%ref a index)
      (cond ((vector? a) (vector-ref a (vector-ref index 0)))
            ((%store a)
             (vector-ref (%store a) (+ (%offset a) (dot (%strides a) index))))
            (else ((%getter a) index))))

    (define (%set! a index v)
      (cond ((vector? a) (vector-set! a (vector-ref index 0) v))
            ((not (%mutable? a)) (error "array-set!: immutable array" a))
            ((%store a)
             (vector-set! (%store a) (+ (%offset a) (dot (%strides a) index)) v))
            (else ((%setter a) index v))))

    ;; An index argument list -> an index vector.
    (define (index-vector who args)
      (if (and (pair? args) (null? (cdr args)) (not (exact-integer? (car args))))
          (let ((x (car args)))
            (cond ((vector? x) x)
                  ((and (%array? x) (= 1 (vector-length (%lo x))))
                   (let* ((b (vector-ref (%lo x) 0))
                          (n (- (vector-ref (%hi x) 0) b))
                          (v (make-vector n)))
                     (do ((k 0 (+ k 1))) ((= k n) v)
                       (vector-set! v k (%ref x (vector (+ b k)))))))
                  (else (error (string-append who ": bad index") x))))
          (list->vector args)))

    (define (array-ref a . args)
      (check-array "array-ref" a)
      (let ((index (index-vector "array-ref" args)))
        (check-index "array-ref" a index)
        (%ref a index)))

    (define (array-set! a . args)
      (check-array "array-set!" a)
      (when (null? args) (error "array-set!: no value"))
      (let* ((rev (reverse args))
             (index (index-vector "array-set!" (reverse (cdr rev)))))
        (check-index "array-set!" a index)
        (%set! a index (car rev))))

    ;; Call PROC on each index vector of bounds LO HI, in row-major order;
    ;; each is fresh.
    (define (for-each-index lo hi proc)
      (let ((r (vector-length lo)))
        (unless (zero? (bounds-size lo hi))
          (let loop ((index (vector-copy lo)))
            (proc (vector-copy index))
            (let next ((k (- r 1)))
              (when (>= k 0)
                (let ((i (+ 1 (vector-ref index k))))
                  (if (< i (vector-ref hi k))
                      (begin (vector-set! index k i) (loop index))
                      (begin (vector-set! index k (vector-ref lo k))
                             (next (- k 1)))))))))))

    ;; The row-major elements of A, as a fresh vector.
    (define (array-flatten a)
      (check-array "array-flatten" a)
      (if (vector? a)
          (vector-copy a)
          (let ((v (make-vector (array-size a)))
                (j 0))
            (for-each-index (%lo a) (%hi a)
                            (lambda (index)
                              (vector-set! v j (%ref a index))
                              (set! j (+ j 1))))
            v)))

    ;; --- construction ---

    (define (array spec . objs)
      (let-values (((lo hi) (shape-bounds spec)))
        (let ((v (list->vector objs)))
          (unless (= (vector-length v) (bounds-size lo hi))
            (error "array: wrong number of elements" spec objs))
          (make-simple lo hi v #t))))

    (define (make-array spec . values)
      (let-values (((lo hi) (shape-bounds spec)))
        (let* ((n (bounds-size lo hi))
               (v (make-vector n #f)))
          (unless (null? values)
            (let loop ((i 0) (vs values))
              (when (< i n)
                (vector-set! v i (car vs))
                (loop (+ i 1) (if (null? (cdr vs)) values (cdr vs))))))
          (make-simple lo hi v #t))))

    (define build-array
      (case-lambda
        ((spec getter) (build-array spec getter #f))
        ((spec getter setter)
         (let-values (((lo hi) (shape-bounds spec)))
           (%make-array lo hi #f #f #f getter setter (and setter #t))))))

    ;; Row-major position of INDEX within bounds LO HI.
    (define (row-major-position lo hi index)
      (let loop ((k 0) (n 0))
        (if (= k (vector-length lo))
            n
            (loop (+ k 1)
                  (+ (* n (- (vector-ref hi k) (vector-ref lo k)))
                     (- (vector-ref index k) (vector-ref lo k)))))))

    ;; The index vector at row-major position N within bounds LO HI.
    (define (position->index lo hi n)
      (let* ((r (vector-length lo))
             (index (make-vector r)))
        (let loop ((k (- r 1)) (n n))
          (if (< k 0)
              index
              (let ((len (- (vector-ref hi k) (vector-ref lo k))))
                (vector-set! index k (+ (vector-ref lo k) (remainder n len)))
                (loop (- k 1) (quotient n len)))))))

    (define (index-array spec)
      (let-values (((lo hi) (shape-bounds spec)))
        (%make-array lo hi #f #f #f
                     (lambda (index) (row-major-position lo hi index))
                     #f #f)))

    ;; --- views ---

    (define (array-transform a spec transform)
      (check-array "array-transform" a)
      (let-values (((lo hi) (shape-bounds spec)))
        (%make-array lo hi #f #f #f
                     (lambda (index) (%ref a (transform index)))
                     (lambda (index v) (%set! a (transform index) v))
                     (mutable? a))))

    (define (mutable? a) (or (vector? a) (%mutable? a)))

    (define (array-reshape a spec)
      (check-array "array-reshape" a)
      (let-values (((lo hi) (shape-bounds spec)))
        (unless (= (bounds-size lo hi) (array-size a))
          (error "array-reshape: sizes differ" a spec))
        (let ((start (simple-start a)))
          (if start
              (make-simple lo hi (if (vector? a) a (%store a)) (mutable? a)
                           start)
              (let ((alo (lo-of a)) (ahi (hi-of a)))
                (define (old index)
                  (position->index alo ahi (row-major-position lo hi index)))
                (%make-array lo hi #f #f #f
                             (lambda (index) (%ref a (old index)))
                             (lambda (index v) (%set! a (old index) v))
                             (mutable? a)))))))

    (define (array->vector a)
      (check-array "array->vector" a)
      (if (vector? a)
          a
          (let ((start (simple-start a))
                (n (array-size a)))
            (if (and start (zero? start) (= n (vector-length (%store a))))
                (%store a)
                (array-reshape a (vector n))))))

    (define (share-array a spec proc)
      (check-array "share-array" a)
      (let-values (((lo hi) (shape-bounds spec)))
        (let ((r (vector-length lo)))
          (define (mapped index)
            (call-with-values (lambda () (apply proc (vector->list index)))
              vector))
          (if (affine? a)
              ;; Compose PROC with A's affine mapping, by probing PROC at
              ;; zero and at each unit index.
              (let* ((astrides (if (vector? a) (vector 1) (%strides a)))
                     (aoffset (if (vector? a) 0 (%offset a)))
                     (zero (make-vector r 0))
                     (base (+ aoffset (dot astrides (mapped zero))))
                     (strides (make-vector r)))
                (do ((k 0 (+ k 1)))
                    ((= k r))
                  (let ((unit (make-vector r 0)))
                    (vector-set! unit k 1)
                    (vector-set! strides k
                                 (- (+ aoffset (dot astrides (mapped unit)))
                                    base))))
                (unless (zero? (bounds-size lo hi))
                  ;; the corners must be within A's bounds
                  (check-index "share-array" a (mapped lo))
                  (check-index "share-array" a
                               (mapped (vector-map (lambda (h) (- h 1)) hi))))
                (%make-array lo hi (if (vector? a) a (%store a)) base strides
                             #f #f (mutable? a)))
              (%make-array lo hi #f #f #f
                           (lambda (index) (%ref a (mapped index)))
                           (lambda (index v) (%set! a (mapped index) v))
                           (mutable? a))))))

    ;; An index argument of array-index-ref and array-index-share, as
    ;; (values lo hi ref), REF taking the slice of the result's index
    ;; vector that belongs to it.
    (define (index-argument who x)
      (cond ((exact-integer? x)
             (values (vector) (vector) (lambda (index) x)))
            ((range? x)
             (values (vector 0) (vector (range-length x))
                     (lambda (index) (range-ref x (vector-ref index 0)))))
            ((array? x)
             (values (lo-of x) (hi-of x) (lambda (index) (%ref x index))))
            (else (error (string-append who ": bad index") x))))

    (define (array-index-share a . indexes)
      (check-array "array-index-share" a)
      (unless (= (length indexes) (array-rank a))
        (error "array-index-share: wrong number of indexes" indexes))
      (let loop ((xs indexes) (los '()) (his '()) (parts '()))
        (if (pair? xs)
            (let-values (((lo hi ref) (index-argument "array-index-share" (car xs))))
              (loop (cdr xs) (cons lo los) (cons hi his)
                    (cons (cons (vector-length lo) ref) parts)))
            (let ((lo (apply vector-append (reverse los)))
                  (hi (apply vector-append (reverse his)))
                  (parts (reverse parts)))
              (define (base-index index)
                (let ((v (make-vector (length parts))))
                  (let fill ((ps parts) (k 0) (start 0))
                    (if (null? ps)
                        v
                        (let ((n (caar ps)))
                          (vector-set! v k ((cdar ps)
                                            (vector-copy index start (+ start n))))
                          (fill (cdr ps) (+ k 1) (+ start n)))))))
              ;; every selected element must be within A's bounds
              (for-each-index lo hi
                              (lambda (index)
                                (check-index "array-index-share" a
                                             (base-index index))))
              (if (zero? (vector-length lo))
                  (let ((index (base-index (vector))))
                    (%make-array lo hi #f #f #f
                                 (lambda (i) (%ref a index))
                                 (lambda (i v) (%set! a index v))
                                 (mutable? a)))
                  (%make-array lo hi #f #f #f
                               (lambda (index) (%ref a (base-index index)))
                               (lambda (index v) (%set! a (base-index index) v))
                               (mutable? a)))))))

    (define (array-index-ref a . indexes)
      (if (every-integer? indexes)
          (apply array-ref a indexes)
          (let* ((view (apply array-index-share a indexes))
                 (lo (%lo view))
                 (hi (%hi view))
                 (v (array-flatten view)))
            ;; a rank-1, zero-based result is a vector
            (if (and (= 1 (vector-length lo)) (zero? (vector-ref lo 0)))
                v
                (make-simple lo hi v #f)))))

    (define (every-integer? xs)
      (or (null? xs) (and (exact-integer? (car xs)) (every-integer? (cdr xs)))))

    ;; --- modification ---

    (define (same-bounds? a b)
      (and (equal? (lo-of a) (lo-of b)) (equal? (hi-of a) (hi-of b))))

    (define (array-copy! dst src)
      (check-array "array-copy!" dst)
      (check-array "array-copy!" src)
      (unless (same-bounds? dst src)
        (error "array-copy!: shapes differ" dst src))
      (let ((elements (array-flatten src))
            (j 0))
        (for-each-index (lo-of dst) (hi-of dst)
                        (lambda (index)
                          (%set! dst index (vector-ref elements j))
                          (set! j (+ j 1))))))

    (define (array-fill! a value)
      (check-array "array-fill!" a)
      (for-each-index (lo-of a) (hi-of a)
                      (lambda (index) (%set! a index value))))))
