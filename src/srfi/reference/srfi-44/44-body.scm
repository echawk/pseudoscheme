;; R7RS-small implementation of SRFI 44: Collections.

;; SPDX-License-Identifier: MIT
;; SPDX-FileCopyrightText: 2003 Scott G. Miller
;; SPDX-FileCopyrightText: 2003 Taylor Campbell
;; SPDX-FileCopyrightText: 2024 Antero Mejr <mail@antr.me>

;; This SRFI was finalized in an incomplete state.
;; This version uses lists and records with inheritance.
;; It also minimizes the amount of exported identifiers by dispatching.
;; By design, some of the full set of aliases are invalid for certain types.

;; Uncertainties in the specification:
;; - Provides collection-count, but collections don't have the required
;;   equivalence function. Moved it to bag-count.
;; - Provides maps with equivalence and key-equivalence functions, but no way to
;;   set or initialize them. Used equal? and eqv? as the defaults.
;; - It is unclear what an ordering function is. Here, ordering functions take
;;   a list and a natural number from 0 to size-1, and higher arguments
;;   will return the higher precendence items. It's the same signature as
;;   list-ref.
;; - There is no way to set homogeneity, immutability, pure-mutability,
;;   directionality, or orderedness properties.
;; - Why directionality/orderedness are mutually exclusive.
;; - What the optional thunk argument does in alist-map-update-all. This
;;   implementation ignores it.

;; Superseding (and better) SRFIs for these data structures are:
;; - bag/set: SRFI 113
;; - map: SRFI 146
;; - sequence: SRFI 133
;; - flexible-sequence: SRFI 214

;; The body of the (begin ...) in contrib/antero-mejr/srfi/44.sld from the
;; SRFI 44 repository, verbatim; the library form is ../../44.sld.


    ;; SLOW: There is probably a better way to do this. Copy then mutate?
    (define (get-constructor col)
      (let ((name (collection-name col))
            (pmut (collection-purely-mutable? col))
            (imut (collection-immutable? col))
            (lim (collection-limited? col))
            (func (collection-ordering-function col))
            (dir (collection-directional? col)))
        (lambda (lst)
          (cond ((eq? name 'collection)
                 (mk-collection name lst pmut imut lim func dir))
                ((eq? name 'bag)
                 (mk-bag (mk-collection 'bag lst pmut imut lim func dir)
                         (bag-equivalence-function col)))
                ((eq? name 'set)
                 (mk-set (mk-collection 'set lst pmut imut lim func dir)
                         (set-equivalence-function col)))
                ((eq? name 'map)
                 (unless (every pair? lst)
                   (error "Map values must be pairs" lst))
                 (mk-map (mk-collection 'map lst #f #f #f #f #f) equal? eqv?))
                ((eq? name 'sequence)
                 (mk-sequence (mk-bag (mk-collection
                                       'sequence lst pmut imut lim func dir)
                                      equal?)))
                ((eq? name 'flexible-sequence)
                 (mk-flexible-sequence
                  (mk-sequence (mk-bag (mk-collection
                                        'flexible-sequence lst
                                        pmut imut lim func dir)
                                       equal?))))
                ((eq? name 'list)
                 (mk-list (mk-flexible-sequence
                           (mk-sequence
                            (mk-bag (mk-collection 'list lst
                                                   pmut imut lim func dir)
                                    equal?)))))
                ((eq? name 'vector)
                 (mk-vector (mk-sequence (mk-bag (mk-collection
                                                  'vector lst
                                                  pmut imut lim func dir)
                                                 equal?))))
                ((eq? name 'alist-map)
                 (mk-alist-map (mk-map (mk-collection 'alist-map lst
                                                      pmut imut lim func dir)
                                       (map-equivalence-function col)
                                       (map-key-equivalence-function col))))
                ((eq? name 'string)
                 (mk-string (mk-sequence
                             (mk-bag (mk-collection 'string lst
                                                    pmut imut lim func dir)
                                     char=?))))
                (else
                 (error "Unrecognized collection type" name))))))

    ;; Collection

    (define-record-type <collection>
      (mk-collection name lst
                     purely-mutable? immutable? limited?
                     ordering-function directional?)
      collection?
      (name collection-name)
      (lst collection-list collection-list-set!)
      ;; XXX: purely-mutable?, immutable?, limited? and directional? should
      ;; be combined into a single list of symbols.
      (purely-mutable? collection-purely-mutable?)
      (immutable? collection-immutable?)
      (limited? collection-limited?)
      (ordering-function collection-ordering-function)
      (directional? collection-directional?))

    (define (collection . rest)
      (mk-collection 'collection rest #f #f #f #f #f))

    (define (make-collection)
      (mk-collection 'collection '() #f #f #f #f #f))

    (define (collection->list col)
      (collection-list col))

    (define (collection-size col)
      (length (collection->list col)))

    (define collection-get-any
      (case-lambda
       ((col) (collection-get-any col (lambda _ #f)))
       ((col absence-thunk)
        (if (null? (collection->list col))
            (absence-thunk)
            (car (collection->list col))))))

    (define (collection-empty? col)
      (null? (collection->list col)))

    (define (collection-clear col)
      ((get-constructor col) '()))

    (define (collection-clear! col)
      (when (collection-immutable? col)
        (error "Cannot mutate immutable collection" col))
      (collection-list-set! col '())
      col)

    (define (permutation? equiv lst lst2)
      (let loop ((lst lst)
                 (lst2 lst))
        (cond ((and (null? lst) (null? lst2))
               #t)
              ((or (null? lst) (null? lst2))
               #f)
              ((member (car lst) lst2 equiv)
               (loop (cdr lst) (delete-one equiv (car lst) lst2)))
              (else #f))))

    (define (collection= equiv col . rest)
      (if (null? rest)
          #t
          (and (eq? (collection-name col) (collection-name (car rest)))
               (if (or (collection-ordering-function col)
                       (collection-directional? col))
                   ;; Ordered case
                   (every equiv (collection->list col)
                          (collection->list (car rest)))
                   ;; Unordered case
                   (permutation? equiv
                                 (collection->list col)
                                 (collection->list (car rest))))
               (apply collection= equiv col (cdr rest)))))

    (define (collection-copy col)
      ((get-constructor col) (list-copy (collection->list col))))

    (define (collection-fold-left seq f . seeds)
      (let ((size (collection-size seq))
            (seed-count (length seeds)))
        (let loop ((seeds seeds) (i 0))
          (if (>= i size)
              (apply values seeds)
              (receive (proceed? . new-seeds)
                  (apply f (r7rs:list-ref (collection->list seq) i) seeds)
                (if (= (length new-seeds) seed-count)
                    (if proceed?
                        (loop new-seeds (+ i 1))
                        (apply values new-seeds))
                    (error "(collection)-fold-left: Wrong seed count"
                           `(expected ,seed-count)
                           `(got ,(length new-seeds)))))))))

    (define (collection-fold-right seq f . seeds)
      (let ((size (collection-size seq))
            (seed-count (length seeds)))
        (let loop ((seeds seeds) (i (- size 1)))
          (if (negative? i)
              (apply values seeds)
              (receive (proceed? . new-seeds)
                  (apply f (r7rs:list-ref (collection->list seq) i) seeds)
                (if (= (length new-seeds) seed-count)
                    (if proceed?
                        (loop new-seeds (- i 1))
                        (apply values new-seeds))
                    (error "(sequence)-fold-right: Wrong seed count"
                           `(expected ,seed-count)
                           `(got ,(length new-seeds)))))))))

    ;; Limited collection (collection property)

    (define (limited-collection? obj)
      (and (collection? obj) (collection-limited? obj)))

    ;; Purely mutable collection (collection property)

    (define (purely-mutable-collection? obj)
      (and (collection? obj) (collection-purely-mutable? obj)))

    ;; Ordered collections (collection property)

    (define (ordered-collection? obj)
      (and (collection? obj) (collection-ordering-function obj) #t))

    (define (make-ordered-collection ordering-function)
      (mk-collection 'collection '() #f #f #f ordering-function #f))

    (define (ordered-collection-ordering-function col)
      (collection-ordering-function col))

    (define ordered-collection-get-right
      (case-lambda
       ((col) (ordered-collection-get-right col (lambda _ #f)))
       ((col thk)
        (let ((f (collection-ordering-function col)))
          (unless f
            (error "Not an ordered collection" col))
          (if (null? (collection->list col))
              (thk)
              (let ((lst (collection->list col)))
                (f (collection->list col) (- (length lst) 1))))))))

    (define ordered-collection-get-left
      (case-lambda
       ((col) (ordered-collection-get-left col (lambda _ #f)))
       ((col thk)
        (let ((f (collection-ordering-function col)))
          (unless f
            (error "Not an ordered collection" col))
          (if (null? (collection->list col))
              (thk)
              (f (collection->list col) 0))))))

    (define (ordered-collection-delete-right col)
      (unless (collection-ordering-function col)
        (error "Not an ordered collection" col))
      (let* ((f (collection-ordering-function col))
             (small (f (collection->list col) 0))
             (new-lst (r7rs:map (lambda (x)
                                  (f (collection->list col) x))
                                (iota (- (collection-size col) 1)))))
        (values ((get-constructor col) new-lst) small)))

    (define (ordered-collection-delete-left col)
      (unless (collection-ordering-function col)
        (error "Not an ordered collection" col))
      (let* ((f (collection-ordering-function col))
             (big (f (collection->list col) 0))
             (new-lst (r7rs:map (lambda (x)
                                  (f (collection->list col) x))
                                (iota (max 0 (- (collection-size col) 1)) 1))))
        (values ((get-constructor col) new-lst) big)))

    (define (ordered-collection-delete-left! col)
      (unless (collection-ordering-function col)
        (error "Not an ordered collection" col))
      (when (collection-immutable? col)
        (error "Collection immutable" col))
      (let* ((f (collection-ordering-function col))
             (big (f (collection->list col) 0))
             (new-lst (r7rs:map (lambda (x)
                                  (f (collection->list col) x))
                                (iota (max 0 (- (collection-size col) 1)) 1))))
        (collection-list-set! col new-lst)
        (values col big)))

    (define (ordered-collection-delete-right! col)
      (unless (collection-ordering-function col)
        (error "Not an ordered collection" col))
      (when (collection-immutable? col)
        (error "Collection immutable" col))
      (let* ((f (collection-ordering-function col))
             (big (f (collection->list col) (max 0 (- (collection-size col) 1))))
             (new-lst (r7rs:map (lambda (x)
                                  (f col x))
                                (iota (max 0 (- (collection-size col) 2)) 0))))
        (collection-list-set! col new-lst)
        (values col big)))

    ;; Directional collection (collection property)

    (define (directional-collection? obj)
      (and (collection? obj) (collection-directional? obj)))

    (define directional-ordering-function r7rs:list-ref)

    (define directional-collection-get-left
      (case-lambda
       ((col) (directional-collection-get-left col (lambda _ #f)))
       ((col thk)
        (unless (directional-collection? col)
          (error "Not a directional collection" col))
        (if (null? (collection->list col))
            (thk)
            (directional-ordering-function (collection->list col) 0)))))

    (define directional-collection-get-right
      (case-lambda
       ((col) (directional-collection-get-right col (lambda _ #f)))
       ((col thk)
        (unless (directional-collection? col)
          (error "Not a directional collection" col))
        (if (null? (collection->list col))
            (thk)
            (directional-ordering-function (collection->list col)
                                           (- (collection-size col) 1))))))

    (define (directional-collection-insert-left col val)
      (unless (collection-directional? col)
        (error "Not a directional collection" col))
      ((get-constructor col) (cons val (collection->list col))))

    (define (directional-collection-insert-right col val)
      (unless (collection-directional? col)
        (error "Not a directional collection" col))
      ((get-constructor col) (append (collection->list col) (r7rs:list val))))

    (define (directional-collection-insert-left! col val)
      (unless (collection-directional? col)
        (error "Not a directional collection" col))
      (when (collection-immutable? col)
        (error "Collection is immutable" col))
      (collection-list-set! col (cons val (collection->list col)))
      col)

    (define (directional-collection-insert-right! col val)
      (unless (collection-directional? col)
        (error "Not a directional collection" col))
      (when (collection-immutable? col)
        (error "Collection is immutable" col))
      (collection-list-set! col (append (collection->list col) (r7rs:list val)))
      col)

    (define (directional-collection-delete-left col)
      (unless (collection-directional? col)
        (error "Not a directional collection" col))
      (let* ((f directional-ordering-function)
             (small (f (collection->list col) 0))
             (new-lst (r7rs:map (lambda (x)
                                  (f (collection->list col) x))
                                (iota (- (collection-size col) 1) 1))))
        (values ((get-constructor col) new-lst) small)))

    (define (directional-collection-delete-right col)
      (unless (collection-directional? col)
        (error "Not a directional collection" col))
      (let* ((f directional-ordering-function)
             (big (f (collection->list col) (max 0 (- (collection-size col) 1))))
             (new-lst (r7rs:map (lambda (x)
                                  (f (collection->list col) x))
                                (iota (max 0 (- (collection-size col) 2)) 0))))
        (values ((get-constructor col) new-lst) big)))

    (define (directional-collection-delete-left! col)
      (unless (collection-directional? col)
        (error "Not an directional collection" col))
      (when (collection-immutable? col)
        (error "Collection immutable" col))
      (let* ((f directional-ordering-function)
             (small (f (collection->list col) 0))
             (new-lst (r7rs:map (lambda (x)
                                  (f (collection->list col) x))
                                (iota (- (collection-size col) 1) 1 ))))
        (collection-list-set! col new-lst)
        (values col small)))

    (define (directional-collection-delete-right! col)
      (unless (collection-directional? col)
        (error "Not an directional collection" col))
      (when (collection-immutable? col)
        (error "Collection immutable" col))
      (let* ((f directional-ordering-function)
             (big (f (collection->list col) (max 0 (- (collection-size col) 1))))
             (new-lst (r7rs:map (lambda (x)
                                  (f (collection->list col) x))
                                (iota (max 0 (- (collection-size col) 2)) 0))))
        (collection-list-set! col new-lst)
        (values col big)))

    ;; Bag (inherit collection)

    (define-record-type (<bag> <collection>)
      (mk-bag col equiv)
      bag?
      (equiv bag-equivalence-function))

    (define (make-bag)
      (mk-bag (mk-collection 'bag '() #f #f #f #f #f) equal?))

    (define (bag . rest)
      (mk-bag (mk-collection 'bag rest #f #f #f #f #f) equal?))

    (define (bag-count b val)
      (let ((lst (collection->list b))
            (equiv (bag-equivalence-function b)))
        (let loop ((out 0)
                   (lst lst))
          (if (null? lst)
              out
              (if (equiv val (car lst))
                  (loop (+ 1 out) (cdr lst))
                  (loop out (cdr lst)))))))

    (define (bag-contains? b val)
      (and (member val (collection->list b) (bag-equivalence-function b))
           #t))

    (define (bag-add b val)
      ((get-constructor b) (cons val (collection->list b))))

    (define (bag-add! b val)
      (collection-list-set! b (cons val (collection->list b)))
      b)

    (define (delete-one equiv val lst)
      ;; SLOW: I think this is slower than it could be?
      (let loop ((out '())
                 (lst lst))
        (cond ((null? lst)
               (reverse out))
              ((equiv val (car lst))
               (append (reverse out) (cdr lst)))
              (else
               (loop (cons (car lst) out) (cdr lst))))))

    (define (bag-delete b val)
      ((get-constructor b) (delete-one (bag-equivalence-function b) val
                                       (collection->list b))))

    (define (bag-delete! b val)
      (collection-list-set! b (delete-one (bag-equivalence-function b) val
                                          (collection->list b)))
      b)

    (define (bag-delete-all b val)
      ((get-constructor b) (delete val (collection->list b)
                                   (bag-equivalence-function b))))

    (define (bag-delete-all! b val)
      (collection-list-set! b (delete val (collection->list b)
                                      (bag-equivalence-function b)))
      b)

    (define (bag-add-from b b1)
      ((get-constructor b) (append (collection->list b) (collection->list b1))))

    (define (bag-add-from! b b1)
      (collection-list-set! b (append (collection->list b)
                                      (collection->list b1)))
      b)

    (define (bag-delete-from b b1)
      (let* ((bef (bag-equivalence-function b))
             (new (fold (lambda (x acc)
                          (delete-one bef x acc))
                        (collection->list b)
                        (collection->list b1))))
        ((get-constructor b) new)))

    (define (bag-delete-from! b b1)
      (let* ((bef (bag-equivalence-function b))
             (new (fold (lambda (x acc)
                          (delete-one bef x acc))
                        (collection->list b)
                        (collection->list b1))))
        (collection-list-set! b new)
        b))

    (define (bag-delete-all-from b b1)
      (let* ((bef (bag-equivalence-function b))
             (new (fold (lambda (x acc)
                          (delete x acc bef))
                        (collection->list b)
                        (collection->list b1))))
        ((get-constructor b) new)))

    (define (bag-delete-all-from! b b1)
      (let* ((bef (bag-equivalence-function b))
             (new (fold (lambda (x acc)
                          (delete x acc bef))
                        (collection->list b)
                        (collection->list b1))))
        (collection-list-set! b new)
        b))

    ;; Set (inherit collection)

    (define-record-type (<set> <collection>)
      (mk-set col equiv)
      set?
      (equiv set-equivalence-function))

    (define (make-set)
      (mk-set (mk-collection 'set '() #f #f #f #f #f) equal?))

    (define (set . rest)
      (mk-set (mk-collection 'set (delete-duplicates rest)
                             #f #f #f #f #f) equal?))

    (define (set-contains? s val)
      (and (member val (collection->list s) (set-equivalence-function s))
           #t))

    (define (set-subset? s . rest)
      (if (null? rest)
          #t
          (and (lset<= (set-equivalence-function s) (collection->list s)
                       (collection->list (car rest)))
               (apply set-subset? s (cdr rest)))))

    (define (set-add s val)
      ((get-constructor s) (lset-adjoin (set-equivalence-function s)
                                        (collection->list s) val)))

    (define (set-add! s val)
      (collection-list-set! s (lset-adjoin (set-equivalence-function s)
                                           (collection->list s) val))
      s)

    (define (set-count s val)
      (let loop ((lst (collection->list s)))
        (if (null? lst)
            0
            (if ((set-equivalence-function s) val (car lst))
                1
                (loop (cdr lst))))))

    (define (set-delete s val)
      ((get-constructor s) (delete-one (set-equivalence-function s)
                                       val (collection->list s) )))

    (define (set-delete! s val)
      (collection-list-set! s (delete-one (set-equivalence-function s)
                                          val (collection->list s)))
      s)

    (define (set-union s . rest)
      ((get-constructor s) (apply lset-union (set-equivalence-function s)
                                  (collection->list s)
                                  (r7rs:map collection->list rest))))

    (define (set-union! s . rest)
      (collection-list-set! s (apply lset-union (set-equivalence-function s)
                                     (collection->list s)
                                     (r7rs:map collection->list rest)))
      s)

    (define (set-intersection s . rest)
      ((get-constructor s) (apply lset-intersection (set-equivalence-function s)
                                  (collection->list s)
                                  (r7rs:map collection->list rest))))

    (define (set-intersection! s . rest)
      (collection-list-set! s (apply lset-intersection
                                     (set-equivalence-function s)
                                     (collection->list s)
                                     (r7rs:map collection->list rest)))
      s)

    (define (set-difference s . rest)
      ((get-constructor s) (apply lset-difference (set-equivalence-function s)
                                  (collection->list s)
                                  (r7rs:map collection->list rest))))

    (define (set-difference! s . rest)
      (collection-list-set! s (apply lset-difference (set-equivalence-function s)
                                     (collection->list s)
                                     (r7rs:map collection->list rest)))
      s)

    (define (set-symmetric-difference s s1)
      ((get-constructor s) (lset-xor (set-equivalence-function s)
                                     (collection->list s)
                                     (collection->list s1))))

    (define (set-symmetric-difference! s s1)
      (collection-list-set! s (lset-xor (set-equivalence-function s)
                                        (collection->list s)
                                        (collection->list s1)))
      s)

    (define (set-add-from s b)
      ((get-constructor s) (lset-union (set-equivalence-function s)
                                       (collection->list s)
                                       (collection->list b))))

    (define (set-add-from! s b)
      (collection-list-set! s (lset-union (set-equivalence-function s)
                                          (collection->list s)
                                          (collection->list b)))
      s)

    (define (set-delete-from s b)
      ((get-constructor s) (lset-difference (set-equivalence-function s)
                                            (collection->list s)
                                            (collection->list b))))

    (define (set-delete-from! s b)
      (collection-list-set! s (lset-difference (set-equivalence-function s)
                                               (collection->list s)
                                               (collection->list b)))
      s)

    ;; Map (inherit collection)

    (define-record-type (<map> <collection>)
      (mk-map col equiv key-equiv)
      map?
      (equiv map-equivalence-function)
      (key-equiv map-key-equivalence-function))

    (define (make-map)
      (mk-map (mk-collection 'map '() #f #f #f #f #f) equal? eqv?))

    (define (map . rest)
      (unless (every pair? rest)
        (error "Map values must be pairs" rest))
      (mk-map (mk-collection 'map rest #f #f #f #f #f) equal? eqv?))

    (define (map-contains-key? m k)
      (and (member k (r7rs:map car (collection->list m))
                   (map-key-equivalence-function m))
           #t))

    (define (map-keys->list m)
      (r7rs:map car (collection->list m)))

    (define (map-count m val)
      (let loop ((c 0)
                 (lst (collection->list m)))
        (if (null? lst)
            c
            (if ((map-equivalence-function m) (car lst) val)
                (loop (+ 1 c) (cdr lst))
                (loop c (cdr lst))))))

    (define map-get
      (case-lambda
       ((m key) (map-get m key (lambda _ #f)))
       ((m key thk)
        (let ((pair (assoc key (collection->list m)
                           (map-key-equivalence-function m))))
          (if pair
              (cdr pair)
              (thk))))))

    (define map-put
      (case-lambda
       ((m key val) (map-put m key val (lambda _ #f)))
       ((m key val thk)
        (let ((pair (assoc key (collection->list m)
                           (map-key-equivalence-function m))))
          (if pair
              (values ((get-constructor m)
                       (cons (cons key val) (collection->list m)))
                      (cdr pair))
              (values m (thk)))))))

    (define map-put!
      (case-lambda
       ((m key val) (map-put! m key val (lambda _ #f)))
       ((m key val thk)
        (let ((pair (assoc key (collection->list m)
                           (map-key-equivalence-function m))))
          (if pair
              (begin (collection-list-set! m (cons (cons key val)
                                                   (collection->list m)))
                     (values m (cdr pair)))
              (values m (thk)))))))

    (define map-update
      (case-lambda
       ((m key func) (map-update key func (lambda _ #f)))
       ((m key func thk)
        (let ((pair (assoc key (collection->list m)
                           (map-key-equivalence-function m))))
          (if pair
              (values ((get-constructor m)
                       (cons (cons key (func (cdr pair))) (collection->list m)))
                      (cdr pair))
              (values m (thk)))))))

    (define map-update!
      (case-lambda
       ((m key func) (map-update! m key func (lambda _ #f)))
       ((m key func thk)
        (let ((pair (assoc key (collection->list m)
                           (map-key-equivalence-function m))))
          (if pair
              (begin (collection-list-set! m (cons (cons key (func (cdr pair)))
                                                   (collection->list m)))
                     (values m (cdr pair)))
              (values m (thk)))))))

    (define (map-delete m key)
      ((get-constructor m)
       (delete-one (lambda (x y)
                     (let ((comp (map-key-equivalence-function m)))
                       (comp (car x) (car y))))
                   (cons key #f) (collection->list m))))

    (define (map-delete! m key)
      (collection-list-set!
       m (delete-one (lambda (x y)
                       (let ((comp (map-key-equivalence-function m)))
                         (comp (car x) (car y))))
                     (cons key #f) (collection->list m)))
      m)

    (define (map-delete-from m b)
      (let* ((func (lambda (x y)
                     ((map-key-equivalence-function m) x (car y))))
             (new (fold (lambda (x acc)
                          (delete-one func x acc))
                        (collection->list m)
                        (collection->list b))))
        ((get-constructor m) new)))

    (define (map-delete-from! m b)
      (let* ((func (lambda (x y)
                     ((map-key-equivalence-function m) x (car y))))
             (new (fold (lambda (x acc)
                          (delete-one func x acc))
                        (collection->list m)
                        (collection->list b))))
        (collection-list-set! m new)
        m))

    (define (map-add-from m m1)
      ((get-constructor m) (append (collection->list m)
                                   (collection->list m1))))

    (define (map-add-from! m m1)
      (collection-list-set! m (append (collection->list m)
                                      (collection->list m1))))

    (define (list-case l k-nil k-pair)
      (cond ((null? l)
             (k-nil))
            ((pair? l)
             ;; Use CAR+CDR from SRFI 1, perhaps?
             ;; (call-with-values (lambda () (car+cdr l)) k-pair)
             (k-pair (car l) (cdr l)))
            (else
             (error "Not a list" l))))

    (define (list-fold-keys-left lst kons . knils)
      (let ((knil-count (length knils)))
        (let loop ((knils knils) (lst lst) (k 0))
          (list-case lst
                     (lambda () (apply values knils))
                     (lambda (elt1 elt2+)
                       (receive (proceed? . new-knils)
                           (apply kons k elt1 knils)
                         (cond ((not (= (length new-knils) knil-count))
                                (error "Wrong number of knils"
                                       list-fold-keys-left
                                       `(expected ,knil-count)
                                       `(got ,new-knils)))
                               (proceed?
                                (loop new-knils elt2+ (+ k 1)))
                               (else
                                (apply values new-knils)))))))))

    (define (list-fold-keys-right lst kons . knils)
      (let ((knil-count (length knils)))
        (call-with-values
            (lambda ()
              (let recur ((knils knils) (lst lst) (k 0))
                (list-case lst
                           (lambda () (apply values #t knils))
                           (lambda (elt1 elt2+)
                             (receive (proceed? . new-knils)
                                 (recur knils elt2+ (+ k 1))
                               (cond ((not (= (length new-knils) knil-count))
                                      (error "Wrong number of knils"
                                             list-fold-keys-right
                                             `(expected ,knil-count)
                                             `(got ,new-knils)))
                                     (proceed?
                                      (apply kons k elt1 new-knils))
                                     (else
                                      (apply values #f new-knils))))))))
          (lambda (_proceed? . vals)
            (apply values vals)))))

    (define (_map-fold-keys-left lst kons . knils)
      (let ((knil-count (length knils)))
        (let loop ((knils knils) (lst lst) (k 0))
          (list-case lst
                     (lambda () (apply values knils))
                     (lambda (elt1 elt2+)
                       (receive (proceed? . new-knils)
                           (apply kons (car elt1) (cdr elt1) knils)
                         (cond ((not (= (length new-knils) knil-count))
                                (error "Wrong number of knils"
                                       _map-fold-keys-left
                                       `(expected ,knil-count)
                                       `(got ,new-knils)))
                               (proceed?
                                (loop new-knils elt2+ (+ k 1)))
                               (else
                                (apply values new-knils)))))))))

    (define (_map-fold-keys-right lst kons . knils)
      (let ((knil-count (length knils)))
        (call-with-values
            (lambda ()
              (let recur ((knils knils) (lst lst) (k 0))
                (list-case lst
                           (lambda () (apply values #t knils))
                           (lambda (elt1 elt2+)
                             (receive (proceed? . new-knils)
                                 (recur knils elt2+ (+ k 1))
                               (cond ((not (= (length new-knils) knil-count))
                                      (error "Wrong number of knils"
                                             _map-fold-keys-right
                                             `(expected ,knil-count)
                                             `(got ,new-knils)))
                                     (proceed?
                                      (apply kons (car elt1) (cdr elt1)
                                             new-knils))
                                     (else
                                      (apply values #f new-knils))))))))
          (lambda (_proceed? . vals)
            (apply values vals)))))

    (define (map-fold-keys-left m func seed . seeds)
      (apply _map-fold-keys-left (collection->list m) func seed seeds))

    (define (map-fold-keys-right m func seed . seeds)
      (apply _map-fold-keys-right (collection->list m) func seed seeds))

    ;; Sequence (inherit bag)

    ;; XXX: There is some overlap with directional-collection that can be
    ;; cleaned up.

    (define-record-type (<sequence> <bag>)
      (mk-sequence b)
      sequence?)

    (define (make-sequence)
      (mk-sequence (mk-bag (mk-collection 'sequence '()
                                          #f #f #f #f #t)
                           equal?)))

    (define (sequence . rest)
      (mk-sequence (mk-bag (mk-collection 'sequence rest
                                          #f #f #f #f #t)
                           equal?)))

    (define sequence-ref
      (case-lambda
       ((s i) (sequence-ref s i (lambda _
                                  (error "Sequence index of range" s i))))
       ((s i thk)
        (let ((lst (collection->list s)))
          (if (and (>= i 0) (< i (length lst)))
              (r7rs:list-ref lst i)
              (thk))))))

    ;; SLOW: These can be conses, not appends.

    (define (sequence-insert-right s val)
      ((get-constructor s) (append (collection->list s) (r7rs:list val))))

    (define (sequence-insert-right! s val)
      (collection-list-set! s (append (collection->list s) (r7rs:list val)))
      s)

    (define (sequence-set s idx val)
      (let ((lst (collection->list s)))
        ((get-constructor s) (append (take lst idx)
                                     (r7rs:list val)
                                     (drop lst (+ idx 1))))))

    (define (sequence-set! s idx val)
      (let ((lst (collection->list s)))
        (collection-list-set! s (append (take lst idx)
                                        (r7rs:list val)
                                        (drop lst (+ idx 1)))))
      s)

    (define sequence-replace-from
      (case-lambda
       ((s dest-start source-sequence)
        (sequence-replace-from s dest-start source-sequence 0
                               (collection-size source-sequence)))
       ((s dest-start source-sequence source-start)
        (sequence-replace-from s dest-start source-sequence source-start
                               (collection-size source-sequence)))
       ((s dest-start source-sequence source-start source-end)
        (let* ((lst (collection->list s))
               (src (collection->list source-sequence))
               (l (- (length lst) 1))
               (start (take lst dest-start))
               (middle (drop (take src (min l source-end)) source-start)))
          ((get-constructor s) (append start middle
                                       (drop lst (- (length lst)
                                                    (+ (length start)
                                                       (length middle))))))))))

    (define sequence-replace-from!
      (case-lambda
       ((s dest-start source-sequence)
        (sequence-replace-from! s dest-start source-sequence 0
                                (collection-size source-sequence)))
       ((s dest-start source-sequence source-start)
        (sequence-replace-from! s dest-start source-sequence source-start
                                (collection-size source-sequence)))
       ((s dest-start source-sequence source-start source-end)
        (let* ((lst (collection->list s))
               (src (collection->list source-sequence))
               (l (- (length lst) 1))
               (start (take lst dest-start))
               (middle (drop (take src (min l source-end)) source-start)))
          (collection-list-set! s (append start middle
                                          (drop lst (- (length lst)
                                                       (+ (length start)
                                                          (length middle)))))))
        s)))

    (define (sequence-fold-keys-left col func seed . seeds)
      (apply list-fold-keys-left (collection->list col) func seed seeds))

    (define (sequence-fold-keys-right col func seed . seeds)
      (apply list-fold-keys-right (collection->list col) func seed seeds))

    ;; Flexible sequence (inherit sequence)

    (define-record-type (<flexible-sequence> <sequence>)
      (mk-flexible-sequence seq)
      flexible-sequence?)

    (define (make-flexible-sequence)
      (mk-flexible-sequence
       (mk-sequence (mk-bag (mk-collection 'flexible-sequence '()
                                           #f #f #f #f #t)
                            equal?))))

    (define (flexible-sequence . rest)
      (mk-flexible-sequence
       (mk-sequence (mk-bag (mk-collection 'flexible-sequence rest
                                           #f #f #f #f #t)
                            equal?))))

    (define (flexible-sequence-insert fs idx val)
      ((get-constructor fs)
       (let ((lst (collection->list fs)))
         (append (take lst idx)
                 (r7rs:list val)
                 (drop lst idx)))))

    (define (flexible-sequence-insert! fs idx val)
      (collection-list-set!
       fs
       (let ((lst (collection->list fs)))
         (append (take lst idx)
                 (r7rs:list val)
                 (drop lst idx)))))

    (define (flexible-sequence-delete-at fs idx)
      ((get-constructor fs)
       (let ((lst (collection->list fs)))
         (append (take lst idx)
                 (drop lst (+ idx 1))))))

    (define (flexible-sequence-delete-at! fs idx)
      (collection-list-set!
       fs
       (let ((lst (collection->list fs)))
         (append (take lst idx)
                 (drop lst (+ idx 1)))))
      fs)

    ;; Vector (inherit sequence)

    (define-record-type (<vector> <sequence>)
      (mk-vector seq)
      vector?)

    (define make-vector
      (case-lambda
       ((size) (make-vector size #f))
       ((size default)
        (mk-vector (mk-sequence (mk-bag (mk-collection 'vector
                                                       (r7rs:map (lambda _
                                                                   default)
                                                                 (iota size))
                                                       #t #f #t #f #f)
                                        equal?))))))

    (define (vector . rest)
      (mk-vector (mk-sequence (mk-bag (mk-collection 'vector rest
                                                     #t #f #t #f #f)
                                      equal?))))

    ;; List (inherit flexible sequence)

    (define-record-type (<list> <flexible-sequence>)
      (mk-list fseq)
      list?)

    (define make-list
      (case-lambda
       (() (make-list 0 #f))
       ((size) (make-list size #f))
       ((size default)
        (mk-list (mk-flexible-sequence
                  (mk-sequence
                   (mk-bag (mk-collection 'list
                                          (r7rs:map (lambda _
                                                      default)
                                                    (iota size))
                                          #t #f #f #f #t)
                           equal?)))))))

    (define (list . rest)
      (mk-list (mk-flexible-sequence
                (mk-sequence
                 (mk-bag (mk-collection 'list rest #t #f #f #f #t)
                         equal?)))))

    ;; String (inherit sequence)

    (define-record-type (<string> <sequence>)
      (mk-string seq)
      string?)

    (define make-string
      (case-lambda
       ((size) (make-string size #\null))
       ((size default)
        (unless (char? default)
          (error "String contents must be a character" default))
        (mk-string (mk-sequence
                    (mk-bag (mk-collection 'string
                                           (r7rs:map (lambda _
                                                       default)
                                                     (iota size))
                                           #t #f #t #f #f)
                            char=?))))))

    (define (string . rest)
      (unless (every char? rest)
        (error "String contents must be a character" rest))
      (mk-string (mk-sequence
                  (mk-bag (mk-collection 'string rest #t #f #t #f #f)
                          char=?))))

    ;; alist-map (inherit map)

    (define-record-type (<alist-map> <map>)
      (mk-alist-map mp)
      alist-map?)

    (define (make-alist-map equiv)
      (mk-alist-map (mk-map (mk-collection 'alist-map '() #f #f #f #f #f)
                            equiv equiv)))

    (define (alist-map equiv . rest)
      (unless (every pair? rest)
        (error "alist-map must be initialized with pairs" rest))
      (mk-alist-map (mk-map (mk-collection 'alist-map rest #f #f #f #f #f)
                            equiv equiv)))

    (define (alist-map-delete-all am k)
      ((get-constructor am)
       (filter (lambda (x)
                 (not ((map-key-equivalence-function am) (car x) x)))
               (collection->list am))))

    (define (alist-map-delete-all! am k)
      (collection-list-set!
       am
       (filter (lambda (x)
                 (not ((map-key-equivalence-function am) (car x) x)))
               (collection->list am)))
      am)

    (define (alist-map-delete-all-from am b)
      ((get-constructor am)
       (filter (lambda (x)
                 (not (member (car x) (collection->list b)
                              (map-equivalence-function am))))
               (collection->list am))))

    (define (alist-map-delete-all-from! am b)
      (collection-list-set!
       am
       (filter (lambda (x)
                 (not (member (car x) (collection->list b)
                              (map-equivalence-function am))))
               (collection->list am)))
      am)

    (define (alist-map-get-all am k)
      (r7rs:map cdr
                (filter (lambda (x)
                          ((map-key-equivalence-function am) (car x) k))
                        (collection->list am))))

    (define (alist-map-key-count am k)
      (fold (lambda (x acc)
              (if ((map-key-equivalence-function am) (car x) k)
                  (+ 1 acc)
                  acc))
            0
            (collection->list am)))

    (define (alist-map-replace-all am k val)
      ((get-constructor am)
       (r7rs:map (lambda (x)
                   (if ((map-key-equivalence-function am) (car x) k)
                       (cons k val)
                       x))
                 (collection->list am))))

    (define (alist-map-replace-all! am k val)
      (collection-list-set!
       am
       (r7rs:map (lambda (x)
                   (if ((map-key-equivalence-function am) (car x) k)
                       (cons k val)
                       x))
                 (collection->list am)))
      am)

    (define alist-map-update-all
      (case-lambda
       ((am k func) (alist-map-update-all am k func (lambda _ #f)))
       ((am k func thk)
        ;; NOTE: I have no idea why thunk is needed here.
        ((get-constructor am)
         (r7rs:map (lambda (x)
                     (if ((map-key-equivalence-function am) (car x) k)
                         (cons k (func (cdr x)))
                         x))
                   (collection->list am))))))

    (define alist-map-update-all!
      (case-lambda
       ((am k func) (alist-map-update-all! am k func (lambda _ #f)))
       ((am k func thk)
        (collection-list-set!
         am
         (r7rs:map (lambda (x)
                     (if ((map-key-equivalence-function am) (car x) k)
                         (cons k (func (cdr x)))
                         x))
                   (collection->list am)))
        am)))
