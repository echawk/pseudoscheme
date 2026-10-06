; SPDX-FileCopyrightText: 2025 Daphne Preston-Kendal
;
; SPDX-License-Identifier: MIT

;; -*- eldoc-documentation-function: eldoc-documentation-default -*-
;; scheme-complete eldoc is bizarrely slow in this buffer
(begin ; meze only supports one expression per file

(define *nice-n-buckets*
  '#(2 2 3 5 5 7 7 11 11 13 13 17 17 19 19 23 23 23 23 29 29 31 31 31 31
       37 37 41 41 43 43 47 47 47 47 53 53 53 53 59 59 61 61 61 61 67 67
       71 71 73 73 73 73 79 79 83 83 83 83 89 89 89 89 89 89 97 97 101
       101 103 103 107 107 109 109 113 113 113 113 113 113 113 113 113
       113 127 127 131 131 131 131 137 137 139 139 139 139 139 139 149
       149 151 151 151 151 157 157 157 157 163 163 167 167 167 167 173
       173 173 173 179 179 181 181 181 181 181 181 191 191 193 193 197
       197 199 199 199 199 199 199 199 199 211 211 211 211 211 211 211
       211 223 223 227 227 229 229 233 233 233 233 239 239 241 241 241
       241 241 241 251 251 251 251 257 257 257 257 263 263 263 263 269
       269 271 271 271 271 277 277 281 281 283 283 283 283 283 283 293
       293 293 293 293 293 293 293 293 293 307 307 311 311 313 313 317
       317 317 317 317 317 317 317 317 317 331 331 331 331 337 337 337
       337 337 337 347 347 349 349 353 353 353 353 359 359 359 359 359
       359 367 367 367 367 373 373 373 373 379 379 383 383 383 383 389
       389 389 389 389 389 397 397 401 401 401 401 401 401 409 409 409
       409 409 409 419 419 421 421 421 421 421 421 431 431 433 433 433
       433 439 439 443 443 443 443 449 449 449 449 449 449 457 457 461
       461 463 463 467 467 467 467 467 467 467 467 479 479 479 479 479
       479 487 487 491 491 491 491 491 491 499 499 503 503 503 503 509
       509 509 509 509 509 509 509 521 521 523 523 523 523 523 523 523
       523 523 523 523 523 541 541 541 541 547 547 547 547 547 547 557
       557 557 557 563 563 563 563 569 569 571 571 571 571 577 577 577
       577 577 577 587 587 587 587 593 593 593 593 599 599 601 601 601
       601 607 607 607 607 613 613 617 617 619 619 619 619 619 619 619
       619 631 631 631 631 631 631 641 641 643 643 647 647 647 647 653
       653 653 653 659 659 661 661 661 661 661 661 661 661 673 673 677
       677 677 677 683 683 683 683 683 683 691 691 691 691 691 691 701
       701 701 701 701 701 709 709 709 709 709 709 719 719 719 719 719
       719 727 727 727 727 733 733 733 733 739 739 743 743 743 743 743
       743 751 751 751 751 757 757 761 761 761 761 761 761 769 769 773
       773 773 773 773 773 773 773 773 773 787 787 787 787 787 787 797
       797 797 797 797 797 797 797 809 809 811 811 811 811 811 811 821
       821 823 823 827 827 829 829 829 829 829 829 839 839 839 839 839
       839 839 839 839 839 853 853 857 857 859 859 863 863 863 863 863
       863 863 863 863 863 877 877 881 881 883 883 887 887 887 887 887
       887 887 887 887 887 887 887 887 887 907 907 911 911 911 911 911
       911 919 919 919 919 919 919 929 929 929 929 929 929 937 937 941
       941 941 941 947 947 947 947 953 953 953 953 953 953 953 953 953
       953 967 967 971 971 971 971 977 977 977 977 983 983 983 983 983
       983 991 991 991 991 997 997 997 997 997 997 997 997))

(define (find-nice-n-buckets k)
  (if (< k (vector-length *nice-n-buckets*))
      (vector-ref *nice-n-buckets* k)
      (let ((limit (+ 1 (floor (* k 3/2)))))
        ;; todo: put a little bit more effort into this
        (if (even? limit)
            (+ 1 limit)
            limit))))

;; the actual hash table bit: fundamental constructor
(define make-hash-table
  (case-lambda
    ((comparator) (make-hash-table comparator *default-k*))
    ((comparator k)
     (unless (and (comparator? comparator) (comparator-hashable? comparator))
       (assertion-violation 'make-hash-table
                            "not a hashable comparator"
                            comparator))
     (unless (and (integer? k) (exact? k) (not (negative? k)))
       (assertion-violation 'make-hash-table
                            "size is not an exact nonnegative integer"
                            k))
     (let ((k* (max k 1))
           (n-buckets (find-nice-n-buckets k)))
       (%make-hash-table (comparator-type-test-predicate comparator)
                         (comparator-hash-function comparator)
                         (comparator-equality-predicate comparator)
                         0
                         0
                         (make-compact-array n-buckets)
                         (floor (* n-buckets 2/3))
                         (make-vector k* *unfilled*)
                         (make-vector k* *unfilled*)
                         #t)))))

;; construct and pre-fill
(define (prefilled-hash-table comparator . keys-values)
  (define keys-values-length (length keys-values))
  (unless (even? keys-values-length)
    (assertion-violation 'hash-table
                         "there must be as many keys as values"
                         keys-values))
  (let ((ht (make-hash-table comparator (/ keys-values-length 2))))
    (apply hash-table-set! ht keys-values)
    ht))

(define alist->hash-table
  (case-lambda
    ((alist comparator)
     (alist->hash-table alist comparator (length alist)))
    ((alist comparator k)
     (let ((ht (make-hash-table comparator k)))
       (for-each (lambda (entry)
                   (let ((key (car entry))
                         (value (cdr entry)))
                     (hash-table-set! ht key value)))
                 (reverse alist))
       ht))))

(define (hash-table-empty-copy ht)
  (make-hash-table (hash-table-comparator ht)
                   (hash-table-size ht)))

;; internal helpers
(define (hash-table-hash ht key)
  (hash-truncate ((hash-table-hash-function ht) key)))

(define (hash-table-same? ht a b)
  ((hash-table-same?-function ht) a b))

(define (hash-table-right-type? ht obj)
  ((hash-table-type-test-function ht) obj))

(define (%hash-table-bucket-for ht hash key)
  (let ((n-buckets (compact-array-length (hash-table-compact-index ht))))
    (let loop ((hash hash))
      ;;(display hash) (newline)
      (let* ((bucket (modulo hash n-buckets))
             (entry-idx
              (compact-array-ref (hash-table-compact-index ht) bucket)))
        (if entry-idx
            (let ((found-key
                   (vector-ref (hash-table-keys-vector ht) entry-idx)))
              (if (and (not (deletion? found-key))
                       (hash-table-same? ht key found-key))
                  bucket
                  (loop (+ hash 1))))
            bucket)))))

(define (hash-table-bucket-for-key ht key)
  (%hash-table-bucket-for ht (hash-table-hash ht key) key))

(define (hash-table-grow-entries! ht)
  ;; first determine if we actually need to grow the entries arrays,
  ;; or if pruning dead entries would suffice
  (if (< (hash-table-size ht)
         (* (vector-length (hash-table-keys-vector ht))
            (/ 1 *growth-rate*)))
      (hash-table-prune-dead-entries! ht #f)
      ;; otherwise, actually grow the entries array
      (let* ((old-size (vector-length (hash-table-keys-vector ht)))
             (new-size (max (floor (* old-size *growth-rate*))
                            (+ old-size 1)))
             (new-keys (make-vector new-size *unfilled*))
             (new-values (make-vector new-size *unfilled*)))
        (vector-copy! new-keys 0 (hash-table-keys-vector ht))
        (vector-copy! new-values 0 (hash-table-values-vector ht))
        (hash-table-keys-vector-set! ht new-keys)
        (hash-table-values-vector-set! ht new-values)
        (hash-table-prune-dead-entries! ht #f))))

(define (hash-table-prune-dead-entries! ht fast?)
  ;; NB only set fast? to #t if you are going to be rehashing all
  ;; entries anyway!
  (unless (eqv? (hash-table-size ht) (hash-table-next-entry ht))
    (let loop ((from-idx 0)
               (to-idx 0))
      ;;(display from-idx) (newline) (display to-idx) (newline) (newline)
      (cond ((>= from-idx (hash-table-next-entry ht))
             (vector-fill! (hash-table-keys-vector ht)
                           *unfilled*
                           (hash-table-size ht)
                           (hash-table-next-entry ht))
             (vector-fill! (hash-table-values-vector ht)
                           *unfilled*
                           (hash-table-size ht)
                           (hash-table-next-entry ht))
             (hash-table-next-entry-set! ht (hash-table-size ht)))
            ((deletion? (vector-ref (hash-table-keys-vector ht) from-idx))
             (unless fast?
               (let ((deleted-bucket (vector-ref (hash-table-values-vector ht) from-idx)))
                 (compact-array-delete! (hash-table-compact-index ht)
                                        deleted-bucket)
                 (let loop ((deleted-bucket deleted-bucket)
                            (examine-bucket (+ deleted-bucket 1)))
                   (let ((examine-bucket
                          (modulo examine-bucket
                                  (compact-array-length
                                   (hash-table-compact-index ht)))))
                     (let ((collision-idx
                            (compact-array-ref (hash-table-compact-index ht)
                                               examine-bucket)))
                       (when collision-idx
                         (let* ((key (vector-ref (hash-table-keys-vector ht)
                                                 collision-idx))
                                (new-bucket
                                 (if (deletion? key)
                                     (vector-ref (hash-table-values-vector ht)
                                                 collision-idx)
                                     (hash-table-bucket-for-key ht key))))
                           (if (eqv? new-bucket deleted-bucket)
                               (begin
                                 (compact-array-set! (hash-table-compact-index ht)
                                                     deleted-bucket
                                                     collision-idx)
                                 (compact-array-delete! (hash-table-compact-index ht)
                                                        examine-bucket)
                                 (loop examine-bucket (+ examine-bucket 1)))
                               (loop deleted-bucket (+ examine-bucket 1))))))))))
             (loop (+ from-idx 1) to-idx))
            ((eqv? from-idx to-idx) (loop (+ from-idx 1) (+ to-idx 1)))
            (else
             (vector-set! (hash-table-keys-vector ht)
                          to-idx
                          (vector-ref (hash-table-keys-vector ht) from-idx))
             (vector-set! (hash-table-values-vector ht)
                          to-idx
                          (vector-ref (hash-table-values-vector ht) from-idx))
             (unless fast?
               (compact-array-set! (hash-table-compact-index ht)
                                   (hash-table-bucket-for-key
                                    ht
                                    (vector-ref (hash-table-keys-vector ht)
                                                to-idx))
                                   to-idx))
             (loop (+ from-idx 1) (+ to-idx 1)))))))

(define (hash-table-prune-dead-entries-at-end! ht)
  (let loop ((idx (- (hash-table-next-entry ht) 1)))
    (when (and (>= idx 0)
               (deletion? (vector-ref (hash-table-keys-vector ht) idx)))
      (compact-array-delete! (hash-table-compact-index ht)
                             (vector-ref (hash-table-values-vector ht) idx))
      (vector-set! (hash-table-keys-vector ht) idx *unfilled*)
      (vector-set! (hash-table-values-vector ht) idx *unfilled*)
      (hash-table-next-entry-set! ht idx)
      (loop (- idx 1)))))

(define (hash-table-grow-compact-index! ht)
  ;; this parameter isn't tunable: the compact index is always between
  ;; 1/2 and 2/3 full except when the table is brand new
  (let* ((new-size (ceiling (* (compact-array-length (hash-table-compact-index ht))
                               4/3)))
         (new-compact-index (make-compact-array new-size)))
    ;; take the opportunity to prune dead entries, since we have to
    ;; iterate all of them anyway
    (hash-table-prune-dead-entries! ht #t)
    (hash-table-compact-index-set! ht new-compact-index)
    (hash-table-compact-index-max-fill-set! ht (floor (* new-size 2/3)))
    (let loop ((idx 0))
      (unless (>= idx (vector-length (hash-table-keys-vector ht)))
        (let ((key (vector-ref (hash-table-keys-vector ht) idx)))
          (unless (unfilled? key)
            (compact-array-set! new-compact-index
                                (hash-table-bucket-for-key ht key)
                                idx)
            (loop (+ idx 1))))))))

;; #t if the hash table’s compact index has to grow to accommodate the
;; next association added
(define (hash-table-compact-index-must-grow? ht)
  (>= (hash-table-next-entry ht) (hash-table-compact-index-max-fill ht)))

;; add to the entries arrays, setting the bucket in the compact index
(define (hash-table-add-entry! ht bucket key value)
  (when (>= (hash-table-next-entry ht)
            (vector-length (hash-table-keys-vector ht)))
    (if (eqv? (hash-table-size ht) (hash-table-next-entry ht))
        (hash-table-grow-entries! ht)
        (begin
          (hash-table-grow-entries! ht)
          (set! bucket (hash-table-bucket-for-key ht key)))))
  (when (hash-table-compact-index-must-grow? ht)
    (hash-table-grow-compact-index! ht)
    (set! bucket (hash-table-bucket-for-key ht key)))
  (vector-set! (hash-table-keys-vector ht)
               (hash-table-next-entry ht)
               key)
  (vector-set! (hash-table-values-vector ht)
               (hash-table-next-entry ht)
               value)
  (compact-array-set! (hash-table-compact-index ht)
                      bucket
                      (hash-table-next-entry ht))
  (hash-table-size-set! ht (+ (hash-table-size ht) 1))
  (hash-table-next-entry-set! ht (+ (hash-table-next-entry ht) 1)))

;; basic public interface, getting and setting
(define hash-table-add!
  (case-lambda
    ((ht key value)
     (unless (hash-table-mutable? ht)
       (assertion-violation 'hash-table-add!
                            "hash table is immutable"
                            ht))
     (unless (hash-table-right-type? ht key)
       (assertion-violation 'hash-table-add!
                            "not the right type for a key in this hash table"
                            key ht))
     (let* ((bucket (hash-table-bucket-for-key ht key))
            (entry-idx (compact-array-ref (hash-table-compact-index ht)
                                          bucket)))
       (if entry-idx
           (assertion-violation 'hash-table-add!
                                "already an association for this key in this hash table"
                                key ht)
           (hash-table-add-entry! ht bucket key value))))
    ((ht) (void))
    ((ht . keys-values)
     (unless (even? (length keys-values))
       (assertion-violation 'hash-table-add!
                            "there must be as many keys as values"
                            (length keys-values) keys-values))
     (let loop ((key (car keys-values))
                (value (cadr keys-values))
                (more-keys-values (cddr keys-values)))
       (hash-table-add! ht key value)
       (unless (null? more-keys-values)
         (loop (car more-keys-values)
               (cadr more-keys-values)
               (cddr more-keys-values)))))))

(define hash-table-replace!
  (case-lambda
    ((ht key value)
     (unless (hash-table-mutable? ht)
       (assertion-violation 'hash-table-replace!
                            "hash table is immutable"
                            ht))
     (unless (hash-table-right-type? ht key)
       (assertion-violation 'hash-table-replace!
                            "not the right type for a key in this hash table"
                            key ht))
     (let* ((bucket (hash-table-bucket-for-key ht key))
            (entry-idx (compact-array-ref (hash-table-compact-index ht)
                                          bucket)))
       (if entry-idx
           (vector-set! (hash-table-values-vector ht) entry-idx value)
           (assertion-violation 'hash-table-replace!
                                "key not in table"
                                key ht))))
    ((ht) (void))
    ((ht . keys-values)
     (unless (even? (length keys-values))
       (assertion-violation 'hash-table-replace!
                            "there must be as many keys as values"
                            (length keys-values) keys-values))
     (let loop ((key (car keys-values))
                (value (cadr keys-values))
                (more-keys-values (cddr keys-values)))
       (hash-table-replace! ht key value)
       (unless (null? more-keys-values)
         (loop (car more-keys-values)
               (cadr more-keys-values)
               (cddr more-keys-values)))))))

(define hash-table-set!
  (case-lambda
    ((ht key value)
     (unless (hash-table-mutable? ht)
       (assertion-violation 'hash-table-set!
                            "hash table is immutable"
                            ht))
     (unless (hash-table-right-type? ht key)
       (assertion-violation 'hash-table-set!
                            "not the right type for a key in this hash table"
                            key ht))
     (let* ((bucket (hash-table-bucket-for-key ht key))
            (entry-idx (compact-array-ref (hash-table-compact-index ht)
                                          bucket)))
       (if entry-idx
           (vector-set! (hash-table-values-vector ht) entry-idx value)
           (hash-table-add-entry! ht bucket key value))))
    ((ht) (void))
    ((ht . keys-values)
     (unless (even? (length keys-values))
       (assertion-violation 'hash-table-set!
                            "there must be as many keys as values"
                            (length keys-values) keys-values))
     (let loop ((key (car keys-values))
                (value (cadr keys-values))
                (more-keys-values (cddr keys-values)))
       (hash-table-set! ht key value)
       (unless (null? more-keys-values)
         (loop (car more-keys-values)
               (cadr more-keys-values)
               (cddr more-keys-values)))))))

(define hash-table-ref
  (case-lambda
    ((ht key)
     (hash-table-ref ht key
                     (lambda () (assertion-violation 'hash-table-ref
                                                     "key not in table"
                                                     key ht))
                     values))
    ((ht key failure)
     (hash-table-ref ht key failure values))
    ((ht key failure success)
     (unless (hash-table-right-type? ht key)
       (assertion-violation 'hash-table-ref
                            "not the right type for a key in this hash table"
                            key ht))
     (let* ((bucket (hash-table-bucket-for-key ht key))
            (entry-idx (compact-array-ref (hash-table-compact-index ht)
                                          bucket)))
       (if entry-idx
           (success (vector-ref (hash-table-values-vector ht) entry-idx))
           (failure))))))

(define (hash-table-ref/default ht key default)
  (hash-table-ref ht key (lambda () default)))

(define (hash-table-delete! ht . keys)
  (unless (hash-table-mutable? ht)
    (assertion-violation 'hash-table-delete!
                         "hash table is immutable"
                         ht))
  (let loop ((n-deleted 0)
             (more-keys keys))
    (if (null? more-keys)
        (begin
          (hash-table-size-set! ht (- (hash-table-size ht) n-deleted))
          (when (> (- (hash-table-next-entry ht) (hash-table-size ht))
                   (* 1/3 (hash-table-size ht)))
            (hash-table-prune-dead-entries! ht #f))
          n-deleted)
        (if (hash-table-delete-one! ht (car more-keys))
            (loop (+ n-deleted 1) (cdr more-keys))
            (loop n-deleted (cdr more-keys))))))

(define (hash-table-delete-one! ht key)
  (unless (hash-table-right-type? ht key)
    (assertion-violation 'hash-table-delete!
                         "not the right type for a key in this hash table"
                         key ht))
  (let* ((bucket (hash-table-bucket-for-key ht key))
         (entry-idx (compact-array-ref (hash-table-compact-index ht)
                                       bucket)))
    (if entry-idx
        (begin
          (vector-set! (hash-table-keys-vector ht)
                       entry-idx *deletion*)
          (vector-set! (hash-table-values-vector ht)
                       entry-idx bucket)
          (when (eqv? entry-idx (- (hash-table-next-entry ht) 1))
            (hash-table-prune-dead-entries-at-end! ht))
          #t)
        #f)))

(define (hash-table-pop! ht)
  (unless (hash-table-mutable? ht)
    (assertion-violation 'hash-table-pop!
                         "hash table is immutable"
                         ht))
  (when (hash-table-empty? ht)
    (assertion-violation 'hash-table-pop!
                         "hash table is already empty"
                         ht))
  (let* ((idx (- (hash-table-next-entry ht) 1))
         (key (vector-ref (hash-table-keys-vector ht) idx))
         (value (vector-ref (hash-table-values-vector ht) idx))
         (bucket (hash-table-bucket-for-key ht key)))
    (vector-set! (hash-table-keys-vector ht) idx *deletion*)
    (vector-set! (hash-table-values-vector ht) idx bucket)
    (hash-table-size-set! ht (- (hash-table-size ht) 1))
    (hash-table-prune-dead-entries-at-end! ht)
    (values key value)))

(define (hash-table-clear! ht)
  (unless (hash-table-mutable? ht)
    (assertion-violation 'hash-table-clear!
                         "hash table is immutable"
                         ht))
  ;; assumes the hash table is going to be refilled with approximately
  ;; the same number of associations as were previously in it
  (compact-array-clear! (hash-table-compact-index ht))
  (vector-fill! (hash-table-keys-vector ht) *unfilled*)
  (vector-fill! (hash-table-values-vector ht) *unfilled*)
  (hash-table-size-set! ht 0)
  (hash-table-next-entry-set! ht 0))

(define (hash-table-contains? ht key)
  (unless (hash-table-right-type? ht key)
    (assertion-violation 'hash-table-contains?
                         "not the right type for a key in this hash table"
                         key ht))
  (let* ((bucket (hash-table-bucket-for-key ht key))
         (entry-idx (compact-array-ref (hash-table-compact-index ht)
                                       bucket)))
    (not (not entry-idx))))

(define (hash-table-empty? ht) (zero? (hash-table-size ht)))
(define (hash-table-comparator ht)
  (make-comparator (hash-table-type-test-function ht)
                   (hash-table-same?-function ht)
                   #f
                   (hash-table-hash-function ht)))

;; cursor-based iteration
(define (hash-table-cursor-first ht)
  (hash-table-cursor-next ht -1))
(define (hash-table-cursor-last ht)
  (hash-table-cursor-previous ht (vector-length (hash-table-keys-vector ht))))

(define (hash-table-cursor-for-key ht key)
  (unless (hash-table-right-type? ht key)
    (assertion-violation 'hash-table-cursor-for-key
                         "not the right type for a key in this hash table"
                         key ht))
  (let* ((bucket (hash-table-bucket-for-key ht key))
         (entry-idx (compact-array-ref (hash-table-compact-index ht)
                                       bucket)))
    (if entry-idx
        entry-idx
        -1)))

(define (hash-table-cursor-next ht cur)
  (let loop ((n (+ cur 1)))
    (if (>= n (vector-length (hash-table-keys-vector ht)))
        n
        (let ((key (vector-ref (hash-table-keys-vector ht) n)))
          (cond ((unfilled? key) n)
                ((deletion? key) (loop (+ n 1)))
                (else n))))))
(define (hash-table-cursor-previous ht cur)
  (let loop ((n (- cur 1)))
    (if (< n 0)
        n
        (let ((key (vector-ref (hash-table-keys-vector ht) n)))
          (if (or (unfilled? key) (deletion? key))
              (loop (- n 1))
              n)))))

(define (hash-table-cursor-key ht cur)
  (vector-ref (hash-table-keys-vector ht) cur))
(define (hash-table-cursor-value ht cur)
  (vector-ref (hash-table-values-vector ht) cur))
(define (hash-table-cursor-key+value ht cur)
  (values (hash-table-cursor-key ht cur)
          (hash-table-cursor-value ht cur)))

(define (hash-table-cursor-value-set! ht cur val)
  (vector-set! (hash-table-values-vector ht) cur val))

(define (hash-table-cursor-at-end? ht cur)
  (or (negative? cur)
      (>= cur (vector-length (hash-table-keys-vector ht)))
      (unfilled? (vector-ref (hash-table-keys-vector ht) cur))))

;; for when cursor-based iteration is unsafe

(define (hash-table-keys ht)
  (if (hash-table-mutable? ht)
      (let* ((size (hash-table-size ht))
             (vec (make-vector size)))
        (let loop ((cur (hash-table-cursor-first ht))
                   (idx 0))
          (unless (>= idx size)
            (vector-set! vec idx (hash-table-cursor-key ht cur))
            (loop (hash-table-cursor-next ht cur)
                  (+ idx 1)))
          vec))
      (hash-table-keys-vector ht)))
(define (hash-table-values ht)
  (if (hash-table-mutable? ht)
      (let* ((size (hash-table-size ht))
             (vec (make-vector size)))
        (let loop ((cur (hash-table-cursor-first ht))
                   (idx 0))
          (unless (>= idx size)
            (vector-set! vec idx (hash-table-cursor-value ht cur))
            (loop (hash-table-cursor-next ht cur)
                  (+ idx 1)))
          vec))
      (hash-table-values-vector ht)))

(define (hash-table-entries ht)
  (values (hash-table-keys ht)
          (hash-table-values ht)))

(define (hash-table-fold proc seed ht)
  (let loop ((cur (hash-table-cursor-first ht))
             (acc seed))
    (if (hash-table-cursor-at-end? ht cur)
        acc
        (loop (hash-table-cursor-next ht cur)
              (proc (hash-table-cursor-key ht cur)
                    (hash-table-cursor-value ht cur)
                    acc)))))

(define (hash-table-fold-left proc seed ht)
  (let loop ((cur (hash-table-cursor-first ht))
             (acc seed))
    (if (hash-table-cursor-at-end? ht cur)
        acc
        (loop (hash-table-cursor-next ht cur)
              (proc acc
                    (hash-table-cursor-key ht cur)
                    (hash-table-cursor-value ht cur))))))

(define (hash-table-fold-right proc seed ht)
  (let loop ((cur (hash-table-cursor-last ht))
             (acc seed))
    (if (hash-table-cursor-at-end? ht cur)
        acc
        (loop (hash-table-cursor-previous ht cur)
              (proc (hash-table-cursor-key ht cur)
                    (hash-table-cursor-value ht cur)
                    acc)))))

(define (hash-table-for-each proc ht)
  (let loop ((cur (hash-table-cursor-first ht)))
    (unless (hash-table-cursor-at-end? ht cur)
      (call-with-values
          (lambda () (hash-table-cursor-key+value ht cur))
        proc)
      (loop (hash-table-cursor-next ht cur)))))

(define (hash-table-map proc ht)
  (let ((new-ht (hash-table-empty-copy ht)))
    (hash-table-for-each (lambda (k v)
                           (hash-table-add! new-ht k (proc k v)))
                         ht)
    new-ht))

(define (hash-table-map! proc ht)
  (unless (hash-table-mutable? ht)
    (assertion-violation 'hash-table-map!
                         "hash table is immutable"
                         ht))
  (let loop ((cur (hash-table-cursor-first ht)))
    (if (hash-table-cursor-at-end? ht cur)
        ht
        (let ((new-val
               (call-with-values
                   (lambda () (hash-table-cursor-key+value ht cur))
                 proc)))
          (hash-table-cursor-value-set! ht cur new-val)
          (loop (hash-table-cursor-next ht cur))))))

(define (hash-table-map->list proc ht)
  (hash-table-fold-right
   (lambda (k v l)
     (cons (proc k v) l))
   '() ht))

(define (hash-table->alist ht)
  (hash-table-fold-left
   (lambda (l k v)
     (cons (cons k v) l))
   '() ht))

(define (hash-table-find proc ht failure)
  (let loop ((cur (hash-table-cursor-first ht)))
    (cond ((hash-table-cursor-at-end? ht cur)
           (failure))
          ((call-with-values
               (lambda () (hash-table-cursor-key+value ht cur))
             proc))
          (else (loop (hash-table-cursor-next ht cur))))))

(define (hash-table-count pred ht)
  (hash-table-fold
   (lambda (k v acc)
     (if (pred k v)
         (+ acc 1)
         acc))
   0
   ht))

(define (hash-table-prune! proc ht)
  (define original-size (hash-table-size ht))
  (unless (hash-table-mutable? ht)
    (assertion-violation 'hash-table-prune!
                         "hash table is immutable"
                         ht))
  (let loop ((cur (hash-table-cursor-first ht)) (n-deleted 0))
    (if (hash-table-cursor-at-end? ht cur)
        (begin
          (hash-table-prune-dead-entries-at-end! ht)
          (when (> (- (hash-table-next-entry ht) original-size)
                   (* 1/3 (hash-table-size ht)))
            (hash-table-prune-dead-entries! ht #f))
          n-deleted)
        (let-values (((k v) (hash-table-cursor-key+value ht cur)))
          (if (and (proc k v)
                   (hash-table-delete-one! ht k))
              (begin
                (hash-table-size-set! ht (- (hash-table-size ht) 1))
                (loop (hash-table-cursor-next ht cur) (+ n-deleted 1)))
              (loop (hash-table-cursor-next ht cur) n-deleted))))))

(define (hash-table-copy ht mutable?)
  (hash-table-prune-dead-entries! ht #f)
  (if mutable?
      (%make-hash-table (hash-table-type-test-function ht)
                        (hash-table-hash-function ht)
                        (hash-table-same?-function ht)
                        (hash-table-size ht)
                        (hash-table-next-entry ht)
                        (compact-array-copy (hash-table-compact-index ht))
                        (hash-table-compact-index-max-fill ht)
                        (vector-copy (hash-table-keys-vector ht))
                        (vector-copy (hash-table-values-vector ht))
                        #t)
      (let ((out-ht (hash-table-empty-copy ht)))
        (hash-table-for-each (lambda (k v)
                               (hash-table-add! out-ht k v))
                             ht)
        (hash-table-immutablize! out-ht)
        out-ht)))

;; set-like operations
(define (hash-table-union! ht_1 ht_2)
  (hash-table-for-each
   (lambda (k v)
     (unless (hash-table-contains? ht_1 k)
       (hash-table-set! ht_1 k v)))
   ht_2)
  ht_1)

(define (hash-table-intersection! ht_1 ht_2)
  (hash-table-prune!
   (lambda (k v)
     (not (hash-table-contains? ht_2 k)))
   ht_1)
  ht_1)

(define (hash-table-difference! ht_1 ht_2)
  (hash-table-prune!
   (lambda (k v)
     (hash-table-contains? ht_2 k))
   ht_1)
  ht_1)

(define (hash-table-xor! ht_1 ht_2)
  (hash-table-for-each
   (lambda (k v)
     (if (hash-table-contains? ht_1 k)
         (hash-table-delete! ht_1 k)
         (hash-table-set! ht_1 k v)))
   ht_2)
  ht_1)

(define (hash-table= value=? ht_1 ht_2)
  (and
   ;; check every association in ht_1 has a corresponding association
   ;; in ht_2
   (let loop ((cur (hash-table-cursor-first ht_1)))
     (cond ((hash-table-cursor-at-end? ht_1 cur) #t)
           ((and (hash-table-contains? ht_2
                                       (hash-table-cursor-key ht_1 cur))
                 (value=? (hash-table-cursor-value ht_1 cur)
                          (hash-table-ref ht_2 (hash-table-cursor-key ht_1 cur))))
            (loop (hash-table-cursor-next ht_1 cur)))
           (else #f)))
   ;; also check there are no entries in ht_2 absent in ht_1
   (let loop ((cur (hash-table-cursor-first ht_2)))
     (cond ((hash-table-cursor-at-end? ht_2 cur) #t)
           ((hash-table-contains? ht_1
                                  (hash-table-cursor-key ht_2 cur))
            (loop (hash-table-cursor-next ht_1 cur)))
           (else #f)))))

;; public utility procedures, with implementations closely adapted
;; from Will Clinger’s original implementation

;; not continuation-safe :-/
(define hash-table-unfold
  (case-lambda
    ((stop? mapper successor seed comparator)
     (hash-table-unfold stop? mapper successor seed comparator *default-k*))
    ((stop? mapper successor seed comparator k)
     (let ((ht (make-hash-table comparator k)))
       (let loop ((seed seed))
         (if (stop? seed)
             ht
             (call-with-values
                 (lambda () (mapper seed))
               (lambda (key val)
                 (hash-table-set! ht key val)
                 (loop (successor seed))))))))))

(define (hash-table-intern! ht key failure)
  (unless (hash-table-mutable? ht)
    (assertion-violation 'hash-table-intern!
                         "hash table is immutable"
                         ht))
  (if (hash-table-contains? ht key)
      (hash-table-ref ht key)
      (let ((val (failure)))
        (hash-table-set! ht key val)
        val)))

(define (hash-table-update! ht key updater . rest)
  (unless (hash-table-mutable? ht)
    (assertion-violation 'hash-table-update!
                         "hash table is immutable"
                         ht))
  (hash-table-set! ht
                   key
                   (updater (apply hash-table-ref ht key rest))))

(define (hash-table-update!/default ht key updater default)
  (unless (hash-table-mutable? ht)
    (assertion-violation 'hash-table-update!/default
                         "hash table is immutable"
                         ht))
  (hash-table-set! ht key (updater (hash-table-ref/default ht key default))))

)
