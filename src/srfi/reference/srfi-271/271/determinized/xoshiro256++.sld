;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271 determinized xoshiro256++)
  (export make-random-port
          random-port?
          random-port-state
          random-port-state?
          random-port-state=?
          random-port-initialization-error?
          )
  (import (scheme base)
          (scheme bytevector)
          (scheme case-lambda)
          (scheme write)
          (only (srfi 1) every fold)
          (srfi 151)
          (gauche base)
          (gauche keyword)
          (gauche vport)
          (prefix (srfi 271 randomized) r:)
          )
  (begin
    ;;; xoshiro256++ implementation transcribed from Wikipedia's
    ;;; C version (based on Vigna).

    ;;; WARNING: This is very much a sample implementation.  Please do
    ;;; not use this code in any application where security is a
    ;;; concern.  While I will try to fix any bugs uncovered in this
    ;;; implementation as they are revealed, I still request that you
    ;;; use something field-tested instead, preferably written and
    ;;; audited by competent numerical programmers.  My understanding
    ;;; of the arcana of pseudorandom number generation is minimal.

    (define state-number-of-bytes 32)

    (define BE (endianness big))

    (define mask (- (expt 2 64) 1))

    (define (+/mask a b)
      (bitwise-and (+ a b) mask))

    (define (ashift/mask n c)
      (bitwise-and (arithmetic-shift n c) mask))

    (define (rol64 x k)
      (bitwise-ior (ashift/mask x k)
                   (ashift/mask x (- k 64))))

    ;; For paranoia's sake, state values are always accessed from
    ;; 'state', even when two accesses are guaranteed to give the
    ;; same value.  (An earlier version of this procedure produced
    ;; *very* predictable results due to the accidental reuse of
    ;; s2 and s3 state values.)
    (define (xoshiro! state)
      (let-syntax ((get-s0
                    (syntax-rules ()
                      ((_)
                       (bytevector-u64-ref state 0 BE))))
                   (set-s0!
                    (syntax-rules ()
                      ((_ k)
                       (bytevector-u64-set! state 0 k BE))))
                   (get-s1
                    (syntax-rules ()
                      ((_)
                       (bytevector-u64-ref state 8 BE))))
                   (set-s1!
                    (syntax-rules ()
                      ((_ k)
                       (bytevector-u64-set! state 8 k BE))))
                   (get-s2
                    (syntax-rules ()
                      ((_)
                       (bytevector-u64-ref state 16 BE))))
                   (set-s2!
                    (syntax-rules ()
                      ((_ k)
                       (bytevector-u64-set! state 16 k BE))))
                   (get-s3
                    (syntax-rules ()
                      ((_)
                      (bytevector-u64-ref state 24 BE))))
                   (set-s3!
                    (syntax-rules ()
                      ((_ k)
                       (bytevector-u64-set! state 24 k BE)))))
        (let ((result (+/mask (rol64 (+/mask (get-s0) (get-s3)) 23)
                              (get-s0)))
              (t (ashift/mask (get-s1) 17)))
            (set-s2! (bitwise-xor (get-s2) (get-s0)))
            (set-s3! (bitwise-xor (get-s3) (get-s1)))
            (set-s1! (bitwise-xor (get-s1) (get-s2)))
            (set-s0! (bitwise-xor (get-s0) (get-s3)))
            (set-s2! (bitwise-xor (get-s2) t))
            (set-s3! (rol64 (get-s3) 45))
            result)))

    ;; Wrapper to get bytes out of the xoshiro generator.
    ;;
    ;; Note: Taking a big random number modulo the width of the desired
    ;; interval can skew the range.  I believe this is safe, since the
    ;; width of the [0, 2^64) interval of *state* is a multiple of the
    ;; width of the [0, 2^8) range.  (Please check me on this.)
    (define (xoshiro-bytes! state)
      (remainder (xoshiro! state) #x100))

    ;;; Init errors

    (define-condition-type &random-port-init
     &error
     random-port-initialization-error?)

    (define (random-port-initialization-error msg)
      (raise-continuable
       (condition
        (&message (message msg))
        (&random-port-init))))

    ;;; xoshiro state manipulation

    (define (random-port-state? x)
      (and (bytevector? x)
           (= state-number-of-bytes (bytevector-length x))
           (not (every zero? (bytevector->u8-list x)))))

    (define (make-state-from-port port)
      (let ((bvec (read-bytevector state-number-of-bytes port)))
        (when (or (eof-object? bvec)
                  (< (bytevector-length bvec) state-number-of-bytes))
          (random-port-initialization-error
           "couldn't read enough data to initialize port"))
        bvec))

    (define (random-port-state=? state . rest-states)
      (when (null? rest-states)
        (error "invalid number of state arguments"))
      (let ((check-state
             (lambda (x)
               (unless (random-port-state? x)
                 (error "invalid argument" x)))))
        (check-state state)
        (for-each check-state rest-states)
        (every (lambda (st) (equal? state st)) rest-states)))

    ;;; xoshiro warmup

    ;;; To improve the quality of xoshiro output, we execute a number
    ;;; of "warmup" cycles (reads) on a port until its state is nicely
    ;;; scrambled, i.e. it has approximately the same number of 1 and
    ;;; 0 bits.  At least *minimum-warmup-cycles* cycles are run.  If
    ;;; we can't get a scrambled state after *maximum-warmup-cycles*
    ;;; reads, we give up and signal an init. error.

    ;; Returns the total number of 1 bits in *state*'s elements.
    (define (xoshiro-state-bit-count state)
      (fold (lambda (k s) (+ s (bit-count k)))
            0
            (bytevector->u8-list state)))

    ;; True if the binary representation of *state* has an
    ;; approximately even distribution of 1s and 0s.
    (define (xoshiro-state-scrambled? state)
      (let ((ratio (/ (xoshiro-state-bit-count state)
                      (* state-number-of-bytes 8))))
        (< 0.48 ratio 0.52)))

    (define minimum-warmup-cycles 8)

    (define maximum-warmup-cycles 1024)

    (define (random-port-warmup! port)
      (letrec*
       ((c 0)
        (warmup!
         (lambda ()
           (cond ((and (>= c minimum-warmup-cycles)
                       (xoshiro-state-scrambled?
                        (random-port-state port))))
                 ((>= c maximum-warmup-cycles)
                  (random-port-initialization-error
                   "couldn't obtain a valid state"))
                 (else
                  (set! c (+ c 1))
                  (read-u8 port)
                  (warmup!))))))
        (warmup!)
        port))

    (define (make-xoshiro-random-port init)
      (make <random-port>
            :state init
            :getb (lambda () (xoshiro-bytes! init))))

    (define make-random-port
      (case-lambda
        (()
         (call-with-port (r:make-random-port) make-random-port))
        ((initializer)
         (let* ((init
                 (cond ((and (input-port? initializer)
                             (binary-port? initializer))
                        (make-state-from-port initializer))
                       ((random-port-state? initializer)
                        (bytevector-copy initializer))
                       (else
                        (error "invalid initializer" initializer))))
                (port (make-xoshiro-random-port init)))
           ;; Assume a state argument is valid.  We can't run warmup
           ;; cycles in this case without violating the "same state,
           ;; same sequence" rule.
           (unless (random-port-state? initializer)
             (random-port-warmup! port))
           port))))

    ;;; Gauche virtual port type

    (define-class <random-port> (<virtual-input-port>)
      ((state :getter random-port-state :init-keyword :state)))

    (define (random-port? x)
      (is-a? x <random-port>))

    ))
