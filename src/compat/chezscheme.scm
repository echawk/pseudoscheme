;;; -*- Mode: Scheme -*-
;;;; The body of Pseudoscheme's (chezscheme) library: Chez Scheme
;;;; extensions that portable-in-practice libraries use, mostly through
;;;; the Chez variants (foo.chezscheme.sls) of Akku packages.  The library
;;;; form around this, which re-exports R6RS as Chez's does, is built by
;;;; src/r7rs/front.lisp; host procedures come in with a % prefix.
;;;;
;;;; Only what libraries actually use is here (see ROADMAP.md for how the
;;;; list was found).  No FFI, no ftypes, no guardians.

(define (void) (if #f #f))
(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))
(define call/1cc call/cc)
(define gensym %gensym)
(define getenv %get-environment-variable)
(define system %chez:system)
(define with-input-from-string %chez:with-input-from-string)
(define with-output-to-string %chez:with-output-to-string)
(define current-directory %chez:current-directory)
(define file-directory? %chez:file-directory?)
(define file-regular? %chez:file-regular?)
(define file-symbolic-link? %chez:file-symbolic-link?)
(define directory-list %chez:directory-list)
(define mkdir %chez:mkdir)
(define delete-directory %chez:delete-directory)
(define rename-file %chez:rename-file)
(define file-modification-time %chez:file-modification-time)
(define (directory-separator) #\/)
(define machine-type %chez:machine-type)
(define get-mode %chez:get-mode)
(define chmod %chez:chmod)
(define file-change-time %chez:file-change-time)
(define library-directories %chez:library-directories)
(define (source-directories) '("."))
(define (record-writer . args) (if #f #f))
(define (collect . args) (if #f #f))
(define open-input-string %open-input-string)
(define open-output-string %open-output-string)
(define get-output-string %get-output-string)
(define (last-pair l) (if (pair? (cdr l)) (last-pair (cdr l)) l))
(define (fxquotient a b) (quotient a b))
(define (fxremainder a b) (remainder a b))
(define (fxmodulo a b) (modulo a b))
(define (fx1+ n) (fx+ n 1))
(define (fx1- n) (fx- n 1))
(define fxlogand fxand)
(define fxlogor fxior)
(define fxlogxor fxxor)
(define (fxsll n k) (fxarithmetic-shift-left n k))
(define (fxsra n k) (fxarithmetic-shift-right n k))
(define (list-head l k) (if (= k 0) '() (cons (car l) (list-head (cdr l) (- k 1)))))
(define (remq! x l) (remq x l))
(define (remv! x l) (remv x l))
(define (remove! x l) (remove x l))
(define (string-copy! src s-start dst d-start n)   ; Chez's argument order
  (do ((i 0 (+ i 1))) ((= i n)) (string-set! dst (+ d-start i) (string-ref src (+ s-start i)))))
(define make-list %make-list)
(define (fxabs n) (if (fx<? n 0) (fx- 0 n) n))

;; (with-implicit (k id ...) body ...): bind each id to an identifier
;; named id in the context of k.
(define-syntax with-implicit
  (syntax-rules ()
    ((_ (k id ...) body1 body2 ...)
     (with-syntax ((id (datum->syntax #'k 'id)) ...) body1 body2 ...))))

(define weak-cons cons)
(define weak-pair? pair?)
(define (bwp-object? x) #f)

(define (iota n . rest)
  (let ((start (if (pair? rest) (car rest) 0))
        (step (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) 1)))
    (let loop ((i (- n 1)) (acc '()))
      (if (< i 0) acc (loop (- i 1) (cons (+ start (* i step)) acc))))))

;; Boxes
(define-record-type (box-type box box?) (fields (mutable contents unbox set-box!)))

;; format: (format #f fmt arg ...) => string, (format #t ...) to the
;; current output port, (format port ...), and Chez's (format fmt arg ...).
;; Directives: ~a ~s ~w ~d ~b ~o ~x ~c ~% ~n ~~, and ~<newline> to skip
;; to the next non-blank.
(define (format-to port fmt args)
  (let ((n (string-length fmt)))
    (let loop ((i 0) (args args))
      (when (< i n)
        (let ((c (string-ref fmt i)))
          (if (and (char=? c #\~) (< (+ i 1) n))
              (let ((d (char-downcase (string-ref fmt (+ i 1)))))
                (case d
                  ((#\a) (display (car args) port) (loop (+ i 2) (cdr args)))
                  ((#\s #\w) (write (car args) port) (loop (+ i 2) (cdr args)))
                  ((#\d) (display (number->string (car args) 10) port) (loop (+ i 2) (cdr args)))
                  ((#\b) (display (number->string (car args) 2) port) (loop (+ i 2) (cdr args)))
                  ((#\o) (display (number->string (car args) 8) port) (loop (+ i 2) (cdr args)))
                  ((#\x) (display (number->string (car args) 16) port) (loop (+ i 2) (cdr args)))
                  ((#\c) (write-char (car args) port) (loop (+ i 2) (cdr args)))
                  ((#\% #\n) (newline port) (loop (+ i 2) args))
                  ((#\~) (write-char #\~ port) (loop (+ i 2) args))
                  ((#\newline)
                   (let skip ((j (+ i 2)))
                     (if (and (< j n) (char-whitespace? (string-ref fmt j)))
                         (skip (+ j 1))
                         (loop j args))))
                  (else (write-char c port) (loop (+ i 1) args))))
              (begin (write-char c port) (loop (+ i 1) args))))))))

(define (format dest . rest)
  (cond ((string? dest)
         (call-with-string-output-port (lambda (p) (format-to p dest rest))))
        ((eq? dest #f)
         (call-with-string-output-port (lambda (p) (format-to p (car rest) (cdr rest)))))
        ((eq? dest #t) (format-to (current-output-port) (car rest) (cdr rest)))
        (else (format-to dest (car rest) (cdr rest)))))

(define (printf fmt . args) (format-to (current-output-port) fmt args))
(define (fprintf port fmt . args) (format-to port fmt args))

(define (pretty-print x . port)
  (let ((p (if (pair? port) (car port) (current-output-port))))
    (write x p)
    (newline p)))

(define (errorf who fmt . args)
  (error who (call-with-string-output-port (lambda (p) (format-to p fmt args)))))
(define (assertion-violationf who fmt . args)
  (assertion-violation who (call-with-string-output-port (lambda (p) (format-to p fmt args)))))
(define (warningf who fmt . args)
  (let ((p (current-error-port)))
    (display "Warning" p)
    (when who (display " in " p) (display who p))
    (display ": " p)
    (format-to p fmt args)
    (newline p)))

(define-syntax fluid-let
  (syntax-rules ()
    ((_ () b1 b2 ...) (let () b1 b2 ...))
    ((_ ((x v) ...) b1 b2 ...)
     (let ((new (list v ...)) (old #f))
       (dynamic-wind
        (lambda () (set! old (list x ...)) (fluid-let-assign (x ...) new))
        (lambda () b1 b2 ...)
        (lambda () (set! new (list x ...)) (fluid-let-assign (x ...) old)))))))

(define-syntax fluid-let-assign
  (syntax-rules ()
    ((_ () vals) (if #f #f))
    ((_ (x y ...) vals) (begin (set! x (car vals)) (fluid-let-assign (y ...) (cdr vals))))))

(define-syntax time
  (syntax-rules ()
    ((_ e)
     (let ((start (%current-jiffy)))
       (call-with-values (lambda () e)
         (lambda vals
           (let ((p (current-error-port)))
             (display "    " p)
             (display (/ (inexact (- (%current-jiffy) start)) (%jiffies-per-second)) p)
             (display "s elapsed time\n" p))
           (apply values vals)))))))

;;; Time (Chez's time and date objects, the subset libraries use: chez-srfi's
;;; SRFI 19 compat layer is built on these).  CPU time is approximated by
;;; real time since start-up; there is no GC time.

(define-record-type (time-type* make-time time?)
  (fields (mutable type time-type set-time-type!)
          (mutable nanosecond time-nanosecond set-time-nanosecond!)
          (mutable second time-second set-time-second!)))

(define (seconds->time type s)
  (let* ((whole (exact (floor s)))
         (ns (exact (round (* (- s whole) 1000000000)))))
    (make-time type ns whole)))

(define current-time
  (case-lambda
    (() (current-time 'time-utc))
    ((type)
     (case type
       ((time-utc time-monotonic) (seconds->time type (%current-second)))
       ((time-process time-thread) (seconds->time type (/ (%current-jiffy) (%jiffies-per-second))))
       ((time-collector-cpu time-collector-real) (make-time type 0 0))
       (else (assertion-violation 'current-time "unknown time type" type))))))

(define (cpu-time) (exact (round (/ (* 1000 (%current-jiffy)) (%jiffies-per-second)))))
(define (real-time) (cpu-time))

(define-record-type (sstats make-sstats sstats?)
  (fields cpu real bytes gc-count gc-cpu gc-real gc-bytes))
(define (statistics)
  (let ((t (current-time 'time-process)))
    (make-sstats t t 0 0 (make-time 'time-duration 0 0) (make-time 'time-duration 0 0) 0)))

(define-record-type (date-type* make-date date?)
  (fields nanosecond second minute hour day month year zone-offset)
  (nongenerative))
(define (date-nanosecond d) (date-type*-nanosecond d))
(define (date-second d) (date-type*-second d))
(define (date-minute d) (date-type*-minute d))
(define (date-hour d) (date-type*-hour d))
(define (date-day d) (date-type*-day d))
(define (date-month d) (date-type*-month d))
(define (date-year d) (date-type*-year d))
(define (date-zone-offset d) (date-type*-zone-offset d))

(define (time-utc->date t . offset)
  ;; days-from-civil inverted (Howard Hinnant's algorithm)
  (let* ((off (if (pair? offset) (car offset) (%chez:timezone-offset)))
         (secs (+ (time-second t) off))
         (days (floor (/ secs 86400)))
         (rem (- secs (* days 86400)))
         (z (+ days 719468))
         (era (floor (/ z 146097)))
         (doe (- z (* era 146097)))
         (yoe (floor (/ (- doe (floor (/ doe 1460)) (- (floor (/ doe 36524))) (floor (/ doe 146096))) 365)))
         (doy (- doe (- (+ (* 365 yoe) (floor (/ yoe 4))) (floor (/ yoe 100)))))
         (mp (floor (/ (+ (* 5 doy) 2) 153)))
         (d (+ (- doy (floor (/ (+ (* 153 mp) 2) 5))) 1))
         (m (if (< mp 10) (+ mp 3) (- mp 9)))
         (y (+ yoe (* era 400) (if (<= m 2) 1 0))))
    (make-date (time-nanosecond t) (mod rem 60) (mod (div rem 60) 60) (div rem 3600)
               d m y off)))
(define (current-date . offset) (apply time-utc->date (current-time) offset))
