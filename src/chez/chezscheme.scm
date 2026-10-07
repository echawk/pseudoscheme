;;; -*- Mode: Scheme -*-
;;;; The first part of the body of Pseudoscheme's (chezscheme) library:
;;;; the Chez Scheme extensions the Chez variants of Akku packages
;;;; (foo.chezscheme.sls) use.  The library form around this and the
;;;; other src/chez/*.scm files, which re-exports R6RS as Chez's does, is
;;;; built by src/chez/chez.lisp; host procedures come in with a %
;;;; prefix.  docs/chez.md says what is and isn't here.

;; Chez's parameters: called with a value, they set it
(define make-parameter
  (case-lambda ((init) (%chez:make-parameter init))
               ((init converter) (%chez:make-parameter init converter))))
(define (void) (if #f #f))
(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))
(define call/1cc call/cc)
(define gensym
  (case-lambda (() (%chez:make-gensym "g")) ((name) (%chez:make-gensym name))))
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

;; Boxes, the host's (#&x reads as one)
(define box %chez:box)
(define box? %chez:box?)
(define unbox %chez:unbox)
(define set-box! %chez:set-box!)
(define box-cas! %chez:box-cas!)

;; format, printf, fprintf, errorf, warningf: src/chez/conditions.scm

(define (pretty-print x . port)
  (let ((p (if (pair? port) (car port) (current-output-port))))
    (write x p)
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

(define (time->nanoseconds t) (+ (* (time-second t) 1000000000) (time-nanosecond t)))
(define (nanoseconds->time type ns)
  (make-time type (mod ns 1000000000) (div ns 1000000000)))
(define (copy-time t) (make-time (time-type t) (time-nanosecond t) (time-second t)))
(define (time-difference a b)
  (nanoseconds->time 'time-duration (- (time->nanoseconds a) (time->nanoseconds b))))
(define (time-difference! a b) (time-difference a b))
(define (add-duration t d)
  (nanoseconds->time (time-type t) (+ (time->nanoseconds t) (time->nanoseconds d))))
(define (add-duration! t d) (add-duration t d))
(define (subtract-duration t d)
  (nanoseconds->time (time-type t) (- (time->nanoseconds t) (time->nanoseconds d))))
(define (subtract-duration! t d) (subtract-duration t d))
(define (time=? a b) (= (time->nanoseconds a) (time->nanoseconds b)))
(define (time<? a b) (< (time->nanoseconds a) (time->nanoseconds b)))
(define (time<=? a b) (<= (time->nanoseconds a) (time->nanoseconds b)))
(define (time>? a b) (> (time->nanoseconds a) (time->nanoseconds b)))
(define (time>=? a b) (>= (time->nanoseconds a) (time->nanoseconds b)))

(define (date->time-utc d)
  ;; days from the civil date (Howard Hinnant's algorithm)
  (let* ((y (if (<= (date-month d) 2) (- (date-year d) 1) (date-year d)))
         (era (div y 400))
         (yoe (- y (* era 400)))
         (m (date-month d))
         (doy (+ (div (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (- (date-day d) 1)))
         (doe (+ (* yoe 365) (div yoe 4) (- (div yoe 100)) doy))
         (days (+ (* era 146097) doe -719468)))
    (make-time 'time-utc (date-nanosecond d)
               (- (+ (* days 86400) (* 3600 (date-hour d)) (* 60 (date-minute d)) (date-second d))
                  (date-zone-offset d)))))

(define (date-week-day d)
  (mod (+ 4 (div (time-second (date->time-utc d)) 86400)) 7))

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
