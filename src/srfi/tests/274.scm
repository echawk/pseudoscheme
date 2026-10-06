;;; SRFI 274 tests: the SRFI's tests.scm (Peter McGoron; MIT licence),
;;; with its cond-expanded imports made plain (every sublibrary is
;;; available here), and its circular list literals ('#1=(1 2 . #1#))
;;; built at run time instead: Pseudoscheme's compiler loops on a
;;; circular quoted datum.
(import (except (scheme base) list-copy list->string list->vector)
        (scheme process-context)
        (srfi 64)
        (srfi 274 base)
        (srfi 274 41) (except (srfi 41) list->stream)
        (srfi 274 134) (except (srfi 134) list->ideque)
        (srfi 274 158) (except (srfi 158) list->generator)
        (srfi 274 160 base)
        (except (srfi 160 base)
                list->s8vector list->u8vector list->s16vector list->u16vector list->s32vector list->u32vector list->s64vector list->u64vector list->f32vector list->f64vector list->c64vector list->c128vector))

(test-begin "srfi-274")


(define (circular-list a b)  ; '#1=(a b . #1#)
  (let ((l (list a b))) (set-cdr! (cdr l) l) l))
(define clist (circular-list 1 2))

(test-group "scheme base"
  (test-group "list-copy"
    (test-equal "on proper list"
                '(1 2 3 4 5)
                (list-copy '(1 2 3 4 5)))
    (test-equal "on improper list"
                '(1 2 3 4 5 . 6)
                (list-copy '(1 2 3 4 5 . 6)))
    (test-equal "on object"
                10
                (list-copy 10))
    (test-equal "start on proper list"
                '(3 4 5)
                (list-copy '(1 2 3 4 5) 2))
    (test-equal "start and end on proper list, 1"
                '(3 4)
                (list-copy '(1 2 3 4 5) 2 4))
    (test-equal "start and end on proper list, 2"
                '(3 4 5)
                (list-copy '(1 2 3 4 5) 2 5))
    (test-equal "start on improper list"
                '(3 4 5 . 6)
                (list-copy '(1 2 3 4 5 . 6) 2))
    (test-equal "start and end on improper list"
                '(3 4 5)
                (list-copy '(1 2 3 4 5 . 6) 2 5))
    (test-equal "start is length"
                6
                (list-copy '(1 2 3 4 5 . 6) 5))
    (test-equal "start and end are length"
                '()
                (list-copy '(1 2 3 4 5 . 6) 5 5))
    (test-equal "start and end on circular list"
                '(2 1 2 1 2 1 2)
                (list-copy clist 1 8)))
  (test-group "list->string"
    (test-equal "on proper list"
                "asdfg"
                (list->string '(#\a #\s #\d #\f #\g)))
    (test-equal "start on proper list"
                "dfg"
                (list->string '(#\a #\s #\d #\f #\g) 2))
    (test-equal "start and end on proper list, 1"
                "df"
                (list->string '(#\a #\s #\d #\f #\g) 2 4))
    (test-equal "start and end on proper list"
                "dfg"
                (list->string '(#\a #\s #\d #\f #\g) 2 5))
    (test-equal "start and end on improper list"
                "dfg"
                (list->string '(#\a #\s #\d #\f #\g . #\h) 2 5))
    (test-equal "start is length"
                ""
                (list->string '(#\a #\b #\c #\d #\e) 5))
    (test-equal "start and end are length"
                ""
                (list->string '(#\a #\b #\c #\d #\e) 5 5))
    (test-equal "start and end on circular list"
                "2121212"
                (list->string (circular-list #\1 #\2) 1 8)))
  (test-group "list->vector"
    (test-equal "on proper list"
                '#(1 2 3 4 5)
                (list->vector '(1 2 3 4 5)))
    (test-equal "start on proper list"
                '#(3 4 5)
                (list->vector '(1 2 3 4 5) 2))
    (test-equal "start and end on proper list, 1"
                '#(3 4)
                (list->vector '(1 2 3 4 5) 2 4))
    (test-equal "start and end on proper list, 2"
                '#(3 4 5)
                (list->vector '(1 2 3 4 5) 2 5))
    (test-equal "start and end on improper list"
                '#(3 4 5)
                (list->vector '(1 2 3 4 5 . 6) 2 5))
    (test-equal "start is length"
                '#()
                (list->vector '(1 2 3 4 5) 5))
    (test-equal "start and end are length"
                '#()
                (list->vector '(1 2 3 4 5) 5 5))
    (test-equal "start and end on circular list"
                '#(2 1 2 1 2 1 2)
                (list->vector clist 1 8))))

(define (test-to-from list->* *->list)
  (test-equal "start and end on proper list, 1"
              '(3 4)
              (*->list (list->* '(1 2 3 4 5) 2 4)))
  (test-equal "start and end on proper list, 2"
              '(3 4 5)
              (*->list (list->* '(1 2 3 4 5) 2 5)))
  (test-equal "start and end on improper list"
              '(3 4 5)
              (*->list
               (list->* '(1 2 3 4 5 . 6) 2 5)))
  (test-equal "start is length"
              '()
              (*->list
               (list->* '(1 2 3 4 5) 5)))
  (test-equal "start and end are length"
              '()
              (*->list
               (list->* '(1 2 3 4 5) 5 5)))
  (test-equal "start and end on circular list"
              '(2 1 2 1 2 1 2)
              (*->list
               (list->* clist 1 8))))

(define (test-to-from/floats list->* *->list)
  (test-equal "start and end on proper list, 1"
              '(3.0 4.0)
              (*->list (list->* '(1.0 2.0 3.0 4.0 5.0) 2 4)))
  (test-equal "start and end on proper list, 2"
              '(3.0 4.0 5.0)
              (*->list (list->* '(1.0 2.0 3.0 4.0 5.0) 2 5)))
  (test-equal "start and end on improper list"
              '(3.0 4.0 5.0)
              (*->list
               (list->* '(1.0 2.0 3.0 4.0 5.0 . 6) 2 5)))
  (test-equal "start is length"
              '()
              (*->list (list->* '(1.0 2.0 3.0 4.0 5.0) 5)))
  (test-equal "start and end are length"
              '()
              (*->list (list->* '(1.0 2.0 3.0 4.0 5.0) 5 5)))
  (test-equal "start and end on circular list"
              '(2.0 1.0 2.0 1.0 2.0 1.0 2.0)
              (*->list
               (list->* (circular-list 1.0 2.0)
                        1 8))))

(define (test-to-from/cplx list->* *->list)
  (test-equal "start and end on proper list, 1"
              '(3.0+3.0i 4.0+4.0i)
              (*->list (list->* '(1.0+1.0i 2.0+2.0i 3.0+3.0i 4.0+4.0i 5.0+5.0i) 2 4)))
  (test-equal "start and end on proper list, 2"
              '(3.0+3.0i 4.0+4.0i 5.0+5.0i)
              (*->list (list->* '(1.0+1.0i 2.0+2.0i 3.0+3.0i 4.0+4.0i 5.0+5.0i) 2 5)))
  (test-equal "start and end on improper list"
              '(3.0+3.0i 4.0+4.0i 5.0+5.0i)
              (*->list
               (list->* '(1.0+1.0i 2.0+2.0i 3.0+3.0i 4.0+4.0i 5.0+5.0i . 6) 2 5)))
  (test-equal "start is length"
              '()
              (*->list (list->* '(1.0+1.0i 2.0+2.0i 3.0+3.0i 4.0+4.0i 5.0+5.0i) 5)))
  (test-equal "start and end are length"
              '()
              (*->list (list->* '(1.0+1.0i 2.0+2.0i 3.0+3.0i 4.0+4.0i 5.0+5.0i) 5 5)))
  (test-equal "start and end on circular list"
              '(2.0+2.0i 1.0+1.0i 2.0+2.0i 1.0+1.0i 2.0+2.0i 1.0+1.0i 2.0+2.0i)
              (*->list
               (list->* (circular-list 1.0+1.0i 2.0+2.0i)
                        1 8))))

(cond-expand
  ((library (srfi 41))
   (test-group "srfi 41"
     (test-to-from list->stream stream->list)))
  (else))

(cond-expand
  ((library (srfi 134))
   (test-group "srfi 134"
     (test-to-from list->ideque ideque->list)))
  (else))

(cond-expand
  ((library (srfi 158))
   (test-group "srfi 158"
     (test-to-from list->generator generator->list))))

(cond-expand
  ((library (srfi 160 base))
   (test-group "srfi 160"
     (test-group "s8vector"
       (test-to-from list->s8vector s8vector->list))
     (test-group "u8vector"
       (test-to-from list->u8vector u8vector->list))
     (test-group "s16vector"
       (test-to-from list->s16vector s16vector->list))
     (test-group "u16vector"
       (test-to-from list->u16vector u16vector->list))
     (test-group "s32vector"
       (test-to-from list->s32vector s32vector->list))
     (test-group "u32vector"
       (test-to-from list->u32vector u32vector->list))
     (test-group "s64vector"
       (test-to-from list->s64vector s64vector->list))
     (test-group "u64vector"
       (test-to-from list->u64vector u64vector->list))
     (test-group "f32vector"
       (test-to-from/floats list->f32vector f32vector->list))
     (test-group "f64vector"
       (test-to-from/floats list->f64vector f64vector->list))
     (test-group "c64vector"
       (test-to-from/cplx list->c64vector c64vector->list))
     (test-group "c128vector"
       (test-to-from/cplx list->c128vector c128vector->list))))
  (else))



(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-274")
  (exit (if (zero? failures) 0 1)))
