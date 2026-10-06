;;; Tests for SRFI 89, from the examples in the SRFI document.  Keywords
;;; are written name: in parameter lists, as in the SRFI, and passed as
;;; #:name (Pseudoscheme has no SRFI 88; see 89.sld).
(import (scheme base) (scheme process-context) (scheme write)
        (srfi 64) (srfi 89))
(test-begin "srfi-89")

(define* (f a (b #f)) (list a b))
(test-equal '(1 #f) (f 1))
(test-equal '(1 2) (f 1 2))
(test-error #t (f 1 2 3))

(define* (g a (b a) (key: k (* a b))) (list a b k))
(test-equal '(3 3 9) (g 3))
(test-equal '(3 4 12) (g 3 4))
(test-error #t (g 3 4 #:key))
(test-equal '(3 4 5) (g 3 4 #:key 5))
(test-error #t (g 3 4 #:zoo 5))
(test-error #t (g 3 4 #:key 5 #:key 6))

(define* (h1 a (key: k #f) . r) (list a k r))
(test-equal '(7 #f ()) (h1 7))
(test-equal '(7 #f (8 9 10)) (h1 7 8 9 10))
(test-equal '(7 8 (9 10)) (h1 7 #:key 8 9 10))
(test-error #t (h1 7 #:key 8 #:zoo 9))

(define* (h2 (key: k #f) a . r) (list a k r))
(test-equal '(7 #f ()) (h2 7))
(test-equal '(7 #f (8 9 10)) (h2 7 8 9 10))
(test-equal '(9 8 (10)) (h2 #:key 8 9 10))
(test-error #t (h2 #:key 8 #:zoo 9))

;; #:name works in parameter lists too.
(define* (h3 (#:key k 0)) k)
(test-equal 0 (h3))
(test-equal 4 (h3 #:key 4))

;; Required named parameters.
(define* (req (width: w) (height: h 1)) (* w h))
(test-equal 6 (req #:width 3 #:height 2))
(test-equal 3 (req #:width 3))
(test-error #t (req #:height 2))

;; Plain parameter lists are just lambda.
(define* (plain a b . c) (list a b c))
(test-equal '(1 2 (3)) (plain 1 2 3))
(define* (only-rest . r) r)
(test-equal '(1 2) (only-rest 1 2))
(define* x 42)
(test-equal 42 x)
(test-equal '(1 2) ((lambda* (a (b 2)) (list a b)) 1))
(test-equal 15 ((lambda* ((a 1) (b 2) (c 3) (d 4) (e 5)) (+ a b c d e))))
(test-equal 6 ((lambda* ((a 1) (b 2) (c 3)) (+ a b c)) 1 2 3))

;; Defaults see earlier parameters, and are evaluated only when needed.
(let ((count 0))
  (define* (d (a (begin (set! count (+ count 1)) 1)) (b (+ a 1))) (list a b))
  (test-equal '(5 6) (d 5))
  (test-equal 0 count)
  (test-equal '(1 2) (d))
  (test-equal 1 count))

;; Hygiene: user variables named like the implementation's.
(define* (hyg $args (key: $key-values 1)) (list $args $key-values))
(test-equal '(a 1) (hyg 'a))
(test-equal '(a 2) (hyg 'a #:key 2))

;; The HTML example.
(define absent (list 'absent))
(define (element tag content . attributes)
  (list "<" tag attributes ">" content "</" tag ">"))
(define (attribute name value)
  (if (eq? value absent) '() (list " " name "=" (escape value))))
(define (escape value) value)
(define (make-html-styler tag)
  (lambda* ((id: id absent)
            (class: class absent)
            (title: title absent)
            (style: style absent)
            (dir: dir absent)
            (lang: lang absent)
            (onclick: onclick absent)
            . content)
    (element tag
             content
             (attribute "id" id)
             (attribute "class" class)
             (attribute "title" title)
             (attribute "style" style)
             (attribute "dir" dir)
             (attribute "lang" lang)
             (attribute "onclick" onclick))))
(define html-b (make-html-styler 'b))
(define html-big (make-html-styler 'big))
(define html-small (make-html-styler 'small))
(define html-i (make-html-styler 'i))
(define (html->string . args)
  (let ((port (open-output-string)))
    (let pr ((x args))
      (cond ((null? x))
            ((pair? x) (pr (car x)) (pr (cdr x)))
            ((vector? x) (pr (vector->list x)))
            (else (display x port))))
    (get-output-string port)))
(test-equal "<i id=water class=molecule><big>H</big><small>2</small><big>O</big></i>"
  (html->string (html-i #:class 'molecule
                        #:id 'water
                        (html-big "H")
                        (html-small "2")
                        (html-big "O"))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-89")
  (exit (if (zero? failures) 0 1)))
