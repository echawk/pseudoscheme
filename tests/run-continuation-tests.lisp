;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;;; Full continuations (src/continuations.lisp): re-entry, multi-shot
;;;; continuations, dynamic-wind, and the frame-aware map, for-each and
;;;; call-with-values.  Each case is evaluated at the R7RS REPL with
;;;; full continuations on, and its written value compared.
;;;;
;;;; Usage:  sbcl --dynamic-space-size 4GB --control-stack-size 500MB \
;;;;              --script tests/run-continuation-tests.lisp [-v]

(require :asdf)

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))

(defun load-system (system)
  (if (find-package "QL")
      (uiop:symbol-call "QL" "QUICKLOAD" system :silent t)
      (asdf:load-system system)))

(let* ((here (make-pathname :name nil :type nil
			    :defaults (or *load-truename* *load-pathname*)))
       (root (merge-pathnames (make-pathname :directory '(:relative :up)) here)))
  (pushnew (truename root) asdf:*central-registry* :test #'equal))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load-system :pseudoscheme/api)))

(defparameter *verbose* (member "-v" (uiop:command-line-arguments) :test #'string=))

(r7rs:eval "(+ 1 2)")			; boot
(setq psx::*full-continuations* t)

;;; (expression . expected written value)
(defparameter *tests* '(
;; delimited continuations (src/control.sls): aborts, composable
;; continuations called more than once, shift/reset, dynamic-wind
;; through an abort and back, escapes, nested tags, constant stack;
;; each a program, so that its import is in effect
("(import (scheme base) (pseudoscheme control)) (call-with-prompt 'foo (lambda () (+ 1 (abort-to-prompt 'foo 41))) (lambda (k v) (+ v 1)))" . "42")
("(import (scheme base) (pseudoscheme control)) (let ((k (call-with-prompt 'foo (lambda () (+ 34 (abort-to-prompt 'foo))) (lambda (k) k)))) (list (k 10) (k 20)))" . "(44 54)")
("(import (scheme base) (pseudoscheme control)) (list (reset (+ 1 (shift k (k (k 10))))) (+ 1 (reset (* 2 (shift k (k (k 3)))))) (reset (+ 1 (shift k 42))))" . "(12 13 42)")
("(import (scheme base) (pseudoscheme control)) (let () (define (walk tree) (reset (let w ((t tree)) (cond ((null? t) 'done) ((pair? t) (w (car t)) (w (cdr t))) (else (shift k (cons t k))))) 'done)) (let loop ((r (walk '((1 2) (3 (4 5)) 6))) (acc '())) (if (pair? r) (loop ((cdr r) #f) (cons (car r) acc)) (reverse acc))))" . "(1 2 3 4 5 6)")
("(import (scheme base) (pseudoscheme control)) (let* ((log '()) (note (lambda (x) (set! log (cons x log)))) (k (call-with-prompt 'p (lambda () (dynamic-wind (lambda () (note 'in)) (lambda () (abort-to-prompt 'p) (note 'body) 7) (lambda () (note 'out)))) (lambda (k) k)))) (list (k) (reverse log)))" . "(7 (in out in body out))")
("(import (scheme base) (pseudoscheme control)) (list (let/ec out (+ 1 (out 99))) (call-with-prompt 'a (lambda () (call-with-prompt 'b (lambda () (abort-to-prompt 'a 1)) (lambda (k v) 'b))) (lambda (k v) (list 'a v))))" . "(99 (a 1))")
("(import (scheme base) (pseudoscheme control)) (let ((k (reset (let loop ((i 0)) (if (= i 100000) i (begin (shift k k) (loop (+ i 1)))))))) (let run ((k k)) (if (procedure? k) (run (k #f)) k)))" . "100000")
("(import (scheme base) (pseudoscheme control)) (guard (e (#t 'no-prompt)) (abort-to-prompt (make-prompt-tag) 1))" . "no-prompt")
("(let ((path '()) (c #f)) (let ((add (lambda (s) (set! path (cons s path))))) (dynamic-wind (lambda () (add 'connect)) (lambda () (add (call-with-current-continuation (lambda (c0) (set! c c0) 'talk1)))) (lambda () (add 'disconnect))) (if (< (length path) 4) (c 'talk2) (reverse path))))" . "(connect talk1 disconnect connect talk2 disconnect)")
("(let ((k #f) (n 0)) (let ((r (+ 1 (call/cc (lambda (c) (set! k c) 1))))) (set! n (+ n 1)) (if (< n 3) (k r) (list r n))))" . "(4 3)")
("(let ((r '())) (define (gen) (call/cc (lambda (ret) (for-each (lambda (x) (call/cc (lambda (resume) (set! gen (lambda () (resume #f))) (ret x)))) '(1 2 3)) (ret 'done)))) (let loop ((v (gen))) (if (eq? v 'done) (reverse r) (begin (set! r (cons v r)) (loop (gen))))))" . "(1 2 3)")
("(call/cc (lambda (k) (+ 1 (k 42))))" . "42")
("(map (lambda (x) (* x x)) '(1 2 3))" . "(1 4 9)")
("(call-with-values (lambda () (values 1 2)) +)" . "3")
("(guard (e (#t (list 'caught e))) (raise 'oops))" . "(caught oops)")
("(let ((k #f) (xs '())) (let ((x (call/cc (lambda (c) (set! k c) 0)))) (set! xs (cons (lambda () x) xs)) (if (< x 3) (k (+ x 1)) (map (lambda (f) (f)) xs))))" . "(3 2 1 0)")
("(let loop ((i 0) (acc '())) (if (= i 5) (reverse acc) (loop (+ i 1) (cons (call/cc (lambda (k) (k (* i i)))) acc))))" . "(0 1 4 9 16)")
("(let ((n 0)) (for-each (lambda (x) (set! n (+ n x))) '(1 2 3)) n)" . "6")
("(call/cc (lambda (ret) (for-each (lambda (x) (ret x)) '(1 2 3)) 'done))" . "1")
("(let ((k2 #f) (n 0)) (for-each (lambda (x) (call/cc (lambda (k) (if (= x 2) (set! k2 k))))) '(1 2 3)) (set! n (+ n 1)) (if (< n 3) (k2 #f) n))" . "3")
("(let ((k #f) (n 0)) (let ((l (map (lambda (x) (call/cc (lambda (c) (if (= x 2) (set! k c)) x))) '(1 2 3)))) (set! n (+ n 1)) (if (= n 1) (k 20) (list n l))))" . "(2 (1 20 3))")
("(let ((x 1) (k #f)) (let ((v (call/cc (lambda (c) (set! k c) 0)))) (set! x (+ x 1)) (if (< v 2) (k (+ v 1)) x)))" . "4")
("(let ((acc '()) (k #f)) (dynamic-wind (lambda () (set! acc (cons 'in acc))) (lambda () (call/cc (lambda (c) (set! k c))) (set! acc (cons 'body acc))) (lambda () (set! acc (cons 'out acc)))) (if (< (length acc) 6) (k #f) (reverse acc)))" . "(in body out in body out)")
;; re-entry through vector-map, string-for-each and map over two lists
("(let ((k #f) (n 0)) (let ((v (vector-map (lambda (x) (call/cc (lambda (c) (if (= x 2) (set! k c)) x))) #(1 2 3)))) (set! n (+ n 1)) (if (= n 1) (k 20) (list n v))))" . "(2 #(1 20 3))")
("(let ((k #f) (acc '())) (string-for-each (lambda (c) (call/cc (lambda (r) (if (char=? c #\\b) (set! k r)))) (set! acc (cons c acc))) \"abc\") (if (< (length acc) 5) (k #f) (list->string (reverse acc))))" . "\"abcbc\"")
("(let ((k #f) (n 0)) (let ((l (map (lambda (x y) (call/cc (lambda (c) (if (= x 2) (set! k c)) (+ x y)))) '(1 2 3) '(10 20 30)))) (set! n (+ n 1)) (if (= n 1) (k 0) (list n l))))" . "(2 (11 0 33))")
;; re-entering through a Lisp procedure without a frame-aware version is an error
("(let ((k #f) (n 0)) (guard (e (#t 'barrier)) (let ((r (list-sort (lambda (a b) (call/cc (lambda (c) (if (not k) (set! k c)))) (< a b)) '(3 1 2)))) (set! n (+ n 1)) (if (< n 2) (k #t) r))))" . "barrier")
;; a continuation captured inside eval includes the frames outside it
("(let ((n 0)) (let ((r (eval '(call/cc (lambda (c) (cons 1 c))) (environment '(scheme base))))) (set! n (+ n 1)) (if (= n 1) ((cdr r) (cons 10 #f)) (list n (car r)))))" . "(2 10)")
;; continuations only called during their extent (escape-only catches),
;; also after re-entering a continuation captured inside one
("(call/cc (lambda (return) (for-each (lambda (x) (if (> x 2) (return x))) '(1 2 3 4)) 'none))" . "3")
("(let loop ((i 0)) (if (= i 3) (call/cc (lambda (k) (let inner ((j 0)) (if (= j 5) (k j) (inner (+ j 1)))))) (loop (+ i 1))))" . "5")
("(let ((k #f) (n 0)) (let ((r (call/cc (lambda (return) (for-each (lambda (x) (call/cc (lambda (c) (if (= x 2) (set! k c)))) (if (= x 3) (return (list 'early x)))) '(1 2 3 4)) 'done)))) (set! n (+ n 1)) (if (< n 3) (k #f) (list n r))))" . "(3 (early 3))")
("(let () (define (addc x y k) (if (zero? y) (k x) (addc (+ x 1) (- y 1) k))) (define (fibc x c) (if (zero? x) (c 0) (if (zero? (- x 1)) (c 1) (addc (call/cc (lambda (c) (fibc (- x 1) c))) (call/cc (lambda (c) (fibc (- x 2) c))) c)))) (fibc 15 (lambda (n) n)))" . "610")
;; an assigned letrec-bound variable in a procedure with sites is boxed
("(let () (define (g h) (letrec ((x 1) (bump (lambda () (set! x (+ x 1)) x))) (h) (bump) (bump))) (g (lambda () 0)))" . "3")
;; a loop calling an unknown procedure, re-entered mid-loop (its tail
;; calls to itself are jumps)
("(let () (define k #f) (define n 0) (define (count-up i stop f acc) (if (= i stop) (reverse acc) (count-up (+ i 1) stop f (cons (f i) acc)))) (define r (count-up 0 5 (lambda (i) (if (= i 2) (call/cc (lambda (c) (set! k c) i)) i)) '())) (set! n (+ n 1)) (if (< n 3) (k (* n 100)) (list n r)))" . "(3 (0 1 200 3 4))")
;; a generator walking a tree: resumed from different depths, so captures
;; share frames promoted by earlier ones
;; re-entering within a dynamic-wind runs neither its after nor its before
("(let ((acc '()) (k #f) (n 0)) (dynamic-wind (lambda () (set! acc (cons 'in acc))) (lambda () (call/cc (lambda (c) (set! k c))) (set! n (+ n 1)) (if (< n 3) (k #f))) (lambda () (set! acc (cons 'out acc)))) (list n (reverse acc)))" . "(3 (in out))")
;; ... and runs only the before of the one it enters
("(let ((acc '()) (k #f) (n 0)) (dynamic-wind (lambda () (set! acc (cons 'a acc))) (lambda () (dynamic-wind (lambda () (set! acc (cons 'b acc))) (lambda () (call/cc (lambda (c) (set! k c)))) (lambda () (set! acc (cons 'c acc)))) (set! n (+ n 1)) (if (< n 2) (k #f))) (lambda () (set! acc (cons 'd acc)))) (reverse acc))" . "(a b c b c d)")
;; parameterize is re-established on re-entry
("(let ((p (make-parameter 1)) (k #f) (log '())) (parameterize ((p 2)) (call/cc (lambda (c) (set! k c))) (set! log (cons (p) log))) (set! log (cons (p) log)) (if (< (length log) 4) (k #f) (reverse log)))" . "(2 1 2 1)")
;; guard re-raises in the dynamic environment of the raise
("(let ((v '())) (guard (e ((eq? e 5) (list 'five (reverse v)))) (guard (e ((eq? e 6) 'six)) (dynamic-wind (lambda () (set! v (cons 'in v))) (lambda () (raise 5)) (lambda () (set! v (cons 'out v)))))))" . "(five (in out in out))")
;; a known procedure calling an unknown one: its calls are sites
("(let () (define (twice f) (+ (f) (f))) (define k #f) (define n 0) (define r (twice (lambda () (call/cc (lambda (c) (if (not k) (set! k c)) 1))))) (set! n (+ n 1)) (if (< n 3) (k 10) (list n r)))" . "(3 11)")
;; mutually recursive known procedures, one of which captures
("(let () (define k #f) (define (a n) (if (= n 0) (call/cc (lambda (c) (set! k c) 0)) (+ 1 (b (- n 1))))) (define (b n) (+ 1 (a n))) (define count 0) (define r (a 2)) (set! count (+ count 1)) (if (< count 2) (k 100) (list count r)))" . "(2 104)")
;; a procedure assigned after its definition isn't known
("(let () (define (f) 1) (define k (begin (set! f (lambda () (call/cc (lambda (c) (set! k c) 1)))) #f)) (define n 0) (define r (+ 1 (f))) (set! n (+ n 1)) (if (< n 2) (k 5) (list n r)))" . "(2 6)")
("(let () (define (walk tree yield) (cond ((null? tree) #f) ((pair? tree) (walk (car tree) yield) (walk (cdr tree) yield)) (else (yield tree)))) (define (make-gen tree) (define return #f) (define resume #f) (define (yield v) (call/cc (lambda (r) (set! resume r) (return v)))) (lambda () (call/cc (lambda (ret) (set! return ret) (if resume (resume #f) (begin (walk tree yield) (return 'done))))))) (let ((g (make-gen '((1 2) (3 (4 5)) 6)))) (let loop ((acc '())) (let ((v (g))) (if (eq? v 'done) (reverse acc) (loop (cons v acc)))))))" . "(1 2 3 4 5 6)")
;; a continuation captured in a handler, re-entered after an escape from
;; it, raises again there (the handler frame kept its handlers)
("(let ((c1 #f) (n 0)) (let ((r (call/cc (lambda (out) (with-exception-handler (lambda (obj) (call/cc (lambda (c) (set! c1 c) (out (list 'aborted obj))))) (lambda () (let* ((a (raise-continuable 'one)) (b (raise-continuable 'two))) (list a b)))))))) (set! n (+ n 1)) (if (< n 4) (c1 n) (list n r))))" . "(4 (1 3))")
;; thirty sites in one body compile in linear time
("(let ((g (car (list (lambda (m) m)))) (s 0)) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) (set! s (+ s (g 1))) s)" . "30")
))

;; a program big enough to be evaluated as many top-level forms: a
;; continuation re-entered in one goes on to the rest
(setq *tests*
      (append *tests*
	      (list (cons (format nil "(import (scheme base)) (define (f) (call/cc (lambda (return) (lambda () (return (lambda () 'escaped)))))) (define (g) (let ((thunk (f))) (thunk))) (define count 0) (define r (g)) ~{~A ~}(list r count)"
				  (loop repeat 1000 collect "(set! count (+ count 1))"))
			  "(escaped 1000)"))))

(let ((pass 0))
  (dolist (tc *tests*)
    (let ((got (handler-case (r7rs:write-to-string (r7rs:eval (car tc)))
		 (error (e) (format nil "ERROR: ~A" (remove #\Newline (princ-to-string e)))))))
      (cond ((string= got (cdr tc))
	     (incf pass)
	     (when *verbose* (format t "ok   ~A~%" (car tc))))
	    (t (format t "FAIL ~A~%  expected ~A~%  got      ~A~%" (car tc) (cdr tc) got)))))
  (format t "~&Continuations: ~D of ~D tests passed.~%" pass (length *tests*)))
