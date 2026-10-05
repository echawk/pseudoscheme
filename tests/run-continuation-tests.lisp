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
;; a generator walking a tree: resumed from different depths, so captures
;; share frames promoted by earlier ones
("(let () (define (walk tree yield) (cond ((null? tree) #f) ((pair? tree) (walk (car tree) yield) (walk (cdr tree) yield)) (else (yield tree)))) (define (make-gen tree) (define return #f) (define resume #f) (define (yield v) (call/cc (lambda (r) (set! resume r) (return v)))) (lambda () (call/cc (lambda (ret) (set! return ret) (if resume (resume #f) (begin (walk tree yield) (return 'done))))))) (let ((g (make-gen '((1 2) (3 (4 5)) 6)))) (let loop ((acc '())) (let ((v (g))) (if (eq? v 'done) (reverse acc) (loop (cons v acc)))))))" . "(1 2 3 4 5 6)")
))

(let ((pass 0))
  (dolist (tc *tests*)
    (let ((got (handler-case (r7rs:write-to-string (r7rs:eval (car tc)))
		 (error (e) (format nil "ERROR: ~A" (remove #\Newline (princ-to-string e)))))))
      (cond ((string= got (cdr tc))
	     (incf pass)
	     (when *verbose* (format t "ok   ~A~%" (car tc))))
	    (t (format t "FAIL ~A~%  expected ~A~%  got      ~A~%" (car tc) (cdr tc) got)))))
  (format t "~&Continuations: ~D of ~D tests passed.~%" pass (length *tests*)))
