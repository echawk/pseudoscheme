; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File translate.scm / Copyright (c) 1991 Jonathan Rees / See file COPYING

;;;; Translation from Scheme to Common Lisp

; TRANSLATE translates a single Scheme expression into Common Lisp.

(define (translate form env)
  (with-target-package (program-env-package env)
    (lambda ()
      (translate-to-common-lisp (list form) env))))

; Used by translate and translate-file

(define (translate-to-common-lisp forms env)
  (prognify
   (let recur ((forms forms))
     (if (null? forms)
	 '()
	 (cons (with-uid-reset
		(lambda ()
		  (let-fluid @free-variables '()
		    (lambda ()
		      (let ((node (alpha-top (car forms) env)))
			(generate-top
			 node
			 (generation-env (fluid @free-variables))
			 (not (null? (cdr forms)))))))))
	       (recur (cdr forms)))))))

; Used by SCHEME-COMPILE.

(define (translate-lambda form env)
  (with-uid-reset
   (lambda ()
     (let-fluid @free-variables '()
       (lambda ()
	 (let ((node (alpha-top form env)))
	   (if (lambda? node)
	       (generate-lambda-top
		  node
		  (generation-env (fluid @free-variables)))
	       (error "not a lambda expression" form))))))))

; File transduction

(define (really-translate-file source-file-name
			       translated-file-name
			       program-env)
  (let ((source-code (read-file source-file-name)))
    (compiling-to-file
      translated-file-name
      (program-env-package program-env)
      (lambda (port)
	(display ";  from file " port)
	(display (true-name source-file-name) port)
	(newline port))
      (lambda (port)
	(for-each (lambda (form)
		    (write-flattened form port))
		  (translate-to-common-lisp source-code program-env))))))

; The following generates a file CLOSED.PSO from the information we
; have on how to open-code the built-in procedures.

(define (write-closed-definitions structure outfile)
  (compiling-to-file outfile
		     (structure-package structure)
		     (lambda (port) port)
		     (lambda (port)
		       (write-closed-definitions-1 structure port))))

(define (write-closed-definitions-1 structure port)
  (let ((sig (structure-interface structure))
	(env (structure-program-env structure)))
    (let ((funs '())
	  (defs '()))
      (let ((do-it
	     (lambda (name)
	       (let* ((den (program-env-lookup env name))
		      (info (get-integration den)))
		 (if info
		     (let ((sym (program-variable-location den)))
		       (case (car info)
			 ((val)
			  (write-form `(qs-lisp:locally
					   (qs-lisp:declare (qs-lisp:special ,sym))
					 (qs-lisp:setq ,sym ,(cadr info)))
				      port)
			  (write-form `(qs:set-function-from-value
					    (qs-lisp:quote ,sym))
				      port))
			 ((fun)
			  (if (not (memq name '(car cdr))) ;kludge
			      (set! funs (cons (list sym (cadr info))
					       funs))))
			 ((pred)
			  (write-form
			   (case (if (null? (cddr info))
				    'n
				    (caddr info))
			     ((1)
			      `(qs-lisp:defun ,sym (x)
				 (qs:true? (,(cadr info) x))))
			     ((2)
			      `(qs-lisp:defun ,sym (x y)
				 (qs:true? (,(cadr info) x y))))
			     (else
			      `(qs-lisp:defun ,sym (qs-lisp:&rest x)
				 (qs:true? (qs-lisp:apply #',(cadr info)
							 x)))))
			   port)
			  (set! defs (cons sym defs)))
			 ((subst lambda)
			  (write-form `(qs-lisp:defun ,sym ,@(cdr info)) port)
			  (set! defs (cons sym defs)))
			 ((special) 0) ;don't generate any definition
			 (else
			  (error "peculiar built-in" info)))))))))
	(for-each do-it (interface-names sig))
	(for-each do-it (interface-aux-names sig)))
      (write-form
        `(qs-lisp:mapc (qs-lisp:function qs:set-value-from-function)
		    (qs-lisp:quote ,(reverse defs)))
	port)
      (write-form
        `(qs-lisp:mapc #'(qs-lisp:lambda (z)
			(qs-lisp:let ((our-sym (qs-lisp:car z))
				   (cl-sym (qs-lisp:cadr z)))
			  (qs-lisp:setf (qs-lisp:symbol-function our-sym) 
				     (qs-lisp:symbol-function cl-sym))
			  (qs:set-value-from-function our-sym)))
		    (qs-lisp:quote ,(reverse funs)))
	port))))


; Utilities

(define (with-target-package package thunk)
  (let-fluid @target-package package
    thunk))

(define (compiling-to-file outfile package write-message proc)
  (let-fluid @translating-to-file? #t
    (lambda ()
      (with-target-package package
	(lambda ()
	  (call-with-output-file outfile
	    (lambda (port)
	      (write-file-identification port)
	      (write-message port)
	      (newline port)
	      (display "(qs:in-package " port)
	      (write (package-name package) port)
	      (display ")" port)
	      (newline port)
	      (write-form '(qs:begin-translated-file) port)
	      (newline port)
	      ;; Now do the real work.
	      (proc port)
	      (newline port)
	      outfile)))))))

(define (write-file-identification port)
  (newline)
  (display "Writing ")
  (display (true-name port))
  (display "; -*- Mode: Lisp; Syntax: Common-Lisp; Package: " port)
  (display (package-name (fluid @target-package)) port) ;Heuristic
  (display "; -*-" port)
  (newline port)
  (newline port)
  (display "; This file was generated by " port)
  (display (translator-version) port)
  (newline port)
  (display ";  running in " port)
  (display (scheme-implementation-version) port)
  (newline port))

(define (write-flattened form port)
  (cond ((not (pair? form))
	 (if (not (or (symbol? form)
		      (number? form)
		      (boolean? form)
		      (string? form)
		      (char? form)))
	     ;; Who knows, it might be important.
	     (write-form form port)))
        ((eq? (car form) 'qs-lisp:quote)
	 )				;do nothing
	((eq? (car form) 'qs-lisp:progn)
	 (for-each (lambda (form)
		     (write-flattened form port))
		   (cdr form)))
	(else
	 (write-form form port))))

(define (write-form form port)
  (write-pretty form port (fluid @target-package)))
