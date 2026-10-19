; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File utils.scm / Copyright (c) 1991 Jonathan Rees / See file COPYING

;;;; Miscellaneous general and not-so-general utilities

; last-pair (was in r^3, flushed for r^4)

(define (last-pair x)
  (qs-lisp:last x))

; posq

(define (vector-posq thing v)
  (qs-lisp:or (qs-lisp:position thing (qs-lisp:the qs-lisp:simple-vector v))
	   #f))

(define (string-posq c s)
  (qs-lisp:or (qs-lisp:position c (qs-lisp:the qs-lisp:simple-string s))
	   #f))

; Fluids

(define (make-fluid top-level-value)
  (let ((f (qs-lisp:gensym "FLUID")))
    (qs-lisp:set f top-level-value)
    f))

(define (fluid f)
  (qs-lisp:symbol-value f))

(define (set-fluid! f val)
  (qs-lisp:set f val))

(define (let-fluid f val thunk)
  (qs-lisp:progv (list f) (list val) (thunk)))

; Tables

(define (make-table)
  ;; Default size in VAX LISP is 71, which seems rather large.
  (qs-lisp:values (qs-lisp:make-hash-table :size 20 :rehash-size 2.0)))

(define (table-set! table key val)
  (qs-lisp:setf (qs-lisp:gethash key table) val))

(define (table-ref table key)
  (qs-lisp:gethash key table #f))

; Pretty-printer used by translator
; Two cases:
;  - If package is scheme-package, then unqualified symbols must print
;    without package prefixes, and qualified ones must print with.
;  - Otherwise, the opposite, and the package prefix for unqualified
;    symbols ought to be 

(define cl-readtable (qs-lisp:copy-readtable 'qs-lisp:nil))

(qs-lisp:defun write-pretty (form port package)
  (qs-lisp:let ((qs-lisp:*package* package)
	     (qs-lisp:*print-case* :upcase)
	     (qs-lisp:*readtable* cl-readtable))
    (qs-lisp:declare (qs-lisp:special cl-readtable))
    (qs-lisp:format port "~&")
    (qs-lisp:write form :stream port
		     :pretty qs-lisp:t
		     :length 'qs-lisp:nil
		     :level 'qs-lisp:nil)
    (qs-lisp:values)))

; Package stuff, etc.

(define (intern-renaming-perhaps string package)
  (qs-lisp:intern (if (eq? package scheme-package)
		      string
		      (perhaps-rename string))
		  package))

(define (perhaps-rename string)  ;Cf. defune in rts.lisp
  (if (or (qs-lisp:multiple-value-bind (sym status) ;Good candidate for caching
	      (qs-lisp:find-symbol string lisp-package)
	      sym ;ignore
	    (eq? status :external))
	  (and (> (string-length string) 0)
	       (char=? (string-ref string 0) #\&)))
      (string-append "." string)
      string))

(define lisp-package (qs-lisp:find-package "QS-LISP"))

(define (qualified-symbol? sym)
  (qs-lisp:if (qs-lisp:symbolp sym)
	      (not (eq? (qs-lisp:symbol-package sym) scheme-package))
	      #f))

(define scheme-package (qs-lisp:symbol-package 'askdjfh))

;; NB: QS-LISP:SYMBOL-NAME, not SYMBOL->STRING -- these name real CL
;; packages, which must stay canonical (fold-cased) regardless of
;; SYMBOL->STRING's own case-preserving behavior for Scheme-visible
;; use (see rts.lisp).

(define (make-package-using id use-list)
  (let* ((name (qs-lisp:symbol-name id))
	 (probe (qs-lisp:find-package name))
	 (package
	  (cond ((not (eq? probe 'qs-lisp:nil))
	         (for-each (lambda (use)
			     (if (not (or (eq? use lisp-package)
					  (memq use use-list)))
				 (qs-lisp:unuse-package use probe)))
			   (qs-lisp:package-use-list probe))
		 probe)
		(else (qs-lisp:make-package name :use use-list)))))
    (qs-lisp:use-package (if (eq? id 'scheme)
			  use-list	;Kludge
			  (cons lisp-package use-list))
		      package)
    package))

(define (make-package-exporting id syms)
  (let* ((name (qs-lisp:symbol-name id))
	 (new (qs-lisp:or (qs-lisp:find-package name)
		       (qs-lisp:make-package name :use '()))))
    (qs-lisp:import syms new)
    (qs-lisp:export syms new)
    new))


; qs-lisp:namestring
; qs-lisp:truename
; qs-lisp:merge-pathnames
; qs-lisp:make-pathname
; qs-lisp:package-name

; Etc.

(define (scheme-implementation-version)
  (string-append (#'qs-lisp:lisp-implementation-type)
		 " "
		 (#'qs-lisp:lisp-implementation-version)))

(define (defined-as-CL-macro? CL-sym)
  (qs-lisp:if (qs-lisp:macro-function CL-sym)
	      #t
	      #f))

(define (true-name source-file-name)
  (qs-lisp:namestring (qs-lisp:truename source-file-name)))

(define package-name #'qs-lisp:package-name)

(define intern #'qs-lisp:intern)
