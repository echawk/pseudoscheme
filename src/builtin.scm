; File builtin.scm / -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1991 Jonathan Rees / See file COPYING

;;;; Compilation of calls to built-in Scheme procedures

; The usual integrations

; An entry in the integrations table is a pair, one of the following:
;   (FUN foo)            - translate as #'foo or (foo ...)
;   (PRED foo)		 - translate calls as (qs:true? (foo ...))
;   (SUBST bvl body)     - translate calls as appropriate
;   (LAMBDA bvl body)    - ditto
;   (CASE-AUX)	         - a special case kludge

; The integrations table is indexed by Common Lisp symbols.

(define integrations-table (make-table))

(define (define-integration! var int)
  (table-set! integrations-table var int))

(for-each (lambda (z)
	    (define-integration!
	      (program-env-lookup revised^4-scheme-env (car z))
	      (cadr z)))
    `(
      ;; car, cdr and the cXr compositions check for a pair inline:
      ;; (the cons x) is a type check at SBCL's default safety, so
      ;; (car '()) is an error, as R6RS requires, not CL's NIL.
      (*                              (fun qs:scheme*))
      (+                              (fun qs:scheme+))
      (-                              (fun qs:scheme-))
      (/                              (fun qs-lisp:/))
      (<=                             (pred qs:scheme<=))
      (<                              (pred qs:scheme<))
      (=                              (pred qs:scheme=))
      (>=                             (pred qs:scheme>=))
      (>                              (pred qs:scheme>))
      (abs                            (fun qs-lisp:abs))
      (acos                           (fun qs:scheme-acos))
      (angle                          (fun qs:scheme-angle))
      (append                         (fun qs-lisp:append))
      (apply                          (fun qs-lisp:apply))
      (asin                           (fun qs:scheme-asin))
      (assoc
       (subst (obj list)
	 (qs:true? (qs-lisp:assoc obj list
				 :test (qs-lisp:function qs:scheme-equal-p)))))
      (assq
       (subst (obj list)
	 (qs:true? (qs-lisp:assoc obj list :test (qs-lisp:function qs-lisp:eq)))))
      (assv                           (pred qs-lisp:assoc 2))
      (atan                           (fun qs:scheme-atan))
      (boolean?			      (pred qs:booleanp 1))
      (caaaar                          (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))))
      (caaadr                          (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))))
      (caaar                           (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))
      (caadar                          (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))))
      (caaddr                          (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))))
      (caadr                           (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))
      (caar                            (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))
      (cadaar                          (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))))
      (cadadr                          (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))))
      (cadar                           (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))
      (caddar                          (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))))
      (cadddr                          (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))))
      (caddr                           (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))
      (cadr                            (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))
      ;; Escaping only; see rts.lisp for why not BLOCK/RETURN-FROM.
      (call-with-current-continuation (fun qs:call-with-escape))
      (call-with-input-file
	  (lambda (string proc)
	    (qs-lisp:with-open-file (port (qs-lisp:merge-pathnames string)
					  :direction :input)
	      (qs-lisp:funcall proc port))))
      (call-with-output-file
	  (lambda (string proc)
	    (qs-lisp:with-open-file (port (qs-lisp:merge-pathnames string)
					  :direction :output
					  :if-exists :supersede)
	      (qs-lisp:funcall proc port))))
      (call-with-values
	  (subst (thunk proc)
	    (qs-lisp:multiple-value-call proc (qs-lisp:funcall thunk))))
      (car                             (subst (x) (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))
      (cdaaar                          (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))))
      (cdaadr                          (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))))
      (cdaar                           (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))
      (cdadar                          (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))))
      (cdaddr                          (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))))
      (cdadr                           (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))
      (cdar                            (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))
      (cddaar                          (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))))
      (cddadr                          (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))))
      (cddar                           (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))
      (cdddar                          (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:car (qs-lisp:the qs-lisp:cons x))))))))))
      (cddddr                          (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))))
      (cdddr                           (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))))
      (cddr                            (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))))
      (cdr                             (subst (x) (qs-lisp:cdr (qs-lisp:the qs-lisp:cons x))))
      (ceiling                        (fun qs:scheme-ceiling))
      (char->integer                  (fun qs-lisp:char-code))
      (char-alphabetic?               (pred qs-lisp:alpha-char-p 1))
      (char-ci<=?                     (pred qs-lisp:char-not-greaterp))
      (char-ci<?                      (pred qs-lisp:char-lessp))
      (char-ci=?                      (pred qs-lisp:char-equal))
      (char-ci>=?                     (pred qs-lisp:char-not-lessp))
      (char-ci>?                      (pred qs-lisp:char-greaterp))
      (char-downcase                  (fun qs-lisp:char-downcase))
      (char-lower-case?               (pred qs-lisp:lower-case-p 1))
      (char-numeric?                  (pred qs:char-numeric-p 1))
      (char-ready?		      (pred qs-lisp:listen))
      (char-upcase                    (fun qs-lisp:char-upcase))
      (char-upper-case?               (pred qs-lisp:upper-case-p 1))
      (char-whitespace?		      (pred qs:char-whitespace-p 1))
      (char<=?                        (pred qs-lisp:char<=))
      (char<?                         (pred qs-lisp:char<))
      (char=?                         (pred qs-lisp:char=))
      (char>=?                        (pred qs-lisp:char>=))
      (char>?                         (pred qs-lisp:char>))
      (char?                          (pred qs-lisp:characterp 1))
      (close-input-port               (fun qs-lisp:close))
      (close-output-port              (fun qs-lisp:close))
      (complex?                       (pred qs-lisp:numberp 1))
      (cons                           (fun qs-lisp:cons))
      (cos                            (fun qs:scheme-cos))
      (current-input-port
       (subst () qs-lisp:*standard-input*))
      (current-output-port
       (subst () qs-lisp:*standard-output*))
      (denominator                    (fun qs-lisp:denominator))
      (eof-object?
       (subst (obj)
	 (qs:true? (qs-lisp:eq obj qs:eof-object))))
      (eq?                            (pred qs-lisp:eq 2))
      (equal?			      (pred qs:scheme-equal-p 2))
      (eqv?                           (pred qs-lisp:eql 2))
      (even?                          (pred qs:scheme-evenp 1))
      (exact?                         (pred qs:exact-p 1))
      (exact->inexact                 (fun qs:inexact))
      (expt                           (fun qs:scheme-expt))
      (exp                            (fun qs:scheme-exp))
      (floor                          (fun qs:scheme-floor))
      (for-each                       (fun qs-lisp:mapc))
      (gcd                            (fun qs-lisp:gcd))
      (imag-part                      (fun qs:scheme-imag-part))
      (inexact?                       (pred qs:inexact-p 1))
      (inexact->exact                 (fun qs:exact))
      (input-port?		      (pred qs:input-port-p 1))
      (integer->char                  (fun qs-lisp:code-char))
      (integer?                       (pred qs:integer-number-p 1))
      (lcm                            (fun qs-lisp:lcm))
      (length                         (fun qs-lisp:length))
      (list                           (fun qs-lisp:list))
      (list->string
       (subst (l) (qs-lisp:coerce (qs-lisp:the qs-lisp:list l)
				  (qs-lisp:quote qs-lisp:simple-string))))
      (list->vector
       (subst (l) (qs-lisp:coerce (qs-lisp:the qs-lisp:list l)
				  (qs-lisp:quote qs-lisp:simple-vector))))
      (list-ref
       (subst (list n) (qs-lisp:nth n list)))
      (list-tail
       (subst (list n) (qs-lisp:nthcdr n list)))
      (log                            (fun qs:scheme-log))
      (magnitude                      (fun qs:scheme-magnitude))
      (make-polar                     (fun qs:scheme-make-polar))
      (make-rectangular               (fun qs-lisp:complex))
      (map                            (fun qs-lisp:mapcar))
      (max                            (fun qs:scheme-max))
      (member
       (subst (obj list)
	 (qs:true? (qs-lisp:member obj list
				  :test (qs-lisp:function qs:scheme-equal-p)))))
      (memq
       (subst (obj list)
	 (qs:true? (qs-lisp:member obj list :test (qs-lisp:function qs-lisp:eq)))))
      (memv                           (pred qs-lisp:member 2))
      (min                            (fun qs:scheme-min))
      (modulo                         (fun qs:scheme-modulo))
      (negative?                      (pred qs-lisp:minusp 1))
      (newline                        (fun qs:scheme-newline))
      (not			      (special))
      (null?                          (pred qs-lisp:null 1))
      (number?                        (pred qs-lisp:numberp 1))
      (numerator                      (fun qs-lisp:numerator))
      (odd?                           (pred qs:scheme-oddp 1))
      (open-input-file
       (subst (string)
	 (qs-lisp:open (qs-lisp:merge-pathnames string) :direction :input)))
      (open-output-file
       (subst (string)
	 (qs-lisp:open (qs-lisp:merge-pathnames string) :direction :output)))
      (output-port?                   (pred qs:output-port-p 1))
      ;; This isn't quite right; PAIR? wants to return false for
      ;; procedures.  (Some Common Lisps implement some functions as
      ;; pairs.)  But the run-time overhead of this check would be
      ;; prohibitively high.
      (pair?                          (pred qs-lisp:consp 1))
      (positive?                      (pred qs-lisp:plusp 1))
      (procedure?		      (pred qs:procedurep 1))
      (quotient                       (fun qs:scheme-quotient))
      (rational?                      (pred qs:rational-number-p 1))
      (real?			      (pred qs:realp 1))
      (real-part                      (fun qs-lisp:realpart))
      (remainder                      (fun qs:scheme-remainder))
      (reverse                        (fun qs-lisp:reverse))
      (round                          (fun qs:scheme-round))
      (set-car!
       (subst (pair obj)
	 (qs-lisp:setf (qs-lisp:car pair) obj)
	 qs:unspecific))
      (set-cdr!
       (subst (pair obj)
	 (qs-lisp:setf (qs-lisp:cdr pair) obj)
	 qs:unspecific))
      (sin                            (fun qs:scheme-sin))
      (sqrt                           (fun qs:scheme-sqrt))
      (string->list
       (subst (string)
	 (qs-lisp:coerce (qs-lisp:the qs-lisp:simple-string string)
		      (qs-lisp:quote qs-lisp:list))))
      (string->symbol
       (subst (string)
	 (qs:intern-scheme-symbol string)))
      ;; NB: unlike STRING= / STRING-EQUAL, these CL comparison functions
      ;; return the mismatch index (or NIL) rather than a canonical T/NIL
      ;; boolean, so a truthy result can't just be passed through QS:TRUE?
      ;; (which only maps CL NIL to QS:FALSE, leaving any other truthy
      ;; value as-is) -- normalize explicitly to QS:TRUE / QS:FALSE.
      (string-ci<=?
       (subst (a b)
	 (qs-lisp:if (qs-lisp:string-not-greaterp a b) qs:true qs:false)))
      (string-ci<?
       (subst (a b)
	 (qs-lisp:if (qs-lisp:string-lessp a b) qs:true qs:false)))
      (string-ci=?                    (pred qs-lisp:string-equal 2))
      (string-ci>=?
       (subst (a b)
	 (qs-lisp:if (qs-lisp:string-not-lessp a b) qs:true qs:false)))
      (string-ci>?
       (subst (a b)
	 (qs-lisp:if (qs-lisp:string-greaterp a b) qs:true qs:false)))
      (string-copy                    (fun qs-lisp:copy-seq))
      (string-fill!
       (subst (s val)
	 (qs-lisp:fill (qs-lisp:the qs-lisp:simple-string s) val)))
      (string-length
       (subst (s)
         (qs-lisp:length (qs-lisp:the qs-lisp:simple-string s))))
      (string-ref
       (subst (s k)
	 (qs-lisp:char (qs-lisp:the qs-lisp:simple-string s) k)))
      (string-set!
       (subst (s k obj)
	 (qs-lisp:setf (qs-lisp:char (qs-lisp:the qs-lisp:simple-string s) k) obj)
	 qs:unspecific))
      (string<=?
       (subst (a b) (qs-lisp:if (qs-lisp:string<= a b) qs:true qs:false)))
      (string<?
       (subst (a b) (qs-lisp:if (qs-lisp:string< a b) qs:true qs:false)))
      (string=?                       (pred qs-lisp:string= 2))
      (string>=?
       (subst (a b) (qs-lisp:if (qs-lisp:string>= a b) qs:true qs:false)))
      (string>?
       (subst (a b) (qs-lisp:if (qs-lisp:string> a b) qs:true qs:false)))
      (string?                        (pred qs-lisp:simple-string-p 1))
      (substring                      (fun qs-lisp:subseq))
      (symbol?			      (pred qs:scheme-symbol-p 1))
      (tan                            (fun qs:scheme-tan))
      (transcript-off
       (subst ()
         (qs-lisp:dribble)
	 qs:unspecific))
      (transcript-on
       (subst (filespec)
         (qs-lisp:dribble filespec)
	 qs:unspecific))
      (truncate                       (fun qs:scheme-truncate))
      (values                         (fun qs-lisp:values))
      (vector                         (fun qs-lisp:vector))
      (vector->list
       (subst (vec)
	 (qs-lisp:coerce (qs-lisp:the qs-lisp:simple-vector vec)
		      (qs-lisp:quote qs-lisp:list))))
      (vector-fill!
       (subst (vec val)
	 (qs-lisp:fill (qs-lisp:the qs-lisp:simple-vector vec) val)))
      (vector-length
       (subst (vec)
         (qs-lisp:length (qs-lisp:the qs-lisp:simple-vector vec))))
      (vector-ref                     (fun qs-lisp:svref))
      (vector-set!
       (subst (vec k obj)
	 (qs-lisp:setf (qs-lisp:svref vec k) obj)
	 qs:unspecific))
      (with-input-from-file
       (subst (string thunk)
      	 (qs-lisp:with-open-file (qs-lisp:*standard-input*
			         (qs-lisp:merge-pathnames string)
				 :direction :input)
	   (qs-lisp:funcall thunk))))
      (with-output-to-file
       (subst (string thunk)
	 (qs-lisp:with-open-file (qs-lisp:*standard-output*
			         (qs-lisp:merge-pathnames string)
				 :direction :output
				 :if-exists :supersede)
	    (qs-lisp:funcall thunk))))
      (write-char                     (fun qs:scheme-write-char))
      (zero?                          (pred qs:scheme-zerop 1))

      ;; Auxiliaries
      (unassigned (subst () qs:unassigned))
      (unspecific (val qs:unspecific))
      (and-aux (special))
      (or-aux (special))
      (=>-aux (special))
      (case-aux (special))
      ))
