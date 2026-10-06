;;; SRFI 7's PROCESS-PROGRAM implementation, by Richard Kelsey, from the
;;; SRFI document (MIT licence; see LICENSE).

(define (process-program program features)
  (call-with-current-continuation
    (lambda (exit)	; We exit early when an unsatisfiable clause is found.

      ; Process each clause in turn

      (define (process-clauses clauses)
	(if (null? clauses)
	    '()
	    (append (process-clause (car clauses))
		    (process-clauses (cdr clauses)))))
      
      ; Dispatch on the type of the clause.

      (define (process-clause clause)
	(case (car clause)
	  ((requires)
	   (if (all-satisfied? (cdr clause))
	       '()
	       (exit #f)))
	  ((code)
	   (cdr clause))
	  ((files)
	   (read-files (cdr clause)))
	  ((feature-cond)
	   (process-cond-clauses (cdr clause)))))
      
      ; Loop through CLAUSES finding the first that is satisfied.

      (define (process-cond-clauses clauses)
	(cond ((null? clauses)
	       (exit #f))
	      ((or (and (eq? (caar clauses) 'else)
			(null? (cdr clauses)))
		   (satisfied? (caar clauses)))
	       (process-clauses (cdar clauses)))
	      (else
	       (process-cond-clauses (cdr clauses)))))
    
      ; Compound requirements are handled recursively, simple ones are tested.

      (define (satisfied? requirement)
	(if (pair? requirement)
	    (case (car requirement)
	      ((and)
	       (all-satisfied? (cdr requirement)))
	      ((or)
	       (any-satisfied? (cdr requirement)))
	      ((not)
	       (not (satisfied? (cadr requirement)))))
	    (memq requirement features)))
      
      ; True if every requirement in LIST is satisfied.

      (define (all-satisfied? list)
	(if (null? list)
	    #t
	    (and (satisfied? (car list))
		 (all-satisfied? (cdr list)))))
      
      ; True if any requirement in LIST is satisfied.

      (define (any-satisfied? list)
	(if (null? list)
	    #f
	    (or (satisfied? (car list))
		(any-satisfied? (cdr list)))))
      
      ; Start by doing the whole program.

      (process-clauses (cdr program)))))

; Returns a list of the forms in the named files.

(define (read-files filenames)
  (if (null? filenames)
      '()
      (append (call-with-input-file (car filenames)
		(lambda (in)
		  (let label ()
		    (let ((next (read in)))
		      (if (eof-object? next)
			  '()
			  (cons next (label)))))))
	      (read-files (cdr filenames)))))

