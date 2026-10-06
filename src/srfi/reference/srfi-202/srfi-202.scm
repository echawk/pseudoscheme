(define-module (srfi srfi-202)
  #:use-module (ice-9 match)
  #:export ((and-let*/match . and-let*)))

(define-syntax and-let*/match
  (lambda (stx)
    (syntax-case stx (values)

      ((_)
       #'#t)

      ((_ ())
       #'#t)

      ((_ () body ...)
       #'(let () body ...))

      ((_ ((name binding) rest ...) body ...)
       (identifier? #'name)
       #'(let ((name binding))
	   (and name
		(and-let*/match (rest ...)
				body ...))))

      ((_ (((values . structure) binding) rest ...)
	  body ...)
       #'(call-with-values (lambda () binding)
	   (lambda args
	     (match args
	       (structure
		(and-let*/match (rest ...)
				body ...))
	       (_ #f)))))

      ((_ ((value binding) rest ...) body ...)
       #'(match binding
	   (value
	    (and-let*/match (rest ...)
			    body ...))
	   (_ #f)))

      ((_ ((condition) rest ...)
	  body ...)
       #'(and condition
	      (and-let*/match (rest ...)
			      body ...)))

      ((_ ((value * ... expression) rest ...) body ...)
       (identifier? #'value)
       #'(call-with-values (lambda () expression)
	   (lambda args
	     (match args
	       ((value * ... . _)
		(and value
		     (and-let*/match (rest ...)
				     body ...)))
	       (_ #f)))))

      ((_ ((value ... expression) rest ...) body ...)
       #'(call-with-values (lambda () expression)
	   (lambda args
	     (match args
	       ((value ... . _)
		(and-let*/match (rest ...)
				body ...))
	       (_ #f)))))

      )))
