;; Vector processing

(define-syntax em-vector
  (em-syntax-rules ()
    ((em-vector 'element ...)
     '#(element ...))))

(define-syntax em-list->vector
  (em-syntax-rules ()
    ((em-list->vector '(element ...))
     '#(element ...))))

(define-syntax em-vector->list
  (em-syntax-rules ()
    ((em-list->vector '#(x ...))
     '(x ...))))

(define-syntax em-vector-map
  (em-syntax-rules ()
    ((em-vector-map 'proc 'vector ...)
     (em-list->vector (em-map 'proc (em-vector->list 'vector) ...)))))

(define-syntax em-vector-ref
  (em-syntax-rules ()
    ((em-vector-ref '#(element1 element2 ...) '())
     'element1)
    ((em-vector-ref '#(element1 element2 ...) '(h . t))
     (em-vector-ref '#(element2 ...) 't))))

