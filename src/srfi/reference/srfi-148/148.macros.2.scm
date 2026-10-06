;; List processing

(define-syntax em-caar
  (em-syntax-rules ()
    ((em-caar '((a . b) . c))
     'a)))

(define-syntax em-cadr
  (em-syntax-rules ()
    ((em-cadr '(a . (b . c)))
     'b)))

(define-syntax em-cdar
  (em-syntax-rules ()
    ((em-cdar '((a . b) . c))
     'b)))

(define-syntax em-cddr
  (em-syntax-rules ()
    ((em-cddr '(a . (b . c)))
     'c)))

(define-syntax em-first
  (em-syntax-rules ()
    ((em-first '(a . z))
     'a)))

(define-syntax em-second
  (em-syntax-rules ()
    ((em-second '(a b . z))
     'b)))

(define-syntax em-third
  (em-syntax-rules ()
    ((em-third '(a b c . z))
     'c)))

(define-syntax em-fourth
  (em-syntax-rules ()
    ((em-forth '(a b c d . z))
     'd)))

(define-syntax em-fifth
  (em-syntax-rules ()
    ((em-fifth '(a b c d e . z))
     'e)))

(define-syntax em-sixth
  (em-syntax-rules ()
    ((em-sixth '(a b c d e f . z))
     'f)))

(define-syntax em-seventh
  (em-syntax-rules ()
    ((em-seventh '(a b c d e f g . z))
     'g)))

(define-syntax em-eighth
  (em-syntax-rules ()
    ((em-eighth '(a b c d e f g h . z))
     'h)))

(define-syntax em-ninth
  (em-syntax-rules ()
    ((em-ninth '(a b c d e f g h i . z))
     'i)))

(define-syntax em-tenth
  (em-syntax-rules ()
    ((em-tenth '(a b c d e f g h i j . z))
     'j)))

(define-syntax em-make-list
  (em-syntax-rules ()
    ((em-make-list '() 'fill)
     '())
    ((em-make-list '(h . t) 'fill)
     (em-cons 'fill (em-make-list 't 'fill)))))

(define-syntax em-reverse
  (em-syntax-rules ()
    ((em-reverse '())
     '())
    ((em-reverse '(h ... t))
     (em-cons 't (em-reverse '(h ...))))))

(define-syntax em-list-tail
  (em-syntax-rules ()
    ((em-list-tail 'list '())
     'list)
    ((em-list-tail '(h . t) '(u . v))
     (em-list-tail 't 'v))))

(define-syntax em-drop
  (em-syntax-rules ()
    ((em-drop 'arg ...)
     (em-list-ref 'arg ...))))

(define-syntax em-list-ref
  (em-syntax-rules ()
    ((em-list-ref '(h . t) '())
     'h)
    ((em-list-ref '(h . t) '(u . v))
     (em-list-ref 't 'v))))

(define-syntax em-take
  (em-syntax-rules ()
    ((em-take '_ '())
     '())
    ((em-take '(h . t) '(u . v))
     (em-cons 'h (em-take 't 'v)))))

(define-syntax em-take-right
  (em-syntax-rules ()
    ((em-take-right '(a ... . t) '())
     't)
    ((em-take-right '(a ... b . t) '(u . v))     
     `(,@(em-take-right '(a ...) 'v) b . t)
     )))

(define-syntax em-drop-right
  (em-syntax-rules ()
    ((em-drop-right '(a ... . t) '())
     '(a ...))
    ((em-drop-right '(a ... b . t) '(u . v))
     (em-drop-right '(a ...) 'v))))

(define-syntax em-last
  (em-syntax-rules ()
    ((em-last '(a ... b . t))
     'b)))

(define-syntax em-last-pair
  (em-syntax-rules ()
    ((em-last '(a ... b . t))
     '(b . t))))

