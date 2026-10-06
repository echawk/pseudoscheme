;; Folding, unfolding and mapping

(define-syntax em-fold
  (em-syntax-rules ()
    ((em-fold 'kons 'knil '(h . t) ...)
     (em-fold 'kons (kons 'h ... 'knil) 't ...))
    ((em-fold 'kons 'knil '_ ...)
     'knil)))

(define-syntax em-fold-right
  (em-syntax-rules ()
    ((em-fold-right 'kons 'knil '(h . t) ...)
     (kons 'h ... (em-fold-right 'kons 'knil 't ...)))
    ((em-fold-right 'kons 'knil '_ ...)
     'knil)))

(define-syntax em-unfold
  (em-syntax-rules ()
    ((em-unfold 'stop? 'mapper 'successor 'seed 'tail-mapper)
     (em-if (stop? 'seed)
	    (tail-mapper 'seed)
	    (em-cons (mapper 'seed)
		     (em-unfold 'stop? 'mapper 'successor (successor 'seed) 'tail-mapper))))
    ((em-unfold 'stop? 'mapper 'successor 'seed)
     (em-unfold 'stop? 'mapper 'successor 'seed (em-constant '())))))

(define-syntax em-unfold-right
  (em-syntax-rules ()
    ((em-unfold-right 'stop? 'mapper 'successor 'seed 'tail)
     (em-if (stop? 'seed)
	    'tail
	    (em-unfold-right 'stop?
			     'mapper
			     'successor
			     (successor 'seed)
			     (em-cons (mapper 'seed) 'tail))))
    ((em-unfold-right 'stop? 'mapper 'successor 'seed)
     (em-unfold-right 'stop? 'mapper 'successor 'seed '()))))

(define-syntax em-map
  (em-syntax-rules ()
    ((em-map 'proc '(h . t) ...)
     (em-cons (proc 'h ...) (em-map 'proc 't ...)))
    ((em-map 'proc '_ ...)
     '())))

(define-syntax em-append-map
  (em-syntax-rules ()
    ((em-append-map 'proc '(h . t) ...)
     (em-append (proc 'h ...) (em-append-map 'proc 't ...)))
    ((em-append-map map 'proc '_)
     '())))

