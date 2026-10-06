;============================================================================================
; Examples:

; A simple record declaration:

(define-record-type point (make-point x y) point?
  (x point.x point.x-set!)
  (y point.y point.y-set!))

(define p (make-point 1 2))

(point? p)             ;==> #t
(point.y p)            ;==> 2
(point.y-set! p 7)
(point.y p)            ;==> 7

; Simple record schemes.
; Record schemes don't have constructors.
; The predicates and accessors are polymorphic.

(define-record-scheme <point #f <point? 
  (x <point.x)
  (y <point.y))

(define-record-scheme <color #f <color?
  (hue <color.hue))

; Concrete instances of the above schemes.
; Constructors may be declared.
; Predicates and accessors, when provided, are monomorphic.  

(define-record-type (point <point) make-point point?
  (x point.x)
  (y point.y))

(define-record-type (color <color) make-color)

(define-record-type (color-point <color <point) (make-color-point x y hue) color-point?
  (extra color-point.extra))

(define cp (make-color-point 1 2 'blue))

(<point? cp)            ;==> #t         
(<color? cp)            ;==> #t
(color-point? cp)       ;==> #t
;(point.x cp)           ;==> error 
(<point.y cp)           ;==> 2
(<color.hue cp)         ;==> blue
(color-point.extra cp)  ;==> <undefined>

; Constructing records by field labels:

(define p (point (x 1) 
                 (y 2)))
(define cp (color-point (hue 'blue) 
                        (x 1) 
                        (y 2)))

; Monomorphic functional update:

(show
 (record-update p point (x 7)))     ;==> (point (x 7) (y 2))
(show p)                            ;==> (point (x 1) (y 2))   - original unaffected

; Polymorphic functional update:

(show 
 (record-update cp <point (x 7)))   ;==> (color-point (extra <undefined>) (hue blue) (x 7) (y 2))
(show cp)                           ;==> (color-point (extra <undefined>) (hue blue) (x 1) (y 2))

; In-place update:

(show 
 (record-update! cp <point (x 7)))  ;==> color-point (extra <undefined>) (hue blue) (x 7) (y 2))
(show cp)                           ;==> color-point (extra <undefined>) (hue blue) (x 7) (y 2))
 
; Use record-compose for updates polymorphic in argument but monomorphic in result type:

(show
 (record-compose (<point cp) (point (x 8))))  ;==> (point (x 8) (y 2))
(show cp)                                     ;==> (color-point (extra <undefined>) (hue blue) (x 7) (y 2))

; More general record composition example:

(define cp (make-color-point 1 2 'green))
(define c  (make-color 'blue))
 
(show 
 (record-compose (<point cp)                 ; polymorphic import - only fields x and y of cp taken
                 (color c)                   ; monomorphic import
                 (color-point (x 8)          ; override imported field
                              (extra 'hi))))                 
                                      
                                         ;==> (color-point (extra hi) (hue blue) (x 8) (y 2))

; Small module-functor example:
  
(define-record-type monoid #f #f 
  (mult monoid.mult) 
  (one  monoid.one))

(define-record-type abelian-group #f #f 
  (add  group.add) 
  (zero group.zero)
  (sub  group.sub))

(define-record-type ring #f #f
  (mult ring.mult) 
  (one  ring.one)
  (add  ring.add) 
  (zero ring.zero)
  (sub  ring.sub))

(define integer-monoid (monoid (mult *) 
                               (one  1)))

(define integer-group (abelian-group (add  +)
                                     (zero 0)
                                     (sub  -)))

(define (make-ring g m)          ; simple "functor"
  (record-compose (monoid m)
                  (abelian-group g)
                  (ring)))

(define integer-ring (make-ring integer-group 
                                integer-monoid))
  
((ring.add integer-ring) 1 2)    ;==> 3

; Example of tree data type

(define-record-scheme <tree #f <tree?) 

(define-record-type (node <tree) make-node node?
  (lhs node.lhs)
  (rhs node.rhs))

(define-record-type (leaf <tree) make-leaf leaf?
  (val leaf.val))

(define (tree->list t)
  (cond
    ((leaf? t) (leaf.val t))
    ((node? t) (cons (tree->list (node.lhs t))
                     (tree->list (node.rhs t))))))

(define t 
  (make-node (make-node (make-leaf 1)
                        (make-leaf 2))
             (make-leaf 3)))

(<tree? t)         ;==> #t
(tree->list t)     ;==> ((1 . 2) . 3)
