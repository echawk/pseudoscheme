;;;; SPDX-FileCopyrightText: 2009 Joo ChurlSoo
;;;;
;;;; SPDX-License-Identifier: MIT

;;; define-lambda-object --- define-syntax

(define-syntax unquote-get
  (syntax-rules ()
    ((unquote-get symbol ((n0 d0) (n1 d1) ...))
     (if (eq? symbol 'n0)
         d0
         (unquote-get symbol ((n1 d1) ...))))
    ((unquote-get symbol ())
     (error 'define-lambda-object "absent field" symbol))))

(define-syntax unquote-get*
  (syntax-rules ()
    ((unquote-get* symbol (n0 n1 ...))
     (if (eq? symbol 'n0)
         n0
         (unquote-get* symbol (n1 ...))))
    ((unquote-get* symbol ())
     (error 'define-lambda-object "not available inspection" symbol))))

(define-syntax unquote-set!
  (syntax-rules ()
    ((unquote-set! symbol new-val (n0 n1 ...) fi)
     (if (eq? symbol 'n0)
         (set! n0 new-val)
         (unquote-set! symbol new-val (n1 ...) fi)))
    ((unquote-set! symbol new-val () fi)
     (if (memq symbol 'fi)
         (error 'define-lambda-object "read-only field" symbol)
         (error 'define-lambda-object "absent field" symbol)))))

(define-syntax seq-lambda
  (syntax-rules ()
    ((seq-lambda () (r ...) () body)
     (lambda (r ...) body))
    ((seq-lambda () (r ...) (o oo ...) body)
     (lambda (r ... . z)
       (seq-lambda (z) () (o oo ...) body)))
    ((seq-lambda (z) () ((n d) . e) body)
     (let ((y (if (null? z) z (cdr z)))
           (n (if (null? z) d (car z))))
       (seq-lambda (y) () e body)))
    ((seq-lambda (z) () () body)
     (if (null? z)
         body
         (error 'define-lambda-object "too many arguments" z)))))

;; Choose either procedure type or macro type according to your implementation.
;; 1. procedure opt-key
(define (opt-key z k d)
  (let ((x (car z)) (y (cdr z)))
    (if (null? y)
        (cons d z)
        (if (eq? k x)
            y
            (let lp ((head (list x (car y))) (tail (cdr y)))
              (if (null? tail)
                  (cons d z)
                  (let ((x (car tail)) (y (cdr tail)))
                    (if (null? y)
                        (cons d z)
                        (if (eq? k x)
                            (cons (car y) (append head (cdr y)))
                            (lp (cons x (cons (car y) head)) (cdr y)))))))))))
;; 2. macro opt-key!
(define-syntax opt-key!
  (syntax-rules ()
    ((opt-key! z n d)
     (let ((x (car z)) (y (cdr z)))
       (if (null? y)
           d
           (if (eq? 'n x)
               (begin (set! z (cdr y)) (car y))
               (let lp ((head (list x (car y)))
                        (tail (cdr y)))
                 (if (null? tail)
                     d
                     (let ((x (car tail)) (y (cdr tail)))
                       (if (null? y)
                           d
                           (if (eq? 'n x)
                               (begin (set! z (append head (cdr y))) (car y))
                               (lp (cons x (cons (car y) head)) (cdr y)))))))))))))

(define-syntax key-lambda
  (syntax-rules ()
    ((key-lambda () (r ...) () body)
     (lambda (r ...) body))
    ((key-lambda () (r ...) (o oo ...) body)
     (lambda (r ... . z)
       (key-lambda (z) () (o oo ...) body)))
    ((key-lambda (z) () ((n d) . e) body)
     ;; 1. procedure opt-key
     (let* ((y (if (null? z) (cons d z) (opt-key z 'n d)))
            (n (car y))
            (y (cdr y)))
       (key-lambda (y) () e body)))
     ;; 2. macro opt-key!
     ;; (let ((n (if (null? z) d (opt-key! z n d))))
     ;;   (key-lambda (z) () e body)))
    ((key-lambda (z) () () body)
     (if (null? z)
         body
         (error 'define-lambda-object "too many arguments" z)))))

(define (check-duplicate ls err-str)
  (cond ((null? ls) #f)
        ((memq (car ls) (cdr ls)) (error 'define-lambda-object err-str (car ls)))
        (else (check-duplicate (cdr ls) err-str))))

(define (check-field part-list main-list cmp name err-str)
  (let lp ((part part-list) (main main-list))
    (if (null? part)
        main
        (if (null? main)
            (error 'define-lambda-object err-str name (car part))
            (let ((field (car part)))
              (if (cmp field (car main))
                  (lp (cdr part) (cdr main))
                  (let loop ((head (list (car main))) (tail (cdr main)))
                    (if (null? tail)
                        (error 'define-lambda-object err-str name field)
                        (if (cmp field (car tail))
                            (lp (cdr part) (append head (cdr tail)))
                            (loop (cons (car tail) head) (cdr tail)))))))))))

(define-syntax define-object
  (syntax-rules ()
    ((define-object name make-object make-object-by-name pred-object (gr ...) (gi ...) (fm ...) ((fi id) ...) (r ...) (o ...) (a ...) ((c cd) ...) ((v vd) ...) ((h hd) ...))
     (begin
       (define safe-parent
         (begin
           ;; check duplication
           (check-duplicate '(name gi ... gr ...) "duplicated group")
           (check-duplicate '(fm ... fi ... h ...) "duplicated field")
           ;; check field
           (check-field (gi 'read-write-field) '(fm ...) eq? 'gi "incompatible read-write field") ...
           (check-field (gi 'read-only-field) '(fi ...) eq? 'gi "incompatible read-only field") ...
           (check-field (gi 'required-field) '(r ...) eq? 'gi "incompatible required field") ...
           (check-field (gi 'optional-field) '(o ...) equal? 'gi "incompatible optional field") ...
           (check-field (gi 'automatic-field) '((c cd) ... (v vd) ... a ...) equal? 'gi "incompatible automatic field") ...
           (check-field (map car (gi 'common-field)) '(c ...) eq? 'gi "incompatible common field") ...
           (check-field (map car (gi 'virtual-field)) '(v ...) eq? 'gi "incompatible virtual field") ...
           (check-field (map car (gi 'hidden-field)) '(h ...) eq? 'gi "incompatible hidden field") ...
           (check-field (append (gr 'read-write-field) (gr 'read-only-field) (map car (gr 'hidden-field))) '(fm ... fi ... h ...) eq? 'gr "incompatible whole field") ...
           (list gi ... gr ...)))
       (define safe-name 'tmp)
       ;; Alist, vector/enum, vector/alist or hashtable can be used instead of
       ;; unquote-get & unquote-set! according to your implementation.
       ;; cf. (eval-variant expression implementation-specific-namespace)
       ;; An example of vector/enum:
       ;; (define enum-a (make-enumeration '(fm ... fi ...)))
       ;; (define enum-m (make-enumeration '(fm ...)))
       ;; (define enum-index-a (enum-set-indexer enum-a))
       ;; (define enum-index-m (enum-set-indexer enum-m))
       ;; (define makers
       ;;        (let* ((c cd) ...)
       ;;          (cons (seq-lambda () (r ...) (o ...)
       ;;                            (let* (a ... (array (vector (lambda (x) (if (eq? enum-index-a x) fm (set! fm x))) ... (lambda (x) id) ...)))
       ;;                              (define *%lambda-object%*
       ;;                                (lambda (arg . args)
       ;;                                  (if (null? args)
       ;;                                      (let ((n (enum-index-a arg)))
       ;;                                        (if n
       ;;                                            ((vector-ref array n) enum-index-a)
       ;;                                            (error 'define-lambda-object "absent field" arg)))
       ;;                                      (if (null? (cdr args))
       ;;                                          (let ((n (enum-index-m arg)))
       ;;                                            (if n
       ;;                                                ((vector-ref array n) (car args))
       ;;                                                (if (enum-set-member? arg enum-a)
       ;;                                                    (error 'define-lambda-object "read-only field" arg)
       ;;                                                    (error 'define-lambda-object "absent field" arg))))
       ;;                                          safe-name))))
       ;;                              *%lambda-object%*))
       ;;                (key-lambda () (r ...) (o ...)
       ;;                            (let* (a ... (array (vector (lambda (x) (if (eq? enum-index-a x) fm (set! fm x))) ... (lambda (x) id) ...)))
       ;;                              (define *%lambda-object%*
       ;;                                (lambda (arg . args)
       ;;                                  (if (null? args)
       ;;                                      (let ((n (enum-index-a arg)))
       ;;                                        (if n
       ;;                                            ((vector-ref array n) enum-index-a)
       ;;                                            (error 'define-lambda-object "absent field" arg)))
       ;;                                      (if (null? (cdr args))
       ;;                                          (let ((n (enum-index-m arg)))
       ;;                                            (if n
       ;;                                                ((vector-ref array n) (car args))
       ;;                                                (if (enum-set-member? arg enum-a)
       ;;                                                    (error 'define-lambda-object "read-only field" arg)
       ;;                                                    (error 'define-lambda-object "absent field" arg))))
       ;;                                          safe-name))))
       ;;                              *%lambda-object%*)))))
       (define makers
         (let* ((c cd) ...)
           (cons (seq-lambda () (r ...) (o ...)
                             (let* (a ...)
                               (define *%lambda-object%*
                                 (lambda (arg . args)
                                   (if (null? args)
                                       (unquote-get arg ((fm fm) ... (fi id) ...))
                                       (if (null? (cdr args))
                                           (unquote-set! arg (car args) (fm ...) (fi ...))
                                           safe-name))))
                               *%lambda-object%*))
                 (key-lambda () (r ...) (o ...)
                             (let* (a ...)
                               (define *%lambda-object%*
                                 (lambda (arg . args)
                                   (if (null? args)
                                       (unquote-get arg ((fm fm) ... (fi id) ...))
                                       (if (null? (cdr args))
                                           (unquote-set! arg (car args) (fm ...) (fi ...))
                                           safe-name))))
                               *%lambda-object%*)))))
       (define make-object (car makers))
       (define make-object-by-name (cdr makers))
       ;; The predicate procedure is implementation dependant.
       (define (pred-object object)
         (and (eq? '*%lambda-object%* (object-name object)) ;mzscheme
              (let ((group (object #f #f #f)))
                (or (eq? safe-name group)
                    (let lp ((group-list (group 'parent)))
                      (if (null? group-list)
                          #f
                           (or (eq? safe-name (car group-list))
                               (lp ((car group-list) 'parent))
                               (lp (cdr group-list)))))))))
       (define name
         (let ((parent safe-parent)
               (constructor makers)
               (predicate pred-object)
               (read-write-field '(fm ...))
               (read-only-field '(fi ...))
               (required-field '(r ...))
               (optional-field '(o ...))
               (automatic-field '((c cd) ... (v vd) ... a ...))
               (common-field '((c cd) ...))
               (virtual-field '((v vd) ...))
               (hidden-field '((h hd) ...)))
           (lambda (symbol)
             (unquote-get* symbol (parent constructor predicate
                                          read-write-field read-only-field
                                          required-field optional-field
                                          automatic-field common-field
                                          virtual-field hidden-field)))))
       (define tmp (set! safe-name name))))))

(define-syntax define-make-object
  (lambda (x)
    (syntax-case x ()
      ((_ nm gr gi fm fi r o a c v h)
       (let ((name (syntax->datum #'nm)))
         (let ((make-obj (string->symbol (string-append "make-" (symbol->string name))))
               (make-obj-by-name (string->symbol (string-append "make-" (symbol->string name) "-by-name")))
               (pred-obj (string->symbol (string-append (symbol->string name) "?"))))
           (with-syntax
            ((make-object (datum->syntax #'nm make-obj))
             (make-object-by-name (datum->syntax #'nm make-obj-by-name))
             (pred-object (datum->syntax #'nm pred-obj)))
            #'(define-object nm make-object make-object-by-name pred-object gr gi fm fi r o a c v h))))))))

(define-syntax field-sort
  (syntax-rules (quote unquote quasiquote)
    ((field-sort gr gi (fm ...) fi r o a (c ...) v h (((,,n) d) . e))
     (field-sort gr gi (fm ... n) fi r o a (c ... (n d)) v h e))
    ((field-sort gr gi fm (fi ...) r o a (c ...) v h ((,,n d) . e))
     (field-sort gr gi fm (fi ... (n n)) r o a (c ... (n d)) v h e))
    ((field-sort gr gi fm fi r o (a ...) c v (h ...) ((',n d) . e))
     (field-sort gr gi fm fi r o (a ... (n d)) c v (h ... (n d)) e))
    ((field-sort gr gi fm (fi ...) r o a c (v ...) h ((`,n d) . e))
     (field-sort gr gi fm (fi ... (n d)) r o a c (v ... (n d)) h e))
    ((field-sort gr gi (fm ...) fi r o (a ...) c v h (((,n) d) . e))
     (field-sort gr gi (fm ... n) fi r o (a ... (n d)) c v h e))
    ((field-sort gr gi fm (fi ...) r o (a ...) c v h ((,n d) . e))
     (field-sort gr gi fm (fi ... (n n)) r o (a ... (n d)) c v h e))
    ((field-sort gr gi fm fi r (o ...) () () () (h ...) (('n d) . e))
     (field-sort gr gi fm fi r (o ... (n d)) () () () (h ... (n d)) e))
    ((field-sort gr gi (fm ...) fi r (o ...) () () () h (((n) d) . e))
     (field-sort gr gi (fm ... n) fi r (o ... (n d)) () () () h e))
    ((field-sort gr gi fm (fi ...) r (o ...) () () () h ((n d) . e))
     (field-sort gr gi fm (fi ... (n n)) r (o ... (n d)) () () () h e))
    ((field-sort gr gi (fm ...) fi (r ...) () () () () () ((n) . e))
     (field-sort gr gi (fm ... n) fi (r ... n) () () () () () e))
    ((field-sort gr gi fm (fi ...) (r ...) () () () () () (n . e))
     (field-sort gr gi fm (fi ... (n n)) (r ... n) () () () () () e))
    ((field-sort gr (name gi ...) fm fi r o a c v h ())
     (define-make-object name gr (gi ...) fm fi r o a c v h))))

(define-syntax group-sort
  (syntax-rules ()
    ((group-sort (gr ...) (gi ...) ((g) gg ...) f)
     (group-sort (gr ... g) (gi ...) (gg ...) f))
    ((group-sort (gr ...) (gi ...)  (g gg ...) f)
     (group-sort (gr ...) (gi ... g) (gg ...) f))
    ((group-sort () () g f)
     (group-sort () (g) () f))
    ((group-sort gr gi () f)
     (field-sort gr gi () () () () () () () () f))))

(define-syntax define-lambda-object
  (syntax-rules ()
    ((define-lambda-object g . f)
     (group-sort () () g f))))


;;; define-lambda-object --- define-macro

(define-macro (unquote-get symbol args)
  (if (null? args)
      `(error 'define-lambda-object "absent field" ,symbol)
      (let ((arg (car args)))
        `(if (eq? ,symbol ',(car arg))
             ,(cdr arg)
             (unquote-get ,symbol ,(cdr args))))))

(define-macro (unquote-get* symbol args)
  (if (null? args)
      `(error 'define-lambda-object "not available inspection" ,symbol)
      (let ((arg (car args)))
        `(if (eq? ,symbol ',arg)
             ,arg
             (unquote-get* ,symbol ,(cdr args))))))

(define-macro (unquote-set! symbol new-val args iargs)
  (define (lp args)
    (if (null? args)
        `(if (memq ,symbol ',iargs)
             (error 'define-lambda-object "read-only field" ,symbol)
             (error 'define-lambda-object "absent field" ,symbol))
        (let ((arg (car args)))
          `(if (eq? ,symbol ',arg)
               (set! ,arg ,new-val)
               ,(lp (cdr args))))))
  (lp args))

(define-macro (seq-lambda r o body)
  (define (opt-seq z cls body)
    (if (null? cls)
        `(if (null? ,z)
             ,body
             (error 'define-lambda-object "too many arguments" ,z))
        (let ((cl (car cls)))
          `(let ((,(car cl) (if (null? ,z) ,(cadr cl) (car ,z)))
                 (,z (if (null? ,z) ,z (cdr ,z))))
             ,(opt-seq z (cdr cls) body)))))
  (if (null? o)
      `(lambda ,r ,body)
      (let ((z (gensym)))
        `(lambda (,@r . ,z)
           ,(opt-seq z o body)))))

;; Choose either procedure type or macro type according to your implementation.
;; 1. procedure field-key
(define (field-key z k d)
  (let ((x (car z)) (y (cdr z)))
    (if (null? y)
        (cons d z)
        (if (eq? k x)
            y
            (let lp ((head (list x (car y))) (tail (cdr y)))
              (if (null? tail)
                  (cons d z)
                  (let ((x (car tail)) (y (cdr tail)))
                    (if (null? y)
                        (cons d z)
                        (if (eq? k x)
                            (cons (car y) (append head (cdr y)))
                            (lp (cons x (cons (car y) head)) (cdr y)))))))))))
;; 2. macro field-key!
(define-macro (field-key! z n d)
  (let ((x (gensym)) (y (gensym)) (head (gensym)) (tail (gensym)))
    `(let ((,x (car ,z)) (,y (cdr ,z)))
       (if (null? ,y)
           ,d
           (if (eq? ',n ,x)
               (begin (set! ,z (cdr ,y)) (car ,y))
               (let lp ((,head (list ,x (car ,y)))
                        (,tail (cdr ,y)))
                 (if (null? ,tail)
                     ,d
                     (let ((,x (car ,tail)) (,y (cdr ,tail)))
                       (if (null? ,y)
                           ,d
                           (if (eq? ',n ,x)
                               (begin (set! ,z (append ,head (cdr ,y)))
                                      (car ,y))
                               (lp (cons ,x (cons (car ,y) ,head)) (cdr ,y))))))))))))

(define-macro (key-lambda r o body)
  (define (opt-key z cls body)
    (if (null? cls)
        `(if (null? ,z)
             ,body
             (error 'define-lambda-object "too many arguments" ,z))
        (let ((cl (car cls)))
          (let ((var (car cl)) (val (cadr cl)))
            ;; 1. procedure field-key
            `(let* ((,z (if (null? ,z) (cons ,val ,z) (field-key ,z ',var ,val)))
                    (,var (car ,z))
                    (,z (cdr ,z)))
            ;; 2. macro field-key!
            ;; `(let* ((,var (if (null? ,z) ,val (field-key! ,z ,var ,val))))
               ,(opt-key z (cdr cls) body))))))
  (if (null? o)
      `(lambda ,r ,body)
      (let ((z (gensym)))
        `(lambda (,@r . ,z) ,(opt-key z o body)))))

(define (check-duplicate ls err-str)
  (cond ((null? ls) #f)
        ((memq (car ls) (cdr ls)) (error 'define-lambda-object err-str (car ls)))
        (else (check-duplicate (cdr ls) err-str))))

(define (check-field part-list main-list cmp name err-str)
  (let lp ((part part-list) (main main-list))
    (if (null? part)
        main
        (if (null? main)
            (error 'define-lambda-object err-str name (car part))
            (let ((field (car part)))
              (if (cmp field (car main))
                  (lp (cdr part) (cdr main))
                  (let loop ((head (list (car main))) (tail (cdr main)))
                    (if (null? tail)
                        (error 'define-lambda-object err-str name field)
                        (if (cmp field (car tail))
                            (lp (cdr part) (append head (cdr tail)))
                            (loop (cons (car tail) head) (cdr tail)))))))))))

;; (define (number-alist ls)
;;   (let loop ((ls ls) (n 0))
;;     (if (null? ls)
;;      '()
;;      (cons (cons (car ls) n) (loop (cdr ls) (+ 1 n))))))

(define-macro (define-object name gr gi fm fi r o a c v h)
  (let ((safe-name (gensym))
        (safe-parent (gensym))
        (arg (gensym))
        (args (gensym))
        (makers (gensym))
        ;; (alist-a (gensym))
        ;; (alist-m (gensym))
        ;; (array (gensym))
        ;; (safe-eq (gensym))
        ;; (safe-arg (gensym))
        (group-name (symbol->string name)))
    (let ((make-object (string->symbol (string-append "make-" group-name)))
          (make-object-by-name (string->symbol (string-append "make-" group-name "-by-name")))
          (pred-object (string->symbol (string-append group-name "?"))))
      `(begin
         (define ,safe-parent
           (begin
             ;; check duplication
             (check-duplicate (append (list ',name) ',gi ',gr) "duplicated group")
             (check-duplicate ',(append fm (map car fi) (map car h)) "duplicated field")
             ;; check field
             (for-each (lambda (g y)
                         (check-field (g 'read-write-field) ',fm eq? y "incompatible read-write field")
                         (check-field (g 'read-only-field) ',(map car fi) eq? y "incompatible read-only field")
                         (check-field (g 'required-field) ',r eq? y "incompatible required field")
                         (check-field (g 'optional-field) ',o equal? y "incompatible optional field")
                         (check-field (g 'automatic-field) ',(append c v a) equal? y "incompatible automatic field")
                         (check-field (map car (g 'common-field)) ',(map car c) eq? y "incompatible common field")
                         (check-field (map car (g 'virtual-field)) ',(map car v) eq? y "incompatible virtual field")
                         (check-field (map car (g 'hidden-field)) ',(map car h) eq? y "incompatible hidden field"))
                       (list ,@gi) ',gi)
             (for-each (lambda (g y)
                         (check-field (append (g 'read-write-field) (g 'read-only-field) (map car (g 'hidden-field))) ',(append fm (map car fi) (map car h)) eq? y "incompatible whole field"))
                       (list ,@gr) ',gr)
             (list ,@gi ,@gr)))
         ;; Alist, vector/enum, vector/alist or hashtable can be used instead of
         ;; unquote-get & unquote-set! according to your implementation.
         ;; cf. (eval-variant expression implementation-specific-namespace)
         ;; An example of vector/alist:
         ;; (define ,alist-a (number-alist ',(append fm (map car fi))))
         ;; (define ,alist-m (number-alist ',fm))
         ;; (define ,makers
         ;;   (let* ,c
         ;;     (cons (seq-lambda ,r ,o
         ;;                    (let* (,@a (,array (vector ,@(map (lambda (f) `(lambda (,safe-arg) (if (eq? ,safe-arg ',safe-eq) ,f (set! ,f ,safe-arg)))) fm)  ,@(map (lambda (f) `(lambda (,safe-arg) ,f)) (map cdr fi)))))
         ;;                      (define *%lambda-object%*
         ;;                        (lambda (,arg . ,args)
         ;;                          (if (null? ,args)
         ;;                              (let ((pair (assq ,arg ,alist-a)))
         ;;                                (if pair
         ;;                                    ((vector-ref ,array (cdr pair)) ',safe-eq)
         ;;                                    (error 'define-lambda-object "absent field" ,arg)))
         ;;                              (if (null? (cdr ,args))
         ;;                                  (let ((pair (assq ,arg ,alist-m)))
         ;;                                    (if pair
         ;;                                        ((vector-ref ,array (cdr pair)) (car ,args))
         ;;                                        (if (assq ,arg ',fi)
         ;;                                            (error 'define-lambda-object "read-only field" ,arg)
         ;;                                            (error 'define-lambda-object "absent field" ,arg))))
         ;;                                  ,safe-name))))
         ;;                      *%lambda-object%*))
         ;;        (key-lambda ,r ,o
         ;;                    (let* (,@a (,array (vector ,@(map (lambda (f) `(lambda (,safe-arg) (if (eq? ,safe-arg ',safe-eq) ,f (set! ,f ,safe-arg)))) fm)  ,@(map (lambda (f) `(lambda (,safe-arg) ,f)) (map cdr fi)))))
         ;;                      (define *%lambda-object%*
         ;;                        (lambda (,arg . ,args)
         ;;                          (if (null? ,args)
         ;;                              (let ((pair (assq ,arg ,alist-a)))
         ;;                                (if pair
         ;;                                    ((vector-ref ,array (cdr pair)) ',safe-eq)
         ;;                                    (error 'define-lambda-object "absent field" ,arg)))
         ;;                              (if (null? (cdr ,args))
         ;;                                  (let ((pair (assq ,arg ,alist-m)))
         ;;                                    (if pair
         ;;                                        ((vector-ref ,array (cdr pair)) (car ,args))
         ;;                                        (if (assq ,arg ',fi)
         ;;                                            (error 'define-lambda-object "read-only field" ,arg)
         ;;                                            (error 'define-lambda-object "absent field" ,arg))))
         ;;                                  ,safe-name))))
         ;;                      *%lambda-object%*)))))
         (define ,makers
           (let* ,c
             (cons (seq-lambda ,r ,o
                               (let* ,a
                                 (define *%lambda-object%*
                                   (lambda (,arg . ,args)
                                     (if (null? ,args)
                                         (unquote-get ,arg ,(append (map cons fm fm) fi))
                                         (if (null? (cdr ,args))
                                             (unquote-set! ,arg (car ,args) ,fm ,(map car fi))
                                             ,safe-name))))
                                 *%lambda-object%*))
                   (key-lambda ,r ,o
                               (let* ,a
                                 (define *%lambda-object%*
                                   (lambda (,arg . ,args)
                                     (if (null? ,args)
                                         (unquote-get ,arg ,(append (map cons fm fm) fi))
                                         (if (null? (cdr ,args))
                                             (unquote-set! ,arg (car ,args) ,fm ,(map car fi))
                                             ,safe-name))))
                                 *%lambda-object%*)))))
         (define ,make-object (car ,makers))
         (define ,make-object-by-name (cdr ,makers))
         ;; The predicate procedure is implementation dependant.
         (define (,pred-object object)
           (and (eq? '*%lambda-object%* (object-name object)) ;mzscheme
                (let ((group (object #f #f #f)))
                  (or (eq? ,safe-name group)
                      (let lp ((group-list (group 'parent)))
                        (if (null? group-list)
                            #f
                            (or (eq? ,safe-name (car group-list))
                                (lp ((car group-list) 'parent))
                                (lp (cdr group-list)))))))))
         (define ,name
           (let ((parent ,safe-parent)
                 (constructor ,makers)
                 (predicate ,pred-object)
                 (read-write-field ',fm)
                 (read-only-field ',(map car fi))
                 (required-field ',r)
                 (optional-field ',o)
                 (automatic-field ',(append c v a))
                 (common-field ',c)
                 (virtual-field ',v)
                 (hidden-field ',h))
             (lambda (symbol)
               (unquote-get* symbol (parent constructor predicate
                                            read-write-field read-only-field
                                            required-field optional-field
                                            automatic-field common-field
                                            virtual-field hidden-field)))))
         (define ,safe-name ,name)))))

(define-macro (define-lambda-object group . field)
  (define (field-sort gr gi field fm fi r o a c v h)
    (if (null? field)
        `(define-object ,(car gi) ,gr ,(cdr gi) ,fm ,fi ,r ,o ,a ,c ,v ,h)
        (let ((vars (car field)))
          (if (symbol? vars)            ;r
              (if (and (null? o) (null? a) (null? c) (null? v))
                  (field-sort gr gi (cdr field)
                              fm (append fi (list (cons vars vars)))
                              (append r (list vars)) o a c v h)
                  (error 'define-lambda-object "required-field should precede optional-field and automatic-field" vars))
              (let ((var (car vars)))
                (if (symbol? var)
                    (if (null? (cdr vars)) ;(r)
                        (if (and (null? o) (null? a) (null? c) (null? v))
                            (field-sort gr gi (cdr field)
                                        (append fm vars) fi
                                        (append r vars) o a c v h)
                            (error 'define-lambda-object "required-field should precede optional-field and automatic-field" var))
                        (if (null? (cddr vars)) ;(o val)
                            (if (and (null? a) (null? c) (null? v))
                                (field-sort gr gi (cdr field)
                                            fm (append fi (list (cons var var)))
                                            r (append o (list vars)) a c v h)
                                (error 'define-lambda-object "optional-field should precede automatic-field" var))
                            (error 'define-lambda-object "incorrect syntax" vars)))
                    (if (and (pair? (cdr vars)) (null? (cddr vars)))
                        (let ((b (car var)))
                          (if (symbol? b)
                              (if (null? (cdr var)) ;((o) val)
                                  (if (and (null? a) (null? c) (null? v))
                                      (field-sort gr gi (cdr field)
                                                  (append fm var) fi
                                                  r (append o (list (cons b (cdr vars)))) a c v h)
                                      (error 'define-lambda-object "optional-field should precede automatic-field" b))
                                  (if (null? (cddr var))
                                      (let ((d (cadr var)))
                                        (if (symbol? d)
                                            (if (eq? 'unquote b) ;(,a val)
                                                (field-sort gr gi (cdr field)
                                                            fm (append fi (list (cons d d)))
                                                            r o (append a (list (cons d (cdr vars)))) c v h)
                                                (if (eq? 'quote b) ;('o val)
                                                    (if (and (null? a) (null? c) (null? v))
                                                        (field-sort gr gi (cdr field)
                                                                    fm fi
                                                                    r (append o (list (cons d (cdr vars)))) a c v (append h (list (cons d (cdr vars)))))
                                                        (error 'define-lambda-object "optional-field should precede automatic-field" b))
                                                    (error 'define-lambda-object "incorrect syntax" vars)))
                                            (if (and (eq? 'unquote (car d)) (symbol? (cadr d)) (null? (cddr d)))
                                                (if (eq? 'unquote b) ;(,,a val)
                                                    (field-sort gr gi (cdr field)
                                                                fm (append fi (list (cons (cadr d) (cadr d))))
                                                                r o a (append c (list (cons (cadr d) (cdr vars)))) v h)
                                                    (if (eq? 'quote b) ;(',a val)
                                                        (field-sort gr gi (cdr field)
                                                                    fm fi
                                                                    r o (append a (list (cons (cadr d) (cdr vars)))) c v (append h (list (cons (cadr d) (cdr vars)))))
                                                        (if (eq? 'quasiquote b) ;(`,a val)
                                                            (field-sort gr gi (cdr field)
                                                                        fm (append fi (list (cons (cadr d) (cadr vars))))
                                                                        r o a c (append v (list (cons (cadr d) (cdr vars)))) h)
                                                            (error 'define-lambda-object "incorrect syntax" vars))))
                                                (error 'define-lambda-object "incorrect syntax" vars))))
                                      (error 'define-lambda-object "incorrect syntax" vars)))
                              (if (and (null? (cdr var)) (eq? 'unquote (car b)) (null? (cddr b)))
                                  (if (symbol? (cadr b)) ;((,a) val)
                                      (field-sort gr gi (cdr field)
                                                  (append fm (cdr b)) fi
                                                  r o (append a (list (cons (cadr b) (cdr vars)))) c v h)
                                      (let ((e (cadr b)))
                                        (if (and (eq? 'unquote (car e)) (symbol? (cadr e)) (null? (cddr e))) ;((,,a) val)
                                            (field-sort gr gi (cdr field)
                                                        (append fm (cdr e)) fi
                                                        r o a (append c (list (cons (cadr e) (cdr vars)))) v h)
                                            (error 'define-lambda-object "incorrect syntax" vars))))
                                  (error 'define-lambda-object "incorrect syntax" vars))))
                        (error 'define-lambda-object "incorrect syntax" vars))))))))
  (define (group-sort gr gi gg field)
    (if (pair? gg)
        (let ((g (car gg)))
          (if (pair? g)
              (group-sort (append gr g) gi (cdr gg) field)
              (group-sort gr (append gi (list g)) (cdr gg) field)))
        (if (symbol? gg)
            (group-sort gr (cons gg gi) '() field)
            (field-sort gr gi field '() '() '() '() '() '() '() '()))))
  (group-sort '() '() group field))
