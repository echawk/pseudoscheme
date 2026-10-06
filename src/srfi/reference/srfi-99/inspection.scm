;;; The body of the library (srfi :99 records inspection), from srfi-99.sls
;;; (William D Clinger's reference implementation), unmodified.
  ; The record? predicate is already defined by (rnrs records inspection).
  
  ; The record-rtd procedure is already defined by (rnrs records inspection).
  
  (define rtd-name record-type-name)
  
  (define rtd-parent record-type-parent)
  
  (define rtd-field-names record-type-field-names)
  
  (define (rtd-all-field-names rtd)
    (define (loop rtd othernames)
      (let ((parent (rtd-parent rtd))
            (names (append (vector->list
                            (rtd-field-names rtd))
                           othernames)))
        (if parent
            (loop parent names)
            (list->vector names))))
    (loop rtd '()))
  
  (define (rtd-field-mutable? rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-field-mutable? rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-field-mutable?
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))

  
