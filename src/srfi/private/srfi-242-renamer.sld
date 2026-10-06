;;; (srfi private srfi-242-renamer): SRFI 242's reference/srfi-242/renamer.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi renamer) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-renamer)
  (export renamer)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
	  (srfi private srfi-242-make-identifier-hashtable))
  (begin

  (define renamer
    (lambda ()
      (let ([table (make-identifier-hashtable)])
        (lambda (id)
          (assert (identifier? id))
          (or (hashtable-ref table id #f)
              (with-syntax ([(tmp) (generate-temporaries (list id))])
                (hashtable-set! table id #'tmp)
                #'tmp))))))))
