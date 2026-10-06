;;; (srfi private srfi-215-parameter): settable parameters for SRFI 215.
;;;
;;; SRFI 215 sets current-log-callback and current-log-fields by calling
;;; them with an argument, as well as parameterizing them; Pseudoscheme's
;;; R7RS parameter objects take no argument.  (settable-parameter P)
;;; returns a procedure that, called with no argument, returns P's
;;; current value and, called with one, sets it (through P's converter),
;;; and that parameterize accepts in place of P: it shares P's state.
;;;
;;; This reaches into src/r7rs/rts.lisp (the parameter-state struct and
;;; the *parameter-states* table), so it must follow that file.
(define-library (srfi private srfi-215-parameter)
  (export settable-parameter)
  (import (scheme base) (pseudoscheme lisp))
  (begin
    (define make-settable
      (begin
        (lisp-eval-string "
(defun pseudoscheme-r7rs::%srfi-215-settable-parameter (param)
  (let ((state (gethash param pseudoscheme-r7rs::*parameter-states*)))
    (unless state (error \"settable-parameter: not a parameter: ~S\" param))
    (let ((settable
            (lambda (&optional (value nil value-p))
              (if value-p
                  (let ((converter (pseudoscheme-r7rs::parameter-state-converter state)))
                    (setf (pseudoscheme-r7rs::parameter-state-value state)
                          (if converter (funcall converter value) value))
                    (values))
                  (pseudoscheme-r7rs::parameter-state-value state)))))
      (setf (gethash settable pseudoscheme-r7rs::*parameter-states*) state)
      settable)))")
        (lisp-function "%srfi-215-settable-parameter" "pseudoscheme-r7rs")))

    (define (settable-parameter param)
      (lisp-funcall make-settable param))))
