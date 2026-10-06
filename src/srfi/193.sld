;;; SRFI 193: Command line.  Written for Pseudoscheme (the SRFI has no
;;; portable implementation: it describes what the implementation knows).
;;;
;;; The command-line program, bin/pseudoscheme, makes (command-line)
;;; (FILE ARG ...) when it runs FILE, and ("") otherwise, and records
;;; FILE's absolute name before running it; that is script-file.  A file
;;; run with load, from the REPL or elsewhere, is not a script here.
;;; command-line is R7RS's own, so that the two can be imported together;
;;; it is a procedure, not a parameter.
(define-library (srfi 193)
  (export command-line command-name command-args script-file script-directory)
  (import (scheme base) (scheme process-context)
          (only (pseudoscheme lisp) lisp-eval-string))
  (begin
    (define %script-file
      (lisp-eval-string "(lambda () (or ps-r7rs:*script-file* ps:false))"))

    (define (last-slash s)
      (let loop ((i (- (string-length s) 1)))
        (cond ((< i 0) #f)
              ((char=? (string-ref s i) #\/) i)
              (else (loop (- i 1))))))

    ;; the name without its directory, or a .scm, .sps or .ss extension
    (define (command-name)
      (let ((name (car (command-line))))
        (and (not (string=? name ""))
             (let* ((slash (last-slash name))
                    (base (if slash (substring name (+ slash 1) (string-length name)) name))
                    (n (string-length base)))
               (let loop ((exts '(".scm" ".sps" ".ss")))
                 (cond ((null? exts) base)
                       ((let ((m (string-length (car exts))))
                          (and (> n m) (string=? (substring base (- n m) n) (car exts))))
                        (substring base 0 (- n (string-length (car exts)))))
                       (else (loop (cdr exts)))))))))

    (define (command-args) (cdr (command-line)))

    (define (script-file) (%script-file))

    (define (script-directory)
      (let ((file (script-file)))
        (and file (substring file 0 (+ (last-slash file) 1)))))))
