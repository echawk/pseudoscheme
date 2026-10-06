;;; (srfi private srfi-59-support): what SLIB's vicinity code (SRFI 59)
;;; expects of its host, shared with SRFI 96, which exports most of it:
;;; software-type, getenv, slib:warn, slib:error, and the load pathname
;;; that with-load-pathname binds and program-vicinity reads.
(define-library (srfi private srfi-59-support)
  (export software-type getenv slib:warn slib:error
          current-load-pathname with-load-pathname)
  (import (scheme base)
          (scheme write)
          (scheme process-context)
          (only (pseudoscheme lisp) lisp-value lisp-symbol)
          (prefix (only (cl common-lisp) namestring pathnamep) cl:))
  (begin
    ;; SRFI 96: "The software-type of Linux is unix.  The software-type
    ;; of MS-Windows and Vista is ms-dos."  macOS is unix too.
    (define (software-type)
      (cond-expand
        (windows 'ms-dos)
        (else 'unix)))

    (define getenv get-environment-variable)

    (define (display-all args port)
      (for-each (lambda (x) (display " " port) (display x port)) args))

    (define (slib:warn . args)
      (let ((port (current-error-port)))
        (display "Warn:" port)
        (display-all args port)
        (newline port)))

    (define (slib:error . args)
      (if (and (pair? args) (string? (car args)))
          (apply error args)
          (apply error "slib:error" args)))

    ;; The pathname with-load-pathname binds; failing that, the directory
    ;; of the program or library file the R7RS front end is running
    ;; (Lisp's pseudoscheme-r7rs::*include-directory*); failing that, #f.
    (define load-pathname (make-parameter #f))

    (define (current-load-pathname)
      (or (load-pathname)
          (let ((dir (lisp-value (lisp-symbol "*include-directory*" "pseudoscheme-r7rs"))))
            (and (cl:pathnamep dir) (cl:namestring dir)))))

    (define (with-load-pathname path thunk)
      (parameterize ((load-pathname path))
        (thunk)))))
