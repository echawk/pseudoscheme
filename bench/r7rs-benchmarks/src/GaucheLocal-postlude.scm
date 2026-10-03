(import (only (gauche base) gauche-version))
(define (this-scheme-implementation-name) (string-append "gauche-" (gauche-version)))
