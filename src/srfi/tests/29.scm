;;; Tests for SRFI 29, from the example in the SRFI document.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 29))

(test-begin "srfi-29")

(test-assert (symbol? (current-language)))
(test-assert (symbol? (current-country)))
(test-assert (list? (current-locale-details)))

(current-language 'en)
(current-country 'us)
(current-locale-details '())
(test-eq 'en (current-language))
(test-eq 'us (current-country))
(test-equal '() (current-locale-details))

(let ((translations
       '(((en) . ((time . "Its ~a, ~a.")
                  (goodbye . "Goodbye, ~a.")))
         ((fr) . ((time . "~1@*~a, c'est ~a.")
                  (goodbye . "Au revoir, ~a."))))))
  (for-each (lambda (translation)
              (let ((bundle-name (cons 'hello-program (car translation))))
                (if (not (load-bundle! bundle-name))
                    (begin
                      (declare-bundle! bundle-name (cdr translation))
                      (test-eqv #f (store-bundle! bundle-name))))))
            translations))

(define localized-message
  (lambda (message-name . args)
    (apply format (cons (localized-template 'hello-program message-name)
                        args))))

(test-equal "Its 12:00, Fred." (localized-message 'time "12:00" "Fred"))
(test-equal "Goodbye, Fred." (localized-message 'goodbye "Fred"))
(current-language 'fr)
(test-equal "Fred, c'est 12:00." (localized-message 'time "12:00" "Fred"))
(test-equal "Au revoir, Fred." (localized-message 'goodbye "Fred"))

;; A more specific bundle wins; missing keys fall back to less specific ones.
(declare-bundle! '(hello-program fr ca) '((goodbye . "Salut, ~a.")))
(current-country 'ca)
(test-equal "Salut, Fred." (localized-message 'goodbye "Fred"))
(test-equal "Fred, c'est 12:00." (localized-message 'time "12:00" "Fred"))
;; Redeclaring replaces.
(declare-bundle! '(hello-program fr ca) '((goodbye . "Bye, ~a.")))
(test-equal "Bye, Fred." (localized-message 'goodbye "Fred"))
(test-eqv #f (localized-template 'hello-program 'nonesuch))
(test-eqv #f (localized-template 'no-such-package 'goodbye))
;; The default translation, named by the package alone.
(declare-bundle! '(other) '((hi . "hi")))
(test-equal "hi" (localized-template 'other 'hi))

(test-eqv #f (load-bundle! '(hello-program de)))
(test-eqv #f (store-bundle '(hello-program fr)))

;; format
(test-equal "a b" (format "~a ~a" "a" "b"))
(test-equal "\"a\" b" (format "~s ~a" "a" "b"))
(test-equal "x\n~" (format "x~%~~"))
(test-equal "1 2 1" (format "~a ~a ~0@*~a" 1 2))
(test-equal "c a b" (format "~2@*~a ~a ~a" 'a 'b 'c))
(test-error (format "~a"))
(test-error (format "~q" 1))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-29")
  (exit (if (zero? failures) 0 1)))
