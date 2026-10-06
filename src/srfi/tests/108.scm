;;; Tests for SRFI 108: the SRFI's translations and define-simple-constructor.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 108))
(test-begin "srfi-108")

(test-equal '($construct$:tag "...") '&tag{...})
(test-equal '($construct$:foo "s" $<<$ exp1 exp2 $>>$ "t") '&foo{s&[exp1 exp2]t})
(test-equal '($construct$:foo "_" ($construct$:bar "b") "_") '&foo{_&bar{b}_})
(test-equal '($construct$:foo "_" $<<$ ($construct$:bar "b") $>>$ "_") '&foo{_&[&bar{b}]_})
(test-equal '($construct$:name1 exps $>>$ "abc" ($construct$:name2 exps $>>$ "klm") "xyz")
  '&name1[exps]{abc&name2[exps]{klm}xyz})

(define (make-foo . args) (cons 'foo args))
(define-simple-constructor foo make-foo)
(test-equal '(foo 1 2 "text 3!") &foo[1 2]{text &[(+ 1 2)]!})
(test-equal '(foo "plain") &foo{plain})
(test-equal '(foo 1 "") &foo[1]{})
;; a constructor as a plain procedure: $<<$ and $>>$ are ""
(define ($construct$:bar . args) (apply string-append args))
(test-equal "s12t" &bar{s&["1" "2"]t})

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-108")
  (exit (if (zero? failures) 0 1)))
