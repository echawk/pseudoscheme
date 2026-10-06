(import (scheme base)
(srfi 158)
(srfi 190)
(srfi 64))

(test-begin "SRFI 190")

(test-group "Coroutine Generators"
  (define g
    (coroutine-generator
      (do ((i 0 (+ i 1)))
          ((<= 3 i))
        (yield i))))

  (define-coroutine-generator (h n)
    (do ((i 0 (+ i 1)))
        ((<= 3 i))
      (yield i)))

  (test-equal '(0 1 2) (generator->list g))
  (test-equal '(0 1 2) (generator->list (h 3))))

(test-end)