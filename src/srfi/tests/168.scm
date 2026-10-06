;;; Tests for SRFI 168: the sample implementation's srfi/168/test.sld
;;; (reference/srfi-168/168-test.sld), with chibi's
;;; (test [name] expected expr) defined as test-equal.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 158)
        (srfi 128) (srfi 146 hash) (srfi 167 memory) (srfi 167 engine)
        (srfi 168) (srfi 173))

(define-syntax test
  (syntax-rules ()
    ((_ expected expr) (test-equal expected expr))
    ((_ name expected expr) (test-equal name expected expr))))


    (define (run-tests)
      (define engine (make-default-engine))

      (define (triplestore)
        (nstore engine (list 42 1337) '(uid key value)))

      (test "ask empty triplestore"
        #f
        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))
          ;; ask
          (let ((out (engine-in-transaction
                      engine okvs
                      (lambda (transaction)
                        (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
            (engine-close engine okvs)
            out)))

      (test "add and ask triplestore"
        #t
        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))
          ;; add
          (engine-in-transaction
           engine okvs
           (lambda (transaction)
             (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))))
          ;; ask
          (let ((out
                 (engine-in-transaction
                  engine okvs
                  (lambda (transaction)
                    (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
            (engine-close engine okvs)
            out)))

      (test "add, rm and ask triplestore"
        #f
        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))

          (let ((out
                 (engine-in-transaction
                  engine okvs
                  (lambda (transaction)
                    ;; add!
                    (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))
                    ;; remove!
                    (nstore-delete! transaction triplestore '("P4X432" blog/title "hyper.dev"))
                    ;; ask
                    (nstore-ask? transaction triplestore '("P4X432" blog/title "hyper.dev"))))))
            (engine-close engine okvs)
            out)))

      (test "blog query post titles"
        '("DIY a database" "DIY a full-text search engine")

        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))
          (engine-in-transaction
           engine okvs
           (lambda (transaction)
             ;; add hyper.dev blog posts
             (nstore-add! transaction triplestore '("P4X432" blog/title "hyper.dev"))
             (nstore-add! transaction triplestore '("123456" post/title "DIY a database"))
             (nstore-add! transaction triplestore '("123456" post/blog "P4X432"))
             (nstore-add! transaction triplestore '("654321" post/title "DIY a full-text search engine"))
             (nstore-add! transaction triplestore '("654321" post/blog "P4X432"))
             ;; add dthompson.us blog posts
             (nstore-add! transaction triplestore '("1" blog/title "dthompson.us"))
             (nstore-add! transaction triplestore '("2" post/title "Haunt 0.2.4 released"))
             (nstore-add! transaction triplestore '("2" post/blog "1"))
             (nstore-add! transaction triplestore '("3" post/title "Haunt 0.2.3 released"))
             (nstore-add! transaction triplestore '("3" post/blog "1"))))
          ;; query
          (let ()
            (define query
              (lambda (transaction blog/title)
                (generator->list (nstore-query
                                  (nstore-from transaction triplestore
                                               (list (nstore-var 'blog/uid)
                                                     'blog/title
                                                     blog/title))
                                  (nstore-where transaction triplestore
                                                (list (nstore-var 'post/uid)
                                                      'post/blog
                                                      (nstore-var 'blog/uid)))
                                  (nstore-where transaction triplestore
                                                (list (nstore-var 'post/uid)
                                                      'post/title
                                                      (nstore-var 'post/title)))))))
            (let* ((out (engine-in-transaction engine okvs (lambda (transaction) (query transaction "hyper.dev"))))
                   (out (map (lambda (x) (hashmap-ref x 'post/title)) out)))
              (engine-close engine okvs)
              out))))

      (test "nstore-from limit and offset"
        '("hyperdev.fr")
        (let ((okvs (engine-open engine #f))
              (triplestore (triplestore)))
          ;; add!
          (nstore-add! okvs triplestore '("P4X432" blog/title "hyper.dev"))
          (nstore-add! okvs triplestore '("P4X433" blog/title "hyperdev.fr"))
          (nstore-add! okvs triplestore '("P4X434" blog/title "hypermove.net"))
          (let ((out (engine-in-transaction
                      engine okvs
                      (lambda (transaction)
                        (generator-map->list
                         (lambda (item) (hashmap-ref item 'title))
                         (nstore-from transaction triplestore (list (nstore-var 'uid)
                                                                    'blog/title
                                                                    (nstore-var 'title))
                                      `((limit . 1) (offset . 1))))))))
            (engine-close engine okvs)
           out)))

      (test "nstore validation add via hooks"
            #t
            (let* ((okvs (engine-open engine #f))
                   (triplestore (triplestore))
                   (hook (nstore-hook-on-add triplestore)))
              (hook-add! hook (lambda (nstore items)
                                (when (string=? (car items) "private")
                                  (error 'nstore-hook "private is private" items))))
              (guard (ex (else #t))
                (nstore-add! okvs triplestore '("private" private "private"))
                #f)))

      (test "nstore validation delete via hooks"
            #t
            (let* ((okvs (engine-open engine #f))
                   (triplestore (triplestore))
                   (hook (nstore-hook-on-delete triplestore)))
              (hook-add! hook (lambda (nstore items)
                                (when (string=? (car items) "private")
                                  (error 'nstore-hook "private is private" items))))
              (guard (ex (else #t))
                (nstore-delete! okvs triplestore '("private" private "private"))
                #f)))

      )

(test-begin "srfi-168")
(run-tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-168")
  (exit (if (zero? failures) 0 1)))
