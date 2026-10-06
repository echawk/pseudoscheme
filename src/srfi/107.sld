;;; SRFI 107: XML reader syntax.  Written for Pseudoscheme.  The reader
;;; reads #<tag attr="value">content</tag>, #<tag/>, #<!--comment-->,
;;; #<![CDATA[text]]> and #<?target content?> (src/quasi.lisp) as the
;;; SRFI's $xml-element$ and other forms; this library makes SXML of them:
;;;
;;;   #<p class="x">Hi &[name]!</p>  =>  (p (@ (class "x")) "Hi " "Ann" "!")
;;;
;;; An element is (name [(@ (attribute "value") ...)] content ...), a
;;; comment (*COMMENT* "text"), a processing instruction (*PI* target
;;; "content"), CDATA its text.  A name with a prefix is the symbol
;;; prefix:local; namespace declarations (xmlns, xmlns:prefix) are
;;; accepted but not resolved.  xml->string writes SXML as XML.
(define-library (srfi 107)
  (export $xml-element$ $xml-attribute$ $resolve-qname$ $xml-comment$
          $xml-CDATA$ $xml-processing-instruction$ $string$ $<<$ $>>$
          xml-element? xml-element-name xml-element-attributes
          xml-element-content xml->string)
  (import (scheme base) (scheme write) (rnrs syntax-case) (srfi 109))
  (begin
    (define-record-type xml-attribute (make-xml-attribute name value) xml-attribute?
      (name attribute-name) (value attribute-value))

    (define ($xml-attribute$ name . parts)
      (make-xml-attribute name (apply $string$ parts)))

    (define (make-xml-element name items)
      (let loop ((items items) (attributes '()) (content '()))
        (cond ((null? items)
               (let ((content (reverse content)))
                 (if (null? attributes)
                     (cons name content)
                     (cons name (cons (cons '@ (reverse attributes)) content)))))
              ((xml-attribute? (car items))
               (loop (cdr items)
                     (cons (list (attribute-name (car items)) (attribute-value (car items)))
                           attributes)
                     content))
              ((equal? (car items) "") (loop (cdr items) attributes content))
              (else (loop (cdr items) attributes (cons (car items) content))))))

    ;; namespace declarations are accepted, and not resolved
    (define-syntax $xml-element$
      (syntax-rules ()
        ((_ (namespace ...) name item ...) (make-xml-element name (list item ...)))))

    (define-syntax $resolve-qname$
      (lambda (x)
        (syntax-case x ()
          ((_ local) #''local)
          ((_ local prefix)
           (with-syntax ((name (datum->syntax
                                #'local
                                (string->symbol
                                 (string-append (symbol->string (syntax->datum #'prefix)) ":"
                                                (symbol->string (syntax->datum #'local)))))))
             #''name)))))

    (define ($xml-comment$ text) (list '*COMMENT* text))
    (define ($xml-CDATA$ text) text)
    (define ($xml-processing-instruction$ target . parts)
      (list '*PI* (string->symbol target) (apply $string$ parts)))

    (define (xml-element? x)
      (and (pair? x) (symbol? (car x)) (not (memq (car x) '(*COMMENT* *PI* @)))))
    (define (xml-element-name e) (car e))
    (define (xml-element-attributes e)
      (if (and (pair? (cdr e)) (pair? (cadr e)) (eq? (car (cadr e)) '@))
          (cdr (cadr e))
          '()))
    (define (xml-element-content e)
      (if (and (pair? (cdr e)) (pair? (cadr e)) (eq? (car (cadr e)) '@))
          (cddr e)
          (cdr e)))

    (define (escape s attribute?)
      (let ((out (open-output-string)))
        (string-for-each
         (lambda (c)
           (cond ((char=? c #\&) (write-string "&amp;" out))
                 ((char=? c #\<) (write-string "&lt;" out))
                 ((char=? c #\>) (write-string "&gt;" out))
                 ((and attribute? (char=? c #\")) (write-string "&quot;" out))
                 (else (write-char c out))))
         s)
        (get-output-string out)))

    (define (xml->string x)
      (let ((out (open-output-string)))
        (let walk ((x x))
          (cond ((string? x) (write-string (escape x #f) out))
                ((and (pair? x) (eq? (car x) '*COMMENT*))
                 (write-string "<!--" out) (write-string (cadr x) out) (write-string "-->" out))
                ((and (pair? x) (eq? (car x) '*PI*))
                 (write-string "<?" out) (write-string (symbol->string (cadr x)) out)
                 (write-string " " out) (write-string (car (cddr x)) out) (write-string "?>" out))
                ((xml-element? x)
                 (let ((name (symbol->string (xml-element-name x)))
                       (content (xml-element-content x)))
                   (write-string "<" out) (write-string name out)
                   (for-each (lambda (a)
                               (write-string " " out)
                               (write-string (symbol->string (car a)) out)
                               (write-string "=\"" out)
                               (write-string (escape (cadr a) #t) out)
                               (write-string "\"" out))
                             (xml-element-attributes x))
                   (if (null? content)
                       (write-string "/>" out)
                       (begin
                         (write-string ">" out)
                         (for-each walk content)
                         (write-string "</" out) (write-string name out) (write-string ">" out)))))
                (else (display x out))))
        (get-output-string out)))))
