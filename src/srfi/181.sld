;;; SRFI 181: custom ports (including transcoded ports).  Written for
;;; Pseudoscheme on its R6RS ports, rather than Shiro Kawai's sample
;;; implementation, which (as its README says) can't extend a host's
;;; ports and instead replaces the R7RS I/O procedures.  Here custom and
;;; transcoded ports are ordinary ports: read-char, write-string, read,
;;; close-port and the rest of R7RS work on them.
;;;
;;; Most procedures are (rnrs io ports)'s own bindings, so (rnrs) and
;;; (srfi 181) can be imported together.  The exceptions:
;;; - make-custom-textual-input-port, and the output and input/output
;;;   constructors, which take SRFI 181's optional flush procedure, come
;;;   from (srfi private srfi-181-ports) (see that file for why);
;;; - make-codec, which R6RS lacks, knows the names of the three codecs
;;;   there are, UTF-8, ISO 8859-1 and UTF-16, as the WHATWG encoding
;;;   standard spells them (case-insensitively); any other name raises
;;;   an unknown-encoding error, an R6RS condition of type
;;;   &unknown-encoding (a subtype of &error);
;;; - string->bytevector, unless the transcoder's end-of-line style is
;;;   none, first turns every line ending in the string (CR, LF or CRLF)
;;;   into a newline, which R6RS's then encodes in that style: SRFI 181
;;;   converts any line ending on output, R6RS only newlines;
;;; - make-file-error returns an &i/o-filename condition (whose
;;;   filename is the first argument, if any), which satisfies R7RS
;;;   file-error?, with the arguments as its irritants.
;;;
;;; As R6RS's, the textual read! and write! procedures are given
;;; strings, and the ports call them for one byte or character at a
;;; time.  Limitation: a transcoded output port converts only newlines
;;; to its end-of-line style, as R6RS's do, not CR or CRLF as well
;;; (string->bytevector does).  The UTF-16 codec reads a BOM and is
;;; big-endian without one.  There is no custom textual input/output
;;; port, as the SRFI specifies.
(define-library (srfi 181)
  (export make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-file-error
          make-codec latin-1-codec utf-8-codec utf-16-codec
          native-eol-style
          unknown-encoding-error? unknown-encoding-error-name
          i/o-decoding-error? i/o-encoding-error?
          i/o-encoding-error-char
          make-transcoder native-transcoder
          transcoded-port
          bytevector->string string->bytevector)
  (import (scheme base)
          (scheme char)
          (srfi private srfi-181-ports)
          (only (rnrs io ports)
                make-custom-binary-input-port
                latin-1-codec utf-8-codec utf-16-codec native-eol-style
                i/o-decoding-error? i/o-encoding-error?
                i/o-encoding-error-char
                make-transcoder native-transcoder transcoded-port
                bytevector->string transcoder-eol-style
                make-i/o-filename-error)
          (prefix (only (rnrs io ports) string->bytevector) r6:)
          (only (rnrs conditions)
                define-condition-type &error condition
                make-message-condition make-irritants-condition
                make-who-condition))
  (begin
    (define-condition-type &unknown-encoding &error
      make-unknown-encoding-condition unknown-encoding-error?
      (name unknown-encoding-error-name))

    (define codec-names
      `((,utf-8-codec "utf-8" "utf8" "unicode-1-1-utf-8" "unicode11utf8"
                      "unicode20utf8" "x-unicode20utf8")
        (,latin-1-codec "iso-8859-1" "iso8859-1" "iso_8859-1" "iso88591"
                        "iso_8859-1:1987" "latin1" "latin-1" "l1"
                        "iso-ir-100" "csisolatin1" "cp819" "ibm819")
        (,utf-16-codec "utf-16")))

    (define (make-codec name)
      (let ((key (string-foldcase name)))
        (let loop ((entries codec-names))
          (cond ((null? entries)
                 (raise (condition (make-unknown-encoding-condition name)
                                   (make-who-condition 'make-codec)
                                   (make-message-condition "unknown encoding")
                                   (make-irritants-condition (list name)))))
                ((member key (cdar entries)) ((caar entries)))
                (else (loop (cdr entries)))))))

    (define (string->bytevector string transcoder)
      (r6:string->bytevector
       (if (eq? (transcoder-eol-style transcoder) 'none)
           string
           (normalize-line-endings string))
       transcoder))

    (define (normalize-line-endings string)
      (let ((in (open-input-string string))
            (out (open-output-string)))
        (let loop ()
          (let ((c (read-char in)))
            (cond ((eof-object? c) (get-output-string out))
                  ((char=? c #\return)
                   (when (eqv? (peek-char in) #\newline) (read-char in))
                   (write-char #\newline out)
                   (loop))
                  (else (write-char c out) (loop)))))))

    (define (make-file-error . objs)
      (condition (make-i/o-filename-error (if (pair? objs) (car objs) ""))
                 (make-message-condition "file error")
                 (make-irritants-condition objs)))))
