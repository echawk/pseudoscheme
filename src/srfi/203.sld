;;; SRFI 203: a simple picture language in the style of SICP.  Pseudoscheme
;;; has no graphics, so this is Vasilij Schneidermann's portable
;;; implementation from the SRFI's repository (contrib/vasilij-
;;; schneidermann; BSD-3-Clause, after Peter Danenberg's sicp egg; in
;;; reference/srfi-203/ with its LICENSE), which draws into an SVG file:
;;; drawing procedures collect SVG elements, and (canvas-refresh) writes
;;; them to the file (canvas-path) and returns its file:// URL.  The
;;; sample implementation (ImageMagick through Chibi's process library)
;;; isn't portable.  rogers.scm (the picture of Rogers, CC0) is included
;;; unmodified; the code of 203.scm is copied here with these changes:
;;;
;;;   - Coordinates are in the unit square, as in SICP, the SRFI's
;;;     examples and its sample implementation: (draw-line '(0 0) '(1 1))
;;;     is the canvas's diagonal, and (canvas-frame) is ((0 0) (1 0)
;;;     (0 1)).  The contrib code took pixels.  The canvas is
;;;     (canvas-width) x (canvas-height) pixels, 256 x 256 by default.
;;;   - canvas-path defaults to "srfi-203-canvas.svg" in the current
;;;     directory (the SRFI says drawing needs no set-up; the contrib code
;;;     raised an error until it was set).  canvas-name is the same
;;;     parameter, the sample implementation's name for it.  The URL is
;;;     file:// followed by the path as given.
;;;   - canvas-cleanup deletes the file and empties the canvas (the
;;;     contrib code left an empty file).
;;;   - canvas-stack, the drawing so far, was a parameter set by calling
;;;     it with a value (CHICKEN's extension); it is a variable.
;;;   - Numbers are written as integers or decimals (an exact fraction
;;;     like 1/2 isn't SVG).
;;;   - display-svg writes a text node (it computed the escaped string
;;;     and dropped it), and escapes it with string->list (for-each over
;;;     a string isn't R7RS).
;;;   - The sample's extra painter landau and draw-bezier aren't provided.
;;;
;;; image-file->painter (and jpeg-file->painter) link to the file from
;;; the SVG: a relative name is relative to the SVG file.
(define-library (srfi 203)
  (import (scheme base))
  (import (scheme cxr))
  (import (scheme case-lambda))
  (import (scheme file))
  (import (scheme write))
  (export canvas-reset canvas-refresh canvas-cleanup
          canvas-path canvas-name canvas-width canvas-height canvas-frame
          draw-line
          rogers
          jpeg-file->painter
          image-file->painter)
  (include "reference/srfi-203/rogers.scm")
  (begin
    (define canvas-width (make-parameter 256))
    (define canvas-height (make-parameter 256))
    (define canvas-path (make-parameter "srfi-203-canvas.svg"))
    (define canvas-name canvas-path)
    ;; The drawing so far.  (It was a parameter, set by calling it with
    ;; a value, which works in CHICKEN but isn't R7RS.)
    (define drawing '())
    (define canvas-stack
      (case-lambda
        (() drawing)
        ((new) (set! drawing new))))

    (define (canvas-frame)
      '((0 0) (1 0) (0 1)))

    (define (display-svg svg out)
      (define (entity-encode string)
        (let ((out (open-output-string)))
          (for-each (lambda (char)
                      (case char
                        ((#\<) (display "&lt;" out))
                        ((#\>) (display "&gt;" out))
                        ((#\&) (display "&amp;" out))
                        ((#\') (display "&#x27;" out))
                        ((#\") (display "&quot;" out))
                        (else (display char out))))
                    (string->list string))
          (get-output-string out)))
      (cond
       ((string? svg)
        (display (entity-encode svg) out))
       ;; (foo (@ ...) body ...)
       ((and (pair? svg) (symbol? (car svg))
             (pair? (cdr svg)) (pair? (cadr svg)))
        (let ((tag (car svg))
              (attrs (cdr (cadr svg)))
              (body (cddr svg)))
          (display "<" out)
          (display tag out)
          (for-each (lambda (attr)
                      (display " " out)
                      (display (car attr) out)
                      (display "=" out)
                      (write (cadr attr) out))
                    attrs)
          (if (pair? body)
              (begin
                (display ">" out)
                (for-each (lambda (form) (display-svg form out)) body)
                (display "</" out)
                (display tag out)
                (display ">" out))
              (display "/>" out))))
       (else
        (error "Malformed SXML"))))

    (define (canvas-svg)
      `(svg (@ (xmlns "http://www.w3.org/2000/svg")
               (xmlns:xlink "http://www.w3.org/1999/xlink")
               (height ,(number->string (canvas-height)))
               (width ,(number->string (canvas-width))))
            (g (@ (stroke "black"))
               ,@(reverse (canvas-stack)))))

    (define (canvas-cleanup)
      (when (file-exists? (canvas-path))
        (delete-file (canvas-path)))
      (canvas-stack '()))

    (define (canvas-reset)
      (canvas-stack '()))

    (define (canvas-refresh)
      (call-with-output-file (canvas-path)
        (lambda (out)
          (display-svg (canvas-svg) out)))
      (string-append "file://" (canvas-path)))

    (define origin-frame car)
    (define edge1-frame cadr)
    (define edge2-frame caddr)
    (define xcor-vect car)
    (define ycor-vect cadr)

    ;; unit coordinates to SVG's (pixels, y down)
    (define (svg-x x) (* x (canvas-width)))
    (define (svg-y y) (- (canvas-height) (* y (canvas-height))))

    (define (number->svg x)
      (number->string (cond ((exact-integer? x) x)
                            ((zero? x) 0)             ; not -0.0
                            (else (inexact x)))))

    (define (draw-line start end)
      (define svg
        `(line (@ (x1 ,(number->svg (svg-x (xcor-vect start))))
                  (y1 ,(number->svg (svg-y (ycor-vect start))))
                  (x2 ,(number->svg (svg-x (xcor-vect end))))
                  (y2 ,(number->svg (svg-y (ycor-vect end)))))))
      (canvas-stack (cons svg (canvas-stack))))

    (define (draw-image url frame)
      (define svg
        ;; from cartesian, bottom left origin, to SVG, top left origin
        (let* ((origin (origin-frame frame))
               (edge1 (edge1-frame frame))
               (edge2 (edge2-frame frame))
               (w (canvas-width))
               (h (canvas-height))
               ;; the image's upper left-hand corner: origin + edge2
               (svg-origin-x (svg-x (+ (xcor-vect origin) (xcor-vect edge2))))
               (svg-origin-y (svg-y (+ (ycor-vect origin) (ycor-vect edge2))))
               (svg-edge1-x (* w (xcor-vect edge1)))
               (svg-edge1-y (- (* h (ycor-vect edge1))))
               (svg-edge2-x (- (* w (xcor-vect edge2))))
               (svg-edge2-y (* h (ycor-vect edge2)))
               (transform (string-append "matrix("
                                         (number->svg svg-edge1-x)
                                         ", "
                                         (number->svg svg-edge1-y)
                                         ", "
                                         (number->svg svg-edge2-x)
                                         ", "
                                         (number->svg svg-edge2-y)
                                         ", "
                                         (number->svg svg-origin-x)
                                         ", "
                                         (number->svg svg-origin-y)
                                         ")")))
          `(g (@ (transform ,transform))
              (image (@ (xlink:href ,url)
                        (width "1")
                        (height "1"))))))
      (canvas-stack (cons svg (canvas-stack))))

    (define (rogers frame)
      (draw-image rogers-data-url frame))

    (define (image-file->painter file-name)
      (lambda (frame)
        (draw-image file-name frame)))

    (define (jpeg-file->painter file-name)
      (image-file->painter file-name))))
