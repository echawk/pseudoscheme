;;; SRFI 41: streams.  Philip L. Bewig's R6RS reference implementation
;;; (41/primitive.sls, 41/derived.sls), with only the library names
;;; changed from (streams ...) to (srfi :41 ...).
(define-library (srfi 41)
  (export stream-null stream-cons stream? stream-null? stream-pair? stream-car
          stream-cdr stream-lambda define-stream list->stream port->stream stream
          stream->list stream-append stream-concat stream-constant stream-drop
          stream-drop-while stream-filter stream-fold stream-for-each stream-from
          stream-iterate stream-length stream-let stream-map stream-match _
          stream-of stream-range stream-ref stream-reverse stream-scan stream-take
          stream-take-while stream-unfold stream-unfolds stream-zip)
  (import (srfi 41 primitive) (srfi 41 derived)))
