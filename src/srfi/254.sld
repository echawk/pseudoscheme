;;; SRFI 254: Ephemerons and guardians.  Written for Pseudoscheme, on
;;; SBCL's weak pointers, weak hash tables and finalizers; the Lisp is
;;; in a package of its own, PSEUDOSCHEME-SRFI-254, made when the
;;; library is loaded.  (The SRFI's sample implementation is for Chez
;;; Scheme.)
;;;
;;; - An ephemeron holds its key in a weak pointer.  Its value is in a
;;;   two-level table, key -> (ephemeron -> value), both levels weak in
;;;   their keys.  SBCL's key-weak tables have ephemeron semantics -- a
;;;   value is traced only while its key is otherwise alive -- so a value
;;;   that refers to its key doesn't keep it alive.
;;; - A guardian queues a guarded object's representative from a
;;;   finalizer on the object, which SBCL runs once the object is
;;;   unreachable (the representative, not the object, is what SRFI 254
;;;   resurrects, so no resurrection is needed).
;;; - Objects can move in SBCL's collector, and current-hash is an
;;;   object's address.  A transport cell guardian reports a cell as
;;;   possibly moved once a garbage collection has happened since the
;;;   cell was added.
;;;
;;; The composite (srfi 254) is the only library: the R6RS names
;;; (srfi :254 ephemerons-and-guardians ...) resolve to it.
(define-library (srfi 254)
  (export reference-barrier
          make-ephemeron ephemeron? ephemeron-key ephemeron-value
          ephemeron-broken? ephemeron-ref
          make-guardian guardian?
          current-hash
          make-transport-cell-guardian transport-cell-guardian?
          transport-cell? transport-cell-key transport-cell-value
          transport-cell-broken?)
  (import (scheme base) (scheme case-lambda)
          (only (pseudoscheme lisp) lisp-eval-string))
  (begin
    (lisp-eval-string
     "(defpackage \"PSEUDOSCHEME-SRFI-254\" (:use \"COMMON-LISP\"))")
    (lisp-eval-string
     ;; read in the package, so that defstruct's accessors are made there
     "(let ((*package* (find-package \"PSEUDOSCHEME-SRFI-254\")))
       (eval (read-from-string \"(progn
        (defstruct (ephemeron (:constructor %make-ephemeron (pointer))) pointer)
        (defvar *values* (make-hash-table :test 'eq :weakness :key :synchronized t))
        (defstruct (guardian-state (:constructor make-guardian-state ()))
          (queue '()) (lock (sb-thread:make-mutex)))
        (defvar *guardians* (make-hash-table :test 'eq :weakness :key :synchronized t))
        (defvar *gc-epoch* 0)
        (pushnew (lambda () (incf *gc-epoch*)) sb-ext:*after-gc-hooks*)
        (defstruct (transport-cell (:constructor %make-transport-cell (pointer value epoch)))
          pointer value epoch)
        (defvar *cell-guardians* (make-hash-table :test 'eq :weakness :key :synchronized t)))\")))")

    (define %make-ephemeron
      (lisp-eval-string
       "(lambda (key value)
          (let ((e (pseudoscheme-srfi-254::%make-ephemeron (sb-ext:make-weak-pointer key))))
            (let ((inner (or (gethash key pseudoscheme-srfi-254::*values*)
                             (setf (gethash key pseudoscheme-srfi-254::*values*)
                                   (make-hash-table :test 'eq :weakness :key :synchronized t)))))
              (setf (gethash e inner) value))
            e))"))
    (define %ephemeron?
      (lisp-eval-string
       "(lambda (x) (if (pseudoscheme-srfi-254::ephemeron-p x) t ps:false))"))
    ;; the key, and whether the ephemeron is unbroken
    (define %ephemeron-key
      (lisp-eval-string
       "(lambda (e)
          (multiple-value-bind (key alive)
              (sb-ext:weak-pointer-value (pseudoscheme-srfi-254::ephemeron-pointer e))
            (values (if alive key ps:false) (if alive t ps:false))))"))
    (define %ephemeron-value
      (lisp-eval-string
       "(lambda (e)
          (multiple-value-bind (key alive)
              (sb-ext:weak-pointer-value (pseudoscheme-srfi-254::ephemeron-pointer e))
            (let ((inner (and alive (gethash key pseudoscheme-srfi-254::*values*))))
              (if inner (gethash e inner ps:false) ps:false))))"))
    (define reference-barrier
      (lisp-eval-string
       "(lambda (x) (sb-sys:with-pinned-objects (x) (values)) ps:unspecific)"))

    (define (check-ephemeron who e)
      (unless (%ephemeron? e) (error (string-append who ": not an ephemeron") e)))
    (define (make-ephemeron key value) (%make-ephemeron key value))
    (define (ephemeron? x) (%ephemeron? x))
    (define (ephemeron-key e)
      (check-ephemeron "ephemeron-key" e)
      (let-values (((key alive) (%ephemeron-key e))) key))
    (define (ephemeron-value e)
      (check-ephemeron "ephemeron-value" e)
      (%ephemeron-value e))
    (define (ephemeron-broken? e)
      (check-ephemeron "ephemeron-broken?" e)
      (let-values (((key alive) (%ephemeron-key e))) (not alive)))
    (define ephemeron-ref
      (case-lambda
        ((e key) (ephemeron-ref e key #f))
        ((e key default)
         (check-ephemeron "ephemeron-ref" e)
         (let-values (((k alive) (%ephemeron-key e)))
           (let ((result (if (and alive (eq? k key)) (%ephemeron-value e) default)))
             (reference-barrier key)
             result)))))

    ;; Guardians
    (define %make-guardian
      (lisp-eval-string
       "(lambda ()
          (let* ((state (pseudoscheme-srfi-254::make-guardian-state))
                 (guardian
                   (lambda (&rest args)
                     (cond ((= (length args) 2)
                            (let ((rep (second args)))
                              (sb-ext:finalize (first args)
                                               (lambda ()
                                                 (sb-thread:with-mutex ((pseudoscheme-srfi-254::guardian-state-lock state))
                                                   (push rep (pseudoscheme-srfi-254::guardian-state-queue state))))
                                               :dont-save t))
                            ps:unspecific)
                           ((null args)
                            (sb-thread:with-mutex ((pseudoscheme-srfi-254::guardian-state-lock state))
                              (if (pseudoscheme-srfi-254::guardian-state-queue state)
                                  (pop (pseudoscheme-srfi-254::guardian-state-queue state))
                                  ps:false)))
                           (t (error \"guardian: takes an object and its representative, or nothing\"))))))
            (setf (gethash guardian pseudoscheme-srfi-254::*guardians*) t)
            guardian))"))
    (define %guardian?
      (lisp-eval-string
       "(lambda (x) (if (and (functionp x) (gethash x pseudoscheme-srfi-254::*guardians*)) t ps:false))"))
    (define (make-guardian) (%make-guardian))
    (define (guardian? x) (%guardian? x))

    ;; Transport cells
    (define current-hash
      (lisp-eval-string "(lambda (x) (sb-kernel:get-lisp-obj-address x))"))
    (define %make-transport-cell-guardian
      (lisp-eval-string
       "(lambda ()
          (let* ((cells '())
                 (lock (sb-thread:make-mutex))
                 (guardian
                   (lambda (&rest args)
                     (cond ((= (length args) 2)
                            (let ((cell (pseudoscheme-srfi-254::%make-transport-cell
                                         (sb-ext:make-weak-pointer (first args)) (second args)
                                         pseudoscheme-srfi-254::*gc-epoch*)))
                              (sb-thread:with-mutex (lock) (push cell cells))
                              cell))
                           ((null args)
                            (sb-thread:with-mutex (lock)
                              (let ((moved (find-if (lambda (c)
                                                      (< (pseudoscheme-srfi-254::transport-cell-epoch c)
                                                         pseudoscheme-srfi-254::*gc-epoch*))
                                                    cells)))
                                (if moved
                                    (progn (setq cells (delete moved cells)) moved)
                                    ps:false))))
                           (t (error \"transport cell guardian: takes a key and a value, or nothing\"))))))
            (setf (gethash guardian pseudoscheme-srfi-254::*cell-guardians*) t)
            guardian))"))
    (define %transport-cell-guardian?
      (lisp-eval-string
       "(lambda (x) (if (and (functionp x) (gethash x pseudoscheme-srfi-254::*cell-guardians*)) t ps:false))"))
    (define %transport-cell?
      (lisp-eval-string
       "(lambda (x) (if (pseudoscheme-srfi-254::transport-cell-p x) t ps:false))"))
    (define %transport-cell-key
      (lisp-eval-string
       "(lambda (c)
          (multiple-value-bind (key alive)
              (sb-ext:weak-pointer-value (pseudoscheme-srfi-254::transport-cell-pointer c))
            (values (if alive key ps:false) (if alive t ps:false))))"))
    (define %transport-cell-value
      (lisp-eval-string "(lambda (c) (pseudoscheme-srfi-254::transport-cell-value c))"))

    (define (check-cell who c)
      (unless (%transport-cell? c) (error (string-append who ": not a transport cell") c)))
    (define (make-transport-cell-guardian) (%make-transport-cell-guardian))
    (define (transport-cell-guardian? x) (%transport-cell-guardian? x))
    (define (transport-cell? x) (%transport-cell? x))
    (define (transport-cell-key c)
      (check-cell "transport-cell-key" c)
      (let-values (((key alive) (%transport-cell-key c))) key))
    (define (transport-cell-value c)
      (check-cell "transport-cell-value" c)
      (%transport-cell-value c))
    (define (transport-cell-broken? c)
      (check-cell "transport-cell-broken?" c)
      (let-values (((key alive) (%transport-cell-key c))) (not alive)))))
