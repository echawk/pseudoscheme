; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R7RS -*-

;;;; (guile): the first piece of a Guile mode (docs/guile.md).  Guile's
;;;; default environment is a whole language, so (guile) exports all of
;;;; R6RS plus Guile's own procedures (*GUILE-EXTRAS*), whose definitions
;;;; are src/guile/guile.scm.  It's made when first imported, through
;;;; psyntax's library loader (*LIBRARY-LOADERS*), so that booting doesn't
;;;; pay for it or for the SRFIs it imports.

(in-package "PSEUDOSCHEME-R7RS")

(defparameter *guile-extras*
  '(;; prompts and escapes, Guile's core
    "call-with-prompt" "abort-to-prompt" "make-prompt-tag" "default-prompt-tag" "%"
    "call-with-escape-continuation" "call/ec" "let/ec"
    ;; catch and throw, on R6RS conditions
    "catch" "throw" "with-throw-handler" "false-if-exception"
    "guile-exception?" "guile-exception-key" "guile-exception-args"
    "define-syntax-rule"
    ;; numbers, procedures, lists
    "1+" "1-" "identity" "const" "negate" "compose"
    "iota" "last-pair" "delete" "delete!" "list-index" "append-map" "filter-map"
    "reduce" "fold" "first" "second" "third"
    "assq-ref" "assv-ref" "assoc-ref" "assq-set!" "assv-set!" "assoc-set!"
    "assq-remove!" "assv-remove!" "assoc-remove!" "acons"
    ;; hash tables
    "make-hash-table" "hash-table?" "hash-ref" "hash-set!" "hash-remove!" "hash-create-handle!"
    "hashq-ref" "hashq-set!" "hashq-remove!" "hashv-ref" "hashv-set!" "hashv-remove!"
    "hash-for-each" "hash-map->list" "hash-fold" "hash-count" "hash-clear!"
    ;; strings, symbols, output
    "string-null?" "string-join" "string-split" "string-index" "string-contains"
    "string-prefix?" "string-suffix?" "string-trim" "string-trim-right" "string-trim-both"
    "symbol-append" "format" "simple-format" "pk")
  "What (guile) exports besides R6RS.")

(defun guile-library-form ()
  (let ((exports (remove-duplicates
		  (append (psx:table-exports '("r" "mp" "ms" "r5" "ev")) *guile-extras*)
		  :test #'string= :from-end t)))
    (list* (ssym "library") (list (ssym "guile"))
	   (cons (ssym "export") (mapcar #'ssym exports))
	   (read-scheme "(import (rnrs) (rnrs mutable-pairs) (rnrs mutable-strings) (rnrs r5rs) (rnrs eval)
                         (pseudoscheme control)
                         (only (chezscheme) format iota last-pair)
                         (only (srfi :1) delete delete! list-index append-map filter-map
                               reduce fold first second third)
                         (only (srfi :13) string-null? string-join string-index string-contains
                               string-prefix? string-suffix? string-trim string-trim-right
                               string-trim-both))")
	   (read-forms (asdf:system-relative-pathname :pseudoscheme "src/guile/guile.scm")))))

(defun load-guile-library (name)
  "A loader for psyntax (*LIBRARY-LOADERS*): make (guile) when it's asked
for; true if it was."
  (when (equal (mapcar #'sname* (if (listp (car (last name))) (butlast name) name)) '("guile"))
    (psx:eval-library (guile-library-form))
    t))

(pushnew 'load-guile-library psx::*library-loaders*)
