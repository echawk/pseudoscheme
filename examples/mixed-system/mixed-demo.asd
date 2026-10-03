;;; An ASDF system made of Scheme and Lisp.
;;;
;;;   (push #p"examples/mixed-system/" asdf:*central-registry*)
;;;   (asdf:load-system :mixed-demo)
;;;   (mixed-demo:report '(2 4 4 4 5 5 7 9))
;;;
;;; :defsystem-depends-on pseudoscheme/asdf makes the Scheme component
;;; types available.  The Scheme library uses Common Lisp too, through
;;; (cl common-lisp); a library from another Lisp system would be listed
;;; in :depends-on, so Quicklisp/ocicl/qlot resolve it like any other
;;; dependency.

(defsystem "mixed-demo"
  :description "Pseudoscheme example: a Scheme library used by Lisp code"
  :defsystem-depends-on ("pseudoscheme/asdf")
  :components ((:r7rs-library "lib/demo/stats")         ; lib/demo/stats.sld
               (:file "report" :depends-on ("lib/demo/stats"))))
