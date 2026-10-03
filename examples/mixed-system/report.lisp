;;; The Lisp half of mixed-demo: uses (demo stats) as the package STATS.

(defpackage "MIXED-DEMO"
  (:use "COMMON-LISP")
  (:export "REPORT"))

(in-package "MIXED-DEMO")

;; ASDF loaded lib/demo/stats.sld first, which installed the library;
;; make it a package.  EVAL-WHEN, so the package exists while this file
;; is compiled and STATS:MEAN below can be read.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (r7rs:use-library '(demo stats) :package "STATS"))

(defun report (numbers &optional (stream *standard-output*))
  "Print summary statistics of NUMBERS, computed in Scheme; return the
summary as an alist."
  (format stream "~&n = ~D~%mean = ~A~%median = ~A~%standard deviation = ~,3F~%"
          (length numbers)
          (stats:mean numbers)
          (stats:median numbers)
          (stats:standard-deviation numbers))
  (stats:summary numbers))
