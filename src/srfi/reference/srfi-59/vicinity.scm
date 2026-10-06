;;; SRFI 59's implementation, by Aubrey Jaffer, from the SRFI document
;;; (taken from slib/Template.scm and slib/require.scm; MIT licence, see
;;; LICENSE).  Written for a case-folding reader: include it with include-ci.

;;@ (implementation-vicinity) should be defined to be the pathname of
;;; the directory where any auxillary files to your Scheme
;;; implementation reside.
(define (implementation-vicinity)
  (case (software-type)
    ((UNIX)	"/usr/local/src/scheme/")
    ((VMS)	"scheme$src:")
    ((MS-DOS)	"C:\\scheme\\")))

;;@ (library-vicinity) should be defined to be the pathname of the
;;; directory where files of Scheme library functions reside.
(define library-vicinity
  (let ((library-path
	 (or
	  ;; Use this getenv if your implementation supports it.
	  (getenv "SCHEME_LIBRARY_PATH")
	  ;; Use this path if your scheme does not support GETENV
	  ;; or if SCHEME_LIBRARY_PATH is not set.
	  (case (software-type)
	    ((UNIX) "/usr/local/lib/slib/")
	    ((VMS) "lib$scheme:")
	    ((MS-DOS) "C:\\SLIB\\")
	    (else "")))))
    (lambda () library-path)))

;;@ (home-vicinity) should return the vicinity of the user's HOME
;;; directory, the directory which typically contains files which
;;; customize a computer environment for a user.
(define (home-vicinity)
  (let ((home (getenv "HOME")))
    (and home
	 (case (software-type)
	   ((UNIX COHERENT MS-DOS)	;V7 unix has a / on HOME
	    (if (eqv? #\/ (string-ref home (+ -1 (string-length home))))
		home
		(string-append home "/")))
	   (else home)))))
;@
(define in-vicinity string-append)
;@
(define (user-vicinity)
  (case (software-type)
    ((VMS)	"[.]")
    (else	"")))
;@
(define vicinity:suffix?
  (let ((suffi
	 (case (software-type)
	   ((AMIGA)				'(#\: #\/))
	   ((MACOS THINKC)			'(#\:))
	   ((MS-DOS WINDOWS ATARIST OS/2)	'(#\\ #\/))
	   ((NOSVE)				'(#\: #\.))
	   ((UNIX COHERENT PLAN9)		'(#\/))
	   ((VMS)				'(#\: #\]))
	   (else
	    (slib:warn "require.scm" 'unknown 'software-type (software-type))
	    "/"))))
    (lambda (chr) (and (memv chr suffi) #t))))
;@
(define (pathname->vicinity pathname)
  (let loop ((i (- (string-length pathname) 1)))
    (cond ((negative? i) "")
	  ((vicinity:suffix? (string-ref pathname i))
	   (substring pathname 0 (+ i 1)))
	  (else (loop (- i 1))))))
(define (program-vicinity)
  (if *load-pathname*
      (pathname->vicinity *load-pathname*)
      (slib:error 'program-vicinity "called while not within load")))
;@
(define sub-vicinity
  (case (software-type)
    ((VMS) (lambda
	       (vic name)
	     (let ((l (string-length vic)))
	       (if (or (zero? (string-length vic))
		       (not (char=? #\] (string-ref vic (- l 1)))))
		   (string-append vic "[" name "]")
		   (string-append (substring vic 0 (- l 1))
				  "." name "]")))))
    (else (let ((*vicinity-suffix*
		 (case (software-type)
		   ((NOSVE) ".")
		   ((MACOS THINKC) ":")
		   ((MS-DOS WINDOWS ATARIST OS/2) "\\")
		   ((UNIX COHERENT PLAN9 AMIGA) "/"))))
	    (lambda (vic name)
	      (string-append vic name *vicinity-suffix*))))))
;@
(define (make-vicinity pathname) pathname)

