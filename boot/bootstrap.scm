;;; boot/bootstrap.scm -- regenerate Pseudoscheme's .pso files in a
;;; Scheme that isn't Pseudoscheme.
;;;
;;; Loaded by a host adapter (boot/hosts/<name>.scm) after base.scm,
;;; reader.scm, printer.scm and runtime.scm, with the repository root as
;;; the current directory.  Loads the translator from its .scm sources,
;;; then has it translate the run-time system and itself, the way
;;; src/bootit.scm does under Common Lisp, writing to boot/build/out/:
;;;
;;;   closed.pso                 the open-coded built-ins, as functions
;;;   read.pso write.pso         the Scheme reader and printer
;;;   <translator.files>.pso     the translator
;;;   spack.lisp                 the translator's DEFPACKAGEs

(define boot:src-dir "src/")
(define boot:out-dir "boot/build/out/")

(define (boot:src name type) (string-append boot:src-dir name "." type))
(define (boot:out name type) (string-append boot:out-dir name "." type))

;; Must come before anything else is read: it defines what PS exports.
(boot:load-ps-exports! (boot:src "pack" "lisp"))

(define boot:translator-files (car (boot:read-file (boot:src "translator" "files"))))

;; Replaced by runtime.scm.
(define boot:host-supplied-files '("p-record" "p-utils"))

(define (boot:load-translator)
  ;; The translator defines a procedure SYNTAX-ERROR (alpha.scm), and
  ;; calls it in files loaded before that.  Many hosts have a macro of
  ;; that name (R7RS does), which would capture those calls.
  (boot:eval '(define syntax-error #f))
  (for-each
   (lambda (name)
     (if (not (member name boot:host-supplied-files))
	 (begin
	   (display "Loading ")
	   (display (boot:src name "scm"))
	   (newline)
	   (for-each (lambda (form) (boot:eval (boot:host-form form)))
		     (boot:read-file (boot:src name "scm"))))))
   boot:translator-files)
  ;; The translator reads source files with READ; ours reads them as
  ;; the CL reader does.  And it writes them our way.
  (boot:eval '(define read-file boot:read-file))
  (boot:eval '(define compiling-to-file boot:compiling-to-file))
  (boot:eval '(define write-defpackages boot:write-defpackages)))

(define (boot:translate-file name env)
  (boot:eval `(really-translate-file ,(boot:src name "scm")
				     ,(boot:out name "pso")
				     ,env)))

(define (boot:run)
  (boot:load-translator)
  ;; The run-time system (cf. TRANSLATE-RUN-TIME in bootit.scm)
  (boot:eval `(write-closed-definitions revised^4-scheme-structure
					,(boot:out "closed" "pso")))
  (for-each (lambda (name) (boot:translate-file name 'revised^4-scheme-env))
	    '("read" "write"))
  ;; The translator (cf. TRANSLATE-TRANSLATOR)
  (for-each (lambda (name) (boot:translate-file name 'scheme-translator-env))
	    boot:translator-files)
  (boot:eval `(write-defpackages (list revised^4-scheme-structure
				       scheme-translator-structure)
				 ,(boot:out "spack" "lisp")))
  (display "Done.")
  (newline))

(boot:run)
