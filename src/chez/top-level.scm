;;; -*- Mode: Scheme -*-
;;;; (chezscheme): the top level and the system, as Chez Scheme has them:
;;;; eval in the interaction environment, top-level values, load, the
;;;; library path, the compiler's entry points and parameters, and
;;;; Chez's extra syntax.  Part of the library's body
;;;; (src/chez/chez.lisp), whose host procedures run at Chez's top level
;;;; (quasischeme --chez).

;;; eval: in the interaction environment by default, the top level a
;;; Chez script or the REPL runs at

(define (interaction-environment) (%r7rs:interaction-environment))

(define (top-level-environment? env) (eq? env (interaction-environment)))

;; eval: (rnrs eval)'s, which with one argument, or the interaction
;; environment, evaluates at the top level running (src/chez/chez.lisp,
;; install-chez-extensions); so does dynamic-wind take Chez's critical?

(define interpret eval)
(define compile eval)

;; The expanded form: here, the form itself (Chez's is its internal
;; language)
(define (expand x . env) x)
(define (sc-expand x . env) x)

(define (scheme-environment) (environment '(chezscheme)))

;; Libraries: their exports, and (approximately) what they import
(define (enum-set? x) (%chez:enum-set? x))

;; A library reference: a name, perhaps with a version reference (R6RS
;; 7.1): (values name version-reference-or-#f)
(define (library-reference who ref)
  (unless (and (list? ref) (pair? ref)) (assertion-violation who "invalid library reference" ref))
  (let* ([last-part (car (last-pair ref))]
         [versioned (or (null? last-part) (pair? last-part))]
         [name (if versioned (list-head ref (- (length ref) 1)) ref)]
         [vref (and versioned last-part)])
    (unless (and (pair? name) (for-all symbol? name)) (assertion-violation who "invalid library reference" ref))
    (when (and vref (not (version-reference? vref))) (assertion-violation who "invalid version reference" ref))
    (values name vref)))
(define (sub-version-reference? x)
  (or (and (integer? x) (exact? x) (>= x 0))
      (and (list? x) (pair? x)
           (case (car x)
             [(>= <=) (and (= (length x) 2) (integer? (cadr x)) (exact? (cadr x)) (>= (cadr x) 0))]
             [(and or) (for-all sub-version-reference? (cdr x))]
             [(not) (and (= (length x) 2) (sub-version-reference? (cadr x)))]
             [else #f]))))
(define (version-reference? x)
  (and (list? x)
       (if (and (pair? x) (memq (car x) '(and or not)))
           (case (car x)
             [(not) (and (= (length x) 2) (version-reference? (cadr x)))]
             [else (for-all version-reference? (cdr x))])
           (for-all sub-version-reference? x))))
(define (sub-version-match? ref n)
  (if (integer? ref)
      (= ref n)
      (case (car ref)
        [(>=) (>= n (cadr ref))]
        [(<=) (<= n (cadr ref))]
        [(and) (for-all (lambda (r) (sub-version-match? r n)) (cdr ref))]
        [(or) (exists (lambda (r) (sub-version-match? r n)) (cdr ref))]
        [(not) (not (sub-version-match? (cadr ref) n))])))
(define (version-match? ref version)
  (if (and (pair? ref) (memq (car ref) '(and or not)))
      (case (car ref)
        [(and) (for-all (lambda (r) (version-match? r version)) (cdr ref))]
        [(or) (exists (lambda (r) (version-match? r version)) (cdr ref))]
        [(not) (not (version-match? (cadr ref) version))])
      (and (<= (length ref) (length version))
           (let loop ([ref ref] [version version])
             (or (null? ref)
                 (and (sub-version-match? (car ref) (car version))
                      (loop (cdr ref) (cdr version))))))))
;; the installed library REF refers to: its name and version
(define (installed-library who ref)
  (let-values ([(name vref) (library-reference who ref)])
    (unless (member name (library-list)) (assertion-violation who "library not found" ref))
    (let ([version (%chez:library-version name)])
      (when (and vref (not (version-match? vref version)))
        (assertion-violation who "no installed library matches" ref))
      (values name version))))
(define (library-version ref)
  (let-values ([(name version) (installed-library 'library-version ref)]) version))
(define (library-exports ref)
  (let-values ([(name version) (installed-library 'library-exports ref)])
    (%chez:library-exports name)))
(define (library-object-filename ref)
  (installed-library 'library-object-filename ref)
  #f)
(define-enumeration library-requirements-option (import visit@visit invoke@visit invoke)
  library-requirements-options)
;; what a library needs: imports, and the libraries invoked to visit or
;; invoke it (Chez's visit@visit isn't tracked: none); the system's own
;; libraries need nothing
(define library-requirements
  (case-lambda
    [(ref) (library-requirements ref (library-requirements-options import visit@visit invoke@visit invoke))]
    [(ref options)
     (let-values ([(name version) (installed-library 'library-requirements ref)])
       (unless (enum-set? options)
         (assertion-violation 'library-requirements "not a library-requirements-options enum set" options))
       (let* ([reqs (%chez:library-requirements name)]
              [opts (enum-set->list options)]
              ;; the system's libraries are always invoked
              [user (lambda (specs) (filter (lambda (s) (not (%chez:system-library-id? (car s)))) specs))]
              [specs (append (if (memq 'import opts) (car reqs) '())
                             (if (memq 'invoke@visit opts) (user (cadr reqs)) '())
                             (if (memq 'invoke opts) (user (caddr reqs)) '()))]
              [id (%chez:library-id name)])
         (if (and id (%chez:system-library-id? id))
             '()
             (let loop ([specs specs] [seen '()] [acc '()])
               (cond
                 [(null? specs) (reverse acc)]
                 [(memq (caar specs) seen) (loop (cdr specs) seen acc)]
                 [else (let ([s (car specs)])
                         (loop (cdr specs) (cons (car s) seen)
                               (cons (if (null? (caddr s)) (cadr s) (append (cadr s) (list (caddr s)))) acc)))])))))]))
(define (copy-environment env . rest) env)
(define (environment-symbols env)
  (if (top-level-environment? env)
      (%chez:top-level-symbols)
      (%psyntax:environment-symbols env)))

;;; Top-level values: what eval at the top level sees

(define (top-level-value s . env)
  (eval s (if (pair? env) (car env) (interaction-environment))))

(define (set-top-level-value! s v . env)
  (%chez:set-top-level-value! s v))

(define (define-top-level-value s v . env)
  (%chez:define-top-level-value! s v))

(define (top-level-bound? s . env)
  (%chez:top-level-bound? s))

(define (top-level-mutable? s . env) (top-level-bound? s))

;;; Loading and compiling.  Compiling is what loading does here, so the
;;; compile- procedures load or do nothing; libraries are compiled when
;;; imported, into the compiled-library cache.

(define load
  (case-lambda
    ((path) (%chez:load path))
    ;; each form read is passed to eval-proc
    ((path eval-proc)
     (call-with-input-file path
       (lambda (p)
         (let loop ()
           (let ((x (read p)))
             (unless (eof-object? x)
               (eval-proc x)
               (loop)))))))))

(define (load-program path) (%chez:load path))
(define (load-library path) (%chez:load path))
(define (visit path) (%chez:load path))
(define (revisit path) (%chez:load path))
;; Compiling a program: its object file is an SBCL fasl
;; (src/chez/compile.lisp).  Compiling another file: its "object file"
;; is a copy of its source, which load and --script run as they do the
;; source (and compile, as they go, with SBCL); a library's is compiled
;; into the compiled-library cache when it's first imported.  So
;; programs that drive Chez's compiler, as Idris 2's build does, find
;; the files they expect.
(define (object-file-name in)
  (let loop ((i (- (string-length in) 1)))
    (cond ((< i 0) (string-append in ".so"))
          ((char=? (string-ref in i) #\.) (string-append (substring in 0 i) ".so"))
          ((char=? (string-ref in i) #\/) (string-append in ".so"))
          (else (loop (- i 1))))))
(define (copy-to-object-file who in out)
  (unless (string? in) (assertion-violation who "not a string" in))
  (let ((out (or out (object-file-name in))))
    (when (compile-file-message)
      (printf "compiling ~a with output to ~a\n" in out))
    (let ((text (call-with-input-file in get-string-all)))
      (when (file-exists? out) (delete-file out))
      (call-with-output-file out (lambda (p) (unless (eof-object? text) (put-string p text))))
      ;; as Chez's: a program with a #! line can be run
      (chmod out (get-mode in)))
    (if #f #f)))
(define (compile-file in . out) (compile-source 'compile-file in out))
(define (compile-library in . out) (compile-source 'compile-library in out))
;; compile-file and compile-library: with generate-wpo-files, the
;; "whole-program optimization" file foo.wpo too (another copy, which
;; compile-whole-library combines); with compile-imported-libraries, the
;; libraries the file imports whose object files are missing or out of
;; date are compiled too (as they're imported: src/r7rs/front.lisp)
(define (compile-source who in out)
  (let* ((in (source-file-name who in))
         (out (if (pair? out) (car out) (object-file-name in))))
    (unless (string? out) (assertion-violation who "not a string" out))
    (copy-to-object-file who in out)
    (when (generate-wpo-files) (write-wpo-file in out))
    ;; its libraries are expanded and installed, as Chez's compiler
    ;; leaves them, which reports what's wrong with them
    (guard (c (#t (when (file-exists? out) (delete-file out))
                  (let ((wpo (wpo-file-name out)))
                    (when (and (generate-wpo-files) (file-exists? wpo)) (delete-file wpo)))
                  (raise c)))
      (%chez:install-file-libraries in))
    (when (compile-imported-libraries)
      (for-each compile-imported-library (file-imports in)))
    (if #f #f)))
;; the libraries a file imports, anywhere in it or in the files it includes
(define (file-imports path)
  (let loop ((path path) (seen '()))
    (if (or (member path seen) (not (file-exists? path)))
        '()
        (let ((forms (file-forms path)))
          (append (append-map deep-imports forms)
                  (append-map (lambda (f) (loop (include-file-name path f) (cons path seen)))
                              (append-map deep-includes forms)))))))

;; maybe-compile-file, -library and -program: compile only if the object
;; file is missing or older than the source, a file it includes, or the
;; object file of a library it imports (which, with
;; compile-imported-libraries, is brought up to date first)
(define compile-program-handler
  (make-parameter (lambda (in out) (compile-program in out))
    (lambda (x) (unless (procedure? x) (assertion-violation 'compile-program-handler "not a procedure" x)) x)))
(define (maybe-compile-file in . out) (maybe-compile 'maybe-compile-file compile-file in out))
(define (maybe-compile-library in . out)
  (maybe-compile 'maybe-compile-library (compile-library-handler) in out))
(define (maybe-compile-program in . out)
  (maybe-compile 'maybe-compile-program (compile-program-handler) in out))
(define (maybe-compile who handler in out)
  (unless (string? in) (assertion-violation who "not a string" in))
  (unless (or (null? out) (string? (car out))) (assertion-violation who "not a string" (car out)))
  (let* ((in (source-file-name who in))
         (out (if (pair? out) (car out) (object-file-name in))))
    (unless (file-exists? in) (errorf who "source file ~s does not exist" in))
    (when (object-out-of-date? who in out)
      (handler in out))
    (if #f #f)))
(define (object-out-of-date? who in out)
  (let ((forms (guard (c (#t #f)) (file-forms* in))))
    ;; imported libraries first, so that they are brought up to date
    (let ((stale-imports
           (and forms
                (exists (lambda (x) x)
                  (map (lambda (name)
                         (when (compile-imported-libraries) (compile-imported-library name))
                         (and (library-name? name)
                              (let-values (((source object object-exists?) (library-search who name)))
                                (and object-exists? (file-exists? out) (newer? object out)))))
                       (file-imports in))))))
      (or (not (file-exists? out))
          (newer? in out)
          (not forms)
          stale-imports
          (exists (lambda (f) (and (file-exists? f) (newer? f out)))
                  (map (lambda (f) (include-file-name in f)) (append-map deep-includes forms)))))))
(define (newer? a b)
  (time<? (file-modification-time b) (file-modification-time a)))
;; a file's forms, an error if it can't be read
(define (file-forms* path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((acc '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
(define (deep-includes form)
  (let walk ((x form) (acc '()))
    (cond ((not (pair? x)) acc)
          ((eq? (car x) 'quote) acc)
          ((and (eq? (car x) 'include) (pair? (cdr x)) (string? (cadr x)))
           (cons (cadr x) acc))
          ((list? x) (fold-left (lambda (acc y) (walk y acc)) acc x))
          (else (walk (cdr x) (walk (car x) acc))))))
(define (include-file-name source file)
  (if (path-absolute? file)
      file
      (let ((dir (path-parent source)))
        (if (string=? dir "") file (path-build dir file)))))
;; as Chez's: a name with no extension that isn't a file is name.ss
(define (source-file-name who in)
  (unless (string? in) (assertion-violation who "not a string" in))
  (if (and (not (file-exists? in)) (not (path-extension-present? in)))
      (string-append in ".ss")
      in))
(define (path-extension-present? path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 0) #f)
          ((char=? (string-ref path i) #\.) #t)
          ((char=? (string-ref path i) #\/) #f)
          (else (loop (- i 1))))))
(define (write-wpo-file in out)
  (let ((wpo (wpo-file-name out)))
    (when (file-exists? wpo) (delete-file wpo))
    (call-with-output-file wpo
      (lambda (p) (put-string p (call-with-input-file in get-string-all))))))
(define (wpo-file-name object)
  (let ((n (string-length object)))
    (if (and (> n 3) (string=? (substring object (- n 3) n) ".so"))
        (string-append (substring object 0 (- n 3)) ".wpo")
        (string-append object ".wpo"))))
(define (file-forms path)
  (guard (c (#t '()))
    (call-with-input-file path
      (lambda (p)
        (let loop ((acc '()))
          (let ((x (read p)))
            (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))))
(define (append-map f l) (apply append (map f l)))
;; the libraries a form imports anywhere within it, as a library's
;; body's own (import ...) forms do
(define (deep-imports form)
  (let walk ((x form) (acc '()))
    (cond ((not (pair? x)) acc)
          ((eq? (car x) 'quote) acc)
          ((and (memq (car x) '(import library)) (list? x))
           (fold-left (lambda (acc y) (walk y acc)) (append acc (form-imports x)) (cdr x)))
          ((list? x) (fold-left (lambda (acc y) (walk y acc)) acc x))
          (else (walk (cdr x) (walk (car x) acc))))))
;; the libraries a library form, or a file's import form, imports
(define (form-imports form)
  (define (spec-library spec)
    (if (and (pair? spec) (memq (car spec) '(only except prefix rename for library)) (pair? (cdr spec)))
        (spec-library (cadr spec))
        spec))
  (cond ((and (pair? form) (eq? (car form) 'library) (list? form) (>= (length form) 4)
              (pair? (cadddr form)) (eq? (car (cadddr form)) 'import))
         (map spec-library (cdr (cadddr form))))
        ((and (pair? form) (eq? (car form) 'import) (list? form))
         (map spec-library (cdr form)))
        (else '())))
(define (out-of-date? object source)
  (or (not (file-exists? object))
      (time<? (file-modification-time object) (file-modification-time source))))
(define compiling-imported (make-parameter '()))
;; the libraries already brought up to date in this process: Chez checks
;; a library when it first loads it, not at each import (re-reading every
;; importer's sources, for theirs, at each, was most of e's start)
(define imports-checked (make-hashtable equal-hash equal?))
(define (compile-imported-library name)
  (when (and (library-name? name) (not (member name (compiling-imported)))
             (not (hashtable-ref imports-checked (cons name (library-directories)) #f))
             (not (%chez:system-library-name? name)))
    (hashtable-set! imports-checked (cons name (library-directories)) #t)
    (let-values (((source object object-exists?) (library-search 'import name)))
      (when (and source object (file-exists? source))
        (parameterize ((compiling-imported (cons name (compiling-imported))))
          (when (object-out-of-date? 'import source object)
            ;; the object file's directories made first, as Chez's
            ;; import does ("creating subdirectory")
            (let make-parent ((p object))
              (let ((p (path-parent p)))
                (unless (or (string=? p "") (file-exists? p))
                  (make-parent p)
                  (mkdir p))))
            ((compile-library-handler) source object)))))))

;; Chez's library search: (library-search-handler) is called with who
;; is looking, the library name, (library-directories) and
;; (library-extensions); it returns the library's source file name, its
;; object file name, and whether the object file exists.  The default
;; looks for a source in each directory, with each extension, and if
;; none, for an object file.  src/r7rs/front.lisp loads what it finds.
(define (library-name? x)
  (and (list? x) (pair? x) (symbol? (car x))
       (let loop ((x x))
         (cond ((null? x) #t)
               ((symbol? (car x)) (loop (cdr x)))
               (else (and (null? (cdr x)) (list? (car x))))))))
(define (library-name-stem name)
  (let loop ((parts name) (acc ""))
    (if (or (null? parts) (not (symbol? (car parts))))
        acc
        (loop (cdr parts)
              (if (string=? acc "")
                  (symbol->string (car parts))
                  (string-append acc "/" (symbol->string (car parts))))))))
(define (string-pair? x) (and (pair? x) (string? (car x)) (string? (cdr x))))
(define (in-directory dir file)
  (if (or (string=? dir ".") (string=? dir "./") (string=? dir ""))
      file
      (path-build dir file)))
(define (default-library-search-handler who name dirs exts)
  (unless (symbol? who)
    (assertion-violation 'default-library-search-handler "invalid who argument" who))
  (unless (library-name? name)
    (assertion-violation 'default-library-search-handler "invalid library name" name))
  (unless (and (list? dirs) (for-all string-pair? dirs))
    (assertion-violation 'default-library-search-handler "invalid path list" dirs))
  (unless (and (list? exts) (for-all string-pair? exts))
    (assertion-violation 'default-library-search-handler "invalid extension list" exts))
  (let* ((stem (library-name-stem name))
         ;; a name whose first part is an absolute directory is that file
         (dirs (if (path-absolute? stem) '(("" . "")) dirs)))
    (let source-loop ((ds dirs))
      (if (null? ds)
          (let object-loop ((ds dirs))
            (if (null? ds)
                (values #f #f #f)
                (let ext-loop ((es exts))
                  (if (null? es)
                      (object-loop (cdr ds))
                      (let ((object (in-directory (cdar ds) (string-append stem (cdar es)))))
                        (if (file-exists? object)
                            (values #f object #t)
                            (ext-loop (cdr es))))))))
          (let ext-loop ((es exts))
            (if (null? es)
                (source-loop (cdr ds))
                (let ((source (in-directory (caar ds) (string-append stem (caar es)))))
                  (if (file-exists? source)
                      (let ((object (in-directory (cdar ds) (string-append stem (cdar es)))))
                        (values source object (file-exists? object)))
                      (ext-loop (cdr es))))))))))
(define library-search-handler
  (make-parameter default-library-search-handler
    (lambda (x)
      (unless (procedure? x)
        (assertion-violation 'library-search-handler "not a procedure" x))
      x)))
(define (library-search who name . exts)
  (call-with-values
    (lambda ()
      ((library-search-handler) who name (library-directories)
       (if (pair? exts) (car exts) (library-extensions))))
    (case-lambda
      ((source object object-exists?)
       (unless (or (not source) (string? source))
         (errorf who "library-search-handler returned invalid source-file path ~s" source))
       (unless (or (not object) (string? object))
         (errorf who "library-search-handler returned invalid object-file path ~s" object))
       (when (and object-exists? (not object))
         (errorf who "library-search-handler claimed object file was found but returned no path"))
       (values source object (and object-exists? #t)))
      (vals (errorf who "library-search-handler returned ~s values, not 3" (length vals))))))
(%chez:set-library-search!
  (lambda (name)
    (when (compile-imported-libraries) (compile-imported-library name))
    (let-values (((source object object-exists?) (library-search 'import name)))
      (list source object))))

;; compile-whole-library: the library of a .wpo file and, before it, each
;; library it depends on whose .wpo file there is, in one object file.
;; The value: the libraries it depends on that have none (and so stay
;; separate).
(define (compile-whole-library wpo out)
  (let-values (((forms missing) (whole-forms 'compile-whole-library (file-forms wpo))))
    (write-object-file out forms)
    missing))
(define (compile-whole-program wpo out . libs-visible?)
  (let-values (((forms missing) (whole-forms 'compile-whole-program (file-forms wpo))))
    (write-object-file out forms)
    missing))
(define (whole-forms who main)
  (let ((seen '()) (missing '()) (forms '()))
    (define (strip name)
      (if (and (pair? name) (pair? (last-pair name)) (list? (car (last-pair name))))
          (reverse (cdr (reverse name)))
          name))
    (define (include! fs)
      (for-each visit (append-map deep-imports fs))
      (set! forms (append forms fs)))
    (define (visit name)
      (let ((key (and (list? name) (strip name))))
        (when (and key (not (member key seen)) (not (%chez:system-library-name? key)))
          (set! seen (cons key seen))
          (let-values (((source wpo wpo-exists?)
                        (library-search who key
                          (map (lambda (e) (cons (car e) (string-append (path-root (cdr e)) ".wpo")))
                               (library-extensions)))))
            (if wpo-exists?
                (include! (file-forms wpo))
                (let-values (((source object object-exists?) (library-search who key)))
                  (cond ((or source object-exists?)
                         ;; it stays separate, but what it imports is
                         ;; included as for the others
                         (set! missing (cons key missing))
                         (for-each visit
                           (append-map deep-imports
                             (file-forms (if object-exists? object source)))))
                        ((not (%chez:library-exists? key))
                         (set! missing (cons key missing))))))))))
    (include! main)
    (values forms (reverse missing))))
(define (write-object-file out forms)
  (when (file-exists? out) (delete-file out))
  (call-with-output-file out (lambda (p) (write-forms forms p))))

;; concatenate-object-files: object files here are text, so their texts
(define (concatenate-object-files out in . ins)
  (let ((text (apply string-append
                     (map (lambda (f) (call-with-input-file f get-string-all)) (cons in ins)))))
    (when (file-exists? out) (delete-file out))
    (call-with-output-file out (lambda (p) (put-string p text)))))

;; verify-loadability: that each library the object files import is
;; among them or can be found
(define (verify-loadability situation . inputs)
  (unless (memq situation '(visit revisit load))
    (assertion-violation 'verify-loadability "invalid situation" situation))
  (let* ((files (map (lambda (x) (if (pair? x) (car x) x)) inputs))
         (forms (append-map file-forms files))
         (defined (filter (lambda (x) x)
                          (map (lambda (f) (and (pair? f) (eq? (car f) 'library) (pair? (cdr f)) (cadr f)))
                               forms))))
    (for-each (lambda (f) (unless (file-exists? f) (errorf 'verify-loadability "cannot find object file ~a" f)))
              files)
    (for-each
      (lambda (name)
        (unless (or (not (library-name? name))
                    (%chez:system-library-name? name)
                    (exists (lambda (d) (library-name-match? d name)) defined)
                    (let-values (((source object object-exists?) (library-search 'verify-loadability name)))
                      (or object-exists? (and source (file-exists? source))))
                    (%chez:library-exists? name))
          (errorf 'verify-loadability "cannot find library ~s" name)))
      (append-map form-imports forms))))
(define (library-name-match? defined name)
  (let loop ((d defined) (n name))
    (cond ((or (null? n) (pair? (car n))) #t)
          ((or (null? d) (pair? (car d))) #f)
          ((eq? (car d) (car n)) (loop (cdr d) (cdr n)))
          (else #f))))
(define (compile-program in . out)
  (let* ((in (source-file-name 'compile-program in))
         (out (if (pair? out) (car out) (object-file-name in))))
    (when (compile-file-message)
      (printf "compiling ~a with output to ~a\n" in out))
    (%chez:compile-program in out)
    (when (generate-wpo-files) (write-wpo-file in out))
    '()))
(define (compile-script in . out)
  (let ((in (source-file-name 'compile-script in)))
    (copy-to-object-file 'compile-script in (and (pair? out) (car out)))))
;; Compiled code written to a file or a binary port is the forms' text,
;; which load and load-compiled-from-port evaluate
(define (write-forms forms p)
  (for-each (lambda (x) (write x p) (newline p)) forms))
(define (compile-to-file forms out . libs)
  (when (file-exists? out) (delete-file out))
  (call-with-output-file out (lambda (p) (write-forms forms p)))
  (if #f #f))
(define (compile-to-port forms bp . rest)
  (put-bytevector bp (string->utf8 (call-with-string-output-port (lambda (p) (write-forms forms p)))))
  (if #f #f))
(define (load-compiled-from-port bp)
  (let ([p (transcoded-port bp (make-transcoder (utf-8-codec)))])
    (let loop ([v (if #f #f)])
      (let ([x (read p)])
        (if (eof-object? x) v (loop (eval x)))))))

;; (library-directories): the search path as Chez gives it, a list of
;; (source-directory . object-directory) pairs; set with a list of
;; those, of directories, or a string of them separated by colons
(define library-directories
  (case-lambda
    (() (%chez:library-directories))
    ((dirs)
     (%chez:set-library-directories!
       (search-path-list 'library-directories dirs (library-directories)
         (lambda (dir) (cons dir dir)))))))

;; A search path given as a string, "a:b::c:", is split at colons: "b::c"
;; is the pair (b . c), and a final colon appends DEFAULT.  Given as a
;; list, each element is a string or a pair of strings.
(define (search-path-list who x default single)
  (cond
    ((string? x)
     (let loop ((parts (split-at-colons x)) (acc '()))
       (cond ((null? parts) (reverse acc))
             ((and (string=? (car parts) "") (null? (cdr parts)))
              (append (reverse acc) default))
             ((and (pair? (cdr parts)) (string=? (cadr parts) "") (pair? (cddr parts)))
              (loop (cdddr parts) (cons (cons (car parts) (caddr parts)) acc)))
             ((string=? (car parts) "") (loop (cdr parts) acc))
             (else (loop (cdr parts) (cons (single (car parts)) acc))))))
    ((and (list? x)
          (for-all (lambda (e) (or (string? e) (string-pair? e))) x))
     (map (lambda (e) (if (string? e) (single e) e)) x))
    (else (assertion-violation who "invalid path list" x))))
(define (split-at-colons s)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length s)) (reverse (cons (substring s start i) acc)))
          ((char=? (string-ref s i) #\:) (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
          (else (loop (+ i 1) start acc)))))

;; (library-extensions): (source-extension . object-extension) pairs;
;; a source extension alone has the object extension .so, or, if it has
;; two parts (.a1.sls), its first part's (.a1.so)
(define (object-extension ext)
  (let loop ((i (- (string-length ext) 1)))
    (cond ((<= i 0) ".so")
          ((char=? (string-ref ext i) #\.) (string-append (substring ext 0 i) ".so"))
          (else (loop (- i 1))))))
(define library-extensions
  (let ((value '((".chezscheme.sls" . ".chezscheme.so") (".ss" . ".so")
                 (".sls" . ".so") (".scm" . ".so") (".sch" . ".so"))))
    (case-lambda
      (() value)
      ((x)
       (set! value
         (search-path-list 'library-extensions x value
           (lambda (ext) (cons ext (object-extension ext)))))))))

;; The compiler's and the system's parameters: accepted, and mostly
;; without effect here (SBCL compiles everything with its own policy)
;; Chez's flag parameters take any value as a boolean
(define (->boolean x) (and x #t))
(define compile-imported-libraries (make-parameter #f ->boolean))
(define compile-file-message (make-parameter #t ->boolean))
(define generate-wpo-files (make-parameter #f ->boolean))
(define compile-interpret-simple (make-parameter #t ->boolean))
(define optimize-level (make-parameter 0))
(define debug-level (make-parameter 1))
(define commonization-level (make-parameter 0))
(define generate-inspector-information (make-parameter #t ->boolean))
(define generate-procedure-source-information (make-parameter #f ->boolean))
(define generate-interrupt-trap (make-parameter #t ->boolean))
(define enable-object-counts (make-parameter #f ->boolean))
(define undefined-variable-warnings (make-parameter #f ->boolean))
(define import-notify (make-parameter #f ->boolean))
(define library-timestamp-mode (make-parameter 'modification-time))
(define current-expand (make-parameter expand))
(define current-eval (make-parameter eval))
(define collect-request-handler (make-parameter (lambda () (collect))))
(define collect-trip-bytes (make-parameter (* 8 1024 1024)))
(define (collect-rendezvous) (collect))

;; The printer's parameters, which the writer (host.lisp) consults, except
;; print-precision and print-subnormal-precision
(define print-graph (make-parameter #f ->boolean))
(define print-length
  (make-parameter #f
    (lambda (x)
      (unless (or (not x) (and (fixnum? x) (>= x 0)))
        (assertion-violation 'print-length "invalid value" x))
      x)))
(define print-level
  (make-parameter #f
    (lambda (x)
      (unless (or (not x) (and (fixnum? x) (>= x 0)))
        (assertion-violation 'print-level "invalid value" x))
      x)))
(define print-radix
  (make-parameter 10
    (lambda (x)
      (unless (and (fixnum? x) (<= 2 x 36))
        (assertion-violation 'print-radix "invalid value" x))
      x)))
(define print-gensym
  (make-parameter #t
    (lambda (x)
      (unless (memq x '(#t #f pretty pretty/suffix))
        (assertion-violation 'print-gensym "invalid value" x))
      x)))
(define print-brackets (make-parameter #t ->boolean))
(define print-char-name (make-parameter #f ->boolean))
(define print-vector-length (make-parameter #f ->boolean))
(define print-precision
  (make-parameter #f
    (lambda (x)
      (unless (or (not x) (and (fixnum? x) (> x 0)))
        (assertion-violation 'print-precision "invalid value" x))
      x)))
(define print-extended-identifiers (make-parameter #f ->boolean))
(define print-unicode (make-parameter #t ->boolean))
(define pretty-line-length (make-parameter 75))
(define pretty-one-line-limit (make-parameter 60))
(define pretty-initial-indent (make-parameter 0))
(define pretty-standard-indent (make-parameter 1))
(define pretty-maximum-lines (make-parameter #f))
(define case-sensitive (make-parameter #t ->boolean))

;;; The system

(define command-line-arguments
  (case-lambda
    (() (let ((cl (command-line))) (if (pair? cl) (cdr cl) '())))
    ((args) (let ((cl (command-line)))
              (command-line (cons (if (pair? cl) (car cl) "") args))))))

(define get-process-id %chez:get-process-id)
(define char-ready? %char-ready?)
(define putenv %chez:putenv)
(define (scheme-version) "Quasischeme (as Chez Scheme Version 10.4.1)")
(define (scheme-version-number) (values 10 4 1))
(define (petite?) #f)
(define (interactive?) #f)
(define abort-handler (make-parameter (lambda args (exit 1))))
(define (abort . x) (apply (abort-handler) x))
(define exit-handler (make-parameter exit))
(define reset-handler (make-parameter (lambda () (exit 1))))
(define (reset) ((reset-handler)))
(define (cd . dir) (apply current-directory dir))

;;; Interrupts, timers and engines.  Nothing interrupts Scheme code
;;; but the timer, a span of time for a tick (host.lisp), and a thread's
;;; engines, which run in threads of their own.  Disabling interrupts
;;; holds the timer's handler back until they are enabled again.

(define (disable-interrupts) (%chez:sys-disable-interrupts))
(define (enable-interrupts) (%chez:sys-enable-interrupts))
(define-syntax with-interrupts-disabled
  (syntax-rules ()
    ((_ body1 body2 ...)
     (dynamic-wind disable-interrupts (lambda () body1 body2 ...) enable-interrupts))))
(define-syntax critical-section
  (syntax-rules ()
    ((_ body1 body2 ...)
     (dynamic-wind disable-interrupts (lambda () body1 body2 ...) enable-interrupts))))
(define keyboard-interrupt-handler (make-parameter (lambda () (exit 130))))
(define timer-interrupt-handler (make-parameter (lambda () (if #f #f))))
(define (set-timer n) (%chez:sys-set-timer n))
(define timer-trampoline (%chez:sys-set-timer-trampoline (lambda () ((timer-interrupt-handler)))))
(define (register-signal-handler sig proc) (if #f #f))
(define make-engine %chez:make-engine)   ; engines run in threads (host.lisp)
(define engine-return %chez:engine-return)

;;; Syntax

;; (datum template): (syntax->datum (syntax template))
(define-syntax datum
  (lambda (x)
    (syntax-case x ()
      ((_ t) #'(syntax->datum (syntax t))))))

;; (rec var expr): a recursive expression
(define-syntax rec
  (syntax-rules ()
    ((_ var expr) (letrec ((var expr)) var))))

;; (eval-when (situation ...) form ...): here everything is evaluated as
;; it's expanded or run, so the forms are simply spliced in
(define-syntax eval-when
  (syntax-rules ()
    ((_ (situation ...) form ...) (begin form ...))))

;; #%car reads as ($primitive car): the system's car, whatever car is
;; where it's used: the name as this library has it (so also the system
;; procedures below, which (chezscheme) doesn't export)
(define-syntax $primitive
  (lambda (x)
    (syntax-case x ()
      ((_ name) (identifier? #'name) (datum->syntax #'here (syntax->datum #'name)))
      ((_ level name) (identifier? #'name) (datum->syntax #'here (syntax->datum #'name))))))

;; Chez's system procedures for record types whose descriptors have
;; fields of their own (as oop.ss's vtables do): $make-record-type makes
;; a record type descriptor that is a record of the type BASE (#!base-rtd
;; or a type descending from it), with EXTRAS as BASE's fields (FLAGS,
;; Chez's own, are ignored).  Field
;; offsets here stand for positions among a record's fields.
(define ($make-record-type base parent name fields sealed? opaque? flags . extras)
  (let ((rtd (make-record-type parent name fields)))
    (%chez:set-record-type-type! rtd base extras sealed? opaque?)
    rtd))
(define ($record rtd . values) (%chez:make-record rtd values))
(define ($record-type-field-offsets rtd) (%chez:record-type-field-offsets rtd))
(define ($object-ref type x offset) (%chez:object-ref x offset))
(define ($object-set! type x offset value) (%chez:object-set! x offset value))
(define ($record-ref x i) (%chez:object-ref x (* 8 (+ i 1))))
(define ($record-set! x i value) (%chez:object-set! x (* 8 (+ i 1)) value))
(define ($record-type-descriptor x) (%chez:record-type-of x))

;; (fluid-let-syntax ((keyword transformer) ...) body ...): the body
;; with each keyword's binding replaced wherever it's referred to, which
;; is SRFI 139's syntax-parameterize as psyntax has it
(define-syntax fluid-let-syntax
  (syntax-rules ()
    ((_ ((k e) ...) b1 b2 ...) (syntax-parameterize ((k e) ...) b1 b2 ...))))

;; Chez's older name for free-identifier=?
(define literal-identifier=? free-identifier=?)

(define (syntax->list x)
  (syntax-case x ()
    ((a ...) #'(a ...))))

(define (syntax->vector x)
  (syntax-case x ()
    (#(a ...) #'#(a ...))))

;; Inspection and debugging: not here
(define (inspect x) (if #f #f))
(define (inspect/object x) (if #f #f))
(define (debug) (if #f #f))
(define (break . args) (if #f #f))
(define procedure-arity-mask %chez:procedure-arity-mask)
(define make-wrapper-procedure %chez:make-wrapper-procedure)
(define make-arity-wrapper-procedure %chez:make-arity-wrapper-procedure)
(define wrapper-procedure? %chez:wrapper-procedure?)
(define wrapper-procedure-procedure %chez:wrapper-procedure-procedure)
(define wrapper-procedure-data %chez:wrapper-procedure-data)
(define set-wrapper-procedure-procedure! %chez:set-wrapper-procedure-procedure!)
(define set-wrapper-procedure-data! %chez:set-wrapper-procedure-data!)

;;; Ports beyond R6RS

(define port-closed? %chez:port-closed?)
(define (fresh-line . port) (apply %chez:fresh-line port))
(define get-bytevector-some! %chez:get-bytevector-some!)
(define (put-bytevector-some port bv . range)
  (apply put-bytevector port bv range)
  (if (null? range) (bytevector-length bv) (cadr range)))
(define (truncate-file . args) (if #f #f))
(define (set-port-length! port n) (if #f #f))

;;; Source objects and annotations.  An annotation (src/chez/host.lisp)
;;; stands for its expression wherever psyntax looks at syntax;
;;; get-datum/annotations reads a datum annotating each part with its
;;; source object.  load and the reader proper don't annotate.

(define (source-file-descriptor? x) (%chez:sfd? x))
(define (source-file-descriptor-path x)
  (unless (source-file-descriptor? x) (assertion-violation 'source-file-descriptor-path "not a source file descriptor" x))
  (%chez:sfd-path x))
(define (source-file-descriptor-checksum x)
  (unless (source-file-descriptor? x) (assertion-violation 'source-file-descriptor-checksum "not a source file descriptor" x))
  (%chez:sfd-checksum x))
(define (make-sfd path checksum) (%chez:make-sfd path checksum))
(define (source-file-descriptor path checksum)
  (unless (and (integer? checksum) (exact? checksum) (>= checksum 0))
    (assertion-violation 'source-file-descriptor "not an exact nonnegative integer" checksum))
  (make-sfd path checksum))
(define (bytes-checksum bv)
  (let loop ([i 0] [h 0])
    (if (= i (bytevector-length bv))
        h
        (loop (+ i 1) (bitwise-and (+ (* h 31) (bytevector-u8-ref bv i)) #xffffffff)))))
(define make-source-file-descriptor
  (case-lambda
    [(path bip) (make-source-file-descriptor path bip #f)]
    [(path bip reset?)
     (unless (and (input-port? bip) (binary-port? bip))
       (assertion-violation 'make-source-file-descriptor "not a binary input port" bip))
     (when (and reset? (not (and (port-has-port-position? bip) (port-has-set-port-position!? bip))))
       (assertion-violation 'make-source-file-descriptor "port does not support port-position and set-port-position!" bip))
     (let* ([start (and reset? (port-position bip))]
            [bv (get-bytevector-all bip)]
            [bv (if (eof-object? bv) (bytevector) bv)])
       (when reset? (set-port-position! bip start))
       (make-sfd path (bytes-checksum bv)))]))

(define (source-object? x) (%chez:source-object? x))
(define (check-so who x) (unless (source-object? x) (assertion-violation who "not a source object" x)))
(define (source-object-sfd x) (check-so 'source-object-sfd x) (%chez:source-object-sfd x))
(define (source-object-bfp x) (check-so 'source-object-bfp x) (%chez:source-object-bfp x))
(define (source-object-efp x) (check-so 'source-object-efp x) (%chez:source-object-efp x))
(define (source-object-line x) (check-so 'source-object-line x) (%chez:source-object-line x))
(define (source-object-column x) (check-so 'source-object-column x) (%chez:source-object-column x))
(define (%make-source-object sfd bfp efp line column) (%chez:make-source-object sfd bfp efp line column))
(define make-source-object
  (case-lambda
    [(sfd bfp efp) (check-source-object sfd bfp efp) (%make-source-object sfd bfp efp #f #f)]
    [(sfd bfp efp line column)
     (check-source-object sfd bfp efp)
     (unless (and (integer? line) (exact? line) (> line 0))
       (assertion-violation 'make-source-object "not an exact positive integer" line))
     (unless (and (integer? column) (exact? column) (> column 0))
       (assertion-violation 'make-source-object "not an exact positive integer" column))
     (%make-source-object sfd bfp efp line column)]))
(define (check-source-object sfd bfp efp)
  (unless (source-file-descriptor? sfd) (assertion-violation 'make-source-object "not a source file descriptor" sfd))
  (unless (and (integer? bfp) (exact? bfp)) (assertion-violation 'make-source-object "not an exact integer" bfp))
  (unless (and (integer? efp) (exact? efp)) (assertion-violation 'make-source-object "not an exact integer" efp))
  (unless (<= 0 bfp efp) (assertion-violation 'make-source-object "bfp is not between 0 and efp" bfp efp)))
(define current-make-source-object
  (make-parameter make-source-object
    (lambda (p) (unless (procedure? p) (assertion-violation 'current-make-source-object "not a procedure" p)) p)))

(define-enumeration annotation-option (debug profile) annotation-options)
(define make-annotation
  (case-lambda
    [(e source stripped) (make-annotation e source stripped (annotation-options debug profile))]
    [(e source stripped options)
     (unless (source-object? source) (assertion-violation 'make-annotation "not a source object" source))
     (%chez:make-annotation e source stripped options)]))
(define (annotation? x) (%chez:annotation? x))
(define (check-annotation who x) (unless (annotation? x) (assertion-violation who "not an annotation" x)))
(define (annotation-expression a) (check-annotation 'annotation-expression a) (%chez:annotation-expression a))
(define (annotation-source a) (check-annotation 'annotation-source a) (%chez:annotation-source a))
(define (annotation-stripped a) (check-annotation 'annotation-stripped a) (%chez:annotation-stripped a))
(define (annotation-option-set a) (check-annotation 'annotation-option-set a) (%chez:annotation-options a))
(define (syntax->annotation x) (%chez:syntax->annotation x))

;; where in its file a position is: (values path line column), or none
;; if the file isn't there (with the same checksum)
(define locate-source
  (case-lambda
    [(sfd pos) (locate-source sfd pos #f)]
    [(sfd pos use-cache?)
     (unless (source-file-descriptor? sfd) (assertion-violation 'locate-source "not a source file descriptor" sfd))
     (unless (and (integer? pos) (exact? pos) (>= pos 0))
       (assertion-violation 'locate-source "not an exact nonnegative integer" pos))
     (let ([ip (open-source-file sfd)])
       (if (not ip)
           (values)
           (let loop ([n 0] [line 1] [col 1])
             (let ([c (read-char ip)])
               (cond
                 [(or (= n pos) (eof-object? c))
                  (close-port ip)
                  (values (source-file-descriptor-path sfd) line col)]
                 [(char=? c #\newline) (loop (+ n 1) (+ line 1) 1)]
                 [else (loop (+ n 1) line (+ col 1))])))))]))
(define (locate-source-object-source src start? cache?)
  (unless (source-object? src) (assertion-violation 'locate-source-object-source "not a source object" src))
  (if (and start? (source-object-line src))
      (values (source-file-descriptor-path (source-object-sfd src)) (source-object-line src) (source-object-column src))
      (locate-source (source-object-sfd src) (if start? (source-object-bfp src) (source-object-efp src)) cache?)))
(define current-locate-source-object-source
  (make-parameter locate-source-object-source
    (lambda (p) (unless (procedure? p) (assertion-violation 'current-locate-source-object-source "not a procedure" p)) p)))
(define (open-source-file sfd)
  (let ([path (source-file-descriptor-path sfd)])
    (and (string? path) (file-exists? path)
         (let ([bv (call-with-port (open-file-input-port path) get-bytevector-all)])
           (and (= (bytes-checksum (if (eof-object? bv) (bytevector) bv)) (source-file-descriptor-checksum sfd))
                (open-input-file path))))))

;; get-datum/annotations: a reader that annotates each datum with its
;; source object (positions in characters from BFP)
(define (get-datum/annotations ip sfd bfp)
  (unless (and (input-port? ip) (textual-port? ip))
    (assertion-violation 'get-datum/annotations "not a textual input port" ip))
  (unless (source-file-descriptor? sfd)
    (assertion-violation 'get-datum/annotations "not a source file descriptor" sfd))
  (unless (and (integer? bfp) (exact? bfp) (>= bfp 0))
    (assertion-violation 'get-datum/annotations "not a valid file position" bfp))
  (let ([pos bfp])
    (define (peek) (peek-char ip))
    (define (next) (let ([c (read-char ip)]) (unless (eof-object? c) (set! pos (+ pos 1))) c))
    (define (annotate e start stripped)
      (make-annotation e (make-source-object sfd start pos) stripped))
    (define (delimiter? c)
      (or (eof-object? c) (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\" #\;))))
    (define (skip-atmosphere)
      (let ([c (peek)])
        (cond
          [(eof-object? c) #f]
          [(char-whitespace? c) (next) (skip-atmosphere)]
          [(char=? c #\;) (let loop () (let ([c (next)]) (unless (or (eof-object? c) (char=? c #\newline)) (loop)))) (skip-atmosphere)]
          [else #f])))
    (define (read-token)
      (let loop ([cs '()])
        (if (delimiter? (peek)) (list->string (reverse cs)) (loop (cons (next) cs)))))
    (define (read-list close start)
      (let loop ([items '()])
        (skip-atmosphere)
        (let ([c (peek)])
          (cond
            [(eof-object? c) (error 'get-datum/annotations "unexpected end of file")]
            [(memv c '(#\) #\])) (next)
             (let ([es (reverse items)])
               (annotate es start (map annotation-stripped es)))]
            [(and (char=? c #\.) (pair? items))
             (let ([dot-start pos] [tok (read-token)])
               (if (string=? tok ".")
                   (let ([tail (read-one)])
                     (skip-atmosphere) (next)
                     (let ([es (reverse items)])
                       (annotate (append es tail) start (append (map annotation-stripped es) (annotation-stripped tail)))))
                   (loop (cons (atom tok dot-start) items))))]
            [else (loop (cons (read-one) items))]))))
    (define (atom tok start)
      (let ([d (read (open-input-string tok))]) (annotate d start d)))
    (define (prefixed sym start)
      (let ([x (read-one)])
        (annotate (list (annotate sym start sym) x) start (list sym (annotation-stripped x)))))
    (define (read-one)
      (skip-atmosphere)
      (let ([start pos] [c (peek)])
        (cond
          [(eof-object? c) c]
          [(memv c '(#\( #\[)) (next) (read-list c start)]
          [(char=? c #\') (next) (prefixed 'quote start)]
          [(char=? c #\`) (next) (prefixed 'quasiquote start)]
          [(char=? c #\,) (next)
           (if (eqv? (peek) #\@) (begin (next) (prefixed 'unquote-splicing start)) (prefixed 'unquote start))]
          [(char=? c #\")
           (next)
           (let loop ([cs (list #\")])
             (let ([c (next)])
               (cond
                 [(char=? c #\\) (loop (cons (next) (cons c cs)))]
                 [(char=? c #\") (atom (list->string (reverse (cons c cs))) start)]
                 [else (loop (cons c cs))])))]
          [(char=? c #\#)
           (next)
           (let ([d (peek)])
             (cond
               [(eqv? d #\() (next)
                (let ([l (read-list #\( start)])
                  (annotate (list->vector (annotation-expression l)) start
                            (list->vector (annotation-stripped l))))]
               [(eqv? d #\') (next) (prefixed 'syntax start)]
               [(eqv? d #\`) (next) (prefixed 'quasisyntax start)]
               [(eqv? d #\,) (next)
                (if (eqv? (peek) #\@) (begin (next) (prefixed 'unsyntax-splicing start)) (prefixed 'unsyntax start))]
               [(eqv? d #\\) (next)
                (let ([first (next)])
                  (atom (string-append "#\\" (string first) (read-token)) start))]
               [else (atom (string-append "#" (read-token)) start)]))]
          [else (atom (read-token) start)])))
    (let ([x (read-one)])
      (values x pos))))
(define (read-token . args)
  (error 'read-token "not supported"))

(define compile-library-handler
  (make-parameter (lambda (in out) (compile-library in out))
    (lambda (x) (unless (procedure? x) (assertion-violation 'compile-library-handler "not a procedure" x)) x)))
(define engine-block %chez:engine-block)
(define (r6rs:command-line) (command-line))
