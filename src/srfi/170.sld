;;; SRFI 170: POSIX API.  Written for Pseudoscheme, on osicat
;;; (OSICAT-POSIX, its thin CFFI layer over libc), which is loaded from
;;; Quicklisp or ASDF the first time this library is imported.  Building
;;; osicat needs a C compiler: CFFI's groveller compiles a small program
;;; to read the C headers' constants and struct layouts, and osicat
;;; compiles a few C wrappers into a shared library.
;;;
;;; Errors: a failing call signals osicat's POSIX-ERROR, a Lisp error
;;; condition, which Scheme handlers receive as it is.  posix-error?
;;; recognizes it, and it is also an error-object?.  Errors that aren't
;;; from a system call (a wrong argument type) are ordinary errors.
;;;
;;; Time objects are SRFI 19's (srfi 19).
;;;
;;; Notes and limits:
;;; - posix-time has microsecond resolution (gettimeofday), and
;;;   monotonic-time is osicat's get-monotonic-time: osicat has no
;;;   clock_gettime on macOS.
;;; - set-file-times reads the clock, and stat()s the file for
;;;   time/unchanged, rather than passing UTIME_NOW and UTIME_OMIT, which
;;;   osicat doesn't export.  set-file-owner does the same for
;;;   owner/unchanged and group/unchanged.
;;; - A port's file descriptor (file-info, truncate-file, file-space and
;;;   terminal? on a port) is known for SBCL, CCL and ECL file streams,
;;;   synonym streams to them (the standard ports), and the ports
;;;   open-file and fd->port make.  On other ports these raise an error
;;;   (terminal? returns #f).
;;; - The buffer-mode arguments are accepted and ignored.
;;; - set-current-directory! also sets Lisp's *default-pathname-defaults*,
;;;   which is what the Lisp's own file operations (and so R7RS's
;;;   open-input-file and friends) resolve relative names against.
(define-library (srfi 170)
  (export
   ;; 3.1 errors
   posix-error? posix-error-name posix-error-message
   ;; 3.2 I/O
   open-file fd->port
   binary-input textual-input binary-output textual-output
   binary-input/output buffer-none buffer-block buffer-line
   open/append open/create open/exclusive open/nofollow open/truncate
   ;; 3.3 file system
   create-directory create-fifo create-hard-link create-symlink
   read-symlink rename-file delete-directory set-file-owner
   owner/unchanged group/unchanged set-file-times time/now time/unchanged
   truncate-file file-info file-info?
   file-info:device file-info:inode file-info:mode file-info:nlinks
   file-info:uid file-info:gid file-info:rdev file-info:size
   file-info:blksize file-info:blocks file-info:atime file-info:mtime
   file-info:ctime
   file-info-directory? file-info-fifo? file-info-symlink?
   file-info-regular? file-info-socket? file-info-device?
   set-file-mode directory-files make-directory-files-generator
   open-directory read-directory close-directory real-path file-space
   temp-file-prefix create-temp-file call-with-temporary-filename
   ;; 3.5 process state
   umask set-umask! current-directory set-current-directory! pid nice
   user-uid user-gid user-effective-uid user-effective-gid
   user-supplementary-gids
   ;; 3.6 user and group database
   user-info user-info? user-info:name user-info:uid user-info:gid
   user-info:home-dir user-info:shell user-info:full-name
   user-info:parsed-full-name
   group-info group-info? group-info:name group-info:gid
   ;; 3.10 time
   posix-time monotonic-time
   ;; 3.11 environment variables
   set-environment-variable! delete-environment-variable!
   ;; 3.12 terminals
   terminal?)
  (import (scheme base) (scheme char) (scheme process-context)
          (only (srfi 19) make-time time-utc time-monotonic time-second
                time-nanosecond)
          (only (rnrs conditions (6)) define-condition-type condition &error
                make-who-condition make-message-condition make-irritants-condition)
          (only (rnrs io ports (6))
                make-custom-binary-input-port
                make-custom-binary-input/output-port)
          (pseudoscheme lisp)
          ;; Loads the osicat system; its other packages come with it.
          (prefix (only (cl osicat) get-monotonic-time) osicat:)
          (prefix (cl osicat-posix) nix:)
          (prefix (only (cl osicat-sys) system-error-code
                        system-error-identifier)
                  sys:)
          (prefix (only (cl common-lisp) typep symbol-name logand logior
                        lognot ash read-byte write-byte read-sequence
                        write-sequence finish-output close
                        unsigned-byte pathname *default-pathname-defaults*
                        random gethash handler-case)
                  cl:)
          (prefix (only (cl trivial-garbage) make-weak-hash-table) tg:))
  (begin
    ;; ------------------------------------------------------------
    ;; 3.1 Errors

    ;; A failing call raises an R6RS condition: &posix-error (a kind of
    ;; &error), with &who, &message (strerror) and &irritants (the
    ;; call's arguments).  osicat's own condition is caught where the
    ;; call is made (call-posix), because by the time a Scheme handler
    ;; sees a Lisp condition it has been converted to a generic one.
    (define-condition-type &posix-error &error
      make-posix-error-condition posix-error?
      (name posix-error-name)
      (errno posix-error-errno)
      (syscall posix-error-syscall))

    (define (posix-error-message e)
      (nix:strerror (posix-error-errno e)))

    ;; The errno name for osicat's condition.  osicat's errno table
    ;; gives some codes several names, and on macOS can name a POSIX
    ;; errno after a Linux-only one (ENOTEMPTY as ENONET), so a POSIX
    ;; name with the same value is preferred.
    (define %errno-name
      (lisp-eval-string
       "(let ((posix '(\"E2BIG\" \"EACCES\" \"EADDRINUSE\" \"EADDRNOTAVAIL\" \"EAFNOSUPPORT\"
                     \"EAGAIN\" \"EALREADY\" \"EBADF\" \"EBADMSG\" \"EBUSY\" \"ECANCELED\"
                     \"ECHILD\" \"ECONNABORTED\" \"ECONNREFUSED\" \"ECONNRESET\" \"EDEADLK\"
                     \"EDESTADDRREQ\" \"EDOM\" \"EDQUOT\" \"EEXIST\" \"EFAULT\" \"EFBIG\"
                     \"EHOSTUNREACH\" \"EIDRM\" \"EILSEQ\" \"EINPROGRESS\" \"EINTR\" \"EINVAL\"
                     \"EIO\" \"EISCONN\" \"EISDIR\" \"ELOOP\" \"EMFILE\" \"EMLINK\" \"EMSGSIZE\"
                     \"EMULTIHOP\" \"ENAMETOOLONG\" \"ENETDOWN\" \"ENETRESET\" \"ENETUNREACH\"
                     \"ENFILE\" \"ENOBUFS\" \"ENODATA\" \"ENODEV\" \"ENOENT\" \"ENOEXEC\"
                     \"ENOLCK\" \"ENOLINK\" \"ENOMEM\" \"ENOMSG\" \"ENOPROTOOPT\" \"ENOSPC\"
                     \"ENOSR\" \"ENOSTR\" \"ENOSYS\" \"ENOTCONN\" \"ENOTDIR\" \"ENOTEMPTY\"
                     \"ENOTRECOVERABLE\" \"ENOTSOCK\" \"ENOTSUP\" \"ENOTTY\" \"ENXIO\"
                     \"EOPNOTSUPP\" \"EOVERFLOW\" \"EOWNERDEAD\" \"EPERM\" \"EPIPE\" \"EPROTO\"
                     \"EPROTONOSUPPORT\" \"EPROTOTYPE\" \"ERANGE\" \"EROFS\" \"ESPIPE\" \"ESRCH\"
                     \"ESTALE\" \"ETIME\" \"ETIMEDOUT\" \"ETXTBSY\" \"EWOULDBLOCK\" \"EXDEV\")))
          (lambda (c)
            (let ((code (osicat-sys:system-error-code c))
                  (id (osicat-sys:system-error-identifier c)))
              (flet ((posix-p (k) (member (symbol-name k) posix :test #'string=)))
                (symbol-name
                 (cond ((and id (posix-p id)) id)
                       ((find-if (lambda (k)
                                   (and (posix-p k)
                                        (eql code (cffi:foreign-enum-value
                                                   'osicat-posix::errno-values k :errorp nil))))
                                 (cffi:foreign-enum-keyword-list 'osicat-posix::errno-values)))
                       (id)
                       (t (format nil \"E~D\" code))))))))"))

    ;; Call F on ARGS, raising a &posix-error if osicat signals one.
    (define (call-posix who f args)
      (let ((results '())
            (failure #f))
        (let ((body (lambda ()
                      (set! results (call-with-values (lambda () (apply f args)) list))))
              (fail (lambda (c) (set! failure c))))
          (cl:handler-case (body)
            (nix:posix-error (c) (fail c))))
        (if failure
            (raise
             (condition
              (make-posix-error-condition
               (string->symbol (%errno-name failure))
               (sys:system-error-code failure)
               (let ((syscall (nix:posix-error-syscall failure)))
                 (if (string? syscall) (string->symbol syscall) #f)))
              (make-who-condition who)
              (make-message-condition (nix:strerror (sys:system-error-code failure)))
              (make-irritants-condition args)))
            (apply values results))))

    (define-syntax define-posix
      (syntax-rules ()
        ((_ (name who f) ...)
         (begin (define (name . args) (call-posix 'who f args)) ...))))

    (define-posix
      ;; open() is variadic, and osicat declares it with a fixed mode
      ;; argument, which on Apple's arm64 ABI (variadic arguments go on
      ;; the stack) passes a garbage mode.
      (%open open-file
             (lisp-eval-string
              "(lambda (path flags mode)
                 (let ((fd (cffi:foreign-funcall-varargs
                            \"open\" (:string path :int flags) :int mode :int)))
                   (if (minusp fd)
                       (osicat-posix:posix-error (osicat-posix:get-errno) path \"open\")
                       fd)))"))
      (%mkdir create-directory nix:mkdir)
      (%mkfifo create-fifo nix:mkfifo) (%link create-hard-link nix:link)
      (%symlink create-symlink nix:symlink) (%readlink read-symlink nix:readlink)
      (%rename rename-file nix:rename) (%rmdir delete-directory nix:rmdir)
      (%chmod set-file-mode nix:chmod) (%chown set-file-owner nix:chown)
      (%utimensat set-file-times nix:utimensat) (%truncate truncate-file nix:truncate)
      (%ftruncate truncate-file nix:ftruncate) (%fstat file-info nix:fstat)
      (%stat file-info nix:stat) (%lstat file-info nix:lstat)
      (%opendir open-directory nix:opendir) (%readdir read-directory nix:readdir)
      (%closedir close-directory nix:closedir) (%realpath real-path nix:realpath)
      (%statvfs file-space nix:statvfs) (%fstatvfs file-space nix:fstatvfs)
      ;; Raw: the interop would take MKSTEMP for a predicate (its name
      ;; ends in P) and keep only its first value.
      (%mkstemp create-temp-file (lisp-function "mkstemp" "osicat-posix"))
      (%chdir set-current-directory! nix:chdir) (%nice nice nix:nice)
      (%getpwnam user-info nix:getpwnam) (%getpwuid user-info nix:getpwuid)
      (%getgrnam group-info nix:getgrnam) (%getgrgid group-info nix:getgrgid)
      (%isatty terminal? nix:isatty) (%unsetenv delete-environment-variable! nix:unsetenv)
      ;; osicat's setenv passes C's setenv no overwrite argument.
      (%setenv set-environment-variable!
               (lisp-eval-string
                "(lambda (name value)
                   (when (minusp (cffi:foreign-funcall \"setenv\" :string name :string value
                                                       :int 1 :int))
                     (osicat-posix:posix-error))
                   t)")))

    ;; ------------------------------------------------------------
    ;; Ports and file descriptors

    ;; Lisp helpers, as raw Lisp functions (no boolean conversion).
    ;; The descriptor under a Lisp stream, or NIL.
    (define %stream-fd
      (lisp-eval-string
       "(labels ((fd (s)
                   (typecase s
                     (synonym-stream (fd (symbol-value (synonym-stream-symbol s))))
                     (two-way-stream (fd (if (output-stream-p s)
                                             (two-way-stream-output-stream s)
                                             (two-way-stream-input-stream s))))
                     (t #+sbcl (and (typep s 'sb-sys:fd-stream) (sb-sys:fd-stream-fd s))
                        #+ccl (ignore-errors (ccl::stream-device s (if (output-stream-p s) :output :input)))
                        #+ecl (ignore-errors (ext:file-stream-fd s))
                        #-(or sbcl ccl ecl) nil))))
          #'fd)"))
    (define %make-fd-stream
      (lisp-eval-string
       "(lambda (fd direction element-type)
          (if (fboundp 'osicat::make-fd-stream)
              (osicat::make-fd-stream fd :direction direction :element-type element-type
                                         :external-format :utf-8)
              (open (format nil \"/dev/fd/~D\" fd) :direction direction
                    :element-type element-type :external-format :utf-8
                    :if-exists :append)))"))

    ;; Ports made here -> their descriptors (the custom binary ports
    ;; are not fd streams).
    (define port-fds (tg:make-weak-hash-table #:weakness #:key))
    (define (remember-fd! port fd)
      (lisp-set! (cl:gethash port port-fds) fd)
      port)

    (define (port->fd port)
      (let ((fd (cl:gethash port port-fds)))
        (if (exact-integer? fd)
            fd
            (let ((fd (%stream-fd port)))
              (and (exact-integer? fd) fd)))))

    (define (port-fd who port)
      (or (port->fd port) (error "no file descriptor for this port" who port)))

    (define binary-input 'binary-input)
    (define textual-input 'textual-input)
    (define binary-output 'binary-output)
    (define textual-output 'textual-output)
    (define binary-input/output 'binary-input/output)
    (define buffer-none 'none)
    (define buffer-block 'block)
    (define buffer-line 'line)

    (define open/append nix:o-append)
    (define open/create nix:o-creat)
    (define open/exclusive nix:o-excl)
    (define open/nofollow nix:o-nofollow)
    (define open/truncate nix:o-trunc)

    (define octet (list cl:unsigned-byte 8))
    ;; (cl:character is the function.)
    (define character-type (lisp-symbol "character" "cl"))

    (define (fd->port fd port-type . buffer-mode)
      (case port-type
        ((textual-input)
         (remember-fd! (%make-fd-stream fd #:input character-type) fd))
        ((textual-output)
         (remember-fd! (%make-fd-stream fd #:output character-type) fd))
        ((binary-output)
         (remember-fd! (%make-fd-stream fd #:output octet) fd))
        ((binary-input)
         (let ((s (%make-fd-stream fd #:input octet)))
           (remember-fd!
            (make-custom-binary-input-port "fd" (reader s) #f #f
                                           (lambda () (cl:close s)))
            fd)))
        ((binary-input/output)
         (let ((s (%make-fd-stream fd #:io octet)))
           (remember-fd!
            (make-custom-binary-input/output-port
             "fd" (reader s) (writer s) #f #f (lambda () (cl:close s)))
            fd)))
        (else (error "fd->port: unknown port type" port-type))))

    (define (reader s)
      (lambda (bv start count)
        (if (zero? count)
            0
            (let ((b (cl:read-byte s #f 'eof)))
              (if (eq? b 'eof)
                  0
                  (begin (bytevector-u8-set! bv start b) 1))))))

    (define (writer s)
      (lambda (bv start count)
        (cl:write-sequence bv s #:start start #:end (+ start count))
        (cl:finish-output s)
        count))

    (define (open-file fname port-type flags . opts)
      (let* ((perm (if (pair? opts) (car opts) #o666))
             (access (case port-type
                       ((binary-input textual-input) nix:o-rdonly)
                       ((binary-output textual-output) nix:o-wronly)
                       ((binary-input/output) nix:o-rdwr)
                       (else (error "open-file: unknown port type" port-type))))
             (fd (%open fname (cl:logior access flags) perm)))
        (fd->port fd port-type)))

    ;; ------------------------------------------------------------
    ;; 3.3 File system

    (define (create-directory fname . perm)
      (%mkdir fname (if (pair? perm) (car perm) #o775))
      (if #f #f))
    (define (create-fifo fname . perm)
      (%mkfifo fname (if (pair? perm) (car perm) #o664))
      (if #f #f))
    (define (create-hard-link old new) (%link old new) (if #f #f))
    (define (create-symlink old new) (%symlink old new) (if #f #f))
    (define (read-symlink fname) (%readlink fname))
    (define (rename-file old new) (%rename old new) (if #f #f))
    (define (delete-directory fname) (%rmdir fname) (if #f #f))
    (define (set-file-mode fname mode) (%chmod fname mode) (if #f #f))

    (define owner/unchanged -1)
    (define group/unchanged -1)
    (define (set-file-owner fname uid gid)
      (let ((info (and (or (eqv? uid owner/unchanged) (eqv? gid group/unchanged))
                       (file-info fname #t))))
        (%chown fname
                   (if (eqv? uid owner/unchanged) (file-info:uid info) uid)
                   (if (eqv? gid group/unchanged) (file-info:gid info) gid))
        (if #f #f)))

    (define time/now 'time/now)
    (define time/unchanged 'time/unchanged)
    (define (set-file-times fname . times)
      (let* ((times (if (null? times) (list time/now time/now) times))
             (info (and (memq time/unchanged times) (file-info fname #t)))
             (now (posix-time))
             (resolve (lambda (t old)
                        (cond ((eq? t time/now) now)
                              ((eq? t time/unchanged) (old info))
                              (else t))))
             (atime (resolve (car times) file-info:atime))
             (mtime (resolve (cadr times) file-info:mtime)))
        (%utimensat nix:at-fdcwd fname
                       (time-second atime) (time-nanosecond atime)
                       (time-second mtime) (time-nanosecond mtime)
                       0)
        (if #f #f)))

    (define (truncate-file fname/port len)
      (if (string? fname/port)
          (%truncate fname/port len)
          (%ftruncate (port-fd 'truncate-file fname/port) len))
      (if #f #f))

    (define-record-type file-info-record
      (make-file-info device inode mode nlinks uid gid rdev size blksize
                      blocks atime mtime ctime)
      file-info?
      (device file-info:device) (inode file-info:inode)
      (mode file-info:mode) (nlinks file-info:nlinks)
      (uid file-info:uid) (gid file-info:gid) (rdev file-info:rdev)
      (size file-info:size) (blksize file-info:blksize)
      (blocks file-info:blocks) (atime file-info:atime)
      (mtime file-info:mtime) (ctime file-info:ctime))

    (define (file-info fname/port follow?)
      (let ((s (cond ((not (string? fname/port))
                      (%fstat (port-fd 'file-info fname/port)))
                     (follow? (%stat fname/port))
                     (else (%lstat fname/port)))))
        (make-file-info
         (nix:stat-dev s) (nix:stat-ino s) (nix:stat-mode s)
         (nix:stat-nlink s) (nix:stat-uid s) (nix:stat-gid s)
         (nix:stat-rdev s) (nix:stat-size s) (nix:stat-blksize s)
         (nix:stat-blocks s)
         (make-time time-utc (nix:stat-atime-nsec s) (nix:stat-atime-sec s))
         (make-time time-utc (nix:stat-mtime-nsec s) (nix:stat-mtime-sec s))
         (make-time time-utc (nix:stat-ctime-nsec s) (nix:stat-ctime-sec s)))))

    (define (file-type? info type)
      (= (cl:logand (file-info:mode info) nix:s-ifmt) type))
    (define (file-info-directory? info) (file-type? info nix:s-ifdir))
    (define (file-info-fifo? info) (file-type? info nix:s-ififo))
    (define (file-info-symlink? info) (file-type? info nix:s-iflnk))
    (define (file-info-regular? info) (file-type? info nix:s-ifreg))
    (define (file-info-socket? info) (file-type? info nix:s-ifsock))
    (define (file-info-device? info)
      (or (file-type? info nix:s-ifchr) (file-type? info nix:s-ifblk)))

    ;; Directories

    (define-record-type directory-object
      (make-directory-object pointer dot-files?)
      directory-object?
      (pointer directory-pointer set-directory-pointer!)
      (dot-files? directory-dot-files?))

    (define (open-directory dir . dot-files?)
      (make-directory-object (%opendir dir)
                             (and (pair? dot-files?) (car dot-files?))))

    (define (read-directory d)
      (let ((p (directory-pointer d)))
        (unless p (error "read-directory: directory is closed" d))
        (let loop ()
          (let ((name (call-with-values (lambda () (%readdir p))
                        (lambda (name . _) name))))
            (cond ((not (string? name)) (eof-object))
                  ((or (string=? name ".") (string=? name ".."))
                   (loop))
                  ((and (not (directory-dot-files? d))
                        (char=? (string-ref name 0) #\.))
                   (loop))
                  (else name))))))

    (define (close-directory d)
      (let ((p (directory-pointer d)))
        (when p
          (set-directory-pointer! d #f)
          (%closedir p))
        (if #f #f)))

    (define (directory-files . args)
      (let* ((dir (if (pair? args) (car args) "."))
             (dot-files? (and (pair? args) (pair? (cdr args)) (cadr args)))
             (d (open-directory dir dot-files?)))
        (let loop ((names '()))
          (let ((name (read-directory d)))
            (if (eof-object? name)
                (begin (close-directory d) (reverse names))
                (loop (cons name names)))))))

    (define (make-directory-files-generator . args)
      (let* ((dir (if (pair? args) (car args) "."))
             (dot-files? (and (pair? args) (pair? (cdr args)) (cadr args)))
             (d (open-directory dir dot-files?)))
        (lambda ()
          (if (directory-pointer d)
              (let ((name (read-directory d)))
                (when (eof-object? name) (close-directory d))
                name)
              (eof-object)))))

    (define (real-path path) (%realpath path))

    ;; Bytes available to unprivileged users: f_bavail * f_frsize.
    (define (file-space path-or-port)
      (call-with-values
          (lambda ()
            (if (string? path-or-port)
                (%statvfs path-or-port)
                (%fstatvfs (port-fd 'file-space path-or-port))))
        (lambda (bsize frsize blocks bfree bavail . _)
          (* bavail frsize))))

    ;; Temporary files

    (define temp-file-prefix
      (make-parameter
       (string-append (or (get-environment-variable "TMPDIR") "/tmp")
                      "/" (number->string (nix:getpid)))))

    (define (create-temp-file . prefix)
      (call-with-values
          (lambda () (%mkstemp (if (pair? prefix) (car prefix) (temp-file-prefix))))
        (lambda (fd name)
          (nix:close fd)
          name)))

    (define (call-with-temporary-filename maker . prefix)
      (let ((prefix (if (pair? prefix) (car prefix) (temp-file-prefix))))
        (let loop ((tries 0))
          (when (> tries 1000)
            (error "call-with-temporary-filename: no unused name found" prefix))
          (let* ((name (string-append prefix (number->string (cl:random 100000000) 36)))
                 (results (call-with-current-continuation
                           (lambda (k)
                             (with-exception-handler
                              (lambda (e)
                                (if (and (posix-error? e)
                                         (eq? (posix-error-name e) 'EEXIST))
                                    (k #f)
                                    (raise-continuable e)))
                              (lambda ()
                                (call-with-values (lambda () (maker name)) list)))))))
            (if (and results (car results))
                (apply values results)
                (loop (+ tries 1)))))))

    ;; ------------------------------------------------------------
    ;; 3.5 Process state

    (define (umask)
      (let ((old (nix:umask 0)))
        (nix:umask old)
        old))
    (define (set-umask! mask) (nix:umask mask) (if #f #f))

    (define (current-directory) (nix:getcwd))
    (define (set-current-directory! dir)
      (%chdir dir)
      (let ((cwd (nix:getcwd)))
        (set! cl:*default-pathname-defaults*
              (cl:pathname (if (and (> (string-length cwd) 0)
                                    (char=? (string-ref cwd (- (string-length cwd) 1)) #\/))
                               cwd
                               (string-append cwd "/")))))
      (if #f #f))

    (define (pid) (nix:getpid))
    (define (nice . delta) (%nice (if (pair? delta) (car delta) 1)))
    (define (user-uid) (nix:getuid))
    (define (user-gid) (nix:getgid))
    (define (user-effective-uid) (nix:geteuid))
    (define (user-effective-gid) (nix:getegid))
    (define (user-supplementary-gids) (nix:getgroups))

    ;; ------------------------------------------------------------
    ;; 3.6 User and group database

    (define-record-type user-info-record
      (make-user-info name uid gid home-dir shell full-name)
      user-info?
      (name user-info:name) (uid user-info:uid) (gid user-info:gid)
      (home-dir user-info:home-dir) (shell user-info:shell)
      (full-name user-info:full-name))

    (define (user-info uid/name)
      (call-with-values
          (lambda ()
            (if (string? uid/name) (%getpwnam uid/name) (%getpwuid uid/name)))
        (lambda entry
          (if (and (pair? entry) (string? (car entry)))
              (apply (lambda (name passwd uid gid gecos dir shell)
                       (make-user-info name uid gid dir shell gecos))
                     entry)
              #f))))

    ;; The gecos field split on commas; & in the first part is the
    ;; user's name, capitalized.
    (define (user-info:parsed-full-name info)
      (let* ((parts (split-string (or (user-info:full-name info) "") #\,))
             (name (user-info:name info))
             (cap (if (and (> (string-length name) 0)
                           (char<=? #\a (string-ref name 0) #\z))
                      (string-append (string (char-upcase (string-ref name 0)))
                                     (substring name 1 (string-length name)))
                      name)))
        (cons (replace-ampersands (car parts) cap) (cdr parts))))

    (define (split-string s c)
      (let loop ((i 0) (start 0) (out '()))
        (cond ((= i (string-length s))
               (reverse (cons (substring s start i) out)))
              ((char=? (string-ref s i) c)
               (loop (+ i 1) (+ i 1) (cons (substring s start i) out)))
              (else (loop (+ i 1) start out)))))

    (define (replace-ampersands s name)
      (let loop ((cs (string->list s)) (out '()))
        (cond ((null? cs) (apply string-append (reverse out)))
              ((char=? (car cs) #\&) (loop (cdr cs) (cons name out)))
              (else (loop (cdr cs) (cons (string (car cs)) out))))))

    (define-record-type group-info-record
      (make-group-info name gid)
      group-info?
      (name group-info:name) (gid group-info:gid))

    (define (group-info gid/name)
      (call-with-values
          (lambda ()
            (if (string? gid/name) (%getgrnam gid/name) (%getgrgid gid/name)))
        (lambda entry
          (if (and (pair? entry) (string? (car entry)))
              (make-group-info (car entry) (car (cddr entry)))
              #f))))

    ;; ------------------------------------------------------------
    ;; 3.10 Time

    (define (posix-time)
      (call-with-values nix:gettimeofday
        (lambda (sec usec) (make-time time-utc (* usec 1000) sec))))

    (define (monotonic-time)
      (let* ((t (exact (osicat:get-monotonic-time)))
             (sec (floor t)))
        (make-time time-monotonic (round (* (- t sec) 1000000000)) sec)))

    ;; ------------------------------------------------------------
    ;; 3.11 Environment variables

    (define (set-environment-variable! name value)
      (%setenv name value)
      (if #f #f))
    (define (delete-environment-variable! name)
      (%unsetenv name)
      (if #f #f))

    ;; ------------------------------------------------------------
    ;; 3.12 Terminals

    (define (terminal? port)
      (let ((fd (port->fd port)))
        (and fd (= 1 (%isatty fd)))))))
