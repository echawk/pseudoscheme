;;; SRFI 238: Codesets.  Written for Pseudoscheme after Lassi Kortela's
;;; sample implementation (reference/srfi-238/; the SRFI's MIT licence):
;;; the codesets errno and signal, their names those of the sample's
;;; errnolist.txt and signallist.txt that SBCL defines here, with the
;;; numbers it gives them on this system.  errno messages are the C
;;; library's (strerror); signal messages, which SBCL has no access to,
;;; are the usual descriptions.
(define-library (srfi 238)
  (export codeset? codeset-symbols codeset-symbol codeset-number codeset-message)
  (import (scheme base) (scheme char) (only (pseudoscheme lisp) lisp-eval-string))
  (begin
    ;; ((number . symbol) ...), of the NAMES SBCL's package has constants for
    (define %alist
      (lisp-eval-string
       "(lambda (names package)
          (require :sb-posix)
          (loop for name in names
                for sym = (find-symbol (string-upcase (ps:scheme-symbol-name name)) package)
                when (and sym (boundp sym) (integerp (symbol-value sym)))
                  collect (cons (symbol-value sym) name)))"))
    (define %strerror (lisp-eval-string "(lambda (n) (sb-int:strerror n))"))

    (define errno-alist
      (%alist '(E2BIG EACCES EADDRINUSE EADDRNOTAVAIL EADV EAFNOSUPPORT EAGAIN EALREADY EAUTH EBADE EBADF EBADFD EBADMSG EBADR EBADRPC EBADRQC EBADSLT EBFONT EBUSY ECANCELED ECAPMODE ECHILD ECHRNG ECOMM ECONNABORTED ECONNREFUSED ECONNRESET EDEADLK EDEADLOCK EDESTADDRREQ EDOM EDOOFUS EDOTDOT EDQUOT EEXIST EFAULT EFBIG EFTYPE EHOSTDOWN EHOSTUNREACH EHWPOISON EIDRM EILSEQ EINPROGRESS EINTR EINVAL EIO EIPSEC EISCONN EISDIR EISNAM EKEYEXPIRED EKEYREJECTED EKEYREVOKED EL2HLT EL2NSYNC EL3HLT EL3RST ELAST ELIBACC ELIBBAD ELIBEXEC ELIBMAX ELIBSCN ELNRNG ELOCKUNMAPPED ELOOP EMEDIUMTYPE EMFILE EMLINK EMSGSIZE EMULTIHOP ENAMETOOLONG ENAVAIL ENEEDAUTH ENETDOWN ENETRESET ENETUNREACH ENFILE ENOANO ENOATTR ENOBUFS ENOCSI ENODATA ENODEV ENOENT ENOEXEC ENOKEY ENOLCK ENOLINK ENOMEDIUM ENOMEM ENOMSG ENONET ENOPKG ENOPROTOOPT ENOSPC ENOSR ENOSTR ENOSYS ENOTACTIVE ENOTBLK ENOTCAPABLE ENOTCONN ENOTDIR ENOTEMPTY ENOTNAM ENOTRECOVERABLE ENOTSOCK ENOTSUP ENOTTY ENOTUNIQ ENXIO EOPNOTSUPP EOVERFLOW EOWNERDEAD EPERM EPFNOSUPPORT EPIPE EPROCLIM EPROCUNAVAIL EPROGMISMATCH EPROGUNAVAIL EPROTO EPROTONOSUPPORT EPROTOTYPE ERANGE EREMCHG EREMOTE EREMOTEIO ERESTART ERFKILL EROFS ERPCMISMATCH ESHUTDOWN ESOCKTNOSUPPORT ESPIPE ESRCH ESRMNT ESTALE ESTRPIPE ETIME ETIMEDOUT ETOOMANYREFS ETXTBSY EUCLEAN EUNATCH EUSERS EWOULDBLOCK EXDEV EXFULL ) "SB-POSIX"))
    (define signal-alist
      (%alist '(SIGABRT SIGALRM SIGBUS SIGCHLD SIGCONT SIGEMT SIGFPE SIGHUP SIGILL SIGINFO SIGINT SIGIO SIGKILL SIGPIPE SIGPROF SIGQUIT SIGSEGV SIGSTOP SIGSYS SIGTERM SIGTRAP SIGTSTP SIGTTIN SIGTTOU SIGURG SIGUSR1 SIGUSR2 SIGVTALRM SIGWINCH SIGXCPU SIGXFSZ ) "SB-UNIX"))

    (define signal-descriptions
      '((SIGABRT . "Abort trap") (SIGALRM . "Alarm clock") (SIGBUS . "Bus error")
        (SIGCHLD . "Child exited") (SIGCONT . "Continued") (SIGFPE . "Floating point exception")
        (SIGHUP . "Hangup") (SIGILL . "Illegal instruction") (SIGINT . "Interrupt")
        (SIGKILL . "Killed") (SIGPIPE . "Broken pipe") (SIGPROF . "Profiling timer expired")
        (SIGQUIT . "Quit") (SIGSEGV . "Segmentation fault") (SIGSTOP . "Stopped (signal)")
        (SIGSYS . "Bad system call") (SIGTERM . "Terminated") (SIGTRAP . "Trace/BPT trap")
        (SIGTSTP . "Stopped") (SIGTTIN . "Stopped (tty input)") (SIGTTOU . "Stopped (tty output)")
        (SIGURG . "Urgent I/O condition") (SIGUSR1 . "User defined signal 1")
        (SIGUSR2 . "User defined signal 2") (SIGVTALRM . "Virtual timer expired")
        (SIGWINCH . "Window size changes") (SIGXCPU . "Cputime limit exceeded")
        (SIGXFSZ . "Filesize limit exceeded") (SIGIO . "I/O possible")
        (SIGPWR . "Power failure") (SIGINFO . "Information request")
        (SIGEMT . "EMT trap") (SIGPOLL . "Pollable event")))

    (define (errno-message n) (%strerror n))
    (define (signal-message n)
      (let* ((entry (assv n signal-alist))
             (description (and entry (assq (cdr entry) signal-descriptions))))
        (and description (cdr description))))

    (define codesets
      (list (list 'errno errno-alist errno-message)
            (list 'signal signal-alist signal-message)))

    (define (codeset-entry codeset)
      (unless (symbol? codeset) (error "not a codeset" codeset))
      (assq codeset codesets))
    (define (codeset-alist codeset)
      (let ((entry (codeset-entry codeset))) (if entry (cadr entry) '())))

    (define (codeset? object)
      (and (symbol? object) (codeset-entry object) #t))

    (define (codeset-symbols codeset)
      (map cdr (codeset-alist codeset)))

    (define (codeset-symbol codeset code)
      (cond ((symbol? code) (codeset-entry codeset) code)
            ((exact-integer? code)
             (let ((entry (assv code (codeset-alist codeset)))) (and entry (cdr entry))))
            (else (error "codeset-symbol: bad code" code))))

    (define (codeset-number codeset code)
      (cond ((exact-integer? code) (codeset-entry codeset) code)
            ((symbol? code)
             (let loop ((alist (codeset-alist codeset)))
               (cond ((null? alist) #f)
                     ((eq? (cdar alist) code) (caar alist))
                     (else (loop (cdr alist))))))
            (else (error "codeset-number: bad code" code))))

    (define (codeset-message codeset code)
      (let ((number (codeset-number codeset code))
            (entry (codeset-entry codeset)))
        (and number entry ((car (cddr entry)) number))))))
