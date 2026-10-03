;;; Cryptography from Scheme with Ironclad, a Common Lisp library.
;;;
;;;   bin/pseudoscheme --quicklisp examples/scheme-uses-ironclad.scm
;;;
;;; (cl ironclad) loads the system on first import (through Quicklisp
;;; here, which fetches it if need be).  Ironclad's byte arrays are
;;; (unsigned-byte 8) vectors, which is exactly what Scheme bytevectors
;;; are, so string->utf8, bytevector-u8-ref and friends work on its
;;; inputs and outputs directly.  Its keyword arguments are written
;;; #:mode, #:key; its algorithm names are keywords too (#:sha256).

(import (scheme base) (scheme write)
        (prefix (cl ironclad) ic:)
        (prefix (cl common-lisp) cl:)
        (pseudoscheme lisp))

(define (show label value)
  (display label) (display ": ") (write value) (newline))

(define (hex bytes) (ic:byte-array-to-hex-string bytes))

;;; Message digests

(define message (string->utf8 "The quick brown fox jumps over the lazy dog"))

(show "sha256" (hex (ic:digest-sequence #:sha256 message)))
(show "sha512" (substring (hex (ic:digest-sequence #:sha512 message)) 0 32))
(show "blake2" (hex (ic:digest-sequence #:blake2 message)))

;; Digests are incremental objects too: feed the message in pieces.
(define digest (ic:make-digest #:sha256))
(ic:update-digest digest (string->utf8 "The quick brown fox "))
(ic:update-digest digest (string->utf8 "jumps over the lazy dog"))
(show "sha256, incrementally" (string=? (hex (ic:produce-digest digest))
                                        (hex (ic:digest-sequence #:sha256 message))))

;;; Message authentication

(define (hmac-sha256 key data)
  (let ((mac (ic:make-hmac key #:sha256)))
    (ic:update-hmac mac data)
    (ic:hmac-digest mac)))

(show "hmac-sha256" (hex (hmac-sha256 (string->utf8 "key") message)))

;;; Key derivation: PBKDF2

(define salt (string->utf8 "NaCl"))
(define kdf (ic:make-kdf #:pbkdf2 #:digest #:sha256))
(show "pbkdf2" (hex (ic:derive-key kdf (string->utf8 "password") salt 1000 32)))

;;; Symmetric encryption: AES-256 in CTR mode

(define key (ic:random-data 32))
(define iv (ic:random-data 16))

(define (aes-ctr data)                  ; CTR mode encrypts and decrypts alike
  (let ((cipher (ic:make-cipher #:aes #:key key #:mode #:ctr #:initialization-vector iv))
        (out (bytevector-copy data)))
    (ic:encrypt-in-place cipher out)
    out))

(define plaintext (string->utf8 "attack at dawn"))
(define ciphertext (aes-ctr plaintext))
(show "ciphertext differs" (not (equal? ciphertext plaintext)))
(show "round trip" (utf8->string (aes-ctr ciphertext)))

;;; Public-key signatures: Ed25519

(call-with-values (lambda () (ic:generate-key-pair #:ed25519))
  (lambda (private public)
    ;; verify-signature answers yes or no, but its name doesn't say so
    ;; the Lisp way (verify-signature-p), so its NIL comes back as ();
    ;; lisp-true? reads it as Lisp would.
    (let ((signature (ic:sign-message private message)))
      (show "signature bytes" (bytevector-length signature))
      (show "verifies" (lisp-true? (ic:verify-signature public message signature)))
      (show "tampered message verifies"
            (lisp-true? (ic:verify-signature public (string->utf8 "the slow brown fox")
                                             signature))))))

;;; Lisp macros work too: a hex dump with LOOP and WITH-OUTPUT-TO-STRING.

(define (hex-dump bytes)
  (cl:with-output-to-string (out)
    (cl:loop for b across bytes
             for i from 0
             do (cl:format out "~2,'0x~:[ ~;~%~]" b (= (remainder (+ i 1) 8) 0)))))

(display (hex-dump (ic:digest-sequence #:md5 message)))
