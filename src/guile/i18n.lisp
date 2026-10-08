; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; The C half of (ice-9 i18n), on the C library's locales (newlocale,
;;;; nl_langinfo_l, localeconv_l, wcscoll_l, strtod_l) through CFFI.  A
;;;; locale object holds a locale_t; %global-locale stands for the
;;;; process's locale (setlocale).  Case mapping and case folding are
;;;; SBCL's Unicode ones, with the locale's language for Turkish,
;;;; Azerbaijani and Lithuanian, as libunistring's are in Guile.

(in-package "PSEUDOSCHEME-GUILE")

(defstruct (glocale (:constructor %make-glocale (pointer name)) (:copier nil))
  pointer				; a locale_t address, or NIL for the global locale
  name)

(defmethod print-object ((l glocale) stream)
  (format stream "#<locale ~A>" (or (glocale-name l) "global")))

(defvar *global-locale* (%make-glocale nil nil))

;;; Categories: (category mask) for each of the platform's LC_ constants
(defparameter *locale-category-masks*
  #+darwin '((0 . 63) (1 . 1) (2 . 2) (3 . 8) (4 . 16) (5 . 32) (6 . 4))
  #-darwin '((6 . 8127) (0 . 1) (1 . 2) (2 . 4) (3 . 8) (4 . 16) (5 . 32)))

(defun category-mask (categories)
  (let ((list (if (listp categories) categories (list categories))))
    (reduce #'logior list
	    :key (lambda (c) (or (cdr (assoc c *locale-category-masks*))
				 (wrong-type "make-locale" 1 categories)))
	    :initial-value 0)))

(defun make-glocale (categories name &optional base)
  (unless (stringp name) (wrong-type "make-locale" 2 name))
  (let* ((base-pointer (if (and base (glocale-p base) (glocale-pointer base))
			   ;; newlocale may consume the base: give it a copy
			   (cffi:pointer-address
			    (cffi:foreign-funcall "duplocale" :pointer (sb-sys:int-sap (glocale-pointer base)) :pointer))
			   0))
	 (p (cffi:foreign-funcall "newlocale" :int (category-mask categories) :string name
					      :pointer (sb-sys:int-sap base-pointer) :pointer)))
    (when (cffi:null-pointer-p p)
      (guile-error (ssym "system-error") "make-locale" "Failed to install locale" '() (list 2)))
    (let* ((address (cffi:pointer-address p))
	   (l (%make-glocale address name)))
      (trivial-garbage:finalize l (lambda () (cffi:foreign-funcall "freelocale" :pointer (sb-sys:int-sap address) :void)))
      l)))

(defun locale-arg (locale)
  "The locale_t pointer of LOCALE (an optional argument), or NIL for the global one."
  (cond ((or (null locale) (eq locale ps:false)) nil)
	((glocale-p locale) (glocale-pointer locale))
	(t (wrong-type "locale" 2 locale))))

(defun locale-language (locale)
  "The language keyword SBCL's case mapping takes for LOCALE, or NIL."
  (let ((name (and (glocale-p locale) (glocale-name locale))))
    (when (and name (>= (length name) 2))
      (let ((lang (string-downcase (subseq name 0 2))))
	(cond ((string= lang "tr") :tr) ((string= lang "az") :az) ((string= lang "lt") :lt))))))

;;; Strings for C

(defun call-with-wide-string (string function)
  "Call FUNCTION with STRING as a NUL-terminated wchar_t (UTF-32) array."
  (let ((n (length string)))
    (cffi:with-foreign-object (w :uint32 (1+ n))
      (dotimes (i n) (setf (cffi:mem-aref w :uint32 i) (char-code (char string i))))
      (setf (cffi:mem-aref w :uint32 n) 0)
      (funcall function w))))

(defun collate (a b locale)
  "<0, 0 or >0 as A collates before, with or after B in LOCALE."
  (let ((p (locale-arg locale)))
    (call-with-wide-string
     a (lambda (wa)
	 (call-with-wide-string
	  b (lambda (wb)
	      (if p
		  (cffi:foreign-funcall "wcscoll_l" :pointer wa :pointer wb :pointer (sb-sys:int-sap p) :int)
		  (cffi:foreign-funcall "wcscoll" :pointer wa :pointer wb :int))))))))

(defun fold (string) (sb-unicode:casefold string))

(defun string-collate-fn (who test &optional ci)
  (lambda (a b &optional locale)
    (unless (stringp a) (wrong-type who 1 a))
    (unless (stringp b) (wrong-type who 2 b))
    (bool (funcall test (if ci (collate (fold a) (fold b) locale) (collate a b locale))))))

(defun char-collate-fn (who test &optional ci)
  (lambda (a b &optional locale)
    (unless (characterp a) (wrong-type who 1 a))
    (unless (characterp b) (wrong-type who 2 b))
    (let ((a (string a)) (b (string b)))
      (bool (funcall test (if ci (collate (fold a) (fold b) locale) (collate a b locale)))))))

(defun map-case (function string locale)
  (let ((language (locale-language locale)))
    (if language (funcall function string :locale language) (funcall function string))))

(defun char-case (function c locale)
  (let ((s (map-case function (string c) locale)))
    (if (= (length s) 1) (char s 0) c)))

;;; nl-langinfo

(defparameter *langinfo-items*
  #+darwin
  '(("CODESET" . 0) ("D_T_FMT" . 1) ("D_FMT" . 2) ("T_FMT" . 3) ("T_FMT_AMPM" . 4) ("AM_STR" . 5)
    ("PM_STR" . 6) ("DAY_1" . 7) ("DAY_2" . 8) ("DAY_3" . 9) ("DAY_4" . 10) ("DAY_5" . 11)
    ("DAY_6" . 12) ("DAY_7" . 13) ("ABDAY_1" . 14) ("ABDAY_2" . 15) ("ABDAY_3" . 16) ("ABDAY_4" . 17)
    ("ABDAY_5" . 18) ("ABDAY_6" . 19) ("ABDAY_7" . 20) ("MON_1" . 21) ("MON_2" . 22) ("MON_3" . 23)
    ("MON_4" . 24) ("MON_5" . 25) ("MON_6" . 26) ("MON_7" . 27) ("MON_8" . 28) ("MON_9" . 29)
    ("MON_10" . 30) ("MON_11" . 31) ("MON_12" . 32) ("ABMON_1" . 33) ("ABMON_2" . 34) ("ABMON_3" . 35)
    ("ABMON_4" . 36) ("ABMON_5" . 37) ("ABMON_6" . 38) ("ABMON_7" . 39) ("ABMON_8" . 40)
    ("ABMON_9" . 41) ("ABMON_10" . 42) ("ABMON_11" . 43) ("ABMON_12" . 44) ("ERA" . 45)
    ("ERA_D_FMT" . 46) ("ERA_D_T_FMT" . 47) ("ERA_T_FMT" . 48) ("ALT_DIGITS" . 49) ("RADIXCHAR" . 50)
    ("THOUSEP" . 51) ("YESEXPR" . 52) ("NOEXPR" . 53) ("YESSTR" . 54) ("NOSTR" . 55) ("CRNCYSTR" . 56))
  ;; glibc's: (category << 16) | index
  #-darwin
  (append
   '(("CODESET" . 14) ("RADIXCHAR" . #x10000) ("THOUSEP" . #x10001)
     ("YESEXPR" . #x50000) ("NOEXPR" . #x50001) ("YESSTR" . #x50002) ("NOSTR" . #x50003)
     ("CRNCYSTR" . #x4000f) ("AM_STR" . #x20026) ("PM_STR" . #x20027) ("D_T_FMT" . #x20028)
     ("D_FMT" . #x20029) ("T_FMT" . #x2002a) ("T_FMT_AMPM" . #x2002b) ("ERA" . #x2002c)
     ("ERA_D_FMT" . #x2002e) ("ALT_DIGITS" . #x2002f) ("ERA_D_T_FMT" . #x20030) ("ERA_T_FMT" . #x20031))
   (loop for i from 1 to 7 collect (cons (format nil "ABDAY_~D" i) (+ #x20000 i -1)))
   (loop for i from 1 to 7 collect (cons (format nil "DAY_~D" i) (+ #x20007 i -1)))
   (loop for i from 1 to 12 collect (cons (format nil "ABMON_~D" i) (+ #x2000e i -1)))
   (loop for i from 1 to 12 collect (cons (format nil "MON_~D" i) (+ #x2001a i -1)))))

;;; Items that are localeconv's fields: numbered here, (name number kind
;;; offset); the string fields first, then the char ones
(defparameter *lconv-items*
  (append
   (loop for name in '("DECIMAL_POINT" "THOUSANDS_SEP" "GROUPING" "INT_CURR_SYMBOL" "CURRENCY_SYMBOL"
		       "MON_DECIMAL_POINT" "MON_THOUSANDS_SEP" "MON_GROUPING" "POSITIVE_SIGN" "NEGATIVE_SIGN")
	 for i from 0
	 collect (list name (+ 1000 i)
		       (if (member name '("GROUPING" "MON_GROUPING") :test #'string=) :grouping :string)
		       (* 8 i)))
   (loop for name in '("INT_FRAC_DIGITS" "FRAC_DIGITS" "P_CS_PRECEDES" "P_SEP_BY_SPACE" "N_CS_PRECEDES"
		       "N_SEP_BY_SPACE" "P_SIGN_POSN" "N_SIGN_POSN" "INT_P_CS_PRECEDES" "INT_N_CS_PRECEDES"
		       "INT_P_SEP_BY_SPACE" "INT_N_SEP_BY_SPACE" "INT_P_SIGN_POSN" "INT_N_SIGN_POSN")
	 for i from 0
	 collect (list name (+ 1100 i)
		       (cond ((search "DIGITS" name) :digits) ((search "POSN" name) :posn) (t :boolean))
		       (+ 80 i)))))

(defun locale-codeset (p)
  (let ((r (if p
	       (cffi:foreign-funcall "nl_langinfo_l" :int (cdr (assoc "CODESET" *langinfo-items* :test #'string=))
						     :pointer (sb-sys:int-sap p) :pointer)
	       (cffi:foreign-funcall "nl_langinfo" :int (cdr (assoc "CODESET" *langinfo-items* :test #'string=)) :pointer))))
    (if (cffi:null-pointer-p r) "UTF-8" (cffi:foreign-string-to-lisp r :encoding :latin-1))))

(defun c-string-in (pointer codeset)
  (cffi:foreign-string-to-lisp pointer :encoding (if (search "UTF-8" (string-upcase codeset)) :utf-8 :latin-1)))

(defun nl-langinfo (item &optional locale)
  (let* ((p (locale-arg locale))
	 (lconv (find item *lconv-items* :key #'second)))
    (if lconv
	(destructuring-bind (name number kind offset) lconv
	  (declare (ignore name number))
	  (let ((conv (if p
			  (cffi:foreign-funcall "localeconv_l" :pointer (sb-sys:int-sap p) :pointer)
			  (cffi:foreign-funcall "localeconv" :pointer))))
	    (ecase kind
	      (:string (c-string-in (cffi:mem-ref conv :pointer offset) (locale-codeset p)))
	      (:grouping (let ((g (cffi:mem-ref conv :pointer offset)))
			   (loop for i from 0
				 for b = (cffi:mem-aref g :int8 i)
				 while (and (> b 0) (/= b 127)) collect b)))
	      (:digits (let ((b (cffi:mem-ref conv :int8 offset))) (if (= b 127) ps:false b)))
	      (:boolean (bool (/= 0 (cffi:mem-ref conv :int8 offset))))
	      (:posn (ssym (case (cffi:mem-ref conv :int8 offset)
			     (0 "parenthesize") (1 "sign-before") (2 "sign-after")
			     (3 "sign-before-currency-symbol") (4 "sign-after-currency-symbol")
			     (t "unspecified")))))))
	(let ((r (if p
		     (cffi:foreign-funcall "nl_langinfo_l" :int item :pointer (sb-sys:int-sap p) :pointer)
		     (cffi:foreign-funcall "nl_langinfo" :int item :pointer))))
	  (if (cffi:null-pointer-p r) ps:false (c-string-in r (locale-codeset p)))))))

;;; Numbers

(defun locale-string->number (s locale function)
  "FUNCTION (strtol_l or strtod_l, as a lambda of string, end pointer and
locale) on S: the number and the characters it took, or #f and 0."
  (let ((p (locale-arg locale))
	(octets (sb-ext:string-to-octets s :external-format :utf-8 :null-terminate t)))
    (cffi:with-foreign-objects ((buf :uint8 (length octets)) (end :pointer))
      (dotimes (i (length octets)) (setf (cffi:mem-aref buf :uint8 i) (aref octets i)))
      (let* ((value (funcall function buf end (if p (sb-sys:int-sap p) (cffi:null-pointer))))
	     (used (- (cffi:pointer-address (cffi:mem-ref end :pointer)) (cffi:pointer-address buf))))
	(if (zerop used)
	    (values ps:false 0)
	    (values value (length (sb-ext:octets-to-string octets :end used :external-format :utf-8))))))))

(defextension "scm_init_i18n"
  (append
   (list
    (cons "make-locale" #'make-glocale)
    (cons "locale?" (lambda (x) (bool (glocale-p x))))
    (cons "%global-locale" *global-locale*)
    (cons "string-locale<?" (string-collate-fn "string-locale<?" #'minusp))
    (cons "string-locale>?" (string-collate-fn "string-locale>?" #'plusp))
    (cons "string-locale-ci<?" (string-collate-fn "string-locale-ci<?" #'minusp t))
    (cons "string-locale-ci>?" (string-collate-fn "string-locale-ci>?" #'plusp t))
    (cons "string-locale-ci=?" (string-collate-fn "string-locale-ci=?" #'zerop t))
    (cons "char-locale<?" (char-collate-fn "char-locale<?" #'minusp))
    (cons "char-locale>?" (char-collate-fn "char-locale>?" #'plusp))
    (cons "char-locale-ci<?" (char-collate-fn "char-locale-ci<?" #'minusp t))
    (cons "char-locale-ci>?" (char-collate-fn "char-locale-ci>?" #'plusp t))
    (cons "char-locale-ci=?" (char-collate-fn "char-locale-ci=?" #'zerop t))
    (cons "char-locale-downcase" (lambda (c &optional l) (char-case #'sb-unicode:lowercase c l)))
    (cons "char-locale-upcase" (lambda (c &optional l) (char-case #'sb-unicode:uppercase c l)))
    (cons "char-locale-titlecase" (lambda (c &optional l) (char-case #'sb-unicode:titlecase c l)))
    (cons "string-locale-downcase" (lambda (s &optional l) (map-case #'sb-unicode:lowercase s l)))
    (cons "string-locale-upcase" (lambda (s &optional l) (map-case #'sb-unicode:uppercase s l)))
    (cons "string-locale-titlecase" (lambda (s &optional l) (map-case #'sb-unicode:titlecase s l)))
    (cons "locale-string->integer"
	  (lambda (s &optional (base 10) locale)
	    (locale-string->number s locale
				   (lambda (buf end loc)
				     (cffi:foreign-funcall "strtol_l" :pointer buf :pointer end :int base
								      :pointer loc :long)))))
    (cons "locale-string->inexact"
	  (lambda (s &optional locale)
	    (locale-string->number s locale
				   (lambda (buf end loc)
				     (cffi:foreign-funcall "strtod_l" :pointer buf :pointer end
								      :pointer loc :double)))))
    (cons "nl-langinfo" #'nl-langinfo))
   (loop for (name . value) in *langinfo-items* collect (cons name value))
   (loop for (name value) in *lconv-items* collect (cons name value))))
