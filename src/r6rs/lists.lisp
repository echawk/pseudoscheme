; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs lists (6)), (rnrs control (6)) procedures, and the
;;;; base-library procedures whose R6RS signatures differ from R5RS/R7RS.
;;;; Library report chapters 3 and 5; language report 11.

(in-package "PSEUDOSCHEME-R6RS")

(defun truthy (x) (ps:truep x))

(defun check-proper-list (who x)
  (unless (and (listp x) (handler-case (list-length x) (error () nil)))
    (ps:scheme-error "~A: not a proper list: ~S" who x)))

(defun check-procedure (who x)
  (unless (functionp x)
    (ps:scheme-error "~A: not a procedure: ~S" who x)))

(defun check-same-lengths (who lists)
  (mapc (lambda (l) (check-proper-list who l)) lists)
  (let ((n (length (car lists))))
    (unless (every (lambda (l) (= (length l) n)) lists)
      (ps:scheme-error "~A: lists differ in length" who))))

;;; 3.  List utilities

(defprim "find" (proc list)
  (check-procedure "find" proc)
  (dolist (x list ps:false)
    (when (truthy (funcall proc x)) (return x))))

(defprim "for-all" (proc list &rest lists)
  (let ((lists (cons list lists)))
    (check-same-lengths "for-all" lists)
    (if (null list)
	t
	(loop
	  (let ((r (apply proc (mapcar #'car lists))))
	    (unless (truthy r) (return ps:false))
	    (setq lists (mapcar #'cdr lists))
	    ;; The last call is in tail position: return its value.
	    (when (null (car lists)) (return r)))))))

(defprim "exists" (proc list &rest lists)
  (let ((lists (cons list lists)))
    (check-same-lengths "exists" lists)
    (loop
      (when (null (car lists)) (return ps:false))
      (let ((r (apply proc (mapcar #'car lists))))
	(when (truthy r) (return r))
	(setq lists (mapcar #'cdr lists))))))

(defprim "filter" (proc list)
  (remove-if-not (lambda (x) (truthy (funcall proc x))) list))

(defprim "partition" (proc list)
  (let ((in '()) (out '()))
    (dolist (x list)
      (if (truthy (funcall proc x)) (push x in) (push x out)))
    (values (nreverse in) (nreverse out))))

(defprim "fold-left" (combine nil-value list &rest lists)
  (let ((lists (cons list lists)) (acc nil-value))
    (check-same-lengths "fold-left" lists)
    (loop (when (null (car lists)) (return acc))
	  (setq acc (apply combine acc (mapcar #'car lists))
		lists (mapcar #'cdr lists)))))

(defprim "fold-right" (combine nil-value list &rest lists)
  (let ((lists (cons list lists)))
    (check-same-lengths "fold-right" lists)
    (let ((rows (apply #'mapcar #'list lists)) (acc nil-value))
      (dolist (row (reverse rows) acc)
	(setq acc (apply combine (append row (list acc))))))))

(defprim "remp" (proc list) (remove-if (lambda (x) (truthy (funcall proc x))) list))
(defprim "remove" (obj list) (remove-if (lambda (x) (ps:scheme-equal-p obj x)) list))
(defprim "remv" (obj list) (remove obj list :test #'eql))
(defprim "remq" (obj list) (remove obj list :test #'eq))

(defun mem-tail (pred list)
  (loop for tail on list
	when (funcall pred (car tail)) return tail
	finally (return ps:false)))

(defprim "memp" (proc list) (mem-tail (lambda (x) (truthy (funcall proc x))) list))
(defprim "member" (obj list &optional compare)
  ;; R7RS's optional third argument; R6RS has two.
  (mem-tail (if compare
		(lambda (x) (truthy (funcall compare obj x)))
		(lambda (x) (ps:scheme-equal-p obj x)))
	    list))
(defprim "memv" (obj list) (mem-tail (lambda (x) (eql obj x)) list))
(defprim "memq" (obj list) (mem-tail (lambda (x) (eq obj x)) list))

(defun ass (pred alist)
  (dolist (pair alist ps:false)
    (unless (consp pair) (ps:scheme-error "assoc: not an association list"))
    (when (funcall pred (car pair)) (return pair))))

(defprim "assp" (proc alist) (ass (lambda (x) (truthy (funcall proc x))) alist))
(defprim "assoc" (obj alist &optional compare)
  (ass (if compare
	   (lambda (x) (truthy (funcall compare obj x)))
	   (lambda (x) (ps:scheme-equal-p obj x)))
       alist))
(defprim "assv" (obj alist) (ass (lambda (x) (eql obj x)) alist))
(defprim "assq" (obj alist) (ass (lambda (x) (eq obj x)) alist))

(defprim "cons*" (obj &rest objs)
  (if objs (apply #'list* obj objs) obj))

;;; Base-library procedures (language report 11.9, 11.12, 11.13)

(defprim "list-tail" (list k) (nthcdr k list))

(defprim "map" (proc list &rest lists)
  ;; R6RS requires equal lengths; R7RS stops at the shortest.  Accept
  ;; R7RS behavior here (a superset) so shared code keeps working.
  (apply #'mapcar proc list lists))

(defprim "for-each" (proc list &rest lists)
  (apply #'mapc proc list lists)
  ps:unspecific)

;;; 7.  Sorting (rnrs sorting)

(defprim "list-sort" (proc list)
  (stable-sort (copy-list list) (lambda (a b) (truthy (funcall proc a b)))))

(defprim "vector-sort" (proc vector)
  (stable-sort (copy-seq vector) (lambda (a b) (truthy (funcall proc a b)))))

(defprim "vector-sort!" (proc vector)
  (let ((sorted (stable-sort (copy-seq vector) (lambda (a b) (truthy (funcall proc a b))))))
    (replace vector sorted)
    ps:unspecific))
