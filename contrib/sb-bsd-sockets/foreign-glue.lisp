(in-package :sb-bsd-sockets-internal)

;;;; Foreign function glue.  This is the only file in the distribution
;;;; that's _intended_ to be vendor-specific.  The macros defined here
;;;; are called from constants.lisp, which was generated from constants.ccon
;;;; by the C compiler as driven by that wacky def-to-lisp thing.

;;;; of course, the whole thing is vendor-specific actually, due to
;;;; the way we use cmucl alien types in constants.ccon as a cheap way
;;;; of transforming C-world alues into Lisp-world values.  But if
;;;; anyone were to port that bit to their preferred implementation, they
;;;; wouldn't need to port all the rest of the cmucl alien interface at
;;;; the same time

;;; (def-foreign-routine ("stat" STAT ) (INTEGER 32) (FILE-NAME
;;; C-CALL:C-STRING) (BUF (* T)) )

;;; I can't help thinking this was originally going to do something a
;;; lot more complex
(defmacro def-foreign-routine
  (&whole it (c-name lisp-name) return-type &rest args)
  (declare (ignorable c-name lisp-name return-type args))
  `(def-alien-routine ,@(cdr it)))
#|
(define-c-accessor FOO-PORT SOCKADDR-IN (ARRAY (UNSIGNED 8) 2) 2 2)
(define-c-accessor SOCKADDR-IN-FAMILY SOCKADDR-IN INTEGER 6 2)
|#
;;; define-c-accessor makes us a setter and a getter for changing
;;; memory at the appropriate offset

;;;    (define-c-accessor STAT-ATIME STAT (INTEGER 32) 56 4)

(defmacro define-c-accessor (el structure type offset length)
  (declare (ignore structure))
  (let* ((ty (cond
	       ((eql type 'integer) `(,type ,(* 8 length)))
	       ((eql (car type) '*) `(unsigned ,(* 8 length)))
	       ((eql type 'c-string) `(unsigned ,(* 8 length)))
	       ((eql (car type) 'array) (cadr type))))
	 (sap-ref-? (intern (format nil "~ASAP-REF-~A"
				    (if (member (car ty) '(INTEGER SIGNED))
					"SIGNED-" "")
				    (cadr ty))
			    (find-package "SB-SYS"))))
    (labels ((template (before after)
	       `(let* ((addr (+ 8 (logandc1 7 (sb-kernel:get-lisp-obj-address ptr))))
		       (sap (sb-sys:int-sap (the (unsigned-byte 32) (+ addr ,offset)))))
		 (,before (,sap-ref-? sap index) ,after))))
      `(progn
	;;(declaim (inline ,el (setf ,el)))
	(defun ,el (ptr &optional (index 0))
	  ,(template 'prog1 nil))
	(defconstant ,(intern (format nil "OFFSET-OF-~A" el)) ,offset)
	(defun (setf ,el) (newval ptr &optional (index 0))
	  ,(template 'setf 'newval))))))


;;; make memory allocator for appropriately-sized block of memory, and
;;; a constant to tell us how big it was anyway
(defmacro define-c-struct (name size)
  (labels ((p (x) (intern (concatenate 'string x (symbol-name name)))))
    `(progn
      (defun ,(p "ALLOCATE-") () (make-array ,size :initial-element 0
					     :element-type '(unsigned-byte 8)))
      (defconstant ,(p "SIZE-OF-") ,size)
      (defun ,(p "FREE-" ) (p) (declare (ignore p))))))

(defun foreign-nullp (c)
  "C is a pointer to 0?"
  (= 0 (sb-sys:sap-int (sb-alien:alien-sap  c))))

;;; this could be a lot faster if I cared enough to think about it
(defun foreign-vector (pointer size length)
  "Compose a vector of the words found in foreign memory starting at
POINTER.  Each word is SIZE bytes long; LENGTH gives the number of
elements of the returned vector.  See also FOREIGN-VECTOR-UNTIL-ZERO"
  (assert (= size 1))
  (let ((ptr
	 (typecase pointer
	   (sb-sys:system-area-pointer
	    (sap-alien pointer (* (sb-alien:unsigned 8))))
	   (t
	    (sb-alien:cast pointer (* (sb-alien:unsigned 8))))))
	(result (make-array length :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (1- length) by size
	  do (setf (aref result i) (sb-alien:deref ptr i)))
     ;;(format t "~S~%" result)
    result))
