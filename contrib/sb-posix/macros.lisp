(in-package :sb-posix-internal)

;;; some explanation may be necessary.  The namestring "[foo]" 
;;; denotes a wild pathname.  When there's a file on the disk whose
;;; Unix name is "[foo]", the appropriate CL namestring for it is
;;; "\\[foo]".  So, don't call NAMESTRING, instead call a function
;;; that gets us the Unix name

(define-designator filename c-string
  (pathname 
   (sb-impl::unix-namestring (translate-logical-pathname filename) nil))
  (string filename))

(define-designator file-descriptor (integer 32)
  (sb-impl::file-stream (sb-impl::fd-stream-fd file-descriptor))
  (fixnum file-descriptor))

(define-designator sap-or-nil sb-sys:system-area-pointer
  (null (sb-sys:int-sap 0))
  (sb-sys:system-area-pointer sap-or-nil))

(defun lisp-for-c-symbol (s)
  (intern (substitute #\- #\_ (string-upcase s)) :sb-posix))

(defmacro define-call (name return-type error-predicate &rest arguments)
  (let ((lisp-name (lisp-for-c-symbol name)))
    `(progn
      (export ',lisp-name :sb-posix)
      (declaim (inline ,lisp-name))
      (defun ,lisp-name ,(mapcar #'car arguments)
	(let ((r (alien-funcall
		  (extern-alien
		   ,name
		   (function ,return-type
			     ,@(mapcar
				(lambda (x)
				  (gethash (cadr x) *designator-types* (cadr x)))
				arguments)))
		  ,@(mapcar (lambda (x)
			      (if (nth-value 1 (gethash (cadr x) *designator-types*))
				  `(,(intern (symbol-name (cadr x)) :sb-posix)
				    ,(car x))
				  (car x)))
			    arguments))))
	  (if (,error-predicate r) (syscall-error) r))))))
