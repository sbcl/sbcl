(in-package :sb-posix-internal)

(define-designator filename c-string
  (pathname  (namestring (translate-logical-pathname filename)))
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
