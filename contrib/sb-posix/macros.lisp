(in-package :sb-posix-internal)

;;; some explanation may be necessary.  The namestring "[foo]" 
;;; denotes a wild pathname.  When there's a file on the disk whose
;;; Unix name is "[foo]", the appropriate CL namestring for it is
;;; "\\[foo]".  So, don't call NAMESTRING, instead call a function
;;; that gets us the Unix name
(defun native-filename (pathname)
  (let ((directory (pathname-directory pathname))
	(name (pathname-name pathname))
	(type (pathname-type pathname)))
    (with-output-to-string (s nil :element-type 'base-char)
      (etypecase directory
	(string (write-string directory s))
	(list
	 (when (eq (car directory) :absolute)
	   (write-char #\/ s))
	 (dolist (piece (cdr directory))
	   (etypecase piece
	     (string (write-string piece s) (write-char #\/ s))))))
      (etypecase name
	(null)
        (string (write-string name s)))
      (etypecase type
	(null)
	(string (write-char #\. s) (write-string type s))))))

(define-designator filename c-string
  (pathname 
   (native-filename (translate-logical-pathname filename)))
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
    (if (sb-fasl::foreign-symbol-address-as-integer-or-nil name)
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
	      (if (,error-predicate r) (syscall-error) r))))
	`(progn
	  (export ',lisp-name :sb-posix)
	  (sb-int:style-warn "Didn't find definition for ~S" ,name)))))
