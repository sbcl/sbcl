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
	     (string (write-string piece s) (write-char #\/ s))
	     ((member :up) (write-string "../" s))))))
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
  (file-stream (sb-sys:fd-stream-fd file-descriptor))
  (fixnum file-descriptor))

(define-designator sap-or-nil sb-sys:system-area-pointer
  (null (sb-sys:int-sap 0))
  (sb-sys:system-area-pointer sap-or-nil))

(define-designator alien-pointer-to-anything-or-nil (* t)
  (null (sb-alien:sap-alien (sb-sys:int-sap 0) (* t)))
  ((alien (* t)) alien-pointer-to-anything-or-nil))

(defun lisp-for-c-symbol (s)
  (intern (substitute #\- #\_ (string-upcase s)) :sb-posix))

(defmacro define-call-internally (lisp-name c-name return-type error-predicate &rest arguments)
  (if (sb-fasl::foreign-symbol-address-as-integer-or-nil
       (sb-vm:extern-alien-name c-name))
      `(progn
	(declaim (inline ,lisp-name))
	(defun ,lisp-name ,(mapcar #'car arguments)
	  (let ((r (alien-funcall
		    (extern-alien
		     ,c-name
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
      `(sb-int:style-warn "Didn't find definition for ~S" ,c-name)))

(defmacro define-call (name return-type error-predicate &rest arguments)
  (let ((lisp-name (lisp-for-c-symbol name)))
    `(progn
       (export ',lisp-name :sb-posix)
       (define-call-internally ,lisp-name ,name ,return-type ,error-predicate ,@arguments))))

(defmacro define-entry-point (name arglist &body body)
  (let ((lisp-name (lisp-for-c-symbol name)))
    `(progn
      (export ',lisp-name :sb-posix)
      (declaim (inline ,lisp-name))
      (defun ,lisp-name ,arglist
	,@body))))
