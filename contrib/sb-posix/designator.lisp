(in-package :sb-posix-internal)
(defmacro define-designator (name result &body conversions)
  (let ((type `(quote (or ,@(mapcar #'car conversions))))
	(typename (intern (format nil "~A-~A"
				  (symbol-name name)
				  (symbol-name :designator))
			  #.*package*)))
    `(progn
      (eval-when (:compile-toplevel :load-toplevel :execute)
	(deftype ,typename () ,type)
	(setf (get ',name 'designator-type) ',result))
      (defun ,(intern (symbol-name name) :sb-posix) (,name)
	(declare (type ,typename ,name))
	(etypecase ,name
	  ,@conversions)))))

(define-designator filename c-string
  (pathname  (namestring (translate-logical-pathname filename)))
  (string filename))

(define-designator file-descriptor (integer 32)
  (sb-impl::file-stream (sb-impl::fd-stream-fd file-descriptor))
  (fixnum file-descriptor))

(define-designator sap-or-nil sb-sys:system-area-pointer
  (null (sb-sys:int-sap 0))
  (sb-sys:system-area-pointer sap-or-nil))
