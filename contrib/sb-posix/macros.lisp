(in-package :sb-posix-internal)

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
				  (get (cadr x) 'designator-type (cadr x)))
				arguments)))
		  ,@(mapcar (lambda (x)
			      (if (get (cadr x) 'designator-type)
				  `(,(intern (symbol-name (cadr x)) :sb-posix)
				    ,(car x))
				  (car x)))
			    arguments))))
	  (if (,error-predicate r) (syscall-error) r))))))
