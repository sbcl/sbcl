(in-package "SB!THREAD")

(sb!xc:defmacro with-recursive-lock ((mutex) &body body)
  (declare (ignore #!-sb-thread mutex))
  #!+sb-thread
  (with-unique-names (cfp)
    `(let ((,cfp (sb!kernel:current-fp)))
      (unless (and (mutex-value ,mutex)
		   (sb!vm:control-stack-pointer-valid-p
		    (sb!sys:int-sap
		     (sb!kernel:get-lisp-obj-address (mutex-value ,mutex)))))
	;; this punning with MAKE-LISP-OBJ depends for its safety on
	;; the frame pointer being a lispobj-aligned integer.  While
	;; it is, then MAKE-LISP-OBJ will always return a FIXNUM, so
	;; we're safe to do that.  Should this ever change, this
	;; MAKE-LISP-OBJ could return something that looks like a
	;; pointer, but pointing into neverneverland, which will
	;; confuse GC completely.  -- CSR, 2003-06-03
	(get-mutex ,mutex (sb!kernel:make-lisp-obj (sb!sys:sap-int ,cfp))))
      (unwind-protect
	   (progn ,@body)
	(when (sb!sys:sap= (sb!sys:int-sap
			    (sb!kernel:get-lisp-obj-address
			     (mutex-value ,mutex)))
			   ,cfp)
	  (release-mutex ,mutex)))))
  #!-sb-thread
  `(progn ,@body))

