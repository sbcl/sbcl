(in-package "SB!THREAD")

(defvar *session-lock*)

(sb!xc:defmacro with-recursive-lock ((mutex) &body body)
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
	;; we're safe to do that.  Should this ever change, than
	;; MAKE-LISP-OBJ could return something that looks like a
	;; pointer, but pointing into neverneverland, which will
	;; confuse GC compiletely.  -- CSR, 2003-06-03
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

#!+sb-thread
(defun get-foreground ()
  (when (not (eql (mutex-value *session-lock*) (current-thread-id)))
    (get-mutex *session-lock*))
  (sb!sys:enable-interrupt :sigint #'sb!unix::sigint-handler)
  t)
#!-sb-thread
(defun get-foreground () t)

#!+sb-thread
(defun release-foreground ()
  (sb!sys:enable-interrupt :sigint :ignore)
  (release-mutex *session-lock*)
  t)
#!-sb-thread
(defun release-foreground () t)
