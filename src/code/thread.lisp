(in-package :sb!thread)

#+sb-xc-host
(defun make-mutex (&key name value) nil)

#+sb-xc-host
(defmacro with-recursive-lock ((mutex) &body body)
  `(progn ,@body))

#-sb-xc-host
(defmacro with-recursive-lock ((mutex) &body body)
  (let ((cfp (gensym "CFP")))
    `(let ((,cfp (ash (sb!sys:sap-int (sb!vm::current-fp) ) -2)))
      (unless (and (mutex-value ,mutex)
		   (SB!DI::control-stack-pointer-valid-p
		    (sb!sys:int-sap (ash (mutex-value ,mutex) 2))))
	(get-mutex ,mutex ,cfp))
      (unwind-protect
	   (progn ,@body)
	(when (eql (mutex-value ,mutex) ,cfp) (free-mutex ,mutex))))))

