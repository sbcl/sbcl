(in-package :sb!thread)

#+sb-xc-host
(defun make-mutex (&key name value) nil)

#+sb-xc-host
(defmacro with-recursive-lock ((mutex) &body body)
  `(progn ,@body))

#-sb-xc-host
(defmacro with-recursive-lock ((mutex) &body body)
  (let ((cfp (gensym "CFP")))
    `(let ((,cfp (sb!sys:sap-int (sb!di::descriptor-sap (sb!vm::current-fp)))))
      (unless (and (mutex-value ,mutex)
		   (SB!DI::control-stack-pointer-valid-p (mutex-value ,mutex)))
	(get-mutex ,mutex ,cfp))
      (unwind-protect
	   (progn ,@body)
	(when (eql (mutex-value ,mutex) ,cfp) (free-mutex ,mutex))))))

