(in-package "SB!THREAD")

(defvar *session-lock*)

(sb!xc:defmacro with-recursive-lock ((mutex) &body body)
  #!+sb-thread
  (with-unique-names (cfp)
    `(let ((,cfp (ash (sb!sys:sap-int (sb!vm::current-fp) ) -2)))
      (unless (and (mutex-value ,mutex)
		   (SB!DI::control-stack-pointer-valid-p
		    (sb!sys:int-sap (ash (mutex-value ,mutex) 2))))
	(get-mutex ,mutex ,cfp))
      (unwind-protect
	   (progn ,@body)
	(when (eql (mutex-value ,mutex) ,cfp) (release-mutex ,mutex)))))
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
