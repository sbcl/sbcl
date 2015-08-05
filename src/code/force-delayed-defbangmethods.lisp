;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL") ;(SB-IMPL, not SB!IMPL, since we're built in warm load.)

(macrolet ((force-delayed-def!methods ()
             `(progn
                ,@(mapcar (lambda (args)
                            `(progn
                               #+sb-show
                               (format t
                                       "~&/about to do ~S~%"
                                       '(defmethod ,@args))
                               (setf (slot-value (defmethod ,@(cdr args))
                                                 'sb-pcl::source)
                                     ,(car args))
                               #+sb-show
                               (format t
                                       "~&/done with DEFMETHOD ~S~%"
                                       ',(first args))))
                          *delayed-def!method-args*)
                ;; We're no longer needed, ordinary DEFMETHOD is enough now.
                (makunbound '*delayed-def!method-args*))))
  (force-delayed-def!methods))
