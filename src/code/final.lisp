;;;; finalization based on weak pointers

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defvar *objects-pending-finalization* nil)

(defun finalize (object function)
  #!+sb-doc
  "Arrange for FUNCTION to be called when there are no more references to
   OBJECT."
  (declare (type function function))
  (sb!sys:without-gcing
   (push (cons (make-weak-pointer object) function)
	 *objects-pending-finalization*))
  object)

(defun cancel-finalization (object)
  #!+sb-doc
  "Cancel any finalization registers for OBJECT."
  (when object
    ;; We check to make sure object isn't nil because if there are any
    ;; broken weak pointers, their value will show up as nil. Therefore,
    ;; they would be deleted from the list, but not finalized. Broken
    ;; weak pointers shouldn't be left in the list, but why take chances?
    (sb!sys:without-gcing
     (setf *objects-pending-finalization*
	   (delete object *objects-pending-finalization*
		   :key #'(lambda (pair)
			    (values (weak-pointer-value (car pair))))))))
  nil)

(defun finalize-corpses ()
  (setf *objects-pending-finalization*
	(delete-if #'(lambda (pair)
		       (multiple-value-bind (object valid)
			   (weak-pointer-value (car pair))
			 (declare (ignore object))
			 (unless valid
			   (funcall (cdr pair))
			   t)))
		   *objects-pending-finalization*))
  nil)

(pushnew 'finalize-corpses *after-gc-hooks*)
