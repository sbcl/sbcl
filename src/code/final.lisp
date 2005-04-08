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

(defvar *finalizer-store* nil)

(defvar *finalizer-store-lock* 
  (sb!thread:make-mutex :name "Finalizer store lock."))

(defun finalize (object function)
  #!+sb-doc 
  "Arrange for the designated FUNCTION to be called when there
are no more references to OBJECT. In a multithreaded environment
the finalizer may run in any thread."
  (sb!thread:with-mutex (*finalizer-store-lock*)
    (push (cons (make-weak-pointer object) function)
	  *finalizer-store*))
  object)

(defun cancel-finalization (object)
  #!+sb-doc
  "Cancel any finalization for OBJECT."
  ;; Check for NIL to avoid deleting finalizers that are waiting to be
  ;; run.
  (when object
    (sb!thread:with-mutex (*finalizer-store-lock*)
      (setf *finalizer-store*
	    (delete object *finalizer-store*
		    :key (lambda (pair) 
			   (weak-pointer-value (car pair))))))
    object))

(defun run-pending-finalizers ()
  (let (pending)
    (sb!thread:with-mutex (*finalizer-store-lock*)
      (setf *finalizer-store*
	    (delete-if  (lambda (pair)
			  (when (null (weak-pointer-value (car pair)))
			    (push (cdr pair) pending)
			    t))
		      *finalizer-store*)))
    ;; We want to run the finalizer bodies outside the lock in case
    ;; finalization of X causes finalization to be added for Y.
    (dolist (fun pending)
      (handler-case
	  (funcall fun)
	(error (c)
	  (warn "Error calling finalizer ~S:~%  ~S" fun c)))))
  nil)
