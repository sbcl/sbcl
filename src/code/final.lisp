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
are no more references to OBJECT, including references in
FUNCTION itself.

In a multithreaded environment FUNCTION may be called in any
thread. In both single and multithreaded environments FUNCTION
may be called in any dynamic scope: consequences are unspecified
if FUNCTION is not fully re-entrant.

Errors from FUNCTION are handled and cause a WARNING to be
signalled in whichever thread the FUNCTION was called in.

Examples:

  ;;; good
  (let* ((handle (get-handle))
         (object (make-object handle)))
   ;; assumes RELEASE-HANDLE is re-entrant
   (finalize object (lambda () (release-handle handle)))
   object)

  ;;; bad, finalizer refers to object being finalized, causing
  ;;; it to be retained indefinitely
  (let* ((handle (get-handle))
         (object (make-object handle)))
    (finalize object (lambda () (release-handle (object-handle object)))))

  ;;; bad, not re-entrant
  (defvar *rec* nil)

  (defun oops ()
   (when *rec* 
     (error \"recursive OOPS\"))
   (let ((*rec* t))
     (gc))) ; or just cons enough to cause one

  (progn 
    (finalize \"oops\" #'oops)
    (oops)) ; causes GC and re-entry to #'oops due to the finalizer
            ; -> ERROR, caught, WARNING signalled"
  (sb!sys:without-gcing
      (sb!thread:with-mutex (*finalizer-store-lock*)
	(push (cons (make-weak-pointer object) function)
	      *finalizer-store*)))
  object)

(defun cancel-finalization (object)
  #!+sb-doc
  "Cancel any finalization for OBJECT."
  ;; Check for NIL to avoid deleting finalizers that are waiting to be
  ;; run.
  (when object
    (sb!sys:without-gcing
	(sb!thread:with-mutex (*finalizer-store-lock*)
	  (setf *finalizer-store*
		(delete object *finalizer-store*
			:key (lambda (pair) 
			       (weak-pointer-value (car pair)))))))
    object))

(defun run-pending-finalizers ()
  (let (pending)
    (sb!sys:without-gcing
	(sb!thread:with-mutex (*finalizer-store-lock*)
	  (setf *finalizer-store*
		(delete-if  (lambda (pair)
			      (when (null (weak-pointer-value (car pair)))
				(push (cdr pair) pending)
				t))
			    *finalizer-store*))))
    ;; We want to run the finalizer bodies outside the lock in case
    ;; finalization of X causes finalization to be added for Y.
    (dolist (fun pending)
      (handler-case
	  (funcall fun)
	(error (c)
	  (warn "Error calling finalizer ~S:~%  ~S" fun c)))))
  nil)
