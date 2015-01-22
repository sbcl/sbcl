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

(defglobal **finalizer-store** nil)

(defglobal **finalizer-store-lock**
  (sb!thread:make-mutex :name "Finalizer store lock."))

(defmacro with-finalizer-store-lock (&body body)
  `(sb!thread::with-system-mutex (**finalizer-store-lock** :without-gcing t)
     ,@body))

(defun finalize (object function &key dont-save)
  #!+sb-doc
  "Arrange for the designated FUNCTION to be called when there
are no more references to OBJECT, including references in
FUNCTION itself.

If DONT-SAVE is true, the finalizer will be cancelled when
SAVE-LISP-AND-DIE is called: this is useful for finalizers
deallocating system memory, which might otherwise be called
with addresses from the old image.

In a multithreaded environment FUNCTION may be called in any
thread. In both single and multithreaded environments FUNCTION
may be called in any dynamic scope: consequences are unspecified
if FUNCTION is not fully re-entrant.

Errors from FUNCTION are handled and cause a WARNING to be
signalled in whichever thread the FUNCTION was called in.

Examples:

  ;;; GOOD, assuming RELEASE-HANDLE is re-entrant.
  (let* ((handle (get-handle))
         (object (make-object handle)))
   (finalize object (lambda () (release-handle handle)))
   object)

  ;;; BAD, finalizer refers to object being finalized, causing
  ;;; it to be retained indefinitely!
  (let* ((handle (get-handle))
         (object (make-object handle)))
    (finalize object
              (lambda ()
                (release-handle (object-handle object)))))

  ;;; BAD, not re-entrant!
  (defvar *rec* nil)

  (defun oops ()
   (when *rec*
     (error \"recursive OOPS\"))
   (let ((*rec* t))
     (gc))) ; or just cons enough to cause one

  (progn
    (finalize \"oops\" #'oops)
    (oops)) ; GC causes re-entry to #'oops due to the finalizer
            ; -> ERROR, caught, WARNING signalled"
  (unless object
    (error "Cannot finalize NIL."))
  (with-finalizer-store-lock
    (push (list (make-weak-pointer object) function dont-save)
          **finalizer-store**))
  object)

(defun deinit-finalizers ()
  ;; remove :dont-save finalizers
  (with-finalizer-store-lock
    (setf **finalizer-store** (delete-if #'third **finalizer-store**)))
  nil)

(defun cancel-finalization (object)
  #!+sb-doc
  "Cancel any finalization for OBJECT."
  ;; Check for NIL to avoid deleting finalizers that are waiting to be
  ;; run.
  (when object
    (with-finalizer-store-lock
        (setf **finalizer-store**
              (delete object **finalizer-store**
                      :key (lambda (list)
                             (weak-pointer-value (car list))))))
    object))

(defun run-pending-finalizers ()
  (let (pending)
    ;; We want to run the finalizer bodies outside the lock in case
    ;; finalization of X causes finalization to be added for Y.
    ;; And to avoid consing we can reuse the deleted conses from the
    ;; store to build the list of functions.
    (with-finalizer-store-lock
      (loop with list = **finalizer-store**
          with previous
          for finalizer = (car list)
          do
          (unless finalizer
            (if previous
                (setf (cdr previous) nil)
                (setf **finalizer-store** nil))
            (return))
          unless (weak-pointer-value (car finalizer))
          do
          (psetf pending finalizer
                 (car finalizer) (second finalizer)
                 (cdr finalizer) pending
                 (car list) (cadr list)
                 (cdr list) (cddr list))
          else
          do (setf previous list
                   list (cdr list))))
    (dolist (fun pending)
      (handler-case
          (funcall fun)
        (error (c)
          (warn "Error calling finalizer ~S:~%  ~S" fun c)))))
  nil)
