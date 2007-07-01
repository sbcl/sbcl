;;;; support for threads needed at cross-compile time

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!THREAD")

(def!struct mutex
  #!+sb-doc
  "Mutex type."
  (name nil :type (or null simple-string))
  (value nil)
  #!+(and sb-lutex sb-thread)
  (lutex (make-lutex)))

(def!struct spinlock
  #!+sb-doc
  "Spinlock type."
  (name nil :type (or null simple-string))
  (value nil))

(sb!xc:defmacro with-mutex ((mutex &key (value '*current-thread*) (wait-p t))
                            &body body)
  #!+sb-doc
  "Acquire MUTEX for the dynamic scope of BODY, setting it to
NEW-VALUE or some suitable default value if NIL.  If WAIT-P is non-NIL
and the mutex is in use, sleep until it is available"
  `(dx-flet ((with-mutex-thunk () ,@body))
     (call-with-mutex
      #'with-mutex-thunk
      ,mutex
      ,value
      ,wait-p)))

(sb!xc:defmacro with-system-mutex ((mutex &key without-gcing) &body body)
  `(dx-flet ((with-system-mutex-thunk () ,@body))
     (call-with-system-mutex
      #'with-system-mutex-thunk
      ,mutex
      ,without-gcing)))

(sb!xc:defmacro with-recursive-lock ((mutex) &body body)
  #!+sb-doc
  "Acquires MUTEX for the dynamic scope of BODY. Within that scope
further recursive lock attempts for the same mutex succeed. It is
allowed to mix WITH-MUTEX and WITH-RECURSIVE-LOCK for the same mutex
provided the default value is used for the mutex."
  `(dx-flet ((with-recursive-lock-thunk () ,@body))
     (call-with-recursive-lock
      #'with-recursive-lock-thunk
      ,mutex)))

(sb!xc:defmacro with-recursive-spinlock ((spinlock) &body body)
  `(dx-flet ((with-recursive-spinlock-thunk () ,@body))
     (call-with-recursive-spinlock
      #'with-recursive-spinlock-thunk
      ,spinlock)))

(sb!xc:defmacro with-recursive-system-spinlock ((spinlock &key without-gcing)
                                                &body body)
  `(dx-flet ((with-recursive-system-spinlock-thunk () ,@body))
     (call-with-recursive-system-spinlock
      #'with-recursive-system-spinlock-thunk
      ,spinlock
      ,without-gcing)))

(sb!xc:defmacro with-spinlock ((spinlock) &body body)
  `(dx-flet ((with-spinlock-thunk () ,@body))
     (call-with-spinlock
      #'with-spinlock-thunk
      ,spinlock)))

;;; KLUDGE: this separate implementation for (NOT SB-THREAD) is not
;;; strictly necessary; GET-MUTEX and RELEASE-MUTEX are implemented.
;;; However, there would be a (possibly slight) performance hit in
;;; using them.
#!-sb-thread
(progn
  (defun call-with-system-mutex (function mutex &optional without-gcing-p)
    (declare (ignore mutex)
             (function function))
    (if without-gcing-p
        (without-gcing
          (funcall function))
        (without-interrupts
          (allow-with-interrupts (funcall function)))))

  (defun call-with-recursive-system-spinlock (function lock
                                              &optional without-gcing-p)
    (declare (ignore lock)
             (function function))
    (if without-gcing-p
        (without-gcing
          (funcall function))
        (without-interrupts
          (allow-with-interrupts (funcall function)))))

  (defun call-with-mutex (function mutex value waitp)
    (declare (ignore mutex value waitp)
             (function function))
    (funcall function))

  (defun call-with-recursive-lock (function mutex)
    (declare (ignore mutex) (function function))
    (funcall function))

  (defun call-with-spinlock (function spinlock)
    (declare (ignore spinlock) (function function))
    (funcall function))

  (defun call-with-recursive-spinlock (function spinlock)
    (declare (ignore spinlock) (function function))
    (funcall function)))

#!+sb-thread
;;; KLUDGE: These need to use DX-LET, because the cleanup form that
;;; closes over GOT-IT causes a value-cell to be allocated for it -- and
;;; we prefer that to go on the stack since it can.
(progn
  (defun call-with-system-mutex (function mutex &optional without-gcing-p)
    (declare (function function))
    (flet ((%call-with-system-mutex ()
             (dx-let (got-it)
               (unwind-protect
                    (when (setf got-it (get-mutex mutex))
                      (funcall function))
                 (when got-it
                   (release-mutex mutex))))))
      (if without-gcing-p
          (without-gcing
            (%call-with-system-mutex))
          (without-interrupts
            (allow-with-interrupts (%call-with-system-mutex))))))

  (defun call-with-recursive-system-spinlock (function lock
                                              &optional without-gcing-p)
    (declare (function function))
    (flet ((%call-with-system-spinlock ()
             (dx-let ((inner-lock-p (eq *current-thread* (spinlock-value lock)))
                      (got-it nil))
               (unwind-protect
                    (when (or inner-lock-p (setf got-it (get-spinlock lock)))
                      (funcall function))
                 (when got-it
                   (release-spinlock lock))))))
      (if without-gcing-p
          (without-gcing
            (%call-with-system-spinlock))
          (without-interrupts
            (allow-with-interrupts (%call-with-system-spinlock))))))

  (defun call-with-spinlock (function spinlock)
    (declare (function function))
    (dx-let ((got-it nil))
      (without-interrupts
        (unwind-protect
             (when (setf got-it (allow-with-interrupts
                                 (get-spinlock spinlock)))
               (with-local-interrupts (funcall function)))
          (when got-it
            (release-spinlock spinlock))))))

  (defun call-with-mutex (function mutex value waitp)
    (declare (function function))
    (dx-let ((got-it nil))
      (without-interrupts
        (unwind-protect
             (when (setq got-it (allow-with-interrupts
                                 (get-mutex mutex value waitp)))
               (with-local-interrupts (funcall function)))
          (when got-it
            (release-mutex mutex))))))

  (defun call-with-recursive-lock (function mutex)
    (declare (function function))
    (dx-let ((inner-lock-p (eq (mutex-value mutex) *current-thread*))
             (got-it nil))
      (without-interrupts
        (unwind-protect
             (when (or inner-lock-p (setf got-it (allow-with-interrupts
                                                  (get-mutex mutex))))
               (with-local-interrupts (funcall function)))
          (when got-it
            (release-mutex mutex))))))



  (defun call-with-recursive-spinlock (function spinlock)
    (declare (function function))
    (dx-let ((inner-lock-p (eq (spinlock-value spinlock) *current-thread*))
          (got-it nil))
      (without-interrupts
        (unwind-protect
             (when (or inner-lock-p (setf got-it (allow-with-interrupts
                                                  (get-spinlock spinlock))))
               (with-local-interrupts (funcall function)))
          (when got-it
            (release-spinlock spinlock)))))))
