;;;; support for threads needed at cross-compile time

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-THREAD")

(!define-thread-local *current-thread* nil
      "Bound in each thread to the thread itself.")

(defstruct (foreign-thread
             (:copier nil)
             (:include thread)
             (:conc-name "THREAD-"))
  "Type of native threads which are attached to the runtime as Lisp threads
temporarily.")

#+(and sb-safepoint-strictly (not win32))
(defstruct (signal-handling-thread
             (:copier nil)
             (:include foreign-thread)
             (:conc-name "THREAD-"))
  "Asynchronous signal handling thread."
  (signal-number nil :type integer))

(defun mutex-value (mutex)
  "Current owner of the mutex, NIL if the mutex is free. May return a
stale value, use MUTEX-OWNER instead."
  (mutex-%owner mutex))

(defun holding-mutex-p (mutex)
  "Test whether the current thread is holding MUTEX."
  ;; This is about the only use for which a stale value of owner is
  ;; sufficient.
  (eq sb-thread:*current-thread* (mutex-%owner mutex)))

(defsetf mutex-value set-mutex-value)

(declaim (sb-ext:deprecated :final ("SBCL" "1.2.15") #'set-mutex-value))

;;; SPINLOCK no longer exists as a type -- provided for backwards compatibility.

(deftype spinlock ()
  "Spinlock type."
  'mutex)

(declaim (sb-ext:deprecated
          :late ("SBCL" "1.0.53.11") (type spinlock :replacement mutex)))

(define-deprecated-function :early "1.0.53.11" make-spinlock make-mutex (&key name)
  (make-mutex :name name))

(define-deprecated-function :early "1.0.53.11" spinlock-name mutex-name (lock)
  (mutex-name lock))

(define-deprecated-function :early "1.0.53.11" (setf spinlock-name) (setf mutex-name) (name lock)
  (setf (mutex-name lock) name))

(define-deprecated-function :early "1.0.53.11" spinlock-value mutex-owner (lock)
  (mutex-owner lock))

(define-deprecated-function :early "1.0.53.11" get-spinlock grab-mutex (lock)
  (grab-mutex lock))

(define-deprecated-function :early "1.0.53.11" release-spinlock release-mutex (lock)
  (release-mutex lock))

(defmacro with-recursive-spinlock ((lock) &body body)
  `(with-recursive-lock (,lock)
     ,@body))

(defmacro with-spinlock ((lock) &body body)
  `(with-mutex (,lock)
     ,@body))

(declaim (sb-ext:deprecated
          :early ("SBCL" "1.0.53.11")
          (function with-recursive-spinlock :replacement with-recursive-lock)
          (function with-spinlock :replacement with-mutex)))

(defmacro without-thread-waiting-for ((&key already-without-interrupts) &body body)
  (with-unique-names (thread prev)
    (let ((without (if already-without-interrupts
                       'progn
                       'without-interrupts))
          (with (if already-without-interrupts
                    'progn
                    'with-local-interrupts)))
      `(let* ((,thread *current-thread*)
              (,prev (progn
                       (barrier (:read))
                       (thread-waiting-for ,thread))))
         (flet ((exec () ,@body))
           (if ,prev
               (,without
                (unwind-protect
                     (progn
                       (setf (thread-waiting-for ,thread) nil)
                       (barrier (:write))
                       (,with (exec)))
                  ;; If we were waiting on a waitqueue, this becomes a bogus
                  ;; wakeup.
                  (when (mutex-p ,prev)
                    (setf (thread-waiting-for ,thread) ,prev)
                    (barrier (:write)))))
               (exec)))))))

(defmacro with-mutex ((mutex &key (wait-p t) timeout value)
                            &body body)
  "Acquire MUTEX for the dynamic scope of BODY. If WAIT-P is true (the default),
and the MUTEX is not immediately available, sleep until it is available.

If TIMEOUT is given, it specifies a relative timeout, in seconds, on how long
the system should try to acquire the lock in the contested case.

If the mutex isn't acquired successfully due to either WAIT-P or TIMEOUT, the
body is not executed, and WITH-MUTEX returns NIL.

Otherwise body is executed with the mutex held by current thread, and
WITH-MUTEX returns the values of BODY.

Historically WITH-MUTEX also accepted a VALUE argument, which when provided
was used as the new owner of the mutex instead of the current thread. This is
no longer supported: if VALUE is provided, it must be either NIL or the
current thread."
  `(dx-flet ((with-mutex-thunk () ,@body))
     (call-with-mutex
      #'with-mutex-thunk
      ,mutex
      ,value
      ,wait-p
      ,timeout)))

(defmacro with-recursive-lock ((mutex &key (wait-p t) timeout) &body body)
  "Acquire MUTEX for the dynamic scope of BODY.

If WAIT-P is true (the default), and the MUTEX is not immediately available or
held by the current thread, sleep until it is available.

If TIMEOUT is given, it specifies a relative timeout, in seconds, on how long
the system should try to acquire the lock in the contested case.

If the mutex isn't acquired successfully due to either WAIT-P or TIMEOUT, the
body is not executed, and WITH-RECURSIVE-LOCK returns NIL.

Otherwise body is executed with the mutex held by current thread, and
WITH-RECURSIVE-LOCK returns the values of BODY.

Unlike WITH-MUTEX, which signals an error on attempt to re-acquire an already
held mutex, WITH-RECURSIVE-LOCK allows recursive lock attempts to succeed."
  `(dx-flet ((with-recursive-lock-thunk () ,@body))
     (call-with-recursive-lock
      #'with-recursive-lock-thunk
      ,mutex
      ,wait-p
      ,timeout)))

(macrolet ((def (name &optional variant)
             `(defun ,(if variant (symbolicate name "/" variant) name)
                  (function mutex)
                (declare (function function))
                (declare (dynamic-extent function))
                (flet ((%call-with-system-mutex ()
                         (dx-let (got-it)
                           (unwind-protect
                                (when (setf got-it (grab-mutex mutex))
                                  (funcall function))
                             (when got-it
                               (release-mutex mutex))))))
                  (declare (inline %call-with-system-mutex))
                  ,(ecase variant
                     (:without-gcing
                       `(without-gcing (%call-with-system-mutex)))
                     (:allow-with-interrupts
                       `(without-interrupts
                          (allow-with-interrupts (%call-with-system-mutex))))
                     ((nil)
                      `(without-interrupts (%call-with-system-mutex))))))))
  (def call-with-system-mutex)
  (def call-with-system-mutex :without-gcing)
  (def call-with-system-mutex :allow-with-interrupts))

#-sb-thread
(progn
  (defun call-with-mutex (function mutex value waitp timeout)
    (declare (ignore mutex waitp timeout)
             (function function))
    (unless (or (null value) (eq *current-thread* value))
      (error "~S called with non-nil :VALUE that isn't the current thread."
             'with-mutex))
    (funcall function))

  (defun call-with-recursive-lock (function mutex waitp timeout)
    (declare (ignore mutex waitp timeout)
             (function function))
    (funcall function))

  (defun call-with-recursive-system-lock (function lock)
    (declare (function function) (ignore lock))
    (without-interrupts
      (funcall function)))

  (defun call-with-recursive-system-lock/without-gcing (function mutex)
    (declare (function function) (ignore mutex))
    (without-gcing
      (funcall function))))

#+sb-thread
;;; KLUDGE: These need to use DX-LET, because the cleanup form that
;;; closes over GOT-IT causes a value-cell to be allocated for it --
;;; and we prefer that to go on the stack since it can.
(progn
  (defun call-with-mutex (function mutex value waitp timeout)
    (declare (function function))
    (declare (dynamic-extent function))
    (unless (or (null value) (eq *current-thread* value))
      (error "~S called with non-nil :VALUE that isn't the current thread."
             'with-mutex))
    (dx-let ((got-it nil))
      (without-interrupts
        (unwind-protect
             (when (setq got-it (allow-with-interrupts
                                  (grab-mutex mutex :waitp waitp
                                                    :timeout timeout)))
               (with-local-interrupts (funcall function)))
          (when got-it
            (release-mutex mutex))))))

  (defun call-with-recursive-lock (function mutex waitp timeout)
    (declare (function function))
    (declare (dynamic-extent function))
    (dx-let ((inner-lock-p (eq (mutex-%owner mutex) *current-thread*))
             (got-it nil))
      (without-interrupts
        (unwind-protect
             (when (or inner-lock-p (setf got-it (allow-with-interrupts
                                                   (grab-mutex mutex :waitp waitp
                                                                     :timeout timeout))))
               (with-local-interrupts (funcall function)))
          (when got-it
            (release-mutex mutex))))))

  (macrolet ((def (name &optional variant)
               `(defun ,(if variant (symbolicate name "/" variant) name)
                    (function lock)
                  (declare (function function))
                  (declare (dynamic-extent function))
                  (flet ((%call-with-recursive-system-lock ()
                           (dx-let ((inner-lock-p
                                     (eq *current-thread* (mutex-owner lock)))
                                    (got-it nil))
                             (unwind-protect
                                  (when (or inner-lock-p
                                            (setf got-it (grab-mutex lock)))
                                    (funcall function))
                               (when got-it
                                 (release-mutex lock))))))
                    (declare (inline %call-with-recursive-system-lock))
                    ,(ecase variant
                      (:without-gcing
                        `(without-gcing (%call-with-recursive-system-lock)))
                      ((nil)
                        `(without-interrupts (%call-with-recursive-system-lock))))))))
    (def call-with-recursive-system-lock)
    (def call-with-recursive-system-lock :without-gcing)))
