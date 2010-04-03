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

(def!type thread-name ()
  'simple-string)

(def!struct (thread (:constructor %make-thread))
  #!+sb-doc
  "Thread type. Do not rely on threads being structs as it may change
in future versions."
  (name          nil :type (or thread-name null))
  (%alive-p      nil :type boolean)
  (os-thread     nil :type (or integer null))
  (interruptions nil :type list)
  (result        nil :type list)
  (interruptions-lock
   (make-mutex :name "thread interruptions lock")
   :type mutex)
  (result-lock
   (make-mutex :name "thread result lock")
   :type mutex))

(def!struct mutex
  #!+sb-doc
  "Mutex type."
  (name   nil :type (or null thread-name))
  (%owner nil :type (or null thread))
  #!+(and (not sb-lutex) sb-thread)
  (state    0 :type fixnum)
  #!+(and sb-lutex sb-thread)
  (lutex (make-lutex)))

(defun mutex-value (mutex)
  "Current owner of the mutex, NIL if the mutex is free. May return a
stale value, use MUTEX-OWNER instead."
  (mutex-%owner mutex))

(defun holding-mutex-p (mutex)
  "Test whether the current thread is holding MUTEX."
  ;; This is about the only use for which a stale value of owner is
  ;; sufficient.
  (eq sb!thread:*current-thread* (mutex-%owner mutex)))

(defsetf mutex-value set-mutex-value)

(declaim (inline set-mutex-value))
(defun set-mutex-value (mutex value)
  (declare (ignore mutex value))
  (error "~S is no longer supported." '(setf mutex-value)))

(define-compiler-macro set-mutex-value (&whole form mutex value)
  (declare (ignore mutex value))
  (warn "~S is no longer supported, and will signal an error at runtime."
        '(setf mutex-value))
  form)

(def!struct spinlock
  #!+sb-doc
  "Spinlock type."
  (name  nil :type (or null thread-name))
  (value nil))

(sb!xc:defmacro with-mutex ((mutex &key (value '*current-thread*) (wait-p t))
                            &body body)
  #!+sb-doc
  "Acquire MUTEX for the dynamic scope of BODY, setting it to VALUE or
some suitable default value if NIL.  If WAIT-P is non-NIL and the mutex
is in use, sleep until it is available"
  `(dx-flet ((with-mutex-thunk () ,@body))
     (call-with-mutex
      #'with-mutex-thunk
      ,mutex
      ,value
      ,wait-p)))

(sb!xc:defmacro with-system-mutex ((mutex
                                    &key without-gcing allow-with-interrupts)
                                   &body body)
  `(dx-flet ((with-system-mutex-thunk () ,@body))
     (,(cond (without-gcing
               'call-with-system-mutex/without-gcing)
             (allow-with-interrupts
              'call-with-system-mutex/allow-with-interrupts)
             (t
              'call-with-system-mutex))
       #'with-system-mutex-thunk
       ,mutex)))

(sb!xc:defmacro with-system-spinlock ((spinlock &key) &body body)
  `(dx-flet ((with-system-spinlock-thunk () ,@body))
     (call-with-system-spinlock
       #'with-system-spinlock-thunk
       ,spinlock)))

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

(sb!xc:defmacro with-recursive-system-spinlock ((spinlock
                                                 &key without-gcing)
                                                &body body)
  `(dx-flet ((with-recursive-system-spinlock-thunk () ,@body))
     (,(cond (without-gcing
               'call-with-recursive-system-spinlock/without-gcing)
             (t
              'call-with-recursive-system-spinlock))
       #'with-recursive-system-spinlock-thunk
       ,spinlock)))

(sb!xc:defmacro with-spinlock ((spinlock) &body body)
  `(dx-flet ((with-spinlock-thunk () ,@body))
     (call-with-spinlock
      #'with-spinlock-thunk
      ,spinlock)))

(macrolet ((def (name &optional variant)
             `(defun ,(if variant (symbolicate name "/" variant) name)
                  (function mutex)
                (declare (function function))
                (flet ((%call-with-system-mutex ()
                         (dx-let (got-it)
                           (unwind-protect
                                (when (setf got-it (get-mutex mutex))
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

#!-sb-thread
(progn
  (macrolet ((def (name &optional variant)
               `(defun ,(if variant (symbolicate name "/" variant) name)
                    (function lock)
                  (declare (ignore lock) (function function))
                  ,(ecase variant
                    (:without-gcing
                      `(without-gcing (funcall function)))
                    (:allow-with-interrupts
                      `(without-interrupts
                         (allow-with-interrupts (funcall function))))
                    ((nil)
                      `(without-interrupts (funcall function)))))))
    (def call-with-system-spinlock)
    (def call-with-recursive-system-spinlock)
    (def call-with-recursive-system-spinlock :without-gcing))

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
;;; closes over GOT-IT causes a value-cell to be allocated for it --
;;; and we prefer that to go on the stack since it can.
(progn
  (defun call-with-system-spinlock (function spinlock)
    (declare (function function))
    (without-interrupts
      (dx-let (got-it)
        (unwind-protect
             (when (setf got-it (get-spinlock spinlock))
               (funcall function))
          (when got-it
            (release-spinlock spinlock))))))

  (macrolet ((def (name &optional variant)
               `(defun ,(if variant (symbolicate name "/" variant) name)
                    (function spinlock)
                  (declare (function function))
                  (flet ((%call-with-system-spinlock ()
                           (dx-let ((inner-lock-p
                                     (eq *current-thread*
                                         (spinlock-value spinlock)))
                                    (got-it nil))
                             (unwind-protect
                                  (when (or inner-lock-p
                                            (setf got-it
                                                  (get-spinlock spinlock)))
                                    (funcall function))
                               (when got-it
                                 (release-spinlock spinlock))))))
                    (declare (inline %call-with-system-spinlock))
                    ,(ecase variant
                      (:without-gcing
                        `(without-gcing (%call-with-system-spinlock)))
                      ((nil)
                        `(without-interrupts (%call-with-system-spinlock))))))))
    (def call-with-recursive-system-spinlock)
    (def call-with-recursive-system-spinlock :without-gcing))

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
    (dx-let ((inner-lock-p (eq (mutex-%owner mutex) *current-thread*))
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
