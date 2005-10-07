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

(sb!xc:defmacro with-mutex ((mutex &key (value '*current-thread*) (wait-p t))
                            &body body)
  #!+sb-doc
  "Acquire MUTEX for the dynamic scope of BODY, setting it to
NEW-VALUE or some suitable default value if NIL.  If WAIT-P is non-NIL
and the mutex is in use, sleep until it is available"
  #!-sb-thread (declare (ignore mutex value wait-p))
  #!+sb-thread
  (with-unique-names (got mutex1)
    `(let ((,mutex1 ,mutex)
           ,got)
       (unwind-protect
            ;; FIXME: async unwind in SETQ form
            (when (setq ,got (get-mutex ,mutex1 ,value ,wait-p))
              (locally
                  ,@body))
         (when ,got
           (release-mutex ,mutex1)))))
  ;; KLUDGE: this separate expansion for (NOT SB-THREAD) is not
  ;; strictly necessary; GET-MUTEX and RELEASE-MUTEX are implemented.
  ;; However, there would be a (possibly slight) performance hit in
  ;; using them.
  #!-sb-thread
  `(locally ,@body))

(sb!xc:defmacro with-recursive-lock ((mutex) &body body)
  #!+sb-doc
  "Acquires MUTEX for the dynamic scope of BODY. Within that scope
further recursive lock attempts for the same mutex succeed. It is
allowed to mix WITH-MUTEX and WITH-RECURSIVE-LOCK for the same mutex
provided the default value is used for the mutex."
  #!-sb-thread
  (declare (ignore mutex)) #!+sb-thread
  (with-unique-names (mutex1 inner-lock-p)
    `(let* ((,mutex1 ,mutex)
            (,inner-lock-p (eq (mutex-value ,mutex1) *current-thread*)))
       (unwind-protect
            (progn
              (unless ,inner-lock-p
                (get-mutex ,mutex1))
              (locally
                  ,@body))
         (unless ,inner-lock-p
           (release-mutex ,mutex1)))))
  #!-sb-thread
  `(locally ,@body))
