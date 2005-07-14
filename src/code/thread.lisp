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

(sb!xc:defmacro with-mutex ((mutex &key value (wait-p t)) &body body)
  #!+sb-doc
  "Acquire MUTEX for the dynamic scope of BODY, setting it to
NEW-VALUE or some suitable default value if NIL.  If WAIT-P is non-NIL
and the mutex is in use, sleep until it is available"
  #!-sb-thread (declare (ignore mutex value wait-p))
  #!+sb-thread
  (with-unique-names (got)
    `(let ((,got (get-mutex ,mutex ,value ,wait-p)))
      (when ,got
        (unwind-protect
             (locally ,@body)
          (release-mutex ,mutex)))))
  ;; KLUDGE: this separate expansion for (NOT SB-THREAD) is not
  ;; strictly necessary; GET-MUTEX and RELEASE-MUTEX are implemented.
  ;; However, there would be a (possibly slight) performance hit in
  ;; using them.
  #!-sb-thread
  `(locally ,@body))

(sb!xc:defmacro with-recursive-lock ((mutex) &body body)
  #!+sb-doc
  "Acquires MUTEX for the dynamic scope of BODY. Within that scope
further recursive lock attempts for the same mutex succeed. However,
it is an error to mix WITH-MUTEX and WITH-RECURSIVE-LOCK for the same
mutex."
  #!-sb-thread (declare (ignore mutex))
  #!+sb-thread
  (with-unique-names (cfp inner-lock)
    `(let ((,cfp (sb!kernel:current-fp))
           (,inner-lock
            (and (mutex-value ,mutex)
                 (sb!vm:control-stack-pointer-valid-p
                  (sb!sys:int-sap
                   (sb!kernel:get-lisp-obj-address (mutex-value ,mutex)))))))
      (unless ,inner-lock
        ;; this punning with MAKE-LISP-OBJ depends for its safety on
        ;; the frame pointer being a lispobj-aligned integer.  While
        ;; it is, then MAKE-LISP-OBJ will always return a FIXNUM, so
        ;; we're safe to do that.  Should this ever change, this
        ;; MAKE-LISP-OBJ could return something that looks like a
        ;; pointer, but pointing into neverneverland, which will
        ;; confuse GC completely.  -- CSR, 2003-06-03
        (get-mutex ,mutex (sb!kernel:make-lisp-obj (sb!sys:sap-int ,cfp))))
      (unwind-protect
           (locally ,@body)
        (unless ,inner-lock
          (release-mutex ,mutex)))))
  #!-sb-thread
  `(locally ,@body))
