;;;; single stepper for SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; Single stepping works by having compiler insert STEP-CONDITION
;;;; signalling forms into code compiled at high debug settings, and
;;;; having a handler for them at the toplevel.

(in-package "SB-IMPL")

;; Used for controlling whether the stepper is enabled / disabled when
;; building without SB-THREAD. With SB-THREAD, a slot in the thread
;; structure is used instead. (See EMIT-SINGLE-STEP-TEST in
;; src/compiler/x86/call.lisp).
#-sb-thread
(defvar *stepping* 0)

#-sb-xc-host
(progn

;; Used for implementing the STEP-OUT restart. The step-wrapper will
;; bind this to :MAYBE, before calling the wrapped code. When
;; unwinding, the wrapper will check whether it's been set to T. If
;; so, it'll re-enable the stepper. This is a tri-state variable (NIL,
;; :MAYBE, T) so that the debugger can detect in advance whether the
;; OUT debugger command will actually have a wrapper to step out to.
(define-thread-local *step-out* nil)

;; These functions make no sense on the host, but putting them in
;; 'step.lisp' is too late, because 'step' is compiled in warm load,
;; but the REPL calls DISABLE-STEPPING right way.
;; Adding a file of target-only code for these isn't worth the trouble.
(symbol-macrolet ((place
                   #+sb-thread (sb-thread::thread-stepping)
                   #-sb-thread *stepping*))
  (defun (setf stepping) (new-value)
    (setf place new-value))
  (defun stepping-enabled-p ()
    (= place 1)))

(defun enable-stepping ()
  (setf (stepping) 1))
(defun disable-stepping ()
  (setf (stepping) 0))

(defmacro with-stepping-enabled (&body body)
  (let ((orig (gensym)))
    `(let ((,orig (stepping-enabled-p)))
       (unwind-protect
            (progn
              (enable-stepping)
              ,@body)
         (setf (stepping) (if ,orig 1 0))))))

(defmacro with-stepping-disabled (&body body)
  (let ((orig (gensym)))
    `(let ((,orig (stepping-enabled-p)))
       (unwind-protect
            (progn
              (disable-stepping)
              ,@body)
         (setf (stepping) (if ,orig 1 0))))))
) ; end PROGN
