;;;; -*-  Lisp -*-
;;;;
;;;; sb-fiber FFI bindings, alien-routines, fiber struct and other
;;;; runtime support

(in-package :sb-fiber)

#+sb-fiber
(progn

(define-alien-routine ("sb_fiber_create" %fiber-create)
    system-area-pointer
  (stack-size unsigned-long)
  (binding-stack-size unsigned-long))

(define-alien-routine ("sb_fiber_create_main" %fiber-create-main)
    system-area-pointer
  (thread system-area-pointer))

(define-alien-routine ("sb_fiber_release" %fiber-release)
    void
  (fiber system-area-pointer))

(define-alien-routine ("sb_fiber_register" %fiber-register)
    void
  (thread system-area-pointer)
  (fiber system-area-pointer))

(define-alien-routine ("sb_fiber_unregister" %fiber-unregister)
    void
  (thread system-area-pointer)
  (fiber system-area-pointer))

(define-alien-routine ("sb_fiber_migrate" %fiber-migrate)
    int
  (fiber system-area-pointer)
  (dest-thread system-area-pointer))

(define-alien-routine ("sb_fiber_prepare" %fiber-prepare)
    void
  (fiber system-area-pointer)
  (fn system-area-pointer)
  (arg system-area-pointer))

(define-alien-routine ("sb_fiber_control_stack_size_bytes"
                       %fiber-control-stack-size-bytes)
    unsigned-long
  (fiber system-area-pointer))

(define-alien-routine ("sb_fiber_control_stack_used_bytes"
                       %fiber-control-stack-used-bytes)
    unsigned-long
  (fiber system-area-pointer))

;;; --- Conditions ---------------------------------------------------------

(define-condition fiber-error (error)
  ((fiber :initarg :fiber :reader fiber-error-fiber :initform nil))
  (:documentation
   "Base class for sb-fiber errors that carry a fiber. Subclasses are
the recommended targets for HANDLER-CASE in scheduler code."))

(define-condition dead-fiber-error (fiber-error) ()
  (:documentation
   "Operation attempted on a fiber that has been released or has run to
completion.")
  (:report (lambda (c stream)
             (format stream "fiber ~S is dead or released"
                     (fiber-error-fiber c)))))

(define-condition pinned-fiber-error (fiber-error)
  ((depth :initarg :depth :reader pinned-fiber-error-depth))
  (:documentation
   "SWITCH-FIBER was asked to suspend a fiber whose pin count is
nonzero. Schedulers may CATCH and retry later.")
  (:report (lambda (c stream)
             (format stream "fiber ~S is pinned (depth ~D); cannot suspend"
                     (fiber-error-fiber c)
                     (pinned-fiber-error-depth c)))))

(define-condition fiber-thread-mismatch-error (fiber-error)
  ((role :initarg :role :reader fiber-thread-mismatch-error-role))
  (:documentation
   "A switch references a fiber owned by a different OS thread. A fiber
is bound to its owner thread (changeable via FIBER-MIGRATE); ROLE is
:FROM or :TO.")
  (:report (lambda (c stream)
             (format stream "fiber ~S (~(~A~)) is owned by a different thread"
                     (fiber-error-fiber c)
                     (fiber-thread-mismatch-error-role c)))))

(define-condition fiber-state-error (fiber-error)
  ((state    :initarg :state    :reader fiber-state-error-state)
   (expected :initarg :expected :reader fiber-state-error-expected))
  (:documentation
   "Fiber was not in an acceptable state for the requested operation.
STATE is the actual keyword state; EXPECTED is a list of acceptable
states.")
  (:report (lambda (c stream)
             (format stream "fiber ~S is in state ~S, expected ~{~S~^ or ~}"
                     (fiber-error-fiber c)
                     (fiber-state-error-state c)
                     (fiber-state-error-expected c)))))

(define-condition no-current-fiber-error (fiber-error)
  ((operation :initarg :operation :initform nil
              :reader no-current-fiber-error-operation))
  (:documentation
   "An operation requiring a current fiber was attempted on a thread with
no fiber bound in *CURRENT-FIBER*.")
  (:report (lambda (c stream)
             (format stream "~@[~(~A~): ~]no current fiber; establish one ~
with make-main-fiber or with-fiber-thread"
                     (no-current-fiber-error-operation c)))))

(declaim (inline %fiber-ctx))
(defun %fiber-ctx (sap)
  (declare (type sb-sys:system-area-pointer sap))
  (sb-alien:sap-alien sap (* fiber-ctx)))

(declaim (inline %switch-prep))
(defun %switch-prep (from to)
  (declare (type system-area-pointer from to))
  (locally (declare (optimize (sb-c:alien-funcall-saves-fp-and-pc 0)))
    (alien-funcall (sb-alien:extern-alien "sb_fiber_switch_prep"
                    (function void system-area-pointer system-area-pointer))
                   from to))
  (values))

(defstruct (fiber (:constructor %make-fiber)
                  (:print-object %print-fiber))
  (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer)
  (function nil :type (or null function))
  (pending-condition nil :type (or null condition))
  (escape-condition nil :type (or null condition))
  (escape-throw-tag nil :type symbol)
  (escape-throw-values nil :type list)
  (pin-count 0 :type (and fixnum unsigned-byte))
  (return-fiber nil :type (or null fiber))
  (name nil :type (or null string))
  (thread nil :type (or null sb-thread:thread))
  (value nil :type list)
  (released-p nil :type boolean))

(declaim (inline %fiber-state))
(defun %fiber-state (fiber)
  "Read the C-side state word of FIBER (an integer +fiber-...+ value)."
  (declare (type fiber fiber))
  (fiber-ctx-state (%fiber-ctx (fiber-sap fiber))))

(declaim (inline fiber-main-p))
(defun fiber-main-p (fiber)
  "Return T if FIBER represents a thread's own stack (created by
MAKE-MAIN-FIBER or WITH-FIBER-THREAD), NIL if FIBER is a worker fiber
created by MAKE-FIBER."
  (declare (type fiber fiber))
  (null (fiber-function fiber)))

(declaim (inline deliver-pending maybe-escape))
(defun deliver-pending (fiber)
  "Post-resume delivery hook. If FIBER has an interrupt-staged
condition, ERROR it (and clear the slot); otherwise a no-op. Called on
the fiber that is about to (re-)run, so interrupt-fiber semantics fire
in the target's own context."
  (declare (type fiber fiber))
  (let ((c (shiftf (fiber-pending-condition fiber) nil)))
    (when c (error c))))

(defun maybe-escape (fiber)
  "Post-resume delivery hook. If FIBER's entry function exited via an
unhandled error, re-ERROR that condition on the resumer's stack (and
clear the slot); otherwise a no-op. Called on the resumer side so the
escaped error re-fires in the resumer's context."
  (declare (type fiber fiber))
  (let ((c (shiftf (fiber-escape-condition fiber) nil)))
    (when c (error c))))

(defun maybe-escape-throw (fiber)
  "Post-resume delivery hook. If a thread-root unwind tag escaped
FIBER's entry function, re-THROW it on the resumer's stack (and clear
the slot); otherwise a no-op."
  (declare (type fiber fiber))
  (let ((tag (shiftf (fiber-escape-throw-tag fiber) nil)))
    (when tag
      (throw tag (values-list (shiftf (fiber-escape-throw-values fiber) nil))))))

(defun %print-fiber (fiber stream)
  (print-unreadable-object (fiber stream :type t :identity t)
    (format stream "~@[~A ~]~A" (fiber-name fiber) (fiber-state fiber))))

(defvar *current-fiber* nil
  "Fiber currently running on this thread, or NIL.")

(define-alien-callable sb-fiber-lisp-entry void
    ((arg unsigned-long))
  (let ((f *current-fiber*))
    (when (and f (fiber-function f))
      (assert (= arg (sb-sys:sap-int (fiber-sap f))))
      (let ((sb-kernel:*handler-clusters* sb-kernel::**initial-handler-clusters**)
            (sb-kernel:*restart-clusters* nil))
        (block done
          (flet ((run ()
                   ;; Non-error signals propagate normally so SIGNAL from
                   ;; inside the fiber behaves like SIGNAL anywhere else.
                   (handler-case
                       (let ((rv-list
                               (multiple-value-list
                                (progn (deliver-pending f)
                                       (funcall (fiber-function f))))))
                         (when (fiber-return-fiber f)
                           (setf (fiber-value (fiber-return-fiber f)) rv-list)))
                     (error (c)
                       (setf (fiber-escape-condition f) c)))
                   (return-from done)))
            (macrolet ((with-root-catch (tag &body inner)
                         `(let ((vals (multiple-value-list (catch ,tag ,@inner))))
                            (setf (fiber-escape-throw-tag f) ,tag
                                  (fiber-escape-throw-values f) vals)
                            (return-from done))))
              (with-root-catch 'sb-impl::toplevel-catcher
                (with-root-catch 'sb-impl::%end-of-the-world
                  (with-root-catch 'sb-thread::%abort-thread
                    (with-root-catch 'sb-thread::%return-from-thread
                      (run))))))))))))

(declaim (inline %thread-slot (setf %thread-slot)))
(defun %thread-slot (th-sap slot)
  (declare (type sb-sys:system-area-pointer th-sap) (type fixnum slot))
  (sb-sys:sap-ref-word th-sap (ash slot sb-vm:word-shift)))

(defun (setf %thread-slot) (val th-sap slot)
  (declare (type sb-sys:system-area-pointer th-sap)
           (type fixnum slot val))
  (setf (sb-sys:sap-ref-word th-sap (ash slot sb-vm:word-shift)) val))

(declaim (inline %current-thread-bits))
(defun %current-thread-bits ()
  (sb-sys:sap-int (sb-thread:current-thread-sap)))

(defvar *fiber-lisp-entry-sap*
  (sb-alien:alien-sap (alien-callable-function 'sb-fiber-lisp-entry))
  "SAP of the SB-FIBER-LISP-ENTRY trampoline, resolved once at load
time and reused by every MAKE-FIBER.")

;;; --- %switch phases ---

(declaim (inline check-switch))
(defun check-switch (from to from-ctx to-ctx)
  "Validate that *current thread* may switch FROM -> TO."
  (declare (type fiber from to))
  (let ((th-int (%current-thread-bits)))
    (unless (= (fiber-ctx-owner from-ctx) th-int)
      (error 'fiber-thread-mismatch-error :fiber from :role :from))
    (unless (= (fiber-ctx-owner to-ctx) th-int)
      (error 'fiber-thread-mismatch-error :fiber to :role :to)))
  (unless (= (fiber-ctx-state from-ctx) +fiber-running+)
    (error 'fiber-state-error
           :fiber from :state (fiber-state from) :expected '(:running)))
  (unless (zerop (fiber-pin-count from))
    (error 'pinned-fiber-error :fiber from :depth (fiber-pin-count from)))
  (let ((ts (fiber-ctx-state to-ctx)))
    (unless (or (= ts +fiber-runnable+) (= ts +fiber-new+))
      (error 'fiber-state-error
             :fiber to :state (fiber-state to) :expected '(:runnable :new)))))

(declaim (inline stage-return))
(defun stage-return (from to from-sap to-ctx values)
  "Record the return path and staged values on TO before the swap.
Safe to do outside PA: these touch Lisp-owned slots only."
  (declare (type fiber from to) (type list values))
  (setf (fiber-ctx-return-fiber to-ctx) (sb-sys:sap-int from-sap)
        (fiber-return-fiber to)        from
        (fiber-value to)               values))

(declaim (inline swap-frames))
(defun swap-frames (th-sap from-ctx to-ctx)
  "Save FROM's catch/unwind frames, install TO's, then install TO's
binding-stack pointer and base. Runs inside PA: catch/unwind chains
point into the running fiber's stack and must swap atomically with SP."
  (declare (type sb-sys:system-area-pointer th-sap))
  (let ((tc (%thread-slot th-sap sb-vm::thread-current-catch-block-slot))
        (tu (%thread-slot th-sap sb-vm::thread-current-unwind-protect-block-slot)))
    (setf (fiber-ctx-catch from-ctx)  tc
          (fiber-ctx-unwind from-ctx) tu
          (%thread-slot th-sap sb-vm::thread-current-catch-block-slot)
            (fiber-ctx-catch to-ctx)
          (%thread-slot th-sap sb-vm::thread-current-unwind-protect-block-slot)
            (fiber-ctx-unwind to-ctx)))
  ;; BSP install: prep already captured FROM's BSP.
  (setf (%thread-slot th-sap sb-vm::thread-binding-stack-pointer-slot)
          (fiber-ctx-bsp to-ctx)
        (%thread-slot th-sap sb-vm::thread-binding-stack-start-slot)
          (fiber-ctx-bs-start to-ctx)))

(declaim (inline flip-states))
(defun flip-states (from-ctx to-ctx)
  (setf (fiber-ctx-state from-ctx) +fiber-runnable+
        (fiber-ctx-state to-ctx)   +fiber-running+))

)
