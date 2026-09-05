;;;; -*-  Lisp -*-
;;;;
;;;; sb-fiber API

(in-package :sb-fiber)

#+sb-fiber
(progn

(defvar *default-fiber-stack-size* 65536
  "Default control stack size (bytes) for MAKE-FIBER.")

(defvar *default-fiber-binding-stack-size* 8192
  "Default binding stack size (bytes) for MAKE-FIBER.")

(defmacro with-fiber-sap ((sap alloc-form) &body body)
  "Bind SAP to ALLOC-FORM.  Signal if the SAP is null; on non-local
exit from BODY, call %FIBER-RELEASE on SAP."
  (let ((ok (gensym "OK")))
    `(let ((,sap ,alloc-form))
       (when (zerop (sb-sys:sap-int ,sap))
         (error "failed to allocate fiber"))
       (let (,ok)
         (unwind-protect
              (multiple-value-prog1 (progn ,@body) (setf ,ok t))
           (unless ,ok (%fiber-release ,sap)))))))

;;; --- Lifecycle ---

(defun %install-fiber (sap function name)
  "Wrap an allocated SAP in a fiber struct, prepare it (if a worker),
and register it with the current thread.  Caller arranges
%FIBER-RELEASE on failure."
  (let ((f (%make-fiber :sap sap :function function :name name
                        :thread sb-thread:*current-thread*)))
    (when function
      (%fiber-prepare sap *fiber-lisp-entry-sap* sap))
    (%fiber-register (sb-thread:current-thread-sap) sap)
    f))

(defun make-main-fiber (&key name)
  "Create a fiber representing the current thread's own stack and bind
*CURRENT-FIBER* to it.  NAME is a string label used by PRINT-OBJECT."
  (with-fiber-sap (sap (%fiber-create-main (sb-thread:current-thread-sap)))
    (setf *current-fiber* (%install-fiber sap nil name))))

(defmacro with-fiber-thread ((&key name) &body body)
  "Register a main fiber on the calling thread for the dynamic extent
of BODY and release it on exit.  A no-op if BODY is reached with
*CURRENT-FIBER* already bound.  NAME is forwarded to MAKE-MAIN-FIBER."
  (let ((created (gensym "CREATED")))
    `(let* ((*current-fiber* *current-fiber*)
            (,created (unless *current-fiber* (make-main-fiber :name ,name))))
       (unwind-protect (progn ,@body)
         (when ,created (release-fiber ,created))))))

(defun make-fiber (function &key name
                                 (stack-size *default-fiber-stack-size*)
                              (binding-stack-size
                               *default-fiber-binding-stack-size*))
  "Create a fiber that runs FUNCTION (zero-argument) when first
switched to.  NAME is a string label used by PRINT-OBJECT.

When FUNCTION returns, the fiber is marked DEAD and control switches
back to its most recent resumer, delivering FUNCTION's return value
as that resumer's SWITCH-FIBER value.

The calling thread must already have a main fiber bound in
*CURRENT-FIBER*.  Use MAKE-MAIN-FIBER or WITH-FIBER-THREAD to establish
one before calling MAKE-FIBER.  Signals an error otherwise.

Fibers are auto-released when their owning thread exits; explicit
RELEASE-FIBER is only needed if you want to reclaim resources sooner."
  (unless *current-fiber*
    (error 'no-current-fiber-error :operation 'make-fiber))
  (with-fiber-sap (sap (%fiber-create stack-size binding-stack-size))
    (%install-fiber sap function name)))

(defun release-fiber (fiber)
  "Release FIBER.  For a worker fiber, unmaps its stacks and frees the
bookkeeping struct.  For a main fiber, just frees the struct; the
thread's own stack and binding stack are untouched.  Signals
FIBER-STATE-ERROR if FIBER is a worker that is still RUNNING.
Registered fibers are released automatically when their owning thread
exits."
  (unless (fiber-released-p fiber)
    (when (and (not (fiber-main-p fiber))
               (= (%fiber-state fiber) +fiber-running+))
      (error 'fiber-state-error
             :fiber fiber :state :running
             :expected '(:new :runnable :dead)))
    (%fiber-release (shiftf (fiber-sap fiber) (sb-sys:int-sap 0)))
    (setf (fiber-thread fiber)     nil
          (fiber-released-p fiber) t)
    (when (eq *current-fiber* fiber)
      (setf *current-fiber* nil))))

(defun fiber-state (fiber)
  "Return the current state of FIBER as a keyword: :NEW, :RUNNABLE,
:RUNNING, or :DEAD."
  (if (fiber-released-p fiber)
      :dead
      (svref #.(coerce '(:new :runnable :running :dead) 'simple-vector)
             (%fiber-state fiber))))

(declaim (inline fiber-alive-p))
(defun fiber-alive-p (fiber)
  "Return T if FIBER has not finished running and has not been
released."
  (declare (type fiber fiber))
  (and (not (fiber-released-p fiber))
       (/= (%fiber-state fiber) +fiber-dead+)))

(declaim (inline current-fiber)
         (ftype (function () (or null fiber)) current-fiber))
(defun current-fiber ()
  "Return the fiber currently running on this thread, or NIL if no
main fiber is bound."
  *current-fiber*)

;;; --- Stack-usage accessors ---

(defun fiber-control-stack-size (fiber)
  "Bytes of usable control-stack region mapped for FIBER.  For a
worker fiber, the size passed to MAKE-FIBER (rounded up to a page);
for a main fiber, the calling thread's control stack."
  (declare (type fiber fiber))
  (if (fiber-released-p fiber)
      0
      (%fiber-control-stack-size-bytes (fiber-sap fiber))))

(defun fiber-control-stack-usage (fiber)
  "Bytes of FIBER's control stack currently in use.  For a :RUNNABLE
or :NEW fiber, this is the depth at suspension (or initial trampoline
frame for :NEW).  For a :RUNNING fiber, this may be the saved SP from
the last suspend."
  (declare (type fiber fiber))
  (if (fiber-released-p fiber)
      0
      (%fiber-control-stack-used-bytes (fiber-sap fiber))))

(defun fiber-binding-stack-size (fiber)
  "Bytes of binding-stack region mapped for FIBER.  Returns 0 for a
main fiber (its binding stack is the thread's own, not owned by the
fiber wrapper)."
  (declare (type fiber fiber))
  (if (fiber-released-p fiber)
      0
      (let* ((c (%fiber-ctx (fiber-sap fiber)))
             (b (fiber-ctx-bs-base c))
             (e (fiber-ctx-bs-end c)))
        (if (or (zerop b) (zerop e)) 0 (- e b)))))

(defun fiber-binding-stack-usage (fiber)
  "Bytes of binding stack currently in use by FIBER (BSP minus
binding-stack start).  Works for both worker and main fibers."
  (declare (type fiber fiber))
  (if (fiber-released-p fiber)
      0
      (let* ((c        (%fiber-ctx (fiber-sap fiber)))
             (bsp      (fiber-ctx-bsp c))
             (bs-start (fiber-ctx-bs-start c)))
        (if (or (zerop bsp) (zerop bs-start))
            0
            (max 0 (- bsp bs-start))))))

(defun %switch (from to values update-return-p)
  "Internal switch primitive.  Suspend FROM and resume TO with VALUES
staged on TO's value slot.  If UPDATE-RETURN-P, also set TO's
return-fiber to FROM (resume/switch semantics); otherwise leave it
alone (yield semantics -- preserves TO's prior caller chain)."
  (declare (type fiber from to)
           (type list values))
  (cond ((fiber-released-p from)
         (error 'dead-fiber-error :fiber from))
        ((fiber-released-p to)
         (error 'dead-fiber-error :fiber to)))
  (let* ((from-sap (fiber-sap from))
         (to-sap (fiber-sap to))
         (th-sap (sb-thread:current-thread-sap))
         (from-ctx (%fiber-ctx from-sap))
         (to-ctx (%fiber-ctx to-sap)))
    (check-switch from to from-ctx to-ctx)
    (if update-return-p
        (stage-return from to from-sap to-ctx values)
        (setf (fiber-value to) values))
    (setf *current-fiber* to)
    (%switch-prep from-sap to-sap)
    (swap-frames th-sap from-ctx to-ctx)
    (flip-states from-ctx to-ctx)
    (%swap-regs from-sap to-sap)
    ;; Control re-enters here when some later switch resumes from.
    (setf *current-fiber* from))
  ;; Post-resume delivery (each a no-op unless its slot is set).  Order is
  ;; deliberate: from's own staged interrupt fires first, in from's context,
  ;; and if it does it unwinds and preempts the rest -- an interrupt on the
  ;; resumed fiber outranks delivering to's escape.  to is already dead once
  ;; it has an escape, so a preempted escape is dropped, not redelivered.
  (deliver-pending from)
  (maybe-escape to)
  (maybe-escape-throw to)
  (values-list (fiber-value from)))

(defun switch-fiber (from to &rest values)
  "Suspend FROM and resume TO, delivering VALUES to the resumer as
multiple values.  Sets TO's return-fiber to FROM, so a subsequent
YIELD-FIBER inside TO will return to FROM.  Returns whatever the next
switch back to FROM delivers, also as multiple values."
  (declare (type fiber from to))
  (%switch from to values t))

(defun resume-fiber (to &rest values)
  "Suspend the current fiber and resume TO, delivering VALUES as
multiple values.  Equivalent to (SWITCH-FIBER *CURRENT-FIBER* TO
. VALUES): sets TO's return-fiber to *CURRENT-FIBER*, so YIELD-FIBER
inside TO comes back here.

  YIELD-FIBER  -- suspend self, deliver to the return fiber, preserve
                  the return fiber's own caller chain.
  RESUME-FIBER -- suspend self, deliver to a chosen fiber, make self
                  that fiber's return fiber.
  SWITCH-FIBER -- explicit FROM/TO form of RESUME-FIBER."
  (declare (type fiber to))
  (let ((from *current-fiber*))
    (unless from
      (error 'no-current-fiber-error :operation 'resume-fiber))
    (%switch from to values t)))

(defmacro with-fiber ((var function &rest make-args) &body body)
  "Create a fiber, bind it to VAR, execute BODY, release on exit."
  `(let ((,var (make-fiber ,function ,@make-args)))
     (unwind-protect (progn ,@body)
       (release-fiber ,var))))

(defun yield-fiber (&rest values)
  "Switch from the current fiber to its return fiber, delivering VALUES
as multiple values to the resumer.  Returns whatever the next
switch-in delivers (as multiple values).  Errors if the current fiber
has no recorded return fiber (never resumed)."
  (let ((self *current-fiber*))
    (unless self
      (error 'no-current-fiber-error :operation 'yield-fiber))
    (let ((target (fiber-return-fiber self)))
      (unless target
        (error "fiber ~S has no return fiber (never resumed)" self))
      (%switch self target values nil))))

(defun join-fiber (fiber)
  "Resume FIBER from the current fiber until it runs to completion,
and return its entry-function values.  FIBER must be RUNNABLE or NEW
and on the current thread."
  (declare (type fiber fiber))
  (loop with vs
        while (fiber-alive-p fiber)
        do (setf vs (multiple-value-list (resume-fiber fiber)))
        finally (return (values-list vs))))

(defun interrupt-fiber (fiber condition)
  "Stage CONDITION to be signalled in FIBER on its next resume.
Cooperative -- nothing happens until SWITCH-FIBER delivers control
into FIBER, at which point CONDITION is signalled in the fiber's own
context.  A NEW fiber is effectively cancelled (the condition fires
before its entry function runs)."
  (declare (type fiber fiber)
           (type condition condition))
  (unless (fiber-alive-p fiber)
    (error 'dead-fiber-error :fiber fiber))
  (setf (fiber-pending-condition fiber) condition)
  fiber)

(defmacro with-interrupted-fiber ((fiber condition) &body body)
  "Stage CONDITION on FIBER for delivery on its next resume, then
execute BODY.  On exit (normal or non-local), if the staged condition
has not been consumed by a resume it is cleared."
  (let ((f (gensym "FIBER"))
        (c (gensym "CONDITION")))
    `(let ((,f ,fiber)
           (,c ,condition))
       (interrupt-fiber ,f ,c)
       (unwind-protect (progn ,@body)
         (when (eq (fiber-pending-condition ,f) ,c)
           (setf (fiber-pending-condition ,f) nil))))))

(declaim (inline fiber-condition))
(defun fiber-condition (fiber)
  "The condition staged for FIBER's next resume by INTERRUPT-FIBER, or
NIL.  Cleared when FIBER consumes it on resume.  (An entry-function
escape is a separate channel, re-signaled in the resumer rather than
in FIBER, and is not visible here.)"
  (declare (type fiber fiber))
  (fiber-pending-condition fiber))

(declaim (inline fiber-pinned-p))
(defun fiber-pinned-p (fiber)
  "Return T if FIBER's pin count is greater than zero. SWITCH-FIBER
will only suspend a fiber with a pin count of zero."
  (declare (type fiber fiber))
  (plusp (fiber-pin-count fiber)))

(defmacro with-fiber-pinned (() &body body)
  "Increment the current fiber's pin count for the dynamic extent of
BODY; SWITCH-FIBER refuses to suspend a pinned fiber.  Pins nest."
  (let ((f (gensym "PINNED-FIBER")))
    `(let ((,f (or *current-fiber*
                   (error 'no-current-fiber-error
                          :operation 'with-fiber-pinned))))
       (incf (fiber-pin-count ,f))
       (unwind-protect (progn ,@body)
         (decf (fiber-pin-count ,f))))))

;;; --- Cross-thread migration ---

(defun fiber-migrate (fiber dest-thread)
  "Atomically move FIBER's ownership from its current owner thread to
DEST-THREAD.  After this returns, FIBER may be switched into only from
DEST-THREAD.

FIBER must be in state :RUNNABLE."
  (declare (type fiber fiber)
           (type sb-thread:thread dest-thread))
  (when (fiber-released-p fiber)
    (error 'dead-fiber-error :fiber fiber))
  (let ((state (%fiber-state fiber)))
    (unless (= state +fiber-runnable+)
      (error 'fiber-state-error
             :fiber fiber
             :state (fiber-state fiber)
             :expected '(:runnable))))
  (let ((dest-sap (sb-thread::thread-primitive-thread dest-thread)))
    (when (zerop dest-sap)
      (error "destination thread is not live"))
    (let ((rc (%fiber-migrate (fiber-sap fiber)
                              (sb-sys:int-sap dest-sap))))
      (case rc
        (0
         (setf (fiber-thread fiber) dest-thread)
         fiber)
        (-1
         (error 'fiber-state-error
                :fiber fiber
                :state (fiber-state fiber)
                :expected '(:runnable)))
        (-2
         (error 'fiber-error :fiber fiber))
        (t
         (error "unexpected return code ~D from fiber migration" rc))))))

)
