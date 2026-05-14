;;;; -*-  Lisp -*-
;;;;
;;;; sb-fiber FFI bindings, alien-routines, fiber struct and other
;;;; low-level stuff to glue API to runtime

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

(define-alien-routine ("sb_fiber_destroy" %fiber-destroy)
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

(define-alien-routine ("sb_fiber_prepare" %fiber-prepare)
    void
  (fiber system-area-pointer)
  (fn system-area-pointer)
  (arg system-area-pointer))

(declaim (inline fiber-c))
(defun fiber-c (sap)
  (declare (type sb-sys:system-area-pointer sap))
  (sb-alien:sap-alien sap (* sb-fiber-c)))

(declaim (inline %switch-fiber-prep))
(defun %switch-fiber-prep (from to)
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
  (pin-count 0 :type (and fixnum unsigned-byte))
  (return-fiber nil :type (or null fiber))
  (name nil :type (or null string))
  (value nil))

(defun %print-fiber (fiber stream)
  (print-unreadable-object (fiber stream :type t :identity t)
    (let* ((sap (fiber-sap fiber))
           (state (if (sb-sys:sap= sap (sb-sys:int-sap 0))
                      :dead
                      (case (sb-fiber-c-state (fiber-c sap))
                        (#.+fiber-new+      :new)
                        (#.+fiber-runnable+ :runnable)
                        (#.+fiber-running+  :running)
                        (t                  :dead))))
           (name  (fiber-name fiber)))
      (if name
          (format stream "~A ~A" name state)
          (format stream "~A" state)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:info :variable :wired-tls '*current-fiber*)
        :always-thread-local))

(defvar *current-fiber* nil
  "Fiber currently running on this thread, or NIL.")

(define-alien-callable sb-fiber-lisp-entry void
    ((arg unsigned-long))
  (let ((f *current-fiber*))
    (when (and f (fiber-function f))
      (assert (= arg (sb-sys:sap-int (fiber-sap f))))
      (let ((sb-kernel:*handler-clusters* sb-kernel::**initial-handler-clusters**)
            (sb-kernel:*restart-clusters* nil))
        (handler-case
            (let ((rv (cond ((fiber-pending-condition f)
                             (let ((c (fiber-pending-condition f)))
                               (setf (fiber-pending-condition f) nil)
                               (error c)))
                            (t (funcall (fiber-function f))))))
              (when (fiber-return-fiber f)
                (setf (fiber-value (fiber-return-fiber f)) rv)))
          (condition (c)
            (setf (fiber-pending-condition f) c)))))))

(declaim (inline %thread-slot (setf %thread-slot)))
(defun %thread-slot (th-sap slot)
  (declare (type sb-sys:system-area-pointer th-sap) (type fixnum slot))
  (sb-sys:sap-ref-word th-sap (ash slot sb-vm:word-shift)))
(defun (setf %thread-slot) (val th-sap slot)
  (declare (type sb-sys:system-area-pointer th-sap)
           (type fixnum slot val))
  (setf (sb-sys:sap-ref-word th-sap (ash slot sb-vm:word-shift)) val))

)
