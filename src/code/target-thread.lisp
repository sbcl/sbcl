(in-package "SB!VM")

;;; XXX this depends on the layout of struct thread in rather brittle
;;; ways.
#|
struct thread {
    lispobj *control_stack_start;
    lispobj *binding_stack_start;
    lispobj *alien_stack_start;
    lispobj *dynamic_values_start;
    pid_t pid;
    int tls_cookie;		/* on x86, the LDT index */
    os_context_t *interrupt_contexts[MAX_INTERRUPTS];
    struct thread *next;
};
|#

(defun current-thread-control-stack-start ()
  (let ((thread (int-sap *current-thread-struct*)))
    (sap-ref-sap thread 0)))

(defun current-thread-control-stack-end ()
  (let ((thread (int-sap *current-thread-struct*)))
    (sap+ (sap-ref-sap thread 4) -4)))

(defun current-thread-interrupt-contexts ()
  (let ((thread (int-sap *current-thread-struct*)))
    (sb!alien:sap-alien (sap-ref-sap thread (* 6 4))
			(array (* os-context-t) nil))))
