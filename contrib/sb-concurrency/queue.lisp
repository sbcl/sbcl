;;;; Written by James M. Lawrence for SBCL.
;;;; API and docstrings by Nikodemus Siivola.
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain. The
;;;; software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

;;; Singly-linked queue with compare-and-swap operations.
;;;
;;; The following invariants hold except during updates:
;;;
;;;   (car (queue-head queue)) == +dummy+
;;;
;;;   (cdr (queue-tail queue)) == nil
;;;
;;;   If the queue is empty, (queue-head queue) == (queue-tail queue).
;;;
;;;   If the queue is non-empty, (cadr (queue-head queue)) is the next
;;;   value to be dequeued and (car (queue-tail queue)) is the most
;;;   recently enqueued value.
;;;
;;; The CDR of a discarded node is set to +DEAD-END+. This flag must
;;; be checked at each traversal.

(in-package :sb-concurrency)

(defconstant +dummy+ '.dummy.)
(defconstant +dead-end+ '.dead-end.)

(declaim (inline %make-queue))
(defstruct (queue (:constructor %make-queue (head tail name))
                  (:copier nil)
                  (:predicate queuep))
  "Lock-free thread safe FIFO queue.

Use ENQUEUE to add objects to the queue, and DEQUEUE to remove them."
  (head (error "No HEAD.") :type cons)
  (tail (error "No TAIL.") :type cons)
  (name nil))
(declaim (sb-ext:freeze-type queue))

(setf (documentation 'queuep 'function)
      "Returns true if argument is a QUEUE, NIL otherwise."
      (documentation 'queue-name 'function)
      "Name of a QUEUE. Can be assigned to using SETF. Queue names
can be arbitrary printable objects, and need not be unique.")

(declaim (ftype (sfunction (&key (:name t)
                                 (:initial-contents sequence)) queue)
                make-queue))
(defun make-queue (&key name initial-contents)
  "Returns a new QUEUE with NAME and contents of the INITIAL-CONTENTS
sequence enqueued."
  (let* ((dummy (cons +dummy+ nil))
         (queue (%make-queue dummy dummy name)))
    (flet ((enc-1 (x)
             (enqueue x queue)))
      (declare (dynamic-extent #'enc-1))
      (map nil #'enc-1 initial-contents))
    queue))


(declaim (ftype (sfunction (t queue) t) enqueue))
(defun enqueue (value queue)
  "Adds VALUE to the end of QUEUE. Returns VALUE."
  ;; Attempt CAS, repeat upon failure. Upon success update QUEUE-TAIL.
  (declare (optimize speed))
  (let ((new (cons value nil)))
    (loop (when (eq nil (sb-ext:compare-and-swap (cdr (queue-tail queue))
                                                 nil new))
            (setf (queue-tail queue) new)
            (return value)))))

(declaim (ftype (sfunction (queue) (values t boolean)) dequeue))
(defun dequeue (queue)
  "Retrieves the oldest value in QUEUE and returns it as the primary value,
and T as secondary value. If the queue is empty, returns NIL as both primary
and secondary value."
  ;; Attempt to CAS QUEUE-HEAD with the next node, repeat upon
  ;; failure. Upon success, clear the discarded node and set the CAR
  ;; of QUEUE-HEAD to +DUMMY+.
  (declare (optimize speed))
  (loop (let* ((head (queue-head queue))
               (next (cdr head)))
          ;; NEXT could be +DEAD-END+, whereupon we try again.
          (typecase next
            (null (return (values nil nil)))
            (cons (when (eq head (sb-ext:compare-and-swap (queue-head queue)
                                                          head next))
                    (let ((value (car next)))
                      ;; Clear the CDR, otherwise the conservative GC could
                      ;; hoard long lists. (car head) is always +dummy+.
                      (setf (cdr head) +dead-end+
                            (car next) +dummy+)
                      (return (values value t)))))))))

(defun try-walk-queue (fun queue)
  ;; This isn't /quite/ as bad as it looks. We're in danger of needing
  ;; to restart only as long as we're close to the head of the queue.
  (let ((node (queue-head queue)))
    (loop
       (let ((value (car node)))
         (unless (eq value +dummy+)
           (funcall fun value)))
       (setf node (cdr node))
       (cond ((eq node +dead-end+)
              (return nil))
             ((null node)
              (return t))))))

(declaim (ftype (sfunction (queue) list) list-queue-contents))
(defun list-queue-contents (queue)
  "Returns the contents of QUEUE as a list without removing them from the
QUEUE. Mainly useful for manual examination of queue state, as the list may be
out of date by the time it is returned, and concurrent dequeue operations may
in the worse case force the queue-traversal to be restarted several times."
  (tagbody
   :retry
     (collect ((result))
       (unless (try-walk-queue (lambda (elem) (result elem)) queue)
         (go :retry))
       (return-from list-queue-contents (result)))))

(declaim (ftype (sfunction (queue) unsigned-byte) queue-count))
(defun queue-count (queue)
  "Returns the number of objects in QUEUE. Mainly useful for manual
examination of queue state, and in PRINT-OBJECT methods: inefficient as it
must walk the entire queue."
  (tagbody
   :retry
     (let ((count 0))
       (unless (try-walk-queue (lambda (elem)
                                 (declare (ignore elem))
                                 (incf count))
                               queue)
         (go :retry))
       (return-from queue-count count))))

(declaim (ftype (sfunction (queue) boolean) queue-empty-p))
(defun queue-empty-p (queue)
  "Returns T if QUEUE is empty, NIL otherwise."
  (null (cdr (queue-head queue))))

;;; Experimental support for compiling in the background.
;;; The use-case is that you have some functions which you'll need later,
;;; but want to pass them around now as compiled-functions without waiting
;;; for COMPILE. If the timing is right, the compiler will be done by the time
;;; of the call to such functions, but if not, that's OK - it just works.
#+sb-thread
(progn
  (define-load-time-global *compilation-queue* (make-queue :name "compiler"))

  (defun run-background-compile (&aux compiled)
    (loop
     (let ((item (dequeue *compilation-queue*)))
       (unless item (return compiled))
       (setq compiled t)
       (let ((fin (elt (the (simple-vector 3) item) 0))
             (lexpr (elt item 1)))
         (multiple-value-bind (compiled-function warnings errors) (compile nil lexpr)
           (declare (ignore warnings))
           ;; It's OK for a closure's raw addr slot to point directly to an address
           ;; within a code blob, but I'm not sure if it's legal in a funinstance.
           ;; Probably need to tweak the GC to allow it. Then we would bypass
           ;; the embedded trampoline for anonymous call; the caller would jump
           ;; directly to where the call is intended to end up.
           (setf (sb-kernel:%funcallable-instance-fun fin)
                 (if errors
                     (lambda (&rest args)
                       (declare (ignore args))
                       (error "Compiling ~S failed" fin))
                     (lambda (&rest args)
                       (apply compiled-function args))))
           (open-gate (elt item 2)))))))

  (setq sb-impl::*bg-compiler-function*
        (lambda ()
          (let ((result1 (sb-c::default-compiler-worker))
                (result2 (run-background-compile)))
            (or result1 result2))))

  (defun promise-compile (lexpr)
    ;;   Dynamic      Immobile
    ;;   -------      --------
    ;;   header       header+layout
    ;;   trampoline   trampoline -----\
    ;;   layout       machine code   <--
    ;;   impl-fun     machine code
    ;;   unused       impl-fun
    ;;   unused
    ;; The constant 1 here is ok for #+ or #- immobile-space.
    (let ((fin (sb-kernel:%make-funcallable-instance 1))
          (gate (make-gate)))
      (sb-kernel:%set-fun-layout fin (sb-kernel:find-layout 'function))
      (sb-vm::write-funinstance-prologue fin)
      (setf (sb-kernel:%funcallable-instance-fun fin)
            (lambda (&rest args)
              (wait-on-gate gate)
              (apply fin args)))
      (enqueue (vector fin lexpr gate) *compilation-queue*)
      (sb-impl::finalizer-thread-notify 0)
      fin)))
