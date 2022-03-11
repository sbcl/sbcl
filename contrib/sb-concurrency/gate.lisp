;;;; -*-  Lisp -*-
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :sb-concurrency)

;;;; FIXME: On Linux a direct futex-based implementation would be more
;;;; efficient.

(defstruct (gate (:constructor %make-gate)
                 (:copier nil)
                 (:predicate gatep))
  "GATE type. Gates are synchronization constructs suitable for making
multiple threads wait for single event before proceeding.

Use WAIT-ON-GATE to wait for a gate to open, OPEN-GATE to open one,
and CLOSE-GATE to close an open gate. GATE-OPEN-P can be used to test
the state of a gate without blocking."
  (mutex (missing-arg) :type mutex)
  (queue (missing-arg) :type waitqueue)
  (state :closed :type (member :open :closed))
  (name  nil :type (or null simple-string)))
(declaim (sb-ext:freeze-type gate))

(setf (documentation 'gatep 'function)
      "Returns true if the argument is a GATE."
      (documentation 'gate-name 'function)
      "Name of a GATE. SETFable.")

(defmethod print-object ((gate gate) stream)
  (print-unreadable-object (gate stream :type t :identity t)
    (format stream "~@[~S ~]~((~A)~)"
            (gate-name gate)
            (gate-state gate))))

(defun make-gate (&key name open)
  "Makes a new gate. Gate will be initially open if OPEN is true, and closed if OPEN
is NIL (the default.) NAME, if provided, is the name of the gate, used when printing
the gate."
  (flet ((generate-name (thing)
           (when name
             (format nil "gate ~S's ~A" name thing))))
    (%make-gate
     :name name
     :mutex (make-mutex :name (generate-name "lock"))
     :queue (make-waitqueue :name (generate-name "condition variable"))
     :state (if open :open :closed))))

(defun open-gate (gate)
  "Opens GATE. Returns T if the gate was previously closed, and NIL
if the gate was already open."
  (declare (gate gate))
  (let (closed)
    (with-mutex ((gate-mutex gate))
      (sb-sys:without-interrupts
        (setf closed (eq :closed (gate-state gate))
              (gate-state gate) :open)
        (condition-broadcast (gate-queue gate))))
    closed))

(defun close-gate (gate)
  "Closes GATE. Returns T if the gate was previously open, and NIL
if the gate was already closed."
  (declare (gate gate))
  (let (open)
    (with-mutex ((gate-mutex gate))
      (setf open (eq :open (gate-state gate))
            (gate-state gate) :closed))
    open))

(defun wait-on-gate (gate &key timeout)
  "Waits for GATE to open, or TIMEOUT seconds to pass. Returns T
if the gate was opened in time, and NIL otherwise."
  (declare (gate gate))
  (with-mutex ((gate-mutex gate))
    (loop until (eq :open (gate-state gate))
          do (or (condition-wait (gate-queue gate) (gate-mutex gate)
                              :timeout timeout)
                 (return-from wait-on-gate nil))))
  t)

(defun gate-open-p (gate)
  "Returns true if GATE is open."
  (declare (gate gate))
  (eq :open (gate-state gate)))
