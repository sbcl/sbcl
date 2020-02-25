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

(defpackage :sb-concurrency
  (:use :cl :sb-thread :sb-int :sb-ext :sb-sys)
  (:export
   ;; MAILBOX
   "LIST-MAILBOX-MESSAGES"
   "MAILBOX"
   "MAILBOX-COUNT"
   "MAILBOX-EMPTY-P"
   "MAILBOX-NAME"
   "MAILBOXP"
   "MAKE-MAILBOX"
   "RECEIVE-MESSAGE"
   "RECEIVE-MESSAGE-NO-HANG"
   "RECEIVE-PENDING-MESSAGES"
   "SEND-MESSAGE"

   ;; QUEUE
   "DEQUEUE"
   "ENQUEUE"
   "LIST-QUEUE-CONTENTS"
   "MAKE-QUEUE"
   "QUEUE"
   "QUEUE-COUNT"
   "QUEUE-EMPTY-P"
   "QUEUE-NAME"
   "QUEUEP"

   ;; GATE
   "CLOSE-GATE"
   "GATE"
   "GATE-NAME"
   "GATE-OPEN-P"
   "GATEP"
   "MAKE-GATE"
   "OPEN-GATE"
   "WAIT-ON-GATE"

   ;; FRLOCK
   "MAKE-FRLOCK"
   "FRLOCK"
   "FRLOCK-NAME"
   "FRLOCK-WRITE"
   "FRLOCK-READ"
   "FRLOCK-READ-BEGIN"
   "FRLOCK-READ-END"
   "GRAB-FRLOCK-WRITE-LOCK"
   "RELEASE-FRLOCK-WRITE-LOCK"
   ))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-CONCURRENCY")) t))
