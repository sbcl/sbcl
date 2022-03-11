;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain. The
;;;; software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

;;; SB-QUEUE has been slurped in SB-CONCURRENCY.

;;; Here we just provide a reexporting stub for backwards
;;; compatibility.

(defpackage :sb-queue
  (:use :cl :sb-concurrency)
  (:export
   "DEQUEUE"
   "ENQUEUE"
   "LIST-QUEUE-CONTENTS"
   "MAKE-QUEUE"
   "QUEUE"
   "QUEUE-COUNT"
   "QUEUE-EMPTY-P"
   "QUEUE-NAME"
   "QUEUEP"))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-QUEUE")) t))
