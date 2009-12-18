;;; -*-  Lisp -*-

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defpackage :sb-queue-system
  (:use :asdf :cl))

(in-package :sb-queue-system)

(defsystem :sb-queue
  :components ((:file "queue")))

(defsystem :sb-queue-tests
  :depends-on (:sb-queue :sb-rt)
  :components ((:file "test-queue")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-queue))))
  (provide 'sb-queue))

(defmethod perform ((o test-op) (c (eql (find-system :sb-queue))))
  (operate 'load-op :sb-queue-tests)
  (operate 'test-op :sb-queue-tests))

(defmethod perform ((op test-op) (com (eql (find-system :sb-queue-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "~S failed" 'test-op)))
