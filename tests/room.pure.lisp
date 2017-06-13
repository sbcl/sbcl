;;;; tests for ROOM and related functionality

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(cl:in-package :cl-user)

(with-test (:name (sb-vm:print-allocated-objects :smoke))
  (dolist (space '(:read-only :dynamic #+immobile-space :immobile :static))
    (with-output-to-string (stream)
      (sb-vm:print-allocated-objects space :percent 10 :stream stream))))
