;;;; This file is for testing bivalent stream functionality, using
;;;; test machinery which might have side effects (e.g.  executing
;;;; DEFUN, writing files).  Note that the tests here might reach into
;;;; unexported functionality, and should not be used as a guide for
;;;; users.

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

;;; Test character decode restarts.
(with-open-file (s "bivalent-stream-test.txt" :direction :output
                 :if-exists :supersede
                 :element-type :default :external-format :utf-8)
  (write-byte 65 s)
  (write-char #\B s)
  (write-byte #xe0 s)
  (write-char #\C s))

(with-open-file (s "bivalent-stream-test.txt" :direction :input
                 :element-type :default
                 :external-format :utf-8)
  (assert (eql (read-char s nil s) #\A))
  (assert (eql (read-byte s nil s) 66))
  (assert (eql (read-byte s nil s) #xe0))
  (assert (eql (read-char s nil s) #\C)))

(delete-file "bivalent-stream-test.txt")

