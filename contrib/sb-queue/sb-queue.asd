;;; -*-  Lisp -*-

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :cl-user)

(asdf:defsystem :sb-queue
  :depends-on (:sb-concurrency)
  :components ((:file "package")))

(defmethod asdf:perform :after ((o asdf:load-op)
                                (c (eql (asdf:find-system :sb-queue))))
  (provide 'sb-queue))


(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :sb-queue))))
  :pass)