;;;; Condition support in target lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(fmakunbound 'install-condition-slot-reader)
(fmakunbound 'install-condition-slot-writer)
(defun install-condition-slot-reader (name condition slot-name)
  (eval `(defmethod ,name ((.condition. ,condition))
           (condition-reader-function .condition. ',slot-name))))
(defun install-condition-slot-writer (name condition slot-name)
  (eval `(defmethod ,name (new-value (.condition. ,condition))
           (condition-writer-function .condition. new-value ',slot-name))))
