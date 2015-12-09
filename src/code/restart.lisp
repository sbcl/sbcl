;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; This defstruct should appear before any use of WITH-CONDITION-RESTARTS
;;;; so that the slot accessors are transformed.

(defstruct (restart (:constructor make-restart
                        ;; Having TEST-FUNCTION at the end allows
                        ;; to not replicate its default value in RESTART-BIND.
                        (name function
                         &optional report-function
                                   interactive-function
                                   test-function))
                    (:copier nil) (:predicate nil))
  (name (missing-arg) :type symbol :read-only t)
  (function (missing-arg) :type function :read-only t)
  (report-function nil :type (or null function) :read-only t)
  (interactive-function nil :type (or null function) :read-only t)
  (test-function (lambda (cond) (declare (ignore cond)) t) :type function :read-only t)
  ;; the list of conditions which are currently associated to the
  ;; restart. maintained by WITH-CONDITION-RESTARTS in a neither
  ;; thread- nor interrupt-safe way. This should not be a problem
  ;; however, since safe uses of restarts have to assume dynamic
  ;; extent.
  (associated-conditions '() :type list))

#!-sb-fluid (declaim (freeze-type restart))
