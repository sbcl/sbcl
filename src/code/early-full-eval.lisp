;;;; An interpreting EVAL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!EVAL")

(defparameter *eval-level* -1)
(defparameter *eval-calls* 0)
(defparameter *eval-verbose* nil)

(defun !full-eval-cold-init ()
  (setf *eval-level* -1
        *eval-calls* 0
        *eval-verbose* nil
        *evaluator-mode* :compile))

;; !defstruct-with-alternate-metaclass is unslammable and the
;; RECOMPILE restart doesn't work on it.  This is the main reason why
;; this stuff is split out into its own file.  Also, it lets the
;; INTERPRETED-FUNCTION type be declared before it is used in
;; compiler/main.
(sb!kernel::!defstruct-with-alternate-metaclass
 interpreted-function
 :slot-names (name lambda-list env declarations documentation body source-location)
 :boa-constructor %make-interpreted-function
 :superclass-name function
 :metaclass-name static-classoid
 :metaclass-constructor make-static-classoid
 :dd-type funcallable-structure
 :runtime-type-checks-p nil)

(defun make-interpreted-function
    (&key name lambda-list env declarations documentation body source-location)
  (let ((function (%make-interpreted-function
                   name lambda-list env declarations documentation body
                   source-location)))
    (setf (sb!kernel:funcallable-instance-fun function)
          #'(lambda (&rest args)
              (interpreted-apply function args)))
    function))

(defun interpreted-function-p (function)
  (typep function 'interpreted-function))

(sb!int:def!method print-object ((obj interpreted-function) stream)
  (print-unreadable-object (obj stream
                            :identity (not (interpreted-function-name obj)))
    (format stream "~A ~A" '#:interpreted-function
            (interpreted-function-name obj))))
