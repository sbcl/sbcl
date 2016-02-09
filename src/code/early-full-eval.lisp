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

(sb!int:!defparameter *eval-level* -1)
(sb!int:!defparameter *eval-verbose* nil)

;; !defstruct-with-alternate-metaclass is unslammable and the
;; RECOMPILE restart doesn't work on it.  This is the main reason why
;; this stuff is split out into its own file.  Also, it lets the
;; INTERPRETED-FUNCTION type be declared before it is used in
;; compiler/main and code/deftypes-for-target.
(sb!kernel::!defstruct-with-alternate-metaclass
 interpreted-function
 ;; DEBUG-NAME and DEBUG-LAMBDA-LIST are initially a copies of the proper
 ;; ones, but is analogous to SIMPLE-FUN-NAME and ARGLIST in the sense that it
 ;; is they are there only for debugging, and do not affect behaviour of the
 ;; function -- so DEFMACRO can set them to more informative values.
 :slot-names (name debug-name lambda-list debug-lambda-list env
                   declarations documentation body source-location)
 :boa-constructor %make-interpreted-function
 :superclass-name function
 :metaclass-name static-classoid
 :metaclass-constructor make-static-classoid
 :dd-type funcallable-structure
 :runtime-type-checks-p nil)

;; INTERPRETED-FUNCTION can not subclassed at runtime.
;; For one, DEFSTRUCT-WITH-ALTERNATE-METACLASS does not exist in the target,
;; and DEFSTRUCT would have to allow SB-KERNEL:FUNCALLABLE-STRUCTURE as
;; the :TYPE option to avoid mismatch, which it does not; nor is there any
;; way to allow it with DEFCLASS.  But, KLUDGE - loading the cross-compiler
;; seals the class before the run of the cross-compiler gets to doing the declaim
;; because 'target-misc' is cross-compiled before 'early-full-eval' is.
(declaim (freeze-type interpreted-function))

(defun make-interpreted-function
      (&key name lambda-list env declarations documentation body source-location
            (debug-lambda-list lambda-list))
    (let ((function (%make-interpreted-function
                     name name lambda-list debug-lambda-list env
                     declarations documentation body source-location)))
      (setf (funcallable-instance-fun function)
            #'(lambda (&rest args)
                (interpreted-apply function args)))
      function))

(defun interpreted-function-p (function)
  (typep function 'interpreted-function))

(defmethod print-object ((obj interpreted-function) stream)
  (print-unreadable-object (obj stream
                            :identity (not (interpreted-function-name obj)))
    (format stream "~A ~A" '#:interpreted-function
            (interpreted-function-name obj))))
