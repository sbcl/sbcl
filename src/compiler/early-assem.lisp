;;;; constants and types for assembly

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ASSEM")

(defvar *asmstream*)

;;; common supertype for all the different kinds of annotations
(defstruct (annotation (:constructor nil)
                        (:copier nil))
  ;; Where in the raw output stream was this annotation emitted?
  (index 0 :type index)
  ;; What position does that correspond to?
  (posn nil :type (or index null)))

(defstruct (label (:include annotation)
                   (:constructor gen-label (&optional comment))
                   (:copier nil))
  (comment)
  (usedp nil :type boolean)) ; whether it was ever used as a branch target

(defmethod print-object ((label label) stream)
  (cond ((not (boundp 'sb-c:*compilation*))
         (print-unreadable-object (label stream :type t :identity t)))
        ((or *print-escape* *print-readably*)
         (print-unreadable-object (label stream :type t)
           (prin1 (sb-c:label-id label) stream)))
        (t
         (format stream "L~D" (sb-c:label-id label)))))

;;; Not only can DEFINE-ASSEMBLY-ROUTINE not work in the target,
;;; the cross-compiler never sees a DEFUN for any of the helper functions
;;; that are called within, and therefore would issue "unknown function"
;;; warnings. So we avoid letting it see a load-time definition of the macro.
(eval-when (:compile-toplevel #-sb-xc :load-toplevel :execute)
(#-sb-xc defmacro #+sb-xc sb-xc:defmacro sb-vm::define-assembly-routine
    (name&options vars &body code)
  (multiple-value-bind (name options)
      (if (atom name&options)
          (values name&options nil)
          (values (car name&options) (cdr name&options)))
    (let ((regs (mapcar (lambda (var) (apply #'sb-c::parse-reg-spec var))
                        vars)))
      (if (member :sb-assembling sb-xc:*features*)
          (sb-c::emit-assemble name options regs code)
          (sb-c::emit-assemble-vop name options regs))))))
