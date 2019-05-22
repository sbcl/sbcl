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

;;; FIXME: It might make sense to use SB-VM:BYTE-FOO values here
;;; instead of the various ASSEMBLY-UNIT-FOO things, and then define a
;;; BYTE type. One problem: BYTE is exported from the CL package, so
;;; ANSI says that we're not supposed to be attaching any new meanings
;;; to it. Perhaps rename SB-VM:BYTE-FOO to SB-VM:VMBYTE-FOO or
;;; SB-VM:VM-BYTE-FOO, and then define the SB-VM:VMBYTE or
;;; SB-VM:VM-BYTE types?
;;;
;;; If this was done, some of this file could go away, and the rest
;;; could probably be merged back into assem.lisp. (This file was
;;; created simply in order to move the ASSEMBLY-UNIT-related
;;; definitions before compiler/generic/core.lisp in the build
;;; sequence.)

;;; ASSEMBLY-UNIT-BITS -- the number of bits in the minimum assembly
;;; unit, (also referred to as a ``byte''). Hopefully, different
;;; instruction sets won't require changing this.
(defconstant assembly-unit-bits 8)
(defconstant assembly-unit-mask (1- (ash 1 assembly-unit-bits)))

(deftype assembly-unit ()
  `(unsigned-byte ,assembly-unit-bits))

;;; Some functions which accept assembly units can meaningfully accept
;;; signed values with the same number of bits and silently munge them
;;; into appropriate unsigned values. (This is handy behavior e.g.
;;; when assembling branch instructions on the X86.)
(deftype possibly-signed-assembly-unit ()
  `(or assembly-unit
       (signed-byte ,assembly-unit-bits)))

;;; the maximum alignment we can guarantee given the object format. If
;;; the loader only loads objects 8-byte aligned, we can't do any
;;; better than that ourselves.
(defconstant max-alignment 5)

(deftype alignment ()
  `(integer 0 ,max-alignment))

(defvar *asmstream*)

;;; common supertype for all the different kinds of annotations
(defstruct (annotation (:constructor nil)
                        (:copier nil))
  ;; Where in the raw output stream was this annotation emitted?
  (index 0 :type index)
  ;; What position does that correspond to?
  (posn nil :type (or index null)))

(defstruct (label (:include annotation)
                   (:constructor gen-label ())
                   (:copier nil))
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
      (declare (special sb-c::*emit-assembly-code-not-vops-p*))
      (if sb-c::*emit-assembly-code-not-vops-p*
          (sb-c::emit-assemble name options regs code)
          (sb-c::emit-assemble-vop name options regs))))))
