;;;; miscellaneous VM definition noise for the RV32

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(!define-storage-bases
 (define-storage-base registers :finite :size 32)
 (define-storage-base control-stack :unbounded :size 8)
 (define-storage-base non-descriptor-stack :unbounded :size 0)

 (define-storage-base float-registers :finite :size 32)

 (define-storage-base constant :non-packed)
 (define-storage-base immediate-constant :non-packed)
 )

(!define-storage-classes
 (constant constant)
 (immediate immediate-constant)

 (control-stack control-stack)
 (any-reg registers :alternate-scs (control-stack))
 (descriptor-reg registers :alternate-scs (control-stack))
 (non-descriptor-reg registers)

 (character-stack non-descriptor-stack)
 (character-reg registers :alternate-scs (character-stack))
 (sap-stack non-descriptor-stack)
 (sap-reg registers :alternate-scs (sap-stack))
 (signed-stack non-descriptor-stack)
 (signed-reg registers :alternate-scs (signed-stack))
 (unsigned-stack non-descriptor-stack)
 (unsigned-reg registers :alternate-scs (unsigned-stack))

 (single-stack non-descriptor-stack)
 (single-reg float-registers :alternate-scs (single-stack))
 (double-stack non-descriptor-stack :element-size 2)
 (double-reg float-registers :alternate-scs (double-stack))

 (complex-single-stack non-descriptor-stack :element-size 2)
 (complex-single-reg float-registers :alternate-scs (complex-single-stack))
 (complex-double-stack non-descriptor-stack :element-size 4)
 (complex-double-reg float-registers :alternate-scs (complex-double-stack))

 (catch-block control-stack :element-size catch-block-size)
 (unwind-block control-stack :element-size unwind-block-size)
 )

(defun immediate-constant-sc (value)
  (typecase value
    ((integer #.sb-xc:most-negative-fixnum #.sb-xc:most-positive-fixnum) immediate-sc-number)))

(defun combination-implementation-style (node)
  (values :default nil))
