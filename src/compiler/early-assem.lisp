;;;; constants and types for assembly

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ASSEM")

;;; FIXME: It might make sense to use SB!VM:BYTE-FOO values here
;;; instead of the various ASSEMBLY-UNIT-FOO things, and then define a
;;; BYTE type. One problem: BYTE is exported from the CL package, so
;;; ANSI says that we're not supposed to be attaching any new meanings
;;; to it. Perhaps rename SB!VM:BYTE-FOO to SB!VM:VMBYTE-FOO or
;;; SB!VM:VM-BYTE-FOO, and then define the SB!VM:VMBYTE or
;;; SB!VM:VM-BYTE types?
;;;
;;; If this was done, some of this file could go away, and the rest
;;; could probably be merged back into assem.lisp. (This file was
;;; created simply in order to move the ASSEMBLY-UNIT-related
;;; definitions before compiler/generic/core.lisp in the build
;;; sequence.)

;;; ASSEMBLY-UNIT-BITS -- the number of bits in the minimum assembly
;;; unit, (also referred to as a ``byte''). Hopefully, different
;;; instruction sets won't require changing this.
(def!constant assembly-unit-bits 8)
(def!constant assembly-unit-mask (1- (ash 1 assembly-unit-bits)))

(def!type assembly-unit ()
  `(unsigned-byte ,assembly-unit-bits))

;;; Some functions which accept assembly units can meaningfully accept
;;; signed values with the same number of bits and silently munge them
;;; into appropriate unsigned values. (This is handy behavior e.g.
;;; when assembling branch instructions on the X86.)
(def!type possibly-signed-assembly-unit ()
  `(or assembly-unit
       (signed-byte ,assembly-unit-bits)))

;;; the maximum alignment we can guarantee given the object format. If
;;; the loader only loads objects 8-byte aligned, we can't do any
;;; better then that ourselves.
(def!constant max-alignment sb!vm:n-lowtag-bits)

(def!type alignment ()
  `(integer 0 ,max-alignment))
