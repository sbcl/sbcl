;;;; floating point support for the RV32

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-move-fun (load-single 1) (vop x y)
  ((single-stack) (single-reg)))
(define-move-fun (store-single 1) (vop x y)
  ((single-reg) (single-stack)))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg)))
(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack)))

(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg)))
(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack)))

(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg)))
(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack)))
