;;;; allocation VOPs for the RV32

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop (list-or-list*)
  (:args (things :more t))
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:generator 0))

(define-vop (list list-or-list*)
  (:variant t))
(define-vop (list* list-or-list*)
  (:variant t))

(define-vop (make-closure)
  (:args (function :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:ignore label)
  (:results (result :scs (descriptor-reg)))
  (:generator 10))

(define-vop (make-value-cell)
  (:args (value :scs (descriptor-reg any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:info stack-allocate-p)
  (:generator 10))

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 4))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:generator 6))
