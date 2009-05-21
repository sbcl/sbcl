;;;; generic array operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB!VM")

;;; (ARRAY NIL) stuff looks the same on all platforms
(define-vop (data-vector-ref/simple-array-nil)
  (:translate data-vector-ref)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg)))
  (:arg-types simple-array-nil positive-fixnum)
  (:results (value :scs (descriptor-reg)))
  (:result-types *)
  (:ignore index value)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (error-call vop
                #!+(or x86 x86-64 ppc) 'nil-array-accessed-error
                #!-(or x86 x86-64 ppc) nil-array-accessed-error
                object)))

;;; It shouldn't be possible to fall through to here in normal user
;;; code, as the system is smart enough to deduce that there must be
;;; an error upstream, as there are no objects of type NIL that can be
;;; stored in this data vector; however, just in case, we provide this
;;; translation, so that
;;;   (LOCALLY
;;;     (DECLARE (TYPE (SIMPLE-ARRAY NIL (*)) X)
;;;              (OPTIMIZE (SPEED 3) (SAFETY 0)))
;;;     (SB-KERNEL:DATA-VECTOR-SET X 3 'FOO))
;;; signals the right kind of error.
(define-vop (data-vector-set/simple-array-nil)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (value :scs (descriptor-reg)))
  (:arg-types simple-array-nil positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:ignore index value result)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (error-call vop
                #!+(or x86 x86-64 ppc) 'nil-array-accessed-error
                #!-(or x86 x86-64 ppc) nil-array-accessed-error
                object)))

(define-vop (data-vector-ref-with-offset/simple-array-nil)
  (:translate data-vector-ref-with-offset)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg)))
  (:info offset)
  (:arg-types simple-array-nil positive-fixnum
              (:constant (integer 0 0)))
  (:results (value :scs (descriptor-reg)))
  (:result-types *)
  (:ignore index value offset)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (error-call vop
                #!+(or x86 x86-64 ppc) 'nil-array-accessed-error
                #!-(or x86 x86-64 ppc) nil-array-accessed-error
                object)))

(define-vop (data-vector-set/simple-array-nil)
  (:translate data-vector-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (value :scs (descriptor-reg)))
  (:info offset)
  (:arg-types simple-array-nil positive-fixnum *
              (:constant (integer 0 0)))
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:ignore index value result offset)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (error-call vop
                #!+(or x86 x86-64 ppc) 'nil-array-accessed-error
                #!-(or x86 x86-64 ppc) nil-array-accessed-error
                object)))

;;; FIXME: There is probably plenty of other array stuff that looks
;;; the same or similar enough to be genericized.  Do so, and move it
;;; here so that a new port doesn't need to do as much work.
