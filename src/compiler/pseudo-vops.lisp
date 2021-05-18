;;;; This file contains definitions of VOPs used as internal markers by
;;;; the compiler. Since they don't emit any code, they should be
;;;; portable.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; This notes the place at which the environment is properly
;;; initialized, for debug-info purposes.
(define-vop (note-environment-start)
  (:info start-lab)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (emit-label start-lab)
    (note-debug-location vop start-lab :non-local-entry)))

;;; Call a move function. Used for register save/restore and spilling.
(define-vop (move-operand)
  (:args (x))
  (:results (y))
  (:info name)
  (:vop-var vop)
  (:generator 0
    (funcall (symbol-function name) vop x y)))

;;; Emit a label for sub-block (or sub-node) reference.
(define-vop (emit-label)
  (:info the-label)
  (:vop-var vop)
  (:generator 0
     (emit-label the-label)))

;;; These guys use a single instruction to record a coverage mark
;;; and (for better or worse) delay insertion of instrumentation until
;;; assembling, just prior to which we try to combine sequences of marking
;;; instructions that have no control flow and which record the same
;;; path as hit. "compresion" should be performed in IR2 instead
;;; so that all the architectures can benefit from it.
#+(or x86 x86-64)
(define-vop (mark-covered)
  (:info path)
  (:generator 0
    (sb-assem:inst* 'sb-assem:.coverage-mark path)))

;;; SPLAT is always a no-op for architectures other than x86-64
;;; because:
;;; - the heap is zero-filled, and only value that can be
;;;   splatted is zero which would be redundant.
;;; - stack-allocation vops for precise GC have to zero-fill at the
;;;   time of allocation, while inside the pseudo-atomic,
;;;   so zero-filling would be redundant.
;;; Those architectures which require the initializing inside the
;;; pseudo-atomic would benefit from a way to advise the allocator
;;; to fill with a non-default value.
#-x86-64
(define-vop ()
  (:policy :fast-safe)
  (:translate sb-vm::splat)
  (:args (vector :scs (sb-vm::descriptor-reg))
         (words :scs (sb-vm::unsigned-reg sb-vm::immediate)))
  (:info value)
  (:arg-types * sb-vm::positive-fixnum (:constant t))
  (:results (result :scs (sb-vm::descriptor-reg)))
  (:ignore words value)
  (:generator 1 (move result vector)))
