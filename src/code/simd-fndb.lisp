;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(deftype 128-element ()
  #+x86-64 '(simd-pack (unsigned-byte 32))
  #+arm64 'complex-double-float)

(defknown vector-ref-128 (t index) 128-element
    (flushable))

(defknown (setf vector-ref-128) (128-element t index) (values)
  ())

(defknown (simd-nreverse8 simd-nreverse32) (vector (simple-array * (*)) fixnum fixnum)
    vector
    (sb-c::fixed-args))

(defknown (simd-reverse8 simd-reverse32) ((simple-array * (*)) (simple-array * (*)) fixnum fixnum)
    (simple-array * (*))
    (sb-c::fixed-args))

(defknown (simd-cmp-8-8 simd-cmp-8-32 simd-cmp-32-32 simd-base-string-equal
           simd-base-character-string-equal)
    ((simple-array * (*)) (simple-array * (*)) fixnum)
    (boolean)
    (sb-c::no-verify-arg-count))

(defknown (simd-position8 simd-position8-from-end
           simd-position32 simd-position32-from-end)
    (fixnum (simple-array * (*)) fixnum fixnum)
    (or (mod #.(1- array-dimension-limit)) null)
    (sb-c::no-verify-arg-count))
