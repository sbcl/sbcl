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
