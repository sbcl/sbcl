;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;;; utilities
;;; Don't need to define it in the host in both passes
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
;;; These functions let us create floats from bits with the
;;; significand uniformly represented as an integer. This is less
;;; efficient for double floats, but is more convenient when making
;;; special values, etc.
(declaim (inline single-from-bits double-from-bits))

(defun single-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 24) sig)
           (type (unsigned-byte 8) exp))
  (make-single-float
   (dpb exp sb-vm:single-float-exponent-byte
        (dpb sig sb-vm:single-float-significand-byte
             (if (zerop sign) 0 -1)))))
(defun double-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 53) sig)
           (type (unsigned-byte 11) exp))
  #-64-bit
  (make-double-float (dpb exp sb-vm:double-float-hi-exponent-byte
                          (dpb (ash sig -32)
                               sb-vm:double-float-hi-significand-byte
                               (if (zerop sign) 0 -1)))
                     (ldb (byte 32 0) sig))
  #+64-bit
  (%make-double-float
   (dpb exp sb-vm:double-float-exponent-byte
        (dpb sig sb-vm:double-float-significand-byte
             (if (zerop sign) 0 -1)))))
#+(and long-float x86)
(defun long-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 64) sig)
           (type (unsigned-byte 15) exp))
  (make-long-float (logior (ash sign 15) exp)
                   (ldb (byte 32 32) sig)
                   (ldb (byte 32 0) sig)))

) ; EVAL-WHEN
