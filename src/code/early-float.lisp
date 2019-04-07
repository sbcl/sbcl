;;;; This file contains the definitions of float-specific number
;;;; support (other than irrational stuff, which is in irrat.) There is
;;;; code in here that assumes there are only two float formats: IEEE
;;;; single and double. (LONG-FLOAT support has been added, but bugs
;;;; may still remain due to old code which assumes this dichotomy.)

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

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; These functions let us create floats from bits with the
;;; significand uniformly represented as an integer. This is less
;;; efficient for double floats, but is more convenient when making
;;; special values, etc.
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
  (make-double-float (dpb exp sb-vm:double-float-exponent-byte
                          (dpb (ash sig -32)
                               sb-vm:double-float-significand-byte
                               (if (zerop sign) 0 -1)))
                     (ldb (byte 32 0) sig)))
#+(and long-float x86)
(defun long-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 64) sig)
           (type (unsigned-byte 15) exp))
  (make-long-float (logior (ash sign 15) exp)
                   (ldb (byte 32 32) sig)
                   (ldb (byte 32 0) sig)))

) ; EVAL-WHEN

;;;; float parameters
(defconstant sb-xc:most-positive-single-float #.sb-xc:most-positive-single-float)
(defconstant sb-xc:most-negative-single-float #.sb-xc:most-negative-single-float)
(defconstant sb-xc:most-positive-double-float #.sb-xc:most-positive-double-float)
(defconstant sb-xc:most-negative-double-float #.sb-xc:most-negative-double-float)
(defconstant sb-xc:pi #.sb-xc:pi)

(defconstant sb-xc:least-positive-single-float (single-from-bits 0 0 1))
(defconstant sb-xc:least-positive-short-float (single-from-bits 0 0 1))
(defconstant sb-xc:least-negative-single-float (single-from-bits 1 0 1))
(defconstant sb-xc:least-negative-short-float (single-from-bits 1 0 1))
(defconstant sb-xc:least-positive-double-float (double-from-bits 0 0 1))
#-long-float
(defconstant sb-xc:least-positive-long-float (double-from-bits 0 0 1))
#+(and long-float x86)
(defconstant sb-xc:least-positive-long-float (long-from-bits 0 0 1))
(defconstant sb-xc:least-negative-double-float (double-from-bits 1 0 1))
#-long-float
(defconstant sb-xc:least-negative-long-float (double-from-bits 1 0 1))
#+(and long-float x86)
(defconstant sb-xc:least-negative-long-float (long-from-bits 1 0 1))

(defconstant sb-xc:least-positive-normalized-single-float
  (single-from-bits 0 sb-vm:single-float-normal-exponent-min 0))
(defconstant sb-xc:least-positive-normalized-short-float
  sb-xc:least-positive-normalized-single-float)
(defconstant sb-xc:least-negative-normalized-single-float
  (single-from-bits 1 sb-vm:single-float-normal-exponent-min 0))
(defconstant sb-xc:least-negative-normalized-short-float
  sb-xc:least-negative-normalized-single-float)
(defconstant sb-xc:least-positive-normalized-double-float
  (double-from-bits 0 sb-vm:double-float-normal-exponent-min 0))
#-long-float
(defconstant sb-xc:least-positive-normalized-long-float
  sb-xc:least-positive-normalized-double-float)
#+(and long-float x86)
(defconstant sb-xc:least-positive-normalized-long-float
  (long-from-bits 0 sb-vm:long-float-normal-exponent-min
                  (ash sb-vm:long-float-hidden-bit 32)))
(defconstant sb-xc:least-negative-normalized-double-float
  (double-from-bits 1 sb-vm:double-float-normal-exponent-min 0))
#-long-float
(defconstant sb-xc:least-negative-normalized-long-float
  sb-xc:least-negative-normalized-double-float)
#+(and long-float x86)
(defconstant sb-xc:least-negative-normalized-long-float
  (long-from-bits 1 sb-vm:long-float-normal-exponent-min
                  (ash sb-vm:long-float-hidden-bit 32)))

(defconstant sb-xc:most-positive-short-float sb-xc:most-positive-single-float)
(defconstant sb-xc:most-negative-short-float sb-xc:most-negative-single-float)
(defconstant sb-xc:most-positive-long-float  sb-xc:most-positive-double-float)
(defconstant sb-xc:most-negative-long-float  sb-xc:most-negative-double-float)

(defconstant single-float-positive-infinity #.(sb-impl::make-flonum :+infinity 'single-float))
(defconstant single-float-negative-infinity #.(sb-impl::make-flonum :-infinity 'single-float))
(defconstant double-float-positive-infinity #.(sb-impl::make-flonum :+infinity 'double-float))
(defconstant double-float-negative-infinity #.(sb-impl::make-flonum :-infinity 'double-float))

(defconstant short-float-positive-infinity single-float-positive-infinity)
(defconstant short-float-negative-infinity single-float-negative-infinity)
#+(not long-float)
(defconstant long-float-positive-infinity double-float-positive-infinity)
#+(and long-float x86)
(defconstant long-float-positive-infinity
  (long-from-bits 0 (1+ sb-vm:long-float-normal-exponent-max)
                  (ash sb-vm:long-float-hidden-bit 32)))
(defconstant double-float-negative-infinity
  (double-from-bits 1 (1+ sb-vm:double-float-normal-exponent-max) 0))
#+(not long-float)
(defconstant long-float-negative-infinity double-float-negative-infinity)
#+(and long-float x86)
(defconstant long-float-negative-infinity
  (long-from-bits 1 (1+ sb-vm:long-float-normal-exponent-max)
                  (ash sb-vm:long-float-hidden-bit 32)))

(defconstant sb-xc:single-float-epsilon
  (single-from-bits 0 (- sb-vm:single-float-bias
                         (1- sb-vm:single-float-digits)) 1))
(defconstant sb-xc:short-float-epsilon sb-xc:single-float-epsilon)
(defconstant sb-xc:single-float-negative-epsilon
  (single-from-bits 0 (- sb-vm:single-float-bias sb-vm:single-float-digits) 1))
(defconstant sb-xc:short-float-negative-epsilon sb-xc:single-float-negative-epsilon)
(defconstant sb-xc:double-float-epsilon
  (double-from-bits 0 (- sb-vm:double-float-bias
                         (1- sb-vm:double-float-digits)) 1))
#-long-float
(defconstant sb-xc:long-float-epsilon sb-xc:double-float-epsilon)
#+(and long-float x86)
(defconstant sb-xc:long-float-epsilon
  (long-from-bits 0 (- sb-vm:long-float-bias (1- sb-vm:long-float-digits))
                  (+ 1 (ash sb-vm:long-float-hidden-bit 32))))
(defconstant sb-xc:double-float-negative-epsilon
  (double-from-bits 0 (- sb-vm:double-float-bias sb-vm:double-float-digits) 1))
#-long-float
(defconstant sb-xc:long-float-negative-epsilon sb-xc:double-float-negative-epsilon)
#+(and long-float x86)
(defconstant sb-xc:long-float-negative-epsilon
  (long-from-bits 0 (- sb-vm:long-float-bias sb-vm:long-float-digits)
                  (+ 1 (ash sb-vm:long-float-hidden-bit 32))))

;;; Limits for floats that can be truncated into a fixnum
;;; with no loss of precision.
;;; (We don't have constants for "most-fooative-fixnum as a mumble-float")
(defconstant most-positive-fixnum-single-float
  (single-from-bits 0 (+ sb-vm:n-fixnum-bits sb-vm:single-float-bias -1)
                               (ldb (byte (1- sb-vm:single-float-digits) 0) -1)))

(defconstant most-negative-fixnum-single-float
  (single-from-bits 1 (+ sb-vm:n-fixnum-bits sb-vm:single-float-bias) 0))

(defconstant most-positive-fixnum-double-float
  (double-from-bits 0 (+ sb-vm:n-fixnum-bits sb-vm:double-float-bias -1)
                               (ldb (byte (1- sb-vm:double-float-digits) 0) -1)))

(defconstant most-negative-fixnum-double-float
  (double-from-bits 1 (+ sb-vm:n-fixnum-bits sb-vm:double-float-bias) 0))
