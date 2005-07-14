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

(in-package "SB!KERNEL")

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
   (dpb exp sb!vm:single-float-exponent-byte
        (dpb sig sb!vm:single-float-significand-byte
             (if (zerop sign) 0 -1)))))
(defun double-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 53) sig)
           (type (unsigned-byte 11) exp))
  (make-double-float (dpb exp sb!vm:double-float-exponent-byte
                          (dpb (ash sig -32)
                               sb!vm:double-float-significand-byte
                               (if (zerop sign) 0 -1)))
                     (ldb (byte 32 0) sig)))
#!+(and long-float x86)
(defun long-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 64) sig)
           (type (unsigned-byte 15) exp))
  (make-long-float (logior (ash sign 15) exp)
                   (ldb (byte 32 32) sig)
                   (ldb (byte 32 0) sig)))

) ; EVAL-WHEN

;;;; float parameters

(defconstant least-positive-single-float (single-from-bits 0 0 1))
(defconstant least-positive-short-float (single-from-bits 0 0 1))
(defconstant least-negative-single-float (single-from-bits 1 0 1))
(defconstant least-negative-short-float (single-from-bits 1 0 1))
(defconstant least-positive-double-float (double-from-bits 0 0 1))
#!-long-float
(defconstant least-positive-long-float (double-from-bits 0 0 1))
#!+(and long-float x86)
(defconstant least-positive-long-float (long-from-bits 0 0 1))
(defconstant least-negative-double-float (double-from-bits 1 0 1))
#!-long-float
(defconstant least-negative-long-float (double-from-bits 1 0 1))
#!+(and long-float x86)
(defconstant least-negative-long-float (long-from-bits 1 0 1))

(defconstant least-positive-normalized-single-float
  (single-from-bits 0 sb!vm:single-float-normal-exponent-min 0))
(defconstant least-positive-normalized-short-float
  least-positive-normalized-single-float)
(defconstant least-negative-normalized-single-float
  (single-from-bits 1 sb!vm:single-float-normal-exponent-min 0))
(defconstant least-negative-normalized-short-float
  least-negative-normalized-single-float)
(defconstant least-positive-normalized-double-float
  (double-from-bits 0 sb!vm:double-float-normal-exponent-min 0))
#!-long-float
(defconstant least-positive-normalized-long-float
  least-positive-normalized-double-float)
#!+(and long-float x86)
(defconstant least-positive-normalized-long-float
  (long-from-bits 0 sb!vm:long-float-normal-exponent-min
                  (ash sb!vm:long-float-hidden-bit 32)))
(defconstant least-negative-normalized-double-float
  (double-from-bits 1 sb!vm:double-float-normal-exponent-min 0))
#!-long-float
(defconstant least-negative-normalized-long-float
  least-negative-normalized-double-float)
#!+(and long-float x86)
(defconstant least-negative-normalized-long-float
  (long-from-bits 1 sb!vm:long-float-normal-exponent-min
                  (ash sb!vm:long-float-hidden-bit 32)))

(defconstant most-positive-single-float
  (single-from-bits 0 sb!vm:single-float-normal-exponent-max
                    (ldb sb!vm:single-float-significand-byte -1)))
(defconstant most-positive-short-float most-positive-single-float)
(defconstant most-negative-single-float
  (single-from-bits 1 sb!vm:single-float-normal-exponent-max
                    (ldb sb!vm:single-float-significand-byte -1)))
(defconstant most-negative-short-float most-negative-single-float)
(defconstant most-positive-double-float
  (double-from-bits 0 sb!vm:double-float-normal-exponent-max
                    (ldb (byte sb!vm:double-float-digits 0) -1)))

(defconstant most-positive-long-float most-positive-double-float)

(defconstant most-negative-double-float
  (double-from-bits 1 sb!vm:double-float-normal-exponent-max
                    (ldb (byte sb!vm:double-float-digits 0) -1)))
(defconstant most-negative-long-float most-negative-double-float)

;;; We don't want to do these DEFCONSTANTs at cross-compilation time,
;;; because the cross-compilation host might not support floating
;;; point infinities. Putting them inside a LET removes
;;; toplevel-formness, so that any EVAL-WHEN trickiness in the
;;; DEFCONSTANT forms is suppressed.
;;;
;;; Note that it might be worth performing a similar MAKE-LOAD-FORM
;;; trick as with -0.0 (see the UNPORTABLE-FLOAT structure).  CSR,
;;; 2004-03-09
(let ()
(defconstant single-float-positive-infinity
  (single-from-bits 0 (1+ sb!vm:single-float-normal-exponent-max) 0))
(defconstant short-float-positive-infinity
  (single-from-bits 0 (1+ sb!vm:single-float-normal-exponent-max) 0))
(defconstant single-float-negative-infinity
  (single-from-bits 1 (1+ sb!vm:single-float-normal-exponent-max) 0))
(defconstant short-float-negative-infinity
  (single-from-bits 1 (1+ sb!vm:single-float-normal-exponent-max) 0))
(defconstant double-float-positive-infinity
  (double-from-bits 0 (1+ sb!vm:double-float-normal-exponent-max) 0))
#!+(not long-float)
(defconstant long-float-positive-infinity
  (double-from-bits 0 (1+ sb!vm:double-float-normal-exponent-max) 0))
#!+(and long-float x86)
(defconstant long-float-positive-infinity
  (long-from-bits 0 (1+ sb!vm:long-float-normal-exponent-max)
                  (ash sb!vm:long-float-hidden-bit 32)))
(defconstant double-float-negative-infinity
  (double-from-bits 1 (1+ sb!vm:double-float-normal-exponent-max) 0))
#!+(not long-float)
(defconstant long-float-negative-infinity
  (double-from-bits 1 (1+ sb!vm:double-float-normal-exponent-max) 0))
#!+(and long-float x86)
(defconstant long-float-negative-infinity
  (long-from-bits 1 (1+ sb!vm:long-float-normal-exponent-max)
                  (ash sb!vm:long-float-hidden-bit 32)))
) ; LET-to-suppress-possible-EVAL-WHENs

(defconstant single-float-epsilon
  (single-from-bits 0 (- sb!vm:single-float-bias
                         (1- sb!vm:single-float-digits)) 1))
(defconstant short-float-epsilon single-float-epsilon)
(defconstant single-float-negative-epsilon
  (single-from-bits 0 (- sb!vm:single-float-bias sb!vm:single-float-digits) 1))
(defconstant short-float-negative-epsilon single-float-negative-epsilon)
(defconstant double-float-epsilon
  (double-from-bits 0 (- sb!vm:double-float-bias
                         (1- sb!vm:double-float-digits)) 1))
#!-long-float
(defconstant long-float-epsilon double-float-epsilon)
#!+(and long-float x86)
(defconstant long-float-epsilon
  (long-from-bits 0 (- sb!vm:long-float-bias (1- sb!vm:long-float-digits))
                  (+ 1 (ash sb!vm:long-float-hidden-bit 32))))
(defconstant double-float-negative-epsilon
  (double-from-bits 0 (- sb!vm:double-float-bias sb!vm:double-float-digits) 1))
#!-long-float
(defconstant long-float-negative-epsilon double-float-negative-epsilon)
#!+(and long-float x86)
(defconstant long-float-negative-epsilon
  (long-from-bits 0 (- sb!vm:long-float-bias sb!vm:long-float-digits)
                  (+ 1 (ash sb!vm:long-float-hidden-bit 32))))
