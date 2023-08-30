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

(declaim (maybe-inline float-denormalized-p float-infinity-p float-nan-p
                       float-trapping-nan-p))

(defmacro sfloat-bits-subnormalp (bits)
  `(zerop (ldb sb-vm:single-float-exponent-byte ,bits)))
#-64-bit
(defmacro dfloat-high-bits-subnormalp (bits)
  `(zerop (ldb sb-vm:double-float-hi-exponent-byte ,bits)))
#+64-bit
(defmacro dfloat-bits-subnormalp (bits)
  `(zerop (ldb sb-vm:double-float-exponent-byte ,bits)))

(defun float-denormalized-p (x)
  "Return true if the float X is denormalized."
  (declare (explicit-check))
  (number-dispatch ((x float))
    ((single-float)
     #+64-bit
     (let ((bits (single-float-bits x)))
       (and (ldb-test (byte 31 0) bits) ; is nonzero (disregard the sign bit)
            (sfloat-bits-subnormalp bits)))
     #-64-bit
     (and (zerop (ldb sb-vm:single-float-exponent-byte (single-float-bits x)))
          (not (zerop x))))
    ((double-float)
     #+64-bit
     (let ((bits (double-float-bits x)))
       ;; is nonzero after shifting out the sign bit
       (and (not (zerop (logand (ash bits 1) most-positive-word)))
            (dfloat-bits-subnormalp bits)))
     #-64-bit
     (and (zerop (ldb sb-vm:double-float-hi-exponent-byte
                      (double-float-high-bits x)))
          (not (zerop x))))
    #+(and long-float x86)
    ((long-float)
     (and (zerop (ldb sb-vm:long-float-exponent-byte (long-float-exp-bits x)))
          (not (zerop x))))))

(defmacro float-inf-or-nan-test (var single double #+(and long-float x86) long)
  `(number-dispatch ((,var float))
     ((single-float)
      (let ((bits (single-float-bits ,var)))
        (and (> (ldb sb-vm:single-float-exponent-byte bits)
                sb-vm:single-float-normal-exponent-max)
             ,single)))
     ((double-float)
      #+64-bit
      (let ((bits (double-float-bits ,var)))
        (and (> (ldb sb-vm:double-float-exponent-byte bits)
                sb-vm:double-float-normal-exponent-max)
             ,double))
      #-64-bit
      (let ((hi (double-float-high-bits ,var))
            (lo (double-float-low-bits ,var)))
        (declare (ignorable lo))
        (and (> (ldb sb-vm:double-float-hi-exponent-byte hi)
                sb-vm:double-float-normal-exponent-max)
             ,double)))
     #+(and long-float x86)
     ((long-float)
      (let ((exp (long-float-exp-bits ,var))
            (hi (long-float-high-bits ,var))
            (lo (long-float-low-bits ,var)))
        (declare (ignorable lo))
        (and (> (ldb sb-vm:long-float-exponent-byte exp)
                sb-vm:long-float-normal-exponent-max)
             ,long)))))

;; Infinities and NANs have the maximum exponent
(defun float-infinity-or-nan-p (x)
  (float-inf-or-nan-test x t t #+(and long-float x86) t))

;; Infinity has 0 for the significand
(defun float-infinity-p (x)
  "Return true if the float X is an infinity (+ or -)."
  (float-inf-or-nan-test
   x
   (zerop (ldb sb-vm:single-float-significand-byte bits))

   #+64-bit (zerop (ldb sb-vm:double-float-significand-byte bits))
   #-64-bit (zerop (logior (ldb sb-vm:double-float-hi-significand-byte hi) lo))

   #+(and long-float x86)
   (and (zerop (ldb sb-vm:long-float-significand-byte hi))
        (zerop lo))))

;; NaNs have nonzero for the significand
(defun float-nan-p (x)
  "Return true if the float X is a NaN (Not a Number)."
  (float-inf-or-nan-test
   x
   (not (zerop (ldb sb-vm:single-float-significand-byte bits)))

   #+64-bit (not (zerop (ldb sb-vm:double-float-significand-byte bits)))
   #-64-bit (not (zerop (logior (ldb sb-vm:double-float-hi-significand-byte hi) lo)))

   #+(and long-float x86)
   (or (not (zerop (ldb sb-vm:long-float-significand-byte hi)))
       (not (zerop lo)))))

(defmacro with-float-inf-or-nan-test (float infinity nan normal)
  `(block nil
     ,(if (equal infinity nan)
          `(float-inf-or-nan-test
            ,float
            (return ,nan)
            (return ,nan))
          `(float-inf-or-nan-test
            ,float
            (if (zerop (ldb sb-vm:single-float-significand-byte bits))
                (return ,infinity)
                (return ,nan))
            (if #+64-bit (zerop (ldb sb-vm:double-float-significand-byte bits))
                #-64-bit (zerop (logior (ldb sb-vm:double-float-hi-significand-byte hi) lo))
                (return ,infinity)
                (return ,nan))))
     ,normal))

(defun float-trapping-nan-p (x)
  "Return true if the float X is a trapping NaN (Not a Number)."
  ;; MIPS has trapping NaNs (SNaNs) with the trapping-nan-bit SET.
  ;; All the others have trapping NaNs (SNaNs) with the
  ;; trapping-nan-bit CLEAR.  Note that the given implementation
  ;; considers infinities to be FLOAT-TRAPPING-NAN-P on most
  ;; architectures.
  (float-inf-or-nan-test
   x
   ;; SINGLE-FLOAT
   #+mips (logbitp 22 bits)
   #-mips (not (logbitp 22 bits))

   ;; DOUBLE-FLOAT
   #+mips (logbitp 19 hi)
   #+(and (not mips) 64-bit) (not (logbitp 51 bits))
   #+(and (not mips) (not 64-bit)) (not (logbitp 19 hi))

   ;; LONG-FLOAT (this code is dead anyway)
   #+(and long-float x86)
   (zerop (logand (ldb sb-vm:long-float-significand-byte hi)
                  (ash 1 30)))))
