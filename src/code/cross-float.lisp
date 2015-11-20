;;;; portable implementations or stubs for nonportable floating point
;;;; things, useful for building Python as a cross-compiler when
;;;; running under an ordinary ANSI Common Lisp implementation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; There seems to be no portable way to mask float traps, but we
;;; shouldn't encounter any float traps when cross-compiling SBCL
;;; itself, anyway, so we just make this a no-op.
(defmacro sb!vm::with-float-traps-masked (traps &body body)
  (declare (ignore traps))
  ;; FIXME: should become STYLE-WARNING?
  (format *error-output*
          "~&(can't portably mask float traps, proceeding anyway)~%")
  `(progn ,@body))

;;; a helper function for DOUBLE-FLOAT-FOO-BITS functions
;;;
;;; Return the low N bits of X as a signed N-bit value.
(defun mask-and-sign-extend (x n)
  (assert (plusp n))
  (let* ((high-bit (ash 1 (1- n)))
         (mask (1- (ash high-bit 1)))
         (uresult (logand mask x)))
    (if (zerop (logand uresult high-bit))
        uresult
        (logior uresult
                (logand -1 (lognot mask))))))

;;; portable implementations of SINGLE-FLOAT-BITS,
;;; DOUBLE-FLOAT-LOW-BITS, and DOUBLE-FLOAT-HIGH-BITS
;;;
;;; KLUDGE: These will fail if the target's floating point isn't IEEE,
;;; and so I'd be more comfortable if there were an assertion
;;; "target's floating point is IEEE" in the code, but I can't see how
;;; to express that.
;;;
;;; KLUDGE: It's sort of weird that these functions return signed
;;; 32-bit values instead of unsigned 32-bit values. This is the way
;;; that the CMU CL machine-dependent functions behaved, and I've
;;; copied that behavior, but it seems to me that it'd be more
;;; idiomatic to return unsigned 32-bit values. Maybe someday the
;;; machine-dependent functions could be tweaked to return unsigned
;;; 32-bit values?
(defun single-float-bits (x)
  (declare (type single-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
      (if (eql x 0.0f0) 0 #x-80000000)
      (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
          (integer-decode-float x)
        (assert (plusp lisp-significand))
        ;; Calculate IEEE-style fields from Common-Lisp-style fields.
        ;;
        ;; KLUDGE: This code was written from my foggy memory of what IEEE
        ;; format looks like, augmented by some experiments with
        ;; the existing implementation of SINGLE-FLOAT-BITS, and what
        ;; I found floating around on the net at
        ;;   <http://www.scri.fsu.edu/~jac/MAD3401/Backgrnd/ieee.html>,
        ;;   <http://rodin.cs.uh.edu/~johnson2/ieee.html>,
        ;; and
        ;;   <http://www.ttu.ee/sidu/cas/IEEE_Floating.htm>.
        ;; And beyond the probable sheer flakiness of the code, all the bare
        ;; numbers floating around here are sort of ugly, too. -- WHN 19990711
        (let* ((significand lisp-significand)
               (exponent (+ lisp-exponent 23 127))
               (unsigned-result
                (if (plusp exponent)    ; if not obviously denormalized
                    (do ()
                        (nil)
                      (cond (;; special termination case, denormalized
                             ;; float number
                             (zerop exponent)
                             ;; Denormalized numbers have exponent one
                             ;; greater than the exponent field.
                             (return (ash significand -1)))
                            (;; ordinary termination case
                             (>= significand (expt 2 23))
                             (assert (< 0 significand (expt 2 24)))
                             ;; Exponent 0 is reserved for
                             ;; denormalized numbers, and 255 is
                             ;; reserved for specials like NaN.
                             (assert (< 0 exponent 255))
                             (return (logior (ash exponent 23)
                                             (logand significand
                                                     (1- (ash 1 23))))))

                            (t
                             ;; Shift as necessary to set bit 24 of
                             ;; significand.
                             (setf significand (ash significand 1)
                                   exponent (1- exponent)))))
                    (do ()
                        ((zerop exponent)
                         ;; Denormalized numbers have exponent one
                         ;; greater than the exponent field.
                         (ash significand -1))
                      (unless (zerop (logand significand 1))
                        (warn "denormalized SINGLE-FLOAT-BITS ~S losing bits"
                              x))
                      (setf significand (ash significand -1)
                            exponent (1+ exponent))))))
          (ecase lisp-sign
            (1 unsigned-result)
            (-1 (logior unsigned-result (- (expt 2 31)))))))))

(defun double-float-bits (x)
  (declare (type double-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
      (if (eql x 0.0d0) 0 #x-8000000000000000)
      ;; KLUDGE: As per comments in SINGLE-FLOAT-BITS, above.
      (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
          (integer-decode-float x)
        (assert (plusp lisp-significand))
        (let* ((significand lisp-significand)
               (exponent (+ lisp-exponent 52 1023))
               (unsigned-result
                (if (plusp exponent)    ; if not obviously denormalized
                    (do ()
                        (nil)
                      (cond (;; special termination case, denormalized
                             ;; float number
                             (zerop exponent)
                             ;; Denormalized numbers have exponent one
                             ;; greater than the exponent field.
                             (return (ash significand -1)))
                            (;; ordinary termination case
                             (>= significand (expt 2 52))
                             (assert (< 0 significand (expt 2 53)))
                             ;; Exponent 0 is reserved for
                             ;; denormalized numbers, and 2047 is
                             ;; reserved for specials like NaN.
                             (assert (< 0 exponent 2047))
                             (return (logior (ash exponent 52)
                                             (logand significand
                                                     (1- (ash 1 52))))))
                            (t
                             ;; Shift as necessary to set bit 53 of
                             ;; significand.
                             (setf significand (ash significand 1)
                                   exponent (1- exponent)))))
                    (do ()
                        ((zerop exponent)
                         ;; Denormalized numbers have exponent one
                         ;; greater than the exponent field.
                         (ash significand -1))
                      (unless (zerop (logand significand 1))
                        (warn "denormalized SINGLE-FLOAT-BITS ~S losing bits"
                              x))
                      (setf significand (ash significand -1)
                            exponent (1+ exponent))))))
          (ecase lisp-sign
            (1 unsigned-result)
            (-1 (logior unsigned-result (- (expt 2 63)))))))))

(defun double-float-low-bits (x)
  (declare (type double-float x))
  (if (zerop x)
      0
      ;; FIXME: Unlike DOUBLE-FLOAT-HIGH-BITS or SINGLE-FLOAT-BITS,
      ;; the CMU CL DOUBLE-FLOAT-LOW-BITS seemed to return a unsigned
      ;; value, not a signed value, so we've done the same. But it
      ;; would be nice to make the family of functions have a more
      ;; consistent return convention.
      (logand #xffffffff (double-float-bits x))))

(defun double-float-high-bits (x)
  (declare (type double-float x))
  (if (zerop x)
      (if (eql x 0.0d0) 0 #x-80000000)
      (mask-and-sign-extend (ash (double-float-bits x) -32) 32)))

;;; KLUDGE: This is a hack to work around a bug in CMU CL 18c which
;;; causes the 18c compiler to die with a floating point exception
;;; when trying to optimize the EXPT forms in the MAKE-FOO-FLOAT
;;; functions below. See the message
;;;   Subject: Re: Compiler bug?
;;;   From: Raymond Toy
;;;   Date: 28 Mar 2001 08:19:59 -0500
;;; on the cmucl-imp mailing list. Once the CMU CL folks
;;; make a bug-fix release, we can get rid of this and go back to
;;; calling EXPT directly. -- WHN 2001-04-05
(defun kludge-opaque-expt (x y)
  (expt x y))

;;; KLUDGE: These functions will blow up on any cross-compilation
;;; host Lisp which has less floating point precision than the target
;;; Lisp. In practice, this may not be a major problem: IEEE
;;; floating point arithmetic is so common these days that most
;;; cross-compilation host Lisps are likely to have exactly the same
;;; floating point precision as the target Lisp. If it turns out to be
;;; a problem, there are possible workarounds involving portable
;;; representations for target floating point numbers, like
;;;   (DEFSTRUCT TARGET-SINGLE-FLOAT
;;;     (SIGN (MISSING-ARG) :TYPE BIT)
;;;     (EXPONENT (MISSING-ARG) :TYPE UNSIGNED-BYTE)
;;;     (MANTISSA (MISSING-ARG) :TYPE UNSIGNED-BYTE))
;;; with some sort of MAKE-LOAD-FORM-ish magic to cause them to be
;;; written out in the appropriate target format. (And yes, those
;;; workarounds *do* look messy to me, which is why I just went
;;; with this quick kludge instead.) -- WHN 19990711
(defun make-single-float (bits)
  (cond
    ;; IEEE float special cases
    ((zerop bits) 0.0)
    ((= bits #x-80000000) -0.0)
    (t (let* ((sign (ecase (ldb (byte 1 31) bits)
                      (0  1.0)
                      (1 -1.0)))
              (iexpt (ldb (byte 8 23) bits))
              (expt (if (zerop iexpt) ; denormalized
                        -126
                        (- iexpt 127)))
              (mant (* (logior (ldb (byte 23 0) bits)
                               (if (zerop iexpt)
                                   0
                                   (ash 1 23)))
                       (expt 0.5 23))))
         (* sign (kludge-opaque-expt 2.0 expt) mant)))))

(defun make-double-float (hi lo)
  (cond
    ;; IEEE float special cases
    ((and (zerop hi) (zerop lo)) 0.0d0)
    ((and (= hi #x-80000000) (zerop lo)) -0.0d0)
    (t (let* ((bits (logior (ash hi 32) lo))
              (sign (ecase (ldb (byte 1 63) bits)
                      (0  1.0d0)
                      (1 -1.0d0)))
              (iexpt (ldb (byte 11 52) bits))
              (expt (if (zerop iexpt) ; denormalized
                        -1022
                        (- iexpt 1023)))
              (mant (* (logior (ldb (byte 52 0) bits)
                               (if (zerop iexpt)
                                   0
                                   (ash 1 52)))
                       (expt 0.5d0 52))))
         (* sign (kludge-opaque-expt 2.0d0 expt) mant)))))

(defun float-infinity-p (x)
  (declare (ignore x))
  (error "Can't call FLOAT-INFINITY-P"))
(defun float-nan-p (x)
  (declare (ignore x))
  (error "Can't call FLOAT-NAN-P"))
