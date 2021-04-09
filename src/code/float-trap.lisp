;;;; This file contains stuff for controlling floating point traps. It
;;;; is IEEE float specific, but should work for pretty much any FPU
;;;; where the state fits in one word and exceptions are represented
;;;; by bits being set.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant-eqx +float-trap-alist+
    `((:underflow . ,float-underflow-trap-bit)
      (:overflow . ,float-overflow-trap-bit)
      (:inexact . ,float-inexact-trap-bit)
      (:invalid . ,float-invalid-trap-bit)
      (:divide-by-zero . ,float-divide-by-zero-trap-bit)
      #+x86 (:denormalized-operand . ,float-denormal-trap-bit))
  #'equal)

(defconstant-eqx +rounding-mode-alist+
    `((:nearest . ,float-round-to-nearest)
      (:zero . ,float-round-to-zero)
      (:positive-infinity . ,float-round-to-positive)
      (:negative-infinity . ,float-round-to-negative))
  #'equal)

#+x86
(defconstant-eqx +precision-mode-alist+
    `((:24-bit . ,float-precision-24-bit)
      (:53-bit . ,float-precision-53-bit)
      (:64-bit . ,float-precision-64-bit))
  #'equal)

;;; Return a mask with all the specified float trap bits set.
(defun float-trap-mask (names)
  (reduce #'logior
          (mapcar (lambda (x)
                    (or (cdr (assoc x +float-trap-alist+))
                        (error "unknown float trap kind: ~S" x)))
                  names)))
) ; EVAL-WHEN

;;; interpreter stubs for floating point modes get/setters for
;;; some architectures have been removed, as they are implemented
;;; in C rather than as VOPs.
#-(or x86-64 mips)
(progn
  (defun floating-point-modes ()
    (floating-point-modes))
  (defun (setf floating-point-modes) (new)
    (setf (floating-point-modes) new)))

(defun set-floating-point-modes (&key
                                 (traps nil traps-p)
                                 (rounding-mode nil round-p)
                                 (current-exceptions nil current-x-p)
                                 (accrued-exceptions nil accrued-x-p)
                                 (fast-mode nil fast-mode-p)
                                 #+x86 (precision nil precisionp))
  "This function sets options controlling the floating-point
hardware. If a keyword is not supplied, then the current value is
preserved. Possible keywords:

 :TRAPS
   A list of the exception conditions that should cause traps.
   Possible exceptions are :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID,
  :DIVIDE-BY-ZERO, and on the X86 :DENORMALIZED-OPERAND.

:ROUNDING-MODE
   The rounding mode to use when the result is not exact. Possible
   values are :NEAREST, :POSITIVE-INFINITY, :NEGATIVE-INFINITY and
   :ZERO.  Setting this away from :NEAREST is liable to upset SBCL's
   maths routines which depend on it.

:CURRENT-EXCEPTIONS
:ACCRUED-EXCEPTIONS
   These arguments allow setting of the exception flags. The main
   use is setting the accrued exceptions to NIL to clear them.

:FAST-MODE
   Set the hardware's \"fast mode\" flag, if any. When set, IEEE
   conformance or debuggability may be impaired. Some machines don't
   have this feature, and some SBCL ports don't implement it anyway
   -- in such cases the value is always NIL.

:PRECISION (x86 only)
  :24-bit, :53-bit and :64-bit, for the internal precision of the mantissa.

GET-FLOATING-POINT-MODES may be used to find the floating point modes
currently in effect. SAVE-LISP-AND-DIE preserves the floating point modes
in effect."
  (let ((modes (floating-point-modes)))
    (when traps-p
      (setf (ldb float-traps-byte modes) (float-trap-mask traps)))
    (when round-p
      (setf (ldb float-rounding-mode modes)
            (or (cdr (assoc rounding-mode +rounding-mode-alist+))
                (error "unknown rounding mode: ~S" rounding-mode))))
    (when current-x-p
      (setf (ldb float-exceptions-byte modes)
            (float-trap-mask current-exceptions)))
    (when accrued-x-p
      (setf (ldb float-sticky-bits modes)
            (float-trap-mask accrued-exceptions)))
    (when fast-mode-p
      (if fast-mode
          (setq modes (logior float-fast-bit modes))
          (setq modes (logand (lognot float-fast-bit) modes))))
    #+x86
    (when precisionp
      (setf (ldb float-precision-control modes)
            (or (cdr (assoc precision +precision-mode-alist+))
                (error "unknown precision mode: ~S" precision))))
    ;; FIXME: This apparently doesn't work on Darwin
    #-(and darwin ppc)
    (setf (floating-point-modes) modes))
  (values))

(defun get-floating-point-modes ()
  "This function returns a list representing the state of the floating
point modes. The list is in the same format as the &KEY arguments to
SET-FLOATING-POINT-MODES, i.e.

  (apply #'set-floating-point-modes (get-floating-point-modes))

sets the floating point modes to their current values (and thus is a no-op)."
  (flet ((exc-keys (bits)
           (macrolet ((frob ()
                        `(collect ((res))
                           ,@(mapcar (lambda (x)
                                       `(when (logtest bits ,(cdr x))
                                          (res ',(car x))))
                                     +float-trap-alist+)
                           (res))))
             (frob))))
    (let ((modes (floating-point-modes)))
      `(:traps ,(exc-keys (ldb float-traps-byte modes))
        :rounding-mode ,(car (rassoc (ldb float-rounding-mode modes)
                                     +rounding-mode-alist+))
        :current-exceptions ,(exc-keys (ldb float-exceptions-byte modes))
        :accrued-exceptions ,(exc-keys (ldb float-sticky-bits modes))
        :fast-mode ,(logtest float-fast-bit modes)
        #+x86 :precision
        #+x86 ,(car (rassoc (ldb float-precision-control modes)
                             +precision-mode-alist+))))))

;;; FIXME: For some unknown reason, NetBSD/x86 won't run with the
;;; :INVALID trap enabled. That should be fixed, but not today...
;;;
;;; PRINT seems not to like x86 NPX denormal floats like
;;; LEAST-NEGATIVE-SINGLE-FLOAT, so the :UNDERFLOW exceptions are
;;; disabled by default. Joe User can explicitly enable them if
;;; desired.
(define-load-time-global *saved-floating-point-modes*
  '(:traps (:overflow #-(or netbsd ppc) :invalid :divide-by-zero)
    :rounding-mode :nearest :current-exceptions nil
    :accrued-exceptions nil :fast-mode nil
    #+x86 :precision #+x86 :53-bit))

(defun float-cold-init-or-reinit ()
  (apply #'set-floating-point-modes *saved-floating-point-modes*))

(defun float-deinit ()
  (setf *saved-floating-point-modes* (get-floating-point-modes)))

;;; Return true if any of the named traps are currently trapped, false
;;; otherwise.
(defmacro current-float-trap (&rest traps)
  `(not (zerop (logand ,(dpb (float-trap-mask traps) float-traps-byte 0)
                       (floating-point-modes)))))

;;; SIGFPE code to floating-point error
#-win32
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant-eqx +sigfpe-code-error-alist+
    `((,sb-unix::fpe-intovf . floating-point-overflow)
      (,sb-unix::fpe-intdiv . division-by-zero)
      (,sb-unix::fpe-fltdiv . division-by-zero)
      (,sb-unix::fpe-fltovf . floating-point-overflow)
      (,sb-unix::fpe-fltund . floating-point-underflow)
      (,sb-unix::fpe-fltres . floating-point-inexact)
      (,sb-unix::fpe-fltinv . floating-point-invalid-operation)
      (,sb-unix::fpe-fltsub . floating-point-exception))
    #'equal))

;;; Signal the appropriate condition when we get a floating-point error.
#-win32
(defun sigfpe-handler (signal info context)
  (declare (ignore signal))
  (declare (type system-area-pointer info))
  (let ((code (sb-unix::siginfo-code info)))
    (multiple-value-bind (op operands) (sb-di::decode-arithmetic-error-operands context)
     (with-interrupts
       ;; Reset the accumulated exceptions, may be needed on other
       ;; platforms too, at least Linux doesn't seem to require it.
       #+sunos (setf (ldb sb-vm:float-sticky-bits (floating-point-modes)) 0)
       (error (or (cdr (assoc code +sigfpe-code-error-alist+))
                  'floating-point-exception)
              :operation op
              :operands operands)))))

;;; Execute BODY with the floating point exceptions listed in TRAPS
;;; masked (disabled). TRAPS should be a list of possible exceptions
;;; which includes :UNDERFLOW, :OVERFLOW, :INEXACT, :INVALID and
;;; :DIVIDE-BY-ZERO and on the X86 :DENORMALIZED-OPERAND. The
;;; respective accrued exceptions are cleared at the start of the body
;;; to support their testing within, and restored on exit.
(defmacro with-float-traps-masked (traps &body body)
  (let ((traps (dpb (float-trap-mask traps) float-traps-byte 0))
        (exceptions (dpb (float-trap-mask traps) float-sticky-bits 0))
        (trap-mask (dpb (lognot (float-trap-mask traps))
                        float-traps-byte #xffffffff))
        (exception-mask (dpb (lognot (float-trap-mask traps))
                             float-sticky-bits #xffffffff))
        ;; MIPS has a second set of "accumulated exceptions" which are
        ;; actually used to cause the exception to be delivered, and
        ;; which can be set from user code.  Compute the mask here,
        ;; and clear them below.
        #+mips (cause-mask (dpb (lognot (float-trap-mask traps))
                                float-exceptions-byte #xffffffff))
        (orig-modes (gensym)))
    #+ppc64
    (unless (logbitp float-invalid-trap-bit (ldb float-sticky-bits exception-mask))
      ;; float-invalid-trap-bit is just a summary of this bits which
      ;; all have to be cleared invidually.
      (setf (ldb float-invalid-byte exception-mask) 0))
    `(let ((,orig-modes (floating-point-modes)))
       (unwind-protect
            (progn
              (setf (floating-point-modes)
                    (logand ,orig-modes ,(logand trap-mask exception-mask)))
              ,@body)
         ;; Restore the original traps and exceptions.
         (setf (floating-point-modes)
               (logior (logand ,orig-modes ,(logior traps exceptions))
                       (logand (floating-point-modes)
                               ,(logand trap-mask exception-mask
                                        #+mips cause-mask))))))))
