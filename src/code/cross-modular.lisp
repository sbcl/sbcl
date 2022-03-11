;;;; cross-compile-time-only replacements for modular functions;
;;;; needed for constant-folding

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defun mask-signed-field (size integer)
  (cond ((zerop size)
         0)
        ((logbitp (1- size) integer)
         (dpb integer (byte size 0) -1))
        (t
         (ldb (byte size 0) integer))))

#.
(collect ((forms))
  (flet ((unsigned-definition (name lambda-list prototype width)
           `(defun ,name ,lambda-list
              (ldb (byte ,width 0) (,prototype ,@lambda-list))))
         (signed-definition (name lambda-list prototype width)
           `(defun ,name ,lambda-list
              (mask-signed-field ,width (,prototype ,@lambda-list)))))
    (flet ((do-mfuns (class)
             (loop for infos being each hash-value of (modular-class-funs class) using (hash-key prototype)
                   when (listp infos)
                   do (loop for info in infos
                            for name = (modular-fun-info-name info)
                            and width = (modular-fun-info-width info)
                            and signedp = (modular-fun-info-signedp info)
                            and lambda-list = (modular-fun-info-lambda-list info)
                            if signedp
                            do (forms (signed-definition name lambda-list prototype width))
                            else
                            do (forms (unsigned-definition name lambda-list prototype width))))))
      (do-mfuns *untagged-unsigned-modular-class*)
      (do-mfuns *untagged-signed-modular-class*)
      (do-mfuns *tagged-modular-class*)))
  `(progn ,@(forms)))

#.`
(defun ,(intern (format nil "ASH-LEFT-MOD~D" sb-vm:n-machine-word-bits)
                "SB-VM")
    (integer amount)
  (ldb (byte ,sb-vm:n-machine-word-bits 0) (ash integer amount)))

#+(or x86 x86-64 arm arm64)
(defun sb-vm::ash-left-modfx (integer amount)
  (mask-signed-field (- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits)
                     (ash integer amount)))

;;;; these cross-compiler compatibility functions have vops for
;;;; the target machine.
;;;; COUNT is regarded as having either 5 (for 32-machines) or 6
;;;; (for 64-bit machines) significant bits. Excess bits must be ignored
;;;; either explicitly in the vops (arm[64], ppc[64]) or by the CPU
;;;; (x86, sparc, mips, riscv).

;;; Shift NUMBER by the low-order bits of COUNT, adding zero bits
;;; at the "end" and removing bits from the "start". On big-endian
;;; machines this is a left-shift and on little-endian machines this
;;; is a right-shift.
(defun shift-towards-start (number count)
  (declare (type word number) (fixnum count))
  (let ((count (ldb (byte (1- (integer-length sb-vm:n-word-bits)) 0) count)))
    #+big-endian (logand (ash number count) most-positive-word)
    #+little-endian (ash number (- count))))

;;; Shift NUMBER by the low-order bits of COUNT, adding zero bits
;;; at the "start" and removing bits from the "end". On big-endian
;;; machines this is a right-shift and on little-endian machines this
;;; is a left-shift.
(defun shift-towards-end (number count)
  (declare (type word number) (fixnum count))
  (let ((count (ldb (byte (1- (integer-length sb-vm:n-word-bits)) 0) count)))
    #+big-endian (ash number (- count))
    #+little-endian (logand (ash number count) most-positive-word)))
