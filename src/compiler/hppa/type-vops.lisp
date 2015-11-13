;;;; type testing and checking VOPs for the HPPA VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; Test generation utilities.
(defun %test-fixnum (value target not-p &key temp)
  (declare (ignore temp))
  (assemble ()
    (inst extru value 31 2 zero-tn (if not-p := :<>))
    (inst b target :nullify t)))

(defun %test-fixnum-and-headers (value target not-p headers &key temp)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst extru value 31 2 zero-tn :<>)
      (inst b (if not-p drop-through target) :nullify t))
    (%test-headers value target not-p nil headers
                   :drop-through drop-through :temp temp)))

(defun %test-immediate (value target not-p immediate &key temp)
  (assemble ()
    (inst extru value 31 8 temp)
    (inst bci := not-p immediate temp target)))

(defun %test-lowtag (value target not-p lowtag &key temp temp-loaded)
  (assemble ()
    (unless temp-loaded
      (inst extru value 31 3 temp))
    (inst bci := not-p lowtag temp target)))

(defun %test-headers (value target not-p function-p headers
                      &key temp (drop-through (gen-label)) temp-loaded)
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind
        (equal greater-or-equal when-true when-false)
        ;; EQUAL and GREATER-OR-EQUAL are the conditions for branching to
        ;; TARGET.  WHEN-TRUE and WHEN-FALSE are the labels to branch to when
        ;; we know it's true and when we know it's false respectively.
        (if not-p
            (values :<> :< drop-through target)
            (values := :>= target drop-through))
      (assemble ()
        (%test-lowtag value when-false t lowtag
                      :temp temp :temp-loaded temp-loaded)
        (inst ldb (- 3 lowtag) value temp)
        (do ((remaining headers (cdr remaining)))
            ((null remaining))
          (let ((header (car remaining))
                (last (null (cdr remaining))))
            (cond
             ((atom header)
              (if last
                  (inst bci equal nil header temp target)
                  (inst bci := nil header temp when-true)))
             (t
              (let ((start (car header))
                    (end (cdr header)))
                (unless (= start bignum-widetag)
                  (inst bci :> nil start temp when-false))
                (if last
                    (inst bci greater-or-equal nil end temp target)
                    (inst bci :>= nil end temp when-true)))))))
        (emit-label drop-through)))))

;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.
(defun signed-byte-32-test (value temp not-p target not-target)
  (multiple-value-bind
      (yep nope)
      (if not-p
          (values not-target target)
          (values target not-target))
    (assemble ()
      (inst extru value 31 2 zero-tn :<>)
      (inst b yep :nullify t)
      (inst extru value 31 3 temp)
      (inst bci :<> nil other-pointer-lowtag temp nope)
      (loadw temp value 0 other-pointer-lowtag)
      (inst bci := not-p (+ (ash 1 n-widetag-bits) bignum-widetag) temp target)))
  (values))

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (signed-byte-32-test value temp not-p target not-target)
    NOT-TARGET))

;;; An (unsigned-byte 32) can be represented with either a positive fixnum, a
;;; bignum with exactly one positive digit, or a bignum with exactly two digits
;;; and the second digit all zeros.
(defun unsigned-byte-32-test (value temp not-p target not-target)
  (let ((nope (if not-p target not-target)))
    (assemble ()
      ;; Is it a fixnum?
      (inst extru value 31 2 zero-tn :<>)
      (inst b fixnum)
      (move value temp t)

      ;; If not, is it an other pointer?
      (inst extru value 31 3 temp)
      (inst bci :<> nil other-pointer-lowtag temp nope)
      ;; Get the header.
      (loadw temp value 0 other-pointer-lowtag)
      ;; Is it one?
      (inst bci := nil (+ (ash 1 n-widetag-bits) bignum-widetag) temp single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst bci :<> nil (+ (ash 2 n-widetag-bits) bignum-widetag) temp nope)
      ;; Get the second digit.
      (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 32).
      ;; Dont nullify comb here, because we cant guarantee target is forward
      (inst comb (if not-p := :<>) temp zero-tn not-target)
      (inst nop)
      (inst b target)

      SINGLE-WORD
      ;; Get the single digit.
      (loadw temp value bignum-digits-offset other-pointer-lowtag)

      ;; positive implies (unsigned-byte 32).
      FIXNUM
      (inst bc :>= not-p temp zero-tn target)))
  (values))

(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (unsigned-byte-32-test value temp not-p target not-target)
    NOT-TARGET))

;;;; List/symbol types:
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (inst bc := nil value null-tn (if not-p drop-thru target))
    (test-type value target not-p (symbol-header-widetag) :temp temp)
    DROP-THRU))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (inst bc := nil value null-tn (if not-p target drop-thru))
    (test-type value target not-p (list-pointer-lowtag) :temp temp)
    DROP-THRU))


