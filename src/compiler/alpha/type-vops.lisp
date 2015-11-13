;;;; type testing and checking VOPs for the Alpha VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(defun %test-fixnum (value target not-p &key temp)
  (assemble ()
    (inst and value fixnum-tag-mask temp)
    (if not-p
        (inst bne temp target)
        (inst beq temp target))))

(defun %test-fixnum-and-headers (value target not-p headers &key temp)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst and value fixnum-tag-mask temp)
      (inst beq temp (if not-p drop-through target)))
    (%test-headers value target not-p nil headers
                   :drop-through drop-through :temp temp)))

(defun %test-immediate (value target not-p immediate &key temp)
  (assemble ()
    (inst and value 255 temp)
    (inst xor temp immediate temp)
    (if not-p
        (inst bne temp target)
        (inst beq temp target))))

(defun %test-lowtag (value target not-p lowtag &key temp)
  (assemble ()
    (inst and value lowtag-mask temp)
    (inst xor temp lowtag temp)
    (if not-p
        (inst bne temp target)
        (inst beq temp target))))

(defun %test-headers (value target not-p function-p headers
                      &key (drop-through (gen-label)) temp)
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind
        (when-true when-false)
        ;; WHEN-TRUE and WHEN-FALSE are the labels to branch to when
        ;; we know it's true and when we know it's false respectively.
        (if not-p
            (values drop-through target)
            (values target drop-through))
      (assemble ()
        (%test-lowtag value when-false t lowtag :temp temp)
        (load-type temp value (- lowtag))
        (let ((delta 0))
          (do ((remaining headers (cdr remaining)))
              ((null remaining))
            (let ((header (car remaining))
                  (last (null (cdr remaining))))
              (cond
               ((atom header)
                (inst subq temp (- header delta) temp)
                (setf delta header)
                (if last
                    (if not-p
                        (inst bne temp target)
                        (inst beq temp target))
                    (inst beq temp when-true)))
               (t
                (let ((start (car header))
                      (end (cdr header)))
                  (unless (= start bignum-widetag)
                    (inst subq temp (- start delta) temp)
                    (setf delta start)
                    (inst blt temp when-false))
                  (inst subq temp (- end delta) temp)
                  (setf delta end)
                  (if last
                      (if not-p
                          (inst bgt temp target)
                          (inst ble temp target))
                      (inst ble temp when-true))))))))
        (emit-label drop-through)))))

;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.
(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:temporary (:scs (non-descriptor-reg)) temp1)
  (:generator 45
    (multiple-value-bind
          (yep nope)
        (if not-p
            (values not-target target)
            (values target not-target))
      (assemble ()
        (inst and value fixnum-tag-mask temp)
        (inst beq temp yep)
        (inst and value lowtag-mask temp)
        (inst xor temp other-pointer-lowtag temp)
        (inst bne temp nope)
        (loadw temp value 0 other-pointer-lowtag)
        (inst li (+ (ash 1 n-widetag-bits) bignum-widetag) temp1)
        (inst xor temp temp1 temp)
        (if not-p
            (inst bne temp target)
            (inst beq temp target))))
    NOT-TARGET))

;;; An (unsigned-byte 32) can be represented with either a positive fixnum, a
;;; bignum with exactly one positive digit, or a bignum with exactly two digits
;;; and the second digit all zeros.

(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:temporary (:scs (non-descriptor-reg)) temp1)
  (:generator 45
    (multiple-value-bind (yep nope)
        (if not-p
            (values not-target target)
            (values target not-target))
      (assemble ()
        ;; Is it a fixnum?
        (inst and value fixnum-tag-mask temp1)
        (inst move value temp)
        (inst beq temp1 fixnum)

        ;; If not, is it an other pointer?
        (inst and value lowtag-mask temp)
        (inst xor temp other-pointer-lowtag temp)
        (inst bne temp nope)
        ;; Get the header.
        (loadw temp value 0 other-pointer-lowtag)
        ;; Is it one?
        (inst li  (+ (ash 1 n-widetag-bits) bignum-widetag) temp1)
        (inst xor temp temp1 temp)
        (inst beq temp single-word)
        ;; If it's other than two, we can't be an (unsigned-byte 32)
        (inst li (logxor (+ (ash 1 n-widetag-bits) bignum-widetag)
                         (+ (ash 2 n-widetag-bits) bignum-widetag))
              temp1)
        (inst xor temp temp1 temp)
        (inst bne temp nope)
        ;; Get the second digit.
        (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
        ;; All zeros, its an (unsigned-byte 32).
        (inst beq temp yep)
        (inst br zero-tn nope)

        SINGLE-WORD
        ;; Get the single digit.
        (loadw temp value bignum-digits-offset other-pointer-lowtag)

        ;; positive implies (unsigned-byte 32).
        FIXNUM
        (if not-p
            (inst blt temp target)
            (inst bge temp target))))
    NOT-TARGET))

;;;; List/symbol types:
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 12
    (inst cmpeq value null-tn temp)
    (inst bne temp (if not-p drop-thru target))
    (test-type value target not-p (symbol-header-widetag) :temp temp)
    DROP-THRU))

(define-vop (consp type-predicate)
  (:translate consp)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 8
    (inst cmpeq value null-tn temp)
    (inst bne temp (if not-p target drop-thru))
    (test-type value target not-p (list-pointer-lowtag) :temp temp)
    DROP-THRU))
