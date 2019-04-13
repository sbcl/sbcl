;;;; type testing and checking VOPs for the RISC-V VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Test generation utilities.
(defun %test-fixnum (value temp target not-p)
  (assemble ()
    (inst andi temp value fixnum-tag-mask)
    (if not-p
        (inst bne temp zero-tn target)
        (inst beq temp zero-tn target))))

(defun %test-fixnum-and-headers (value temp target not-p headers &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst andi temp value fixnum-tag-mask)
      (inst beq temp zero-tn (if not-p drop-through target)))
    (%test-headers value temp target not-p nil headers
                   :drop-through drop-through
                   :value-tn-ref value-tn-ref)))

#+64-bit
(defun %test-fixnum-immediate-and-headers (value temp target not-p immediate
                                           headers &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (inst andi temp value fixnum-tag-mask)
    (inst beq temp zero-tn (if not-p drop-through target))
    (%test-immediate-and-headers value temp target not-p immediate headers
                                 :drop-through drop-through
                                 :value-tn-ref value-tn-ref)))


(defun %test-immediate (value temp target not-p immediate)
  (assemble ()
    (inst andi temp value widetag-mask)
    (inst xori temp temp immediate)
    (if not-p
        (inst bne temp zero-tn target)
        (inst beq temp zero-tn target))))

(defun %test-lowtag (value temp target not-p lowtag)
  (assemble ()
    (inst andi temp value lowtag-mask)
    (inst xori temp temp lowtag)
    (if not-p
        (inst bne temp zero-tn target)
        (inst beq temp zero-tn target))))

(defun %test-headers (value temp target not-p function-p headers
                      &key (drop-through (gen-label)) value-tn-ref)
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind
        (when-true when-false)
        ;; WHEN-TRUE and WHEN-FALSE are the labels to branch to when
        ;; we know it's true and when we know it's false respectively.
        (if not-p
            (values drop-through target)
            (values target drop-through))
      (assemble ()
        (unless (and value-tn-ref
                     (eq lowtag other-pointer-lowtag)
                     (other-pointer-tn-ref-p value-tn-ref))
          (%test-lowtag value temp when-false t lowtag))
        (load-type temp value (- lowtag))
        (let ((delta 0))
          (do ((remaining headers (cdr remaining)))
              ((null remaining))
            (let ((header (car remaining))
                  (last (null (cdr remaining))))
              (cond
               ((atom header)
                (inst subi temp temp (- header delta))
                (setf delta header)
                (if last
                    (if not-p
                        (inst bne temp zero-tn target)
                        (inst beq temp zero-tn target))
                    (inst beq temp zero-tn when-true)))
               (t
                (let ((start (car header))
                      (end (cdr header)))
                  (unless (= start bignum-widetag)
                    (inst subi temp temp (- start delta))
                    (setf delta start)
                    (inst blt temp zero-tn when-false))
                  (inst subi temp temp (- end delta))
                  (setf delta end)
                  (if last
                      (if not-p
                          (inst blt zero-tn temp target)
                          (inst bge zero-tn temp target))
                      (inst bge zero-tn temp when-true))))))))
        (emit-label drop-through)))))

(defun %test-immediate-and-headers (value temp target not-p immediate headers
                                    &key (drop-through (gen-label))
                                         value-tn-ref)
  (inst andi temp value widetag-mask)
  (inst xori temp temp immediate)
  (inst beq temp zero-tn (if not-p drop-through target))
  (%test-headers value temp target not-p nil headers
                 :drop-through drop-through
                 :value-tn-ref value-tn-ref))


;;;; Other integer ranges.

;;; A (SIGNED-BYTE N-WORD-BITS) can be represented with either fixnum or a
;;; bignum with exactly one digit.
(defun signed-byte-n-word-bits-test (value temp not-p target not-target)
  (multiple-value-bind (yep nope)
      (if not-p
          (values not-target target)
          (values target not-target))
    (assemble ()
      (inst andi temp value fixnum-tag-mask)
      (inst beq temp zero-tn yep)
      (test-type value temp nope t (other-pointer-lowtag))
      (loadw temp value 0 other-pointer-lowtag)
      (inst xori temp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
      (if not-p
          (inst bne temp zero-tn target)
          (inst beq temp zero-tn target))))
  (values))

(define-vop (#-64-bit signed-byte-32-p #+64-bit signed-byte-64-p type-predicate)
  (:translate #-64-bit signed-byte-32-p #+64-bit signed-byte-64-p)
  (:generator 45
   (let ((not-target (gen-label)))
     (signed-byte-n-word-bits-test value temp not-p target not-target)
     (emit-label not-target))))

;;; An (UNSIGNED-BYTE N-WORD-BITS) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (#-64-bit unsigned-byte-32-p #+64-bit unsigned-byte-64-p type-predicate)
  (:translate #-64-bit unsigned-byte-32-p #+64-bit unsigned-byte-64-p)
  (:temporary (:scs (non-descriptor-reg)) temp1)
  (:generator 45
    (let ((not-target (gen-label)))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (assemble ()
          ;; Is it a fixnum?
          (inst andi temp1 value fixnum-tag-mask)
          (move temp value)
          (inst beq temp1 zero-tn fixnum)

          ;; If not, is it an other pointer?
          (test-type value temp nope t (other-pointer-lowtag))
          ;; Get the header.
          (loadw temp value 0 other-pointer-lowtag)
          ;; Is it one?
          (inst xori temp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
          (inst beq temp zero-tn single-word)
          ;; If it's other than two, we can't be an (unsigned-byte n-word-bits)
          (inst xori temp temp
                (logxor (+ (ash 1 n-widetag-bits) bignum-widetag)
                        (+ (ash 2 n-widetag-bits) bignum-widetag)))
          (inst bne temp zero-tn nope)
          ;; Get the second digit.
          (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
          ;; All zeros, its an (unsigned-byte n-word-bits).
          (inst beq temp zero-tn yep)
          (inst j nope)

          SINGLE-WORD
          ;; Get the single digit.
          (loadw temp value bignum-digits-offset other-pointer-lowtag)

          ;; positive implies (unsigned-byte n-word-bits).
          FIXNUM
          (if not-p
              (inst blt temp zero-tn target)
              (inst bge temp zero-tn target))))
      (emit-label not-target))))

;;;; List/symbol types:
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (inst beq value null-tn (if not-p drop-thru target))
    (test-type value temp target not-p (symbol-widetag))
    DROP-THRU))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (inst beq value null-tn (if not-p target drop-thru))
    (test-type value temp target not-p (list-pointer-lowtag))
    DROP-THRU))
