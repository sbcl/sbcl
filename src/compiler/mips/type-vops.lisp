;;;; type testing and checking VOPs for the MIPS VM

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
  (assemble ()
    (inst and temp value fixnum-tag-mask)
    (if not-p
        (inst bne temp target)
        (inst beq temp target))
    (inst nop)))

(defun %test-fixnum-and-headers (value target not-p headers &key temp)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst and temp value fixnum-tag-mask)
      (inst beq temp (if not-p drop-through target)))
    (%test-headers value target not-p nil headers
                   :drop-through drop-through :temp temp)))

(defun %test-immediate (value target not-p immediate &key temp)
  (assemble ()
    (inst and temp value widetag-mask)
    (inst xor temp immediate)
    (if not-p
        (inst bne temp target)
        (inst beq temp target))
    (inst nop)))

(defun %test-lowtag (value target not-p lowtag &key temp)
  (assemble ()
    (inst and temp value lowtag-mask)
    (inst xor temp lowtag)
    (if not-p
        (inst bne temp target)
        (inst beq temp target))
    (inst nop)))

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
        (inst nop)
        (let ((delta 0))
          (do ((remaining headers (cdr remaining)))
              ((null remaining))
            (let ((header (car remaining))
                  (last (null (cdr remaining))))
              (cond
               ((atom header)
                (inst subu temp (- header delta))
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
                    (inst subu temp (- start delta))
                    (setf delta start)
                    (inst bltz temp when-false))
                  (inst subu temp (- end delta))
                  (setf delta end)
                  (if last
                      (if not-p
                          (inst bgtz temp target)
                          (inst blez temp target))
                      (inst blez temp when-true))))))))
        (inst nop)
        (emit-label drop-through)))))



;;; Type checking and testing (see also the use of !DEFINE-TYPE-VOPS
;;; in src/compiler/generic/late-type-vops.lisp):
;;;
;;; [FIXME: Like some of the other comments in this file, this one
;;; really belongs somewhere else]
(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :to (:result 0)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe))

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))

(defmacro !define-type-vops (pred-name check-name ptype error-code
                             (&rest type-codes)
                             &key &allow-other-keys)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
           `((define-vop (,pred-name type-predicate)
               (:translate ,pred-name)
               (:generator ,cost
                 (test-type value target not-p (,@type-codes)
                            :temp temp)))))
       ,@(when check-name
           `((define-vop (,check-name check-type)
               (:generator ,cost
                 (let ((err-lab
                        (generate-error-code vop ,error-code value)))
                   (test-type value err-lab t (,@type-codes)
                              :temp temp)
                   (move result value))))))
       ,@(when ptype
           `((primitive-type-vop ,check-name (:check) ,ptype))))))

;;;; TYPE-VOPs for types that are more complex to test for than simple
;;;; LOWTAG and WIDETAG tests, but that are nevertheless important:

;;; A (SIGNED-BYTE 32) can be represented with either fixnum or a
;;; bignum with exactly one digit.
(defun signed-byte-32-test (value temp not-p target not-target)
  (multiple-value-bind
      (yep nope)
      (if not-p
          (values not-target target)
          (values target not-target))
    (assemble ()
      (inst and temp value fixnum-tag-mask)
      (inst beq temp yep)
      (inst and temp value lowtag-mask)
      (inst xor temp other-pointer-lowtag)
      (inst bne temp nope)
      (inst nop)
      (loadw temp value 0 other-pointer-lowtag)
      (inst xor temp (+ (ash 1 n-widetag-bits) bignum-widetag))
      (if not-p
          (inst bne temp target)
          (inst beq temp target))
      (inst nop)))
  (values))

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (signed-byte-32-test value temp not-p target not-target)
    NOT-TARGET))

(define-vop (check-signed-byte-32 check-type)
  (:generator 45
    (let ((loose (generate-error-code vop object-not-signed-byte-32-error
                                      value)))
      (signed-byte-32-test value temp t loose okay))
    OKAY
    (move result value)))

;;; An (UNSIGNED-BYTE 32) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(defun unsigned-byte-32-test (value temp not-p target not-target)
  (multiple-value-bind (yep nope)
                       (if not-p
                           (values not-target target)
                           (values target not-target))
    (assemble ()
      ;; Is it a fixnum?
      (inst and temp value fixnum-tag-mask)
      (inst beq temp fixnum)
      (move temp value t)

      ;; If not, is it an other pointer?
      (inst and temp value lowtag-mask)
      (inst xor temp other-pointer-lowtag)
      (inst bne temp nope)
      (inst nop)
      ;; Get the header.
      (loadw temp value 0 other-pointer-lowtag)
      ;; Is it one?
      (inst xor temp (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst beq temp single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst xor temp (logxor (+ (ash 1 n-widetag-bits) bignum-widetag)
                             (+ (ash 2 n-widetag-bits) bignum-widetag)))
      (inst bne temp nope)
      ;; Get the second digit.
      (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 32).
      (inst beq temp yep)
      (inst nop)
      (inst b nope)

      SINGLE-WORD
      ;; Get the single digit.
      (loadw temp value bignum-digits-offset other-pointer-lowtag)

      ;; positive implies (unsigned-byte 32).
      FIXNUM
      (if not-p
          (inst bltz temp target)
          (inst bgez temp target))
      (inst nop)))
  (values))

(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (unsigned-byte-32-test value temp not-p target not-target)
    NOT-TARGET))

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
    (let ((loose (generate-error-code vop object-not-unsigned-byte-32-error
                                      value)))
      (unsigned-byte-32-test value temp t loose okay))
    OKAY
    (move result value)))

;;; Because of our LOWTAG representation, SYMBOLP and CONSP are
;;; slightly more complex:
;;;
;;; * SYMBOLP is true if the object has SYMBOL-HEADER-WIDETAG or is EQ
;;; to NIL;
;;;
;;; * CONSP is true if the object has LIST-POINTER-LOWTAG and is not
;;; EQ to NIL.
;;;
;;; [ FIXME: This comment should not really be here, in the bowels of
;;; the MIPS type-vops, but where should it be?]
(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (inst beq value null-tn (if not-p drop-thru target))
    (test-type value target not-p (symbol-header-widetag) :temp temp)
    DROP-THRU))

(define-vop (check-symbol check-type)
  (:generator 12
    (inst beq value null-tn drop-thru)
    (let ((error (generate-error-code vop object-not-symbol-error value)))
      (test-type value error t (symbol-header-widetag) :temp temp))
    DROP-THRU
    (move result value)))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (inst beq value null-tn (if not-p target drop-thru))
    (test-type value target not-p (list-pointer-lowtag) :temp temp)
    DROP-THRU))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop object-not-cons-error value)))
      (inst beq value null-tn error)
      (test-type value error t (list-pointer-lowtag) :temp temp))
    (move result value)))
