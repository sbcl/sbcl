;;;; type testing and checking VOPs for the PPC VM

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
    (inst andi. temp value fixnum-tag-mask)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-fixnum-and-headers (value target not-p headers &key temp)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst andi. temp value fixnum-tag-mask)
      (inst beq (if not-p drop-through target)))
    (%test-headers value target not-p nil headers
                   :drop-through drop-through :temp temp)))

(defun %test-immediate (value target not-p immediate &key temp)
  (assemble ()
    (inst andi. temp value widetag-mask)
    (inst cmpwi temp immediate)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-lowtag (value target not-p lowtag &key temp)
  (assemble ()
    (inst andi. temp value lowtag-mask)
    (inst cmpwi temp lowtag)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-headers (value target not-p function-p headers
                      &key temp (drop-through (gen-label)))
    (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (when-true when-false)
        (if not-p
            (values drop-through target)
            (values target drop-through))
      (assemble ()
        (%test-lowtag value when-false t lowtag :temp temp)
        (load-type temp value (- lowtag))
        (do ((remaining headers (cdr remaining)))
            ((null remaining))
          (let ((header (car remaining))
                (last (null (cdr remaining))))
            (cond
              ((atom header)
               (cond
                 ((and (not last) (null (cddr remaining))
                       (atom (cadr remaining))
                       (= (logcount (logxor header (cadr remaining))) 1))
                  (inst andi. temp temp (ldb (byte 8 0) (logeqv header (cadr remaining))))
                  (inst cmpwi temp (ldb (byte 8 0) (logand header (cadr remaining))))
                  (inst b? (if not-p :ne :eq) target)
                  (return))
                 (t
                  (inst cmpwi temp header)
                  (if last
                      (inst b? (if not-p :ne :eq) target)
                      (inst beq when-true)))))
              (t
               (let ((start (car header))
                     (end (cdr header)))
                 (cond
                   ((and last (not (= start bignum-widetag))
                         (= (+ start 4) end)
                         (= (logcount (logxor start end)) 1))
                    (inst andi. temp temp (ldb (byte 8 0) (logeqv start end)))
                    (inst cmpwi temp (ldb (byte 8 0) (logand start end)))
                    (inst b? (if not-p :ne :eq) target))
                   ((and (not last) (null (cddr remaining))
                         (= (+ start 4) end) (= (logcount (logxor start end)) 1)
                         (listp (cadr remaining))
                         (= (+ (caadr remaining) 4) (cdadr remaining))
                         (= (logcount (logxor (caadr remaining) (cdadr remaining))) 1)
                         (= (logcount (logxor (caadr remaining) start)) 1))
                    (inst andi. temp temp (ldb (byte 8 0) (logeqv start (cdadr remaining))))
                    (inst cmpwi temp (ldb (byte 8 0) (logand start (cdadr remaining))))
                    (inst b? (if not-p :ne :eq) target)
                    (return))
                   (t
                    (unless (= start bignum-widetag)
                      (inst cmpwi temp start)
                      (if (= end complex-array-widetag)
                          (progn
                            (aver last)
                            (inst b? (if not-p :lt :ge) target))
                          (inst blt when-false)))
                    (unless (= end complex-array-widetag)
                      (inst cmpwi temp end)
                      (if last
                          (inst b? (if not-p :gt :le) target)
                          (inst ble when-true))))))))))
        (emit-label drop-through)))))

;;; Simple type checking and testing:
(define-vop (check-type)
  (:args (value :target result :scs (any-reg descriptor-reg)))
  (:results (result :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:save-p :compute-only))

(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (non-descriptor-reg)) temp))

(defun cost-to-test-types (type-codes)
  (+ (* 2 (length type-codes))
     (if (> (apply #'max type-codes) lowtag-limit) 7 2)))

(defmacro !define-type-vops (pred-name check-name ptype error-code
                             (&rest type-codes)
                             ;; KLUDGE: ideally, the compiler could
                             ;; derive that it can use the sneaky trap
                             ;; twice mechanism itself.  However, one
                             ;; thing at a time...
                             &key mask &allow-other-keys)
  (let ((cost (cost-to-test-types (mapcar #'eval type-codes))))
    `(progn
       ,@(when pred-name
           `((define-vop (,pred-name type-predicate)
               (:translate ,pred-name)
               (:generator ,cost
                 (test-type value target not-p (,@type-codes) :temp temp)))))
       ,@(when check-name
           `((define-vop (,check-name check-type)
               (:generator ,cost
                 ,@(if mask
                       `((inst andi. temp value ,mask)
                         (inst twi 0 value (error-number-or-lose ',error-code))
                         (inst twi :ne temp ,@(ecase mask
                                                ((fixnum-tag-mask) `(0))
                                                ((lowtag-mask) type-codes)))
                         (move result value))
                       `((let ((err-lab
                                (generate-error-code vop ',error-code value)))
                           (test-type value err-lab t (,@type-codes) :temp temp)
                           (move result value))))))))
       ,@(when ptype
           `((primitive-type-vop ,check-name (:check) ,ptype))))))

;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label)))
      (multiple-value-bind
          (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        (inst andi. temp value fixnum-tag-mask)
        (inst beq yep)
        (test-type value nope t (other-pointer-lowtag) :temp temp)
        (loadw temp value 0 other-pointer-lowtag)
        (inst cmpwi temp (+ (ash 1 n-widetag-bits)
                          bignum-widetag))
        (inst b? (if not-p :ne :eq) target)
        (emit-label not-target)))))

(define-vop (check-signed-byte-32 check-type)
  (:generator 45
    (let ((nope (generate-error-code vop 'object-not-signed-byte-32-error value))
          (yep (gen-label)))
      (inst andi. temp value fixnum-tag-mask)
      (inst beq yep)
      (test-type value nope t (other-pointer-lowtag) :temp temp)
      (loadw temp value 0 other-pointer-lowtag)
      (inst cmpwi temp (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst bne nope)
      (emit-label yep)
      (move result value))))


;;; An (unsigned-byte 32) can be represented with either a positive fixnum, a
;;; bignum with exactly one positive digit, or a bignum with exactly two digits
;;; and the second digit all zeros.

(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 45
    (let ((not-target (gen-label))
          (single-word (gen-label))
          (fixnum (gen-label)))
      (multiple-value-bind
          (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        ;; Is it a fixnum?
        (inst andi. temp value fixnum-tag-mask)
        (inst cmpwi :cr1 value 0)
        (inst beq fixnum)

        ;; If not, is it an other pointer?
        (test-type value nope t (other-pointer-lowtag) :temp temp)
        ;; Get the header.
        (loadw temp value 0 other-pointer-lowtag)
        ;; Is it one?
        (inst cmpwi temp (+ (ash 1 n-widetag-bits) bignum-widetag))
        (inst beq single-word)
        ;; If it's other than two, we can't be an (unsigned-byte 32)
        (inst cmpwi temp (+ (ash 2 n-widetag-bits) bignum-widetag))
        (inst bne nope)
        ;; Get the second digit.
        (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
        ;; All zeros, its an (unsigned-byte 32).
        (inst cmpwi temp 0)
        (inst beq yep)
        ;; Otherwise, it isn't.
        (inst b nope)

        (emit-label single-word)
        ;; Get the single digit.
        (loadw temp value bignum-digits-offset other-pointer-lowtag)
        (inst cmpwi :cr1 temp 0)

        ;; positive implies (unsigned-byte 32).
        (emit-label fixnum)
        (inst b?  :cr1 (if not-p :lt :ge) target)

        (emit-label not-target)))))

(define-vop (check-unsigned-byte-32 check-type)
  (:generator 45
    (let ((nope
           (generate-error-code vop 'object-not-unsigned-byte-32-error value))
          (yep (gen-label))
          (fixnum (gen-label))
          (single-word (gen-label)))
      ;; Is it a fixnum?
      (inst andi. temp value fixnum-tag-mask)
      (inst cmpwi :cr1 value 0)
      (inst beq fixnum)

      ;; If not, is it an other pointer?
      (test-type value nope t (other-pointer-lowtag) :temp temp)
      ;; Get the number of digits.
      (loadw temp value 0 other-pointer-lowtag)
      ;; Is it one?
      (inst cmpwi temp (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst beq single-word)
      ;; If it's other than two, we can't be an (unsigned-byte 32)
      (inst cmpwi temp (+ (ash 2 n-widetag-bits) bignum-widetag))
      (inst bne nope)
      ;; Get the second digit.
      (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
      ;; All zeros, its an (unsigned-byte 32).
      (inst cmpwi temp 0)
      (inst beq yep)
      ;; Otherwise, it isn't.
      (inst b nope)

      (emit-label single-word)
      ;; Get the single digit.
      (loadw temp value bignum-digits-offset other-pointer-lowtag)
      ;; positive implies (unsigned-byte 32).
      (inst cmpwi :cr1 temp 0)

      (emit-label fixnum)
      (inst blt :cr1 nope)

      (emit-label yep)
      (move result value))))




;;;; List/symbol types:
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (let* ((drop-thru (gen-label))
           (is-symbol-label (if not-p drop-thru target)))
      (inst cmpw value null-tn)
      (inst beq is-symbol-label)
      (test-type value target not-p (symbol-header-widetag) :temp temp)
      (emit-label drop-thru))))

(define-vop (check-symbol check-type)
  (:generator 12
    (let ((drop-thru (gen-label))
          (error (generate-error-code vop 'object-not-symbol-error value)))
      (inst cmpw value null-tn)
      (inst beq drop-thru)
      (test-type value error t (symbol-header-widetag) :temp temp)
      (emit-label drop-thru)
      (move result value))))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let* ((drop-thru (gen-label))
           (is-not-cons-label (if not-p target drop-thru)))
      (inst cmpw value null-tn)
      (inst beq is-not-cons-label)
      (test-type value target not-p (list-pointer-lowtag) :temp temp)
      (emit-label drop-thru))))

(define-vop (check-cons check-type)
  (:generator 8
    (let ((error (generate-error-code vop 'object-not-cons-error value)))
      (inst cmpw value null-tn)
      (inst beq error)
      (test-type value error t (list-pointer-lowtag) :temp temp)
      (move result value))))

