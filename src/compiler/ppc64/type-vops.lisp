;;;; type testing and checking VOPs for the PPC VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun %test-fixnum (value temp target not-p)
  (assemble ()
    (inst andi. temp value fixnum-tag-mask)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-fixnum-and-headers (value temp target not-p headers &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst andi. temp value fixnum-tag-mask)
      (inst beq (if not-p drop-through target)))
    (%test-headers value temp target not-p nil headers
                   :drop-through drop-through
                   :value-tn-ref value-tn-ref)))

(defun %test-fixnum-immediate-and-headers (value temp target not-p immediate headers
                                            &key value-tn-ref)
  (let ((drop-through (gen-label)))
    (inst andi. temp value fixnum-tag-mask)
    (inst b? :eq (if not-p drop-through target))
    (%test-immediate-and-headers value temp target not-p immediate headers
                                 :drop-through drop-through
                                 :value-tn-ref value-tn-ref)))

(defun %test-immediate-and-headers (value temp target not-p immediate headers
                                    &key (drop-through (gen-label)) value-tn-ref)
  (inst andi. temp value widetag-mask)
  (inst cmpwi temp immediate)
  (inst b? :eq (if not-p drop-through target))
  (%test-headers value temp target not-p nil headers
                 :drop-through drop-through :value-tn-ref value-tn-ref))

(defun %test-immediate (value temp target not-p immediate)
  (assemble ()
    (inst andi. temp value widetag-mask)
    (inst cmpwi temp immediate)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-lowtag (value temp target not-p lowtag)
  (assemble ()
    (inst andi. temp value lowtag-mask)
    (inst cmpwi temp lowtag)
    (inst b? (if not-p :ne :eq) target)))

(defun %test-headers (value temp target not-p function-p headers
                      &key (drop-through (gen-label)) value-tn-ref)
  (declare (ignore value-tn-ref))
  (let ((lowtag (if function-p fun-pointer-lowtag other-pointer-lowtag)))
    (multiple-value-bind (when-true when-false)
        (if not-p
            (values drop-through target)
            (values target drop-through))
      (assemble ()
        (%test-lowtag value temp when-false t lowtag)
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

;;;; Other integer ranges.

;;; A (SIGNED-BYTE 64) can be represented with either fixnum or a bignum with
;;; exactly one digit.

(define-vop (signed-byte-64-p type-predicate)
  (:translate signed-byte-64-p)
  (:generator 45
    (multiple-value-bind (yep nope)
        (if not-p
            (values not-target target)
            (values target not-target))
      (inst andi. temp value fixnum-tag-mask)
      (inst beq yep)
      (test-type value temp nope t (other-pointer-lowtag))
      (loadw temp value 0 other-pointer-lowtag)
      (inst cmpdi temp (+ (ash 1 n-widetag-bits) bignum-widetag))
      (inst b? (if not-p :ne :eq) target))
    not-target))

;;; An (unsigned-byte 64) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(define-vop (unsigned-byte-64-p type-predicate)
  (:translate unsigned-byte-64-p)
  (:generator 45
    (let ((not-target (gen-label))
          (single-word (gen-label))
          (fixnum (gen-label)))
      (multiple-value-bind (yep nope)
          (if not-p
              (values not-target target)
              (values target not-target))
        ;; Is it a fixnum?
        (inst andi. temp value fixnum-tag-mask)
        (inst cmpdi :cr1 value 0)
        (inst beq fixnum)

        ;; If not, is it an other pointer?
        (test-type value temp nope t (other-pointer-lowtag))
        ;; Get the header.
        (loadw temp value 0 other-pointer-lowtag)
        ;; Is it one?
        (inst cmpdi temp (+ (ash 1 n-widetag-bits) bignum-widetag))
        (inst beq single-word)
        ;; If it's other than two, we can't be an (unsigned-byte 64)
        (inst cmpdi temp (+ (ash 2 n-widetag-bits) bignum-widetag))
        (inst bne nope)
        ;; Get the second digit.
        (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
        ;; All zeros, its an (unsigned-byte 64).
        (inst cmpdi temp 0)
        (inst beq yep)
        ;; Otherwise, it isn't.
        (inst b nope)

        (emit-label single-word)
        ;; Get the single digit.
        (loadw temp value bignum-digits-offset other-pointer-lowtag)
        (inst cmpdi :cr1 temp 0)

        ;; positive implies (unsigned-byte 64).
        (emit-label fixnum)
        (inst b? :cr1 (if not-p :lt :ge) target)

        (emit-label not-target)))))

;;;; List/symbol types:
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (let* ((drop-thru (gen-label))
           (is-symbol-label (if not-p drop-thru target)))
      (inst cmpd value null-tn)
      (inst beq is-symbol-label)
      (test-type value temp target not-p (symbol-widetag))
      (emit-label drop-thru))))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let* ((drop-thru (gen-label))
           (is-not-cons-label (if not-p target drop-thru)))
      (inst cmpd value null-tn)
      (inst beq is-not-cons-label)
      (test-type value temp target not-p (list-pointer-lowtag))
      (emit-label drop-thru))))
