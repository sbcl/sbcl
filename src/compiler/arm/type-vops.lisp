;;;; type testing and checking VOPs for the ARM VM

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
  (declare (ignore temp))
  (assemble ()
    (inst tst value fixnum-tag-mask)
    (inst b (if not-p :ne :eq) target)))

(defun %test-fixnum-and-headers (value temp target not-p headers &key value-tn-ref immediate-tested)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst ands temp value fixnum-tag-mask)
      (inst b :eq (if not-p drop-through target)))
    (%test-headers value temp target not-p nil headers
                   :drop-through drop-through
                   :value-tn-ref value-tn-ref
                   :immediate-tested immediate-tested)))

(defun %test-immediate (value temp target not-p immediate &key value-tn-ref)
  (declare (ignore value-tn-ref))
  (assemble ()
    (inst and temp value widetag-mask)
    (inst cmp temp immediate)
    (inst b (if not-p :ne :eq) target)))

(defun %test-lowtag (value temp target not-p lowtag &key value-tn-ref)
  (declare (ignore value-tn-ref))
  (assemble ()
    (inst and temp value lowtag-mask)
    (inst cmp temp lowtag)
    (inst b (if not-p :ne :eq) target)))

(defun %test-headers (value temp target not-p function-p headers
                      &key (drop-through (gen-label))
                           value-tn-ref immediate-tested)
  (declare (ignore value-tn-ref immediate-tested))
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
                  (inst and temp temp (ldb (byte 8 0) (logeqv header (cadr remaining))))
                  (inst cmp temp (ldb (byte 8 0) (logand header (cadr remaining))))
                  (inst b (if not-p :ne :eq) target)
                  (return))
                 (t
                  (inst cmp temp header)
                  (if last
                      (inst b (if not-p :ne :eq) target)
                      (inst b :eq when-true)))))
              (t
               (let ((start (car header))
                     (end (cdr header)))
                 (cond
                   ((and last (not (= start bignum-widetag))
                         (= (+ start 4) end)
                         (= (logcount (logxor start end)) 1))
                    (inst and temp temp (ldb (byte 8 0) (logeqv start end)))
                    (inst cmp temp (ldb (byte 8 0) (logand start end)))
                    (inst b (if not-p :ne :eq) target))
                   ((and (not last) (null (cddr remaining))
                         (= (+ start 4) end) (= (logcount (logxor start end)) 1)
                         (listp (cadr remaining))
                         (= (+ (caadr remaining) 4) (cdadr remaining))
                         (= (logcount (logxor (caadr remaining) (cdadr remaining))) 1)
                         (= (logcount (logxor (caadr remaining) start)) 1))
                    (inst and temp temp (ldb (byte 8 0) (logeqv start (cdadr remaining))))
                    (inst cmp temp (ldb (byte 8 0) (logand start (cdadr remaining))))
                    (inst b (if not-p :ne :eq) target)
                    (return))
                   (t
                    (unless (= start bignum-widetag)
                      (inst cmp temp start)
                      (if (= end complex-array-widetag)
                          (progn
                            (aver last)
                            (inst b (if not-p :lt :ge) target))
                          (inst b :lt when-false)))
                    (unless (= end complex-array-widetag)
                      (inst cmp temp end)
                      (if last
                          (inst b (if not-p :gt :le) target)
                          (inst b :le when-true))))))))))
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
      (inst ands temp value fixnum-tag-mask)
      (inst b :eq yep)
      (test-type value temp nope t (other-pointer-lowtag))
      (loadw temp value 0 other-pointer-lowtag)
      ;; (+ (ash 1 n-widetag-bits) bignum-widetag) does not fit into a single immediate
      (inst eor temp temp (ash 1 n-widetag-bits))
      (inst eors temp temp bignum-widetag)
      (inst b (if not-p :ne :eq) target)))
  (values))

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 10
   (let ((not-target (gen-label)))
     (signed-byte-32-test value temp not-p target not-target)
     (emit-label not-target))))

;;; An (UNSIGNED-BYTE 32) can be represented with either a positive
;;; fixnum, a bignum with exactly one positive digit, or a bignum with
;;; exactly two digits and the second digit all zeros.
(defun unsigned-byte-32-test (value temp not-p target not-target)
  (let ((single-word (gen-label))
        (fixnum (gen-label)))
    (multiple-value-bind (yep nope)
        (if not-p
            (values not-target target)
            (values target not-target))
      (assemble ()
        ;; Is it a fixnum?
        (move temp value)
        (%test-fixnum temp nil fixnum nil)

        ;; If not, is it an other pointer?
        (test-type value temp nope t (other-pointer-lowtag))
        ;; Get the header.
        (loadw temp value 0 other-pointer-lowtag)
        ;; Is it one?
        ;; (+ (ash 1 n-widetag-bits) bignum-widetag) does not fit into a single immediate
        (inst eor temp temp (ash 1 n-widetag-bits))
        (inst eors temp temp bignum-widetag)
        (inst b :eq single-word)
        ;; If it's other than two, we can't be an (unsigned-byte 32)
        (inst eors temp temp (logxor (+ (ash 1 n-widetag-bits) bignum-widetag)
                                     (+ (ash 2 n-widetag-bits) bignum-widetag)))
        (inst b :ne nope)
        ;; Get the second digit.
        (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
        ;; All zeros, its an (unsigned-byte 32).
        (inst cmp temp 0)
        (inst b :eq yep)
        (inst b nope)

        (emit-label single-word)
        ;; Get the single digit.
        (loadw temp value bignum-digits-offset other-pointer-lowtag)

        ;; positive implies (unsigned-byte 32).
        (emit-label fixnum)
        (inst cmp temp 0)
        (if not-p
            (inst b :lt target)
            (inst b :ge target))))
    (values)))

(define-vop (unsigned-byte-32-p type-predicate)
  (:translate unsigned-byte-32-p)
  (:generator 10
   (let ((not-target (gen-label)))
     (unsigned-byte-32-test value temp not-p target not-target)
     (emit-label not-target))))

;;;; List/symbol types:
;;;
;;; symbolp (or symbol (eq nil))
;;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
    (let* ((drop-thru (gen-label))
           (is-symbol-label (if not-p drop-thru target)))
      (inst cmp value null-tn)
      (inst b :eq is-symbol-label)
      (test-type value temp target not-p (symbol-widetag))
      (emit-label drop-thru))))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
    (let* ((drop-thru (gen-label))
           (is-not-cons-label (if not-p target drop-thru)))
      (inst cmp value null-tn)
      (inst b :eq is-not-cons-label)
      (test-type value temp target not-p (list-pointer-lowtag))
      (emit-label drop-thru))))
