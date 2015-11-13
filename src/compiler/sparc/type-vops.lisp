;;;; type testing and checking VOPs for the Sparc VM

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
  (declare (ignore temp))
  (assemble ()
    (inst andcc zero-tn value fixnum-tag-mask)
    (if (member :sparc-v9 *backend-subfeatures*)
        (inst b (if not-p :ne :eq) target (if not-p :pn :pt))
        (inst b (if not-p :ne :eq) target))
    (inst nop)))

(defun %test-fixnum-and-headers (value target not-p headers
                                 &key temp)
  (let ((drop-through (gen-label)))
    (assemble ()
      (inst andcc zero-tn value fixnum-tag-mask)
      (inst b :eq (if not-p drop-through target)))
    (%test-headers value target not-p nil headers
                   :drop-through drop-through
                   :temp temp)))

(defun %test-immediate (value target not-p immediate &key temp)
  (assemble ()
    (inst and temp value widetag-mask)
    (inst cmp temp immediate)
    ;; FIXME: include SPARC-V9 magic
    (inst b (if not-p :ne :eq) target)
    (inst nop)))

(defun %test-lowtag (value target not-p lowtag
                     &key temp skip-nop)
  (assemble ()
    (inst and temp value lowtag-mask)
    (inst cmp temp lowtag)
    ;; FIXME: include SPARC-V9 magic
    (inst b (if not-p :ne :eq) target)
    (unless skip-nop
      (inst nop))))

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
                  (inst and temp temp (ldb (byte 8 0) (logeqv header (cadr remaining))))
                  (inst cmp temp (ldb (byte 8 0) (logand header (cadr remaining))))
                  (inst b (if not-p :ne :eq) target)
                  (return))
                 (t
                  (inst cmp temp header)
                  (if last
                      ;; FIXME: Some SPARC-V9 magic might not go amiss
                      ;; here, too, if I can figure out what it should
                      ;; be.
                      (inst b (if not-p :ne :eq) target)
                      (inst b :eq when-true)))))
              (t
               (let ((start (car header))
                     (end (cdr header)))
                 ;; FIXME: BIGNUM-WIDETAG here actually means (MIN
                 ;; <widetags>).
                 (cond
                   ;; FIXME: this doesn't catch the {0x2 0x6 0xA 0xE}
                   ;; group
                   ;;
                   ;; also FIXME: exuberant cut'n'paste between
                   ;; backends
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
        (inst nop)
        (emit-label drop-through)))))

;;;; Other integer ranges.

;;; A (signed-byte 32) can be represented with either fixnum or a
;;; bignum with exactly one digit.

(define-vop (signed-byte-32-p type-predicate)
  (:translate signed-byte-32-p)
  (:generator 45
              (let ((not-target (gen-label)))
                (multiple-value-bind
                      (yep nope)
                    (if not-p
                        (values not-target target)
                        (values target not-target))
                  (inst andcc zero-tn value fixnum-tag-mask)
                  (inst b :eq yep)
                  (test-type value nope t (other-pointer-lowtag) :temp temp)
                  (loadw temp value 0 other-pointer-lowtag)
                  (inst cmp temp (+ (ash 1 n-widetag-bits)
                                    bignum-widetag))
                  (inst b (if not-p :ne :eq) target)
                  (inst nop)
                  (emit-label not-target)))))




;;; An (unsigned-byte 32) can be represented with either a
;;; positive fixnum, a bignum with exactly one positive digit, or
;;; a bignum with exactly two digits and the second digit all
;;; zeros.

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
                  (inst andcc temp value fixnum-tag-mask)
                  (inst b :eq fixnum)
                  (inst cmp value)

                  ;; If not, is it an other pointer?
                  (test-type value nope t (other-pointer-lowtag) :temp temp)
                  ;; Get the header.
                  (loadw temp value 0 other-pointer-lowtag)
                  ;; Is it one?
                  (inst cmp temp (+ (ash 1 n-widetag-bits) bignum-widetag))
                  (inst b :eq single-word)
                  ;; If it's other than two, we can't be an
                  ;; (unsigned-byte 32)
                  (inst cmp temp (+ (ash 2 n-widetag-bits) bignum-widetag))
                  (inst b :ne nope)
                  ;; Get the second digit.
                  (loadw temp value (1+ bignum-digits-offset) other-pointer-lowtag)
                  ;; All zeros, its an (unsigned-byte 32).
                  (inst cmp temp)
                  (inst b :eq yep)
                  (inst nop)
                  ;; Otherwise, it isn't.
                  (inst b nope)
                  (inst nop)

                  (emit-label single-word)
                  ;; Get the single digit.
                  (loadw temp value bignum-digits-offset other-pointer-lowtag)
                  (inst cmp temp)

                  ;; positive implies (unsigned-byte 32).
                  (emit-label fixnum)
                  (inst b (if not-p :lt :ge) target)
                  (inst nop)

                  (emit-label not-target)))))

;;;; List/symbol types:

  ;; symbolp (or symbol (eq nil))
  ;; consp (and list (not (eq nil)))

(define-vop (symbolp type-predicate)
  (:translate symbolp)
  (:generator 12
              (let* ((drop-thru (gen-label))
                     (is-symbol-label (if not-p drop-thru target)))
                (inst cmp value null-tn)
                (inst b :eq is-symbol-label)
                (test-type value target not-p (symbol-header-widetag) :temp temp)
                (emit-label drop-thru))))

(define-vop (consp type-predicate)
  (:translate consp)
  (:generator 8
              (let* ((drop-thru (gen-label))
                     (is-not-cons-label (if not-p target drop-thru)))
                (inst cmp value null-tn)
                (inst b :eq is-not-cons-label)
                (test-type value target not-p (list-pointer-lowtag) :temp temp)
                (emit-label drop-thru))))
