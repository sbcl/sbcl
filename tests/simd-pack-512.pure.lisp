;;;; Potentially side-effectful tests of the simd-pack infrastructure.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

#-sb-simd-pack-512 (invoke-restart 'run-tests::skip-file)

(when (zerop (sb-alien:extern-alien "avx512_supported" int))
  (format t "~&INFO: simd-pack-512 not supported")
  (invoke-restart 'run-tests::skip-file))


(defun %simd-pack-512-singles (pack)
  (values (sb-vm::%simd-pack-ref-single pack 0)
          (sb-vm::%simd-pack-ref-single pack 1)
          (sb-vm::%simd-pack-ref-single pack 2)
          (sb-vm::%simd-pack-ref-single pack 3)
          (sb-vm::%simd-pack-ref-single pack 4)
          (sb-vm::%simd-pack-ref-single pack 5)
          (sb-vm::%simd-pack-ref-single pack 6)
          (sb-vm::%simd-pack-ref-single pack 7)
          (sb-vm::%simd-pack-ref-single pack 8)
          (sb-vm::%simd-pack-ref-single pack 9)
          (sb-vm::%simd-pack-ref-single pack 10)
          (sb-vm::%simd-pack-ref-single pack 11)
          (sb-vm::%simd-pack-ref-single pack 12)
          (sb-vm::%simd-pack-ref-single pack 13)
          (sb-vm::%simd-pack-ref-single pack 14)
          (sb-vm::%simd-pack-ref-single pack 15)))


(defun %simd-pack-512-doubles (pack)
  (values (sb-vm::%simd-pack-ref-double pack 0)
          (sb-vm::%simd-pack-ref-double pack 1)
          (sb-vm::%simd-pack-ref-double pack 2)
          (sb-vm::%simd-pack-ref-double pack 3)
          (sb-vm::%simd-pack-ref-double pack 4)
          (sb-vm::%simd-pack-ref-double pack 5)
          (sb-vm::%simd-pack-ref-double pack 6)
          (sb-vm::%simd-pack-ref-double pack 7)))

(defun make-constant-packs ()
  (values (sb-ext:%make-simd-pack-512-ub64 1 2 3 4 5 6 7 8)
          (sb-ext:%make-simd-pack-512-ub32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
          (sb-ext:%make-simd-pack-512-ub64 (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1))

          (sb-ext:%make-simd-pack-512-single 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0
                                             1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)
          (sb-ext:%make-simd-pack-512-single 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0
                                             0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
          (sb-ext:%make-simd-pack-512-single (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1))

          (sb-ext:%make-simd-pack-512-double 1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0)
          (sb-ext:%make-simd-pack-512-double 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
          (sb-ext:%make-simd-pack-512-double (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1)))))


(with-test (:name :compile-simd-pack-512-512)
  (multiple-value-bind (i i0 i-1
                        f f0 f-1
                        d d0 d-1)
      (make-constant-packs)
    (loop for (p0 p1 p2 p3 p4 p5 p6 p7) in (list '(1 2 3 4 5 6 7 8) '(0 0 0 0 0 0 0 0)
                                                 (list (ldb (byte 64 0) -1)
                                                       (ldb (byte 64 0) -1)
                                                       (ldb (byte 64 0) -1)
                                                       (ldb (byte 64 0) -1)
                                                       (ldb (byte 64 0) -1)
                                                       (ldb (byte 64 0) -1)
                                                       (ldb (byte 64 0) -1)
                                                       (ldb (byte 64 0) -1)))
          for pack in (list i i0 i-1)
          do (print (list p0 p1 p2 p3 p4 p5 p6 p7))
             (assert (eql p0 (sb-kernel:%simd-pack-512-0 pack)))
             (assert (eql p1 (sb-kernel:%simd-pack-512-1 pack)))
             (assert (eql p2 (sb-kernel:%simd-pack-512-2 pack)))
             (assert (eql p3 (sb-kernel:%simd-pack-512-3 pack)))
             (assert (eql p4 (sb-kernel:%simd-pack-512-4 pack)))
             (assert (eql p5 (sb-kernel:%simd-pack-512-5 pack)))
             (assert (eql p6 (sb-kernel:%simd-pack-512-6 pack)))
             (assert (eql p7 (sb-kernel:%simd-pack-512-7 pack))))
    (loop for expected in (list '(1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0
                                  1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)
                                '(0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0
                                  0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
                                (make-list
                                 16 :initial-element (sb-kernel:make-single-float -1)))
          for pack in (list f f0 f-1)
          do (assert (every #'eql expected
                            (multiple-value-list (%simd-pack-512-singles pack)))))
    (loop for expected in (list '(1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0)
                                '(0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0)
                                (make-list
                                 8 :initial-element (sb-kernel:make-double-float
                                                     -1 (ldb (byte 32 0) -1))))
          for pack in (list d d0 d-1)
          do (assert (every #'eql expected
                            (multiple-value-list (%simd-pack-512-doubles pack)))))
    ))

(with-test (:name (simd-pack-512 print :smoke))
  (let ((packs (multiple-value-list (make-constant-packs))))
    (flet ((print-them (expect)
             (dolist (pack packs)
               (flet ((do-it ()
                        (with-output-to-string (stream)
                          (write pack :stream stream :pretty t :escape nil))))
                 (case expect
                   (print-not-readable
                    (assert-error (do-it) print-not-readable))
                   (t
                    (do-it)))))))
      ;; Default
      (print-them t)
      ;; Readably
      (let ((*print-readably* t)
            (*read-eval* t))
        (print-them t))
      ;; Want readably but can't without *READ-EVAL*.
      (let ((*print-readably* t)
            (*read-eval* nil))
        (print-them 'print-not-readable)))))

(defvar *tmp-filename* (scratch-file-name))

(defvar *pack*)
(with-test (:name :load-simd-pack-512-int)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-ext:%make-simd-pack-512-ub64 2 4 8 16 2 4 8 16)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-ext:simd-pack-512 (unsigned-byte 64))))
             (assert (= 2  (sb-kernel:%simd-pack-512-0 *pack*)))
             (assert (= 4  (sb-kernel:%simd-pack-512-1 *pack*)))
             (assert (= 8  (sb-kernel:%simd-pack-512-2 *pack*)))
             (assert (= 16 (sb-kernel:%simd-pack-512-3 *pack*)))
             (assert (= 2  (sb-kernel:%simd-pack-512-4 *pack*)))
             (assert (= 4  (sb-kernel:%simd-pack-512-5 *pack*)))
             (assert (= 8  (sb-kernel:%simd-pack-512-6 *pack*)))
             (assert (= 16 (sb-kernel:%simd-pack-512-7 *pack*)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

(with-test (:name :load-simd-pack-512-single)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-ext:%make-simd-pack-512-single 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0
                                                            1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-ext:simd-pack-512 single-float)))
             (assert (equal (multiple-value-list (%simd-pack-512-singles *pack*))
                            '(1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

(with-test (:name :load-simd-pack-512-double)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-ext:%make-simd-pack-512-double 1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-ext:simd-pack-512 double-float)))
             (assert (equal (multiple-value-list (%simd-pack-512-doubles *pack*))
                            '(1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))


(with-test (:name :spilling)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (declare ((sb-ext:simd-pack-512 (unsigned-byte 64)) x))
         (eval y)
         (list (sb-kernel:%simd-pack-512-0 x)
               (sb-kernel:%simd-pack-512-1 x)
               (sb-kernel:%simd-pack-512-2 x)
               (sb-kernel:%simd-pack-512-3 x)
               (sb-kernel:%simd-pack-512-4 x)
               (sb-kernel:%simd-pack-512-5 x)
               (sb-kernel:%simd-pack-512-6 x)
               (sb-kernel:%simd-pack-512-7 x) y))
    (((sb-ext:%make-simd-pack-512-ub64 1 2 3 4 5 6 7 8) 0) '(1 2 3 4 5 6 7 8 0) :test #'equal)))

(with-test (:name (simd-pack-512 subtypep :smoke))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (unsigned-byte 8))  'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (unsigned-byte 16)) 'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (unsigned-byte 32)) 'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (unsigned-byte 64)) 'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (signed-byte 8))  'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (signed-byte 16)) 'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (signed-byte 32)) 'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (signed-byte 64)) 'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 single-float) 'simd-pack-512))
  (assert-tri-eq t t (subtypep '(simd-pack-512 double-float) 'simd-pack-512))
  (assert-tri-eq nil t (subtypep 'simd-pack-512 '(simd-pack-512 (unsigned-byte 64))))
  (assert-tri-eq nil t (subtypep 'simd-pack-512 '(simd-pack-512 single-float)))
  (assert-tri-eq nil t (subtypep 'simd-pack-512 '(simd-pack-512 double-float)))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (unsigned-byte 64))
                               '(or (simd-pack-512 (unsigned-byte 64)) (simd-pack-512 single-float))))
  (assert-tri-eq t t (subtypep '(simd-pack-512 (unsigned-byte 64))
                               '(or (simd-pack-512 (unsigned-byte 64)) (simd-pack-512 double-float))))
  (assert-tri-eq nil t (subtypep '(simd-pack-512 (unsigned-byte 64))
                                 '(or (simd-pack-512 single-float) (simd-pack-512 double-float))))
  (assert-tri-eq nil t (subtypep '(or (simd-pack-512 (unsigned-byte 64)) (simd-pack-512 single-float))
                                 '(simd-pack-512 (unsigned-byte 64))))
  (assert-tri-eq nil t (subtypep '(or (simd-pack-512 (unsigned-byte 64)) (simd-pack-512 double-float))
                                 '(simd-pack-512 (unsigned-byte 64))))
  (assert-tri-eq nil t (subtypep '(or (simd-pack-512 single-float) (simd-pack-512 double-float))
                                 '(simd-pack-512 (unsigned-byte 64)))))

(with-test (:name (simd-pack-512 :ctype-unparse :smoke))
  (flet ((unparsed (s) (sb-kernel:type-specifier (sb-kernel:specifier-type s))))
    (assert (equal (unparsed 'simd-pack-512) 'simd-pack-512))
    (assert (equal (unparsed '(simd-pack-512 (unsigned-byte 8)))  '(simd-pack-512 (unsigned-byte 8))))
    (assert (equal (unparsed '(simd-pack-512 (unsigned-byte 16))) '(simd-pack-512 (unsigned-byte 16))))
    (assert (equal (unparsed '(simd-pack-512 (unsigned-byte 32))) '(simd-pack-512 (unsigned-byte 32))))
    (assert (equal (unparsed '(simd-pack-512 (unsigned-byte 64))) '(simd-pack-512 (unsigned-byte 64))))
    (assert (equal (unparsed '(simd-pack-512 (signed-byte 8)))  '(simd-pack-512 (signed-byte 8))))
    (assert (equal (unparsed '(simd-pack-512 (signed-byte 16))) '(simd-pack-512 (signed-byte 16))))
    (assert (equal (unparsed '(simd-pack-512 (signed-byte 32))) '(simd-pack-512 (signed-byte 32))))
    (assert (equal (unparsed '(simd-pack-512 (signed-byte 64))) '(simd-pack-512 (signed-byte 64))))
    (assert (equal (unparsed '(simd-pack-512 single-float)) '(simd-pack-512 single-float)))
    (assert (equal (unparsed '(simd-pack-512 double-float)) '(simd-pack-512 double-float)))
    (assert (equal (unparsed '(or (simd-pack-512 (unsigned-byte 64)) (simd-pack-512 double-float)))
                   ;; depends on *SIMD-PACK-ELEMENT-TYPES* order
                   '(or (simd-pack-512 double-float) (simd-pack-512 (unsigned-byte 64)))))
    (assert (equal (unparsed '(or
                               (simd-pack-512 (unsigned-byte 8))
                               (simd-pack-512 (unsigned-byte 16))
                               (simd-pack-512 (unsigned-byte 32))
                               (simd-pack-512 (unsigned-byte 64))
                               (simd-pack-512 (signed-byte 8))
                               (simd-pack-512 (signed-byte 16))
                               (simd-pack-512 (signed-byte 32))
                               (simd-pack-512 (signed-byte 64))
                               (simd-pack-512 single-float)
                               (simd-pack-512 double-float)))
                   'simd-pack-512))))

(with-test (:name :simd-pack-512-type-errors)
  (locally (declare (muffle-conditions warning))
    ;; Bignum overflow
    (assert-error (sb-ext:%make-simd-pack-512-ub64
                   (1+ (ldb (byte 64 0) -1)) 0 0 0 0 0 0 0)
                  type-error)
    ;; Float mismatch
    (assert-error (sb-ext:%make-simd-pack-512-single
                   1d0 0f0 0f0 0f0 0f0 0f0 0f0 0f0
                   0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
                  type-error)))

;; evex patch
(cl:in-package "SB-VM")

(macrolet ((def (name)
             `(progn
                (sb-c::defknown ,name ()
                    (unsigned-byte 64)
                  (sb-c::flushable sb-c::movable))
                (defun ,name ()
                  (error ,(format nil "~A stub" name))))))
  (def %test-evex-high-regs)
  (def %test-evex-disp8)
  (def %test-evex-disp-vector-lengths)
  (def %test-evex-disp-negative)
  (def %test-evex-disp-nonmultiple)
  (def %test-evex-disp-large)
  (def %test-evex-vpmovzx-vpslldq-disassem)
  (def %test-evex-high-registers-poke))

(define-vop (%test-evex-high-regs)
  (:translate %test-evex-high-regs)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 16) z16)
  (:temporary (:sc single-avx512-reg :offset 17) z17)
  (:temporary (:sc single-avx512-reg :offset 18) z18)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vaddps z16 z17 z18)
    (inst xor :dword res res)))

(define-vop (%test-evex-disp8)
  (:translate %test-evex-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vmovdqu64 zmm (ea 64 rsp))))

(define-vop (%test-evex-disp-vector-lengths)
  (:translate %test-evex-disp-vector-lengths)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm)
  (:temporary (:sc single-avx2-reg :offset 1) ymm)
  (:temporary (:sc single-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vmovdqu64 xmm (ea 16 rsp))
    (inst vmovdqu64 ymm (ea 32 rsp))
    (inst vmovdqu64 zmm (ea 64 rsp))))

(define-vop (%test-evex-disp-negative)
  (:translate %test-evex-disp-negative)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vmovdqu64 zmm (ea -64 rsp))))

(define-vop (%test-evex-disp-nonmultiple)
  (:translate %test-evex-disp-nonmultiple)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vmovdqu64 zmm (ea 65 rsp))))

(define-vop (%test-evex-disp-large)
  (:translate %test-evex-disp-large)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vmovdqu64 zmm (ea 8192 rsp))))

(define-vop (%test-evex-vpmovzx-vpslldq-disassem)
  (:translate %test-evex-vpmovzx-vpslldq-disassem)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg :offset rax-offset) rax)
  (:temporary (:sc complex-double-reg :offset 10) xmm)
  (:temporary (:sc complex-double-reg :offset 30) xmm2)
  (:temporary (:sc int-avx512-reg :offset 20) zmm)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vpmovzxbd zmm xmm)
    (inst vmovdqu64 zmm (ea 8192 rax))
    (inst vmovdqu64 zmm (ea 65 rax))
    (inst vpinsrq  xmm xmm rax 1)
    (inst vpinsrq xmm2 xmm2 rax 1)
    (inst vpslldq xmm2 xmm2 1)
    (inst vpand xmm xmm2 xmm2)))

(define-vop (%test-evex-high-registers-poke)
  (:translate %test-evex-high-registers-poke)
  (:policy :fast-safe)
  (:temporary (:sc complex-double-reg :offset 16) xmm16)
  (:temporary (:sc complex-double-reg :offset 30) xmm30)
  (:temporary (:sc complex-double-reg :offset 31) xmm31)
  (:temporary (:sc single-avx2-reg :offset 17) ymm17)
  (:temporary (:sc single-avx2-reg :offset 28) ymm28)
  (:temporary (:sc int-avx512-reg :offset 18) zmm18)
  (:temporary (:sc int-avx512-reg :offset 29) zmm29)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vpslldq xmm16 xmm16 2)
    (inst vpslldq xmm31 xmm31 1)
    (inst vpsrldq xmm16 xmm30 8)
    (inst vpslld  zmm29 zmm18 3)
    (inst vpsrlq  zmm18 zmm29 5)
    (inst vpsraq  zmm29 zmm18 7)
    (inst vpmovzxbd zmm29 xmm30)
    (inst vpmovzxbd zmm18 xmm16)
    (inst vpmovsxbd zmm29 xmm16)
    (inst vpmovsxbw zmm29 ymm17)
    (inst vpmovzxbw zmm18 ymm28)
    (inst vpmovzxbq zmm29 xmm16)
    (inst vpmovsxdq zmm29 ymm17)
    (inst vpmovzxwd zmm18 ymm28)
    (inst vpmovzxwq zmm29 xmm16)))

(cl:in-package :test-util)

(with-test (:name :evex-high-register-disassembly)
  (let* ((fun (compile nil
                       '(lambda ()
                         (sb-vm::%test-evex-high-regs))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    ;; These names can only appear if the disassembler correctly
    ;; handles EVEX R', V', and X-as-B'.
    (assert (search "ZMM16" text))
    (assert (search "ZMM17" text))
    (assert (search "ZMM18" text))
    ;; Ideally we see a decoded instruction, not raw EVEX bytes.
    (assert (search "VADDPS" text))
    ;; While development, the decoder was a bit too broad
    (assert (not (search "VADDPS-MASKED" text)))))

#| tests for evex compressed displacement patch:

   EVEX vector lengths
   signed negative compressed displacement
   non-compressible displacement fallback to disp32
   compressible displacement too large for disp8
|#

(with-test (:name :evex-compressed-displacement-vector-lengths)
  (let* ((fun (compile nil
                       '(lambda ()
                         (sb-vm::%test-evex-disp-vector-lengths))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "VMOVDQU64 XMM0, [RSP+16]" text))
    (assert (search "VMOVDQU64 YMM1, [RSP+32]" text))
    (assert (search "VMOVDQU64 ZMM2, [RSP+64]" text))))

(with-test (:name :evex-compressed-displacement)
  (let* ((fun (compile nil
                       '(lambda ()
                         (sb-vm::%test-evex-disp8))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    ;; The disassembler must scale EVEX disp8 by 64.
    (assert (search "VMOVDQU64 ZMM0, [RSP+64]" text))
    ;; A failure mode is showing the unscaled compressed byte instead:
    (assert (not (search "[RSP+1]" text)))))

(with-test (:name :evex-compressed-displacement-negative)
  (let* ((fun (compile nil
                       '(lambda ()
                         (sb-vm::%test-evex-disp-negative))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "VMOVDQU64 ZMM0, [RSP-64]" text))))

(with-test (:name :evex-compressed-displacement-nonmultiple)
  (let* ((fun (compile nil
                       '(lambda ()
                         (sb-vm::%test-evex-disp-nonmultiple))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "VMOVDQU64 ZMM0, [RSP+65]" text))))

(with-test (:name :evex-compressed-displacement-large)
  (let* ((fun (compile nil
                       '(lambda ()
                         (sb-vm::%test-evex-disp-large))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "VMOVDQU64 ZMM0, [RSP+8192]" text))))

(with-test (:name :evex-disassembler-vpmov-and-high-reg-shifts)
  (let* ((fun (compile nil '(lambda () (sb-vm::%test-evex-vpmovzx-vpslldq-disassem))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "VPMOVZXBD ZMM20, XMM10" text))
    (assert (search "VMOVDQU64 ZMM20, [RAX+8192]" text))
    (assert (search "VMOVDQU64 ZMM20, [RAX+65]" text))
    (assert (search "VPINSRQ XMM10, XMM10, RAX, 1" text))
    (assert (search "VPINSRQ XMM30, XMM30, RAX, 1" text))
    (assert (search "VPSLLDQ XMM30, XMM30, 1" text))
    (assert (search "VPANDD XMM10, XMM30, XMM30" text))))

(with-test (:name :evex-disassembler-high-registers-poke)
  (let* ((fun (compile nil '(lambda () (sb-vm::%test-evex-high-registers-poke))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "VPSLLDQ XMM16, XMM16, 2" text))
    (assert (search "VPSLLDQ XMM31, XMM31, 1" text))
    (assert (search "VPSRLDQ XMM16, XMM30, 8" text))
    (assert (search "VPSLLD ZMM29, ZMM18, 3" text))
    (assert (search "VPSRLQ ZMM18, ZMM29, 5" text))
    (assert (search "VPSRAQ ZMM29, ZMM18, 7" text))
    (assert (search "VPMOVZXBD ZMM29, XMM30" text))
    (assert (search "VPMOVZXBD ZMM18, XMM16" text))
    (assert (search "VPMOVSXBD ZMM29, XMM16" text))
    (assert (search "VPMOVSXBW ZMM29, YMM17" text))
    (assert (search "VPMOVZXBW ZMM18, YMM28" text))
    (assert (search "VPMOVZXBQ ZMM29, XMM16" text))
    (assert (search "VPMOVSXDQ ZMM29, YMM17" text))
    (assert (search "VPMOVZXWD ZMM18, YMM28" text))
    (assert (search "VPMOVZXWQ ZMM29, XMM16" text))))
