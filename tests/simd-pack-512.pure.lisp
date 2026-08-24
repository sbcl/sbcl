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

;; evex patch - stubs

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
  (def %test-evex-high-registers-poke)
  (def %test-auto-promoted-vaddps)
  (def %test-auto-promoted-vaddpd)
  (def %test-auto-promoted-vpbroadcastq)
  (def %test-auto-promoted-vmovdqu)
  (def %test-evex-vpternlogd)
  (def %test-evex-vpermt2d)
  (def %test-evex-vblendmps)
  (def %test-evex-vpcmpd)
  (def %test-evex-vpmovqd)
  (def %test-auto-promoted-vmovaps-disp8)
  (def %test-auto-promoted-vmovaps-disp-nonmultiple)
  (def %test-broadcast-f32x4-disp8)
  (def %test-broadcast-f32x4-disp32)
  (def %test-broadcast-f64x4-disp8)
  (def %test-broadcast-f64x4-disp32)
  (def %test-compress-disp8)
  (def %test-compress-disp32)
  (def %test-expand-disp8)
  (def %test-expand-disp32)
  (def %test-vpcmpd-disp8)
  (def %test-vpcmpd-disp32)
  (def %test-vptestmd-disp8)
  (def %test-vptestmd-disp32)
  (def %test-vpmovqd-xmm-disp8)
  (def %test-vpmovqd-ymm-disp8)
  (def %test-vpmovqd-zmm-disp8)
  (def %test-vpmovqd-zmm-disp32)
  (def %test-vpmovsqb-xmm-disp8)
  (def %test-vpmovsqb-ymm-disp8)
  (def %test-vpmovsqb-zmm-disp8)
  (def %test-vpmovsqb-zmm-disp32)
  (def %test-vrcp14ps-xmm-disp8)
  (def %test-vrcp14ps-ymm-disp8)
  (def %test-vrcp14ps-zmm-disp8)
  (def %test-vrcp14ps-zmm-disp32)
  (def %test-vpabsq-zmm-disp8)
  (def %test-valignd-xmm-disp8)
  (def %test-valignd-ymm-disp8)
  (def %test-valignd-zmm-disp8)
  (def %test-valignd-zmm-disp32)
  (def %test-vrangess-disp8)
  (def %test-vrangess-disp32)
  (def %test-vrangesd-disp8)
  (def %test-vrndscaleps-xmm-disp8)
  (def %test-vrndscaleps-zmm-disp8)
  (def %test-vrndscaleps-zmm-disp32)
  (def %test-vrndscaless-disp8)
  (def %test-vrndscaless-disp32)
  (def %test-vrndscalesd-disp8)
  (def %test-vfixupimmps-xmm-disp8)
  (def %test-vfixupimmps-zmm-disp8)
  (def %test-vfixupimmps-zmm-disp32)
  (def %test-vfixupimmss-disp8)
  (def %test-vfixupimmss-disp32)
  (def %test-vfixupimmsd-disp8)
  (def %test-vreducess-disp8)
  (def %test-vreducess-disp32)
  (def %test-vreducesd-disp8)
  (def %test-vgetmantss-disp8)
  (def %test-vgetmantss-disp32)
  (def %test-vgetmantsd-disp8)
  (def %test-vgetexpss-disp8)
  (def %test-vgetexpss-disp32)
  (def %test-vgetexpsd-disp8)
  (def %test-vscalefps-xmm-disp8)
  (def %test-vscalefps-zmm-disp8)
  (def %test-vscalefps-zmm-disp32)
  (def %test-vscalefss-disp8)
  (def %test-vscalefss-disp32)
  (def %test-vscalefsd-disp8)
  (def %test-vcvtss2usi-disp8)
  (def %test-vcvtss2usi-disp32)
  (def %test-vcvtsd2usi-disp8)
  (def %test-vcvttss2usi-disp8)
  (def %test-vcvttsd2usi-disp8)
  (def %test-vcvtusi2sd-disp8)
  (def %test-vcvtusi2sd-disp32)
  (def %test-vcvtusi2ss-disp8))


;; instruction vops

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

(define-vop (%test-auto-promoted-vaddps)
  (:translate %test-auto-promoted-vaddps)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 16) z16)
  (:temporary (:sc single-avx512-reg :offset 17) z17)
  (:temporary (:sc single-avx512-reg :offset 18) z18)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vaddps z16 z17 z18)
    (inst xor :dword res res)))

;; needed for testing w-bit with auto-promoted instructions
(define-vop (%test-auto-promoted-vaddpd)
  (:translate %test-auto-promoted-vaddpd)
  (:policy :fast-safe)
  (:temporary (:sc double-avx512-reg :offset 16) z16)
  (:temporary (:sc double-avx512-reg :offset 17) z17)
  (:temporary (:sc double-avx512-reg :offset 18) z18)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vaddpd z16 z17 z18)
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vpbroadcastq)
  (:translate %test-auto-promoted-vpbroadcastq)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vpbroadcastq zmm (ea 64 rsp))))

(define-vop (%test-auto-promoted-vmovdqu)
  (:translate %test-auto-promoted-vmovdqu)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst xor :dword res res)
    (inst vmovdqu zmm (ea 64 rsp))))

(define-vop (%test-evex-vpternlogd)
  (:translate %test-evex-vpternlogd)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) z0)
  (:temporary (:sc int-avx512-reg :offset 1) z1)
  (:temporary (:sc int-avx512-reg :offset 2) z2)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpternlogd z0 z1 z2 #xFF)
    (inst xor :dword res res)))

;; needed for evex emitters and printers
(define-vop (%test-evex-vpermt2d)
  (:translate %test-evex-vpermt2d)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) z0)
  (:temporary (:sc int-avx512-reg :offset 1) z1)
  (:temporary (:sc int-avx512-reg :offset 2) z2)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpermt2d z0 z1 z2)
    (inst xor :dword res res)))

(define-vop (%test-evex-vblendmps)
  (:translate %test-evex-vblendmps)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 16) z16)
  (:temporary (:sc single-avx512-reg :offset 17) z17)
  (:temporary (:sc single-avx512-reg :offset 18) z18)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vblendmps z16 z17 z18)
    (inst xor :dword res res)))

(define-vop (%test-evex-vpcmpd)
  (:translate %test-evex-vpcmpd)
  (:policy :fast-safe)
  (:temporary (:sc mask-reg :offset 1) k1)
  (:temporary (:sc int-avx512-reg :offset 0) z0)
  (:temporary (:sc int-avx512-reg :offset 1) z1)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpcmpd k1 z0 z1 #x1)
    (inst xor :dword res res)))

(define-vop (%test-evex-vpmovqd)
  (:translate %test-evex-vpmovqd)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) z0)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovqd (ea 32 rsp) z0)
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vmovaps-disp8)
  (:translate %test-auto-promoted-vmovaps-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vmovaps zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vmovaps-disp-nonmultiple)
  (:translate %test-auto-promoted-vmovaps-disp-nonmultiple)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vmovaps zmm (ea 65 rsp))
    (inst xor :dword res res)))

(define-vop (%test-broadcast-f32x4-disp8)
  (:translate %test-broadcast-f32x4-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vbroadcastf32x4 zmm (ea 16 rsp))
    (inst xor :dword res res)))

(define-vop (%test-broadcast-f32x4-disp32)
  (:translate %test-broadcast-f32x4-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vbroadcastf32x4 zmm (ea 17 rsp))
    (inst xor :dword res res)))

(define-vop (%test-broadcast-f64x4-disp8)
  (:translate %test-broadcast-f64x4-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vbroadcastf64x4 zmm (ea 32 rsp))
    (inst xor :dword res res)))

(define-vop (%test-broadcast-f64x4-disp32)
  (:translate %test-broadcast-f64x4-disp32)
  (:policy :fast-safe)
  (:temporary (:sc double-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vbroadcastf64x4 zmm (ea 33 rsp))
    (inst xor :dword res res)))

(define-vop (%test-compress-disp8)
  (:translate %test-compress-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcompressps (ea 64 rsp) zmm)
    (inst xor :dword res res)))

(define-vop (%test-compress-disp32)
  (:translate %test-compress-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcompressps (ea 65 rsp) zmm)
    (inst xor :dword res res)))

(define-vop (%test-expand-disp8)
  (:translate %test-expand-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpexpandq zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-expand-disp32)
  (:translate %test-expand-disp32)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpexpandq zmm (ea 65 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vpcmpd-disp8)
  (:translate %test-vpcmpd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc mask-reg :offset 1) k1)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpcmpd k1 zmm (ea 64 rsp) #x1)
    (inst xor :dword res res)))

(define-vop (%test-vpcmpd-disp32)
  (:translate %test-vpcmpd-disp32)
  (:policy :fast-safe)
  (:temporary (:sc mask-reg :offset 1) k1)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpcmpd k1 zmm (ea 65 rsp) #x1)
    (inst xor :dword res res)))

(define-vop (%test-vptestmd-disp8)
  (:translate %test-vptestmd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc mask-reg :offset 1) k1)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vptestmd k1 zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vptestmd-disp32)
  (:translate %test-vptestmd-disp32)
  (:policy :fast-safe)
  (:temporary (:sc mask-reg :offset 1) k1)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vptestmd k1 zmm (ea 65 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vpmovqd-xmm-disp8)
  (:translate %test-vpmovqd-xmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovqd (ea 8 rsp) xmm)
    (inst xor :dword res res)))

(define-vop (%test-vpmovqd-ymm-disp8)
  (:translate %test-vpmovqd-ymm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx2-reg :offset 1) ymm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovqd (ea 16 rsp) ymm)
    (inst xor :dword res res)))

(define-vop (%test-vpmovqd-zmm-disp8)
  (:translate %test-vpmovqd-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovqd (ea 32 rsp) zmm)
    (inst xor :dword res res)))

(define-vop (%test-vpmovqd-zmm-disp32)
  (:translate %test-vpmovqd-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovqd (ea 33 rsp) zmm)
    (inst xor :dword res res)))

(define-vop (%test-vpmovsqb-xmm-disp8)
  (:translate %test-vpmovsqb-xmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovsqb (ea 2 rsp) xmm)
    (inst xor :dword res res)))

(define-vop (%test-vpmovsqb-ymm-disp8)
  (:translate %test-vpmovsqb-ymm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx2-reg :offset 1) ymm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovsqb (ea 4 rsp) ymm)
    (inst xor :dword res res)))

(define-vop (%test-vpmovsqb-zmm-disp8)
  (:translate %test-vpmovsqb-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovsqb (ea 8 rsp) zmm)
    (inst xor :dword res res)))

(define-vop (%test-vpmovsqb-zmm-disp32)
  (:translate %test-vpmovsqb-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovsqb (ea 9 rsp) zmm)
    (inst xor :dword res res)))

;; Full-vector 2-operand group: vrcp14ps (W=0)
(define-vop (%test-vrcp14ps-xmm-disp8)
  (:translate %test-vrcp14ps-xmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrcp14ps xmm (ea 16 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vrcp14ps-ymm-disp8)
  (:translate %test-vrcp14ps-ymm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx2-reg :offset 1) ymm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrcp14ps ymm (ea 32 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vrcp14ps-zmm-disp8)
  (:translate %test-vrcp14ps-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrcp14ps zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vrcp14ps-zmm-disp32)
  (:translate %test-vrcp14ps-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrcp14ps zmm (ea 65 rsp))
    (inst xor :dword res res)))

;; Ensure integer W=1 full-vector group also uses compressed disp
(define-vop (%test-vpabsq-zmm-disp8)
  (:translate %test-vpabsq-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 3) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpabsq zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-valignd-xmm-disp8)
  (:translate %test-valignd-xmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst valignd xmm0 xmm1 (ea 16 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-valignd-ymm-disp8)
  (:translate %test-valignd-ymm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx2-reg :offset 1) ymm1)
  (:temporary (:sc int-avx2-reg :offset 2) ymm2)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst valignd ymm1 ymm2 (ea 32 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-valignd-zmm-disp8)
  (:translate %test-valignd-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 1) zmm1)
  (:temporary (:sc int-avx512-reg :offset 2) zmm2)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst valignd zmm1 zmm2 (ea 64 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-valignd-zmm-disp32)
  (:translate %test-valignd-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 1) zmm1)
  (:temporary (:sc int-avx512-reg :offset 2) zmm2)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst valignd zmm1 zmm2 (ea 65 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrangess-disp8)
  (:translate %test-vrangess-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrangess xmm0 xmm1 (ea 4 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrangess-disp32)
  (:translate %test-vrangess-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrangess xmm0 xmm1 (ea 5 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrangesd-disp8)
  (:translate %test-vrangesd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm0)
  (:temporary (:sc double-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrangesd xmm0 xmm1 (ea 8 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrndscaleps-xmm-disp8)
  (:translate %test-vrndscaleps-xmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrndscaleps xmm (ea 16 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrndscaleps-zmm-disp8)
  (:translate %test-vrndscaleps-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrndscaleps zmm (ea 64 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrndscaleps-zmm-disp32)
  (:translate %test-vrndscaleps-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 2) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrndscaleps zmm (ea 65 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrndscaless-disp8)
  (:translate %test-vrndscaless-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrndscaless xmm (ea 4 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrndscaless-disp32)
  (:translate %test-vrndscaless-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrndscaless xmm (ea 5 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vrndscalesd-disp8)
  (:translate %test-vrndscalesd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrndscalesd xmm (ea 8 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vfixupimmps-xmm-disp8)
  (:translate %test-vfixupimmps-xmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vfixupimmps xmm0 xmm1 (ea 16 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vfixupimmps-zmm-disp8)
  (:translate %test-vfixupimmps-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 1) zmm1)
  (:temporary (:sc single-avx512-reg :offset 2) zmm2)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vfixupimmps zmm1 zmm2 (ea 64 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vfixupimmps-zmm-disp32)
  (:translate %test-vfixupimmps-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 1) zmm1)
  (:temporary (:sc single-avx512-reg :offset 2) zmm2)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vfixupimmps zmm1 zmm2 (ea 65 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vfixupimmss-disp8)
  (:translate %test-vfixupimmss-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vfixupimmss xmm0 xmm1 (ea 4 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vfixupimmss-disp32)
  (:translate %test-vfixupimmss-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vfixupimmss xmm0 xmm1 (ea 5 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vfixupimmsd-disp8)
  (:translate %test-vfixupimmsd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm0)
  (:temporary (:sc double-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vfixupimmsd xmm0 xmm1 (ea 8 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vreducess-disp8)
  (:translate %test-vreducess-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vreducess xmm0 xmm1 (ea 4 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vreducess-disp32)
  (:translate %test-vreducess-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vreducess xmm0 xmm1 (ea 5 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vreducesd-disp8)
  (:translate %test-vreducesd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm0)
  (:temporary (:sc double-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vreducesd xmm0 xmm1 (ea 8 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vgetmantss-disp8)
  (:translate %test-vgetmantss-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vgetmantss xmm0 xmm1 (ea 4 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vgetmantss-disp32)
  (:translate %test-vgetmantss-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vgetmantss xmm0 xmm1 (ea 5 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vgetmantsd-disp8)
  (:translate %test-vgetmantsd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm0)
  (:temporary (:sc double-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vgetmantsd xmm0 xmm1 (ea 8 rsp) 0)
    (inst xor :dword res res)))

(define-vop (%test-vgetexpss-disp8)
  (:translate %test-vgetexpss-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vgetexpss xmm0 xmm1 (ea 4 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vgetexpss-disp32)
  (:translate %test-vgetexpss-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vgetexpss xmm0 xmm1 (ea 5 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vgetexpsd-disp8)
  (:translate %test-vgetexpsd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm0)
  (:temporary (:sc double-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vgetexpsd xmm0 xmm1 (ea 8 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vscalefps-xmm-disp8)
  (:translate %test-vscalefps-xmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vscalefps xmm0 xmm1 (ea 16 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vscalefps-zmm-disp8)
  (:translate %test-vscalefps-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 1) zmm1)
  (:temporary (:sc single-avx512-reg :offset 2) zmm2)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vscalefps zmm1 zmm2 (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vscalefps-zmm-disp32)
  (:translate %test-vscalefps-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 1) zmm1)
  (:temporary (:sc single-avx512-reg :offset 2) zmm2)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vscalefps zmm1 zmm2 (ea 65 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vscalefss-disp8)
  (:translate %test-vscalefss-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vscalefss xmm0 xmm1 (ea 4 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vscalefss-disp32)
  (:translate %test-vscalefss-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm0)
  (:temporary (:sc single-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vscalefss xmm0 xmm1 (ea 5 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vscalefsd-disp8)
  (:translate %test-vscalefsd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm0)
  (:temporary (:sc double-sse-reg :offset 1) xmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vscalefsd xmm0 xmm1 (ea 8 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtss2usi-disp8)
  (:translate %test-vcvtss2usi-disp8)
  (:policy :fast-safe)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:generator 1
    (inst vcvtss2usi res (ea 4 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtss2usi-disp32)
  (:translate %test-vcvtss2usi-disp32)
  (:policy :fast-safe)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:generator 1
    (inst vcvtss2usi res (ea 5 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtsd2usi-disp8)
  (:translate %test-vcvtsd2usi-disp8)
  (:policy :fast-safe)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:generator 1
    (inst vcvtsd2usi res (ea 8 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvttss2usi-disp8)
  (:translate %test-vcvttss2usi-disp8)
  (:policy :fast-safe)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:generator 1
    (inst vcvttss2usi res (ea 4 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvttsd2usi-disp8)
  (:translate %test-vcvttsd2usi-disp8)
  (:policy :fast-safe)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:generator 1
    (inst vcvttsd2usi res (ea 8 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtusi2sd-disp8)
  (:translate %test-vcvtusi2sd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:generator 1
    (inst vcvtusi2sd xmm xmm (ea 8 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtusi2sd-disp32)
  (:translate %test-vcvtusi2sd-disp32)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:generator 1
    (inst vcvtusi2sd xmm xmm (ea 9 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtusi2ss-disp8)
  (:translate %test-vcvtusi2ss-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-sse-reg :offset 0) xmm)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:generator 1
    (inst vcvtusi2ss xmm xmm (ea 8 rsp))
    (inst xor :dword res res)))

;; evex tests
(cl:in-package :test-util)

(defmacro define-evex-disasm-test (name function expects &key unexpected)
    `(with-test (:name ,name)
       (let* ((func (compile nil '(lambda () (,function))))
              (text (with-output-to-string (s) (disassemble func :stream s))))
         (dolist (s ',expects)
           (assert (search s text)))
         (dolist (s ',unexpected)
           (assert (not (search s text)))))))

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
;; call avx2-inst-printer-list for vmovdqu and assert it is restricted to
;; L'L = 512 (ll = 2), uses disp-n = 64 => selects evex-ymmreg/mem-disp64.
(with-test (:name :auto-promoted-evex-disp8-printer)
  (let* ((asm-pkg (find-package "SB-X86-64-ASM"))
         (printer-fun (find-symbol "AVX2-INST-PRINTER-LIST" asm-pkg))
         (inst-format (find-symbol "YMM-YMM/MEM" asm-pkg))
         (disp64 (find-symbol "EVEX-YMMREG/MEM-DISP64" asm-pkg)))
    (when (and printer-fun inst-format disp64)
      (let* ((printer-forms
               (funcall printer-fun
                        inst-format
                        #xf3       ; prefix
                        #x6f       ; opcode
                        :opcode-prefix #x0f
                        :w 0))
             (evex-form
               (find-if (lambda (form)
                          (and (eq (first form) :printer)
                               (let ((name (second form)))
                                 (and (symbolp name)
                                      (search "EVEX-" (symbol-name name))))))
                        printer-forms)))
        (assert evex-form)
        (let ((fields (third evex-form)))
          ;; Auto-promoted EVEX forms are ZMM-only, so L'L = #b10.
          (assert (equal (second (assoc 'll fields)) 2))
          ;; The full-vector ZMM memory form uses compressed displacement N=64.
          (assert (eq (third (assoc 'reg/mem fields)) disp64)))))))

(with-test (:name :auto-promoted-vmovaps-disp8-printer)
  (let* ((asm-pkg (find-package "SB-X86-64-ASM"))
         (printer-fun (find-symbol "AVX2-INST-PRINTER-LIST" asm-pkg))
         (inst-format (find-symbol "YMM-YMM/MEM" asm-pkg))
         (disp64 (find-symbol "EVEX-YMMREG/MEM-DISP64" asm-pkg)))
    (when (and printer-fun inst-format disp64)
      (let* ((printer-forms
               (funcall printer-fun
                        inst-format
                        nil        ; prefix for vmovaps
                        #x28       ; opcode-from
                        :opcode-prefix #x0f
                        :w 0
                        :disp-n 64))
             (evex-form
               (find-if (lambda (form)
                          (and (eq (first form) :printer)
                               (let ((name (second form)))
                                 (and (symbolp name)
                                      (search "EVEX-" (symbol-name name))))))
                        printer-forms)))
        (assert evex-form)
        (let ((fields (third evex-form)))
          (assert (eq (third (assoc 'reg/mem fields)) disp64)))))))
;; EVEX high registers: R', V', X-as-B'
(define-evex-disasm-test
    :evex-high-register-disassembly
    sb-vm::%test-evex-high-regs
  ("ZMM16" "ZMM17" "ZMM18" "VADDPS")
  :unexpected ("VADDPS-MASKED"))

;; Full-vector compressed displacement at XMM/YMM/ZMM widths
(define-evex-disasm-test
    :evex-compressed-displacement-vector-lengths
    sb-vm::%test-evex-disp-vector-lengths
  ("VMOVDQU64 XMM0, [RSP+16]"
   "VMOVDQU64 YMM1, [RSP+32]"
   "VMOVDQU64 ZMM2, [RSP+64]"))

;; ZMM full-vector compressed displacement: disp8*64
(define-evex-disasm-test
    :evex-compressed-displacement
    sb-vm::%test-evex-disp8
  ("VMOVDQU64 ZMM0, [RSP+64]")
  :unexpected ("[RSP+1]"))

;; Negative compressed displacement
(define-evex-disasm-test
    :evex-compressed-displacement-negative
    sb-vm::%test-evex-disp-negative
  ("VMOVDQU64 ZMM0, [RSP-64]"))

;; Non-multiple displacement falls back to disp32
(define-evex-disasm-test
    :evex-compressed-displacement-nonmultiple
    sb-vm::%test-evex-disp-nonmultiple
  ("VMOVDQU64 ZMM0, [RSP+65]"))

;; Too large for disp8 falls back to disp32
(define-evex-disasm-test
    :evex-compressed-displacement-large
    sb-vm::%test-evex-disp-large
  ("VMOVDQU64 ZMM0, [RSP+8192]"))

;; Auto-promoted VADDPS must remain W=0
(define-evex-disasm-test
    :auto-promoted-evex-vaddps-disasm
    sb-vm::%test-auto-promoted-vaddps
  ("VADDPS" "ZMM16" "ZMM17" "ZMM18")
  :unexpected ("VADDPD"))

;; Auto-promoted VADDPD must remain W=1
(define-evex-disasm-test
    :auto-promoted-evex-vaddpd-disasm
    sb-vm::%test-auto-promoted-vaddpd
  ("VADDPD" "ZMM16" "ZMM17" "ZMM18")
  :unexpected ("VADDPS"))

;; Auto-promoted VPBROADCASTQ uses W=1 and compressed disp8
(define-evex-disasm-test
    :auto-promoted-evex-vpbroadcastq-disasm
    sb-vm::%test-auto-promoted-vpbroadcastq
  ("VPBROADCASTQ" "ZMM0" "[RSP+64]"))

;; VMOVDQU skip prevents conflict with explicit VMOVDQU32
(define-evex-disasm-test
    :auto-promoted-vmovdqu-skip-conflict-disasm
    sb-vm::%test-auto-promoted-vmovdqu
  ("VMOVDQU32" "ZMM0" "[RSP+64]"))

;; Explicit EVEX: VPTERNLOGD with immediate
(define-evex-disasm-test
    :evex-explicit-vpternlogd-disasm
    sb-vm::%test-evex-vpternlogd
  ("VPTERNLOGD" "ZMM0" "ZMM1" "ZMM2" "255"))

;; Explicit EVEX: VPERMT2D
(define-evex-disasm-test
    :evex-explicit-vpermt2d-disasm
    sb-vm::%test-evex-vpermt2d
  ("VPERMT2D" "ZMM0" "ZMM1" "ZMM2"))

;; Explicit EVEX: VBLENDMPS with high regs
(define-evex-disasm-test
    :evex-explicit-vblendmps-disasm
    sb-vm::%test-evex-vblendmps
  ("VBLENDMPS" "ZMM16" "ZMM17" "ZMM18"))

;; Explicit EVEX: VPCMPD with opmask
(define-evex-disasm-test
    :evex-explicit-vpcmpd-disasm
    sb-vm::%test-evex-vpcmpd
  ("VPCMPD" "K1" "ZMM0" "ZMM1"))

;; Explicit EVEX: VPMOVQD reversed store with compressed disp
(define-evex-disasm-test
    :evex-explicit-vpmovqd-disasm
    sb-vm::%test-evex-vpmovqd
  ("VPMOVQD" "[RSP+32]" "ZMM0"))

;; Auto-promoted VMOVAPS with compressed disp8
(define-evex-disasm-test
    :auto-promoted-vmovaps-disp8-disasm
    sb-vm::%test-auto-promoted-vmovaps-disp8
  ("VMOVAPS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Auto-promoted VMOVAPS falls back to disp32 for non-multiple
(define-evex-disasm-test
    :auto-promoted-vmovaps-disp-nonmultiple-disasm
    sb-vm::%test-auto-promoted-vmovaps-disp-nonmultiple
  ("VMOVAPS" "ZMM0" "[RSP+65]"))

;; Block broadcast: VBROADCASTF32X4 uses disp-n=16
(define-evex-disasm-test
    :evex-broadcast-f32x4-compressed-disp8
    sb-vm::%test-broadcast-f32x4-disp8
  ("VBROADCASTF32X4" "ZMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Block broadcast: VBROADCASTF32X4 disp32 fallback
(define-evex-disasm-test
    :evex-broadcast-f32x4-disp32-fallback
    sb-vm::%test-broadcast-f32x4-disp32
  ("VBROADCASTF32X4" "ZMM0" "[RSP+17]"))

;; Block broadcast: VBROADCASTF64X4 uses disp-n=32
(define-evex-disasm-test
    :evex-broadcast-f64x4-compressed-disp8
    sb-vm::%test-broadcast-f64x4-disp8
  ("VBROADCASTF64X4" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; Block broadcast: VBROADCASTF64X4 disp32 fallback
(define-evex-disasm-test
    :evex-broadcast-f64x4-disp32-fallback
    sb-vm::%test-broadcast-f64x4-disp32
  ("VBROADCASTF64X4" "ZMM0" "[RSP+33]"))

;; Compress: VCOMPRESSPS uses disp-n=64
(define-evex-disasm-test
    :evex-compress-compressed-disp8
    sb-vm::%test-compress-disp8
  ("VCOMPRESSPS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Compress: disp32 fallback
(define-evex-disasm-test
    :evex-compress-disp32-fallback
    sb-vm::%test-compress-disp32
  ("VCOMPRESSPS" "ZMM0" "[RSP+65]"))

;; Expand: VPEXPANDQ uses disp-n=64
(define-evex-disasm-test
    :evex-expand-compressed-disp8
    sb-vm::%test-expand-disp8
  ("VPEXPANDQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Expand: disp32 fallback
(define-evex-disasm-test
    :evex-expand-disp32-fallback
    sb-vm::%test-expand-disp32
  ("VPEXPANDQ" "ZMM0" "[RSP+65]"))

;; Compare-to-k with compressed disp8
(define-evex-disasm-test
    :evex-vpcmpd-compressed-disp8
    sb-vm::%test-vpcmpd-disp8
  ("VPCMPD" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Compare-to-k falls back to disp32 for non-multiple
(define-evex-disasm-test
    :evex-vpcmpd-disp32-fallback
    sb-vm::%test-vpcmpd-disp32
  ("VPCMPD" "K1" "ZMM0" "[RSP+65]"))

;; Test-to-k with compressed disp8
(define-evex-disasm-test
    :evex-vptestmd-compressed-disp8
    sb-vm::%test-vptestmd-disp8
  ("VPTESTMD" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Test-to-k falls back to disp32 for non-multiple
(define-evex-disasm-test
    :evex-vptestmd-disp32-fallback
    sb-vm::%test-vptestmd-disp32
  ("VPTESTMD" "K1" "ZMM0" "[RSP+65]"))

;; Down-convert store: XMM source has disp-n=8
(define-evex-disasm-test
    :evex-vpmovqd-xmm-compressed-disp8
    sb-vm::%test-vpmovqd-xmm-disp8
  ("VPMOVQD" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Down-convert store: YMM source has disp-n=16
(define-evex-disasm-test
    :evex-vpmovqd-ymm-compressed-disp8
    sb-vm::%test-vpmovqd-ymm-disp8
  ("VPMOVQD" "YMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Down-convert store: ZMM source has disp-n=32
(define-evex-disasm-test
    :evex-vpmovqd-zmm-compressed-disp8
    sb-vm::%test-vpmovqd-zmm-disp8
  ("VPMOVQD" "ZMM2" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; Down-convert store: ZMM falls back to disp32 for non-multiple
(define-evex-disasm-test
    :evex-vpmovqd-zmm-disp32-fallback
    sb-vm::%test-vpmovqd-zmm-disp32
  ("VPMOVQD" "ZMM2" "[RSP+33]"))

;; Saturating truncation: XMM source, byte result -> disp-n=2
(define-evex-disasm-test
    :evex-vpmovsqb-xmm-compressed-disp8
    sb-vm::%test-vpmovsqb-xmm-disp8
  ("VPMOVSQB" "XMM0" "[RSP+2]")
  :unexpected ("[RSP+1]"))

;; Saturating truncation: YMM source, byte result -> disp-n=4
(define-evex-disasm-test
    :evex-vpmovsqb-ymm-compressed-disp8
    sb-vm::%test-vpmovsqb-ymm-disp8
  ("VPMOVSQB" "YMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Saturating truncation: ZMM source, byte result -> disp-n=8
(define-evex-disasm-test
    :evex-vpmovsqb-zmm-compressed-disp8
    sb-vm::%test-vpmovsqb-zmm-disp8
  ("VPMOVSQB" "ZMM2" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Saturating truncation: non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vpmovsqb-zmm-disp32-fallback
    sb-vm::%test-vpmovsqb-zmm-disp32
  ("VPMOVSQB" "ZMM2" "[RSP+9]"))

;; Full-vector reciprocal approximation: XMM -> disp-n=16
(define-evex-disasm-test
    :evex-vrcp14ps-xmm-compressed-disp8
    sb-vm::%test-vrcp14ps-xmm-disp8
  ("VRCP14PS" "XMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Full-vector reciprocal approximation: YMM -> disp-n=32
(define-evex-disasm-test
    :evex-vrcp14ps-ymm-compressed-disp8
    sb-vm::%test-vrcp14ps-ymm-disp8
  ("VRCP14PS" "YMM1" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; Full-vector reciprocal approximation: ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vrcp14ps-zmm-compressed-disp8
    sb-vm::%test-vrcp14ps-zmm-disp8
  ("VRCP14PS" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Full-vector reciprocal approximation: non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vrcp14ps-zmm-disp32-fallback
    sb-vm::%test-vrcp14ps-zmm-disp32
  ("VRCP14PS" "ZMM2" "[RSP+65]"))

;; Integer full-vector group with W=1: vpabsq
(define-evex-disasm-test
    :evex-vpabsq-zmm-compressed-disp8
    sb-vm::%test-vpabsq-zmm-disp8
  ("VPABSQ" "ZMM3" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Packed 3-operand immediate: valignd XMM -> disp-n=16
(define-evex-disasm-test
    :evex-valignd-xmm-compressed-disp8
    sb-vm::%test-valignd-xmm-disp8
  ("VALIGND" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Packed 3-operand immediate: valignd YMM -> disp-n=32
(define-evex-disasm-test
    :evex-valignd-ymm-compressed-disp8
    sb-vm::%test-valignd-ymm-disp8
  ("VALIGND" "YMM1" "YMM2" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; Packed 3-operand immediate: valignd ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-valignd-zmm-compressed-disp8
    sb-vm::%test-valignd-zmm-disp8
  ("VALIGND" "ZMM1" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Packed 3-operand immediate: non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-valignd-zmm-disp32-fallback
    sb-vm::%test-valignd-zmm-disp32
  ("VALIGND" "ZMM1" "ZMM2" "[RSP+65]"))

;; Scalar range: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vrangess-compressed-disp8
    sb-vm::%test-vrangess-disp8
  ("VRANGESS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar range: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vrangess-disp32-fallback
    sb-vm::%test-vrangess-disp32
  ("VRANGESS" "XMM0" "XMM1" "[RSP+5]"))

;; Scalar range: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vrangesd-compressed-disp8
    sb-vm::%test-vrangesd-disp8
  ("VRANGESD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Packed round: XMM full-vector -> disp-n=16
(define-evex-disasm-test
    :evex-vrndscaleps-xmm-compressed-disp8
    sb-vm::%test-vrndscaleps-xmm-disp8
  ("VRNDSCALEPS" "XMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Packed round: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vrndscaleps-zmm-compressed-disp8
    sb-vm::%test-vrndscaleps-zmm-disp8
  ("VRNDSCALEPS" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Packed round: non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vrndscaleps-zmm-disp32-fallback
    sb-vm::%test-vrndscaleps-zmm-disp32
  ("VRNDSCALEPS" "ZMM2" "[RSP+65]"))

;; Scalar round: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vrndscaless-compressed-disp8
    sb-vm::%test-vrndscaless-disp8
  ("VRNDSCALESS" "XMM0" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar round: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vrndscaless-disp32-fallback
    sb-vm::%test-vrndscaless-disp32
  ("VRNDSCALESS" "XMM0" "[RSP+5]"))

;; Scalar round: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vrndscalesd-compressed-disp8
    sb-vm::%test-vrndscalesd-disp8
  ("VRNDSCALESD" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Packed fixup: XMM full-vector -> disp-n=16
(define-evex-disasm-test
    :evex-vfixupimmps-xmm-compressed-disp8
    sb-vm::%test-vfixupimmps-xmm-disp8
  ("VFIXUPIMMPS" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Packed fixup: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vfixupimmps-zmm-compressed-disp8
    sb-vm::%test-vfixupimmps-zmm-disp8
  ("VFIXUPIMMPS" "ZMM1" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Packed fixup: non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vfixupimmps-zmm-disp32-fallback
    sb-vm::%test-vfixupimmps-zmm-disp32
  ("VFIXUPIMMPS" "ZMM1" "ZMM2" "[RSP+65]"))

;; Scalar fixup: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vfixupimmss-compressed-disp8
    sb-vm::%test-vfixupimmss-disp8
  ("VFIXUPIMMSS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar fixup: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vfixupimmss-disp32-fallback
    sb-vm::%test-vfixupimmss-disp32
  ("VFIXUPIMMSS" "XMM0" "XMM1" "[RSP+5]"))

;; Scalar fixup: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vfixupimmsd-compressed-disp8
    sb-vm::%test-vfixupimmsd-disp8
  ("VFIXUPIMMSD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Scalar reduce: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vreducess-compressed-disp8
    sb-vm::%test-vreducess-disp8
  ("VREDUCESS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar reduce: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vreducess-disp32-fallback
    sb-vm::%test-vreducess-disp32
  ("VREDUCESS" "XMM0" "XMM1" "[RSP+5]"))

;; Scalar reduce: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vreducesd-compressed-disp8
    sb-vm::%test-vreducesd-disp8
  ("VREDUCESD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Scalar getmant: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vgetmantss-compressed-disp8
    sb-vm::%test-vgetmantss-disp8
  ("VGETMANTSS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar getmant: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vgetmantss-disp32-fallback
    sb-vm::%test-vgetmantss-disp32
  ("VGETMANTSS" "XMM0" "XMM1" "[RSP+5]"))

;; Scalar getmant: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vgetmantsd-compressed-disp8
    sb-vm::%test-vgetmantsd-disp8
  ("VGETMANTSD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Scalar getexp: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vgetexpss-compressed-disp8
    sb-vm::%test-vgetexpss-disp8
  ("VGETEXPSS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar getexp: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vgetexpss-disp32-fallback
    sb-vm::%test-vgetexpss-disp32
  ("VGETEXPSS" "XMM0" "XMM1" "[RSP+5]"))

;; Scalar getexp: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vgetexpsd-compressed-disp8
    sb-vm::%test-vgetexpsd-disp8
  ("VGETEXPSD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Packed scale: XMM full-vector -> disp-n=16
(define-evex-disasm-test
    :evex-vscalefps-xmm-compressed-disp8
    sb-vm::%test-vscalefps-xmm-disp8
  ("VSCALEFPS" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Packed scale: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vscalefps-zmm-compressed-disp8
    sb-vm::%test-vscalefps-zmm-disp8
  ("VSCALEFPS" "ZMM1" "ZMM2" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Packed scale: non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vscalefps-zmm-disp32-fallback
    sb-vm::%test-vscalefps-zmm-disp32
  ("VSCALEFPS" "ZMM1" "ZMM2" "[RSP+65]"))

;; Scalar scale: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vscalefss-compressed-disp8
    sb-vm::%test-vscalefss-disp8
  ("VSCALEFSS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar scale: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vscalefss-disp32-fallback
    sb-vm::%test-vscalefss-disp32
  ("VSCALEFSS" "XMM0" "XMM1" "[RSP+5]"))

;; Scalar scale: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vscalefsd-compressed-disp8
    sb-vm::%test-vscalefsd-disp8
  ("VSCALEFSD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Scalar unsigned conversion: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vcvtss2usi-compressed-disp8
    sb-vm::%test-vcvtss2usi-disp8
  ("VCVTSS2USI" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar unsigned conversion: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vcvtss2usi-disp32-fallback
    sb-vm::%test-vcvtss2usi-disp32
  ("VCVTSS2USI" "[RSP+5]"))

;; Scalar unsigned conversion: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vcvtsd2usi-compressed-disp8
    sb-vm::%test-vcvtsd2usi-disp8
  ("VCVTSD2USI" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Scalar truncating unsigned conversion: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vcvttss2usi-compressed-disp8
    sb-vm::%test-vcvttss2usi-disp8
  ("VCVTTSS2USI" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar truncating unsigned conversion: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vcvttsd2usi-compressed-disp8
    sb-vm::%test-vcvttsd2usi-disp8
  ("VCVTTSD2USI" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Scalar unsigned convert to double, default W=1 -> disp-n=8
(define-evex-disasm-test
    :evex-vcvtusi2sd-compressed-disp8
    sb-vm::%test-vcvtusi2sd-disp8
  ("VCVTUSI2SD" "XMM0" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Scalar unsigned convert to double, non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vcvtusi2sd-disp32-fallback
    sb-vm::%test-vcvtusi2sd-disp32
  ("VCVTUSI2SD" "XMM0" "XMM0" "[RSP+9]"))

;; Scalar unsigned convert to single, default W=1 -> disp-n=8
(define-evex-disasm-test
    :evex-vcvtusi2ss-compressed-disp8
    sb-vm::%test-vcvtusi2ss-disp8
  ("VCVTUSI2SS" "XMM0" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Printer metadata: W=0 and W=1 entries have correct disp-n
(with-test (:name :scalar-unsigned-convert-printer-disp-n)
  (let* ((asm-pkg (find-package "SB-X86-64-ASM"))
         (printer-fun (find-symbol "AVX512-INST-PRINTER-LIST" asm-pkg))
         (inst-format (find-symbol "YMM-YMM/MEM" asm-pkg)))
    (when (and printer-fun inst-format)
      (flet ((find-disp-n (forms w)
               (find-if (lambda (form)
                          (and (eq (first form) :printer)
                               (let ((fields (third form)))
                                 (and (eql (second (assoc 'w fields)) w)
                                      (assoc 'reg/mem fields)))))
                        forms)))
        (let ((w0-forms (funcall printer-fun inst-format #xf3 #x7b
                                 :nds t :w 0 :disp-n 4))
              (w1-forms (funcall printer-fun inst-format #xf3 #x7b
                                 :nds t :w 1 :disp-n 8)))
          (let ((w0-form (find-disp-n w0-forms 0))
                (w1-form (find-disp-n w1-forms 1)))
            (assert w0-form)
            (assert w1-form)
            (let ((w0-reg/mem (assoc 'reg/mem (third w0-form)))
                  (w1-reg/mem (assoc 'reg/mem (third w1-form))))
              ;; The third element of the reg/mem field is the arg type.
              (assert (eq (third w0-reg/mem)
                          (find-symbol "EVEX-YMMREG/MEM-DISP4" asm-pkg)))
              (assert (eq (third w1-reg/mem)
                          (find-symbol "EVEX-YMMREG/MEM-DISP8" asm-pkg))))))))))
