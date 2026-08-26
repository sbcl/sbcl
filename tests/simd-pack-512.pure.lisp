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
  (def %test-vcvtusi2ss-disp8)
  (def %test-vcvtps2udq-zmm-disp8)
  (def %test-vcvtps2udq-zmm-disp32)
  (def %test-vcvtudq2pd-zmm-disp8)
  (def %test-vcvtudq2pd-xmm-disp8)
  (def %test-vcvtps2qq-zmm-disp8)
  (def %test-vcvtps2qq-zmm-disp32)
  (def %test-vcvtpd2qq-zmm-disp8)
  (def %test-vcvtqq2ps-zmm-disp8)
  (def %test-vcvtqq2ps-zmm-disp32)
  (def %test-auto-promoted-vcvtdq2pd-disp8)
  (def %test-auto-promoted-vcvtdq2pd-disp32)
  (def %test-auto-promoted-vcvtps2dq-disp8)
  (def %test-auto-promoted-vcvtps2dq-disp32)
  (def %test-auto-promoted-vrcpps-disp8)
  (def %test-auto-promoted-vpmovsxbw-disp8)
  (def %test-vpand-zmm-disasm)
  (def %test-vmovdqa-zmm-disasm)
  (def %test-vbroadcastf128-zmm-disasm)
  (def %test-vpermt2d-zmm-disp8)
  (def %test-vpermt2d-zmm-disp32)
  (def %test-vpmaxsq-zmm-disp8)
  (def %test-vprolvd-xmm-disp8)
  (def %test-vpermt2q-zmm-disp8)
  (def %test-vpermt2q-zmm-disp32)
  (def %test-vshuff32x4-zmm-disp8)
  (def %test-vshuff32x4-zmm-disp32)
  (def %test-vshufi32x4-xmm-disp8)
  (def %test-vblendmps-zmm-disp8)
  (def %test-vblendmps-zmm-disp32)
  (def %test-vpblendmq-zmm-disp8)
  (def %test-vrangepd-zmm-disp8)
  (def %test-vreduceps-xmm-disp8)
  (def %test-vreduceps-zmm-disp8)
  (def %test-vreduceps-zmm-disp32)
  (def %test-vrcp14ss-disp8)
  (def %test-vrcp14ss-disp32)
  (def %test-vrcp14sd-disp8)
  (def %test-vrsqrt14ss-disp8)
  (def %test-vrsqrt14sd-disp8)
  (def %test-vpblendmb-zmm-disp8)
  (def %test-vpcmpb-zmm-disp8)
  (def %test-vptestmw-zmm-disp8)
  (def %test-vpermw-zmm-disp8)
  (def %test-vpsllvw-zmm-disp8)
  (def %test-vdbpsadbw-zmm-disp8)
  (def %test-vpmadd52luq-zmm-disp8)
  (def %test-vpermb-zmm-disp8)
  (def %test-vpcompressb-zmm-disp8)
  (def %test-vpshldw-zmm-disp8)
  (def %test-vpopcntd-zmm-disp8)
  (def %test-vpshufbitqmb-zmm-disp8)
  (def %test-vaddps-masked-zmm-disp8)
  (def %test-vaddps-masked-zmm-disp32)
  (def %test-vpaddq-masked-zmm-disp8)
  (def %test-vaddpd-masked-zmm-disp8)
  (def %test-vinsertf32x4-zmm-disp8)
  (def %test-vinsertf32x8-zmm-disp8)
  (def %test-vextractf32x4-zmm-disp8)
  (def %test-vextractf32x8-zmm-disp8)
  (def %test-vfmadd132ps-zmm-disp8)
  (def %test-vfmadd132ss-disp8)
  (def %test-vcvtph2ps-zmm-disp8)
  (def %test-vcvtps2ph-zmm-disp8)
  (def %test-vgf2p8mulb-zmm-disp8)
  (def %test-vgf2p8affineqb-zmm-disp8)
  (def %test-vfpclassps-zmm-disp8)
  (def %test-vfpclassss-disp8)
  (def %test-vpmullq-zmm-disp8)
  (def %test-vsib-high-index)
  (def %test-vsib-no-base-low-index)
  (def %test-vsib-high-base-high-index)
  (def %test-vsib-scatter-low-index)
  (def %test-vsib-scale-2)
  (def %test-vsib-scale-8)
  (def %test-vsib-disp8)
  (def %test-vsib-disp8-compressed)
  (def %test-vsib-scatter-disp8-compressed)
  (def %test-vsib-qword-disp8-compressed)
  (def %test-vsib-nonmultiple-disp32)
  (def %test-vsib-high-index-disp8-compressed)
  (def %test-vaddps-masked-z-disp8)
  (def %test-vaddps-masked-z-disp32)
  (def %test-vpaddq-masked-z-disp8)
  (def %test-vaddpd-masked-z-disp8)
  (def %test-vgatherdps-z-zero)
  (def %test-vsib-no-disp)
  (def %test-vpconflictd-zmm-disp8)
  (def %test-vpconflictq-zmm-disp8)
  (def %test-vplzcntd-xmm-disp8)
  (def %test-vplzcntq-zmm-disp8)
  (def %test-vpdpbusd-zmm-disp8)
  (def %test-vpdpbusds-zmm-disp8)
  (def %test-vpdpwssd-xmm-disp8)
  (def %test-vpdpwssds-zmm-disp32)
  (def %test-vcvtne2ps2bf16-zmm-disp8)
  (def %test-vcvtneps2bf16-zmm-disp8)
  (def %test-vdpbf16ps-xmm-disp8)
  (def %test-vp2intersectd)
  (def %test-vp2intersectq)
  (def %test-vaddps-bcast-zmm-disp8)
  (def %test-vaddps-bcast-xmm-disp8)
  (def %test-vaddpd-bcast-zmm-disp8)
  (def %test-vmulps-bcast-ymm-disp8)
  (def %test-vdivps-masked-zmm-disp8)
  (def %test-vdivpd-masked-zmm-disp8)
  (def %test-vdivps-masked-z-zmm-disp8)
  (def %test-vdivpd-masked-z-zmm-disp8)
  (def %test-vsubps-bcast-ymm-disp8)
  (def %test-vsubpd-bcast-zmm-disp8)
  (def %test-vdivps-bcast-xmm-disp8)
  (def %test-vdivpd-bcast-zmm-disp8)
  (def %test-vminps-masked-zmm-disp8)
  (def %test-vmaxpd-masked-zmm-disp8)
  (def %test-vminps-masked-z-zmm-disp8)
  (def %test-vmaxpd-masked-z-zmm-disp8)
  (def %test-vminps-bcast-ymm-disp8)
  (def %test-vmaxpd-bcast-zmm-disp8)
  (def %test-vpaddd-bcast-zmm-disp8)
  (def %test-vpaddq-bcast-zmm-disp8)
  (def %test-vpandd-bcast-xmm-disp8)
  (def %test-vporq-bcast-ymm-disp8)
  (def %test-vpminsd-bcast-zmm-disp8)
  (def %test-vpmaxsd-bcast-xmm-disp8)
  (def %test-vpminud-bcast-ymm-disp8)
  (def %test-vpmaxuq-bcast-zmm-disp8)
  (def %test-vpminsd-masked-zmm-disp8)
  (def %test-vpmaxsq-masked-zmm-disp8)
  (def %test-vpminsd-masked-z-zmm-disp8)
  (def %test-vpmaxsq-masked-z-zmm-disp8)
  (def %test-vfmadd132ps-masked-zmm-disp8)
  (def %test-vfmadd231pd-masked-zmm-disp8)
  (def %test-vfmadd132ps-masked-z-zmm-disp8)
  (def %test-vfmadd213pd-masked-z-zmm-disp8)
  (def %test-vfmadd132ps-bcast-xmm-disp8)
  (def %test-vfmadd213pd-bcast-zmm-disp8)
  (def %test-vfmsub132ps-masked-zmm-disp8)
  (def %test-vfmsub213pd-masked-z-zmm-disp8)
  (def %test-vfnmadd231ps-bcast-xmm-disp8)
  (def %test-vfmaddsub132ps-masked-zmm-disp8)
  (def %test-vfmaddsub213pd-masked-z-zmm-disp8)
  (def %test-vfmaddsub231ps-bcast-xmm-disp8)
  (def %test-vfmsubadd132ps-masked-zmm-disp8)
  (def %test-vfmsubadd213pd-masked-z-zmm-disp8)
  (def %test-vfmsubadd231ps-bcast-xmm-disp8)
  (def %test-vfmadd132ss-masked-disp8)
  (def %test-vfmsub213sd-masked-z-disp8)
  (def %test-vpaddb-masked-zmm-disp8)
  (def %test-vpsubw-masked-z-zmm-disp8)
  (def %test-vpsllvd-masked-zmm-disp8)
  (def %test-vpsrlvq-masked-z-zmm-disp8)
  (def %test-vpsravd-masked-zmm-disp8)
  (def %test-vrangeps-masked-zmm-disp8)
  (def %test-vrangepd-masked-z-zmm-disp8)
  (def %test-vrangeps-bcast-xmm-disp8)
  (def %test-vpdpbusd-masked-zmm-disp8)
  (def %test-vpdpwssd-masked-z-zmm-disp8)
  (def %test-vpdpbusd-bcast-xmm-disp8)
  (def %test-vcvtne2ps2bf16-masked-zmm-disp8)
  (def %test-vdpbf16ps-masked-z-zmm-disp8)
  (def %test-vcvtneps2bf16-masked-zmm-disp8)
  (def %test-vcvtneps2bf16-masked-z-zmm-disp8)
  (def %test-vpermt2d-masked-zmm-disp8)
  (def %test-vpermt2q-masked-z-zmm-disp8)
  (def %test-vpermb-masked-zmm-disp8)
  (def %test-vpermw-masked-z-zmm-disp8)
  (def %test-vpmadd52luq-masked-zmm-disp8)
  (def %test-vpshldvw-masked-z-zmm-disp8)
  (def %test-vpopcntd-masked-zmm-disp8)
  (def %test-vpopcntq-masked-z-zmm-disp8)
  (def %test-vrcp14ps-masked-zmm-disp8)
  (def %test-vrcp14pd-masked-z-zmm-disp8)
  (def %test-vrsqrt14ps-masked-zmm-disp8)
  (def %test-vrcp14ps-bcast-xmm-disp8)
  (def %test-vrsqrt14pd-bcast-zmm-disp8)
  (def %test-vreduceps-masked-zmm-disp8)
  (def %test-vreducepd-masked-z-zmm-disp8)
  (def %test-vreduceps-bcast-xmm-disp8)
  (def %test-vscalefps-masked-zmm-disp8)
  (def %test-vscalefpd-masked-z-zmm-disp8)
  (def %test-vscalefps-bcast-ymm-disp8)
  (def %test-vfixupimmps-masked-zmm-disp8)
  (def %test-vfixupimmpd-masked-z-zmm-disp8)
  (def %test-vfixupimmps-bcast-xmm-disp8)
  (def %test-vrndscaleps-masked-zmm-disp8)
  (def %test-vrndscalepd-masked-z-zmm-disp8)
  (def %test-vrndscaleps-bcast-ymm-disp8)
  (def %test-vrcp14ss-masked-disp8)
  (def %test-vrsqrt14sd-masked-z-disp8)
  (def %test-vgetexpss-masked-disp8)
  (def %test-vscalefsd-masked-z-disp8)
  (def %test-vgetmantss-masked-disp8)
  (def %test-vrangesd-masked-z-disp8)
  (def %test-vfixupimmss-masked-disp8)
  (def %test-vrndscaless-masked-disp8)
  (def %test-vrndscalesd-masked-z-disp8)
  (def %test-vgetexpps-masked-zmm-disp8)
  (def %test-vgetexppd-masked-z-zmm-disp8)
  (def %test-vgetmantps-masked-zmm-disp8)
  (def %test-vgetmantpd-masked-z-zmm-disp8)
  (def %test-vpabsq-masked-zmm-disp8)
  (def %test-vgf2p8mulb-masked-zmm-disp8)
  (def %test-vgf2p8affineqb-masked-z-zmm-disp8)
  (def %test-vgf2p8affineinvqb-masked-zmm-disp8)
  (def %test-vcvtne2ps2bf16-bcast-zmm-disp8)
  (def %test-vdpbf16ps-bcast-xmm-disp8)
  (def %test-vpconflictd-masked-zmm-disp8)
  (def %test-vpconflictq-masked-z-zmm-disp8)
  (def %test-vplzcntd-masked-xmm-disp8)
  (def %test-vplzcntq-masked-z-zmm-disp8)
  (def %test-vaesenc-masked-zmm-disp8)
  (def %test-vaesenclast-masked-z-zmm-disp8)
  (def %test-vaesdec-masked-xmm-disp8)
  (def %test-vaesdeclast-masked-z-ymm-disp8)
  (def %test-vaesenc-evex-zmm)
  (def %test-vaesenclast-evex-zmm)
  (def %test-vaesdec-evex-zmm)
  (def %test-vaesdeclast-evex-zmm)
  (def %test-vcvtph2ps-masked-zmm-disp8)
  (def %test-vcvtph2ps-masked-z-zmm-disp8)
  (def %test-vcvtph2ps-masked-xmm-disp8)
  (def %test-vcvtps2ph-masked-zmm-disp8)
  (def %test-vcvtps2ph-masked-z-zmm-disp8)
  (def %test-vcvtps2ph-masked-xmm-disp8)
  (def %test-vpshldw-masked-zmm-disp8)
  (def %test-vpshldd-masked-z-zmm-disp8)
  (def %test-vpshrdq-masked-zmm-disp8)
  (def %test-vpmultishiftqb-masked-zmm-disp8)
  (def %test-vcompressps-masked-zmm-disp8)
  (def %test-vcompresspd-masked-z-zmm-disp8)
  (def %test-vpcompressd-masked-xmm-disp8)
  (def %test-vexpandps-masked-zmm-disp8)
  (def %test-vexpandpd-masked-z-zmm-disp8)
  (def %test-vpexpandd-masked-xmm-disp8)
  (def %test-vpmovqd-masked-zmm-disp8)
  (def %test-vpmovqd-masked-z-zmm-disp8)
  (def %test-vpmovsqb-masked-xmm-disp8)
  (def %test-vpmovsqb-masked-z-ymm-disp8)
  (def %test-vdbpsadbw-masked-zmm-disp8)
  (def %test-vdbpsadbw-masked-z-zmm-disp8)
  (def %test-vpternlogd-masked-zmm-disp8)
  (def %test-vpternlogd-masked-z-zmm-disp8)
  (def %test-vpternlogq-masked-xmm-disp8)
  (def %test-vpternlogq-masked-z-ymm-disp8)
  (def %test-vshuff32x4-masked-zmm-disp8)
  (def %test-vshuff32x4-masked-z-zmm-disp8)
  (def %test-vshuff64x2-masked-xmm-disp8)
  (def %test-vshuff64x2-masked-z-ymm-disp8)
  (def %test-vshufi32x4-masked-zmm-disp8)
  (def %test-vshufi32x4-masked-z-zmm-disp8)
  (def %test-vshufi64x2-masked-xmm-disp8)
  (def %test-vshufi64x2-masked-z-ymm-disp8)
  (def %test-vinsertf32x4-masked-zmm-disp8)
  (def %test-vinsertf32x4-masked-z-zmm-disp8)
  (def %test-vinsertf64x2-masked-xmm-disp8)
  (def %test-vinsertf64x2-masked-z-ymm-disp8)
  (def %test-vinserti32x4-masked-zmm-disp8)
  (def %test-vinserti32x4-masked-z-zmm-disp8)
  (def %test-vinserti32x8-masked-ymm-disp8)
  (def %test-vinserti32x8-masked-z-ymm-disp8))

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

(define-vop (%test-vcvtps2udq-zmm-disp8)
  (:translate %test-vcvtps2udq-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtps2udq zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtps2udq-zmm-disp32)
  (:translate %test-vcvtps2udq-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtps2udq zmm (ea 65 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtudq2pd-zmm-disp8)
  (:translate %test-vcvtudq2pd-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtudq2pd zmm (ea 32 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtudq2pd-xmm-disp8)
  (:translate %test-vcvtudq2pd-xmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-sse-reg :offset 0) xmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtudq2pd xmm (ea 8 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtps2qq-zmm-disp8)
  (:translate %test-vcvtps2qq-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtps2qq zmm (ea 32 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtps2qq-zmm-disp32)
  (:translate %test-vcvtps2qq-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtps2qq zmm (ea 33 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtpd2qq-zmm-disp8)
  (:translate %test-vcvtpd2qq-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtpd2qq zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtqq2ps-zmm-disp8)
  (:translate %test-vcvtqq2ps-zmm-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtqq2ps zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vcvtqq2ps-zmm-disp32)
  (:translate %test-vcvtqq2ps-zmm-disp32)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtqq2ps zmm (ea 65 rsp))
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vcvtdq2pd-disp8)
  (:translate %test-auto-promoted-vcvtdq2pd-disp8)
  (:policy :fast-safe)
  (:temporary (:sc double-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtdq2pd zmm (ea 32 rsp))
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vcvtdq2pd-disp32)
  (:translate %test-auto-promoted-vcvtdq2pd-disp32)
  (:policy :fast-safe)
  (:temporary (:sc double-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtdq2pd zmm (ea 33 rsp))
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vcvtps2dq-disp8)
  (:translate %test-auto-promoted-vcvtps2dq-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtps2dq zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vcvtps2dq-disp32)
  (:translate %test-auto-promoted-vcvtps2dq-disp32)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vcvtps2dq zmm (ea 65 rsp))
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vrcpps-disp8)
  (:translate %test-auto-promoted-vrcpps-disp8)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vrcpps zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-auto-promoted-vpmovsxbw-disp8)
  (:translate %test-auto-promoted-vpmovsxbw-disp8)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpmovsxbw zmm (ea 32 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vpand-zmm-disasm)
  (:translate %test-vpand-zmm-disasm)
  (:policy :fast-safe)
  (:temporary (:sc int-avx512-reg :offset 0) zmm0)
  (:temporary (:sc int-avx512-reg :offset 1) zmm1)
  (:temporary (:sc int-avx512-reg :offset 2) zmm2)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vpand zmm0 zmm1 zmm2)
    (inst xor :dword res res)))

(define-vop (%test-vmovdqa-zmm-disasm)
  (:translate %test-vmovdqa-zmm-disasm)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vmovdqa zmm (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vbroadcastf128-zmm-disasm)
  (:translate %test-vbroadcastf128-zmm-disasm)
  (:policy :fast-safe)
  (:temporary (:sc single-avx512-reg :offset 0) zmm)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vbroadcastf128 zmm (ea 16 rsp))
    (inst xor :dword res res)))
(defmacro define-test-vop-vsib
    (name inst data-sc data-off index-sc index-off disp scale mask
     &key (base-sc nil) (base-off nil) (reverse-p nil))
  (let ((base-sym (if base-sc 'base nil)))
    `(define-vop (,name)
       (:translate ,name)
       (:policy :fast-safe)
       (:temporary (:sc ,data-sc :offset ,data-off) data)
       (:temporary (:sc ,index-sc :offset ,index-off) index)
       ,@(when base-sc
           `((:temporary (:sc ,base-sc :offset ,base-off) base)
             (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)))
       (:results (res :scs (unsigned-reg)))
       (:result-types unsigned-num)
       (:generator 1
         ,@(when base-sc
             `((inst mov :qword base rsp)))
         ,(if reverse-p
              `(inst ,inst (ea ,disp ,base-sym index ,scale) data ,mask)
              `(inst ,inst data (ea ,disp ,base-sym index ,scale) ,mask))
         (inst xor :dword res res)))))

(defmacro define-test-vop-masked-reg-mem-imm
    (name inst reg-sc reg-off disp mask imm)
  `(define-vop (,name)
     (:translate ,name)
     (:policy :fast-safe)
     (:temporary (:sc ,reg-sc :offset ,reg-off) vec)
     (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
     (:results (res :scs (unsigned-reg)))
     (:result-types unsigned-num)
     (:generator 1
       (inst ,inst vec (ea ,disp rsp) ,mask ,imm)
       (inst xor :dword res res))))

(defmacro define-test-vop-masked-reg-mem (name inst reg-sc reg-off disp mask)
  `(define-vop (,name)
     (:translate ,name)
     (:policy :fast-safe)
     (:temporary (:sc ,reg-sc :offset ,reg-off) vec)
     (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
     (:results (res :scs (unsigned-reg)))
     (:result-types unsigned-num)
     (:generator 1
       (inst ,inst vec (ea ,disp rsp) ,mask)
       (inst xor :dword res res))))

(defmacro define-test-vop-masked-reg-mem-imm
    (name inst reg-sc reg-off disp mask imm)
  `(define-vop (,name)
     (:translate ,name)
     (:policy :fast-safe)
     (:temporary (:sc ,reg-sc :offset ,reg-off) vec)
     (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
     (:results (res :scs (unsigned-reg)))
     (:result-types unsigned-num)
     (:generator 1
       (inst ,inst vec (ea ,disp rsp) ,mask ,imm)
       (inst xor :dword res res))))

(defmacro define-test-vop-masked-mem-reg-imm
    (name inst reg-sc reg-off disp mask imm)
  `(define-vop (,name)
     (:translate ,name)
     (:policy :fast-safe)
     (:temporary (:sc ,reg-sc :offset ,reg-off) vec)
     (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
     (:results (res :scs (unsigned-reg)))
     (:result-types unsigned-num)
     (:generator 1
       (inst ,inst (ea ,disp rsp) vec ,mask ,imm)
       (inst xor :dword res res))))

(defmacro define-test-vop-masked-mem-reg
    (name inst reg-sc reg-off disp mask)
  `(define-vop (,name)
     (:translate ,name)
     (:policy :fast-safe)
     (:temporary (:sc ,reg-sc :offset ,reg-off) vec)
     (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
     (:results (res :scs (unsigned-reg)))
     (:result-types unsigned-num)
     (:generator 1
       (inst ,inst (ea ,disp rsp) vec ,mask)
       (inst xor :dword res res))))

(define-vop (%test-vp2intersectd)
  (:translate %test-vp2intersectd)
  (:policy :fast-safe)
  (:temporary (:sc mask-reg :offset 1) k1)
  (:temporary (:sc mask-reg :offset 2) k2)
  (:temporary (:sc int-avx512-reg :offset 0) zmm0)
  ;;(:temporary (:sc int-avx512-reg :offset 1) zmm1)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    ;; k1 = first mask, k2 = second mask, src1 = zmm0, src2 = memory
    (inst vp2intersectd k1 k2 zmm0 (ea 64 rsp))
    (inst xor :dword res res)))

(define-vop (%test-vp2intersectq)
  (:translate %test-vp2intersectq)
  (:policy :fast-safe)
  (:temporary (:sc mask-reg :offset 3) k3)
  (:temporary (:sc mask-reg :offset 4) k4)
  (:temporary (:sc int-avx512-reg :offset 2) zmm2)
  ;; (:temporary (:sc int-avx512-reg :offset 3) zmm3)
  (:temporary (:sc unsigned-reg :offset rsp-offset) rsp)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst vp2intersectq k3 k4 zmm2 (ea 64 rsp))
    (inst xor :dword res res)))

(define-test-vop-three-reg %test-evex-high-regs vaddps single-avx512-reg 16 single-avx512-reg 17 single-avx512-reg 18)
(define-test-vop-load %test-evex-disp8 vmovdqu64 single-avx512-reg 0 64)
(define-test-vop-multi-load %test-evex-disp-vector-lengths
  (vmovdqu64 single-sse-reg 0 16)
  (vmovdqu64 single-avx2-reg 1 32)
  (vmovdqu64 single-avx512-reg 2 64))
(define-test-vop-load %test-evex-disp-negative vmovdqu64 single-avx512-reg 0 -64)
(define-test-vop-load %test-evex-disp-nonmultiple vmovdqu64 single-avx512-reg 0 65)
(define-test-vop-load %test-evex-disp-large vmovdqu64 single-avx512-reg 0 8192)
(define-test-vop-three-reg %test-auto-promoted-vaddps vaddps single-avx512-reg 16 single-avx512-reg 17 single-avx512-reg 18)
(define-test-vop-three-reg %test-auto-promoted-vaddpd vaddpd double-avx512-reg 16 double-avx512-reg 17 double-avx512-reg 18)
(define-test-vop-load %test-auto-promoted-vpbroadcastq vpbroadcastq int-avx512-reg 0 64)
(define-test-vop-load %test-auto-promoted-vmovdqu vmovdqu single-avx512-reg 0 64)
(define-test-vop-three-reg %test-evex-vpternlogd vpternlogd int-avx512-reg 0 int-avx512-reg 1 int-avx512-reg 2 #xFF)
(define-test-vop-three-reg %test-evex-vpermt2d vpermt2d int-avx512-reg 0 int-avx512-reg 1 int-avx512-reg 2)
(define-test-vop-three-reg %test-evex-vblendmps vblendmps single-avx512-reg 16 single-avx512-reg 17 single-avx512-reg 18)
(define-test-vop-mask-reg-reg %test-evex-vpcmpd vpcmpd mask-reg 1 int-avx512-reg 0 int-avx512-reg 1 #x1)
(define-test-vop-store %test-evex-vpmovqd vpmovqd int-avx512-reg 0 32)
(define-test-vop-load %test-auto-promoted-vmovaps-disp8 vmovaps single-avx512-reg 0 64)
(define-test-vop-load %test-auto-promoted-vmovaps-disp-nonmultiple vmovaps single-avx512-reg 0 65)
(define-test-vop-load %test-broadcast-f32x4-disp8 vbroadcastf32x4 single-avx512-reg 0 16)
(define-test-vop-load %test-broadcast-f32x4-disp32 vbroadcastf32x4 single-avx512-reg 0 17)
(define-test-vop-load %test-broadcast-f64x4-disp8 vbroadcastf64x4 double-avx512-reg 0 32)
(define-test-vop-load %test-broadcast-f64x4-disp32 vbroadcastf64x4 double-avx512-reg 0 33)
(define-test-vop-store %test-compress-disp8 vcompressps single-avx512-reg 0 64)
(define-test-vop-store %test-compress-disp32 vcompressps single-avx512-reg 0 65)
(define-test-vop-load %test-expand-disp8 vpexpandq int-avx512-reg 0 64)
(define-test-vop-load %test-expand-disp32 vpexpandq int-avx512-reg 0 65)
(define-test-vop-mask-mem %test-vpcmpd-disp8 vpcmpd mask-reg 1 int-avx512-reg 0 64 #x1)
(define-test-vop-mask-mem %test-vpcmpd-disp32 vpcmpd mask-reg 1 int-avx512-reg 0 65 #x1)
(define-test-vop-mask-mem %test-vptestmd-disp8 vptestmd mask-reg 1 int-avx512-reg 0 64)
(define-test-vop-mask-mem %test-vptestmd-disp32 vptestmd mask-reg 1 int-avx512-reg 0 65)
(define-test-vop-store %test-vpmovqd-xmm-disp8 vpmovqd single-sse-reg 0 8)
(define-test-vop-store %test-vpmovqd-ymm-disp8 vpmovqd single-avx2-reg 1 16)
(define-test-vop-store %test-vpmovqd-zmm-disp8 vpmovqd int-avx512-reg 2 32)
(define-test-vop-store %test-vpmovqd-zmm-disp32 vpmovqd int-avx512-reg 2 33)
(define-test-vop-store %test-vpmovsqb-xmm-disp8 vpmovsqb single-sse-reg 0 2)
(define-test-vop-store %test-vpmovsqb-ymm-disp8 vpmovsqb int-avx2-reg 1 4)
(define-test-vop-store %test-vpmovsqb-zmm-disp8 vpmovsqb int-avx512-reg 2 8)
(define-test-vop-store %test-vpmovsqb-zmm-disp32 vpmovsqb int-avx512-reg 2 9)
(define-test-vop-load %test-vrcp14ps-xmm-disp8 vrcp14ps single-sse-reg 0 16)
(define-test-vop-load %test-vrcp14ps-ymm-disp8 vrcp14ps single-avx2-reg 1 32)
(define-test-vop-load %test-vrcp14ps-zmm-disp8 vrcp14ps single-avx512-reg 2 64)
(define-test-vop-load %test-vrcp14ps-zmm-disp32 vrcp14ps single-avx512-reg 2 65)
(define-test-vop-load %test-vpabsq-zmm-disp8 vpabsq int-avx512-reg 3 64)
(define-test-vop-reg-reg-mem %test-valignd-xmm-disp8 valignd single-sse-reg 0 single-sse-reg 1 16 0)
(define-test-vop-reg-reg-mem %test-valignd-ymm-disp8 valignd int-avx2-reg 1 int-avx2-reg 2 32 0)
(define-test-vop-reg-reg-mem %test-valignd-zmm-disp8 valignd int-avx512-reg 1 int-avx512-reg 2 64 0)
(define-test-vop-reg-reg-mem %test-valignd-zmm-disp32 valignd int-avx512-reg 1 int-avx512-reg 2 65 0)
(define-test-vop-reg-reg-mem %test-vrangess-disp8 vrangess single-sse-reg 0 single-sse-reg 1 4 0)
(define-test-vop-reg-reg-mem %test-vrangess-disp32 vrangess single-sse-reg 0 single-sse-reg 1 5 0)
(define-test-vop-reg-reg-mem %test-vrangesd-disp8 vrangesd double-sse-reg 0 double-sse-reg 1 8 0)
(define-test-vop-load %test-vrndscaleps-xmm-disp8 vrndscaleps single-sse-reg 0 16 0)
(define-test-vop-load %test-vrndscaleps-zmm-disp8 vrndscaleps single-avx512-reg 2 64 0)
(define-test-vop-load %test-vrndscaleps-zmm-disp32 vrndscaleps single-avx512-reg 2 65 0)
(define-test-vop-load %test-vrndscaless-disp8 vrndscaless single-sse-reg 0 4 0)
(define-test-vop-load %test-vrndscaless-disp32 vrndscaless single-sse-reg 0 5 0)
(define-test-vop-load %test-vrndscalesd-disp8 vrndscalesd double-sse-reg 0 8 0)
(define-test-vop-reg-reg-mem %test-vfixupimmps-xmm-disp8 vfixupimmps single-sse-reg 0 single-sse-reg 1 16 0)
(define-test-vop-reg-reg-mem %test-vfixupimmps-zmm-disp8 vfixupimmps single-avx512-reg 1 single-avx512-reg 2 64 0)
(define-test-vop-reg-reg-mem %test-vfixupimmps-zmm-disp32 vfixupimmps single-avx512-reg 1 single-avx512-reg 2 65 0)
(define-test-vop-reg-reg-mem %test-vfixupimmss-disp8 vfixupimmss single-sse-reg 0 single-sse-reg 1 4 0)
(define-test-vop-reg-reg-mem %test-vfixupimmss-disp32 vfixupimmss single-sse-reg 0 single-sse-reg 1 5 0)
(define-test-vop-reg-reg-mem %test-vfixupimmsd-disp8 vfixupimmsd double-sse-reg 0 double-sse-reg 1 8 0)
(define-test-vop-reg-reg-mem %test-vreducess-disp8 vreducess single-sse-reg 0 single-sse-reg 1 4 0)
(define-test-vop-reg-reg-mem %test-vreducess-disp32 vreducess single-sse-reg 0 single-sse-reg 1 5 0)
(define-test-vop-reg-reg-mem %test-vreducesd-disp8 vreducesd double-sse-reg 0 double-sse-reg 1 8 0)
(define-test-vop-reg-reg-mem %test-vgetmantss-disp8 vgetmantss single-sse-reg 0 single-sse-reg 1 4 0)
(define-test-vop-reg-reg-mem %test-vgetmantss-disp32 vgetmantss single-sse-reg 0 single-sse-reg 1 5 0)
(define-test-vop-reg-reg-mem %test-vgetmantsd-disp8 vgetmantsd double-sse-reg 0 double-sse-reg 1 8 0)
(define-test-vop-reg-reg-mem %test-vgetexpss-disp8 vgetexpss single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vgetexpss-disp32 vgetexpss single-sse-reg 0 single-sse-reg 1 5)
(define-test-vop-reg-reg-mem %test-vgetexpsd-disp8 vgetexpsd double-sse-reg 0 double-sse-reg 1 8)
(define-test-vop-reg-reg-mem %test-vscalefps-xmm-disp8 vscalefps single-sse-reg 0 single-sse-reg 1 16)
(define-test-vop-reg-reg-mem %test-vscalefps-zmm-disp8 vscalefps single-avx512-reg 1 single-avx512-reg 2 64)
(define-test-vop-reg-reg-mem %test-vscalefps-zmm-disp32 vscalefps single-avx512-reg 1 single-avx512-reg 2 65)
(define-test-vop-reg-reg-mem %test-vscalefss-disp8 vscalefss single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vscalefss-disp32 vscalefss single-sse-reg 0 single-sse-reg 1 5)
(define-test-vop-reg-reg-mem %test-vscalefsd-disp8 vscalefsd double-sse-reg 0 double-sse-reg 1 8)
(define-test-vop-scalar-to-gp %test-vcvtss2usi-disp8 vcvtss2usi 4)
(define-test-vop-scalar-to-gp %test-vcvtss2usi-disp32 vcvtss2usi 5)
(define-test-vop-scalar-to-gp %test-vcvtsd2usi-disp8 vcvtsd2usi 8)
(define-test-vop-scalar-to-gp %test-vcvttss2usi-disp8 vcvttss2usi 4)
(define-test-vop-scalar-to-gp %test-vcvttsd2usi-disp8 vcvttsd2usi 8)
(define-test-vop-same-reg-mem %test-vcvtusi2sd-disp8 vcvtusi2sd double-sse-reg 0 8)
(define-test-vop-same-reg-mem %test-vcvtusi2sd-disp32 vcvtusi2sd double-sse-reg 0 9)
(define-test-vop-same-reg-mem %test-vcvtusi2ss-disp8 vcvtusi2ss single-sse-reg 0 8)
(define-test-vop-load %test-vcvtps2udq-zmm-disp8 vcvtps2udq int-avx512-reg 0 64)
(define-test-vop-load %test-vcvtps2udq-zmm-disp32 vcvtps2udq int-avx512-reg 0 65)
(define-test-vop-load %test-vcvtudq2pd-zmm-disp8 vcvtudq2pd double-avx512-reg 0 32)
(define-test-vop-load %test-vcvtudq2pd-xmm-disp8 vcvtudq2pd double-sse-reg 0 8)
(define-test-vop-load %test-vcvtps2qq-zmm-disp8 vcvtps2qq int-avx512-reg 0 32)
(define-test-vop-load %test-vcvtps2qq-zmm-disp32 vcvtps2qq int-avx512-reg 0 33)
(define-test-vop-load %test-vcvtpd2qq-zmm-disp8 vcvtpd2qq int-avx512-reg 0 64)
(define-test-vop-load %test-vcvtqq2ps-zmm-disp8 vcvtqq2ps single-avx512-reg 0 64)
(define-test-vop-load %test-vcvtqq2ps-zmm-disp32 vcvtqq2ps single-avx512-reg 0 65)
(define-test-vop-load %test-auto-promoted-vcvtdq2pd-disp8 vcvtdq2pd double-avx512-reg 0 32)
(define-test-vop-load %test-auto-promoted-vcvtdq2pd-disp32 vcvtdq2pd double-avx512-reg 0 33)
(define-test-vop-load %test-auto-promoted-vcvtps2dq-disp8 vcvtps2dq int-avx512-reg 0 64)
(define-test-vop-load %test-auto-promoted-vcvtps2dq-disp32 vcvtps2dq int-avx512-reg 0 65)
(define-test-vop-load %test-auto-promoted-vrcpps-disp8 vrcpps single-avx512-reg 0 64)
(define-test-vop-load %test-auto-promoted-vpmovsxbw-disp8 vpmovsxbw int-avx512-reg 0 32)
(define-test-vop-three-reg %test-vpand-zmm-disasm vpand int-avx512-reg 0 int-avx512-reg 1 int-avx512-reg 2)
(define-test-vop-load %test-vmovdqa-zmm-disasm vmovdqa single-avx512-reg 0 64)
(define-test-vop-load %test-vbroadcastf128-zmm-disasm vbroadcastf128 single-avx512-reg 0 16)
(define-test-vop-reg-reg-mem %test-vpermt2d-zmm-disp8 vpermt2d single-avx512-reg 0 single-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vpermt2d-zmm-disp32 vpermt2d single-avx512-reg 0 single-avx512-reg 1 65)
(define-test-vop-reg-reg-mem %test-vpmaxsq-zmm-disp8 vpmaxsq int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vprolvd-xmm-disp8 vprolvd int-sse-reg 0 int-sse-reg 1 16)
(define-test-vop-reg-reg-mem %test-vpermt2q-zmm-disp8 vpermt2q int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vpermt2q-zmm-disp32 vpermt2q int-avx512-reg 0 int-avx512-reg 1 65)
(define-test-vop-reg-reg-mem %test-vshuff32x4-zmm-disp8 vshuff32x4 int-avx512-reg 2 int-avx512-reg 3 64 0)
(define-test-vop-reg-reg-mem %test-vshuff32x4-zmm-disp32 vshuff32x4 int-avx512-reg 2 int-avx512-reg 3 65 0)
(define-test-vop-reg-reg-mem %test-vshufi32x4-xmm-disp8 vshufi32x4 int-sse-reg 0 int-sse-reg 1 16 0)
(define-test-vop-reg-reg-mem %test-vblendmps-zmm-disp8 vblendmps single-avx512-reg 0 single-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vblendmps-zmm-disp32 vblendmps single-avx512-reg 0 single-avx512-reg 1 65)
(define-test-vop-reg-reg-mem %test-vpblendmq-zmm-disp8 vpblendmq int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vrangepd-zmm-disp8 vrangepd double-avx512-reg 0 double-avx512-reg 1 64 0)
(define-test-vop-load %test-vreduceps-xmm-disp8 vreduceps single-sse-reg 0 16 0)
(define-test-vop-load %test-vreduceps-zmm-disp8 vreduceps single-avx512-reg 0 64 0)
(define-test-vop-load %test-vreduceps-zmm-disp32 vreduceps single-avx512-reg 0 65 0)
(define-test-vop-reg-reg-mem %test-vrcp14ss-disp8 vrcp14ss single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vrcp14ss-disp32 vrcp14ss single-sse-reg 0 single-sse-reg 1 5)
(define-test-vop-reg-reg-mem %test-vrcp14sd-disp8 vrcp14sd double-sse-reg 0 double-sse-reg 1 8)
(define-test-vop-reg-reg-mem %test-vrsqrt14ss-disp8 vrsqrt14ss single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vrsqrt14sd-disp8 vrsqrt14sd double-sse-reg 0 double-sse-reg 1 8)
(define-test-vop-reg-reg-mem %test-vpblendmb-zmm-disp8 vpblendmb int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-mask-mem %test-vpcmpb-zmm-disp8 vpcmpb mask-reg 1 int-avx512-reg 0 64 #x1)
(define-test-vop-mask-mem %test-vptestmw-zmm-disp8 vptestmw mask-reg 1 int-avx512-reg 0 64)
(define-test-vop-reg-reg-mem %test-vpermw-zmm-disp8 vpermw int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vpsllvw-zmm-disp8 vpsllvw int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vdbpsadbw-zmm-disp8 vdbpsadbw int-avx512-reg 0 int-avx512-reg 1 64 0)
(define-test-vop-reg-reg-mem %test-vpmadd52luq-zmm-disp8 vpmadd52luq int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vpermb-zmm-disp8 vpermb int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-store %test-vpcompressb-zmm-disp8 vpcompressb int-avx512-reg 0 64)
(define-test-vop-reg-reg-mem %test-vpshldw-zmm-disp8 vpshldw int-avx512-reg 0 int-avx512-reg 1 64 0)
(define-test-vop-load %test-vpopcntd-zmm-disp8 vpopcntd int-avx512-reg 0 64)
(define-test-vop-mask-mem %test-vpshufbitqmb-zmm-disp8 vpshufbitqmb mask-reg 1 int-avx512-reg 0 64)
(define-test-vop-masked-reg-reg-mem %test-vaddps-masked-zmm-disp8 vaddps-masked single-avx512-reg 0 single-avx512-reg 1 64 1)
(define-test-vop-masked-reg-reg-mem %test-vaddps-masked-zmm-disp32 vaddps-masked single-avx512-reg 0 single-avx512-reg 1 65 1)
(define-test-vop-masked-reg-reg-mem %test-vpaddq-masked-zmm-disp8 vpaddq-masked int-avx512-reg 0 int-avx512-reg 1 64 1)
(define-test-vop-masked-reg-reg-mem %test-vaddpd-masked-zmm-disp8 vaddpd-masked double-avx512-reg 0 double-avx512-reg 1 64 1)
(define-test-vop-reg-reg-mem %test-vinsertf32x4-zmm-disp8 vinsertf32x4 single-avx512-reg 0 single-avx512-reg 1 16 0)
(define-test-vop-reg-reg-mem %test-vinsertf32x8-zmm-disp8 vinsertf32x8 single-avx512-reg 0 single-avx512-reg 1 32 0)
(define-test-vop-store-imm %test-vextractf32x4-zmm-disp8 vextractf32x4 single-avx512-reg 0 16 0)
(define-test-vop-store-imm %test-vextractf32x8-zmm-disp8 vextractf32x8 single-avx512-reg 0 32 0)
(define-test-vop-reg-reg-mem %test-vfmadd132ps-zmm-disp8 vfmadd132ps single-avx512-reg 0 single-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vfmadd132ss-disp8 vfmadd132ss single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-load %test-vcvtph2ps-zmm-disp8 vcvtph2ps single-avx512-reg 0 32)
(define-test-vop-store-imm %test-vcvtps2ph-zmm-disp8 vcvtps2ph single-avx512-reg 0 32 0)
(define-test-vop-reg-reg-mem %test-vgf2p8mulb-zmm-disp8 vgf2p8mulb int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vgf2p8affineqb-zmm-disp8 vgf2p8affineqb int-avx512-reg 0 int-avx512-reg 1 64 0)
(define-test-vop-reg-reg-mem %test-vpmullq-zmm-disp8 vpmullq int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-mask-single-mem %test-vfpclassps-zmm-disp8 vfpclassps mask-reg 1 64 #x1)
(define-test-vop-mask-single-mem %test-vfpclassss-disp8 vfpclassss mask-reg 1 4 #x1)
(define-test-vop-vsib %test-vsib-no-base-low-index vgatherdps-z
  single-avx512-reg 0 single-avx512-reg 1 64 4 1)
(define-test-vop-vsib %test-vsib-high-index vgatherdps-z single-avx512-reg 0
  single-avx512-reg 16 0 4 1 :base-sc unsigned-reg :base-off rax-offset)
(define-test-vop-vsib %test-vsib-high-base-high-index vgatherdps-z
  single-avx512-reg 0 single-avx512-reg 16 0 8 2 :base-sc unsigned-reg :base-off
  r8-offset)
(define-test-vop-vsib %test-vsib-scatter-low-index vscatterdps-z
  single-avx512-reg 0 single-avx512-reg 3 0 4 3 :base-sc unsigned-reg :base-off
  rax-offset :reverse-p t)
(define-test-vop-vsib %test-vsib-scale-2 vgatherdps-z single-avx512-reg 0
  single-avx512-reg 5 0 2 4 :base-sc unsigned-reg :base-off rbx-offset)
(define-test-vop-vsib %test-vsib-scale-8 vgatherdps-z single-avx512-reg 0
  single-avx512-reg 7 0 8 5 :base-sc unsigned-reg :base-off rdx-offset)
(define-test-vop-vsib %test-vsib-disp8 vgatherdps-z single-avx512-reg 0
  single-avx512-reg 2 8 4 6 :base-sc unsigned-reg :base-off rax-offset)
(define-test-vop-vsib %test-vsib-disp8-compressed vgatherdps-z single-avx512-reg
  0 single-avx512-reg 1 4 4 1 :base-sc unsigned-reg :base-off rax-offset)
(define-test-vop-vsib %test-vsib-scatter-disp8-compressed vscatterdps-z
  single-avx512-reg 0 single-avx512-reg 1 4 4 1 :base-sc unsigned-reg :base-off
  rax-offset :reverse-p t)
(define-test-vop-vsib %test-vsib-qword-disp8-compressed vpgatherqq-z
  int-avx512-reg 0 int-avx512-reg 2 8 8 2 :base-sc unsigned-reg :base-off
  rbx-offset)
(define-test-vop-vsib %test-vsib-nonmultiple-disp32 vgatherdps-z
  single-avx512-reg 0 single-avx512-reg 3 5 4 3 :base-sc unsigned-reg :base-off
  rcx-offset)
(define-test-vop-vsib %test-vsib-high-index-disp8-compressed vgatherdps-z
  single-avx512-reg 0 single-avx512-reg 16 4 4 4 :base-sc unsigned-reg :base-off
  rdx-offset)
(define-test-vop-masked-reg-reg-mem %test-vaddps-masked-z-disp8
  vaddps-masked-z single-avx512-reg 0 single-avx512-reg 1 64 1)
(define-test-vop-masked-reg-reg-mem %test-vaddps-masked-z-disp32
  vaddps-masked-z single-avx512-reg 0 single-avx512-reg 1 65 1)
(define-test-vop-masked-reg-reg-mem %test-vpaddq-masked-z-disp8
  vpaddq-masked-z int-avx512-reg 0 int-avx512-reg 1 64 1)
(define-test-vop-masked-reg-reg-mem %test-vaddpd-masked-z-disp8
  vaddpd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 1)
(define-test-vop-vsib %test-vgatherdps-z-zero
  vgatherdps-z-zero single-avx512-reg 0 single-avx512-reg 1 0 4 1
  :base-sc unsigned-reg :base-off rax-offset)
(define-test-vop-vsib %test-vsib-no-disp
  vgatherdps-z single-avx512-reg 0 single-avx512-reg 1 0 4 1
 :base-sc unsigned-reg :base-off rax-offset)
(define-test-vop-load %test-vpconflictd-zmm-disp8 vpconflictd int-avx512-reg 0 64)
(define-test-vop-load %test-vpconflictq-zmm-disp8 vpconflictq int-avx512-reg 0 64)
(define-test-vop-load %test-vplzcntd-xmm-disp8 vplzcntd int-sse-reg 0 16)
(define-test-vop-load %test-vplzcntq-zmm-disp8 vplzcntq int-avx512-reg 0 64)
(define-test-vop-reg-reg-mem %test-vpdpbusd-zmm-disp8 vpdpbusd int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vpdpbusds-zmm-disp8 vpdpbusds int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-reg-reg-mem %test-vpdpwssd-xmm-disp8 vpdpwssd int-sse-reg 0 int-sse-reg 1 16)
(define-test-vop-reg-reg-mem %test-vpdpwssds-zmm-disp32 vpdpwssds int-avx512-reg 0 int-avx512-reg 1 65)
(define-test-vop-reg-reg-mem %test-vcvtne2ps2bf16-zmm-disp8 vcvtne2ps2bf16 int-avx512-reg 0 int-avx512-reg 1 64)
(define-test-vop-load %test-vcvtneps2bf16-zmm-disp8 vcvtneps2bf16 int-avx512-reg 0 64)
(define-test-vop-reg-reg-mem %test-vdpbf16ps-xmm-disp8 vdpbf16ps int-sse-reg 0 int-sse-reg 1 16)
(define-test-vop-reg-reg-mem %test-vaddps-bcast-zmm-disp8 vaddps-bcast single-avx512-reg 0 single-avx512-reg 1 4)
(define-test-vop-reg-reg-mem %test-vaddps-bcast-xmm-disp8 vaddps-bcast single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vaddpd-bcast-zmm-disp8 vaddpd-bcast double-avx512-reg 0 double-avx512-reg 1 8)
(define-test-vop-reg-reg-mem %test-vmulps-bcast-ymm-disp8 vmulps-bcast single-avx2-reg 1 single-avx2-reg 2 4)
(define-test-vop-masked-reg-reg-mem %test-vdivps-masked-zmm-disp8 vdivps-masked single-avx512-reg 0 single-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vdivpd-masked-zmm-disp8 vdivpd-masked double-avx512-reg 0 double-avx512-reg 1 64 3)
(define-test-vop-masked-reg-reg-mem %test-vdivps-masked-z-zmm-disp8 vdivps-masked-z single-avx512-reg 0 single-avx512-reg 1 64 4)
(define-test-vop-masked-reg-reg-mem %test-vdivpd-masked-z-zmm-disp8 vdivpd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 5)
(define-test-vop-reg-reg-mem %test-vsubps-bcast-ymm-disp8 vsubps-bcast single-avx2-reg 0 single-avx2-reg 1 4)
(define-test-vop-reg-reg-mem %test-vsubpd-bcast-zmm-disp8 vsubpd-bcast double-avx512-reg 0 double-avx512-reg 1 8)
(define-test-vop-reg-reg-mem %test-vdivps-bcast-xmm-disp8 vdivps-bcast single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vdivpd-bcast-zmm-disp8 vdivpd-bcast double-avx512-reg 0 double-avx512-reg 1 8)
(define-test-vop-masked-reg-reg-mem %test-vminps-masked-zmm-disp8 vminps-masked single-avx512-reg 0 single-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vmaxpd-masked-zmm-disp8 vmaxpd-masked double-avx512-reg 0 double-avx512-reg 1 64 3)
(define-test-vop-masked-reg-reg-mem %test-vminps-masked-z-zmm-disp8 vminps-masked-z single-avx512-reg 0 single-avx512-reg 1 64 4)
(define-test-vop-masked-reg-reg-mem %test-vmaxpd-masked-z-zmm-disp8 vmaxpd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 5)
(define-test-vop-reg-reg-mem %test-vminps-bcast-ymm-disp8 vminps-bcast single-avx2-reg 0 single-avx2-reg 1 4)
(define-test-vop-reg-reg-mem %test-vmaxpd-bcast-zmm-disp8 vmaxpd-bcast double-avx512-reg 0 double-avx512-reg 1 8)
(define-test-vop-reg-reg-mem %test-vpaddd-bcast-zmm-disp8 vpaddd-bcast int-avx512-reg 0 int-avx512-reg 1 4)
(define-test-vop-reg-reg-mem %test-vpaddq-bcast-zmm-disp8 vpaddq-bcast int-avx512-reg 0 int-avx512-reg 1 8)
(define-test-vop-reg-reg-mem %test-vpandd-bcast-xmm-disp8 vpandd-bcast int-sse-reg 0 int-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vporq-bcast-ymm-disp8 vporq-bcast int-avx2-reg 1 int-avx2-reg 2 8)
(define-test-vop-reg-reg-mem %test-vpminsd-bcast-zmm-disp8 vpminsd-bcast int-avx512-reg 0 int-avx512-reg 1 4)
(define-test-vop-reg-reg-mem %test-vpmaxsd-bcast-xmm-disp8 vpmaxsd-bcast int-sse-reg 0 int-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vpminud-bcast-ymm-disp8 vpminud-bcast int-avx2-reg 1 int-avx2-reg 2 4)
(define-test-vop-reg-reg-mem %test-vpmaxuq-bcast-zmm-disp8 vpmaxuq-bcast int-avx512-reg 0 int-avx512-reg 1 8)
(define-test-vop-masked-reg-reg-mem %test-vpminsd-masked-zmm-disp8 vpminsd-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vpmaxsq-masked-zmm-disp8 vpmaxsq-masked int-avx512-reg 0 int-avx512-reg 1 64 3)
(define-test-vop-masked-reg-reg-mem %test-vpminsd-masked-z-zmm-disp8 vpminsd-masked-z int-avx512-reg 0 int-avx512-reg 1 64 4)
(define-test-vop-masked-reg-reg-mem %test-vpmaxsq-masked-z-zmm-disp8 vpmaxsq-masked-z int-avx512-reg 0 int-avx512-reg 1 64 5)
(define-test-vop-masked-reg-reg-mem %test-vfmadd132ps-masked-zmm-disp8 vfmadd132ps-masked single-avx512-reg 0 single-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vfmadd231pd-masked-zmm-disp8 vfmadd231pd-masked double-avx512-reg 0 double-avx512-reg 1 64 3)
(define-test-vop-masked-reg-reg-mem %test-vfmadd132ps-masked-z-zmm-disp8 vfmadd132ps-masked-z single-avx512-reg 0 single-avx512-reg 1 64 4)
(define-test-vop-masked-reg-reg-mem %test-vfmadd213pd-masked-z-zmm-disp8 vfmadd213pd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 5)
(define-test-vop-reg-reg-mem %test-vfmadd132ps-bcast-xmm-disp8 vfmadd132ps-bcast single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-reg-reg-mem %test-vfmadd213pd-bcast-zmm-disp8 vfmadd213pd-bcast double-avx512-reg 0 double-avx512-reg 1 8)
(define-test-vop-masked-reg-reg-mem %test-vfmsub132ps-masked-zmm-disp8 vfmsub132ps-masked single-avx512-reg 0 single-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vfmsub213pd-masked-z-zmm-disp8 vfmsub213pd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 3)
(define-test-vop-reg-reg-mem %test-vfnmadd231ps-bcast-xmm-disp8 vfnmadd231ps-bcast single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-masked-reg-reg-mem %test-vfmaddsub132ps-masked-zmm-disp8
  vfmaddsub132ps-masked single-avx512-reg 0 single-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vfmaddsub213pd-masked-z-zmm-disp8
  vfmaddsub213pd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 3)
(define-test-vop-reg-reg-mem %test-vfmaddsub231ps-bcast-xmm-disp8
  vfmaddsub231ps-bcast single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-masked-reg-reg-mem %test-vfmsubadd132ps-masked-zmm-disp8
  vfmsubadd132ps-masked single-avx512-reg 0 single-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vfmsubadd213pd-masked-z-zmm-disp8
  vfmsubadd213pd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 3)
(define-test-vop-reg-reg-mem %test-vfmsubadd231ps-bcast-xmm-disp8
  vfmsubadd231ps-bcast single-sse-reg 0 single-sse-reg 1 4)
(define-test-vop-masked-reg-reg-mem %test-vfmadd132ss-masked-disp8
  vfmadd132ss-masked single-sse-reg 0 single-sse-reg 1 4 2)
(define-test-vop-masked-reg-reg-mem %test-vfmsub213sd-masked-z-disp8
  vfmsub213sd-masked-z double-sse-reg 0 double-sse-reg 1 8 3)
(define-test-vop-masked-reg-reg-mem %test-vpaddb-masked-zmm-disp8 vpaddb-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vpsubw-masked-z-zmm-disp8 vpsubw-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3)
(define-test-vop-masked-reg-reg-mem %test-vpsllvd-masked-zmm-disp8 vpsllvd-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vpsrlvq-masked-z-zmm-disp8 vpsrlvq-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3)
(define-test-vop-masked-reg-reg-mem %test-vpsravd-masked-zmm-disp8 vpsravd-masked int-avx512-reg 0 int-avx512-reg 1 64 4)
(define-test-vop-masked-reg-reg-mem %test-vrangeps-masked-zmm-disp8 vrangeps-masked single-avx512-reg 0 single-avx512-reg 1 64 2 0)
(define-test-vop-masked-reg-reg-mem %test-vrangepd-masked-z-zmm-disp8 vrangepd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 3 0)
(define-test-vop-reg-reg-mem %test-vrangeps-bcast-xmm-disp8 vrangeps-bcast single-sse-reg 0 single-sse-reg 1 4 0)
(define-test-vop-masked-reg-reg-mem %test-vpdpbusd-masked-zmm-disp8 vpdpbusd-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vpdpwssd-masked-z-zmm-disp8 vpdpwssd-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3)
(define-test-vop-reg-reg-mem %test-vpdpbusd-bcast-xmm-disp8 vpdpbusd-bcast int-sse-reg 0 int-sse-reg 1 4)
(define-test-vop-masked-reg-reg-mem %test-vcvtne2ps2bf16-masked-zmm-disp8 vcvtne2ps2bf16-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vdpbf16ps-masked-z-zmm-disp8 vdpbf16ps-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3)
(define-test-vop-masked-reg-mem %test-vcvtneps2bf16-masked-zmm-disp8 vcvtneps2bf16-masked int-avx512-reg 0 64 2)
(define-test-vop-masked-reg-mem %test-vcvtneps2bf16-masked-z-zmm-disp8 vcvtneps2bf16-masked-z int-avx512-reg 0 64 3)
(define-test-vop-masked-reg-reg-mem %test-vpermt2d-masked-zmm-disp8 vpermt2d-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vpermt2q-masked-z-zmm-disp8 vpermt2q-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3)
(define-test-vop-masked-reg-reg-mem %test-vpermb-masked-zmm-disp8 vpermb-masked int-avx512-reg 0 int-avx512-reg 1 64 4)
(define-test-vop-masked-reg-reg-mem %test-vpermw-masked-z-zmm-disp8 vpermw-masked-z int-avx512-reg 0 int-avx512-reg 1 64 5)
(define-test-vop-masked-reg-reg-mem %test-vpmadd52luq-masked-zmm-disp8 vpmadd52luq-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vpshldvw-masked-z-zmm-disp8 vpshldvw-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3)
(define-test-vop-masked-reg-mem %test-vpopcntd-masked-zmm-disp8 vpopcntd-masked int-avx512-reg 0 64 4)
(define-test-vop-masked-reg-mem %test-vpopcntq-masked-z-zmm-disp8 vpopcntq-masked-z int-avx512-reg 0 64 5)
(define-test-vop-masked-reg-mem %test-vrcp14ps-masked-zmm-disp8 vrcp14ps-masked single-avx512-reg 0 64 2)
(define-test-vop-masked-reg-mem %test-vrcp14pd-masked-z-zmm-disp8 vrcp14pd-masked-z double-avx512-reg 0 64 3)
(define-test-vop-masked-reg-mem %test-vrsqrt14ps-masked-zmm-disp8 vrsqrt14ps-masked single-avx512-reg 0 64 4)
(define-test-vop-load %test-vrcp14ps-bcast-xmm-disp8 vrcp14ps-bcast single-sse-reg 0 4)
(define-test-vop-load %test-vrsqrt14pd-bcast-zmm-disp8 vrsqrt14pd-bcast double-avx512-reg 0 8)
(define-test-vop-masked-reg-mem-imm %test-vreduceps-masked-zmm-disp8
  vreduceps-masked single-avx512-reg 0 64 2 0)
(define-test-vop-masked-reg-mem-imm %test-vreducepd-masked-z-zmm-disp8
  vreducepd-masked-z double-avx512-reg 0 64 3 0)
(define-test-vop-load %test-vreduceps-bcast-xmm-disp8
  vreduceps-bcast single-sse-reg 0 4 0)
(define-test-vop-masked-reg-reg-mem %test-vscalefps-masked-zmm-disp8
  vscalefps-masked single-avx512-reg 0 single-avx512-reg 1 64 4)
(define-test-vop-masked-reg-reg-mem %test-vscalefpd-masked-z-zmm-disp8
  vscalefpd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 5)
(define-test-vop-reg-reg-mem %test-vscalefps-bcast-ymm-disp8
  vscalefps-bcast single-avx2-reg 0 single-avx2-reg 1 4)
(define-test-vop-masked-reg-reg-mem %test-vfixupimmps-masked-zmm-disp8
  vfixupimmps-masked single-avx512-reg 0 single-avx512-reg 1 64 6 0)
(define-test-vop-masked-reg-reg-mem %test-vfixupimmpd-masked-z-zmm-disp8
  vfixupimmpd-masked-z double-avx512-reg 0 double-avx512-reg 1 64 7 0)
(define-test-vop-reg-reg-mem %test-vfixupimmps-bcast-xmm-disp8
  vfixupimmps-bcast single-sse-reg 0 single-sse-reg 1 4 0)
(define-test-vop-masked-reg-mem-imm %test-vrndscaleps-masked-zmm-disp8
  vrndscaleps-masked single-avx512-reg 0 64 2 0)
(define-test-vop-masked-reg-mem-imm %test-vrndscalepd-masked-z-zmm-disp8
  vrndscalepd-masked-z double-avx512-reg 0 64 3 0)
(define-test-vop-load %test-vrndscaleps-bcast-ymm-disp8
  vrndscaleps-bcast single-avx2-reg 0 4 0)
(define-test-vop-masked-reg-reg-mem %test-vrcp14ss-masked-disp8
  vrcp14ss-masked single-sse-reg 0 single-sse-reg 1 4 2)
(define-test-vop-masked-reg-reg-mem %test-vrsqrt14sd-masked-z-disp8
  vrsqrt14sd-masked-z double-sse-reg 0 double-sse-reg 1 8 3)
(define-test-vop-masked-reg-reg-mem %test-vgetexpss-masked-disp8
  vgetexpss-masked single-sse-reg 0 single-sse-reg 1 4 2)
(define-test-vop-masked-reg-reg-mem %test-vscalefsd-masked-z-disp8
  vscalefsd-masked-z double-sse-reg 0 double-sse-reg 1 8 3)
(define-test-vop-masked-reg-reg-mem %test-vgetmantss-masked-disp8
  vgetmantss-masked single-sse-reg 0 single-sse-reg 1 4 2 0)
(define-test-vop-masked-reg-reg-mem %test-vrangesd-masked-z-disp8
  vrangesd-masked-z double-sse-reg 0 double-sse-reg 1 8 3 0)
(define-test-vop-masked-reg-reg-mem %test-vfixupimmss-masked-disp8
  vfixupimmss-masked single-sse-reg 0 single-sse-reg 1 4 4 0)
(define-test-vop-masked-reg-mem-imm %test-vrndscaless-masked-disp8
  vrndscaless-masked single-sse-reg 0 4 2 0)
(define-test-vop-masked-reg-mem-imm %test-vrndscalesd-masked-z-disp8
  vrndscalesd-masked-z double-sse-reg 0 8 3 0)
(define-test-vop-masked-reg-mem %test-vgetexpps-masked-zmm-disp8
  vgetexpps-masked single-avx512-reg 0 64 2)
(define-test-vop-masked-reg-mem %test-vgetexppd-masked-z-zmm-disp8
  vgetexppd-masked-z double-avx512-reg 0 64 3)
(define-test-vop-masked-reg-mem-imm %test-vgetmantps-masked-zmm-disp8
  vgetmantps-masked single-avx512-reg 0 64 4 0)
(define-test-vop-masked-reg-mem-imm %test-vgetmantpd-masked-z-zmm-disp8
  vgetmantpd-masked-z double-avx512-reg 0 64 5 0)
(define-test-vop-masked-reg-mem %test-vpabsq-masked-zmm-disp8
  vpabsq-masked int-avx512-reg 0 64 6)
(define-test-vop-masked-reg-reg-mem %test-vgf2p8mulb-masked-zmm-disp8
  vgf2p8mulb-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vgf2p8affineqb-masked-z-zmm-disp8
  vgf2p8affineqb-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3 0)
(define-test-vop-masked-reg-reg-mem %test-vgf2p8affineinvqb-masked-zmm-disp8
  vgf2p8affineinvqb-masked int-avx512-reg 0 int-avx512-reg 1 64 4 0)
(define-test-vop-reg-reg-mem %test-vcvtne2ps2bf16-bcast-zmm-disp8
  vcvtne2ps2bf16-bcast int-avx512-reg 0 int-avx512-reg 1 4)
(define-test-vop-reg-reg-mem %test-vdpbf16ps-bcast-xmm-disp8
  vdpbf16ps-bcast int-sse-reg 0 int-sse-reg 1 4)
(define-test-vop-masked-reg-mem %test-vpconflictd-masked-zmm-disp8
  vpconflictd-masked int-avx512-reg 0 64 2)
(define-test-vop-masked-reg-mem %test-vpconflictq-masked-z-zmm-disp8
  vpconflictq-masked-z int-avx512-reg 0 64 3)
(define-test-vop-masked-reg-mem %test-vplzcntd-masked-xmm-disp8
  vplzcntd-masked int-sse-reg 0 16 4)
(define-test-vop-masked-reg-mem %test-vplzcntq-masked-z-zmm-disp8
  vplzcntq-masked-z int-avx512-reg 0 64 5)
(define-test-vop-masked-reg-reg-mem %test-vaesenc-masked-zmm-disp8
  vaesenc-masked int-avx512-reg 0 int-avx512-reg 1 64 2)
(define-test-vop-masked-reg-reg-mem %test-vaesenclast-masked-z-zmm-disp8
  vaesenclast-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3)
(define-test-vop-masked-reg-reg-mem %test-vaesdec-masked-xmm-disp8
  vaesdec-masked int-sse-reg 0 int-sse-reg 1 16 4)
(define-test-vop-masked-reg-reg-mem %test-vaesdeclast-masked-z-ymm-disp8
  vaesdeclast-masked-z int-avx2-reg 1 int-avx2-reg 2 32 5)
(define-test-vop-three-reg %test-vaesenc-evex-zmm
  vaesenc int-avx512-reg 0 int-avx512-reg 1 int-avx512-reg 2)
(define-test-vop-three-reg %test-vaesenclast-evex-zmm
  vaesenclast int-avx512-reg 0 int-avx512-reg 1 int-avx512-reg 2)
(define-test-vop-three-reg %test-vaesdec-evex-zmm
  vaesdec int-avx512-reg 0 int-avx512-reg 1 int-avx512-reg 2)
(define-test-vop-three-reg %test-vaesdeclast-evex-zmm
  vaesdeclast int-avx512-reg 0 int-avx512-reg 1 int-avx512-reg 2)
(define-test-vop-masked-reg-mem %test-vcvtph2ps-masked-zmm-disp8
  vcvtph2ps-masked single-avx512-reg 0 32 2)
(define-test-vop-masked-reg-mem %test-vcvtph2ps-masked-z-zmm-disp8
  vcvtph2ps-masked-z single-avx512-reg 0 32 3)
(define-test-vop-masked-reg-mem %test-vcvtph2ps-masked-xmm-disp8
  vcvtph2ps-masked single-sse-reg 0 8 4)
(define-test-vop-masked-mem-reg-imm %test-vcvtps2ph-masked-zmm-disp8
  vcvtps2ph-masked single-avx512-reg 0 32 2 0)
(define-test-vop-masked-mem-reg-imm %test-vcvtps2ph-masked-z-zmm-disp8
  vcvtps2ph-masked-z single-avx512-reg 0 32 3 0)
(define-test-vop-masked-mem-reg-imm %test-vcvtps2ph-masked-xmm-disp8
  vcvtps2ph-masked single-sse-reg 0 8 4 0)
(define-test-vop-masked-reg-reg-mem %test-vpshldw-masked-zmm-disp8
  vpshldw-masked int-avx512-reg 0 int-avx512-reg 1 64 2 0)
(define-test-vop-masked-reg-reg-mem %test-vpshldd-masked-z-zmm-disp8
  vpshldd-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3 0)
(define-test-vop-masked-reg-reg-mem %test-vpshrdq-masked-zmm-disp8
  vpshrdq-masked int-avx512-reg 0 int-avx512-reg 1 64 4 0)
(define-test-vop-masked-reg-reg-mem %test-vpmultishiftqb-masked-zmm-disp8
  vpmultishiftqb-masked int-avx512-reg 0 int-avx512-reg 1 64 5)
(define-test-vop-masked-mem-reg %test-vcompressps-masked-zmm-disp8
  vcompressps-masked single-avx512-reg 0 64 2)
(define-test-vop-masked-mem-reg %test-vcompresspd-masked-z-zmm-disp8
  vcompresspd-masked-z double-avx512-reg 0 64 3)
(define-test-vop-masked-mem-reg %test-vpcompressd-masked-xmm-disp8
  vpcompressd-masked int-sse-reg 0 16 4)
(define-test-vop-masked-reg-mem %test-vexpandps-masked-zmm-disp8
  vexpandps-masked single-avx512-reg 0 64 2)
(define-test-vop-masked-reg-mem %test-vexpandpd-masked-z-zmm-disp8
  vexpandpd-masked-z double-avx512-reg 0 64 3)
(define-test-vop-masked-reg-mem %test-vpexpandd-masked-xmm-disp8
  vpexpandd-masked int-sse-reg 0 16 4)
(define-test-vop-masked-mem-reg %test-vpmovqd-masked-zmm-disp8
  vpmovqd-masked int-avx512-reg 0 32 2)
(define-test-vop-masked-mem-reg %test-vpmovqd-masked-z-zmm-disp8
  vpmovqd-masked-z int-avx512-reg 0 32 3)
(define-test-vop-masked-mem-reg %test-vpmovsqb-masked-xmm-disp8
  vpmovsqb-masked single-sse-reg 0 2 4)
(define-test-vop-masked-mem-reg %test-vpmovsqb-masked-z-ymm-disp8
  vpmovsqb-masked-z int-avx2-reg 1 4 5)
(define-test-vop-masked-reg-reg-mem %test-vdbpsadbw-masked-zmm-disp8
  vdbpsadbw-masked int-avx512-reg 0 int-avx512-reg 1 64 2 0)
(define-test-vop-masked-reg-reg-mem %test-vdbpsadbw-masked-z-zmm-disp8
  vdbpsadbw-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3 0)
(define-test-vop-masked-reg-reg-mem %test-vpternlogd-masked-zmm-disp8
  vpternlogd-masked int-avx512-reg 0 int-avx512-reg 1 64 2 0)
(define-test-vop-masked-reg-reg-mem %test-vpternlogd-masked-z-zmm-disp8
  vpternlogd-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3 0)
(define-test-vop-masked-reg-reg-mem %test-vpternlogq-masked-xmm-disp8
  vpternlogq-masked int-sse-reg 0 int-sse-reg 1 16 4 0)
(define-test-vop-masked-reg-reg-mem %test-vpternlogq-masked-z-ymm-disp8
  vpternlogq-masked-z int-avx2-reg 1 int-avx2-reg 2 32 5 0)
(define-test-vop-masked-reg-reg-mem %test-vshuff32x4-masked-zmm-disp8
  vshuff32x4-masked int-avx512-reg 0 int-avx512-reg 1 64 2 0)
(define-test-vop-masked-reg-reg-mem %test-vshuff32x4-masked-z-zmm-disp8
  vshuff32x4-masked-z int-avx512-reg 0 int-avx512-reg 1 64 3 0)
(define-test-vop-masked-reg-reg-mem %test-vshuff64x2-masked-xmm-disp8
  vshuff64x2-masked int-sse-reg 0 int-sse-reg 1 16 4 0)
(define-test-vop-masked-reg-reg-mem %test-vshuff64x2-masked-z-ymm-disp8
  vshuff64x2-masked-z int-avx2-reg 1 int-avx2-reg 2 32 5 0)
(define-test-vop-masked-reg-reg-mem %test-vshufi32x4-masked-zmm-disp8
  vshufi32x4-masked int-avx512-reg 0 int-avx512-reg 1 64 6 0)
(define-test-vop-masked-reg-reg-mem %test-vshufi32x4-masked-z-zmm-disp8
  vshufi32x4-masked-z int-avx512-reg 0 int-avx512-reg 1 64 7 0)
(define-test-vop-masked-reg-reg-mem %test-vshufi64x2-masked-xmm-disp8
  vshufi64x2-masked int-sse-reg 0 int-sse-reg 1 16 2 0)
(define-test-vop-masked-reg-reg-mem %test-vshufi64x2-masked-z-ymm-disp8
  vshufi64x2-masked-z int-avx2-reg 1 int-avx2-reg 2 32 3 0)
(define-test-vop-masked-reg-reg-mem %test-vinsertf32x4-masked-zmm-disp8
  vinsertf32x4-masked single-avx512-reg 0 single-avx512-reg 1 16 2 0)
(define-test-vop-masked-reg-reg-mem %test-vinsertf32x4-masked-z-zmm-disp8
  vinsertf32x4-masked-z single-avx512-reg 0 single-avx512-reg 1 16 3 0)
(define-test-vop-masked-reg-reg-mem %test-vinsertf64x2-masked-xmm-disp8
  vinsertf64x2-masked double-sse-reg 0 double-sse-reg 1 16 4 0)
(define-test-vop-masked-reg-reg-mem %test-vinsertf64x2-masked-z-ymm-disp8
  vinsertf64x2-masked-z double-avx2-reg 1 double-avx2-reg 2 16 5 0)
(define-test-vop-masked-reg-reg-mem %test-vinserti32x4-masked-zmm-disp8
  vinserti32x4-masked int-avx512-reg 0 int-avx512-reg 1 16 6 0)
(define-test-vop-masked-reg-reg-mem %test-vinserti32x4-masked-z-zmm-disp8
  vinserti32x4-masked-z int-avx512-reg 0 int-avx512-reg 1 16 7 0)
(define-test-vop-masked-reg-reg-mem %test-vinserti32x8-masked-ymm-disp8
  vinserti32x8-masked int-avx2-reg 1 int-avx2-reg 2 32 2 0)
(define-test-vop-masked-reg-reg-mem %test-vinserti32x8-masked-z-ymm-disp8
  vinserti32x8-masked-z int-avx2-reg 1 int-avx2-reg 2 32 3 0)

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

;; Full-vector unsigned conversion: vcvtps2udq ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vcvtps2udq-zmm-compressed-disp8
    sb-vm::%test-vcvtps2udq-zmm-disp8
  ("VCVTPS2UDQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Full-vector unsigned conversion: non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vcvtps2udq-zmm-disp32-fallback
    sb-vm::%test-vcvtps2udq-zmm-disp32
  ("VCVTPS2UDQ" "ZMM0" "[RSP+65]"))

;; Widening unsigned conversion: vcvtudq2pd ZMM source -> disp-n=32
(define-evex-disasm-test
    :evex-vcvtudq2pd-zmm-compressed-disp8
    sb-vm::%test-vcvtudq2pd-zmm-disp8
  ("VCVTUDQ2PD" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; Widening unsigned conversion: XMM source -> disp-n=8
(define-evex-disasm-test
    :evex-vcvtudq2pd-xmm-compressed-disp8
    sb-vm::%test-vcvtudq2pd-xmm-disp8
  ("VCVTUDQ2PD" "XMM0" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Narrowing conversion: single -> qword, ZMM memory = 32 bytes, disp-n=32
(define-evex-disasm-test
    :evex-vcvtps2qq-zmm-compressed-disp8
    sb-vm::%test-vcvtps2qq-zmm-disp8
  ("VCVTPS2QQ" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; Narrowing conversion non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vcvtps2qq-zmm-disp32-fallback
    sb-vm::%test-vcvtps2qq-zmm-disp32
  ("VCVTPS2QQ" "ZMM0" "[RSP+33]"))

;; Full-width conversion: double -> qword, ZMM memory = 64 bytes, disp-n=64
(define-evex-disasm-test
    :evex-vcvtpd2qq-zmm-compressed-disp8
    sb-vm::%test-vcvtpd2qq-zmm-disp8
  ("VCVTPD2QQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Full-width conversion: qword -> single, ZMM memory = 64 bytes, disp-n=64
(define-evex-disasm-test
    :evex-vcvtqq2ps-zmm-compressed-disp8
    sb-vm::%test-vcvtqq2ps-zmm-disp8
  ("VCVTQQ2PS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Full-width conversion non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vcvtqq2ps-zmm-disp32-fallback
    sb-vm::%test-vcvtqq2ps-zmm-disp32
  ("VCVTQQ2PS" "ZMM0" "[RSP+65]"))

;; Auto-promoted widening conversion: vcvtdq2pd ZMM -> disp-n=32
(define-evex-disasm-test
    :auto-promoted-vcvtdq2pd-compressed-disp8
    sb-vm::%test-auto-promoted-vcvtdq2pd-disp8
  ("VCVTDQ2PD" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; Auto-promoted widening conversion non-multiple falls back to disp32
(define-evex-disasm-test
    :auto-promoted-vcvtdq2pd-disp32-fallback
    sb-vm::%test-auto-promoted-vcvtdq2pd-disp32
  ("VCVTDQ2PD" "ZMM0" "[RSP+33]"))

;; Auto-promoted same-width conversion: vcvtps2dq ZMM -> disp-n=64
(define-evex-disasm-test
    :auto-promoted-vcvtps2dq-compressed-disp8
    sb-vm::%test-auto-promoted-vcvtps2dq-disp8
  ("VCVTPS2DQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Auto-promoted same-width conversion non-multiple falls back to disp32
(define-evex-disasm-test
    :auto-promoted-vcvtps2dq-disp32-fallback
    sb-vm::%test-auto-promoted-vcvtps2dq-disp32
  ("VCVTPS2DQ" "ZMM0" "[RSP+65]"))

;; Auto-promoted full-vector same width: vrcpps ZMM -> disp-n=64
(define-evex-disasm-test
    :auto-promoted-vrcpps-compressed-disp8
    sb-vm::%test-auto-promoted-vrcpps-disp8
  ("VRCPPS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Auto-promoted widening conversion: vpmovsxbw ZMM -> disp-n=32
(define-evex-disasm-test
    :auto-promoted-vpmovsxbw-compressed-disp8
    sb-vm::%test-auto-promoted-vpmovsxbw-disp8
  ("VPMOVSXBW" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; vpand skip: explicit VPANDD should be used, not auto-promoted VPAND
(define-evex-disasm-test
    :auto-evex-skip-vpand
    sb-vm::%test-vpand-zmm-disasm
  ("VPANDD" "ZMM0" "ZMM1" "ZMM2")
  :unexpected ("VPAND "))

;; vmovdqa skip: explicit VMOVDQA32 should be used
(define-evex-disasm-test
    :auto-evex-skip-vmovdqa
    sb-vm::%test-vmovdqa-zmm-disasm
  ("VMOVDQA32" "ZMM0" "[RSP+64]")
  :unexpected ("VMOVDQA "))

;; vbroadcastf128 skip: explicit VBROADCASTF32X4 should be used
(define-evex-disasm-test
    :auto-evex-skip-vbroadcastf128
    sb-vm::%test-vbroadcastf128-zmm-disasm
  ("VBROADCASTF32X4" "ZMM0" "[RSP+16]")
  :unexpected ("VBROADCASTF128"))

;; Full-vector 3-operand NDS: vpermt2d ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vpermt2d-zmm-compressed-disp8
    sb-vm::%test-vpermt2d-zmm-disp8
  ("VPERMT2D" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Full-vector 3-operand NDS: vpermt2d disp32 fallback
(define-evex-disasm-test
    :evex-vpermt2d-zmm-disp32-fallback
    sb-vm::%test-vpermt2d-zmm-disp32
  ("VPERMT2D" "ZMM0" "ZMM1" "[RSP+65]"))

;; Full-vector 3-operand NDS: vpmaxsq ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vpmaxsq-zmm-compressed-disp8
    sb-vm::%test-vpmaxsq-zmm-disp8
  ("VPMAXSQ" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Full-vector 3-operand NDS: vprolvd XMM -> disp-n=16
(define-evex-disasm-test
    :evex-vprolvd-xmm-compressed-disp8
    sb-vm::%test-vprolvd-xmm-disp8
  ("VPROLVD" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Full-vector 3-operand NDS: vpermt2q ZMM, W=1 -> disp-n=64
(define-evex-disasm-test
    :evex-vpermt2q-zmm-compressed-disp8
    sb-vm::%test-vpermt2q-zmm-disp8
  ("VPERMT2Q" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; vpermt2q disp32 fallback
(define-evex-disasm-test
    :evex-vpermt2q-zmm-disp32-fallback
    sb-vm::%test-vpermt2q-zmm-disp32
  ("VPERMT2Q" "ZMM0" "ZMM1" "[RSP+65]"))

;; Cross-lane shuffle: vshuff32x4 ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vshuff32x4-zmm-compressed-disp8
    sb-vm::%test-vshuff32x4-zmm-disp8
  ("VSHUFF32X4" "ZMM2" "ZMM3" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; vshuff32x4 disp32 fallback
(define-evex-disasm-test
    :evex-vshuff32x4-zmm-disp32-fallback
    sb-vm::%test-vshuff32x4-zmm-disp32
  ("VSHUFF32X4" "ZMM2" "ZMM3" "[RSP+65]"))

;; Cross-lane shuffle: vshufi32x4 XMM -> disp-n=16
(define-evex-disasm-test
    :evex-vshufi32x4-xmm-compressed-disp8
    sb-vm::%test-vshufi32x4-xmm-disp8
  ("VSHUFI32X4" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Blend with mask: vblendmps ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vblendmps-zmm-compressed-disp8
    sb-vm::%test-vblendmps-zmm-disp8
  ("VBLENDMPS" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Blend with mask: vblendmps disp32 fallback
(define-evex-disasm-test
    :evex-vblendmps-zmm-disp32-fallback
    sb-vm::%test-vblendmps-zmm-disp32
  ("VBLENDMPS" "ZMM0" "ZMM1" "[RSP+65]"))

;; Blend with mask (integer): vpblendmq ZMM, W=1 -> disp-n=64
(define-evex-disasm-test
    :evex-vpblendmq-zmm-compressed-disp8
    sb-vm::%test-vpblendmq-zmm-disp8
  ("VPBLENDMQ" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Range: vrangepd ZMM, W=1 -> disp-n=64
(define-evex-disasm-test
    :evex-vrangepd-zmm-compressed-disp8
    sb-vm::%test-vrangepd-zmm-disp8
  ("VRANGEPD" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Reduce: vreduceps XMM -> disp-n=16
(define-evex-disasm-test
    :evex-vreduceps-xmm-compressed-disp8
    sb-vm::%test-vreduceps-xmm-disp8
  ("VREDUCEPS" "XMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Reduce: vreduceps ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vreduceps-zmm-compressed-disp8
    sb-vm::%test-vreduceps-zmm-disp8
  ("VREDUCEPS" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Reduce: vreduceps disp32 fallback
(define-evex-disasm-test
    :evex-vreduceps-zmm-disp32-fallback
    sb-vm::%test-vreduceps-zmm-disp32
  ("VREDUCEPS" "ZMM0" "[RSP+65]"))

;; Scalar reciprocal: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vrcp14ss-compressed-disp8
    sb-vm::%test-vrcp14ss-disp8
  ("VRCP14SS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar reciprocal: single precision non-multiple falls back to disp32
(define-evex-disasm-test
    :evex-vrcp14ss-disp32-fallback
    sb-vm::%test-vrcp14ss-disp32
  ("VRCP14SS" "XMM0" "XMM1" "[RSP+5]"))

;; Scalar reciprocal: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vrcp14sd-compressed-disp8
    sb-vm::%test-vrcp14sd-disp8
  ("VRCP14SD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Scalar rsqrt: single precision uses disp-n=4
(define-evex-disasm-test
    :evex-vrsqrt14ss-compressed-disp8
    sb-vm::%test-vrsqrt14ss-disp8
  ("VRSQRT14SS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; Scalar rsqrt: double precision uses disp-n=8
(define-evex-disasm-test
    :evex-vrsqrt14sd-compressed-disp8
    sb-vm::%test-vrsqrt14sd-disp8
  ("VRSQRT14SD" "XMM0" "XMM1" "[RSP+8]")
  :unexpected ("[RSP+1]"))

;; Blend byte: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vpblendmb-zmm-compressed-disp8
    sb-vm::%test-vpblendmb-zmm-disp8
  ("VPBLENDMB" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Compare byte to k: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vpcmpb-zmm-compressed-disp8
    sb-vm::%test-vpcmpb-zmm-disp8
  ("VPCMPB" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Test word to k: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vptestmw-zmm-compressed-disp8
    sb-vm::%test-vptestmw-zmm-disp8
  ("VPTESTMW" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Permute word: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vpermw-zmm-compressed-disp8
    sb-vm::%test-vpermw-zmm-disp8
  ("VPERMW" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Variable shift word: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vpsllvw-zmm-compressed-disp8
    sb-vm::%test-vpsllvw-zmm-disp8
  ("VPSLLVW" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Double-block SAD: ZMM full-vector -> disp-n=64
(define-evex-disasm-test
    :evex-vdbpsadbw-zmm-compressed-disp8
    sb-vm::%test-vdbpsadbw-zmm-disp8
  ("VDBPSADBW" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; IFMA: vpmadd52luq ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vpmadd52luq-zmm-compressed-disp8
    sb-vm::%test-vpmadd52luq-zmm-disp8
  ("VPMADD52LUQ" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VBMI permute: vpermb ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vpermb-zmm-compressed-disp8
    sb-vm::%test-vpermb-zmm-disp8
  ("VPERMB" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VBMI2 compress: vpcompressb ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vpcompressb-zmm-compressed-disp8
    sb-vm::%test-vpcompressb-zmm-disp8
  ("VPCOMPRESSB" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VBMI2 shift immediate: vpshldw ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vpshldw-zmm-compressed-disp8
    sb-vm::%test-vpshldw-zmm-disp8
  ("VPSHLDW" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VPOPCNTDQ: vpopcntd ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vpopcntd-zmm-compressed-disp8
    sb-vm::%test-vpopcntd-zmm-disp8
  ("VPOPCNTD" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; BITALG: vpshufbitqmb ZMM -> disp-n=64
(define-evex-disasm-test
    :evex-vpshufbitqmb-zmm-compressed-disp8
    sb-vm::%test-vpshufbitqmb-zmm-disp8
  ("VPSHUFBITQMB" "K1" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; Masked arithmetic: vaddps-masked ZMM, W=0 -> disp-n=64
(define-evex-disasm-test
    :evex-vaddps-masked-zmm-compressed-disp8
    sb-vm::%test-vaddps-masked-zmm-disp8
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+64]" "{K1}")
  :unexpected ("[RSP+1]"))

;; Masked arithmetic: vaddps-masked ZMM disp32 fallback
(define-evex-disasm-test
    :evex-vaddps-masked-zmm-disp32-fallback
    sb-vm::%test-vaddps-masked-zmm-disp32
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+65]" "{K1}"))

;; Masked integer arithmetic: vpaddq-masked ZMM, W=1 -> disp-n=64
(define-evex-disasm-test
    :evex-vpaddq-masked-zmm-compressed-disp8
    sb-vm::%test-vpaddq-masked-zmm-disp8
  ("VPADDQ" "ZMM0" "ZMM1" "[RSP+64]" "{K1}")
  :unexpected ("[RSP+1]"))

;; Masked double arithmetic: vaddpd-masked ZMM, W=1 -> disp-n=64
(define-evex-disasm-test
    :evex-vaddpd-masked-zmm-compressed-disp8
    sb-vm::%test-vaddpd-masked-zmm-disp8
  ("VADDPD" "ZMM0" "ZMM1" "[RSP+64]" "{K1}")
  :unexpected ("[RSP+1]"))

;; Insert 128-bit lane: disp-n=16
(define-evex-disasm-test
    :evex-vinsertf32x4-zmm-compressed-disp8
    sb-vm::%test-vinsertf32x4-zmm-disp8
  ("VINSERTF32X4" "ZMM0" "ZMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; Insert 256-bit lane: disp-n=32
(define-evex-disasm-test
    :evex-vinsertf32x8-zmm-compressed-disp8
    sb-vm::%test-vinsertf32x8-zmm-disp8
  ("VINSERTF32X8" "ZMM0" "ZMM1" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; Extract 128-bit lane: disp-n=16
(define-evex-disasm-test
    :evex-vextractf32x4-zmm-compressed-disp8
    sb-vm::%test-vextractf32x4-zmm-disp8
  ("VEXTRACTF32X4" "[RSP+16]" "ZMM0")
  :unexpected ("[RSP+1]"))

;; Extract 256-bit lane: disp-n=32
(define-evex-disasm-test
    :evex-vextractf32x8-zmm-compressed-disp8
    sb-vm::%test-vextractf32x8-zmm-disp8
  ("VEXTRACTF32X8" "[RSP+32]" "ZMM0")
  :unexpected ("[RSP+1]"))

;; FMA packed: full-vector, disp-n=64
(define-evex-disasm-test
    :evex-vfmadd132ps-zmm-compressed-disp8
    sb-vm::%test-vfmadd132ps-zmm-disp8
  ("VFMADD132PS" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; FMA scalar: fixed disp-n=4
(define-evex-disasm-test
    :evex-vfmadd132ss-compressed-disp8
    sb-vm::%test-vfmadd132ss-disp8
  ("VFMADD132SS" "XMM0" "XMM1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; F16C load: widening, disp-n=32 for ZMM
(define-evex-disasm-test
    :evex-vcvtph2ps-zmm-compressed-disp8
    sb-vm::%test-vcvtph2ps-zmm-disp8
  ("VCVTPH2PS" "ZMM0" "[RSP+32]")
  :unexpected ("[RSP+1]"))

;; F16C store: narrowing, disp-n=32 for ZMM
(define-evex-disasm-test
    :evex-vcvtps2ph-zmm-compressed-disp8
    sb-vm::%test-vcvtps2ph-zmm-disp8
  ("VCVTPS2PH" "[RSP+32]" "ZMM0")
  :unexpected ("[RSP+1]"))

;; GFNI mulb: full-vector, disp-n=64
(define-evex-disasm-test
    :evex-vgf2p8mulb-zmm-compressed-disp8
    sb-vm::%test-vgf2p8mulb-zmm-disp8
  ("VGF2P8MULB" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; GFNI affine: full-vector, disp-n=64
(define-evex-disasm-test
    :evex-vgf2p8affineqb-zmm-compressed-disp8
    sb-vm::%test-vgf2p8affineqb-zmm-disp8
  ("VGF2P8AFFINEQB" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; vfpclassps packed: full-vector source, disp-n=64
(define-evex-disasm-test
    :evex-vfpclassps-zmm-compressed-disp8
  sb-vm::%test-vfpclassps-zmm-disp8
  ("VFPCLASSPS" "K1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; vfpclassss scalar: fixed disp-n=4
(define-evex-disasm-test
    :evex-vfpclassss-compressed-disp8
    sb-vm::%test-vfpclassss-disp8
  ("VFPCLASSSS" "K1" "[RSP+4]")
  :unexpected ("[RSP+1]"))

;; vpmullq full-vector: disp-n=64
(define-evex-disasm-test
    :evex-vpmullq-zmm-compressed-disp8
    sb-vm::%test-vpmullq-zmm-disp8
  ("VPMULLQ" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(with-test (:name :evex-vsib-high-index-disasm)
  (let* ((fun (compile nil '(lambda () (sb-vm::%test-vsib-high-index))))
         (text (with-output-to-string (s) (disassemble fun :stream s))))
    (assert (search "VGATHERDPS" text))
    (assert (search "ZMM0" text))     ; destination register
    (assert (search "ZMM16" text))    ; high vector index
    (assert (search "{K1}" text))))   ; mask

;; VSIB No base, low index, displacement
(define-evex-disasm-test
    :evex-vsib-no-base-low-index
    sb-vm::%test-vsib-no-base-low-index
  ("VGATHERDPS" "ZMM0" "[ZMM1*4+64]" "K1"))

;; VSIB High base and high vector index
(define-evex-disasm-test
    :evex-vsib-high-base-high-index
    sb-vm::%test-vsib-high-base-high-index
  ("VGATHERDPS" "ZMM0" "ZMM16*8" "K2"))

;; VSIB Scatter, low index
(define-evex-disasm-test
    :evex-vsib-scatter-low-index
    sb-vm::%test-vsib-scatter-low-index
  ("VSCATTERDPS" "ZMM0" "ZMM3" "K3"))

;; VSIB Scale 2
(define-evex-disasm-test
    :evex-vsib-scale-2
    sb-vm::%test-vsib-scale-2
  ("VGATHERDPS" "ZMM0" "ZMM5*2" "K4"))

;; VSIB Scale 8
(define-evex-disasm-test
    :evex-vsib-scale-8
    sb-vm::%test-vsib-scale-8
  ("VGATHERDPS" "ZMM0" "ZMM7*8" "K5"))

;; VSIB Compressed displacement (disp8)
(define-evex-disasm-test
    :evex-vsib-disp8
    sb-vm::%test-vsib-disp8
  ("VGATHERDPS" "ZMM0" "ZMM2*4+8" "K6"))

(define-evex-disasm-test
    :evex-vsib-disp8-compressed
    sb-vm::%test-vsib-disp8-compressed
  ("VGATHERDPS" "ZMM0" "ZMM1*4+4" "K1"))

;; Scatter with compressed displacement
(define-evex-disasm-test
    :evex-vsib-scatter-disp8-compressed
    sb-vm::%test-vsib-scatter-disp8-compressed
  ("VSCATTERDPS" "ZMM0" "ZMM1*4+4" "K1"))

;; Qword gather uses disp-n=8
(define-evex-disasm-test
    :evex-vsib-qword-disp8-compressed
    sb-vm::%test-vsib-qword-disp8-compressed
  ("VPGATHERQQ" "ZMM0" "ZMM2*8+8" "K2"))

;; Non-multiple displacement falls back to disp32
(define-evex-disasm-test
    :evex-vsib-nonmultiple-disp32
    sb-vm::%test-vsib-nonmultiple-disp32
  ("VGATHERDPS" "ZMM0" "ZMM3*4+5" "K3"))

;; High vector index with compressed displacement
(define-evex-disasm-test
    :evex-vsib-high-index-disp8-compressed
    sb-vm::%test-vsib-high-index-disp8-compressed
  ("VGATHERDPS" "ZMM0" "ZMM16*4+4" "K4"))

(define-evex-disasm-test
    :evex-vaddps-masked-z-disp32-fallback
    sb-vm::%test-vaddps-masked-z-disp32
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+65]" "{K1}{z}"))

;; Zeroing masked arithmetic: single precision
(define-evex-disasm-test
    :evex-vaddps-masked-z-compressed-disp8
    sb-vm::%test-vaddps-masked-z-disp8
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+64]" "{K1}{z}")
  :unexpected ("[RSP+1]"))

;; Zeroing masked arithmetic: single precision, disp32 fallback
(define-evex-disasm-test
    :evex-vaddps-masked-z-disp32-fallback
    sb-vm::%test-vaddps-masked-z-disp32
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+65]" "{K1}{z}"))

;; Zeroing masked integer arithmetic: qword
(define-evex-disasm-test
    :evex-vpaddq-masked-z-compressed-disp8
    sb-vm::%test-vpaddq-masked-z-disp8
  ("VPADDQ" "ZMM0" "ZMM1" "[RSP+64]" "{K1}{z}")
  :unexpected ("[RSP+1]"))

;; Zeroing masked double precision
(define-evex-disasm-test
    :evex-vaddpd-masked-z-compressed-disp8
    sb-vm::%test-vaddpd-masked-z-disp8
  ("VADDPD" "ZMM0" "ZMM1" "[RSP+64]" "{K1}{z}")
  :unexpected ("[RSP+1]"))


;; Zeroing masked arithmetic: single precision
(define-evex-disasm-test
    :evex-vaddps-masked-z-compressed-disp8
    sb-vm::%test-vaddps-masked-z-disp8
  ("VADDPS" "ZMM0" "ZMM1" "[RSP+64]" "{K1}{z}")
  :unexpected ("[RSP+1]"))

;; Zeroing masked integer arithmetic: qword
(define-evex-disasm-test
    :evex-vpaddq-masked-z-compressed-disp8
    sb-vm::%test-vpaddq-masked-z-disp8
  ("VPADDQ" "ZMM0" "ZMM1" "[RSP+64]" "{K1}{z}")
  :unexpected ("[RSP+1]"))

;; Zeroing masked double precision
(define-evex-disasm-test
    :evex-vaddpd-masked-z-compressed-disp8
    sb-vm::%test-vaddpd-masked-z-disp8
  ("VADDPD" "ZMM0" "ZMM1" "[RSP+64]" "{K1}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vgatherdps-z-zero
    sb-vm::%test-vgatherdps-z-zero
  ("VGATHERDPS" "ZMM0" "ZMM1*4" "{K1}{z}")
  :unexpected ("[RSP+1]"))

;; Ensure zero displacement with base+index does not print "+0"
(define-evex-disasm-test
    :evex-vsib-no-disp
    sb-vm::%test-vsib-no-disp
  ("VGATHERDPS" "ZMM0" "ZMM1*4" "K1")
  :unexpected ("+0"))

;; VPCONFLICTD ZMM full-vector
(define-evex-disasm-test
    :evex-vpconflictd-zmm-compressed-disp8
    sb-vm::%test-vpconflictd-zmm-disp8
  ("VPCONFLICTD" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VPCONFLICTQ ZMM full-vector, W=1
(define-evex-disasm-test
    :evex-vpconflictq-zmm-compressed-disp8
    sb-vm::%test-vpconflictq-zmm-disp8
  ("VPCONFLICTQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VPLZCNTD XMM full-vector
(define-evex-disasm-test
    :evex-vplzcntd-xmm-compressed-disp8
    sb-vm::%test-vplzcntd-xmm-disp8
  ("VPLZCNTD" "XMM0" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; VPLZCNTQ ZMM full-vector, W=1
(define-evex-disasm-test
    :evex-vplzcntq-zmm-compressed-disp8
    sb-vm::%test-vplzcntq-zmm-disp8
  ("VPLZCNTQ" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VPDPBUSD ZMM full-vector
(define-evex-disasm-test
    :evex-vpdpbusd-zmm-compressed-disp8
    sb-vm::%test-vpdpbusd-zmm-disp8
  ("VPDPBUSD" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VPDPBUSDS ZMM full-vector
(define-evex-disasm-test
    :evex-vpdpbusds-zmm-compressed-disp8
    sb-vm::%test-vpdpbusds-zmm-disp8
  ("VPDPBUSDS" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

;; VPDPWSSD XMM full-vector
(define-evex-disasm-test
    :evex-vpdpwssd-xmm-compressed-disp8
    sb-vm::%test-vpdpwssd-xmm-disp8
  ("VPDPWSSD" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

;; VPDPWSSDS ZMM with disp32 fallback
(define-evex-disasm-test
    :evex-vpdpwssds-zmm-disp32-fallback
    sb-vm::%test-vpdpwssds-zmm-disp32
  ("VPDPWSSDS" "ZMM0" "ZMM1" "[RSP+65]"))

(define-evex-disasm-test
    :evex-vcvtne2ps2bf16-zmm-compressed-disp8
    sb-vm::%test-vcvtne2ps2bf16-zmm-disp8
  ("VCVTNE2PS2BF16" "ZMM0" "ZMM1" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vcvtneps2bf16-zmm-compressed-disp8
    sb-vm::%test-vcvtneps2bf16-zmm-disp8
  ("VCVTNEPS2BF16" "ZMM0" "[RSP+64]")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vdpbf16ps-xmm-compressed-disp8
    sb-vm::%test-vdpbf16ps-xmm-disp8
  ("VDPBF16PS" "XMM0" "XMM1" "[RSP+16]")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vp2intersectd
    sb-vm::%test-vp2intersectd
  ("VP2INTERSECTD" "K1" "K2" "ZMM0" "[RSP+64]"))

(define-evex-disasm-test
    :evex-vp2intersectq
    sb-vm::%test-vp2intersectq
  ("VP2INTERSECTQ" "K3" "K4" "ZMM2" "[RSP+64]"))

;; Embedded broadcast: vaddps ZMM {1to16}
(define-evex-disasm-test
    :evex-vaddps-bcast-zmm
    sb-vm::%test-vaddps-bcast-zmm-disp8
  ("VADDPS-BCAST" "ZMM0" "ZMM1" "[RSP+4]" "{1to16}")
  :unexpected ("[RSP+1]"))

;; Embedded broadcast: vaddps XMM {1to4}
(define-evex-disasm-test
    :evex-vaddps-bcast-xmm
    sb-vm::%test-vaddps-bcast-xmm-disp8
  ("VADDPS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; Embedded broadcast: vaddpd ZMM {1to8}
(define-evex-disasm-test
    :evex-vaddpd-bcast-zmm
    sb-vm::%test-vaddpd-bcast-zmm-disp8
  ("VADDPD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Embedded broadcast: vmulps YMM {1to8}
(define-evex-disasm-test
    :evex-vmulps-bcast-ymm
    sb-vm::%test-vmulps-bcast-ymm-disp8
  ("VMULPS-BCAST" "YMM1" "YMM2" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Masked divide single precision
(define-evex-disasm-test
    :evex-vdivps-masked-zmm-disp8
    sb-vm::%test-vdivps-masked-zmm-disp8
  ("VDIVPS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Masked divide double precision
(define-evex-disasm-test
    :evex-vdivpd-masked-zmm-disp8
    sb-vm::%test-vdivpd-masked-zmm-disp8
  ("VDIVPD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}")
  :unexpected ("[RSP+1]"))

;; Zeroing divide single precision
(define-evex-disasm-test
    :evex-vdivps-masked-z-zmm-disp8
    sb-vm::%test-vdivps-masked-z-zmm-disp8
  ("VDIVPS" "ZMM0" "ZMM1" "[RSP+64]" "{K4}{z}")
  :unexpected ("[RSP+1]"))

;; Zeroing divide double precision
(define-evex-disasm-test
    :evex-vdivpd-masked-z-zmm-disp8
    sb-vm::%test-vdivpd-masked-z-zmm-disp8
  ("VDIVPD" "ZMM0" "ZMM1" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Broadcast subtract single precision
(define-evex-disasm-test
    :evex-vsubps-bcast-ymm-disp8
    sb-vm::%test-vsubps-bcast-ymm-disp8
  ("VSUBPS-BCAST" "YMM0" "YMM1" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Broadcast subtract double precision
(define-evex-disasm-test
    :evex-vsubpd-bcast-zmm-disp8
    sb-vm::%test-vsubpd-bcast-zmm-disp8
  ("VSUBPD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Broadcast divide single precision
(define-evex-disasm-test
    :evex-vdivps-bcast-xmm-disp8
    sb-vm::%test-vdivps-bcast-xmm-disp8
  ("VDIVPS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; Broadcast divide double precision
(define-evex-disasm-test
    :evex-vdivpd-bcast-zmm-disp8
    sb-vm::%test-vdivpd-bcast-zmm-disp8
  ("VDIVPD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Masked min single
(define-evex-disasm-test
    :evex-vminps-masked-zmm-disp8
    sb-vm::%test-vminps-masked-zmm-disp8
  ("VMINPS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Masked max double
(define-evex-disasm-test
    :evex-vmaxpd-masked-zmm-disp8
    sb-vm::%test-vmaxpd-masked-zmm-disp8
  ("VMAXPD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}")
  :unexpected ("[RSP+1]"))

;; Zeroing min single
(define-evex-disasm-test
    :evex-vminps-masked-z-zmm-disp8
    sb-vm::%test-vminps-masked-z-zmm-disp8
  ("VMINPS" "ZMM0" "ZMM1" "[RSP+64]" "{K4}{z}")
  :unexpected ("[RSP+1]"))

;; Zeroing max double
(define-evex-disasm-test
    :evex-vmaxpd-masked-z-zmm-disp8
    sb-vm::%test-vmaxpd-masked-z-zmm-disp8
  ("VMAXPD" "ZMM0" "ZMM1" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Broadcast min single
(define-evex-disasm-test
    :evex-vminps-bcast-ymm-disp8
    sb-vm::%test-vminps-bcast-ymm-disp8
  ("VMINPS-BCAST" "YMM0" "YMM1" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Broadcast max double
(define-evex-disasm-test
    :evex-vmaxpd-bcast-zmm-disp8
    sb-vm::%test-vmaxpd-bcast-zmm-disp8
  ("VMAXPD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Broadcast integer add dword
(define-evex-disasm-test
    :evex-vpaddd-bcast-zmm-disp8
    sb-vm::%test-vpaddd-bcast-zmm-disp8
  ("VPADDD-BCAST" "ZMM0" "ZMM1" "[RSP+4]" "{1to16}")
  :unexpected ("[RSP+1]"))

;; Broadcast integer add qword
(define-evex-disasm-test
    :evex-vpaddq-bcast-zmm-disp8
    sb-vm::%test-vpaddq-bcast-zmm-disp8
  ("VPADDQ-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Broadcast logical and dword
(define-evex-disasm-test
    :evex-vpandd-bcast-xmm-disp8
    sb-vm::%test-vpandd-bcast-xmm-disp8
  ("VPANDD-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; Broadcast logical or qword
(define-evex-disasm-test
    :evex-vporq-bcast-ymm-disp8
    sb-vm::%test-vporq-bcast-ymm-disp8
  ("VPORQ-BCAST" "YMM1" "YMM2" "[RSP+8]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; Broadcast signed dword min
(define-evex-disasm-test
    :evex-vpminsd-bcast-zmm-disp8
    sb-vm::%test-vpminsd-bcast-zmm-disp8
  ("VPMINSD-BCAST" "ZMM0" "ZMM1" "[RSP+4]" "{1to16}")
  :unexpected ("[RSP+1]"))

;; Broadcast signed dword max
(define-evex-disasm-test
    :evex-vpmaxsd-bcast-xmm-disp8
    sb-vm::%test-vpmaxsd-bcast-xmm-disp8
  ("VPMAXSD-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; Broadcast unsigned dword min
(define-evex-disasm-test
    :evex-vpminud-bcast-ymm-disp8
    sb-vm::%test-vpminud-bcast-ymm-disp8
  ("VPMINUD-BCAST" "YMM1" "YMM2" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Broadcast unsigned qword max
(define-evex-disasm-test
    :evex-vpmaxuq-bcast-zmm-disp8
    sb-vm::%test-vpmaxuq-bcast-zmm-disp8
  ("VPMAXUQ-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Masked signed dword min
(define-evex-disasm-test
    :evex-vpminsd-masked-zmm-disp8
    sb-vm::%test-vpminsd-masked-zmm-disp8
  ("VPMINSD" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Masked signed qword max
(define-evex-disasm-test
    :evex-vpmaxsq-masked-zmm-disp8
    sb-vm::%test-vpmaxsq-masked-zmm-disp8
  ("VPMAXSQ" "ZMM0" "ZMM1" "[RSP+64]" "{K3}")
  :unexpected ("[RSP+1]"))

;; Zeroing signed dword min
(define-evex-disasm-test
    :evex-vpminsd-masked-z-zmm-disp8
    sb-vm::%test-vpminsd-masked-z-zmm-disp8
  ("VPMINSD" "ZMM0" "ZMM1" "[RSP+64]" "{K4}{z}")
  :unexpected ("[RSP+1]"))

;; Zeroing signed qword max
(define-evex-disasm-test
    :evex-vpmaxsq-masked-z-zmm-disp8
    sb-vm::%test-vpmaxsq-masked-z-zmm-disp8
  ("VPMAXSQ" "ZMM0" "ZMM1" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmadd132ps-masked-zmm-disp8
    sb-vm::%test-vfmadd132ps-masked-zmm-disp8
  ("VFMADD132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmadd231pd-masked-zmm-disp8
    sb-vm::%test-vfmadd231pd-masked-zmm-disp8
  ("VFMADD231PD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmadd132ps-masked-z-zmm-disp8
    sb-vm::%test-vfmadd132ps-masked-z-zmm-disp8
  ("VFMADD132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K4}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmadd213pd-masked-z-zmm-disp8
    sb-vm::%test-vfmadd213pd-masked-z-zmm-disp8
  ("VFMADD213PD" "ZMM0" "ZMM1" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmadd132ps-bcast-xmm-disp8
    sb-vm::%test-vfmadd132ps-bcast-xmm-disp8
  ("VFMADD132PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmadd213pd-bcast-zmm-disp8
    sb-vm::%test-vfmadd213pd-bcast-zmm-disp8
  ("VFMADD213PD-BCAST" "ZMM0" "ZMM1" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmsub132ps-masked-zmm-disp8
    sb-vm::%test-vfmsub132ps-masked-zmm-disp8
  ("VFMSUB132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmsub213pd-masked-z-zmm-disp8
    sb-vm::%test-vfmsub213pd-masked-z-zmm-disp8
  ("VFMSUB213PD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfnmadd231ps-bcast-xmm-disp8
    sb-vm::%test-vfnmadd231ps-bcast-xmm-disp8
  ("VFNMADD231PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmaddsub132ps-masked-zmm-disp8
    sb-vm::%test-vfmaddsub132ps-masked-zmm-disp8
  ("VFMADDSUB132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmaddsub213pd-masked-z-zmm-disp8
    sb-vm::%test-vfmaddsub213pd-masked-z-zmm-disp8
  ("VFMADDSUB213PD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmaddsub231ps-bcast-xmm-disp8
    sb-vm::%test-vfmaddsub231ps-bcast-xmm-disp8
  ("VFMADDSUB231PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmsubadd132ps-masked-zmm-disp8
    sb-vm::%test-vfmsubadd132ps-masked-zmm-disp8
  ("VFMSUBADD132PS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmsubadd213pd-masked-z-zmm-disp8
    sb-vm::%test-vfmsubadd213pd-masked-z-zmm-disp8
  ("VFMSUBADD213PD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmsubadd231ps-bcast-xmm-disp8
    sb-vm::%test-vfmsubadd231ps-bcast-xmm-disp8
  ("VFMSUBADD231PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmadd132ss-masked-disp8
    sb-vm::%test-vfmadd132ss-masked-disp8
  ("VFMADD132SS" "XMM0" "XMM1" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfmsub213sd-masked-z-disp8
    sb-vm::%test-vfmsub213sd-masked-z-disp8
  ("VFMSUB213SD" "XMM0" "XMM1" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpaddb-masked-zmm-disp8
    sb-vm::%test-vpaddb-masked-zmm-disp8
  ("VPADDB" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpsubw-masked-z-zmm-disp8
    sb-vm::%test-vpsubw-masked-z-zmm-disp8
  ("VPSUBW" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpsllvd-masked-zmm-disp8
    sb-vm::%test-vpsllvd-masked-zmm-disp8
  ("VPSLLVD" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpsrlvq-masked-z-zmm-disp8
    sb-vm::%test-vpsrlvq-masked-z-zmm-disp8
  ("VPSRLVQ" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpsravd-masked-zmm-disp8
    sb-vm::%test-vpsravd-masked-zmm-disp8
  ("VPSRAVD" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrangeps-masked-zmm-disp8
    sb-vm::%test-vrangeps-masked-zmm-disp8
  ("VRANGEPS" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrangepd-masked-z-zmm-disp8
    sb-vm::%test-vrangepd-masked-z-zmm-disp8
  ("VRANGEPD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrangeps-bcast-xmm-disp8
    sb-vm::%test-vrangeps-bcast-xmm-disp8
  ("VRANGEPS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpdpbusd-masked-zmm-disp8
    sb-vm::%test-vpdpbusd-masked-zmm-disp8
  ("VPDPBUSD" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpdpwssd-masked-z-zmm-disp8
    sb-vm::%test-vpdpwssd-masked-z-zmm-disp8
  ("VPDPWSSD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpdpbusd-bcast-xmm-disp8
    sb-vm::%test-vpdpbusd-bcast-xmm-disp8
  ("VPDPBUSD-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vcvtne2ps2bf16-masked-zmm-disp8
    sb-vm::%test-vcvtne2ps2bf16-masked-zmm-disp8
  ("VCVTNE2PS2BF16" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vdpbf16ps-masked-z-zmm-disp8
    sb-vm::%test-vdpbf16ps-masked-z-zmm-disp8
  ("VDPBF16PS" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vcvtneps2bf16-masked-zmm-disp8
    sb-vm::%test-vcvtneps2bf16-masked-zmm-disp8
  ("VCVTNEPS2BF16" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vcvtneps2bf16-masked-z-zmm-disp8
    sb-vm::%test-vcvtneps2bf16-masked-z-zmm-disp8
  ("VCVTNEPS2BF16" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpermt2d-masked-zmm-disp8
    sb-vm::%test-vpermt2d-masked-zmm-disp8
  ("VPERMT2D" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpermt2q-masked-z-zmm-disp8
    sb-vm::%test-vpermt2q-masked-z-zmm-disp8
  ("VPERMT2Q" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpermb-masked-zmm-disp8
    sb-vm::%test-vpermb-masked-zmm-disp8
  ("VPERMB" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpermw-masked-z-zmm-disp8
    sb-vm::%test-vpermw-masked-z-zmm-disp8
  ("VPERMW" "ZMM0" "ZMM1" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpmadd52luq-masked-zmm-disp8
    sb-vm::%test-vpmadd52luq-masked-zmm-disp8
  ("VPMADD52LUQ" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpshldvw-masked-z-zmm-disp8
    sb-vm::%test-vpshldvw-masked-z-zmm-disp8
  ("VPSHLDVW" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpopcntd-masked-zmm-disp8
    sb-vm::%test-vpopcntd-masked-zmm-disp8
  ("VPOPCNTD" "ZMM0" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vpopcntq-masked-z-zmm-disp8
    sb-vm::%test-vpopcntq-masked-z-zmm-disp8
  ("VPOPCNTQ" "ZMM0" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrcp14ps-masked-zmm-disp8
    sb-vm::%test-vrcp14ps-masked-zmm-disp8
  ("VRCP14PS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrcp14pd-masked-z-zmm-disp8
    sb-vm::%test-vrcp14pd-masked-z-zmm-disp8
  ("VRCP14PD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrsqrt14ps-masked-zmm-disp8
    sb-vm::%test-vrsqrt14ps-masked-zmm-disp8
  ("VRSQRT14PS" "ZMM0" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrcp14ps-bcast-xmm-disp8
    sb-vm::%test-vrcp14ps-bcast-xmm-disp8
  ("VRCP14PS-BCAST" "XMM0" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrsqrt14pd-bcast-zmm-disp8
    sb-vm::%test-vrsqrt14pd-bcast-zmm-disp8
  ("VRSQRT14PD-BCAST" "ZMM0" "[RSP+8]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Reduce
(define-evex-disasm-test
    :evex-vreduceps-masked-zmm-disp8
    sb-vm::%test-vreduceps-masked-zmm-disp8
  ("VREDUCEPS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vreducepd-masked-z-zmm-disp8
    sb-vm::%test-vreducepd-masked-z-zmm-disp8
  ("VREDUCEPD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vreduceps-bcast-xmm-disp8
    sb-vm::%test-vreduceps-bcast-xmm-disp8
  ("VREDUCEPS-BCAST" "XMM0" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; Scale
(define-evex-disasm-test
    :evex-vscalefps-masked-zmm-disp8
    sb-vm::%test-vscalefps-masked-zmm-disp8
  ("VSCALEFPS" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vscalefpd-masked-z-zmm-disp8
    sb-vm::%test-vscalefpd-masked-z-zmm-disp8
  ("VSCALEFPD" "ZMM0" "ZMM1" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vscalefps-bcast-ymm-disp8
    sb-vm::%test-vscalefps-bcast-ymm-disp8
  ("VSCALEFPS-BCAST" "YMM0" "YMM1" "[RSP+4]" "{1to8}")
  :unexpected ("[RSP+1]"))

;; Fixup
(define-evex-disasm-test
    :evex-vfixupimmps-masked-zmm-disp8
    sb-vm::%test-vfixupimmps-masked-zmm-disp8
  ("VFIXUPIMMPS" "ZMM0" "ZMM1" "[RSP+64]" "{K6}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfixupimmpd-masked-z-zmm-disp8
    sb-vm::%test-vfixupimmpd-masked-z-zmm-disp8
  ("VFIXUPIMMPD" "ZMM0" "ZMM1" "[RSP+64]" "{K7}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vfixupimmps-bcast-xmm-disp8
    sb-vm::%test-vfixupimmps-bcast-xmm-disp8
  ("VFIXUPIMMPS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; Round
(define-evex-disasm-test
    :evex-vrndscaleps-masked-zmm-disp8
    sb-vm::%test-vrndscaleps-masked-zmm-disp8
  ("VRNDSCALEPS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vrndscalepd-masked-z-zmm-disp8
    sb-vm::%test-vrndscalepd-masked-z-zmm-disp8
  ("VRNDSCALEPD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; masked scalar reciprocal
(define-evex-disasm-test
    :evex-vrcp14ss-masked-disp8
    sb-vm::%test-vrcp14ss-masked-disp8
  ("VRCP14SS" "XMM0" "XMM1" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

;; zeroing scalar sqrt
(define-evex-disasm-test
    :evex-vrsqrt14sd-masked-z-disp8
    sb-vm::%test-vrsqrt14sd-masked-z-disp8
  ("VRSQRT14SD" "XMM0" "XMM1" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vgetexpss-masked-disp8
    sb-vm::%test-vgetexpss-masked-disp8
  ("VGETEXPSS" "XMM0" "XMM1" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

(define-evex-disasm-test
    :evex-vscalefsd-masked-z-disp8
    sb-vm::%test-vscalefsd-masked-z-disp8
  ("VSCALEFSD" "XMM0" "XMM1" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked scalar getmant
(define-evex-disasm-test
    :evex-vgetmantss-masked-disp8
    sb-vm::%test-vgetmantss-masked-disp8
  ("VGETMANTSS" "XMM0" "XMM1" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing scalar range
(define-evex-disasm-test
    :evex-vrangesd-masked-z-disp8
    sb-vm::%test-vrangesd-masked-z-disp8
  ("VRANGESD" "XMM0" "XMM1" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked scalar fixup
(define-evex-disasm-test
    :evex-vfixupimmss-masked-disp8
    sb-vm::%test-vfixupimmss-masked-disp8
  ("VFIXUPIMMSS" "XMM0" "XMM1" "[RSP+4]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Masked 2-operand round
(define-evex-disasm-test
    :evex-vrndscaless-masked-disp8
    sb-vm::%test-vrndscaless-masked-disp8
  ("VRNDSCALESS" "XMM0" "[RSP+4]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing 2-operand round
(define-evex-disasm-test
    :evex-vrndscalesd-masked-z-disp8
    sb-vm::%test-vrndscalesd-masked-z-disp8
  ("VRNDSCALESD" "XMM0" "[RSP+8]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked get exponent
(define-evex-disasm-test
    :evex-vgetexpps-masked-zmm-disp8
    sb-vm::%test-vgetexpps-masked-zmm-disp8
  ("VGETEXPPS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing get exponent double
(define-evex-disasm-test
    :evex-vgetexppd-masked-z-zmm-disp8
    sb-vm::%test-vgetexppd-masked-z-zmm-disp8
  ("VGETEXPPD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked get mantissa
(define-evex-disasm-test
    :evex-vgetmantps-masked-zmm-disp8
    sb-vm::%test-vgetmantps-masked-zmm-disp8
  ("VGETMANTPS" "ZMM0" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Zeroing get mantissa double
(define-evex-disasm-test
    :evex-vgetmantpd-masked-z-zmm-disp8
    sb-vm::%test-vgetmantpd-masked-z-zmm-disp8
  ("VGETMANTPD" "ZMM0" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Masked absolute value qword
(define-evex-disasm-test
    :evex-vpabsq-masked-zmm-disp8
    sb-vm::%test-vpabsq-masked-zmm-disp8
  ("VPABSQ" "ZMM0" "[RSP+64]" "{K6}")
  :unexpected ("[RSP+1]"))

;; Masked GFNI multiply
(define-evex-disasm-test
    :evex-vgf2p8mulb-masked-zmm-disp8
    sb-vm::%test-vgf2p8mulb-masked-zmm-disp8
  ("VGF2P8MULB" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing GFNI affine QB
(define-evex-disasm-test
    :evex-vgf2p8affineqb-masked-z-zmm-disp8
    sb-vm::%test-vgf2p8affineqb-masked-z-zmm-disp8
  ("VGF2P8AFFINEQB" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked GFNI affine INVQB
(define-evex-disasm-test
    :evex-vgf2p8affineinvqb-masked-zmm-disp8
    sb-vm::%test-vgf2p8affineinvqb-masked-zmm-disp8
  ("VGF2P8AFFINEINVQB" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Broadcast BF16 conversion
(define-evex-disasm-test
    :evex-vcvtne2ps2bf16-bcast-zmm-disp8
    sb-vm::%test-vcvtne2ps2bf16-bcast-zmm-disp8
  ("VCVTNE2PS2BF16-BCAST" "ZMM0" "ZMM1" "[RSP+4]" "{1to16}")
  :unexpected ("[RSP+1]"))

;; Broadcast BF16 dot product
(define-evex-disasm-test
    :evex-vdpbf16ps-bcast-xmm-disp8
    sb-vm::%test-vdpbf16ps-bcast-xmm-disp8
  ("VDPBF16PS-BCAST" "XMM0" "XMM1" "[RSP+4]" "{1to4}")
  :unexpected ("[RSP+1]"))

;; Masked conflict detection dword
(define-evex-disasm-test
    :evex-vpconflictd-masked-zmm-disp8
    sb-vm::%test-vpconflictd-masked-zmm-disp8
  ("VPCONFLICTD" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing conflict detection qword
(define-evex-disasm-test
    :evex-vpconflictq-masked-z-zmm-disp8
    sb-vm::%test-vpconflictq-masked-z-zmm-disp8
  ("VPCONFLICTQ" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked leading zero count dword
(define-evex-disasm-test
    :evex-vplzcntd-masked-xmm-disp8
    sb-vm::%test-vplzcntd-masked-xmm-disp8
  ("VPLZCNTD" "XMM0" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Zeroing leading zero count qword
(define-evex-disasm-test
    :evex-vplzcntq-masked-z-zmm-disp8
    sb-vm::%test-vplzcntq-masked-z-zmm-disp8
  ("VPLZCNTQ" "ZMM0" "[RSP+64]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Masked VAESENC
(define-evex-disasm-test
    :evex-vaesenc-masked-zmm-disp8
    sb-vm::%test-vaesenc-masked-zmm-disp8
  ("VAESENC" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing VAESENCLAST
(define-evex-disasm-test
    :evex-vaesenclast-masked-z-zmm-disp8
    sb-vm::%test-vaesenclast-masked-z-zmm-disp8
  ("VAESENCLAST" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked VAESDEC
(define-evex-disasm-test
    :evex-vaesdec-masked-xmm-disp8
    sb-vm::%test-vaesdec-masked-xmm-disp8
  ("VAESDEC" "XMM0" "XMM1" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Zeroing VAESDECLAST
(define-evex-disasm-test
    :evex-vaesdeclast-masked-z-ymm-disp8
    sb-vm::%test-vaesdeclast-masked-z-ymm-disp8
  ("VAESDECLAST" "YMM1" "YMM2" "[RSP+32]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Unmasked EVEX VAESENC ZMM
(define-evex-disasm-test
    :evex-vaesenc-unmasked-zmm
    sb-vm::%test-vaesenc-evex-zmm
  ("VAESENC" "ZMM0" "ZMM1" "ZMM2"))

;; Unmasked EVEX VAESENCLAST ZMM
(define-evex-disasm-test
    :evex-vaesenclast-unmasked-zmm
    sb-vm::%test-vaesenclast-evex-zmm
  ("VAESENCLAST" "ZMM0" "ZMM1" "ZMM2"))

;; Unmasked EVEX VAESDEC ZMM
(define-evex-disasm-test
    :evex-vaesdec-unmasked-zmm
    sb-vm::%test-vaesdec-evex-zmm
  ("VAESDEC" "ZMM0" "ZMM1" "ZMM2"))

;; Unmasked EVEX VAESDECLAST ZMM
(define-evex-disasm-test
    :evex-vaesdeclast-unmasked-zmm
    sb-vm::%test-vaesdeclast-evex-zmm
  ("VAESDECLAST" "ZMM0" "ZMM1" "ZMM2"))

;; Masked vcvtph2ps ZMM
(define-evex-disasm-test
    :evex-vcvtph2ps-masked-zmm-disp8
    sb-vm::%test-vcvtph2ps-masked-zmm-disp8
  ("VCVTPH2PS" "ZMM0" "[RSP+32]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing vcvtph2ps ZMM
(define-evex-disasm-test
    :evex-vcvtph2ps-masked-z-zmm-disp8
    sb-vm::%test-vcvtph2ps-masked-z-zmm-disp8
  ("VCVTPH2PS" "ZMM0" "[RSP+32]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked vcvtph2ps XMM
(define-evex-disasm-test
    :evex-vcvtph2ps-masked-xmm-disp8
    sb-vm::%test-vcvtph2ps-masked-xmm-disp8
  ("VCVTPH2PS" "XMM0" "[RSP+8]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Masked vcvtps2ph ZMM
(define-evex-disasm-test
    :evex-vcvtps2ph-masked-zmm-disp8
    sb-vm::%test-vcvtps2ph-masked-zmm-disp8
  ("VCVTPS2PH" "[RSP+32]" "ZMM0" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing vcvtps2ph ZMM
(define-evex-disasm-test
    :evex-vcvtps2ph-masked-z-zmm-disp8
    sb-vm::%test-vcvtps2ph-masked-z-zmm-disp8
  ("VCVTPS2PH" "[RSP+32]" "ZMM0" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked vcvtps2ph XMM
(define-evex-disasm-test
    :evex-vcvtps2ph-masked-xmm-disp8
    sb-vm::%test-vcvtps2ph-masked-xmm-disp8
  ("VCVTPS2PH" "[RSP+8]" "XMM0" "{K4}")
  :unexpected ("[RSP+1]"))

;; Masked variable shift double word left
(define-evex-disasm-test
    :evex-vpshldw-masked-zmm-disp8
    sb-vm::%test-vpshldw-masked-zmm-disp8
  ("VPSHLDW" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing shift double dword left
(define-evex-disasm-test
    :evex-vpshldd-masked-z-zmm-disp8
    sb-vm::%test-vpshldd-masked-z-zmm-disp8
  ("VPSHLDD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked shift double qword right
(define-evex-disasm-test
    :evex-vpshrdq-masked-zmm-disp8
    sb-vm::%test-vpshrdq-masked-zmm-disp8
  ("VPSHRDQ" "ZMM0" "ZMM1" "[RSP+64]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Masked multishift QB
(define-evex-disasm-test
    :evex-vpmultishiftqb-masked-zmm-disp8
    sb-vm::%test-vpmultishiftqb-masked-zmm-disp8
  ("VPMULTISHIFTQB" "ZMM0" "ZMM1" "[RSP+64]" "{K5}")
  :unexpected ("[RSP+1]"))

;; Masked compress single
(define-evex-disasm-test
    :evex-vcompressps-masked-zmm-disp8
    sb-vm::%test-vcompressps-masked-zmm-disp8
  ("VCOMPRESSPS" "[RSP+64]" "ZMM0" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing compress double
(define-evex-disasm-test
    :evex-vcompresspd-masked-z-zmm-disp8
    sb-vm::%test-vcompresspd-masked-z-zmm-disp8
  ("VCOMPRESSPD" "[RSP+64]" "ZMM0" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked compress dword
(define-evex-disasm-test
    :evex-vpcompressd-masked-xmm-disp8
    sb-vm::%test-vpcompressd-masked-xmm-disp8
  ("VPCOMPRESSD" "[RSP+16]" "XMM0" "{K4}")
  :unexpected ("[RSP+1]"))

;; Masked expand single
(define-evex-disasm-test
    :evex-vexpandps-masked-zmm-disp8
    sb-vm::%test-vexpandps-masked-zmm-disp8
  ("VEXPANDPS" "ZMM0" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing expand double
(define-evex-disasm-test
    :evex-vexpandpd-masked-z-zmm-disp8
    sb-vm::%test-vexpandpd-masked-z-zmm-disp8
  ("VEXPANDPD" "ZMM0" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked expand dword
(define-evex-disasm-test
    :evex-vpexpandd-masked-xmm-disp8
    sb-vm::%test-vpexpandd-masked-xmm-disp8
  ("VPEXPANDD" "XMM0" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Masked down-convert store: vpmovqd ZMM
(define-evex-disasm-test
    :evex-vpmovqd-masked-zmm-disp8
    sb-vm::%test-vpmovqd-masked-zmm-disp8
  ("VPMOVQD" "[RSP+32]" "ZMM0" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing down-convert store: vpmovqd ZMM
(define-evex-disasm-test
    :evex-vpmovqd-masked-z-zmm-disp8
    sb-vm::%test-vpmovqd-masked-z-zmm-disp8
  ("VPMOVQD" "[RSP+32]" "ZMM0" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked saturating truncation: vpmovsqb XMM
(define-evex-disasm-test
    :evex-vpmovsqb-masked-xmm-disp8
    sb-vm::%test-vpmovsqb-masked-xmm-disp8
  ("VPMOVSQB" "[RSP+2]" "XMM0" "{K4}")
  :unexpected ("[RSP+1]"))

;; Zeroing saturating truncation: vpmovsqb YMM
(define-evex-disasm-test
    :evex-vpmovsqb-masked-z-ymm-disp8
    sb-vm::%test-vpmovsqb-masked-z-ymm-disp8
  ("VPMOVSQB" "[RSP+4]" "YMM1" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Masked vdbpsadbw
(define-evex-disasm-test
    :evex-vdbpsadbw-masked-zmm-disp8
    sb-vm::%test-vdbpsadbw-masked-zmm-disp8
  ("VDBPSADBW" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing vdbpsadbw
(define-evex-disasm-test
    :evex-vdbpsadbw-masked-z-zmm-disp8
    sb-vm::%test-vdbpsadbw-masked-z-zmm-disp8
  ("VDBPSADBW" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked ternary logic dword ZMM
(define-evex-disasm-test
    :evex-vpternlogd-masked-zmm-disp8
    sb-vm::%test-vpternlogd-masked-zmm-disp8
  ("VPTERNLOGD" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing ternary logic dword ZMM
(define-evex-disasm-test
    :evex-vpternlogd-masked-z-zmm-disp8
    sb-vm::%test-vpternlogd-masked-z-zmm-disp8
  ("VPTERNLOGD" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked ternary logic qword XMM
(define-evex-disasm-test
    :evex-vpternlogq-masked-xmm-disp8
    sb-vm::%test-vpternlogq-masked-xmm-disp8
  ("VPTERNLOGQ" "XMM0" "XMM1" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Zeroing ternary logic qword YMM
(define-evex-disasm-test
    :evex-vpternlogq-masked-z-ymm-disp8
    sb-vm::%test-vpternlogq-masked-z-ymm-disp8
  ("VPTERNLOGQ" "YMM1" "YMM2" "[RSP+32]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Masked cross-lane shuffle: vshuff32x4 ZMM
(define-evex-disasm-test
    :evex-vshuff32x4-masked-zmm-disp8
    sb-vm::%test-vshuff32x4-masked-zmm-disp8
  ("VSHUFF32X4" "ZMM0" "ZMM1" "[RSP+64]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing cross-lane shuffle: vshuff32x4 ZMM
(define-evex-disasm-test
    :evex-vshuff32x4-masked-z-zmm-disp8
    sb-vm::%test-vshuff32x4-masked-z-zmm-disp8
  ("VSHUFF32X4" "ZMM0" "ZMM1" "[RSP+64]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked cross-lane shuffle: vshuff64x2 XMM
(define-evex-disasm-test
    :evex-vshuff64x2-masked-xmm-disp8
    sb-vm::%test-vshuff64x2-masked-xmm-disp8
  ("VSHUFF64X2" "XMM0" "XMM1" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Zeroing cross-lane shuffle: vshuff64x2 YMM
(define-evex-disasm-test
    :evex-vshuff64x2-masked-z-ymm-disp8
    sb-vm::%test-vshuff64x2-masked-z-ymm-disp8
  ("VSHUFF64X2" "YMM1" "YMM2" "[RSP+32]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Masked cross-lane shuffle: vshufi32x4 ZMM
(define-evex-disasm-test
    :evex-vshufi32x4-masked-zmm-disp8
    sb-vm::%test-vshufi32x4-masked-zmm-disp8
  ("VSHUFI32X4" "ZMM0" "ZMM1" "[RSP+64]" "{K6}")
  :unexpected ("[RSP+1]"))

;; Zeroing cross-lane shuffle: vshufi32x4 ZMM
(define-evex-disasm-test
    :evex-vshufi32x4-masked-z-zmm-disp8
    sb-vm::%test-vshufi32x4-masked-z-zmm-disp8
  ("VSHUFI32X4" "ZMM0" "ZMM1" "[RSP+64]" "{K7}{z}")
  :unexpected ("[RSP+1]"))

;; Masked cross-lane shuffle: vshufi64x2 XMM
(define-evex-disasm-test
    :evex-vshufi64x2-masked-xmm-disp8
    sb-vm::%test-vshufi64x2-masked-xmm-disp8
  ("VSHUFI64X2" "XMM0" "XMM1" "[RSP+16]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing cross-lane shuffle: vshufi64x2 YMM
(define-evex-disasm-test
    :evex-vshufi64x2-masked-z-ymm-disp8
    sb-vm::%test-vshufi64x2-masked-z-ymm-disp8
  ("VSHUFI64X2" "YMM1" "YMM2" "[RSP+32]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked insert 128-bit lane single
(define-evex-disasm-test
    :evex-vinsertf32x4-masked-zmm-disp8
    sb-vm::%test-vinsertf32x4-masked-zmm-disp8
  ("VINSERTF32X4" "ZMM0" "ZMM1" "[RSP+16]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing insert 128-bit lane single
(define-evex-disasm-test
    :evex-vinsertf32x4-masked-z-zmm-disp8
    sb-vm::%test-vinsertf32x4-masked-z-zmm-disp8
  ("VINSERTF32X4" "ZMM0" "ZMM1" "[RSP+16]" "{K3}{z}")
  :unexpected ("[RSP+1]"))

;; Masked insert 128-bit lane double
(define-evex-disasm-test
    :evex-vinsertf64x2-masked-xmm-disp8
    sb-vm::%test-vinsertf64x2-masked-xmm-disp8
  ("VINSERTF64X2" "XMM0" "XMM1" "[RSP+16]" "{K4}")
  :unexpected ("[RSP+1]"))

;; Zeroing insert 128-bit lane double YMM
(define-evex-disasm-test
    :evex-vinsertf64x2-masked-z-ymm-disp8
    sb-vm::%test-vinsertf64x2-masked-z-ymm-disp8
  ("VINSERTF64X2" "YMM1" "YMM2" "[RSP+16]" "{K5}{z}")
  :unexpected ("[RSP+1]"))

;; Masked integer insert 128-bit lane
(define-evex-disasm-test
    :evex-vinserti32x4-masked-zmm-disp8
    sb-vm::%test-vinserti32x4-masked-zmm-disp8
  ("VINSERTI32X4" "ZMM0" "ZMM1" "[RSP+16]" "{K6}")
  :unexpected ("[RSP+1]"))

;; Zeroing integer insert 128-bit lane
(define-evex-disasm-test
    :evex-vinserti32x4-masked-z-zmm-disp8
    sb-vm::%test-vinserti32x4-masked-z-zmm-disp8
  ("VINSERTI32X4" "ZMM0" "ZMM1" "[RSP+16]" "{K7}{z}")
  :unexpected ("[RSP+1]"))

;; Masked integer insert 256-bit lane
(define-evex-disasm-test
    :evex-vinserti32x8-masked-ymm-disp8
    sb-vm::%test-vinserti32x8-masked-ymm-disp8
  ("VINSERTI32X8" "YMM1" "YMM2" "[RSP+32]" "{K2}")
  :unexpected ("[RSP+1]"))

;; Zeroing integer insert 256-bit lane
(define-evex-disasm-test
    :evex-vinserti32x8-masked-z-ymm-disp8
    sb-vm::%test-vinserti32x8-masked-z-ymm-disp8
  ("VINSERTI32X8" "YMM1" "YMM2" "[RSP+32]" "{K3}{z}")
  :unexpected ("[RSP+1]"))
