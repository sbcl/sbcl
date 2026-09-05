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
  (def %test-evex-disp-large))

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

(defun %float-eq (a b &optional (eps 1.0e-5))
  (< (abs (- a b)) eps))

(with-test (:name (simd-pack-512 :arithmetic-single))
  (let ((fn-add (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 single-float) a b))
                                (sb-vm::simd-pack-512-single+ a b))))
        (fn-sub (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 single-float) a b))
                                (sb-vm::simd-pack-512-single- a b))))
        (fn-mul (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 single-float) a b))
                                (sb-vm::simd-pack-512-single* a b))))
        (fn-div (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 single-float) a b))
                                (sb-vm::simd-pack-512-single/ a b))))
        (fn-min (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 single-float) a b))
                                (sb-vm::simd-pack-512-single-min a b))))
        (fn-max (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 single-float) a b))
                                (sb-vm::simd-pack-512-single-max a b))))
        (fn-sqrt (compile nil `(lambda (a)
                                 (declare (type (sb-ext:simd-pack-512 single-float) a))
                                 (sb-vm::simd-pack-512-single-sqrt a)))))
    (let ((a (sb-ext:%make-simd-pack-512-single
              1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0
              9f0 10f0 11f0 12f0 13f0 14f0 15f0 16f0))
          (b (sb-ext:%make-simd-pack-512-single
              2f0 2f0 2f0 2f0 2f0 2f0 2f0 2f0
              2f0 2f0 2f0 2f0 2f0 2f0 2f0 2f0)))
      (let ((res (funcall fn-add a b)))
        (loop for i from 0 to 15
              do (assert (= (sb-vm::%simd-pack-ref-single res i) (+ (+ 1f0 i) 2f0)))))
      (let ((res (funcall fn-sub a b)))
        (loop for i from 0 to 15
              do (assert (= (sb-vm::%simd-pack-ref-single res i) (- (+ 1f0 i) 2f0)))))
      (let ((res (funcall fn-mul a b)))
        (loop for i from 0 to 15
              do (assert (= (sb-vm::%simd-pack-ref-single res i) (* (+ 1f0 i) 2f0)))))
      (let ((res (funcall fn-div a b)))
        (loop for i from 0 to 15
              do (assert (= (sb-vm::%simd-pack-ref-single res i) (/ (+ 1f0 i) 2f0)))))
      (let ((res (funcall fn-min a b)))
        (loop for i from 0 to 15
              do (assert (= (sb-vm::%simd-pack-ref-single res i) (min (+ 1f0 i) 2f0)))))
      (let ((res (funcall fn-max a b)))
        (loop for i from 0 to 15
              do (assert (= (sb-vm::%simd-pack-ref-single res i) (max (+ 1f0 i) 2f0)))))
      (let ((sq-pack (sb-ext:%make-simd-pack-512-single
                      4f0 9f0 16f0 25f0 36f0 49f0 64f0 81f0
                      100f0 121f0 144f0 169f0 196f0 225f0 256f0 289f0)))
        (let ((res (funcall fn-sqrt sq-pack)))
          (loop for i from 0 to 15
                do (assert (%float-eq (sb-vm::%simd-pack-ref-single res i) (coerce (+ 2 i) 'single-float)))))))))

(with-test (:name (simd-pack-512 :arithmetic-double))
  (let ((fn-add (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 double-float) a b))
                                (sb-vm::simd-pack-512-double+ a b))))
        (fn-sub (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 double-float) a b))
                                (sb-vm::simd-pack-512-double- a b))))
        (fn-mul (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 double-float) a b))
                                (sb-vm::simd-pack-512-double* a b))))
        (fn-div (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 double-float) a b))
                                (sb-vm::simd-pack-512-double/ a b))))
        (fn-min (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 double-float) a b))
                                (sb-vm::simd-pack-512-double-min a b))))
        (fn-max (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 double-float) a b))
                                (sb-vm::simd-pack-512-double-max a b))))
        (fn-sqrt (compile nil `(lambda (a)
                                 (declare (type (sb-ext:simd-pack-512 double-float) a))
                                 (sb-vm::simd-pack-512-double-sqrt a)))))
    (let ((a (sb-ext:%make-simd-pack-512-double 1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0))
          (b (sb-ext:%make-simd-pack-512-double 2d0 2d0 2d0 2d0 2d0 2d0 2d0 2d0)))
      (let ((res (funcall fn-add a b)))
        (loop for i from 0 to 7
              do (assert (= (sb-vm::%simd-pack-ref-double res i) (+ (+ 1d0 i) 2d0)))))
      (let ((res (funcall fn-sub a b)))
        (loop for i from 0 to 7
              do (assert (= (sb-vm::%simd-pack-ref-double res i) (- (+ 1d0 i) 2d0)))))
      (let ((res (funcall fn-mul a b)))
        (loop for i from 0 to 7
              do (assert (= (sb-vm::%simd-pack-ref-double res i) (* (+ 1d0 i) 2d0)))))
      (let ((res (funcall fn-div a b)))
        (loop for i from 0 to 7
              do (assert (= (sb-vm::%simd-pack-ref-double res i) (/ (+ 1d0 i) 2d0)))))
      (let ((res (funcall fn-min a b)))
        (loop for i from 0 to 7
              do (assert (= (sb-vm::%simd-pack-ref-double res i) (min (+ 1d0 i) 2d0)))))
      (let ((res (funcall fn-max a b)))
        (loop for i from 0 to 7
              do (assert (= (sb-vm::%simd-pack-ref-double res i) (max (+ 1d0 i) 2d0)))))
      (let ((sq-pack (sb-ext:%make-simd-pack-512-double 4d0 9d0 16d0 25d0 36d0 49d0 64d0 81d0)))
        (let ((res (funcall fn-sqrt sq-pack)))
          (loop for i from 0 to 7
                do (assert (%float-eq (sb-vm::%simd-pack-ref-double res i) (coerce (+ 2 i) 'double-float)))))))))

(with-test (:name (simd-pack-512 :arithmetic-integer))
  (let ((fn-ub32+ (compile nil `(lambda (a b)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 32)) a b))
                                  (sb-vm::simd-pack-512-ub32+ a b))))
        (fn-ub32- (compile nil `(lambda (a b)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 32)) a b))
                                  (sb-vm::simd-pack-512-ub32- a b))))
        (fn-ub32* (compile nil `(lambda (a b)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 32)) a b))
                                  (sb-vm::simd-pack-512-ub32* a b))))
        (fn-ub64+ (compile nil `(lambda (a b)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b))
                                  (sb-vm::simd-pack-512-ub64+ a b))))
        (fn-ub64- (compile nil `(lambda (a b)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b))
                                  (sb-vm::simd-pack-512-ub64- a b))))
        (fn-ub64* (compile nil `(lambda (a b)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b))
                                  (sb-vm::simd-pack-512-ub64* a b)))))
    (let ((a32 (sb-ext:%make-simd-pack-512-ub32 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160))
          (b32 (sb-ext:%make-simd-pack-512-ub32 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
      (let ((res+ (funcall fn-ub32+ a32 b32))
            (res- (funcall fn-ub32- a32 b32))
            (res* (funcall fn-ub32* a32 b32)))
        (assert (= (sb-kernel:%simd-pack-512-0 res+) (logior 11 (ash 22 32))))
        (assert (= (sb-kernel:%simd-pack-512-0 res-) (logior 9 (ash 18 32))))
        (assert (= (sb-kernel:%simd-pack-512-0 res*) (logior 10 (ash 40 32))))))
    (let ((a64 (sb-ext:%make-simd-pack-512-ub64 100 200 300 400 500 600 700 800))
          (b64 (sb-ext:%make-simd-pack-512-ub64 10 20 30 40 50 60 70 80)))
      (let ((res+ (funcall fn-ub64+ a64 b64))
            (res- (funcall fn-ub64- a64 b64))
            (res* (funcall fn-ub64* a64 b64)))
        (assert (= (sb-kernel:%simd-pack-512-0 res+) 110))
        (assert (= (sb-kernel:%simd-pack-512-7 res+) 880))
        (assert (= (sb-kernel:%simd-pack-512-0 res-) 90))
        (assert (= (sb-kernel:%simd-pack-512-7 res-) 720))
        (assert (= (sb-kernel:%simd-pack-512-0 res*) 1000))
        (assert (= (sb-kernel:%simd-pack-512-7 res*) 64000))))))

(with-test (:name (simd-pack-512 :bitwise-logical))
  (let ((fn-and (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b))
                                (sb-vm::simd-pack-512-and a b))))
        (fn-or  (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b))
                                (sb-vm::simd-pack-512-or a b))))
        (fn-xor (compile nil `(lambda (a b)
                                (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b))
                                (sb-vm::simd-pack-512-xor a b))))
        (fn-andc1 (compile nil `(lambda (a b)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b))
                                  (sb-vm::simd-pack-512-andc1 a b)))))
    (let ((a (sb-ext:%make-simd-pack-512-ub64 #xF0 #xF0 #xF0 #xF0 #xF0 #xF0 #xF0 #xF0))
          (b (sb-ext:%make-simd-pack-512-ub64 #xCC #xCC #xCC #xCC #xCC #xCC #xCC #xCC)))
      (assert (= (sb-kernel:%simd-pack-512-0 (funcall fn-and a b)) #xC0))
      (assert (= (sb-kernel:%simd-pack-512-0 (funcall fn-or a b)) #xFC))
      (assert (= (sb-kernel:%simd-pack-512-0 (funcall fn-xor a b)) #x3C))
      (assert (= (sb-kernel:%simd-pack-512-0 (funcall fn-andc1 a b)) #x0C)))))

(with-test (:name (simd-pack-512 :broadcast))
  (let ((fn-b-single (compile nil `(lambda (x)
                                     (declare (type single-float x))
                                     (sb-vm::simd-pack-512-broadcast-single x))))
        (fn-b-double (compile nil `(lambda (x)
                                     (declare (type double-float x))
                                     (sb-vm::simd-pack-512-broadcast-double x))))
        (fn-b-ub32   (compile nil `(lambda (x)
                                     (declare (type (unsigned-byte 32) x))
                                     (sb-vm::simd-pack-512-broadcast-ub32 x))))
        (fn-b-ub64   (compile nil `(lambda (x)
                                     (declare (type (unsigned-byte 64) x))
                                     (sb-vm::simd-pack-512-broadcast-ub64 x)))))
    (let ((res (funcall fn-b-single 42.0f0)))
      (loop for i from 0 to 15
            do (assert (= (sb-vm::%simd-pack-ref-single res i) 42.0f0))))
    (let ((res (funcall fn-b-double 3.141592653589793d0)))
      (loop for i from 0 to 7
            do (assert (= (sb-vm::%simd-pack-ref-double res i) 3.141592653589793d0))))
    (let ((res (funcall fn-b-ub32 #x12345678)))
      (loop for i from 0 to 7
            do (assert (= (sb-kernel:%simd-pack-512-0 res)
                          (logior #x12345678 (ash #x12345678 32))))))
    (let ((res (funcall fn-b-ub64 #xCAFEBABE12345678)))
      (assert (= (sb-kernel:%simd-pack-512-0 res) #xCAFEBABE12345678))
      (assert (= (sb-kernel:%simd-pack-512-7 res) #xCAFEBABE12345678)))))

(with-test (:name (simd-pack-512 :comparisons))
  (let ((fn-s= (compile nil `(lambda (a b)
                               (declare (type (sb-ext:simd-pack-512 single-float) a b))
                               (sb-vm::simd-pack-512-single= a b))))
        (fn-s< (compile nil `(lambda (a b)
                               (declare (type (sb-ext:simd-pack-512 single-float) a b))
                               (sb-vm::simd-pack-512-single< a b))))
        (fn-d= (compile nil `(lambda (a b)
                               (declare (type (sb-ext:simd-pack-512 double-float) a b))
                               (sb-vm::simd-pack-512-double= a b))))
        (fn-d< (compile nil `(lambda (a b)
                               (declare (type (sb-ext:simd-pack-512 double-float) a b))
                               (sb-vm::simd-pack-512-double< a b))))
        (fn-u32= (compile nil `(lambda (a b)
                                 (declare (type (sb-ext:simd-pack-512 (unsigned-byte 32)) a b))
                                 (sb-vm::simd-pack-512-ub32= a b))))
        (fn-u64= (compile nil `(lambda (a b)
                                 (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b))
                                 (sb-vm::simd-pack-512-ub64= a b))))
        (fn-s32> (compile nil `(lambda (a b)
                                 (declare (type (sb-ext:simd-pack-512 (signed-byte 32)) a b))
                                 (sb-vm::simd-pack-512-sb32> a b))))
        (fn-s64> (compile nil `(lambda (a b)
                                 (declare (type (sb-ext:simd-pack-512 (signed-byte 64)) a b))
                                 (sb-vm::simd-pack-512-sb64> a b)))))
    (let ((a (sb-ext:%make-simd-pack-512-single 1f0 2f0 1f0 2f0 1f0 2f0 1f0 2f0
                                                1f0 2f0 1f0 2f0 1f0 2f0 1f0 2f0))
          (b (sb-ext:%make-simd-pack-512-single 1f0 0f0 1f0 0f0 1f0 0f0 1f0 0f0
                                                1f0 0f0 1f0 0f0 1f0 0f0 1f0 0f0)))
      (assert (= (sb-kernel:%simd-pack-512-mask-value (funcall fn-s= a b)) #x5555)))
    (let ((a (sb-ext:%make-simd-pack-512-single 0f0 2f0 0f0 2f0 0f0 2f0 0f0 2f0
                                                0f0 2f0 0f0 2f0 0f0 2f0 0f0 2f0))
          (b (sb-ext:%make-simd-pack-512-single 1f0 1f0 1f0 1f0 1f0 1f0 1f0 1f0
                                                1f0 1f0 1f0 1f0 1f0 1f0 1f0 1f0)))
      (assert (= (sb-kernel:%simd-pack-512-mask-value (funcall fn-s< a b)) #x5555)))
    (let ((a (sb-ext:%make-simd-pack-512-double 1d0 2d0 1d0 2d0 1d0 2d0 1d0 2d0))
          (b (sb-ext:%make-simd-pack-512-double 1d0 0d0 1d0 0d0 1d0 0d0 1d0 0d0)))
      (assert (= (sb-kernel:%simd-pack-512-mask-value (funcall fn-d= a b)) #x55)))
    (let ((a (sb-ext:%make-simd-pack-512-double 0d0 2d0 0d0 2d0 0d0 2d0 0d0 2d0))
          (b (sb-ext:%make-simd-pack-512-double 1d0 1d0 1d0 1d0 1d0 1d0 1d0 1d0)))
      (assert (= (sb-kernel:%simd-pack-512-mask-value (funcall fn-d< a b)) #x55)))
    (let ((a (sb-ext:%make-simd-pack-512-ub32 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2))
          (b (sb-ext:%make-simd-pack-512-ub32 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)))
      (assert (= (sb-kernel:%simd-pack-512-mask-value (funcall fn-u32= a b)) #x5555)))
    (let ((a (sb-ext:%make-simd-pack-512-ub64 1 2 1 2 1 2 1 2))
          (b (sb-ext:%make-simd-pack-512-ub64 1 0 1 0 1 0 1 0)))
      (assert (= (sb-kernel:%simd-pack-512-mask-value (funcall fn-u64= a b)) #x55)))
    (let ((a (sb-kernel:%make-simd-pack-512 8 5 5 5 5 5 5 5 5))
          (b (sb-kernel:%make-simd-pack-512 8 (ash 5 32) (ash 5 32) (ash 5 32) (ash 5 32) (ash 5 32) (ash 5 32) (ash 5 32) (ash 5 32))))
      (assert (= (sb-kernel:%simd-pack-512-mask-value (funcall fn-s32> a b)) #x5555)))
    (let ((a (sb-kernel:%make-simd-pack-512 9 5 0 5 0 5 0 5 0))
          (b (sb-kernel:%make-simd-pack-512 9 0 5 0 5 0 5 0 5)))
      (assert (= (sb-kernel:%simd-pack-512-mask-value (funcall fn-s64> a b)) #x55)))))

(with-test (:name (simd-pack-512 :zeroing))
  (let ((fn-z-single (compile nil `(lambda () (sb-vm::%simd-pack-512-zero-single))))
        (fn-z-double (compile nil `(lambda () (sb-vm::%simd-pack-512-zero-double))))
        (fn-z-int    (compile nil `(lambda () (sb-vm::%simd-pack-512-zero-int)))))
    (let ((s (funcall fn-z-single))
          (d (funcall fn-z-double))
          (i (funcall fn-z-int)))
      (loop for idx from 0 to 15
            do (assert (= (sb-vm::%simd-pack-ref-single s idx) 0.0f0)))
      (loop for idx from 0 to 7
            do (assert (= (sb-vm::%simd-pack-ref-double d idx) 0.0d0)))
      (assert (= (sb-kernel:%simd-pack-512-0 i) 0))
      (assert (= (sb-kernel:%simd-pack-512-7 i) 0)))))

(with-test (:name (simd-pack-512 :masked-operations))
  (let ((fn-add-m-single (compile nil `(lambda (a b m)
                                        (declare (type (sb-ext:simd-pack-512 single-float) a b)
                                                 (type sb-ext:simd-pack-512-mask m))
                                        (sb-vm::simd-pack-512-single+-masked a b m))))
        (fn-add-m-double (compile nil `(lambda (a b m)
                                        (declare (type (sb-ext:simd-pack-512 double-float) a b)
                                                 (type sb-ext:simd-pack-512-mask m))
                                        (sb-vm::simd-pack-512-double+-masked a b m))))
        (fn-add-m-ub32   (compile nil `(lambda (a b m)
                                        (declare (type (sb-ext:simd-pack-512 (unsigned-byte 32)) a b)
                                                 (type sb-ext:simd-pack-512-mask m))
                                        (sb-vm::simd-pack-512-ub32+-masked a b m))))
        (fn-add-m-ub64   (compile nil `(lambda (a b m)
                                        (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                                                 (type sb-ext:simd-pack-512-mask m))
                                        (sb-vm::simd-pack-512-ub64+-masked a b m))))
        (fn-sqrt-m-single (compile nil `(lambda (a m)
                                          (declare (type (sb-ext:simd-pack-512 single-float) a)
                                                   (type sb-ext:simd-pack-512-mask m))
                                          (sb-vm::simd-pack-512-single-sqrt-masked a m))))
        (fn-sqrt-m-double (compile nil `(lambda (a m)
                                          (declare (type (sb-ext:simd-pack-512 double-float) a)
                                                   (type sb-ext:simd-pack-512-mask m))
                                          (sb-vm::simd-pack-512-double-sqrt-masked a m))))
        (fn-and-m (compile nil `(lambda (a b m)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                                           (type sb-ext:simd-pack-512-mask m))
                                  (sb-vm::simd-pack-512-and-masked a b m))))
        (fn-or-m  (compile nil `(lambda (a b m)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                                           (type sb-ext:simd-pack-512-mask m))
                                  (sb-vm::simd-pack-512-or-masked a b m))))
        (fn-xor-m (compile nil `(lambda (a b m)
                                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                                           (type sb-ext:simd-pack-512-mask m))
                                  (sb-vm::simd-pack-512-xor-masked a b m)))))
    ;; Single+
    (let ((a (sb-ext:%make-simd-pack-512-single
              10f0 10f0 10f0 10f0 10f0 10f0 10f0 10f0
              10f0 10f0 10f0 10f0 10f0 10f0 10f0 10f0))
          (b (sb-ext:%make-simd-pack-512-single
              5f0 5f0 5f0 5f0 5f0 5f0 5f0 5f0
              5f0 5f0 5f0 5f0 5f0 5f0 5f0 5f0))
          (m (sb-ext:%make-simd-pack-512-mask #x5555)))
      (let ((res (funcall fn-add-m-single a b m)))
        (loop for i from 0 to 15
              do (if (evenp i)
                     (assert (= (sb-vm::%simd-pack-ref-single res i) 15f0))
                     (assert (= (sb-vm::%simd-pack-ref-single res i) 10f0))))))
    ;; Double+
    (let ((a (sb-ext:%make-simd-pack-512-double 10d0 10d0 10d0 10d0 10d0 10d0 10d0 10d0))
          (b (sb-ext:%make-simd-pack-512-double 5d0 5d0 5d0 5d0 5d0 5d0 5d0 5d0))
          (m (sb-ext:%make-simd-pack-512-mask #x55)))
      (let ((res (funcall fn-add-m-double a b m)))
        (loop for i from 0 to 7
              do (if (evenp i)
                     (assert (= (sb-vm::%simd-pack-ref-double res i) 15d0))
                     (assert (= (sb-vm::%simd-pack-ref-double res i) 10d0))))))
    ;; Ub32+
    (let ((a (sb-ext:%make-simd-pack-512-ub32 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10))
          (b (sb-ext:%make-simd-pack-512-ub32 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5))
          (m (sb-ext:%make-simd-pack-512-mask #x5555)))
      (let ((res (funcall fn-add-m-ub32 a b m)))
        (assert (= (sb-kernel:%simd-pack-512-0 res) (logior 15 (ash 10 32))))))
    ;; Ub64+
    (let ((a (sb-ext:%make-simd-pack-512-ub64 10 10 10 10 10 10 10 10))
          (b (sb-ext:%make-simd-pack-512-ub64 5 5 5 5 5 5 5 5))
          (m (sb-ext:%make-simd-pack-512-mask #x55)))
      (let ((res (funcall fn-add-m-ub64 a b m)))
        (assert (= (sb-kernel:%simd-pack-512-0 res) 15))
        (assert (= (sb-kernel:%simd-pack-512-1 res) 10))))
    ;; Single Sqrt
    (let ((a (sb-ext:%make-simd-pack-512-single 4f0 4f0 4f0 4f0 4f0 4f0 4f0 4f0
                                                4f0 4f0 4f0 4f0 4f0 4f0 4f0 4f0))
          (m (sb-ext:%make-simd-pack-512-mask #x5555)))
      (let ((res (funcall fn-sqrt-m-single a m)))
        (loop for i from 0 to 15
              do (if (evenp i)
                     (assert (%float-eq (sb-vm::%simd-pack-ref-single res i) 2f0))
                     (assert (%float-eq (sb-vm::%simd-pack-ref-single res i) 4f0))))))
    ;; Double Sqrt
    (let ((a (sb-ext:%make-simd-pack-512-double 4d0 4d0 4d0 4d0 4d0 4d0 4d0 4d0))
          (m (sb-ext:%make-simd-pack-512-mask #x55)))
      (let ((res (funcall fn-sqrt-m-double a m)))
        (loop for i from 0 to 7
              do (if (evenp i)
                     (assert (%float-eq (sb-vm::%simd-pack-ref-double res i) 2d0))
                     (assert (%float-eq (sb-vm::%simd-pack-ref-double res i) 4d0))))))
    ;; Logical masked
    (let ((a (sb-ext:%make-simd-pack-512-ub64 #xF0 #xF0 #xF0 #xF0 #xF0 #xF0 #xF0 #xF0))
          (b (sb-ext:%make-simd-pack-512-ub64 #xCC #xCC #xCC #xCC #xCC #xCC #xCC #xCC))
          (m (sb-ext:%make-simd-pack-512-mask #x55)))
      (let ((res (funcall fn-and-m a b m)))
        (assert (= (sb-kernel:%simd-pack-512-0 res) #xC0))
        (assert (= (sb-kernel:%simd-pack-512-1 res) #xF0)))
      (let ((res (funcall fn-or-m a b m)))
        (assert (= (sb-kernel:%simd-pack-512-0 res) #xFC))
        (assert (= (sb-kernel:%simd-pack-512-1 res) #xF0)))
      (let ((res (funcall fn-xor-m a b m)))
        (assert (= (sb-kernel:%simd-pack-512-0 res) #x3C))
        (assert (= (sb-kernel:%simd-pack-512-1 res) #xF0))))))
