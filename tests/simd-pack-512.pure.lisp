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

(defun %simd-pack-512-ints (pack)
  (values (sb-vm::%simd-pack-ref-64 pack 0)
          (sb-vm::%simd-pack-ref-64 pack 1)
          (sb-vm::%simd-pack-ref-64 pack 2)
          (sb-vm::%simd-pack-ref-64 pack 3)
          (sb-vm::%simd-pack-ref-64 pack 4)
          (sb-vm::%simd-pack-ref-64 pack 5)
          (sb-vm::%simd-pack-ref-64 pack 6)
          (sb-vm::%simd-pack-ref-64 pack 7)))

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
          do (assert (eql p0 (sb-kernel:%simd-pack-512-0 pack)))
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
                          (write pack
                            :stream stream
                            :pretty t
                            :escape nil))))
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

;; evex patch
(push-package)

(cl:in-package :test-util)

(with-test (:name :auto-promoted-evex-disp8-printer)
  (let* ((asm-pkg (find-package "SB-X86-64-ASM"))
         (printer-fun (find-symbol "AVX2-INST-PRINTER-LIST" asm-pkg))
         (inst-format (find-symbol "YMM-YMM/MEM" asm-pkg))
         (disp64 (find-symbol "EVEX-YMMREG/MEM-DISP64" asm-pkg)))
    (when (and printer-fun inst-format disp64)
      (let* ((printer-forms
               (funcall printer-fun inst-format #xf3 ; prefix
                 #x6f ; opcode
                 :opcode-prefix #x0f
                 :w 0))
             (evex-form
               (find-if (lambda (form)
                          (and (eq (first form) :printer)
                               (let ((name (second form)))
                                 (and (symbolp name)
                                      (search "EVEX-" (symbol-name name))
                                      (equal (second (assoc 'sb-x86-64-asm::ll
                                                             (third form)))
                                             2)))))
                        printer-forms)))
        (assert evex-form)
        (let ((fields (third evex-form)))
          (assert (equal (second (assoc 'sb-x86-64-asm::ll fields)) 2))
          ;; The full-vector ZMM memory form uses compressed displacement N=64.
          ;; The reg/mem field is (REG/MEM NIL :TYPE '<type>) -- the type name
          ;; is quoted since it's evaluated when this list is later spliced
          ;; into the instruction's compiled :PRINTER clause.
          (assert (eq (second
                       (getf (cddr (assoc 'sb-x86-64-asm::reg/mem fields))
                             :type))
                      disp64)))))))

(with-test (:name :auto-promoted-vmovaps-disp8-printer)
  (let* ((asm-pkg (find-package "SB-X86-64-ASM"))
         (printer-fun (find-symbol "AVX2-INST-PRINTER-LIST" asm-pkg))
         (inst-format (find-symbol "YMM-YMM/MEM" asm-pkg))
         (disp64 (find-symbol "EVEX-YMMREG/MEM-DISP64" asm-pkg)))
    (when (and printer-fun inst-format disp64)
      (let* ((printer-forms
               (funcall printer-fun inst-format nil ; prefix for vmovaps
                 #x28 ; opcode-from
                 :opcode-prefix #x0f
                 :w 0
                 :disp-n 64))
             (evex-form
               (find-if (lambda (form)
                          (and (eq (first form) :printer)
                               (let ((name (second form)))
                                 (and (symbolp name)
                                      (search "EVEX-" (symbol-name name))
                                      (equal (second (assoc 'sb-x86-64-asm::ll
                                                             (third form)))
                                             2)))))
                        printer-forms)))
        (assert evex-form)
        (let ((fields (third evex-form)))
          (assert (eq (second
                       (getf (cddr (assoc 'sb-x86-64-asm::reg/mem fields))
                             :type))
                      disp64)))))))

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
                                 (and (eql (second
                                            (assoc 'sb-x86-64-asm::w fields))
                                           w)
                                      (assoc 'sb-x86-64-asm::reg/mem
                                             fields)))))
                        forms)))
        (let ((w0-forms (funcall printer-fun inst-format #xf3 #x7b
                          :nds t
                          :w 0
                          :disp-n 4))
              (w1-forms (funcall printer-fun inst-format #xf3 #x7b
                          :nds t
                          :w 1
                          :disp-n 8)))
          (let ((w0-form (find-disp-n w0-forms 0))
                (w1-form (find-disp-n w1-forms 1)))
            (assert w0-form)
            (assert w1-form)
            (let ((w0-reg/mem (assoc 'sb-x86-64-asm::reg/mem (third w0-form)))
                  (w1-reg/mem (assoc 'sb-x86-64-asm::reg/mem (third w1-form))))
              ;; The reg/mem field is (REG/MEM NIL :TYPE '<type>).
              (assert (eq (second (getf (cddr w0-reg/mem) :type))
                          (find-symbol "EVEX-YMMREG/MEM-DISP4" asm-pkg)))
              (assert (eq (second (getf (cddr w1-reg/mem) :type))
                          (find-symbol "EVEX-YMMREG/MEM-DISP8" asm-pkg))))))))))

(test-util:pop-package)

(with-test (:name :constant-1)
  (checked-compile-and-assert
      (:optimize :default)
      `(lambda ()
         (%simd-pack-512-ints
          sb-vm::(inline-vop
                     (((x int-avx512-reg sb-vm::simd-pack-512-ub64)
                       (sb-ext:%make-simd-pack-512-ub64 . #.(make-list 8 :initial-element (ldb (byte 64 0) -1)))))
                     ((r int-avx512-reg sb-vm::simd-pack-512-ub64))
                   (move r x))))
      (() #1=(values #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF
                     #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF)))
  (checked-compile-and-assert
      (:optimize :default)
      `(lambda ()
         (%simd-pack-512-ints
          sb-vm::(inline-vop
                     (((x double-avx512-reg simd-pack-512-double)
                       (sb-ext:%make-simd-pack-512-double . #.(make-list 8 :initial-element (sb-kernel:%make-double-float -1)))))
                     ((r int-avx512-reg sb-vm::simd-pack-512-ub64))
                   (move r x))))
    (() #1#)))
