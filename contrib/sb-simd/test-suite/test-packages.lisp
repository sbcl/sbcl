(in-package #:sb-simd-test-suite)

(define-test packages)

(define-test packages
  (check-package '#:sb-simd-internals)
  (check-package '#:sb-simd)
  #+x86-64
  (progn
    (check-package '#:sb-simd-x86-64)
    (check-package '#:sb-simd-sse)
    (check-package '#:sb-simd-sse2)
    (check-package '#:sb-simd-sse3)
    (check-package '#:sb-simd-ssse3)
    (check-package '#:sb-simd-sse4.1)
    (check-package '#:sb-simd-sse4.2)
    (check-package '#:sb-simd-avx)
    (check-package '#:sb-simd-avx2)
    (check-package '#:sb-simd-avx512f :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd))
    (check-package '#:sb-simd-avx512bw :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd))
    (check-package '#:sb-simd-avx512dq :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd))
    (check-package '#:sb-simd-avx512fp16 :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd))
    (check-package '#:sb-simd-avx10.1 :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd))
    (check-package '#:sb-simd-avx10.2 :skip '(f32-fmaddsub f32-fmsubadd f64-fmaddsub f64-fmsubadd)))
  #+arm64
  (check-package '#:sb-simd-neon)
  ;; Ensure that every instruction has a corresponding VOP.
  (dolist (instruction-record (filter-available-function-records #'instruction-record-p))
    (is (fboundp (instruction-record-vop instruction-record)))))

(define-test sap-ref-bindings
  #+x86-64
  (let ((packages '(sb-simd-sse sb-simd-sse2 sb-simd-avx sb-simd-avx2
                    sb-simd-avx512f sb-simd-avx512bw sb-simd-avx512dq
                    sb-simd-avx512fp16 sb-simd-avx10.1 sb-simd-avx10.2)))
    (dolist (pkg packages)
      (let ((p (find-package pkg)))
        (when p
          (do-external-symbols (sym p)
            (let ((name (symbol-name sym)))
              (when (and (find #\. name)
                         (search "-SAP-REF" name)
                         (not (search "NON-TEMPORAL" name))
                         (not (search "STRING" name)))
                (is (fboundp sym))
                (is (fboundp `(setf ,sym)))))))))))

(define-test cpu-identification-bounds
  #+x86-64
  (progn
    (is (typep (sb-simd-internals:sse-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx2-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx512f-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx512fp16-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx10-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx10.1-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx10.2-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx10-128-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx10-256-supported-p) 'boolean))
    (is (typep (sb-simd-internals:avx10-512-supported-p) 'boolean))))

(define-test missing-instructions-and-constant-folding
  (let ((unavailable-sets (loop for s being the hash-values of sb-simd-internals:*instruction-sets*
                                unless (sb-simd-internals:instruction-set-available-p s)
                                collect s)))
    (dolist (s (remove-duplicates unavailable-sets :key #'sb-simd-internals:instruction-set-name))
      (let ((pkg (sb-simd-internals:instruction-set-package s)))
        (do-external-symbols (sym pkg)
          (when (and (fboundp sym)
                     (not (macro-function sym))
                     (not (special-operator-p sym)))
            (let ((rec (sb-simd-internals:find-function-record sym nil)))
              (when (and rec
                         (eq (sb-simd-internals:function-record-instruction-set rec) s)
                         (sb-simd-internals:instruction-record-p rec))
                (let* ((req-args (sb-simd-internals:function-record-required-argument-records rec))
                       (dummy-args (mapcar (lambda (a)
                                             (let ((type (sb-simd-internals:value-record-name a)))
                                               (case type
                                                 ((f32 f64) 0.0)
                                                 ((s8 s16 s32 s64 u8 u16 u32 u64 imm8) 0)
                                                 (t nil))))
                                           req-args)))
                  (signals sb-simd-internals::missing-instruction
                    (apply (fdefinition sym) dummy-args))
                  (is (functionp (compile nil `(lambda () (,sym ,@dummy-args))))))))))))))
