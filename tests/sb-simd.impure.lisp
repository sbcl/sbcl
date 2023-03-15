;;; Under the evaluator
;;; (SB-SIMD-TEST-SUITE::|SB-SIMD-SSE4.1:U32.4-IF|)
;;; failed for me with
;;;    RESULT-0 = #<SIMD-PACK   33554433        127       8191    2097153>,
;;;    OUTPUT-0 = #<SIMD-PACK          3       8193       2303 1073741823>.
;;; maybe there are other problems but I didn't investigate further.

#+interpreter (invoke-restart 'run-tests::skip-file)

(handler-case (require :sb-simd)
  (condition (c)
    (cond ((search "Don't know how" (princ-to-string c))
           (format t "~&Skipping test of sb-simd~%")
           (invoke-restart 'run-tests::skip-file))
          (t
           (error "Unexpected error: ~A" c)))))

(when (zerop (sb-alien:extern-alien "avx2_supported" int))
  (format t "~&Skipping test of sb-simd~%")
  (invoke-restart 'run-tests::skip-file))

(with-compilation-unit ()
  (dolist (file '("packages.lisp"
                  "numbers.lisp"
                  "utilities.lisp"
                  "test-suite.lisp"
                  "test-arefs.lisp"
                  "test-simple-simd-functions.lisp"
                  "test-horizontal-functions.lisp"
                  "test-hairy-simd-functions.lisp"
                  "test-packages.lisp"))
    (load (merge-pathnames file #P"../contrib/sb-simd/test-suite/"))))
(sb-simd-test-suite::run-test-suite)
