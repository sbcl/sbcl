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

#+sb-simd-pack
(defun make-constant-packs ()
  (values (sb-kernel:%make-simd-pack-ub64 1 2)
          (sb-kernel:%make-simd-pack-ub32 0 0 0 0)
          (sb-kernel:%make-simd-pack-ub64 (ldb (byte 64 0) -1)
                                          (ldb (byte 64 0) -1))

          (sb-kernel:%make-simd-pack-single 1f0 2f0 3f0 4f0)
          (sb-kernel:%make-simd-pack-single 0f0 0f0 0f0 0f0)
          (sb-kernel:%make-simd-pack-single (sb-kernel:make-single-float -1)
                                            (sb-kernel:make-single-float -1)
                                            (sb-kernel:make-single-float -1)
                                            (sb-kernel:make-single-float -1))

          (sb-kernel:%make-simd-pack-double 1d0 2d0)
          (sb-kernel:%make-simd-pack-double 0d0 0d0)
          (sb-kernel:%make-simd-pack-double (sb-kernel:make-double-float
                                             -1 (ldb (byte 32 0) -1))
                                            (sb-kernel:make-double-float
                                             -1 (ldb (byte 32 0) -1)))))

#+sb-simd-pack
(with-test (:name :compile-simd-pack)
  (multiple-value-bind (i i0 i-1
                        f f0 f-1
                        d d0 d-1)
      (make-constant-packs)
    (loop for (lo hi) in (list '(1 2) '(0 0)
                               (list (ldb (byte 64 0) -1)
                                     (ldb (byte 64 0) -1)))
          for pack in (list i i0 i-1)
          do (assert (eql lo (sb-kernel:%simd-pack-low pack)))
             (assert (eql hi (sb-kernel:%simd-pack-high pack))))
    (loop for expected in (list '(1f0 2f0 3f0 4f0)
                                '(0f0 0f0 0f0 0f0)
                                (make-list
                                 4 :initial-element (sb-kernel:make-single-float -1)))
          for pack in (list f f0 f-1)
          do (assert (every #'eql expected
                            (multiple-value-list (sb-kernel:%simd-pack-singles pack)))))
    (loop for expected in (list '(1d0 2d0)
                                '(0d0 0d0)
                                (make-list
                                 2 :initial-element (sb-kernel:make-double-float
                                                     -1 (ldb (byte 32 0) -1))))
          for pack in (list d d0 d-1)
          do (assert (every #'eql expected
                            (multiple-value-list (sb-kernel:%simd-pack-doubles pack)))))))

#+sb-simd-pack
(with-test (:name :print-simd-pack-smoke-test)
  (let ((packs (multiple-value-list (make-constant-packs))))
    (format t "Standard~%~{~A~%~}" packs)
    (let ((*print-readably* t)
          (*read-eval* t))
      (format t "Readably~%~{~A~%~}" packs))
    (let ((*print-readably* t)
          (*read-eval* nil))
      (format t "Readably, no read-eval~%~{~A~%~}" packs))))
