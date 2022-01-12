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

#-sb-simd-pack-256 (invoke-restart 'run-tests::skip-file)

(when (zerop (sb-alien:extern-alien "avx2_supported" int))
  (format t "~&INFO: simd-pack-256 not supported")
  (invoke-restart 'run-tests::skip-file))

(defun make-constant-packs ()
  (values (sb-ext:%make-simd-pack-256-ub64 1 2 3 4)
          (sb-ext:%make-simd-pack-256-ub32 0 0 0 0 0 0 0 0)
          (sb-ext:%make-simd-pack-256-ub64 (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1))

          (sb-ext:%make-simd-pack-256-single 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)
          (sb-ext:%make-simd-pack-256-single 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
          (sb-ext:%make-simd-pack-256-single (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1)
                                             (sb-kernel:make-single-float -1))

          (sb-ext:%make-simd-pack-256-double 1d0 2d0 3d0 4d0)
          (sb-ext:%make-simd-pack-256-double 0d0 0d0 0d0 0d0)
          (sb-ext:%make-simd-pack-256-double (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1))
                                             (sb-kernel:make-double-float
                                              -1 (ldb (byte 32 0) -1)))))


(with-test (:name :compile-simd-pack-256-256)
  (multiple-value-bind (i i0 i-1
                        f f0 f-1
                        d d0 d-1)
      (make-constant-packs)
    (loop for (p0 p1 p2 p3) in (list '(1 2 3 4) '(0 0 0 0)
                                     (list (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)
                                           (ldb (byte 64 0) -1)))
          for pack in (list i i0 i-1)
          do (assert (eql p0 (sb-kernel:%simd-pack-256-0 pack)))
             (assert (eql p1 (sb-kernel:%simd-pack-256-1 pack)))
             (assert (eql p2 (sb-kernel:%simd-pack-256-2 pack)))
             (assert (eql p3 (sb-kernel:%simd-pack-256-3 pack))))
    (loop for expected in (list '(1f0 2f0 3f0 4f0 5f0 6f0 7f0)
                                '(0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
                                (make-list
                                 8 :initial-element (sb-kernel:make-single-float -1)))
          for pack in (list f f0 f-1)
          do (assert (every #'eql expected
                            (multiple-value-list (sb-ext:%simd-pack-256-singles pack)))))
    (loop for expected in (list '(1d0 2d0 3d0 4d0)
                                '(0d0 0d0 0d0 0d0)
                                (make-list
                                 4 :initial-element (sb-kernel:make-double-float
                                                     -1 (ldb (byte 32 0) -1))))
          for pack in (list d d0 d-1)
          do (assert (every #'eql expected
                            (multiple-value-list (sb-ext:%simd-pack-256-doubles pack)))))))

(with-test (:name (simd-pack-256 print :smoke))
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
                    (typecase pack
                      ((simd-pack-256 single-float)
                       (if (and *print-readably*
                                (some #'float-nan-p (multiple-value-list
                                                     (%simd-pack-256-singles pack))))
                           (assert-error (do-it) print-not-readable)
                           (do-it)))
                      ((simd-pack-256 double-float)
                       (if (and *print-readably*
                                (some #'float-nan-p (multiple-value-list
                                                     (%simd-pack-256-doubles pack))))
                           (assert-error (do-it) print-not-readable)
                           (do-it)))
                      (t
                       (do-it)))))))))
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
(with-test (:name :load-simd-pack-256-int)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-ext:%make-simd-pack-256-ub64 2 4 8 16)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-ext:simd-pack-256 integer)))
             (assert (= 2 (sb-kernel:%simd-pack-256-0 *pack*)))
             (assert (= 4 (sb-kernel:%simd-pack-256-1 *pack*)))
             (assert (= 8 (sb-kernel:%simd-pack-256-2 *pack*)))
             (assert (= 16 (sb-kernel:%simd-pack-256-3 *pack*)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

(with-test (:name :load-simd-pack-256-single)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-ext:%make-simd-pack-256-single 1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-ext:simd-pack-256 single-float)))
             (assert (equal (multiple-value-list (sb-ext:%simd-pack-256-singles *pack*))
                            '(1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

(with-test (:name :load-simd-pack-256-double)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-ext:%make-simd-pack-256-double 1d0 2d0 3d0 4d0)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-ext:simd-pack-256 double-float)))
             (assert (equal (multiple-value-list (sb-ext:%simd-pack-256-doubles *pack*))
                            '(1d0 2d0 3d0 4d0)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))


(with-test (:name :spilling)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (declare ((sb-ext:simd-pack-256 (unsigned-byte 64)) x))
         (eval y)
         (list (sb-kernel:%simd-pack-256-0 x)
               (sb-kernel:%simd-pack-256-1 x)
               (sb-kernel:%simd-pack-256-2 x)
               (sb-kernel:%simd-pack-256-3 x) y))
    (((sb-ext:%make-simd-pack-256-ub64 1 2 3 4) 0) '(1 2 3 4 0) :test #'equal)))
