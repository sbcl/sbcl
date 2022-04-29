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

#-sb-simd-pack (invoke-restart 'run-tests::skip-file)
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

(with-test (:name (simd-pack print :smoke))
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
(with-test (:name :load-simd-pack-int)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-kernel:%make-simd-pack-ub64 2 4)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-kernel:simd-pack (unsigned-byte 64))))
             (assert (= 2 (sb-kernel:%simd-pack-low *pack*)))
             (assert (= 4 (sb-kernel:%simd-pack-high *pack*)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

(with-test (:name :load-simd-pack-single)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-kernel:%make-simd-pack-single 1f0 2f0 3f0 4f0)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-kernel:simd-pack single-float)))
             (assert (equal (multiple-value-list (sb-kernel:%simd-pack-singles *pack*))
                            '(1f0 2f0 3f0 4f0)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

(with-test (:name :load-simd-pack-double)
  (with-open-file (s *tmp-filename*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print '(setq *pack* (sb-kernel:%make-simd-pack-double 1d0 2d0)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*pack* nil))
             (load tmp-fasl)
             (assert (typep *pack* '(sb-kernel:simd-pack double-float)))
             (assert (equal (multiple-value-list (sb-kernel:%simd-pack-doubles *pack*))
                            '(1d0 2d0)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

(with-test (:name (simd-pack subtypep :smoke))
  (assert-tri-eq t t (subtypep '(simd-pack (unsigned-byte 8)) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack (unsigned-byte 16)) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack (unsigned-byte 32)) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack (unsigned-byte 64)) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack (signed-byte 8)) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack (signed-byte 16)) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack (signed-byte 32)) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack (signed-byte 64)) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack single-float) 'simd-pack))
  (assert-tri-eq t t (subtypep '(simd-pack double-float) 'simd-pack))
  (assert-tri-eq nil t (subtypep 'simd-pack '(simd-pack (unsigned-byte 64))))
  (assert-tri-eq nil t (subtypep 'simd-pack '(simd-pack single-float)))
  (assert-tri-eq nil t (subtypep 'simd-pack '(simd-pack double-float)))
  (assert-tri-eq t t (subtypep '(simd-pack (unsigned-byte 64))
                               '(or (simd-pack (unsigned-byte 64)) (simd-pack single-float))))
  (assert-tri-eq t t (subtypep '(simd-pack (unsigned-byte 64))
                               '(or (simd-pack (unsigned-byte 64)) (simd-pack double-float))))
  (assert-tri-eq nil t (subtypep '(simd-pack (unsigned-byte 64))
                                 '(or (simd-pack single-float) (simd-pack double-float))))
  (assert-tri-eq nil t (subtypep '(or (simd-pack (unsigned-byte 64)) (simd-pack single-float))
                                 '(simd-pack (unsigned-byte 64))))
  (assert-tri-eq nil t (subtypep '(or (simd-pack (unsigned-byte 64)) (simd-pack double-float))
                                 '(simd-pack (unsigned-byte 64))))
  (assert-tri-eq nil t (subtypep '(or (simd-pack single-float) (simd-pack double-float))
                                 '(simd-pack (unsigned-byte 64)))))

(with-test (:name (simd-pack :ctype-unparse :smoke))
  (flet ((unparsed (s) (sb-kernel:type-specifier (sb-kernel:specifier-type s))))
    (assert (equal (unparsed 'simd-pack) 'simd-pack))
    (assert (equal (unparsed '(simd-pack (unsigned-byte 8))) '(simd-pack (unsigned-byte 8))))
    (assert (equal (unparsed '(simd-pack (unsigned-byte 16))) '(simd-pack (unsigned-byte 16))))
    (assert (equal (unparsed '(simd-pack (unsigned-byte 32))) '(simd-pack (unsigned-byte 32))))
    (assert (equal (unparsed '(simd-pack (unsigned-byte 64))) '(simd-pack (unsigned-byte 64))))
    (assert (equal (unparsed '(simd-pack (signed-byte 8))) '(simd-pack (signed-byte 8))))
    (assert (equal (unparsed '(simd-pack (signed-byte 16))) '(simd-pack (signed-byte 16))))
    (assert (equal (unparsed '(simd-pack (signed-byte 32))) '(simd-pack (signed-byte 32))))
    (assert (equal (unparsed '(simd-pack (signed-byte 64))) '(simd-pack (signed-byte 64))))
    (assert (equal (unparsed '(simd-pack single-float)) '(simd-pack single-float)))
    (assert (equal (unparsed '(simd-pack double-float)) '(simd-pack double-float)))
    (assert (equal (unparsed '(or (simd-pack (unsigned-byte 64)) (simd-pack double-float)))
                   ;; depends on *SIMD-PACK-ELEMENT-TYPES* order
                   '(or (simd-pack double-float) (simd-pack (unsigned-byte 64)))))
    (assert (equal (unparsed '(or
                               (simd-pack (unsigned-byte 8))
                               (simd-pack (unsigned-byte 16))
                               (simd-pack (unsigned-byte 32))
                               (simd-pack (unsigned-byte 64))
                               (simd-pack (signed-byte 8))
                               (simd-pack (signed-byte 16))
                               (simd-pack (signed-byte 32))
                               (simd-pack (signed-byte 64))
                               (simd-pack single-float)
                               (simd-pack double-float)))
                   'simd-pack))))
