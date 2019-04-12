;;; Arrange so that all layouts have bignums as their bitmap,
;;; for exercising the C instance scavenging code.
;;;
;;; The use of a hashtable here was supposed end up with some layouts
;;; whose bitmap is at a lower address than the layout itself.
;;; This should randomize scavenging such that it sometimes sees
;;; a layout-bitmap that was already forwarded.
;;; However it doesn't work as desired; something else will have
;;; to be done to force that to happen.
;;;
;;; This test can't be run with immobile space enabled
;;; due simply to the assertion that instances in immobile space
;;; (which all happen to be layouts at the moment)
;;; do not have bignum bitmaps, which is true because a LAYOUT
;;; does not have enough slots to warrant a bignum bitmap.
;;;
;;; But the test's entire purpose is to assign noncanonical bignums
;;; (that are equivalent to fixnums) into every layout.
;;;
#-(or immobile-space (and));; FIXME: it's breaking something
(let ((ht (make-hash-table :test 'eql)))
  (flet ((bignumify (int)
           (or (gethash int ht)
               (setf (gethash int ht)
                     (if (typep int 'fixnum)
                         (sb-bignum:make-small-bignum int)
                         int)))))
    (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare (ignore type size))
       (when (and (typep obj 'sb-kernel:layout)
                  (typep (sb-kernel:layout-bitmap obj) 'fixnum))
         (let ((flags (sb-kernel:layout-%bits obj)))
           (setf (sb-kernel:layout-bitmap obj)
                 (if (logtest (logior sb-kernel:+condition-layout-flag+
                                      sb-kernel:+pcl-object-layout-flag+)
                              flags)
                     ;; *** this is bogus, since conditions have an arbitrary
                     ;; number of slots, and need the special case of "all 1s"
                     ;; But it's ok as a test.
                     (1- (ash 1 99))
                     (bignumify
                      (let ((bitmap (sb-kernel:layout-bitmap obj)))
                        (if (/= bitmap -1)
                            bitmap
                            (let ((len (sb-kernel:layout-length obj)))
                              ;; force the 0th bit to 1, for the layout
                              ;; (Shouldn't LENGTH be right? Doesn't seem to be)
                              (logior #-compact-instance-header 1
                                      (1- (ash 1 len)))))))))
           (unless (plusp (length (sb-kernel:layout-equalp-tests obj)))
             (let ((n (- (sb-kernel:layout-length obj) sb-vm:instance-data-start)))
               (when (>= n 0)
                 (setf (sb-kernel:layout-equalp-tests obj)
                       (make-array n :initial-element 0))))))))
     :all)))
