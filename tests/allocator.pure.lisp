(progn
(defun on-large-page-p (x)
  (and (eq (sb-ext:heap-allocated-p x) :dynamic)
       (let ((flags
              (sb-sys:with-pinned-objects (x)
                (sb-alien:slot (sb-alien:deref sb-vm::page-table
                                               (sb-vm:find-page-index
                                                (sb-kernel:get-lisp-obj-address x)))
                               'sb-vm::flags))))
         (logbitp 4 flags)))) ; SINGLE_OBJECT_FLAG
(compile 'on-large-page-p)

;;; Pseudo-static large objects should retain the single-object flag

;;; Prior to change 3b137be67217 ("speed up trans_list"),
;;; gc_general_alloc() would always test whether it was allocating a large
;;; object via "if (nbytes >= LARGE_OBJECT_SIZE)" and in that case it would
;;; call gc_alloc_large(). It was not overwhelmingly necessary to perform the
;;; size test - which is an extra branch for almost no reason - because large
;;; objects should end up in the slow path by default. (So we only make the
;;; slow path a little slower, and speed up the fast path)
;;; However, 32-bit machines with small page size (4Kb) have a sufficiently small
;;; "large" object size that many more objects ought to be characterized as large.
;;; In conjunction with the fact that code allocation always opens allocations
;;; regions of at least 64k (= 16 pages), we find that code blobs end up in the
;;; open region by accident. This doesn't happen for the 32-bit architectures
;;; where the GENCGC-PAGE-BYTES is defined as 64KB because the minimum
;;; of 64KB is only 1 page, but a "large" object is 4 pages or more.
;;; So the fix is for trans_code() to do the size test, and then we don't
;;; slow down the general case of gc_general_alloc.

;;; With #+mark-region-gc there is a range of "large-ish" objects (between
;;; 3/4 and 1 page large) where we try to allocate in a small page if
;;; possible, but claim a fresh large page instead of wasting the small
;;; page, so these tests don't work.
(with-test (:name :pseudostatic-large-objects :skipped-on :mark-region-gc)
  (sb-vm:map-allocated-objects
   (lambda (obj type size)
     (declare (ignore type size))
     (when (>= (sb-ext:primitive-object-size obj) sb-vm:large-object-size)
       (let* ((addr (sb-kernel:get-lisp-obj-address obj))
              (pte (deref sb-vm:page-table (sb-vm:find-page-index addr))))
         (when (eq (slot pte 'sb-vm::gen) sb-vm:+pseudo-static-generation+)
           (assert (on-large-page-p obj))))))
   :dynamic))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter large-n-words (/ sb-vm:large-object-size sb-vm:n-word-bytes))
  (defparameter large-n-conses (/ large-n-words 2))))

(with-test (:name :large-object-pages :skipped-on (:or :mark-region-gc (:not :gencgc)))
  ;; adding in a 2-word vector header makes it at least large-object-size.
  ;; The threshold in the allocator is exact equality for that.
  (let ((definitely-large-vector (make-array (- large-n-words 2)))
        ;; Decreasing by 1 word isn't enough, because of padding, so decrease by 2 words
        (not-large-vector (make-array (- large-n-words 4))))
    ;; Verify the edge case for LARGE-OBJECT-P
    (assert (on-large-page-p definitely-large-vector))
    (assert (not (on-large-page-p not-large-vector)))
    (assert (not (on-large-page-p (list 1 2))))))
(with-test (:name :no-&rest-on-large-object-pages :skipped-on (:not :gencgc))
  (let ((fun (checked-compile '(lambda (&rest params) params))))
    (assert (not (on-large-page-p (apply fun (make-list large-n-conses)))))))

;;; MIPS either: (1) runs for 10 minutes just in COMPILE and then croaks in the assembler
;;; due to an overly large displacement in an instruction, (2) crashes with heap exhaustion.
;;; I don't really care enough to fix it.  A flat profile shows the following top hot spots:
;;;
;;;            Self        Total        Cumul
;;;   Nr  Count     %  Count     %  Count     %    Calls  Function
;;; ------------------------------------------------------------------------
;;;    1    813 677.5    813 677.5    813 677.5        -  SB-REGALLOC::CONFLICTS-IN-SC
;;;    2    208 173.3    208 173.3   1021 850.8        -  SB-C::COALESCE-MORE-LTN-NUMBERS
;;;    3    118  98.3    118  98.3   1139 949.2        -  NTH
;;;    4     63  52.5    878 731.7   1202 1001.7        -  (LABELS SB-REGALLOC::ATTEMPT-LOCATION :IN SB-REGALLOC::SELECT-LOCATION)
;;;
;;; (And I don't know much about math, but I don't think that's how percentages work)
;;;
;;; I don't remember what the problem is with PPC.
(with-test (:name :no-list-on-large-object-pages
                  :skipped-on (:or :mips :ppc :ppc64))
  (let* ((fun (checked-compile
               '(lambda ()
                 (macrolet ((expand (n) `(list ,@(loop for i from 1 to n collect i))))
                   (expand #.large-n-conses)))))
         (list (funcall fun)))
    (assert (not (on-large-page-p list)))))
