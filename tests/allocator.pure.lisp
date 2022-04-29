#+gencgc
(progn
(defun large-object-p (x)
  (and (eq (sb-ext:heap-allocated-p x) :dynamic)
       (let ((flags
              (sb-sys:with-pinned-objects (x)
                (sb-alien:slot (sb-alien:deref sb-vm::page-table
                                               (sb-vm:find-page-index
                                                (sb-kernel:get-lisp-obj-address x)))
                               'sb-vm::flags))))
         (logbitp 4 ; SINGLE_OBJECT_FLAG
                  (ldb (byte 6 (+ #+big-endian 2)) flags)))))
(compile 'large-object-p)

;;; Pseudo-static large objects should retain the single-object flag

;;; This test fails on certain 32-bit architectures depending on GENCGC-CARD-BYTES.
;;; The failure stems from the line of code in gc_find_freeish_pages()
;;; at the remark "Increase the region size to avoid excessive fragmentation"
;;; as well as the order of operations in the fast version of gc_general_alloc()
;;; which always tries the currently open region before trying a large allocation.
;;; It's a harmless failure, and a solution for it is worse than allowing failure.
;;; The fix would entail some combination of three (or more) ideas:
;;;  - region sizes must never be larger than LARGE-OBJECT-SIZE so that inline
;;;    allocation is never "accidentally" succesful on a large object, OR
;;;  - the fast path of gc_general_alloc() would always have to check object size
;;;    before comparing the request against the open region, so that for objects
;;;    deemed large, we ignore the open region even if it is contains enough space,
;;;    and so we go straight to gc_alloc_large(), OR
;;;  - code allocation can use something other than gc_general_alloc().
;;; The last suggestion is probably the best, but I don't care to do it.
;;; The second idea slows down the fast path, and the first increases fragmentation.
;;; In the pristine core image for x68, I observed the following code page counts
;;; with and without the first fix applied:
;;;   2124 small + 55 large = 2168 total (before rev 3b137be6)
;;;   2147 small + 35 large = 2182 total (after, without either "fix")
;;;   2177 small + 55 large = 2232 total (after, with putative "fix")
;;; So as expected, we can force large code blobs to get placed on large-object pages
;;; as they should, but with no commensurate decrease in use of small-object pages.
;;; In fact the total usage goes up.  This is understandable, because code blobs tend
;;; to be among the larger heap objects, making it relatively more likely that the
;;; allocator abandons the tail part of an open region and begins a new contiguous block.
;;; Larger regions are generally worse for root scavenging, so all other things equal
;;; it may be better to have more discontiguous regions versus larger regions.
;;; Or in other words, forcing the heap_scavenge() function to operate on larger ranges
;;; by opening a region of 16 pages at a time makes card marking less exact.
;;; So a failure here seems to be preferable to strict large/small separation,
;;; despite it being not very aesthetically pleasing to have the nondeterminism
;;; that sometimes puts potential large objects on non-large-object pages.
;;; The 32-bit architectures that use GENCGC-PAGE-BYTES = 65536 are unaffected
;;; by the change that took the size test out of the allocator fast path.

(with-test (:name :pseudostatic-large-objects :fails-on (or :mips :x86))
  (sb-vm:map-allocated-objects
   (lambda (obj type size)
     (declare (ignore type size))
     (when (>= (sb-ext:primitive-object-size obj) (* 4 sb-vm:gencgc-page-bytes))
       (let* ((addr (sb-kernel:get-lisp-obj-address obj))
              (pte (deref sb-vm:page-table (sb-vm:find-page-index addr))))
         (when (eq (slot pte 'sb-vm::gen) sb-vm:+pseudo-static-generation+)
           (assert (large-object-p obj))))))
   :all))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter large-n-words (/ sb-vm:large-object-size sb-vm:n-word-bytes))
  (defparameter large-n-conses (/ large-n-words 2))))

(with-test (:name :no-&rest-on-large-object-pages :skipped-on (:not :gencgc))
  ;; adding in a 2-word vector header makes it at least large-object-size.
  ;; The threshold in the allocator is exact equality for that.
  (let ((definitely-large-vector (make-array (- large-n-words 2)))
        ;; Decreasing by 1 word isn't enough, because of padding, so decrease by 2 words
        (not-large-vector (make-array (- large-n-words 4))))
    ;; Verify the edge case for LARGE-OBJECT-P
    (assert (large-object-p definitely-large-vector))
    (assert (not (large-object-p not-large-vector)))
    (assert (not (large-object-p (list 1 2)))))
  (let ((fun (checked-compile '(lambda (&rest params) params))))
    (assert (not (large-object-p (apply fun (make-list large-n-conses)))))))

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
    (assert (not (large-object-p list)))))
