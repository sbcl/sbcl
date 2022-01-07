;;;; more gc tests

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

#-(and gencgc (not metaspace))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:exit :code 104))

(defconstant min-code-header-bytes
  (let ((min-header-words (* 2 (ceiling sb-vm:code-constants-offset 2))))
    (* min-header-words sb-vm:n-word-bytes)))

;;; A newly opened region must not start on a page that has 0 bytes available.
;;; The effect of that was to cause start_addr to be the next page's address,
;;; where the OPEN_REGION_PAGE_FLAG was already set on each page in the region
;;; including the one that was completely full. This caused a failure when
;;; closing the region because find_page_index(start_addr) was not the *first*
;;; page on which the open flag should be removed.
;;; Strangely, the assertion that caught this was far removed from the
;;; point of failure, in conservative_root_p()
(with-test (:name :gc-region-pickup :skipped-on (not (or :x86 :x86-64))
                  ;; (and apparently can also fail on linux with larger card size)
                  :fails-on (and :win32 :x86))
  (flet ((allocate-code-bytes (nbytes)
           ;; Make a code component occupying exactly NBYTES bytes in total.
           (assert (zerop (mod nbytes (* 2 sb-vm:n-word-bytes))))
           (assert (>= nbytes min-code-header-bytes))
           (sb-c:allocate-code-object nil 0
                                      sb-vm:code-constants-offset
                                      (- nbytes min-code-header-bytes)))
         (get-code-region (a)
           (declare (type (simple-array sb-ext:word (4)) a))
           ;; Return array of 4: free-ptr, end-addr, last-page, start-addr
           (dotimes (i 4 a)
             (setf (aref a i)
                   (deref (extern-alien "gc_alloc_region" (array unsigned 12))
                          ;; code region is the third region in the GC global
                          ;; regions. Each region is described by 4 words.
                          (+ 8 i))))))
    (let ((a (make-array 4 :element-type 'sb-ext:word))
          (code-size
           #+64-bit 10240
           #-64-bit (- (* 4 sb-vm:gencgc-page-bytes) (* 2 sb-vm:n-word-bytes)))
          (saved-region-start)
          (saved-region-end))
      (symbol-macrolet ((free-ptr (aref a 0))
                        (end-addr (aref a 1))
                        (last-page (aref a 2))
                        (start-addr (aref a 3)))
        (gc) ; This will leave the code region in a closed state
        (get-code-region a)
        (assert (= free-ptr end-addr))
        ;; Allocate a teency amount to start a new region
        (sb-c:allocate-code-object nil 0 sb-vm:code-constants-offset 0)
        (get-code-region a)
        (setq saved-region-start start-addr
              saved-region-end end-addr)
        (multiple-value-bind (n-chunks remainder)
            (floor (- end-addr free-ptr) code-size)
          ;; (Maybe) use up a few bytes more so that the larger objects
          ;; exactly consume the entirety of the region.
          (when (plusp remainder)
            (allocate-code-bytes (max remainder min-code-header-bytes))
            (get-code-region a)
            (multiple-value-setq (n-chunks remainder)
              (floor (- end-addr free-ptr) code-size)))
          (when (plusp remainder)
            ;; This happens only if the MAX expression above bumped the
            ;; remainder up, so now there is a different remainder.
            (allocate-code-bytes remainder))
          (dotimes (i (1- n-chunks))
            (allocate-code-bytes code-size))
          ;; Now make two more objects, one consuming almost the entirety
          ;; of the region, and one touching just the final page.
          (allocate-code-bytes (- code-size 128))
          (let ((c (allocate-code-bytes 128)))
            (get-code-region a)
            ;; The region should be the same region we started with,
            ;; not a new one.
            (assert (= start-addr saved-region-start))
            (assert (= end-addr saved-region-end))
            ;; It should be totally full
            (assert (= free-ptr end-addr))
            ;; Create an object to open a new region where the last one
            ;; ended. It should start on the next completely empty page,
            ;; not the prior totally full page.
            (allocate-code-bytes 128)
            ;; This GC failed
            (gc)
            ;; Return C (so that it has to be kept live on the stack).
            c))))))

;;; This test pertains only to the compact-instance-header feature.
;;; It should pass regardless of the feature presence though.

;;; Everything from here down to the WITH-TEST is the setup to try
;;; to hit "implausible layout" in verify_gc() which would occur
;;; prior to the associated fix in update_page_write_prot().
;;; GC has to use care if the sole pointer to a layout is the header
;;; of an obsolete instance, so not to miss any old -> young pointers
;;; where the layout pointer is in the high half of the header word.
(setf (extern-alien "verify_gens" char) 0)

(defstruct foo)

(defun change-foo-layout (myfoo)
  ;; Slam a new layout (a copy of the existing one) into a FOO
  ;; The new layout will be *younger* than the FOO itself,
  ;; which is exactly the situation that tickled the GC bug.
  (flet ((copy-layout (layout)
           ;; don't just COPY-STRUCTURE - that would place it in dynamic space
           (let ((new-layout
                  (sb-kernel:make-layout (sb-kernel::hash-layout-name nil)
                                         (sb-kernel:wrapper-classoid layout))))
             (sb-kernel:%byte-blt
              (sb-sys:int-sap
               (- (sb-kernel:get-lisp-obj-address layout)
                  sb-vm:instance-pointer-lowtag))
              sb-vm:n-word-bytes ; do not copy the header!
              (sb-sys:int-sap
               (- (sb-kernel:get-lisp-obj-address new-layout)
                  sb-vm:instance-pointer-lowtag))
              sb-vm:n-word-bytes ; correspondingly with above
              (* (1+ (sb-kernel:%instance-length layout))
                 sb-vm:n-word-bytes))
             new-layout)))
    (sb-kernel:%set-instance-layout myfoo
          (copy-layout (sb-kernel:%instance-layout myfoo)))
    nil))

(defconstant n-conses-per-page
  (/ sb-vm:gencgc-page-bytes (* 2 sb-vm:n-word-bytes)))

(defparameter *junk*
  (make-array (1+ (* 2 n-conses-per-page))))

;;; In order to get a FOO on a page by itself, we pad the page
;;; with conses of immediate values, before and after.
;;; So it doesn't matter where the FOO is, but there won't be
;;; other objects with pointers in them (the conses don't have pointers).
;;; And we point at all those conses from a single vector
;;; so that scavenging is forced to linearize them onto one page,
;;; along with the FOO, after discovering that the vector is live.
(defun filljunk (n)
  (let ((j -1))
    (dotimes (i n)
      (setf (svref *junk* (incf j)) (cons 1 2)))
    (setf (svref *junk* (incf j)) (make-foo))
    (dotimes (i n)
      (setf (svref *junk* (incf j)) (cons 1 2)))))

(filljunk n-conses-per-page)

;; Promote *junk* and all its referenced objects into generation 1
;; With luck, they should be in order, and the page on which the FOO
;; resides should have some leading and trailing conses.
;; (The conses ensure that nothing else is on the page impeding
;; validity of the test)
(gc :gen 1)
(assert (= (sb-kernel:generation-of *junk*) 1))

;;; These aren't defined anywhere, just "implied" by gencgc-internal.h
(defconstant page-write-protect-bit #+big-endian 2 #+little-endian 5)

;;; This test is very contrived, but this bug was observed in real life,
;;; having something to do with SB-PCL::CHECK-WRAPPER-VALIDITY.
(with-test (:name :gc-anonymous-layout)
  ;;; The page on which the FOO instance resides should be WPed
  ;;; and should have nothing else but conses on it.
  (let* ((foo (svref *junk* n-conses-per-page))
         (page (sb-vm::find-page-index (sb-kernel:get-lisp-obj-address foo)))
         (gen (slot (deref sb-vm::page-table page) 'sb-vm::gen))
         (wp (sb-sys:with-pinned-objects (foo) (sb-kernel:page-protected-p foo)))
         (page-addr (+ sb-vm:dynamic-space-start
                       (* sb-vm:gencgc-page-bytes page)))
         (aok t))
    (declare (ignorable gen wp))
    ; (format t "~&page ~d: gen=~d flags=~b (wp=~a)~%" page gen flags wp))
    ;; Check that the page holding the FOO has those conses and nothing else
    (sb-vm::map-objects-in-range
     (lambda (obj type size)
       (declare (ignore type size))
       (unless (typep obj '(or (cons (eql 1) (eql 2)) foo))
         (setq aok nil)))
     (sb-kernel:%make-lisp-obj page-addr)
     (sb-kernel:%make-lisp-obj (+ page-addr sb-vm:gencgc-page-bytes)))
    (assert aok) ; page should have nothing but a foo and the conses

    (change-foo-layout foo)

    ;; Assert that we didn't accidentally copy the header word of the layout,
    ;; which would place it in generation 1 (and probably break other parts of GC)
    (assert (= (sb-kernel:generation-of (sb-kernel:%instance-layout foo)) 0))

    ;; And the page with FOO must have gotten touched
    (sb-sys:with-pinned-objects (foo) (assert (not (sb-kernel:page-protected-p foo))))

    ;; It requires *two* GCs, not one, to cause this bug.
    ;; The first GC sees that the page with the FOO on it was touched,
    ;; and so GC is required to scavenge the whole page.
    ;; This scavenge pass enlivens the wonky layout that we created,
    ;; but it INCORRECTLY would re-protect the page, because there did
    ;; not seem to be any pointer to younger. (The conses are immediates,
    ;; the FOO has no slots, and its compact header was opaque)
    (gc)
    #+nil
    (format t "~&page ~d: wp=~a~%"
            page (sb-kernel:page-protected-p foo))

    ;; This GC would fail in the verify step because it trashes the apparently
    ;; orphaned layout, which actually does have a referer.
    (gc)))
