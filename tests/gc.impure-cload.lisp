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

#+(or cheneygc metaspace)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:exit :code 104))

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
