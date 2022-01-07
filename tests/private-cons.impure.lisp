;;;; Tests of gc_private_cons

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

(defun private-list (&rest items)
  (let ((list 0))
    (dolist (x (reverse items) list)
      (setq list
            (alien-funcall
             (extern-alien "gc_private_cons" (function unsigned unsigned unsigned))
             (the fixnum x)
             list)))))

(defun sapify-private-list (ptr)
  (sb-int:collect ((items))
    (loop (when (zerop ptr) (return (items)))
          (items (sb-sys:int-sap ptr))
          (setq ptr (sb-sys:sap-ref-word (sb-sys:int-sap ptr)
                                         sb-vm:n-word-bytes)))))

(defun private-free (list)
  (alien-funcall (extern-alien "gc_private_free" (function void unsigned))
                 list))

#+gencgc
(progn
(defun page-need-to-zero (index)
  (logbitp #+little-endian 5 #+big-endian 2
           (slot (deref sb-vm::page-table index) 'sb-vm::flags)))
(defun test-private-consing ()
  (let ((conses-per-page ; subtract one for the page header cons
         (1- (/ sb-vm:gencgc-page-bytes (* 2 sb-vm:n-word-bytes))))
        (counter 0)
        (pages)
        (recycle-me))
    (dotimes (i 10)
      (let* ((cons (private-list (incf counter)))
             (index (sb-vm::find-page-index cons))
             (base-address
              (+ sb-vm:dynamic-space-start (* index sb-vm:gencgc-page-bytes)))
             (final))
        (push index pages)
        (assert (= cons (+ base-address (* 2 sb-vm:n-word-bytes))))
        ;; words-used should be 4, for 2 conses,
        ;; and the need_zerofill bit should be 1.
        (assert (= (slot (deref sb-vm::page-table index) 'sb-vm::words-used) 4))
        (assert (page-need-to-zero index))
        (dotimes (i (1- conses-per-page))
          (setq final (private-list (incf counter))))
        (assert (= final (+ base-address sb-vm:gencgc-page-bytes
                            (* -2 sb-vm:n-word-bytes))))
        (push final recycle-me)))
    (dolist (list recycle-me)
      (private-free list)) ; push each page's last cons back onto the recycle list
    ;; Make a list of 10 conses
    (let ((morelist (private-list 1 2 3 4 5 6 7 8 9 10))
          (pages pages))
      (dolist (sap (sapify-private-list morelist))
        ;; Should be on a page that was previously allocated
        (assert (= (sb-vm::find-page-index (sb-sys:sap-int sap))
                   (pop pages)))))
    (alien-funcall (extern-alien "gc_dispose_private_pages" (function void)))
    ;; Each of the pages should have zero words used and need-to-zero = 1
    (dolist (index pages)
      (assert (page-need-to-zero index))
      (assert (= (slot (deref sb-vm::page-table index) 'sb-vm::words-used) 0))))))

#-gencgc
(defun test-private-consing ()
  (let ((conses-per-chunk ; subtract one for the chunk header cons
         (1- (/ 4096 (* 2 sb-vm:n-word-bytes)))) ; 4096 = CHUNKSIZE
        (counter 0)
        (chain))
    (dotimes (i 5) ; 5 = number of times to invoke malloc()
      (push nil chain)
      ;; Use up the chunk, which happens in descending address order.
      ;; So the last cons allocated is nearest the head of the chunk.
      (dotimes (i conses-per-chunk)
        (let ((list (private-list (incf counter))))
          (setf (car chain) list)))
      ;; The malloc() result was 1 cons below the lowest cons
      ;; return by the suballocator.
      (decf (car chain) (* 2 sb-vm:n-word-bytes)))
    ;; Test that there are 5 chunks on which to invoke free()
    (let ((len 0))
      (loop (unless chain (return))
            (assert (= (sb-sys:sap-ref-word (sb-sys:int-sap (car chain))
                                            sb-vm:n-word-bytes)
                       (or (cadr chain) 0)))
            (incf len)
            (pop chain))
      (assert (= len 5))))
  (alien-funcall (extern-alien "gc_dispose_private_pages" (function void))))

;;; These tests disable GC because the private cons allocator
;;; assumes exclusive use of the page table, and moreover if GC
;;; were to occur, free_oldspace() could obliterate our test data.
(with-test (:name :private-consing)
  (sb-sys:without-gcing
   (test-private-consing)
   ;; Test pushback
   (let* ((data '(1 2 3 4 5 6))
          (list (apply 'private-list data))
          (saps (sapify-private-list list)))
     (private-free list)
     (dolist (x data)
       (progn x)
       ;; pull items from the recycle list.
       (let ((cons (sb-sys:int-sap (private-list 42))))
         (assert (member cons saps :test 'sb-sys:sap=))
         (setf saps (delete cons saps :test 'sb-sys:sap=)))))
   ;; Clean up
   (alien-funcall (extern-alien "gc_dispose_private_pages" (function void)))))
