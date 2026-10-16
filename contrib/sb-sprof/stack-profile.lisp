(in-package "SB-THREAD")

;;; Stack profiler

;;; Return in-use memory stack memory for each thread, as a list
;;    ((thread N-control-pages N-binding-pageas N-alien-pages) ...)
;;; "page" is defined to be 4K
(defun get-stack-touched-page-counts (&optional print)
  (let ((results)
        (pagemap (sb-unix:unix-open "/proc/self/pagemap" sb-unix:o_rdonly 0)))
    (labels
        ((pageno (addr) (floor addr 4096))
         (get-used-pages (thread thread-sap)
           (macrolet ((thread-slot (x)
                        `(sap-ref-word thread-sap (ash ,x sb-vm:word-shift))))
             (let* ((cs-start (thread-slot sb-vm::thread-control-stack-start-slot))
                    (cs-end   (thread-slot sb-vm::thread-control-stack-end-slot))
                    (bs-start (thread-slot sb-vm::thread-binding-stack-start-slot))
                    (bs-end   (+ bs-start (extern-alien "thread_binding_stack_size" unsigned-int)))
                    (as-start (thread-slot sb-vm::thread-alien-stack-start-slot))
                    (as-end   (+ as-start (extern-alien "thread_alien_stack_size" unsigned-int))))
               #+nil
               (when (typep thread 'foreign-thread)
                 ;; This isn't really telling us how much Lisp stack is used.
                 ;; We need the difference between the thread's SP now
                 ;; and from when it entered Lisp. Neither is knowable.
                 (setq cs-start (align-down cs-start 4096)
                       cs-end (align-up cs-end 4096)))
               ;; new_thread_trampoline decreases control_stack_end by an amount that
               ;; excludes whatever can not possibly contain Lisp pointers. Align it up
               ;; again to satisfy the sanity-check in COUNT-PAGES.
               (push (list thread
                           (if (typep thread 'foreign-thread)
                               0
                               (count-pages cs-start (align-up cs-end 4096) :down #\c))
                           (count-pages bs-start bs-end :up #\b)
                           (count-pages as-start as-end :down #\a))
                     results))))
         (count-pages (addr-from addr-to growth-direction which)
           ;; The pagemap words for guard pages reads as "swapped" - this is an artifact
           ;; of the lightweight guard mechanism but it presents the wrong impresion
           ;; in our count of live pages, so skip over them.
           (let ((guard-size (* 2 (if (eql which #\c) sb-c:+backend-page-bytes+ 4096))))
             (ecase growth-direction
               (:up   (decf addr-to   guard-size))
               (:down (incf addr-from guard-size))))
           (binding* ((len (- addr-to addr-from))
                      ((npages remainder) (floor len 4096)))
             ;; If a stack were 16MiB (which seems more than reasonable as an upper bound)
             ;; it would occupy 4096 4KiB pages, so constrain NPAGES to that range.
             ;; This should inform the compiler that the DX array is not more than abour 32K.
             (declare (type (integer 1 4096) npages))
             (aver (= remainder 0))
             (dx-let ((pagemap-buffer
                       (make-array npages :element-type '(unsigned-byte 64)
                                          :initial-element 0)))

               (with-pinned-objects (pagemap-buffer)
                 (let* ((pagemap-size (* (length pagemap-buffer) 8))
                        (pagemap-result
                         (alien-funcall (extern-alien "pread"
                                                      (function word int system-area-pointer word word))
                                        pagemap (vector-sap pagemap-buffer)
                                        pagemap-size (* (pageno addr-from) 8))))
                   (aver (= pagemap-result pagemap-size))
                   (when print
                     (format t "pagemap swapped:~%~S~%"
                             (map 'simple-bit-vector (lambda (x) (if (logbitp 62 x) 1 0)) pagemap-buffer))
                     (format t "pagemap present:~%~S~%"
                             (map 'simple-bit-vector (lambda (x) (if (logbitp 63 x) 1 0)) pagemap-buffer))
                     (format t "pagemap dirty:~%~S~%"
                             (map 'simple-bit-vector (lambda (x) (if (logbitp 55 x) 1 0)) pagemap-buffer)))
                   (let ((count
                          (loop for elt across pagemap-buffer
                                ;; bit 63 = present, bit 62 = swapped
                                count (ldb-test (byte 2 62) elt))))
                     (when print
                       (format t "~&  Range : ~x ~x ~d npages=~3D/~3D (~A)~%"
                               addr-from addr-to npages count npages which))
                     count)))))))
      (unwind-protect
           (dolist (thread (%list-all-threads) results)
             (when print
               (format t "~&Looking at ~S~%" thread))
             (if (eq thread *current-thread*)
                 (get-used-pages thread (current-thread-sap))
                 (with-tls-lock (thread c-thread)
                   (when (/= c-thread 0)
                     (get-used-pages thread (int-sap c-thread))))))
        (sb-unix:unix-close pagemap)))))
