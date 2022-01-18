
#-immobile-space (sb-ext:exit :code 104)

(defun make-page-full-of-fdefns ()
  ;; Make a bunch of fdefns until we're aligned at a page boundary.
  (let* ((page-size 4096)
         (n-per-page (floor page-size sb-vm:symbol-size))
         (fdefn)
         (page-base)
         (list)
         (retries 0))
    (tagbody
       try-again
       (format t "~&Try ~D~%" (1+ retries))
       (dotimes (i n-per-page)
         (setq fdefn (sb-vm::alloc-immobile-fdefn))
         (when (not (logtest (logandc2 (sb-kernel:get-lisp-obj-address fdefn)
                                       sb-vm:lowtag-mask)
                             (1- page-size)))
           ;; Yay! This fdefn is immobile-page-aligned
           (return)))
       (setq page-base (logandc2 (sb-kernel:get-lisp-obj-address fdefn)
                                 sb-vm:lowtag-mask))
       (setq list (list fdefn))
       ;; Now try to allocate enough more to fill up the page so that
       ;; it is assured that there are not other (older) random fdefns
       ;; on that page.
       (dotimes (i (1- n-per-page))
         (let* ((other-fdefn (sb-vm::alloc-immobile-fdefn))
                (addr (logandc2 (sb-kernel:get-lisp-obj-address other-fdefn)
                                sb-vm:lowtag-mask)))
           (cond ((= addr (+ page-base (* (1+ i) sb-vm:fdefn-size sb-vm:n-word-bytes)))
                  ;; (format t "Winner~%")
                  (push fdefn list)) ; ensure liveness
                 (t
                  ;; (format t "Oops~%")
                  (setq list nil)
                  (if (<= (incf retries) 10)
                      (go try-again)
                      (error "Test fails")))))))
    (format t "~&Made page of fdefns~%")
    (let ((wps (mapcar 'make-weak-pointer list)))
      (setq fdefn nil list nil)
      ;; Now we need to make one more fdefn (or anything really) so that it gets
      ;; a higher address than the "victim" page, so that the high-water-mark
      ;; of allocated pages is strictly higher than PAGE-BASE, thus ensuring that
      ;; after GC it looks like the page at PAGE-BASE could be in use.
      (let ((another (sb-vm::alloc-immobile-fdefn)))
        (assert (>= (sb-kernel:get-lisp-obj-address another)
                    (+ page-base page-size))))
      ;; And return page-base so we don't read any weak-pointer-value henceforth.
      (values wps page-base))))

;;; This could get SIGFPE in search_immobile_space() with the following unfortunate result:
;;; UNEXPECTED-FAILURE :FIND-ON-EMPTY-FIXEDOBJ-PAGE
;;;     due to SB-KERNEL:FLOATING-POINT-EXCEPTION:
;;;         "An arithmetic error SB-KERNEL:FLOATING-POINT-EXCEPTION was signalled.
;;; No traps are enabled? How can this be?
(with-test (:name :find-on-empty-fixedobj-page
            :skipped-on (not :sb-thread)) ;; fails intermittently
  (multiple-value-bind (wps page-base) (make-page-full-of-fdefns)
    (format t "~&Fdefn page base = ~x~%" page-base)
    (sb-sys:scrub-control-stack)
    (gc :full t)
    (dolist (wp wps) (assert (not (weak-pointer-value wp))))
    (assert (not (sb-di::code-header-from-pc
                  (logior page-base sb-vm:other-pointer-lowtag))))))
