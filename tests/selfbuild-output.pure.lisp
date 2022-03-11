
(with-test (:name :make-list-%make-list-not-called
                  :fails-on (:not :x86-64))
  (assert (not (ctu:find-named-callees #'make-list))))

;;; Make sure that we see the literal value of
;;; SB-UNICODE::+CHARACTER-MISC-DATABASE+
(with-test (:name :alphanumericp-is-inlined
                  :skipped-on (:not :sb-unicode))
  (let ((code (sb-kernel:fun-code-header #'sb-impl::case-frob-capitalize-out)))
    (loop for i from sb-vm:code-constants-offset below (sb-kernel:code-header-words code)
          thereis (eq (sb-kernel:code-header-ref code i)
                      sb-unicode::+character-misc-database+))))

(with-test (:name :byte-bash-copier-mixup :skipped-on (or (:not :sb-devel)
                                                          :sb-devel-no-errors))
  (let ((a (make-array 20 :element-type '(unsigned-byte 8)
                          :initial-element 0))
        (b (make-array 5 :element-type '(unsigned-byte 32))))
    ;; Can't mix-and-match array element types.
    (assert-error (sb-kernel:ub8-bash-copy a 0 b 0 5))))
