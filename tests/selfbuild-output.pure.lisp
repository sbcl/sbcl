
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
