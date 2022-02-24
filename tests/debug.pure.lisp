
;;; Cross-check the C and lisp implementations of varint decoding
;;; the compiled debug fun locations.
(with-test (:name :c-decode-compiled-debug-fun-locs)
  (with-alien ((df-decode-locs (function int unsigned (* int) (* int))
                               :extern)
               (offset int)
               (elsewhere-pc int))
    (dolist (code (sb-vm::list-allocated-objects
                   :all :type sb-vm:code-header-widetag))
      (when (typep (sb-kernel:%code-debug-info code)
                   'sb-c::compiled-debug-info)
        (do ((cdf (sb-c::compiled-debug-info-fun-map
                   (sb-kernel:%code-debug-info code))
                  (sb-c::compiled-debug-fun-next cdf)))
            ((null cdf))
          (let* ((locs (sb-c::compiled-debug-fun-encoded-locs cdf))
                 (res (sb-sys:with-pinned-objects (locs)
                        (alien-funcall df-decode-locs (sb-kernel:get-lisp-obj-address locs)
                                       (addr offset) (addr elsewhere-pc)))))
            (assert (= res 1))
            (multiple-value-bind (start-pc expect-elsewhere-pc form-number expect-offset)
                (sb-c::cdf-decode-locs cdf)
              (declare (ignore start-pc form-number))
              (unless (and (= offset expect-offset)
                           (= elsewhere-pc expect-elsewhere-pc))
                (format t "Fail: ~X ~S ~S ~S ~S~%"
                        (sb-kernel:get-lisp-obj-address cdf)
                        offset expect-offset
                        elsewhere-pc expect-elsewhere-pc)))))))))
