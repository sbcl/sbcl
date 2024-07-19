#+(or (not x86) (not linux)) (invoke-restart 'run-tests::skip-file)

;;; I'm not entirely satisfied with the solution to fixing the crash on Alpine linux
;;; and while investigating that, I found also that we supplied too high a limit
;;; to set_thread_area().

;;; [As to the problem on Alpine: if it were the case that we don't pre-decrement the
;;; alien-stack-pointer then we'd surely clobber th->no_tls_value_marker.
;;; But it works everywhere else. Alpine uses musl libc, so perhaps that's the distinguishing
;;; feature - something in libc takes an alien we allocated and writes past it by a word
;;; because we still after all these years don't fully get the structure padding requirements
;;; in the 32-bit ABI correct.]

(in-package sb-vm)

(defconstant os-page-size 4096)
(defparameter *tls-size-in-bytes* (extern-alien "dynamic_values_bytes" int))
(defparameter *tls-size-in-pages* (/ *tls-size-in-bytes* os-page-size))

(defun make-tls-reader (disp)
  ;; Return anything that proves that the memory load worked.
  ;; True I don't really need to compile a new function, because
  ;; CURRENT-THREAD-OFFSET-SAP can obviously take a variable.
  ;; I prefer it this way which asserts on the disassembly.
  ;; And if this stuff weren't confusing, then we wouldn't have gotten it
  ;; wrong now, would we have?
  (let ((f
         (compile nil
          `(lambda ()
            (declare (optimize (sb-c::verify-arg-count 0)))
             (%make-lisp-obj
              ;; CURRENT-THREAD-OFFSET-SAP wants a word index, not a byte index.
              (logand (sb-sys:sap-int (sb-vm::current-thread-offset-sap
                                       ,(ash disp (- sb-vm:word-shift))))
                      #xFFFFFFFC))))))
    ;; Being pedantic here...
    (let ((string (with-output-to-string (s) (disassemble f :stream s))))
      (assert (search (format nil "FS:[#x~X]" disp) string)))
    f))

;;; Assert that loading via the FS: segment can see up to and including the last TLS cell
(test-util:with-test (:name :tls-fencepost-positive-test)
  (dotimes (i *tls-size-in-pages*)
    (let* ((begin (* os-page-size i))
           (end (+ begin os-page-size (- n-word-bytes)))
           (f1 (make-tls-reader begin))
           (f2 (make-tls-reader end)))
      (funcall f1)
      (funcall f2))))
;;; ... and not one word beyond.
;;; This says the fault address is 0, I wonder if we can do better than that.
(test-util:with-test (:name :tls-fencepost-negative-test)
  (let ((f (make-tls-reader *tls-size-in-bytes*)))
    (setf (extern-alien "lose_on_corruption_p" int) 0)
    (catch 'success
      (handler-bind ((memory-fault-error
                      (lambda (c)
                        (format t "~&Got [~A]" c)
                        (throw 'success t))))
        (funcall f))
      (error "Expected a memory fault"))))
