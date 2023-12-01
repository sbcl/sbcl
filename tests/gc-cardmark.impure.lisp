
;;; The layout of FOO must not get promoted from gen0 to gen1.
;;; Due to random variation, there might be a gc-with-promotion cycle
;;; just after the defstruct. So prevent that.
#+generational (setf (generation-number-of-gcs-before-promotion 0) 1000000)
(defstruct foo x)
(defun get-layouts-for-test ()
  ;; Return a young layout and an old layout
  (vector (sb-kernel:find-layout 'foo)
          (sb-kernel:find-layout 'class)))

(with-test (:name :compact-instance-header-layout
            :skipped-on (or :win32
                            :gc-stress
                            (:not (:and :compact-instance-header :soft-card-marks)))
            :fails-on (and :darwin :x86-64)) ;; can't compile the .so
  (unless (probe-file "gc-testlib.so")
    (sb-ext:run-program "sh"
                        `("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                          #+darwin ,@'("-flat_namespace" "-undefined" "suppress"  "-mmacosx-version-min=10.7")
                          "-I../src/runtime"
                          "gc-testlib.c" "-o" "gc-testlib.so")
                        :search t
                        :output t :error t))
  (load-shared-object (truename "gc-testlib.so"))
  #+sb-thread (sb-impl::finalizer-thread-stop)
  (let ((v (get-layouts-for-test)))
    (sb-sys:with-pinned-objects (v)
      (alien-funcall (extern-alien "compact_instance_layout_pointer_test"
                                   (function int unsigned))
                     (sb-kernel:get-lisp-obj-address v)))))
