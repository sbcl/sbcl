#+(or (not system-tlabs) (not sb-thread) interpreter)
(invoke-restart 'run-tests::skip-file)

;; Prevent the finalizer from performing background compilation
(sb-impl::finalizer-thread-stop)

(defstruct foo a b c d e f g)

(with-test (:name :fast-slot-map-fn-not-in-arena)
  (let ((a (sb-vm:new-arena 1048576)))
    (sb-vm:with-arena (a)
      (let ((val (sb-pcl::structure-slot-value (make-foo :g "gee") 'g)))
        (assert (string= val "gee"))))
    (assert (= (length sb-c::*background-tasks*) 1))
    (assert (heap-allocated-p sb-c::*background-tasks*))
    (assert (sb-kernel:closurep (car sb-c::*background-tasks*)))
    (assert (heap-allocated-p (car sb-c::*background-tasks*)))))
