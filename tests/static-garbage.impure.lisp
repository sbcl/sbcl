;;; Ideally we want to assert that there are no pseudostatic unreachable objects
;;; at all, but that's not simple to do from lisp.
;;; Perform this by itself to remain unaffected by anything else.
#-cheneygc
(with-test (:name :pseudostatic-garbage :skipped-on (:not :sb-thread))
  (flet ((check-not-static (list)
           (dolist (x list)
             (assert (< (sb-kernel:generation-of x) sb-vm:+pseudo-static-generation+)))))
    (check-not-static (sb-vm:list-allocated-objects :all :test #'sb-thread::thread-p))
    (check-not-static (sb-vm:list-allocated-objects :all :test #'sb-thread::semaphore-p))))
