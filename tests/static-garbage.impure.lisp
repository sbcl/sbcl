;;; Ideally we want to assert that there are no pseudostatic unreachable objects
;;; at all, but that's not simple to do from lisp.
;;; Perform this by itself to remain unaffected by anything else.
(with-test (:name :pseudostatic-garbage :skipped-on (:not :sb-thread))
  (let ((list1 (sb-vm::list-allocated-objects :all :test #'sb-thread::thread-p))
        (list2 (sb-vm::list-allocated-objects :all :test #'sb-thread::semaphore-p)))
    (assert (not (cdr list1)) nil "~a" list1)
    (assert (not (cdr list2)))))
