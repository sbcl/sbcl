(defpackage #:sb-gmp-system (:use #:asdf #:cl))

(in-package #:sb-gmp-system)

(defsystem sb-gmp
  :name "SB-GMP"
  :version "0.1"
  :description "bignum calculations for SBCL using the GMP library"
  :serial t
  :components ((:module sb-gmp
                :pathname ""
                :components ((:file "gmp")))))

(defsystem sb-gmp-tests
  :depends-on (sb-rt sb-gmp)
  :components ((:file "tests")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-gmp))))
  (provide 'sb-gmp))

(defmethod perform ((o test-op) (c (eql (find-system :sb-gmp))))
  (operate 'load-op 'sb-gmp)
  (cond ((member :sb-gmp *features*)
         (operate 'load-op 'sb-gmp-tests)
         (operate 'test-op 'sb-gmp-tests))
        (t
         (warn "unable to test sb-gmp: libgmp unavailable"))))

(defmethod perform ((o test-op) (c (eql (find-system :sb-gmp-tests))))
  (multiple-value-bind (soft strict pending)
      (funcall (intern "DO-TESTS" (find-package "SB-RT")))
    (declare (ignorable pending))
    (fresh-line)
    (unless strict
      (warn "ignoring expected failures in sb-gmp-tests"))
    (unless soft
      (error "sb-gmp-tests failed with unexpected failures"))))
