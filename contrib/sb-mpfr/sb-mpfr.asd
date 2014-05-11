(defpackage #:sb-mpfr-system (:use #:asdf #:cl))

(in-package #:sb-mpfr-system)

(defsystem sb-mpfr
  :name "SB-MPFR"
  :version "0.1"
  :description "bignum float calculations for SBCL using the MPFR library"
  :serial t
  :depends-on (sb-gmp)
  :components ((:module sb-mpfr
                :pathname ""
                :components ((:file "mpfr")))))

(defsystem sb-mpfr-tests
  :depends-on (sb-rt sb-mpfr)
  :components ((:file "tests")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-mpfr))))
  (provide 'sb-mpfr))

(defmethod perform ((o test-op) (c (eql (find-system :sb-mpfr))))
  (operate 'load-op 'sb-mpfr)
  (cond ((member :sb-mpfr *features*)
         (operate 'load-op 'sb-mpfr-tests)
         (operate 'test-op 'sb-mpfr-tests))
        (t
         (warn "unable to test sb-mpfr: libmpfr unavailable"))))

(defmethod perform ((o test-op) (c (eql (find-system :sb-mpfr-tests))))
  (multiple-value-bind (soft strict pending)
      (funcall (intern "DO-TESTS" (find-package "SB-RT")))
    (declare (ignorable pending))
    (fresh-line)
    (unless strict
      (warn "ignoring expected failures in sb-mpfr-tests"))
    (unless soft
      (error "sb-mpfr-tests failed with unexpected failures"))))
