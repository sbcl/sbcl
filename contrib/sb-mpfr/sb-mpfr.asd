#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-mpfr"
  :name "SB-MPFR"
  :version "0.1"
  :description "bignum float calculations for SBCL using the MPFR library"
  :serial t
  :depends-on ("sb-gmp")
  :components ((:file "mpfr"))
  :perform (load-op :after (o c) (provide 'sb-mpfr))
  :in-order-to ((test-op (test-op "sb-mpfr/tests"))))

(defsystem "sb-mpfr/tests"
  :depends-on ("sb-rt" "sb-mpfr")
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system "sb-mpfr/tests"))))
  (if (not (member :sb-mpfr *features*))
      (warn "unable to test sb-mpfr: libmpfr unavailable")
      (multiple-value-bind (soft strict pending)
          (funcall (intern "DO-TESTS" (find-package "SB-RT")))
        (declare (ignorable pending))
        (fresh-line)
        (unless strict
          (warn "ignoring expected failures in sb-mpfr-tests"))
        (unless soft
          (error "sb-mpfr-tests failed with unexpected failures")))))
