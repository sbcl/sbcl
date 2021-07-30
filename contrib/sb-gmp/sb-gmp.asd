#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-gmp"
  :name "SB-GMP"
  :version "0.1"
  :description "bignum calculations for SBCL using the GMP library"
  :serial t
  :components ((:file "gmp"))
  :perform (load-op :after (o c) (provide 'sb-gmp))
  :in-order-to ((test-op (test-op "sb-gmp/tests"))))

(defsystem "sb-gmp/tests"
  :depends-on ("sb-rt" "sb-gmp")
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system "sb-gmp/tests"))))
  (if (not (member :sb-gmp *features*))
      (warn "unable to test sb-gmp: libgmp unavailable")
      (multiple-value-bind (soft strict pending)
          (funcall (intern "DO-TESTS" (find-package "SB-RT")))
        (declare (ignorable pending))
        (fresh-line)
        (unless strict
          (warn "ignoring expected failures in sb-gmp-tests"))
        (unless soft
          (error "sb-gmp-tests failed with unexpected failures")))))
