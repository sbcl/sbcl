#-(or sb-testing-contrib sb-building-contrib)
(error "Can't build contribs with ASDF")

(defsystem "sb-capstone"
  :name "SB-CAPSTONE"
  :version "0.1"
  :description "Multi-target disassembly for SBCL using Capstone library"
  :serial t
  :components ((:file "capstone"))
  :perform (load-op :after (o c) (provide 'sb-capstone))
  :in-order-to ((test-op (test-op "sb-capstone/tests"))))

(defsystem "sb-capstone/tests"
  :depends-on ("sb-capstone" "sb-rt")
  :version "0.1"
  :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system "sb-capstone/tests"))))
  (if (member :sb-capstone *features*)
      (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
          (error "test-op failed"))
      (warn "Could not test sb-capstone")))
