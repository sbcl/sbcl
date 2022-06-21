(defsystem "sb-simd-test-suite"
  :description "The sb-simd test suite."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on ("sb-simd")

  :perform
  (test-op (o c) (symbol-call '#:sb-simd-test-suite '#:run-test-suite))

  :serial t
  :components
  ((:file "packages")
   (:file "numbers")
   (:file "utilities")
   (:file "test-suite")
   (:file "test-arefs")
   (:file "test-simple-simd-functions")
   (:file "test-horizontal-functions")
   (:file "test-hairy-simd-functions")
   (:file "test-packages")))
