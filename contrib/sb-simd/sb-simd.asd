(error "Can't build contribs with ASDF")

(defsystem #:sb-simd
  :description "A convenient SIMD interface for SBCL."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"
  :bind ((*compile-verbose* t)) ; very slow, I want to see progress
  :serial t
  :components
  ((:module "code"
    :components
     ((:file "packages")
      (:file "constants")
      (:file "utilities")
      (:file "printable")
      (:file "cpu-identification")
      (:file "instruction-set")
      (:file "instruction-set-case")
      (:file "record")
      (:file "missing-instruction")
      (:module "instruction-sets"
       :components
       ((:file "sb-simd")
        (:file "x86-64" :if-feature :x86-64)
        (:file "sse" :if-feature :x86-64)
        (:file "sse2" :if-feature :x86-64)
        (:file "sse3" :if-feature :x86-64)
        (:file "ssse3" :if-feature :x86-64)
        (:file "sse4-1" :if-feature :x86-64)
        (:file "sse4-2" :if-feature :x86-64)
        (:file "avx" :if-feature :x86-64)
        (:file "avx2" :if-feature :x86-64)
        (:file "fma" :if-feature :x86-64)
        (:file "arm64" :if-feature :arm64)
        (:file "neon" :if-feature :arm64)))
      (:file "define-types")
      (:file "define-instruction-vops")
      (:file "define-vref-vops")
      (:file "define-custom-vops")
      (:file "define-vop-functions")
      (:file "define-scalar-casts")
      (:file "define-fake-vops")
      (:file "x86-64-fake-vops" :if-feature :x86-64)
      (:file "arm64-fake-vops" :if-feature :arm64)
      (:file "define-simd-casts")
      (:file "define-instructions")
      (:file "define-vrefs")
      (:file "define-reffers")
      (:file "define-arefs")
      (:file "define-ifs")
      (:file "define-associatives")
      (:file "define-reducers")
      (:file "define-comparisons")
      (:file "define-unequals")
      (:file "x86-64-rounders" :if-feature :x86-64)
      (:file "define-modify-macros")
      (:file "x86-64-modify-macros" :if-feature :x86-64)
      (:file "arm64-modify-macros" :if-feature :arm64)))))
