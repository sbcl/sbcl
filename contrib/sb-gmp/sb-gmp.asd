(error "Can't build contribs with ASDF")

(defsystem "sb-gmp"
  :name "SB-GMP"
  :version "0.1"
  :description "bignum calculations for SBCL using the GMP library"
  :serial t
  :components ((:file "gmp")))
