(error "Can't build contribs with ASDF")

(defsystem "sb-mpfr"
  :name "SB-MPFR"
  :version "0.1"
  :description "bignum float calculations for SBCL using the MPFR library"
  :serial t
  :components ((:file "mpfr")))
