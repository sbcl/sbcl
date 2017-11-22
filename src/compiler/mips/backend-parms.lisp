(in-package "SB!VM")

(defconstant +backend-fasl-file-implementation+ :mips)

  ;; The o32 ABI specifies 4k-64k as page size. We have to pick the
  ;; maximum since mprotect() works only with page granularity.
(defconstant +backend-page-bytes+ 65536)
