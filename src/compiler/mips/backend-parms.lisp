(in-package "SB!VM")

(def!constant +backend-fasl-file-implementation+ :mips)
(setf *backend-register-save-penalty* 3)
(setf *backend-byte-order*
      #!+little-endian :little-endian
      #!-little-endian :big-endian)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The o32 ABI specifies 4k-64k as page size. We have to pick the
  ;; maximum since mprotect() works only with page granularity.
  (setf *backend-page-bytes* 65536))
