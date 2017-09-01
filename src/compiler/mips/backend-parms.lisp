(in-package "SB!VM")

(defconstant +backend-fasl-file-implementation+ :mips)
(setf *backend-register-save-penalty* 3)
(setf *backend-byte-order*
      #!+little-endian :little-endian
      #!-little-endian :big-endian)

  ;; The o32 ABI specifies 4k-64k as page size. We have to pick the
  ;; maximum since mprotect() works only with page granularity.
(defconstant +backend-page-bytes+ 65536)
