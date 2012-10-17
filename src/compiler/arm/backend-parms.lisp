(in-package "SB!VM")

(def!constant +backend-fasl-file-implementation+ :arm)
(setf *backend-byte-order*
      #!+little-endian :little-endian
      #!-little-endian :big-endian)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Minumum observed value, not authoritative.
  (setf *backend-page-bytes* 4096))
