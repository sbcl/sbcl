(in-package "SB!VM")

(def!constant +backend-fasl-file-implementation+ :ppc)
(setf *backend-register-save-penalty* 3)
(setf *backend-byte-order* :big-endian)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; On Linux, the ABI specifies the page size to be 4k-65k, use the
  ;; maximum of that range. FIXME: it'd be great if somebody would
  ;; find out whether using exact multiples of the page size actually
  ;; matters in the few places where that's done, or whether we could
  ;; just use 4k everywhere.
  (setf *backend-page-size* #!+linux 65536 #!-linux 4096))

;;; The size in bytes of the GENCGC pages. Should be a multiple of the
;;; architecture page size.
(def!constant gencgc-page-size *backend-page-size*)
