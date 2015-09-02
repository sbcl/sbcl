(in-package "SB!VM")

(def!constant +backend-fasl-file-implementation+ :arm)
(setf *backend-byte-order*
      #!+little-endian :little-endian
      #!-little-endian :big-endian)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Minumum observed value, not authoritative.
  (setf *backend-page-bytes* #!+linux 4096 #!+netbsd 8192))

;;; The size in bytes of GENCGC cards, i.e. the granularity at which
;;; writes to old generations are logged.  With mprotect-based write
;;; barriers, this must be a multiple of the OS page size.
(def!constant gencgc-card-bytes *backend-page-bytes*)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(def!constant gencgc-alloc-granularity 0)
;;; The minimum size at which we release address ranges to the OS.
;;; This must be a multiple of the OS page size.
(def!constant gencgc-release-granularity *backend-page-bytes*)
