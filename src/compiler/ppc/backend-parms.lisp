(in-package "SB!VM")

(setf *backend-fasl-file-type* "fasl")
(defconstant +backend-fasl-file-implementation+ :ppc)
(setf *backend-register-save-penalty* 3)
(setf *backend-byte-order* :big-endian)
(setf *backend-page-size* 4096)

