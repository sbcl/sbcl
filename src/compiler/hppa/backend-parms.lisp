(in-package "SB!VM")

(defconstant +backend-fasl-file-implementation+ :hppa)
(setf *backend-register-save-penalty* 3)
(setf *backend-byte-order* :big-endian)
(defconstant +backend-page-bytes+ 4096)
