(in-package "SB!VM")

(def!constant +backend-fasl-file-implementation+ :hppa)
(setf *backend-register-save-penalty* 3)
(setf *backend-byte-order* :big-endian)
(setf *backend-page-bytes* 4096)
