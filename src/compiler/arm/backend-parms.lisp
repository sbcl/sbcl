(in-package "SB!VM")

(def!constant +backend-fasl-file-implementation+ :arm)
(setf *backend-byte-order*
      #!+little-endian :little-endian
      #!-little-endian :big-endian)
