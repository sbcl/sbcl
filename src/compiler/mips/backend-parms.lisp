(in-package "SB!VM")

;;; FIXME: Do I need a different one for little-endian? :spim,
;;; perhaps?
(def!constant +backend-fasl-file-implementation+ :mips)
(setf *backend-register-save-penalty* 3)
(setf *backend-byte-order* 
      #!+little-endian :little-endian 
      #!-little-endian :big-endian)
;;; FIXME: Check this. Where is it used?
(setf *backend-page-size* 4096)
