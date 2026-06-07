(in-package :sb-manual)

(defsection @sb-rotate-byte (:title "sb-rotate-byte")
  ;; FIXME: Copy the spec to the manual here.
  "The `SB-ROTATE-BYTE` module offers an interface to bitwise
  rotation, with an efficient implementation for operations which can
  be performed directly using the platform's arithmetic routines. It
  implements the specification at <http://www.cliki.net/ROTATE-BYTE>."
  ;; FIXME: cite
  "Bitwise rotation is a component of various cryptographic or hashing
  algorithms: MD5, SHA-1, etc.; often these algorithms are specified
  on 32-bit rings."
  (sb-rotate-byte:rotate-byte function))
