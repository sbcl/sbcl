(in-package :sb-manual)

(defsection @sb-md5 (:title "sb-md5")
  ;; FIXME: cite
  "The `SB-MD5` module implements the RFC1321 MD5 Message Digest
  Algorithm."
  (sb-md5:md5sum-file function)
  (sb-md5:md5sum-sequence function)
  (sb-md5:md5sum-stream function)
  (sb-md5:md5sum-string function)
  "The implementation for CMUCL was largely done by Pierre Mai, with help
  from members of the `cmucl-help` mailing list. Since CMUCL and SBCL
  are similar in many respects, it was not too difficult to extend the
  low-level implementation optimizations for CMUCL to SBCL. Following
  this, SBCL's compiler was extended to implement efficient
  compilation of modular arithmetic (@MODULAR-ARITHMETIC), which
  enabled the implementation to be expressed in portable arithmetical
  terms, apart from the use of @SB-ROTATE-BYTE for bitwise rotation.")
