;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; SC+OFFSET
;;;;
;;;; We represent the place where some value is stored with an
;;;; SC+OFFSET value, which is the storage class number and an offset
;;;; within that storage class encoded as an integer.
;;;;
;;;; The concrete encoding is described by +SC+OFFSET-SCN-BYTES+ and
;;;; +SC+OFFSET-OFFSET-BYTES+ and exported to sc-offset.h during
;;;; genesis for use by the runtime.

;;; This has to be DEF!TYPE because it appears in COMPILED-DEBUG-FUN
;;; which is DEF!STRUCT.
(def!type sc+offset ()
  `(unsigned-byte ,(+ sb-vm:sc-number-bits sb-vm:sc-offset-bits)))

(defconstant-eqx +sc+offset-scn-bytes+
    `(,(byte 2 0) ,(byte 4 23))
  #'equalp)
(assert (= (reduce #'+ +sc+offset-scn-bytes+ :key #'byte-size)
           sb-vm:sc-number-bits))

(defconstant-eqx +sc+offset-offset-bytes+
    `(,(byte 21 2))
  #'equalp)
(assert (= (reduce #'+ +sc+offset-offset-bytes+ :key #'byte-size)
           sb-vm:sc-offset-bits))

(declaim (ftype (sfunction (sc-number sb-vm:sc-offset) sc+offset)
                make-sc+offset))
(defun make-sc+offset (sc-number offset)
  (let ((result 0))
    (flet ((add-bits (bytes source)
             (loop for byte in bytes
                for size = (byte-size byte)
                with i = 0
                do
                  (setf result (dpb (ldb (byte size i) source) byte result))
                  (incf i size))))
      (add-bits +sc+offset-scn-bytes+ sc-number)
      (add-bits +sc+offset-offset-bytes+ offset))
    result))

(declaim (ftype (sfunction (sc+offset) sc-number)
                sc+offset-scn)
         (ftype (sfunction (sc+offset) sb-vm:sc-offset)
                sc+offset-offset))
(flet ((extract-bits (bytes source)
         (loop with result = 0
            for byte in bytes
            for size = (byte-size byte)
            with i = 0
            do
              (setf result (dpb (ldb byte source) (byte size i) result))
              (incf i size)
            finally (return result))))

  (defun sc+offset-scn (sc+offset)
    (extract-bits +sc+offset-scn-bytes+ sc+offset))

  (defun sc+offset-offset (sc+offset)
    (extract-bits +sc+offset-offset-bytes+ sc+offset)))
