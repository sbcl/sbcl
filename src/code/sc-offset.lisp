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
(assert (and (= (length +sc+offset-offset-bytes+) 1)
             (= (length +sc+offset-scn-bytes+) 2)
             (= (byte-position (car +sc+offset-scn-bytes+)) 0)))
(defun make-sc+offset (sc-number offset)
  (dpb (ash sc-number (- (byte-size (car +sc+offset-scn-bytes+))))
       (cadr +sc+offset-scn-bytes+)
       (logior (ash offset (byte-position (car +sc+offset-offset-bytes+)))
               (logand sc-number (ldb (car +sc+offset-scn-bytes+) -1)))))

(declaim (ftype (sfunction (sc+offset) sc-number)
                sc+offset-scn)
         (ftype (sfunction (sc+offset) sb-vm:sc-offset)
                sc+offset-offset))
(defun sc+offset-scn (sc+offset)
  (truly-the sc-number
             (dpb (ldb (cadr +sc+offset-scn-bytes+) sc+offset)
                  (byte (+ (byte-position (cadr +sc+offset-scn-bytes+))
                           (byte-size (cadr +sc+offset-scn-bytes+)))
                        (byte-size (car +sc+offset-scn-bytes+)))
                  sc+offset)))

(defun sc+offset-offset (sc+offset)
  (ldb (car +sc+offset-offset-bytes+) sc+offset))
