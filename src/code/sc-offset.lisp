;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; SC-OFFSETs
;;;;
;;;; We represent the place where some value is stored with a SC-OFFSET,
;;;; which is the SC number and offset encoded as an integer.
;;;;
;;;; The concrete encoding is exported to sc-offset.h during genesis
;;;; for use by the runtime.

(def!type sc-offset () '(unsigned-byte 27))

(defconstant-eqx +sc-offset-scn-bytes+
    `(,(byte 2 0) ,(byte 4 23))
  #'equalp)

(defconstant-eqx +sc-offset-offset-bytes+
    `(,(byte 21 2))
  #'equalp)

(declaim (ftype (sfunction ((unsigned-byte 6) (unsigned-byte 21)) sc-offset)
                make-sc-offset))
(defun make-sc-offset (sc-number offset)
  (let ((result 0))
    (flet ((add-bits (bytes source)
             (loop for byte in bytes
                for size = (byte-size byte)
                with i = 0
                do
                  (setf result (dpb (ldb (byte size i) source) byte result))
                  (incf i size))))
      (add-bits +sc-offset-scn-bytes+ sc-number)
      (add-bits +sc-offset-offset-bytes+ offset))
    result))

(declaim (ftype (sfunction (sc-offset) unsigned-byte)
                sc-offset-scn sc-offset-offset))
(flet ((extract-bits (bytes source)
         (loop with result = 0
            for byte in bytes
            for size = (byte-size byte)
            with i = 0
            do
              (setf result (dpb (ldb byte source) (byte size i) result))
              (incf i size)
            finally (return result))))

  (defun sc-offset-scn (sc-offset)
    (extract-bits +sc-offset-scn-bytes+ sc-offset))

  (defun sc-offset-offset (sc-offset)
    (extract-bits +sc-offset-offset-bytes+ sc-offset)))
