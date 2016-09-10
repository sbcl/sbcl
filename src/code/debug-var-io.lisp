;;;; variable-length encoding and other i/o tricks for the debugger

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; reading variable length integers
;;;;
;;;; The debug info representation makes extensive use of 32-bit
;;;; integers encoded in an octet vector using between one and five
;;;; octets. Each octet
;;;;
;;;;   miiiiiii
;;;;
;;;; encodes 7 bits of the integer (i) while the final bit indicates
;;;; whether more (m) octets follow. For example:
;;;;
;;;;   #x88888888 => 10001000 10010001 10100010 11000100 00001000

;;; Given an octet-vector SOURCE and an initial offset START, read a
;;; variable length integer and return two values 1) the integer 2)
;;; the new offset
(defun var-integer-decoding-error (source start offset)
  (error "~@<Improperly terminated variable-length integer in ~S at ~
          ~D (starting at ~D).~@:>"
         source offset start))

(macrolet
    ((define-read-var-integer (function-name macro-name source-type accessor)
       `(progn
          (declaim (ftype (function (,source-type index)
                                    (values (unsigned-byte 32) index))
                          ,function-name))
          (defun ,function-name (source start)
            (loop
               for offset :of-type index              from start  ; position in buffer
               for k      :of-type (integer 0 28)     from 0 by 7 ; position in integer
               for octet                              =    (,accessor source offset)
               for finalp                             =    (not (logbitp 7 octet))
               for accum  :of-type (unsigned-byte 36) =    (mask-field (byte 7 0) octet)
                                                      then (dpb octet (byte 7 k) accum)
               when (and (= k 28) (not (zerop (ldb (byte 4 4) octet))))
               do (var-integer-decoding-error source start offset)
               when finalp return (values accum (1+ offset))))

          (defmacro ,macro-name (vector index)
            `(multiple-value-bind (value new-index)
                 (,',function-name ,vector ,index)
               (setf ,index new-index)
               value)))))

  (define-read-var-integer read-var-integer read-var-integerf
    (array (unsigned-byte 8) 1) aref)

  #-sb-xc-host
  (define-read-var-integer sap-read-var-integer sap-read-var-integerf
    system-area-pointer sap-ref-8))

;;; Take an adjustable vector VECTOR with a fill pointer and push the
;;; variable length representation of VALUE on the end.
(declaim (ftype (sfunction ((unsigned-byte 32) (array (unsigned-byte 8) 1)) (integer 0 5))
                write-var-integer))
(defun write-var-integer (value vector)
  (loop
     for v      :of-type (unsigned-byte 32) = value then (ash v -7)
     for v-next :of-type (unsigned-byte 32) = (ash v -7)
     for i                                  from 0
     until (and (plusp i) (zerop v))
     do (vector-push-extend (dpb (if (zerop v-next) 0 1) (byte 1 7)
                                 (ldb (byte 7 0) v))
                            vector)
     finally (return i)))


;;;; packed strings
;;;;
;;;; A packed string is a variable length integer length followed by
;;;; the character codes.

;;; Read a packed string from VEC starting at INDEX, advancing INDEX.
(defmacro read-var-string (vec index)
  (once-only ((len `(read-var-integerf ,vec ,index)))
    (once-only ((res `(make-string ,len)))
      `(progn
         (loop for i from 0 below ,len
               do (setf (aref ,res i)
                        (code-char (read-var-integerf ,vec ,index))))
         ,res))))

;;; Write STRING into VEC (adjustable, with fill-pointer) represented
;;; as the length (in a var-length integer) followed by the codes of
;;; the characters.
(defun write-var-string (string vec)
  (declare (simple-string string))
  (let ((len (length string)))
    (write-var-integer len vec)
    (dotimes (i len)
      (write-var-integer (char-code (schar string i)) vec)))
  (values))

;;;; packed bit vectors

;;; Read the specified number of BYTES out of VEC at INDEX and convert
;;; them to a BIT-VECTOR. INDEX is incremented.
(defmacro read-packed-bit-vector (bytes vec index)
  (once-only ((n-bytes bytes))
    (once-only ((n-res `(make-array (* ,n-bytes 8) :element-type 'bit)))
      `(progn
         (%byte-blt ,vec ,index ,n-res 0 ,n-bytes)
         (incf ,index ,n-bytes)
         ,n-res))))
