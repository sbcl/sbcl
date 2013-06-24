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
;;;; The debug info representation makes extensive use of integers
;;;; encoded in a byte vector using a variable number of bytes:
;;;;    0..253 => the integer
;;;;    254 => read next two bytes for integer
;;;;    255 => read next four bytes for integer

;;; Given a byte vector VEC and an index variable INDEX, read a
;;; variable length integer and advance index.
(defun %read-var-integer (vec index)
  (let ((val (aref vec index)))
    (cond ((<= val 253)
           (values val (1+ index)))
          ((= val 254)
           (values
            (logior (aref vec (+ index 1))
                    (ash (aref vec (+ index 2)) 8))
            (+ index 3)))
          (t
           (values
            (logior (aref vec (+ index 1))
                    (ash (aref vec (+ index 2)) 8)
                    (ash (aref vec (+ index 3)) 16)
                    (ash (aref vec (+ index 4)) 24))
            (+ index 5))))))

(defmacro read-var-integer (vec index)
  (once-only ((vec vec))
    `(multiple-value-bind (value new-index)
         (%read-var-integer ,vec ,index)
       (setf ,index new-index)
       value)))

;;; Take an adjustable vector VEC with a fill pointer and push the
;;; variable length representation of INT on the end.
(defun write-var-integer (int vec)
  (declare (type (unsigned-byte 32) int))
  (cond ((<= int 253)
         (vector-push-extend int vec))
        (t
         (let ((32-p (> int #xFFFF)))
           (vector-push-extend (if 32-p 255 254) vec)
           (vector-push-extend (ldb (byte 8 0) int) vec)
           (vector-push-extend (ldb (byte 8 8) int) vec)
           (when 32-p
             (vector-push-extend (ldb (byte 8 16) int) vec)
             (vector-push-extend (ldb (byte 8 24) int) vec)))))
  (values))

;;;; packed strings
;;;;
;;;; A packed string is a variable length integer length followed by
;;;; the character codes.

;;; Read a packed string from VEC starting at INDEX, advancing INDEX.
(defmacro read-var-string (vec index)
  (once-only ((len `(read-var-integer ,vec ,index)))
    (once-only ((res `(make-string ,len)))
      `(progn
         (loop for i from 0 below ,len
               do (setf (aref ,res i)
                        (code-char (read-var-integer ,vec ,index))))
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
