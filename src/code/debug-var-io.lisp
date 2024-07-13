;;;; variable-length encoding and other i/o tricks for the debugger

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

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

;;; Code fixup locations are stored as varints even more densely than
;;; would be an array of unsigned-byte. The backing storage is an integer
;;; (typically a bignum, but a fixnum will do),
;;; and each value represents the difference from the preceding value.

(defun integer-from-octets (octets)
  (declare (type (array (unsigned-byte 8) (*)) octets))
  #-sb-xc-host (aver (array-header-p octets))

  ;; Bignums are little-endian by word, but native-endian within each word,
  ;; making use of the less consy algorithm too tricky for big-endian CPUs.
  ;; And this does not work for little-endian, but I don't know why.
  ;; It should not be too hard to use random testing to find an input
  ;; at which the output obviously differs from the shift+add loop.
  #+nil
  (let ((input (%array-data octets)))
    (if (typep (%vector-raw-bits input 0) 'fixnum)
        (%vector-raw-bits input 0)
        (let* ((nbytes (length octets))
               (last-byte (if (plusp nbytes) (aref octets (1- nbytes)) 0))
               ;; If the high bit of the highest byte is 1, we might need one more
               ;; byte to avoid the bignum coming out negative.
               (nbits (* (+ nbytes (ash last-byte -7)) sb-vm:n-byte-bits))
               (ndigits (ceiling nbits sb-vm:n-word-bits))
               (bignum (sb-bignum:%allocate-bignum ndigits)))
          (dotimes (i ndigits (sb-bignum::%normalize-bignum bignum ndigits))
            (setf (%bignum-ref bignum i) (%vector-raw-bits input i))))))

  (let ((result 0) (shift 0))
    (dovector (byte octets result)
      (setf result (logior result (ash byte shift))
            shift (+ shift 8)))))

;;; Pack one, two, or three lists of fixup locations into a single varint.
;;; It makes sense to store these externally to the object, as it would otherwise
;;; intrude on text pages. Also, some of the bignums are shareable this way.
(defun pack-code-fixup-locs (list1 &optional list2 list3)
  (dx-let ((bytes (make-array (min (* 2 (+ (length list1) ; guess at final length
                                           (length list2)
                                           (length list3)))
                                   1024) ; limit the stack usage
                              :fill-pointer 0 :adjustable t
                              :element-type '(unsigned-byte 8))))
    (flet ((pack (list &aux (prev 0))
             (dolist (x list)
               ;; two fixups at the same location have to be wrong,
               ;; and a delta of 0 would mark the end of the list.
               (aver (> x prev))
               (write-var-integer (- x prev) bytes)
               (setq prev x))))
      (pack (sort list1 #'<))
      (when (or list2 list3)
        (write-var-integer 0 bytes)
        (pack (sort list2 #'<)))
      (when list3
        (write-var-integer 0 bytes)
        (pack (sort list3 #'<))))
    ;; Stuff octets into an integer
    ;; It would be quite possible in the target to do something clever here
    ;; by creating a bignum directly from the ub8 vector.
    (integer-from-octets bytes)))

(defun join-varint-streams (first second)
  (let ((length-in-bits (integer-length first)))
    (logior first (ash second (+ (align-up length-in-bits 8) 8)))))

(defmacro do-packed-varints ((loc locs &optional (bytepos nil bytepos-sup-p))
                             &body body)
  (with-unique-names (integer byte shift acc prev)
    (unless bytepos-sup-p
      (setq bytepos (make-symbol "BYTEPOS")))
    `(let ((,integer ,locs)
           ,@(unless bytepos-sup-p `((,bytepos 0)))
           (,shift 0)
           (,acc 0)
           (,prev 0))
       #-sb-xc-host (declare (notinline sb-kernel:%ldb)) ; lp#1573398
       (declare (type (mod ,sb-vm:n-word-bits) ,shift)
                (type word ,acc ,prev))
       (loop
        (let ((,byte (ldb (byte 8 ,bytepos) ,integer)))
          (incf ,bytepos 8)
          (setf ,acc (logior ,acc (logand (ash (logand ,byte #x7f) ,shift)
                                          most-positive-word)))
          (cond ((logtest ,byte #x80) (incf ,shift 7))
                ;; No offset can be zero, so this is the delimiter
                ((zerop ,acc) (return))
                (t
                 (let ((,loc (+ ,prev ,acc))) ,@body (setq ,prev ,loc))
                 (setq ,acc 0 ,shift 0))))))))

;;; Unpack the (potentially) three stream of data in PACKED-INTEGER.
(defun unpack-code-fixup-locs (packed-integer)
  (collect ((stream1) (stream2) (stream3))
    (let ((pos 0))
      (do-packed-varints (loc packed-integer pos) (stream1 loc))
      (do-packed-varints (loc packed-integer pos) (stream2 loc))
      (do-packed-varints (loc packed-integer pos) (stream3 loc)))
    (values (stream1) (stream2) (stream3))))
