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

;;; Pack two lists of fixup locations into a single varint.
;;; * 32-bit x86 code tends to have more absolute fixups than relative fixups.
;;;   An absolute fixup is necessary for each reference to a boxed constant
;;;   due to lack of PC-relative addressing. A relative fixup is necessary for each
;;;   call to an assembly routine.
;;; * x86-64 can use 64-bit absolute fixups to encode jump tables within a movable
;;;   or immobile object.  It uses 32-bit fixups only in immobile code.
;;;   Therefore the GC is concerned only with 64-bit fixups, whereas core relocation
;;;   and defragmentation makes use of all of them.
;;;   However, there are no 64-bit fixups recorded in the packed fixup locs.
;;;   This would need to change if we had any 64-bit fixups at locations within
;;;   the code that could not be deduced by examining the object.
;;; It makes sense to store these externally to the object, as it would otherwise
;;; intrude on text pages. Also, some of the bignums are shareable this way.
(defun pack-code-fixup-locs (abs-fixups rel-fixups more)
  (dx-let ((bytes (make-array (min (* 2 (+ (length abs-fixups) ; guess at final length
                                           (length rel-fixups)
                                           (length more)))
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
      (pack (sort abs-fixups #'<))
      (when (or rel-fixups more)
        (write-var-integer 0 bytes)
        (pack (sort rel-fixups #'<)))
      (when more
        (write-var-integer 0 bytes)
        (pack (sort more #'<))))
    ;; Stuff octets into an integer
    ;; It would be quite possible in the target to do something clever here
    ;; by creating a bignum directly from the ub8 vector.
    (integer-from-octets bytes)))

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

(define-symbol-macro lz-symbol-1 210) ; arbitrary value that isn't frequent in the input
(define-symbol-macro lz-symbol-2 218) ; ditto

(defconstant +max-lz-size+ (* 1024 64))

;;; A somewhat bad (slow and not-very-squishy) compressor
;;; that gets between 15% and 20% space savings in debug blocks.
;;; Lengthy input may be compressible by as much as 3:1.
(declaim (ftype (sfunction ((array (unsigned-byte 8) 1))
                           (or (simple-array (unsigned-byte 8) 1)
                               (simple-array (signed-byte 8) 1)))
                lz-compress))
(defun lz-compress (input)
  (if (> (length input) +max-lz-size+)
      (coerce input '(simple-array (unsigned-byte 8) (*)))
      (let* ((length (length input))
             (output (make-array length
                                 :element-type '(unsigned-byte 8)
                                 :fill-pointer 0 :adjustable t))
             (tempbuf (make-array 8 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0))
             #-sb-xc-host
             (input (truly-the simple-array (%array-data input))))
        (flet ((compare (index1 index2 end &aux (start1 index1))
                 (loop
                  (when (or (eql index2 end)
                            (not (eql (aref input index1) (aref input index2))))
                    (return-from compare (- index1 start1)))
                  (incf index1)
                  (incf index2))))
          (loop with pos of-type index = 0
                while (< pos length)
                do
                (let ((match-start 0)
                      (match-len 2))
                  ;; limit the lookback amount to make the running time n^2 in input
                  ;; length instead of n^3.
                  (loop for start from (max 0 (- pos 4000)) below pos
                        do
                        (let ((this-len (compare start pos length)))
                          (when (> this-len match-len)
                            (setq match-start start match-len this-len))))
                  (let ((offset (- pos match-start)))
                    ;; Length = 3 is emitted as symbol-2 followed by a single byte
                    ;; for the offset. Longer lengths are written as symbol-1 and
                    ;; then two varint-encoded values. We first determine whether
                    ;; writing the back-reference is shorter than the source bytes.
                    (cond ((and (> match-len 3)
                                (progn (setf (fill-pointer tempbuf) 0)
                                       (write-var-integer offset tempbuf)
                                       (write-var-integer match-len tempbuf)
                                       (< (1+ (fill-pointer tempbuf)) match-len)))
                           ;; marker symbol if followed by 0 would represent a literal
                           (aver (/= (aref tempbuf 0) 0))
                           (vector-push-extend lz-symbol-1 output)
                           (dovector (elt tempbuf) (vector-push-extend elt output))
                           (incf pos match-len))
                          ((and (= match-len 3) (< offset 256))
                           (vector-push-extend lz-symbol-2 output)
                           (vector-push-extend offset output)
                           (incf pos 3))
                          (t
                           (let ((byte (aref input pos)))
                             (incf pos)
                             (vector-push-extend byte output)
                             (when (or (= byte lz-symbol-1) (= byte lz-symbol-2))
                               (vector-push-extend 0 output)))))))))
        (let ((result
                (if (>= (length output) length)
                    (map-into (sb-xc:make-array length :element-type '(signed-byte 8))
                              (lambda (x)
                                (mask-signed-field 8 (the (unsigned-byte 8) x)))
                              input)
                    #+sb-xc-host
                    (coerce output '(simple-array (unsigned-byte 8) (*)))
                    #-sb-xc-host
                    (%shrink-vector (%array-data output) (fill-pointer output)))))
          #+(or)
          (aver (equalp input (lz-decompress result)))
          result))))

#-sb-xc-host
(progn
(declaim (ftype (sfunction ((or (simple-array (unsigned-byte 8) 1)
                                (simple-array (signed-byte 8) 1)))
                           (simple-array (unsigned-byte 8) 1))
                lz-decompress))
(defun lz-decompress (input)
  (cond ((typep input '(simple-array (signed-byte 8) 1))
         ;; Uncompressed
         (let ((result (make-array (length input) :element-type '(unsigned-byte 8))))
           (ub8-bash-copy input 0 result 0 (length input))
           result))
        ((> (length input) +max-lz-size+)
         input)
        (t
         (let* ((length (length input))
                (output (make-array (* length 2)
                                    :element-type '(unsigned-byte 8)
                                    :fill-pointer 0 :adjustable t))
                (inpos 0))
           (flet ((copy (offset length)
                    (let ((index (- (fill-pointer output) offset)))
                      (dotimes (i length)
                        (vector-push-extend (aref output index) output)
                        (incf index)))))
             (loop while (< inpos length)
                   do
                   (let ((byte (aref input inpos)))
                     (incf inpos)
                     (cond ((= byte lz-symbol-1) ; general case
                            (let ((byte (aref input inpos)))
                              (cond ((= byte 0) ; literal symbol
                                     (incf inpos)
                                     (vector-push-extend lz-symbol-1 output))
                                    (t
                                     (binding* (((offset new-inpos)
                                                 (read-var-integer input inpos))
                                                ((len new-new-inpos)
                                                 (read-var-integer input new-inpos)))
                                       (setf inpos new-new-inpos)
                                       (copy offset len))))))
                           ((= byte lz-symbol-2) ; special case
                            (let ((offset (aref input inpos)))
                              (incf inpos)
                              (if (= offset 0) ; literal symbol
                                  (vector-push-extend lz-symbol-2 output)
                                  (copy offset 3))))
                           (t
                            (vector-push-extend byte output))))))
           (%shrink-vector (%array-data output) (fill-pointer output)))))))
