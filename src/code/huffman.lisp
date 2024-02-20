;;;; a simple huffman encoder/decoder, used to compress unicode
;;;; character names.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-UNICODE")

(eval-when (:compile-toplevel :execute)
(defstruct (huffman-node (:constructor make-huffman-node (key weight))
                         (:copier nil))
  (key nil :read-only t :type simple-base-string)
  (weight nil :read-only t))

(defstruct (huffman-pair
             (:include huffman-node)
             (:copier nil)
             (:constructor make-huffman-pair
                           (left right &aux
                                 (key   (concatenate 'base-string
                                                     (huffman-node-key left)
                                                     (huffman-node-key right)))
                                 (weight (+ (huffman-node-weight left)
                                            (huffman-node-weight right))))))
  (left nil :read-only t)
  (right nil :read-only t))
(declaim (sb-ext:freeze-type huffman-node))

(defun huffman-weights (corpus)
  (let ((weight-table (make-hash-table :test #'equal)))
    (loop for string in corpus
       do (loop for char across string
             do (incf (gethash char weight-table 0))))
    (let (alist)
      (maphash (lambda (char weight)
                 ;; Stupidly verbose code here because (some seem to think) the spec
                 ;; permits absurd constructs like (SETF (CHAR (STRING #\a) 0) #\u+fffff)
                 ;; and thus (STRING base-char) does not produce a base-string.
                 ;; To me this is manifestly wrong.
                 (push (make-huffman-node (coerce (string char) 'base-string) weight) alist))
               weight-table)
      (sort alist #'string< :key #'huffman-node-key))))

(defun make-huffman-tree (corpus)
  (labels ((merge-table (table)
             (setf table (stable-sort table #'< :key #'huffman-node-weight))
             (push (make-huffman-pair (pop table) (pop table))
                   table)
             (if (second table)
                 (merge-table table)
                 (car table)))
           (finish-tree (tree)
             (if (huffman-pair-p tree)
                 (list (huffman-node-key tree)
                       (finish-tree (huffman-pair-left tree))
                       (finish-tree (huffman-pair-right tree)))
                 (huffman-node-key tree))))
    (finish-tree (merge-table (huffman-weights corpus)))))

(defun huffman-match (char node)
  (if (consp node)
      (find char (the string (car node)) :test #'equal)
      (eql char (character node))))

(defun huffman-encode (string tree)
  (let ((code 1))
    (labels ((encode (bit char tree)
               (when bit
                 (setf code (+ (ash code 1) bit)))
               (if (consp tree)
                   (destructuring-bind (key left right) tree
                     (declare (ignore key))
                     (cond ((huffman-match char left)
                            (encode 0 char left))
                           ((huffman-match char right)
                            (encode 1 char right))
                           (t
                            ;; unknown
                            (return-from huffman-encode nil))))
                   (unless (huffman-match char tree)
                     (error "Error encoding ~S (bad tree)." char)))))
      (loop for char across string
         do (encode nil char tree))
      code)))

;;; Given a list of integers (usually bignums) return a bit-vector holding the
;;; concatenation of all the bits of those integers end-to-end, and a vector of
;;; (UNSIGNED-BYTE 32) indicating the starting bit and length of each integer.
;;; The space savings is approximately as follows for a 64-bit word size -
;;; * the widetag of each bignum is esentially useless information.
;;; * the payload length never needs more than 1 byte, wasting 6 bytes.
;;; * half the time there is a padding word for alignment.
;;; * on average half the bits of the last bignum digit are wasted.
;;; * the pointers take 1 word each.
;;; The resulting simple-bit-vector and UB32 vector can go in read-only space.
(defun pack-bit-strings (inputs)
  ;; Each N as supplied uses its highest 1 bit to convey the length of the
  ;; bit stream. Drop that bit from the data as stored.
  (let* ((indices (make-array (length inputs) :element-type '(unsigned-byte 32)))
         (total-nbits (loop for n in inputs sum (1- (integer-length n))))
         (bits (make-array total-nbits :element-type 'bit))
         (input-index 0)
         (bit-index 0))
    (dolist (n inputs (values bits indices))
      (let ((nbits (1- (integer-length n))))
        ;; I anticipate that some Unicode update could cause overflow of the
        ;; 23-bit index field. (The 9-bit length should be OK for a while)
        ;; If we need more than 23 bits to store the maximum output pointer,
        ;; INDICES could easily be turned into a UB32 vector with a separate
        ;; UB8 vector to indicate the length. One array is adequate for now.
        ;; Another possibility: round the starting bit number to even, and store
        ;; the index as half its actual value. At most this wastes 1 bit per char.
        ;; But the actual length in bits is stored as-is
        (setf (aref indices input-index)
              (logior (ash (the (unsigned-byte 23) bit-index) 9)
                      (the (unsigned-byte 9) nbits)))
        (dotimes (i nbits)
          (setf (sbit bits (+ bit-index i)) (if (logbitp i n) 1 0)))
        (incf bit-index nbits)
        (incf input-index)))))
) ; end EVAL-WHEN

;; actually 88 (for now), but leave some wiggle room
(defconstant longest-unicode-char-name 96)

(defmacro with-name->char-buffer ((varname) form)
  ;; After git rev 8b606d636cc1 we have no feature indicating
  ;; support for DX strings but this is close enough.
  #+c-stack-is-control-stack
  `(dx-let ((,varname (make-array longest-unicode-char-name :element-type 'base-char)))
     ,form)

  #-c-stack-is-control-stack
  (sb-c::if-vop-existsp (:named sb-vm::allocate-vector-on-number-stack)
     (let ((nsp (gensym "NSP")))
       `(let ((,nsp (sb-sys:%primitive sb-c:current-nsp)))
          (sb-c::restoring-nsp ,nsp
                               (let ((,varname (sb-ext:truly-the
                                                (simple-base-string ,longest-unicode-char-name)
                                                (sb-sys:%primitive sb-vm::allocate-vector-on-number-stack
                                                                   sb-vm:simple-base-string-widetag
                                                                   longest-unicode-char-name
                                                                   (1+ (/ longest-unicode-char-name
                                                                          (/ sb-vm:n-word-bits sb-vm:n-byte-bits)))))))
                                 ,form))))
     `(let ((,varname (or (sb-ext:atomic-pop *name->char-buffers*)
                          (make-array longest-unicode-char-name :element-type 'base-char))))
        (prog1 ,form
          (sb-ext:atomic-push ,varname *name->char-buffers*)))))

(defun huffman-decode (bits start nbits tree output)
  (declare (type (or (simple-base-string #.longest-unicode-char-name) null) output)
           (simple-bit-vector bits)
           (index start)
           ((mod 512) nbits))
  (let ((bufpos longest-unicode-char-name)
        (pointer (+ start nbits)))
    (declare (index pointer))
    (with-name->char-buffer (buffer)
      (labels ((choose (branch)
                 (destructuring-bind (left right) (cdr branch)
                   (if (= (sbit bits (decf pointer)) 1) right left)))
               (decode (branch)
                 (when (zerop nbits)
                   (error "Invalid Huffman-code: ~S" (subseq bits start (+ start nbits))))
                 (let ((next (choose branch)))
                   (cond ((consp next)
                          (decode next))
                         (t
                          (when (> pointer start) (decode tree))
                          (setf (char buffer (decf bufpos))
                                (char (the (simple-base-string 1) next) 0)))))))
        (decode tree)
        (cond (output
               (replace output buffer :start2 bufpos)
               (- (length output) bufpos)) ; return number of significant chars
              (t
               (subseq buffer bufpos)))))))
