;;;; implementation-dependent transforms

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; We need to define these predicates, since the TYPEP source
;;; transform picks whichever predicate was defined last when there
;;; are multiple predicates for equivalent types.
(define-source-transform short-float-p (x) `(single-float-p ,x))
#!-long-float
(define-source-transform long-float-p (x) `(double-float-p ,x))

(define-source-transform compiled-function-p (x)
  #!-sb-eval
  `(functionp ,x)
  #!+sb-eval
  (once-only ((x x))
    `(and (functionp ,x)
          (not (sb!eval:interpreted-function-p ,x)))))

(define-source-transform char-int (x)
  `(char-code ,x))

(deftransform abs ((x) (rational))
  '(if (< x 0) (- x) x))

;;; The layout is stored in slot 0.
(define-source-transform %instance-layout (x)
  `(truly-the layout (%instance-ref ,x 0)))
(define-source-transform %set-instance-layout (x val)
  `(%instance-set ,x 0 (the layout ,val)))
(define-source-transform %funcallable-instance-layout (x)
  `(truly-the layout (%funcallable-instance-info ,x 0)))
(define-source-transform %set-funcallable-instance-layout (x val)
  `(setf (%funcallable-instance-info ,x 0) (the layout ,val)))

;;;; character support

;;; In our implementation there are really only BASE-CHARs.
#+nil
(define-source-transform characterp (obj)
  `(base-char-p ,obj))

;;;; simplifying HAIRY-DATA-VECTOR-REF and HAIRY-DATA-VECTOR-SET

(deftransform hairy-data-vector-ref ((string index) (simple-string t))
  (let ((ctype (lvar-type string)))
    (if (array-type-p ctype)
        ;; the other transform will kick in, so that's OK
        (give-up-ir1-transform)
        `(etypecase string
          ((simple-array character (*)) (data-vector-ref string index))
          #!+sb-unicode
          ((simple-array base-char (*)) (data-vector-ref string index))
          ((simple-array nil (*)) (data-vector-ref string index))))))

(deftransform hairy-data-vector-ref ((array index) (array t) *)
  "avoid runtime dispatch on array element type"
  (let ((element-ctype (extract-upgraded-element-type array))
        (declared-element-ctype (extract-declared-element-type array)))
    (declare (type ctype element-ctype))
    (when (eq *wild-type* element-ctype)
      (give-up-ir1-transform
       "Upgraded element type of array is not known at compile time."))
    ;; (The expansion here is basically a degenerate case of
    ;; WITH-ARRAY-DATA. Since WITH-ARRAY-DATA is implemented as a
    ;; macro, and macros aren't expanded in transform output, we have
    ;; to hand-expand it ourselves.)
    (let ((element-type-specifier (type-specifier element-ctype)))
      `(multiple-value-bind (array index)
           (%data-vector-and-index array index)
         (declare (type (simple-array ,element-type-specifier 1) array))
         ,(let ((bare-form '(data-vector-ref array index)))
            (if (type= element-ctype declared-element-ctype)
                bare-form
                `(the ,(type-specifier declared-element-ctype)
                      ,bare-form)))))))

;;; Transform multi-dimensional array to one dimensional data vector
;;; access.
(deftransform data-vector-ref ((array index)
                               (simple-array t))
  (let ((array-type (lvar-type array)))
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (when (or (atom dims) (= (length dims) 1))
        (give-up-ir1-transform))
      (let ((el-type (array-type-specialized-element-type array-type))
            (total-size (if (member '* dims)
                            '*
                            (reduce #'* dims))))
        `(data-vector-ref (truly-the (simple-array ,(type-specifier el-type)
                                                   (,total-size))
                                     (%array-data-vector array))
                          index)))))

(deftransform hairy-data-vector-set ((string index new-value)
                                     (simple-string t t))
  (let ((ctype (lvar-type string)))
    (if (array-type-p ctype)
        ;; the other transform will kick in, so that's OK
        (give-up-ir1-transform)
        `(etypecase string
          ((simple-array character (*))
           (data-vector-set string index new-value))
          #!+sb-unicode
          ((simple-array base-char (*))
           (data-vector-set string index new-value))
          ((simple-array nil (*))
           (data-vector-set string index new-value))))))

(deftransform hairy-data-vector-set ((array index new-value)
                                     (array t t)
                                     *)
  "avoid runtime dispatch on array element type"
  (let ((element-ctype (extract-upgraded-element-type array))
        (declared-element-ctype (extract-declared-element-type array)))
    (declare (type ctype element-ctype))
    (when (eq *wild-type* element-ctype)
      (give-up-ir1-transform
       "Upgraded element type of array is not known at compile time."))
    (let ((element-type-specifier (type-specifier element-ctype)))
      `(multiple-value-bind (array index)
           (%data-vector-and-index array index)
         (declare (type (simple-array ,element-type-specifier 1) array)
                  (type ,element-type-specifier new-value))
         ,(if (type= element-ctype declared-element-ctype)
              '(data-vector-set array index new-value)
              `(truly-the ,(type-specifier declared-element-ctype)
                 (data-vector-set array index
                  (the ,(type-specifier declared-element-ctype)
                       new-value))))))))

(deftransform data-vector-set ((array index new-value)
                               (simple-array t t))
  (let ((array-type (lvar-type array)))
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (when (or (atom dims) (= (length dims) 1))
        (give-up-ir1-transform))
      (let ((el-type (array-type-specialized-element-type array-type))
            (total-size (if (member '* dims)
                            '*
                            (reduce #'* dims))))
        `(data-vector-set (truly-the (simple-array ,(type-specifier el-type)
                                                   (,total-size))
                                     (%array-data-vector array))
                          index
                          new-value)))))

(defoptimizer (%data-vector-and-index derive-type) ((array index))
  (let ((atype (lvar-type array)))
    (when (array-type-p atype)
      (values-specifier-type
       `(values (simple-array ,(type-specifier
                                (array-type-specialized-element-type atype))
                              (*))
                index)))))

(deftransform %data-vector-and-index ((%array %index)
                                      (simple-array t)
                                      *)
  ;; KLUDGE: why the percent signs?  Well, ARRAY and INDEX are
  ;; respectively exported from the CL and SB!INT packages, which
  ;; means that they're visible to all sorts of things.  If the
  ;; compiler can prove that the call to ARRAY-HEADER-P, below, either
  ;; returns T or NIL, it will delete the irrelevant branch.  However,
  ;; user code might have got here with a variable named CL:ARRAY, and
  ;; quite often compiler code with a variable named SB!INT:INDEX, so
  ;; this can generate code deletion notes for innocuous user code:
  ;; (DEFUN F (ARRAY I) (DECLARE (SIMPLE-VECTOR ARRAY)) (AREF ARRAY I))
  ;; -- CSR, 2003-04-01

  ;; We do this solely for the -OR-GIVE-UP side effect, since we want
  ;; to know that the type can be figured out in the end before we
  ;; proceed, but we don't care yet what the type will turn out to be.
  (upgraded-element-type-specifier-or-give-up %array)

  '(if (array-header-p %array)
       (values (%array-data-vector %array) %index)
       (values %array %index)))

;;; transforms for getting at simple arrays of (UNSIGNED-BYTE N) when (< N 8)
;;;
;;; FIXME: In CMU CL, these were commented out with #+NIL. Why? Should
;;; we fix them or should we delete them? (Perhaps these definitions
;;; predate the various DATA-VECTOR-REF-FOO VOPs which have
;;; (:TRANSLATE DATA-VECTOR-REF), and are redundant now?)
#+nil
(macrolet
    ((frob (type bits)
       (let ((elements-per-word (truncate sb!vm:n-word-bits bits)))
         `(progn
            (deftransform data-vector-ref ((vector index)
                                           (,type *))
              `(multiple-value-bind (word bit)
                   (floor index ,',elements-per-word)
                 (ldb ,(ecase sb!vm:target-byte-order
                         (:little-endian '(byte ,bits (* bit ,bits)))
                         (:big-endian '(byte ,bits (- sb!vm:n-word-bits
                                                      (* (1+ bit) ,bits)))))
                      (%raw-bits vector (+ word sb!vm:vector-data-offset)))))
            (deftransform data-vector-set ((vector index new-value)
                                           (,type * *))
              `(multiple-value-bind (word bit)
                   (floor index ,',elements-per-word)
                 (setf (ldb ,(ecase sb!vm:target-byte-order
                               (:little-endian '(byte ,bits (* bit ,bits)))
                               (:big-endian
                                '(byte ,bits (- sb!vm:n-word-bits
                                                (* (1+ bit) ,bits)))))
                            (%raw-bits vector (+ word sb!vm:vector-data-offset)))
                       new-value)))))))
  (frob simple-bit-vector 1)
  (frob (simple-array (unsigned-byte 2) (*)) 2)
  (frob (simple-array (unsigned-byte 4) (*)) 4))

;;;; BIT-VECTOR hackery

;;; SIMPLE-BIT-VECTOR bit-array operations are transformed to a word
;;; loop that does 32 bits at a time.
;;;
;;; FIXME: This is a lot of repeatedly macroexpanded code. It should
;;; be a function call instead.
(macrolet ((def (bitfun wordfun)
             `(deftransform ,bitfun ((bit-array-1 bit-array-2 result-bit-array)
                                     (simple-bit-vector
                                      simple-bit-vector
                                      simple-bit-vector)
                                     *
                                     :node node :policy (>= speed space))
                `(progn
                   ,@(unless (policy node (zerop safety))
                             '((unless (= (length bit-array-1)
                                          (length bit-array-2)
                                          (length result-bit-array))
                                 (error "Argument and/or result bit arrays are not the same length:~
                         ~%  ~S~%  ~S  ~%  ~S"
                                        bit-array-1
                                        bit-array-2
                                        result-bit-array))))
                  (let ((length (length result-bit-array)))
                    (if (= length 0)
                        ;; We avoid doing anything to 0-length
                        ;; bit-vectors, or rather, the memory that
                        ;; follows them. Other divisible-by-32 cases
                        ;; are handled by the (1- length), below.
                        ;; CSR, 2002-04-24
                        result-bit-array
                        (do ((index sb!vm:vector-data-offset (1+ index))
                             (end-1 (+ sb!vm:vector-data-offset
                                       ;; bit-vectors of length 1-32
                                       ;; need precisely one (SETF
                                       ;; %RAW-BITS), done here in the
                                       ;; epilogue. - CSR, 2002-04-24
                                       (truncate (truly-the index (1- length))
                                                 sb!vm:n-word-bits))))
                            ((>= index end-1)
                             (setf (%raw-bits result-bit-array index)
                                   (,',wordfun (%raw-bits bit-array-1 index)
                                               (%raw-bits bit-array-2 index)))
                             result-bit-array)
                          (declare (optimize (speed 3) (safety 0))
                                   (type index index end-1))
                          (setf (%raw-bits result-bit-array index)
                                (,',wordfun (%raw-bits bit-array-1 index)
                                            (%raw-bits bit-array-2 index))))))))))
 (def bit-and word-logical-and)
 (def bit-ior word-logical-or)
 (def bit-xor word-logical-xor)
 (def bit-eqv word-logical-eqv)
 (def bit-nand word-logical-nand)
 (def bit-nor word-logical-nor)
 (def bit-andc1 word-logical-andc1)
 (def bit-andc2 word-logical-andc2)
 (def bit-orc1 word-logical-orc1)
 (def bit-orc2 word-logical-orc2))

(deftransform bit-not
              ((bit-array result-bit-array)
               (simple-bit-vector simple-bit-vector) *
               :node node :policy (>= speed space))
  `(progn
     ,@(unless (policy node (zerop safety))
         '((unless (= (length bit-array)
                      (length result-bit-array))
             (error "Argument and result bit arrays are not the same length:~
                     ~%  ~S~%  ~S"
                    bit-array result-bit-array))))
    (let ((length (length result-bit-array)))
      (if (= length 0)
          ;; We avoid doing anything to 0-length bit-vectors, or rather,
          ;; the memory that follows them. Other divisible-by
          ;; n-word-bits cases are handled by the (1- length), below.
          ;; CSR, 2002-04-24
          result-bit-array
          (do ((index sb!vm:vector-data-offset (1+ index))
               (end-1 (+ sb!vm:vector-data-offset
                         ;; bit-vectors of length 1 to n-word-bits need
                         ;; precisely one (SETF %RAW-BITS), done here in
                         ;; the epilogue. - CSR, 2002-04-24
                         (truncate (truly-the index (1- length))
                                   sb!vm:n-word-bits))))
              ((>= index end-1)
               (setf (%raw-bits result-bit-array index)
                     (word-logical-not (%raw-bits bit-array index)))
               result-bit-array)
            (declare (optimize (speed 3) (safety 0))
                     (type index index end-1))
            (setf (%raw-bits result-bit-array index)
                  (word-logical-not (%raw-bits bit-array index))))))))

(deftransform bit-vector-= ((x y) (simple-bit-vector simple-bit-vector))
  `(and (= (length x) (length y))
        (let ((length (length x)))
          (or (= length 0)
              (do* ((i sb!vm:vector-data-offset (+ i 1))
                    (end-1 (+ sb!vm:vector-data-offset
                              (floor (1- length) sb!vm:n-word-bits))))
                   ((>= i end-1)
                    (let* ((extra (1+ (mod (1- length) sb!vm:n-word-bits)))
                           (mask (ash #.(1- (ash 1 sb!vm:n-word-bits))
                                      (- extra sb!vm:n-word-bits)))
                           (numx
                            (logand
                             (ash mask
                                  ,(ecase sb!c:*backend-byte-order*
                                     (:little-endian 0)
                                     (:big-endian
                                      '(- sb!vm:n-word-bits extra))))
                             (%raw-bits x i)))
                           (numy
                            (logand
                             (ash mask
                                  ,(ecase sb!c:*backend-byte-order*
                                     (:little-endian 0)
                                     (:big-endian
                                      '(- sb!vm:n-word-bits extra))))
                             (%raw-bits y i))))
                      (declare (type (integer 1 #.sb!vm:n-word-bits) extra)
                               (type sb!vm:word mask numx numy))
                      (= numx numy)))
                (declare (type index i end-1))
                (let ((numx (%raw-bits x i))
                      (numy (%raw-bits y i)))
                  (declare (type sb!vm:word numx numy))
                  (unless (= numx numy)
                    (return nil))))))))

(deftransform count ((item sequence) (bit simple-bit-vector) *
                     :policy (>= speed space))
  `(let ((length (length sequence)))
    (if (zerop length)
        0
        (do ((index sb!vm:vector-data-offset (1+ index))
             (count 0)
             (end-1 (+ sb!vm:vector-data-offset
                       (truncate (truly-the index (1- length))
                                 sb!vm:n-word-bits))))
            ((>= index end-1)
             (let* ((extra (1+ (mod (1- length) sb!vm:n-word-bits)))
                    (mask (ash #.(1- (ash 1 sb!vm:n-word-bits))
                               (- extra sb!vm:n-word-bits)))
                    (bits (logand (ash mask
                                       ,(ecase sb!c:*backend-byte-order*
                                               (:little-endian 0)
                                               (:big-endian
                                                '(- sb!vm:n-word-bits extra))))
                                  (%raw-bits sequence index))))
               (declare (type (integer 1 #.sb!vm:n-word-bits) extra))
               (declare (type sb!vm:word mask bits))
               (incf count (logcount bits))
               ,(if (constant-lvar-p item)
                    (if (zerop (lvar-value item))
                        '(- length count)
                        'count)
                    '(if (zerop item)
                         (- length count)
                         count))))
          (declare (type index index count end-1)
                   (optimize (speed 3) (safety 0)))
          (incf count (logcount (%raw-bits sequence index)))))))

(deftransform fill ((sequence item) (simple-bit-vector bit) *
                    :policy (>= speed space))
  (let ((value (if (constant-lvar-p item)
                   (if (= (lvar-value item) 0)
                       0
                       #.(1- (ash 1 sb!vm:n-word-bits)))
                   `(if (= item 0) 0 #.(1- (ash 1 sb!vm:n-word-bits))))))
    `(let ((length (length sequence))
           (value ,value))
       (if (= length 0)
           sequence
           (do ((index sb!vm:vector-data-offset (1+ index))
                (end-1 (+ sb!vm:vector-data-offset
                          ;; bit-vectors of length 1 to n-word-bits need
                          ;; precisely one (SETF %RAW-BITS), done here
                          ;; in the epilogue. - CSR, 2002-04-24
                          (truncate (truly-the index (1- length))
                                    sb!vm:n-word-bits))))
               ((>= index end-1)
                (setf (%raw-bits sequence index) value)
                sequence)
             (declare (optimize (speed 3) (safety 0))
                      (type index index end-1))
             (setf (%raw-bits sequence index) value))))))

(deftransform fill ((sequence item) (simple-base-string base-char) *
                    :policy (>= speed space))
  (let ((value (if (constant-lvar-p item)
                   (let* ((char (lvar-value item))
                          (code (sb!xc:char-code char))
                          (accum 0))
                     (dotimes (i sb!vm:n-word-bytes accum)
                       (setf accum (logior accum (ash code (* 8 i))))))
                   `(let ((code (sb!xc:char-code item)))
                     (logior ,@(loop for i from 0 below sb!vm:n-word-bytes
                                     collect `(ash code ,(* 8 i))))))))
    `(let ((length (length sequence))
           (value ,value))
      (multiple-value-bind (times rem)
          (truncate length sb!vm:n-word-bytes)
        (do ((index sb!vm:vector-data-offset (1+ index))
             (end (+ times sb!vm:vector-data-offset)))
            ((>= index end)
             (let ((place (* times sb!vm:n-word-bytes)))
               (declare (fixnum place))
               (dotimes (j rem sequence)
                 (declare (index j))
                 (setf (schar sequence (the index (+ place j))) item))))
          (declare (optimize (speed 3) (safety 0))
                   (type index index))
          (setf (%raw-bits sequence index) value))))))

;;;; %BYTE-BLT

;;; FIXME: The old CMU CL code used various COPY-TO/FROM-SYSTEM-AREA
;;; stuff (with all the associated bit-index cruft and overflow
;;; issues) even for byte moves. In SBCL, we're converting to byte
;;; moves as problems are discovered with the old code, and this is
;;; currently (ca. sbcl-0.6.12.30) the main interface for code in
;;; SB!KERNEL and SB!SYS (e.g. i/o code). It's not clear that it's the
;;; ideal interface, though, and it probably deserves some thought.
(deftransform %byte-blt ((src src-start dst dst-start dst-end)
                         ((or (simple-unboxed-array (*)) system-area-pointer)
                          index
                          (or (simple-unboxed-array (*)) system-area-pointer)
                          index
                          index))
  ;; FIXME: CMU CL had a hairier implementation of this (back when it
  ;; was still called (%PRIMITIVE BYTE-BLT). It had the small problem
  ;; that it didn't work for large (>16M) values of SRC-START or
  ;; DST-START. However, it might have been more efficient. In
  ;; particular, I don't really know how much the foreign function
  ;; call costs us here. My guess is that if the overhead is
  ;; acceptable for SQRT and COS, it's acceptable here, but this
  ;; should probably be checked. -- WHN
  '(flet ((sapify (thing)
            (etypecase thing
              (system-area-pointer thing)
              ;; FIXME: The code here rather relies on the simple
              ;; unboxed array here having byte-sized entries. That
              ;; should be asserted explicitly, I just haven't found
              ;; a concise way of doing it. (It would be nice to
              ;; declare it in the DEFKNOWN too.)
              ((simple-unboxed-array (*)) (vector-sap thing)))))
     (declare (inline sapify))
     (without-gcing
      (memmove (sap+ (sapify dst) dst-start)
               (sap+ (sapify src) src-start)
               (- dst-end dst-start)))
     (values)))

;;;; transforms for EQL of floating point values

(deftransform eql ((x y) (single-float single-float))
  '(= (single-float-bits x) (single-float-bits y)))

(deftransform eql ((x y) (double-float double-float))
  '(and (= (double-float-low-bits x) (double-float-low-bits y))
        (= (double-float-high-bits x) (double-float-high-bits y))))


;;;; modular functions
(define-good-modular-fun logand :unsigned)
(define-good-modular-fun logior :unsigned)
;;; FIXME: XOR? ANDC1, ANDC2?  -- CSR, 2003-09-16

(macrolet
    ((def (name class width)
       (let ((type (ecase class
                     (:unsigned 'unsigned-byte)
                     (:signed 'signed-byte))))
         `(progn
            (defknown ,name (integer (integer 0)) (,type ,width)
                      (foldable flushable movable))
            (define-modular-fun-optimizer ash ((integer count) ,class :width width)
              (when (and (<= width ,width)
                         (or (and (constant-lvar-p count)
                                  (plusp (lvar-value count)))
                             (csubtypep (lvar-type count)
                                        (specifier-type '(and unsigned-byte fixnum)))))
                (cut-to-width integer ,class width)
                ',name))
            (setf (gethash ',name (modular-class-versions (find-modular-class ',class)))
                  `(ash ,',width))))))
  ;; This should really be dependent on SB!VM:N-WORD-BITS, but since we
  ;; don't have a true Alpha64 port yet, we'll have to stick to
  ;; SB!VM:N-MACHINE-WORD-BITS for the time being.  --njf, 2004-08-14
  #!+#.(cl:if (cl:= 32 sb!vm:n-machine-word-bits) '(and) '(or))
  (progn
    #!+x86 (def sb!vm::ash-left-smod30 :signed 30)
    (def sb!vm::ash-left-mod32 :unsigned 32))
  #!+#.(cl:if (cl:= 64 sb!vm:n-machine-word-bits) '(and) '(or))
  (progn
    #!+x86-64 (def sb!vm::ash-left-smod61 :signed 61)
    (def sb!vm::ash-left-mod64 :unsigned 64)))


;;;; word-wise logical operations

;;; These transforms assume the presence of modular arithmetic to
;;; generate efficient code.

(define-source-transform word-logical-not (x)
  `(logand (lognot (the sb!vm:word ,x)) #.(1- (ash 1 sb!vm:n-word-bits))))

(deftransform word-logical-and ((x y))
  '(logand x y))

(deftransform word-logical-nand ((x y))
  '(logand (lognand x y) #.(1- (ash 1 sb!vm:n-word-bits))))

(deftransform word-logical-or ((x y))
  '(logior x y))

(deftransform word-logical-nor ((x y))
  '(logand (lognor x y) #.(1- (ash 1 sb!vm:n-word-bits))))

(deftransform word-logical-xor ((x y))
  '(logxor x y))

(deftransform word-logical-eqv ((x y))
  '(logand (logeqv x y) #.(1- (ash 1 sb!vm:n-word-bits))))

(deftransform word-logical-orc1 ((x y))
  '(logand (logorc1 x y) #.(1- (ash 1 sb!vm:n-word-bits))))

(deftransform word-logical-orc2 ((x y))
  '(logand (logorc2 x y) #.(1- (ash 1 sb!vm:n-word-bits))))

(deftransform word-logical-andc1 ((x y))
  '(logand (logandc1 x y) #.(1- (ash 1 sb!vm:n-word-bits))))

(deftransform word-logical-andc2 ((x y))
  '(logand (logandc2 x y) #.(1- (ash 1 sb!vm:n-word-bits))))


;;; There are two different ways the multiplier can be recoded. The
;;; more obvious is to shift X by the correct amount for each bit set
;;; in Y and to sum the results. But if there is a string of bits that
;;; are all set, you can add X shifted by one more then the bit
;;; position of the first set bit and subtract X shifted by the bit
;;; position of the last set bit. We can't use this second method when
;;; the high order bit is bit 31 because shifting by 32 doesn't work
;;; too well.
(defun ub32-strength-reduce-constant-multiply (arg num)
  (declare (type (unsigned-byte 32) num))
  (let ((adds 0) (shifts 0)
        (result nil) first-one)
    (labels ((add (next-factor)
               (setf result
                     (if result
                         (progn (incf adds) `(+ ,result ,next-factor))
                         next-factor))))
      (declare (inline add))
      (dotimes (bitpos 32)
        (if first-one
            (when (not (logbitp bitpos num))
              (add (if (= (1+ first-one) bitpos)
                       ;; There is only a single bit in the string.
                       (progn (incf shifts) `(ash ,arg ,first-one))
                       ;; There are at least two.
                       (progn
                         (incf adds)
                         (incf shifts 2)
                         `(- (ash ,arg ,bitpos)
                             (ash ,arg ,first-one)))))
              (setf first-one nil))
            (when (logbitp bitpos num)
              (setf first-one bitpos))))
      (when first-one
        (cond ((= first-one 31))
              ((= first-one 30) (incf shifts) (add `(ash ,arg 30)))
              (t
               (incf shifts 2)
               (incf adds)
               (add `(- (ash ,arg 31)
                        (ash ,arg ,first-one)))))
        (incf shifts)
        (add `(ash ,arg 31))))
    (values (if (plusp adds)
                `(logand ,result #.(1- (ash 1 32))) ; using modular arithmetic
                result)
            adds
            shifts)))
