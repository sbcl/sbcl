;;;; implementation-dependent transforms

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; We need to define these predicates, since the TYPEP source
;;; transform picks whichever predicate was defined last when there
;;; are multiple predicates for equivalent types.
(define-source-transform short-float-p (x) `(single-float-p ,x))
#-long-float
(define-source-transform long-float-p (x) `(double-float-p ,x))

(define-source-transform compiled-function-p (x)
  (once-only ((x x))
    `(and (functionp ,x)
          #+(or sb-fasteval sb-eval)
          (not (typep ,x 'interpreted-function)))))

(define-source-transform char-int (x)
  `(char-code ,x))

(deftransform abs ((x) (rational))
  '(if (< x 0) (- x) x))

(deftransform make-symbol ((string) (simple-string))
  `(%make-symbol 0 string))

;;; We don't want to clutter the bignum code.
#+(and (or x86 x86-64) (not bignum-assertions))
(define-source-transform sb-bignum:%bignum-ref (bignum index)
  ;; KLUDGE: We use TRULY-THE here because even though the bignum code
  ;; is (currently) compiled with (SAFETY 0), the compiler insists on
  ;; inserting CAST nodes to ensure that INDEX is of the correct type.
  ;; These CAST nodes do not generate any type checks, but they do
  ;; interfere with the operation of FOLD-INDEX-ADDRESSING, below.
  ;; This scenario is a problem for the more user-visible case of
  ;; folding as well.  --njf, 2006-12-01
  `(sb-bignum:%bignum-ref-with-offset ,bignum
                                      (truly-the bignum-index ,index) 0))

#+(or x86 x86-64)
(defun fold-index-addressing (fun-name element-size lowtag data-offset
                              index offset &optional setter-p)
  ;; UNINITIALIZED-ELEMENT-ERROR does not take the ADDEND,
  ;; so we need to make sure that it's always 0 for #+ubsan.
  ;; #-ubsan has a minor problem in terms of error-reporting.
  ;; The condition report method does not try to show the index of the
  ;; trapping element, so we're at least not wrong. A good solution might be
  ;; to have separate 2-arg and 3-arg variants of UNINITIALIZED-ELEMENT,
  ;; or else always emit the addend even if zero. As things are, a 0 in the
  ;; error payload eats 4 bytes due to design choices in sc+offset encoding.
  #+ubsan (give-up-ir1-transform)
  (multiple-value-bind (func index-args) (extract-fun-args index '(+ -) 2)
    (destructuring-bind (x constant) index-args
      (unless (and (constant-lvar-p constant)
                   ;; we lose if the remaining argument isn't a fixnum
                   (csubtypep (lvar-type x) (specifier-type 'fixnum)))
        (give-up-ir1-transform))
      (let ((value (lvar-value constant))
            new-offset)
        (unless (and (integerp value)
                     (sb-vm::foldable-constant-offset-p
                      element-size lowtag data-offset
                      (setf new-offset (funcall func (lvar-value offset)
                                                value))))
          (give-up-ir1-transform "constant is too large for inlining"))
        (splice-fun-args index func 2)
        `(lambda (thing index off1 off2 ,@(when setter-p
                                            '(value)))
           (declare (ignore off1 off2))
           (,fun-name thing index ',new-offset ,@(when setter-p
                                                   '(value))))))))

#+(or x86 x86-64)
(deftransform sb-bignum:%bignum-ref-with-offset
    ((bignum index offset) * * :node node)
  (fold-index-addressing 'sb-bignum:%bignum-ref-with-offset
                         sb-vm:n-word-bits sb-vm:other-pointer-lowtag
                         sb-vm:bignum-digits-offset
                         index offset))

(defun dd-contains-raw-slots-p (dd)
  (dolist (dsd (dd-slots dd))
    (unless (eq (dsd-raw-type dsd) t) (return t))))

(deftransform copy-structure ((instance) * * :result result :node node)
  (let* ((classoid (lvar-type instance))
         (name (and (structure-classoid-p classoid) (classoid-name classoid)))
         (layout (and name
                      (sb-kernel::compiler-layout-ready-p name)
                      (sb-kernel::compiler-layout-or-lose name)))
         ;; CLASS-EQ is T if the layout at runtime will be EQ to the
         ;; layout of the specified type, and not that of a subtype thereof.
         (class-eq (and name
                        (eq (classoid-state classoid) :sealed)
                        (not (classoid-subclasses classoid))))
         (dd (and class-eq (wrapper-info layout)))
         (max-inlined-words 5))
    (unless (and result ; could be unused result (but entire call wasn't flushed?)
                 layout
                 ;; And don't copy if raw slots are present on the precisely GCed backends.
                 ;; To enable that, we'd want variants for {"all-raw", "all-boxed", "mixed"}
                 ;; Also note that VAR-ALLOC can not cope with dynamic-extent except where
                 ;; support has been added (x86oid so far); so requiring an exact type here
                 ;; causes VAR-ALLOC to become FIXED-ALLOC which works on more architectures.
                 #-c-stack-is-control-stack (and dd (not (dd-contains-raw-slots-p dd)))

                 ;; Definitely do this if copying to stack
                 (or (lvar-dynamic-extent result)
                     ;; Or if it's a small fixed number of words
                     ;; and speed at least as important as size.
                     (and class-eq
                          (<= (dd-length dd) max-inlined-words)
                          (policy node (>= speed space)))))
      (give-up-ir1-transform))
    ;; There are some benefits to using the simple case for a known exact type:
    ;; - the layout can be wired in which might or might not save 1 instruction
    ;;   depending on whether layouts are in immobile space.
    ;; - for a small number of slots, copying them is inlined
    (cond ((not dd) ; it's going to be some subtype of NAME
           `(%copy-instance (%make-instance (%instance-length instance)) instance))
          ((<= (dd-length dd) max-inlined-words)
           `(let ((copy (%make-structure-instance ,dd nil)))
              ;; ASSUMPTION: either %INSTANCE-REF is the correct accessor for this word,
              ;; or the GC will treat random bit patterns as conservative pointers
              ;; (i.e. not alter them if %INSTANCE-REF is not the correct accessor)
              ,@(loop for i from sb-vm:instance-data-start below (dd-length dd)
                      collect `(%instance-set copy ,i (%instance-ref instance ,i)))
              copy))
          (t
           `(%copy-instance-slots (%make-structure-instance ,dd nil) instance)))))

(defun varying-length-struct-p (classoid)
  ;; This is a nice feature to have in general, but at present it is only possible
  ;; to make varying length instances of SB-VM:LAYOUT (or WRAPPER if that is the same type),
  ;; and nothing else.
  (eq classoid (load-time-value (find-classoid #+metaspace 'sb-vm:layout
                                               #-metaspace 'wrapper))))

(deftransform %instance-length ((instance))
  (let ((classoid (lvar-type instance)))
    (if (and (structure-classoid-p classoid)
             (sb-kernel::compiler-layout-ready-p (classoid-name classoid))
             (eq (classoid-state classoid) :sealed)
             (not (varying-length-struct-p classoid))
             ;; TODO: if sealed with subclasses which add no slots, use the fixed length
             (not (classoid-subclasses classoid)))
        (dd-length (wrapper-dd (sb-kernel::compiler-layout-or-lose (classoid-name classoid))))
        (give-up-ir1-transform))))

(define-source-transform %instance-wrapper (x) `(layout-friend (%instance-layout ,x)))
(define-source-transform %fun-wrapper (x) `(layout-friend (%fun-layout ,x)))

;;; *** These transforms should be the only code, aside from the C runtime
;;;     with knowledge of the layout index.
#+compact-instance-header
(define-source-transform function-with-layout-p (x) `(functionp ,x))
#-compact-instance-header
(progn
  (define-source-transform %instance-layout (x)
    `(truly-the sb-vm:layout (%instance-ref ,x 0)))
  (define-source-transform %set-instance-layout (instance layout)
    `(%instance-set ,instance 0 (the sb-vm:layout ,layout)))
  (define-source-transform function-with-layout-p (x)
    `(funcallable-instance-p ,x)))

;;;; simplifying HAIRY-DATA-VECTOR-REF and HAIRY-DATA-VECTOR-SET

(deftransform hairy-data-vector-ref ((string index) (simple-string t))
  (let ((ctype (lvar-type string)))
    (if (array-type-p ctype)
        ;; the other transform will kick in, so that's OK
        (give-up-ir1-transform)
        `(etypecase string
          ((simple-array character (*))
           (data-vector-ref string index))
          #+sb-unicode
          ((simple-array base-char (*))
           (data-vector-ref string index))))))

;;; This and the corresponding -SET transform work equally well on non-simple
;;; arrays, but after benchmarking (on x86), Nikodemus didn't find any cases
;;; where it actually helped with non-simple arrays -- to the contrary, it
;;; only made for bigger and up to 100% slower code.
(deftransform hairy-data-vector-ref ((array index) (simple-array t) *)
  "avoid runtime dispatch on array element type"
  (let* ((type (lvar-type array))
         (element-ctype (array-type-upgraded-element-type type))
         (declared-element-ctype (declared-array-element-type type)))
    (declare (type ctype element-ctype))
    (when (eq *wild-type* element-ctype)
      (give-up-ir1-transform
       "Upgraded element type of array is not known at compile time."))
    ;; (The expansion here is basically a degenerate case of
    ;; WITH-ARRAY-DATA. Since WITH-ARRAY-DATA is implemented as a
    ;; macro, and macros aren't expanded in transform output, we have
    ;; to hand-expand it ourselves.)
    (let* ((element-type-specifier (type-specifier element-ctype)))
      `(multiple-value-bind (array index)
           (%data-vector-and-index array index)
         (declare (type (simple-array ,element-type-specifier 1) array))
         ,(let ((bare-form '(data-vector-ref array index)))
            (cond ((eql element-ctype *empty-type*)
                   `(data-nil-vector-ref array index))
                  ((type= element-ctype declared-element-ctype)
                   bare-form)
                  (t
                   (the-unwild declared-element-ctype bare-form))))))))

;;; Transform multi-dimensional array to one dimensional data vector
;;; access.
(deftransform data-vector-ref ((array index) (simple-array t))
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
                                     (%array-data array))
                          index)))))

;;; Transform data vector access to a form that opens up optimization
;;; opportunities. On platforms that support DATA-VECTOR-REF-WITH-OFFSET
;;; DATA-VECTOR-REF is not supported at all.
#+(or x86 x86-64)
(define-source-transform data-vector-ref (array index)
  `(data-vector-ref-with-offset ,array ,index 0))

#+(or x86 x86-64)
(deftransform data-vector-ref-with-offset ((array index offset))
  (let ((array-type (lvar-type array)))
    (when (or (not (array-type-p array-type))
              (eql (array-type-specialized-element-type array-type)
                   *wild-type*))
      (give-up-ir1-transform))
    ;; It shouldn't be possible to get here with anything but a non-complex
    ;; vector.
    (aver (not (array-type-complexp array-type)))
    (let* ((element-type (type-specifier (array-type-specialized-element-type array-type)))
           (saetp (find-saetp element-type)))
      (when (< (sb-vm:saetp-n-bits saetp) sb-vm:n-byte-bits)
        (give-up-ir1-transform))
      (fold-index-addressing 'data-vector-ref-with-offset
                             (sb-vm:saetp-n-bits saetp)
                             sb-vm:other-pointer-lowtag
                             sb-vm:vector-data-offset
                             index offset))))

(deftransform hairy-data-vector-set ((string index new-value)
                                     (simple-string t t))
  (let ((ctype (lvar-type string)))
    (if (array-type-p ctype)
        ;; the other transform will kick in, so that's OK
        (give-up-ir1-transform)
        ;; HAIRY-DATA-VECTOR-SET returns a value but DATA-VECTOR-SET does not,
        ;; so explicitly return the NEW-VALUE
        `(typecase string
           ((simple-array character (*))
            (let ((c (the* (character :context :aref) new-value)))
              (data-vector-set string index c)
              c))
           #+sb-unicode
           ((simple-array base-char (*))
            (let ((c (the* (base-char :context :aref :silent-conflict t) new-value)))
              (data-vector-set string index c)
              c))))))

;;; This and the corresponding -REF transform work equally well on non-simple
;;; arrays, but after benchmarking (on x86), Nikodemus didn't find any cases
;;; where it actually helped with non-simple arrays -- to the contrary, it
;;; only made for bigger and up 1o 100% slower code.
(deftransform hairy-data-vector-set ((array index new-value)
                                     (simple-array t t)
                                     *)
  "avoid runtime dispatch on array element type"
  (let* ((type (lvar-type array))
         (element-ctype (array-type-upgraded-element-type type))
         (declared-element-ctype (declared-array-element-type type)))
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
              '(progn (data-vector-set array index new-value)
                      new-value)
              `(progn (data-vector-set array index
                       ,(the-unwild declared-element-ctype 'new-value))
                      ,(truly-the-unwild declared-element-ctype 'new-value)))))))

;;; Transform multi-dimensional array to one dimensional data vector
;;; access.
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
                                     (%array-data array))
                          index
                          new-value)))))

;;; Transform data vector access to a form that opens up optimization
;;; opportunities.
#+(or x86 x86-64)
(define-source-transform data-vector-set (array index new-value)
  `(data-vector-set-with-offset ,array ,index 0 ,new-value))

#+(or x86 x86-64)
(deftransform data-vector-set-with-offset ((array index offset new-value))
  (let ((array-type (lvar-type array)))
    (when (or (not (array-type-p array-type))
              (eql (array-type-specialized-element-type array-type)
                   *wild-type*))
      ;; We don't yet know the exact element type, but will get that
      ;; knowledge after some more type propagation.
      (give-up-ir1-transform))
    (aver (not (array-type-complexp array-type)))
    (let* ((element-type (type-specifier (array-type-specialized-element-type array-type)))
           (saetp (find-saetp element-type)))
      (when (< (sb-vm:saetp-n-bits saetp) sb-vm:n-byte-bits)
        (give-up-ir1-transform))
      (fold-index-addressing 'data-vector-set-with-offset
                             (sb-vm:saetp-n-bits saetp)
                             sb-vm:other-pointer-lowtag
                             sb-vm:vector-data-offset
                             index offset t))))

(defun simple-array-storage-vector-type (type)
  (let ((dims (array-type-dimensions type)))
    (cond ((array-type-complexp type)
           nil)
          (t
           `(simple-array ,(type-specifier
                            (array-type-specialized-element-type type))
                          (,(if (and (listp dims)
                                     (every #'integerp dims))
                                (reduce #'* dims)
                                '*)))))))

(defoptimizer (array-storage-vector derive-type) ((array))
  (let ((atype (lvar-type array)))
    (when (array-type-p atype)
      (specifier-type (or (simple-array-storage-vector-type atype)
                          `(simple-array ,(type-specifier
                                           (array-type-specialized-element-type atype))
                                         (*)))))))

(deftransform array-storage-vector ((array) ((simple-array * (*))))
  'array)

(defoptimizer (%array-data derive-type) ((array))
  (let ((atype (lvar-type array)))
    (when (array-type-p atype)
      (specifier-type (or
                       (simple-array-storage-vector-type atype)
                       `(array ,(type-specifier
                                 (array-type-specialized-element-type atype))))))))

(defoptimizer (%data-vector-and-index derive-type) ((array index))
  (let ((atype (lvar-type array))
        (index-type (lvar-type index)))
    (when (array-type-p atype)
      (values-specifier-type
       `(values ,(or
                  (simple-array-storage-vector-type atype)
                  `(simple-array ,(type-specifier
                                   (array-type-specialized-element-type atype))
                                 (*)))
                ,(if (and (integer-type-p index-type)
                          (numeric-type-low index-type))
                     `(integer ,(numeric-type-low index-type)
                               (,array-dimension-limit))
                     `index))))))

(deftransform %data-vector-and-index ((%array %index)
                                      (simple-array t)
                                      *)
  ;; KLUDGE: why the percent signs?  Well, ARRAY and INDEX are
  ;; respectively exported from the CL and SB-INT packages, which
  ;; means that they're visible to all sorts of things.  If the
  ;; compiler can prove that the call to ARRAY-HEADER-P, below, either
  ;; returns T or NIL, it will delete the irrelevant branch.  However,
  ;; user code might have got here with a variable named CL:ARRAY, and
  ;; quite often compiler code with a variable named SB-INT:INDEX, so
  ;; this can generate code deletion notes for innocuous user code:
  ;; (DEFUN F (ARRAY I) (DECLARE (SIMPLE-VECTOR ARRAY)) (AREF ARRAY I))
  ;; -- CSR, 2003-04-01

  ;; We do this solely for the -OR-GIVE-UP side effect, since we want
  ;; to know that the type can be figured out in the end before we
  ;; proceed, but we don't care yet what the type will turn out to be.
  (upgraded-element-type-specifier-or-give-up %array)

  '(if (array-header-p %array)
       (values (%array-data %array) %index)
       (values %array %index)))

;;;; BIT-VECTOR hackery

;;; SIMPLE-BIT-VECTOR bit-array operations are transformed to a word
;;; loop that does N-WORD-BITS bits at a time.
;;; Note that all of these array operations cause the unused bits
;;; of the last word to be operated on, so they are effectively
;;; in an indeterminate state which is why equality testing, COUNT,
;;; and FIND have to ignore them.
(deftransform bit-op->word-op ((bit-array-1 bit-array-2 result-bit-array)
                               (simple-bit-vector simple-bit-vector simple-bit-vector)
                               *
                               :node node :defun-only t :info wordfun)
  `(let ((length (vector-length result-bit-array)))
     ,@(unless (policy node (zerop safety))
         `((unless (= length
                      ,@(unless (same-leaf-ref-p bit-array-1 result-bit-array)
                          '((vector-length bit-array-1)))
                      (vector-length bit-array-2))
             (error "Argument and/or result bit arrays are not the same length:~
                         ~%  ~S~%  ~S  ~%  ~S"
                    bit-array-1 bit-array-2 result-bit-array))))
     (dotimes (index (ceiling length sb-vm:n-word-bits))
       (declare (optimize (speed 3) (safety 0)) (type index index))
       (setf (%vector-raw-bits result-bit-array index)
             (,wordfun (%vector-raw-bits bit-array-1 index)
                       (%vector-raw-bits bit-array-2 index))))
     result-bit-array))

(flet ((policy-test (node) (policy node (>= speed space))))
(macrolet ((def (bitfun wordfun)
             `(%deftransform ',bitfun #'policy-test
                             '(function (simple-bit-vector simple-bit-vector simple-bit-vector)
                                        *)
                             (cons #'bit-op->word-op ',wordfun))))
 (def bit-and word-logical-and)
 (def bit-ior word-logical-or)
 (def bit-xor word-logical-xor)
 (def bit-eqv word-logical-eqv)
 (def bit-nand word-logical-nand)
 (def bit-nor word-logical-nor)
 (def bit-andc1 word-logical-andc1)
 (def bit-andc2 word-logical-andc2)
 (def bit-orc1 word-logical-orc1)
 (def bit-orc2 word-logical-orc2)))

(deftransform bit-not
              ((bit-array result-bit-array)
               (simple-bit-vector simple-bit-vector) *
               :node node :policy (>= speed space))
  `(progn
     ,@(unless (or (policy node (zerop safety))
                   (same-leaf-ref-p bit-array result-bit-array))
         '((unless (= (vector-length bit-array)
                      (vector-length result-bit-array))
             (error "Argument and result bit arrays are not the same length:~
                     ~%  ~S~%  ~S"
                    bit-array result-bit-array))))
    (let ((length (vector-length result-bit-array)))
      (dotimes (index (ceiling length sb-vm:n-word-bits))
        (declare (optimize (speed 3) (safety 0)) (type index index))
        (setf (%vector-raw-bits result-bit-array index)
              (word-logical-not (%vector-raw-bits bit-array index))))
      result-bit-array)))

;;; This transform has to deal with the fact that unused bits
;;; in the last data word of a simple-bit-vector can be random.
(deftransform bit-vector-= ((x y) (simple-bit-vector simple-bit-vector))
  ;; TODO: unroll if length is known and not more than a few words
  `(let ((length (vector-length x)))
     (and (= (vector-length y) length)
          (let ((words (floor length sb-vm:n-word-bits)))
            (and (dotimes (i words t)
                   (unless (= (%vector-raw-bits x i) (%vector-raw-bits y i))
                     (return nil)))
                 (let ((remainder (mod length sb-vm:n-word-bits)))
                   (or (zerop remainder)
                       ;; - To examine 1 bit, shift over 63 bits, 0-filling on the other side
                       ;; - To examine 2 bits, shift over 62 bits, etc
                       ;; SHIFT-TOWARDS-END accepts a negative shift COUNT which is
                       ;; congruent to desired shift modulo the maximum shift.
                       (zerop (shift-towards-end (logxor (%vector-raw-bits x words)
                                                         (%vector-raw-bits y words))
                                                 (- remainder))))))))))

;;; This transform has to deal with the fact that unused bits
;;; in the last data word of a simple-bit-vector can be random.
(deftransform count ((item sequence) (bit simple-bit-vector) *
                     :policy (>= speed space))
  `(let* ((length (vector-length sequence))
          (count 0)
          (words (floor length sb-vm:n-word-bits)))
     (declare (index count))
     (declare (optimize (speed 3) (safety 0)))
     (dotimes (i words)
       (incf count (logcount (%vector-raw-bits sequence i))))
     (let ((remainder (mod length sb-vm:n-word-bits)))
       (unless (zerop remainder)
         (incf count (logcount (shift-towards-end (%vector-raw-bits sequence words)
                                                  (- sb-vm:n-word-bits remainder))))))
     ,(if (constant-lvar-p item)
          (if (zerop (lvar-value item)) '(- length count) 'count)
          '(if (zerop item) (- length count) count))))

;;; This transform does not require that ITEM be derived as BIT,
;;; but at runtime it has to be.
(deftransform fill ((sequence item) (simple-bit-vector t) *
                    :policy (>= speed space))
  `(let ((value (logand (- (the bit item)) most-positive-word)))
     ;; Unlike for the SIMPLE-BASE-STRING case, we are allowed to touch
     ;; bits beyond LENGTH with impunity.
     (dotimes (index (ceiling (vector-length sequence) sb-vm:n-word-bits))
       (declare (optimize (speed 3) (safety 0))
                (type index index))
       (setf (%vector-raw-bits sequence index) value))
     sequence))

(deftransform fill ((sequence item) (simple-base-string t) *
                                    :policy (>= speed space))
  (let ((multiplier (logand #x0101010101010101 most-positive-word)))
    `(let* ((value ,(if (and (constant-lvar-p item) (typep item 'base-char))
                        (* multiplier (char-code (lvar-value item)))
                        ;; Use multiplication if it's known to be cheap
                        #+(or x86 x86-64)
                        `(* ,multiplier (char-code (the base-char item)))
                        #-(or x86 x86-64)
                        '(let ((code (char-code (the base-char item))))
                          (setf code (dpb code (byte 8 8) code))
                          (setf code (dpb code (byte 16 16) code))
                          #+64-bit (dpb code (byte 32 32) code))))
            (len (vector-length sequence))
            (words (truncate len sb-vm:n-word-bytes)))
       (dotimes (index words)
         (declare (optimize (speed 3) (safety 0))
                  (type index index))
         (setf (%vector-raw-bits sequence index) value))
       ;; For 64-bit:
       ;;  if 1 more byte should be written, then shift-towards-start 56
       ;;  if 2 more bytes ...               then shift-towards-start 48
       ;;  etc
       ;; This correctly rewrites the trailing null in its proper place.
       (let ((bits (ash (mod len sb-vm:n-word-bytes) 3)))
         (when (plusp bits)
           (setf (%vector-raw-bits sequence words)
                 (shift-towards-start value (- bits)))))
       sequence)))

;;;; %BYTE-BLT

;;; FIXME: The old CMU CL code used various COPY-TO/FROM-SYSTEM-AREA
;;; stuff (with all the associated bit-index cruft and overflow
;;; issues) even for byte moves. In SBCL, we're converting to byte
;;; moves as problems are discovered with the old code, and this is
;;; currently (ca. sbcl-0.6.12.30) the main interface for code in
;;; SB-KERNEL and SB-SYS (e.g. i/o code). It's not clear that it's the
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
    (with-pinned-objects (dst src)
      ;; Prevent failure caused by memmove() hitting a write-protected page
      ;; and the fault handler losing, since it thinks you're not in Lisp.
      ;; This is wasteful, but better than being randomly broken (lp#1366263).
      #+cheneygc
      (let ((dst (sapify dst)))
        (setf (sap-ref-8 dst dst-start) (sap-ref-8 dst dst-start)
              (sap-ref-8 dst (1- dst-end)) (sap-ref-8 dst (1- dst-end))))
      (memmove (sap+ (sapify dst) dst-start)
               (sap+ (sapify src) src-start)
               (- dst-end dst-start)))
     (values)))

;;;; transforms for EQL of floating point values
(unless (vop-existsp :named sb-vm::eql/single-float)
(deftransform eql ((x y) (single-float single-float))
  '(= (single-float-bits x) (single-float-bits y))))

(unless (vop-existsp :named sb-vm::eql/double-float)
(deftransform eql ((x y) (double-float double-float))
  #-64-bit '(and (= (double-float-low-bits x) (double-float-low-bits y))
                  (= (double-float-high-bits x) (double-float-high-bits y)))
  #+64-bit '(= (double-float-bits x) (double-float-bits y))))


;;;; modular functions
;;;
;;; FIXME: I think that the :GOODness of a modular function boils down
;;; to whether the normal definition can be used in the middle of a
;;; modular arrangement.  LOGAND and LOGIOR can be for all unsigned
;;; modular implementations, I believe, because for all unsigned
;;; arguments of a given size the result of the ordinary definition is
;;; the right one.  This should follow through to other logical
;;; functions, such as LOGXOR, should it not?  -- CSR, 2007-12-29,
;;; trying to understand a comment he wrote over four years
;;; previously: "FIXME: XOR? ANDC1, ANDC2?  -- CSR, 2003-09-16"
(define-good-modular-fun logand :untagged nil)
(define-good-modular-fun logior :untagged nil)
(define-good-modular-fun logxor :untagged nil)
(macrolet ((define-good-signed-modular-funs (&rest funs)
             (let (result)
               `(progn
                 ,@(dolist (fun funs (nreverse result))
                     (push `(define-good-modular-fun ,fun :untagged t) result)
                     (push `(define-good-modular-fun ,fun :tagged t) result))))))
  (define-good-signed-modular-funs
      logand logandc1 logandc2 logeqv logior lognand lognor lognot
      logorc1 logorc2 logxor))

;;;; word-wise logical operations

;;; These transforms assume the presence of modular arithmetic to
;;; generate efficient code.

(define-source-transform word-logical-not (x)
  `(logand (lognot (the sb-vm:word ,x)) ,most-positive-word))

(deftransform word-logical-and ((x y))
  '(logand x y))

(deftransform word-logical-nand ((x y))
  `(logand (lognand x y) ,most-positive-word))

(deftransform word-logical-or ((x y))
  '(logior x y))

(deftransform word-logical-nor ((x y))
  `(logand (lognor x y) ,most-positive-word))

(deftransform word-logical-xor ((x y))
  '(logxor x y))

(deftransform word-logical-eqv ((x y))
  `(logand (logeqv x y) ,most-positive-word))

(deftransform word-logical-orc1 ((x y))
  `(logand (logorc1 x y) ,most-positive-word))

(deftransform word-logical-orc2 ((x y))
  `(logand (logorc2 x y) ,most-positive-word))

(deftransform word-logical-andc1 ((x y))
  `(logand (logandc1 x y) ,most-positive-word))

(deftransform word-logical-andc2 ((x y))
  `(logand (logandc2 x y) ,most-positive-word))


;;; There are two different ways the multiplier can be recoded. The
;;; more obvious is to shift X by the correct amount for each bit set
;;; in Y and to sum the results. But if there is a string of bits that
;;; are all set, you can add X shifted by one more then the bit
;;; position of the first set bit and subtract X shifted by the bit
;;; position of the last set bit. We can't use this second method when
;;; the high order bit is bit 31 because shifting by 32 doesn't work
;;; too well.
;;; This is used only for ppc + sparc.
;;; FIXME: ppc64 should have a UB64-STRENGTH-REDUCE-
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


;;; Transform GET-LISP-OBJ-ADDRESS for constant immediates, since the normal
;;; VOP can't handle them.

(deftransform sb-vm::get-lisp-obj-address ((obj) ((constant-arg fixnum)))
  (ash (lvar-value obj) sb-vm:n-fixnum-tag-bits))

(deftransform sb-vm::get-lisp-obj-address ((obj) ((constant-arg character)))
  (logior sb-vm:character-widetag
          (ash (char-code (lvar-value obj)) sb-vm:n-widetag-bits)))

;; So that the PCL code walker doesn't observe any use of %PRIMITIVE,
;; MAKE-UNBOUND-MARKER is an ordinary function, not a macro.
#-sb-xc-host
(defun make-unbound-marker () ; for interpreters
  (sb-sys:%primitive make-unbound-marker))
;; Get the main compiler to transform MAKE-UNBOUND-MARKER
;; without the fopcompiler seeing it - the fopcompiler does
;; expand compiler-macros, but not source-transforms -
;; because %PRIMITIVE is not generally fopcompilable.
(sb-c:define-source-transform make-unbound-marker ()
  `(sb-sys:%primitive make-unbound-marker))

(deftransform (cas symbol-value) ((old new symbol))
  (let ((cname (and (constant-lvar-p symbol) (lvar-value symbol))))
    (case (and cname (info :variable :kind cname))
      ((:special :global)
       (let ((type (info :variable :type cname)))
         `(truly-the ,type
                     (%compare-and-swap-symbol-value ',cname old (the ,type new)))))
      (t
       `(progn
          (about-to-modify-symbol-value symbol 'compare-and-swap new)
          (%compare-and-swap-symbol-value symbol old new))))))

(deftransform (cas svref) ((old new vector index))
  '(let ((v (the simple-vector vector)))
    (%compare-and-swap-svref v (check-bound v (length v) index) old new)))
