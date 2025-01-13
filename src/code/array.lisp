;;;; functions to implement arrays

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(declaim (inline adjustable-array-p
                 array-displacement))

;;;; miscellaneous accessor functions

;;; These functions are only needed by the interpreter, 'cause the
;;; compiler inlines them.
(macrolet ((def (name)
             `(progn
                (defun ,name (array)
                  (,name array))
                (defun (setf ,name) (value array)
                  (setf (,name array) value)))))
  (def %array-fill-pointer)
  (def %array-available-elements)
  (def %array-data)
  (def %array-displacement)
  (def %array-displaced-p)
  (def %array-displaced-from))

(defun %array-rank (array)
  (%array-rank array))

(defun %array-dimension (array axis)
  (%array-dimension array axis))

(defun %check-bound (array bound index)
  (declare (type index bound)
           (fixnum index))
  (%check-bound array bound index))

(defun check-bound (array bound index)
  (declare (type index bound)
           (fixnum index))
  (%check-bound array bound index)
  index)

(defun %with-array-data/fp (array start end)
  (%with-array-data-macro array start end :check-bounds t :check-fill-pointer t))

(defun %with-array-data (array start end)
  (%with-array-data-macro array start end :check-bounds t :array-header-p t))

(defun %data-vector-and-index (array index)
  (if (array-header-p array)
      (multiple-value-bind (vector index)
          (%with-array-data array index nil)
        (values vector index))
      (values (truly-the (simple-array * (*)) array) index)))

(defun sb-c::%data-vector-and-index/check-bound (array index)
  (%check-bound array (array-dimension array 0) index)
  (%data-vector-and-index array index))


;;;; MAKE-ARRAY
(defun %integer-vector-widetag-and-n-bits-shift (signed high)
  (let ((unsigned-table
          #.(let ((map (make-array (1+ n-word-bits))))
              (loop for saetp across
                    (reverse *specialized-array-element-type-properties*)
                    for ctype = (saetp-ctype saetp)
                    when (and (numeric-type-p ctype)
                              (eq (numeric-type-class ctype) 'integer)
                              (zerop (numeric-type-low ctype)))
                    do (fill map (cons (saetp-typecode saetp)
                                       (saetp-n-bits-shift saetp))
                             :end (1+ (integer-length (numeric-type-high ctype)))))
              map))
        (signed-table
          #.(let ((map (make-array (1+ n-word-bits))))
              (loop for saetp across
                    (reverse *specialized-array-element-type-properties*)
                    for ctype = (saetp-ctype saetp)
                    when (and (numeric-type-p ctype)
                              (eq (numeric-type-class ctype) 'integer)
                              (minusp (numeric-type-low ctype)))
                    do (fill map (cons (saetp-typecode saetp)
                                       (saetp-n-bits-shift saetp))
                             :end (+ (integer-length (numeric-type-high ctype)) 2)))
              map)))
    (cond ((> high n-word-bits)
           (values #.simple-vector-widetag
                   #.(1- (integer-length n-word-bits))))
          (signed
           (let ((x (aref signed-table high)))
             (values (car x) (cdr x))))
          (t
           (let ((x (aref unsigned-table high)))
             (values (car x) (cdr x)))))))

;;; This is a bit complicated, but calling subtypep over all
;;; specialized types is exceedingly slow
(defun %vector-widetag-and-n-bits-shift (type)
  (declare (explicit-check))
  (macrolet ((with-parameters ((arg-type &key intervals)
                               (&rest args) &body body)
               (let ((type-sym (gensym)))
                 `(let (,@(loop for arg in args
                                collect `(,arg '*)))
                    (declare (ignorable ,@args))
                    (when (consp type)
                      (let ((,type-sym (cdr type)))
                        (block nil
                          ,@(loop for arg in args
                                  collect
                                  `(cond ((consp ,type-sym)
                                          (let ((value (pop ,type-sym)))
                                            (if (or (eq value '*)
                                                    (typep value ',arg-type)
                                                    ,(if intervals
                                                         `(and (consp value)
                                                               (null (cdr value))
                                                               (typep (car value)
                                                                      ',arg-type))))
                                                (setf ,arg value)
                                                (ill-type))))
                                         ((null ,type-sym)
                                          (return))
                                         (t
                                          (ill-type)))))
                        (when ,type-sym
                          (ill-type))))
                    ,@body)))
             (ill-type ()
               `(go fastidiously-parse))
             (result (widetag)
               (let ((value (symbol-value widetag)))
                 `(values ,value
                          ,(saetp-n-bits-shift
                            (find value
                                  *specialized-array-element-type-properties*
                                  :key #'saetp-typecode))))))
    (flet ((integer-interval-widetag (low high)
             (if (minusp low)
                 (%integer-vector-widetag-and-n-bits-shift
                  t
                  (1+ (max (integer-length low) (integer-length high))))
                 (%integer-vector-widetag-and-n-bits-shift
                  nil
                  (max (integer-length low) (integer-length high))))))
      (tagbody
         (binding*
             ((consp (consp type))
              (type-name (if consp (car type) type))
              ((widetag n-bits-shift)
               (case type-name
                 ((t)
                  (when consp
                    (ill-type))
                  (result simple-vector-widetag))
                 ((base-char standard-char #-sb-unicode character)
                  (when consp
                    (ill-type))
                  (result simple-base-string-widetag))
                 #+sb-unicode
                 ((character extended-char)
                  (when consp
                    (ill-type))
                  (result simple-character-string-widetag))
                 (bit
                  (when consp
                    (ill-type))
                  (result simple-bit-vector-widetag))
                 (fixnum
                  (when consp
                    (ill-type))
                  (result simple-array-fixnum-widetag))
                 (unsigned-byte
                  (with-parameters ((integer 1)) (high)
                    (if (eq high '*)
                        (result simple-vector-widetag)
                        (%integer-vector-widetag-and-n-bits-shift nil high))))
                 (signed-byte
                  (with-parameters ((integer 1)) (high)
                    (if (eq high '*)
                        (result simple-vector-widetag)
                        (%integer-vector-widetag-and-n-bits-shift t high))))
                 (double-float
                  (with-parameters (double-float :intervals t) (low high)
                    (if (and (not (eq low '*))
                             (not (eq high '*))
                             (if (or (consp low) (consp high))
                                 (>= (type-bound-number low) (type-bound-number high))
                                 (> low high)))
                        (result simple-array-nil-widetag)
                        (result simple-array-double-float-widetag))))
                 (single-float
                  (with-parameters (single-float :intervals t) (low high)
                    (if (and (not (eq low '*))
                             (not (eq high '*))
                             (if (or (consp low) (consp high))
                                 (>= (type-bound-number low) (type-bound-number high))
                                 (> low high)))
                        (result simple-array-nil-widetag)
                        (result simple-array-single-float-widetag))))
                 (mod
                  (if (and (consp type)
                           (consp (cdr type))
                           (null (cddr type))
                           (typep (cadr type) '(integer 1)))
                      (%integer-vector-widetag-and-n-bits-shift
                       nil (integer-length (1- (cadr type))))
                      (ill-type)))
                 #+long-float
                 (long-float
                  (with-parameters (long-float :intervals t) (low high)
                    (if (and (not (eq low '*))
                             (not (eq high '*))
                             (if (or (consp low) (consp high))
                                 (>= (type-bound-number low) (type-bound-number high))
                                 (> low high)))
                        (result simple-array-nil-widetag)
                        (result simple-array-long-float-widetag))))
                 (integer
                  (with-parameters (integer :intervals t) (low high)
                    (let ((low (if (consp low)
                                   (1+ (car low))
                                   low))
                          (high (if (consp high)
                                    (1- (car high))
                                    high)))
                      (cond ((or (eq high '*)
                                 (eq low '*))
                             (result simple-vector-widetag))
                            ((> low high)
                             (result simple-array-nil-widetag))
                            (t
                             (integer-interval-widetag low high))))))
                 (complex
                  (with-parameters (t) (subtype)
                    (if (eq subtype '*)
                        (result simple-vector-widetag)
                        (case subtype
                          ((short-float single-float)
                           (result simple-array-complex-single-float-widetag))
                          ((double-float long-float)
                           (result simple-array-complex-double-float-widetag))
                          ((real rational float)
                           (result simple-vector-widetag))
                          (t
                           (go fastidiously-parse))))))
                 ((nil)
                  (result simple-array-nil-widetag))
                 (t
                  (go fastidiously-parse)))))
           (return-from %vector-widetag-and-n-bits-shift
             (values widetag n-bits-shift)))
       fastidiously-parse)
      ;; Do things the hard way after falling through the tagbody.
      (let* ((ctype (type-or-nil-if-unknown type))
             (ctype (and ctype
                         (sb-kernel::replace-hairy-type ctype))))
        (typecase ctype
          (null (result simple-vector-widetag))
          (numeric-union-type
           (case (sb-kernel::numtype-aspects-id (sb-kernel::numeric-union-type-aspects ctype))
             (#.(sb-kernel::!compute-numtype-aspect-id :real 'integer nil)
              (let* ((ranges (sb-kernel::numeric-union-type-ranges ctype))
                     (low (aref ranges 1))
                     (high (aref ranges (1- (length ranges)))))
                (if (and (integerp low) (integerp high))
                    (integer-interval-widetag low high)
                    (result simple-vector-widetag))))
             (#.(sb-kernel::!compute-numtype-aspect-id :real 'float 'double-float)
              (result simple-array-double-float-widetag))
             (#.(sb-kernel::!compute-numtype-aspect-id :real 'float 'single-float)
              (result simple-array-single-float-widetag))
             (#.(sb-kernel::!compute-numtype-aspect-id :complex 'float 'single-float)
              (result simple-array-complex-single-float-widetag))
             (#.(sb-kernel::!compute-numtype-aspect-id :complex 'float 'double-float)
              (result simple-array-complex-double-float-widetag))
             (t
              (result simple-vector-widetag))))
          (union-type
           (let ((types (union-type-types ctype)))
             (cond ((not (every #'numeric-type-p types))
                    (result simple-vector-widetag))
                   ((csubtypep ctype (specifier-type 'integer))
                    (block nil
                      (integer-interval-widetag
                       (dx-flet ((low (x)
                                      (or (numeric-type-low x)
                                          (return (result simple-vector-widetag)))))
                         (reduce #'min types :key #'low))
                       (dx-flet ((high (x)
                                       (or (numeric-type-high x)
                                           (return (result simple-vector-widetag)))))
                         (reduce #'max types :key #'high)))))
                   ((csubtypep ctype (specifier-type 'double-float))
                    (result simple-array-double-float-widetag))
                   ((csubtypep ctype (specifier-type 'single-float))
                    (result simple-array-single-float-widetag))
                   #+long-float
                   ((csubtypep ctype (specifier-type 'long-float))
                    (result simple-array-long-float-widetag))
                   ((csubtypep ctype (specifier-type 'complex-double-float))
                    (result simple-array-complex-double-float-widetag))
                   ((csubtypep ctype (specifier-type 'complex-single-float))
                    (result simple-array-complex-single-float-widetag))
                   (t
                    (result simple-vector-widetag)))))
          (intersection-type
           (let ((types (intersection-type-types ctype)))
             (loop for type in types
                   unless (hairy-type-p type)
                   return (%vector-widetag-and-n-bits-shift (type-specifier type)))))
          (character-set-type
           #-sb-unicode (result simple-base-string-widetag)
           #+sb-unicode
           (if (loop for (start . end)
                     in (character-set-type-pairs ctype)
                     always (and (< start base-char-code-limit)
                                 (< end base-char-code-limit)))
               (result simple-base-string-widetag)
               (result simple-character-string-widetag)))
          (t
           (let ((expansion (type-specifier ctype)))
             (if (equal expansion type)
                 (result simple-vector-widetag)
                 (%vector-widetag-and-n-bits-shift expansion)))))))))

(defun %complex-vector-widetag (widetag)
  (macrolet ((make-case ()
               `(case widetag
                  ,@(loop for saetp across *specialized-array-element-type-properties*
                          for complex = (saetp-complex-typecode saetp)
                          when complex
                          collect (list (saetp-typecode saetp) complex))
                  (t
                   #.complex-vector-widetag))))
    (make-case)))

(declaim (inline vector-length-in-words))
(defun vector-length-in-words (length n-bits-shift)
  (declare (type index length)
           (type (integer 0 7) n-bits-shift))
  #.(if (fixnump (ash array-dimension-limit 7))
        '(values (ceiling (ash length n-bits-shift) 64))
        '(let ((mask (ash (1- n-word-bits) (- n-bits-shift)))
               (shift (- n-bits-shift
                       (1- (integer-length n-word-bits)))))
          (ash (+ length mask) shift))))


;;; N-BITS-SHIFT is the shift amount needed to turn LENGTH into array-size-in-bits,
;;; i.e. log(2,bits-per-elt)
(declaim (inline allocate-vector-with-widetag))
(defun allocate-vector-with-widetag (#+ubsan poisoned widetag length n-bits-shift)
  (declare (type (unsigned-byte 8) widetag)
           (type index length))
  (let* (    ;; KLUDGE: add SAETP-N-PAD-ELEMENTS "by hand" since there is
             ;; but a single case involving it now.
         (full-length (+ length (if (= widetag simple-base-string-widetag) 1 0)))
         ;; Be careful not to allocate backing storage for element type NIL.
         ;; Both it and type BIT have N-BITS-SHIFT = 0, so the determination
         ;; of true size can't be left up to VECTOR-LENGTH-IN-WORDS.
         ;; VECTOR-LENGTH-IN-WORDS potentially returns a machine-word-sized
         ;; integer, so it doesn't match the primitive type restriction of
         ;; POSITIVE-FIXNUM for the last argument of the vector alloc vops.
         (nwords (the fixnum
                      (if (/= widetag simple-array-nil-widetag)
                          (vector-length-in-words full-length n-bits-shift)
                          0))))
    #+ubsan (if poisoned ; first arg to allocate-vector must be a constant
                      (allocate-vector t widetag length nwords)
                      (allocate-vector nil widetag length nwords))
    #-ubsan (allocate-vector widetag length nwords)))

(declaim (ftype (sfunction (array) (integer 128 255)) array-underlying-widetag)
         (inline array-underlying-widetag))
(defun array-underlying-widetag (array)
  (macrolet ((generate-table ()
               (macrolet ((to-index (x) `(ash ,x -2)))
                 (let ((table (sb-xc:make-array 64 :initial-element 0
                                                   :element-type '(unsigned-byte 8))))
                   (dovector (saetp *specialized-array-element-type-properties*)
                     (let* ((typecode (saetp-typecode saetp))
                            (complex-typecode (saetp-complex-typecode saetp)))
                       (setf (aref table (to-index typecode)) typecode)
                       (when complex-typecode
                         (setf (aref table (to-index complex-typecode)) typecode))))
                   (setf (aref table (to-index simple-array-widetag)) 0
                         (aref table (to-index complex-vector-widetag)) 0
                         (aref table (to-index complex-array-widetag)) 0)
                   table)))
             (to-index (x) `(ash ,x -2)))
    (named-let recurse ((x array))
      (let ((result (aref (generate-table)
                          (to-index (%other-pointer-widetag x)))))
        (if (= 0 result)
            (recurse (%array-data x))
            (truly-the (integer 128 255) result))))))

(defun array-underlying-widetag-and-shift (array)
  (declare (explicit-check))
  (let ((widetag (array-underlying-widetag array)))
    (values widetag
            (truly-the (unsigned-byte 8)
                       (aref %%simple-array-n-bits-shifts%% widetag)))))

;; Complain in various ways about wrong MAKE-ARRAY and ADJUST-ARRAY arguments,
;; returning the two initialization arguments needed for DATA-VECTOR-FROM-INITS.
;; This is an unhygienic macro which would be a MACROLET other than for
;; doing so would entail moving toplevel defuns around for no good reason.
(defmacro check-make-array-initargs (displaceable &optional element-type size)
  `(cond ,@(when displaceable
             `((displaced-to
                (when (or element-p contents-p)
                  (if (and element-p contents-p)
                      (error "Neither :INITIAL-ELEMENT nor :INITIAL-CONTENTS ~
                               may be specified with the :DISPLACED-TO option")
                      (error "~S may not be specified with the :DISPLACED-TO option"
                             (if element-p :initial-element :initial-contents))))
                (unless (= (array-underlying-widetag displaced-to) widetag)
                  ;; Require exact match on upgraded type (lp#1331299)
                  (error "Can't displace an array of type ~/sb-impl:print-type-specifier/ ~
                           into another of type ~/sb-impl:print-type-specifier/"
                         ,element-type (array-element-type displaced-to)))
                (when (< (array-total-size displaced-to)
                         (+ displaced-index-offset ,size))
                  (error "The :DISPLACED-TO array is too small.")))
               (offset-p
                (error "Can't specify :DISPLACED-INDEX-OFFSET without :DISPLACED-TO"))))
         ((and element-p contents-p)
          (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
         (element-p  (values :initial-element initial-element))
         (contents-p (values :initial-contents initial-contents))))
(defmacro make-array-bad-fill-pointer (actual max adjective)
  ;; There was a comment implying that this should be TYPE-ERROR
  ;; but I don't see that as a spec requirement.
  `(error "Can't supply a value for :FILL-POINTER (~S) that is larger ~
           than the~A size of the vector (~S)" ,actual ,adjective ,max))

(declaim (inline %save-displaced-array-backpointer
                 %save-displaced-new-array-backpointer))
(defun %save-displaced-array-backpointer (array data)
  (flet ((purge (pointers)
           (remove-if (lambda (value)
                        (or (not value) (eq array value)))
                      pointers
                      :key #'weak-pointer-value)))
    (let ((old-data (%array-data array)))
      (unless (eq old-data data)
        ;; Add backpointer to the new data vector if it has a header.
        (when (array-header-p data)
          (setf (%array-displaced-from data)
                (cons (make-weak-pointer array)
                      (purge (%array-displaced-from data)))))
        ;; Remove old backpointer, if any.
        (when (array-header-p old-data)
          (setf (%array-displaced-from old-data)
                (purge (%array-displaced-from old-data))))))))

(defun %save-displaced-new-array-backpointer (array data)
  (flet ((purge (pointers)
           (remove-if-not #'weak-pointer-value pointers)))
    (setf (%array-displaced-from data)
          (cons (make-weak-pointer array)
                (purge (%array-displaced-from data))))))

(defmacro populate-dimensions (header list-or-index rank)
  `(if (listp ,list-or-index)
       (let ((dims ,list-or-index))
         (dotimes (axis ,rank)
           (declare ((integer 0 ,array-rank-limit) axis))
           (%set-array-dimension ,header axis (pop dims))))
       (%set-array-dimension ,header 0 ,list-or-index)))

(declaim (inline rank-and-total-size-from-dims))
(defun rank-and-total-size-from-dims (dims)
  (cond ((not (listp dims)) (values 1 (the index dims)))
        ((not dims) (values 0 1))
        (t (let ((rank 1) (product (car dims)))
             (declare (%array-rank rank) (index product))
             (dolist (dim (cdr dims) (values rank product))
               (setq product (* product (the index dim)))
               (incf rank))))))

(declaim (inline widetag->element-type))
(defun widetag->element-type (widetag)
  (svref #.(let ((a (make-array 32 :initial-element 0)))
             (dovector (saetp *specialized-array-element-type-properties* a)
               (let ((tag (saetp-typecode saetp)))
                 (setf (aref a (ash (- tag #x80) -2)) (saetp-specifier saetp)))))
         (- (ash widetag -2) 32)))

(defun initial-contents-error (content-length length)
  (error "There are ~W elements in the :INITIAL-CONTENTS, but ~
                                the vector length is ~W."
         content-length length))

;;; Widetag is the widetag of the underlying vector,
;;; it'll be the same as the resulting array widetag only for simple vectors
(defun %make-array (dimensions widetag n-bits
                    &key
                      element-type
                      (initial-element nil element-p)
                      (initial-contents nil contents-p)
                      adjustable fill-pointer
                      displaced-to
                      (displaced-index-offset 0 offset-p))
  (declare (ignore element-type))
  (binding* (((array-rank total-size) (rank-and-total-size-from-dims dimensions))
             ((initialize initial-data)
              ;; element-type might not be supplied, but widetag->element is always good
              (check-make-array-initargs t (widetag->element-type widetag) total-size))
             (simple (and (null fill-pointer)
                          (not adjustable)
                          (null displaced-to))))

    (cond ((and simple (= array-rank 1))
           (let ((vector ; a (SIMPLE-ARRAY * (*))
                  (allocate-vector-with-widetag #+ubsan (not (or element-p contents-p))
                                                widetag total-size n-bits)))
             ;; presence of at most one :INITIAL-thing keyword was ensured above
             (cond (element-p
                    (fill vector initial-element))
                   (contents-p
                    (let ((content-length (length initial-contents)))
                      (unless (= total-size content-length)
                        (initial-contents-error content-length total-size)))
                    (replace vector initial-contents))
                   #+ubsan
                   (t
                    ;; store the function which bears responsibility for creation of this
                    ;; array in case we need to blame it for not initializing.
                    (set-vector-extra-data (if (= widetag simple-vector-widetag) ; no shadow bits.
                                               vector ; use the LENGTH slot directly
                                               (vector-extra-data vector))
                                           (ash (sap-ref-word (current-fp) n-word-bytes) 3)) ; XXX: magic
                    (cond ((= widetag simple-vector-widetag)
                           (fill vector (%make-lisp-obj unwritten-vector-element-marker)))
                          ((array-may-contain-random-bits-p widetag)
                           ;; Leave the last word alone for base-string,
                           ;; in case the mandatory trailing null is part of a data word.
                           (dotimes (i (- (vector-length-in-words total-size n-bits)
                                          (if (= widetag simple-base-string-widetag) 1 0)))
                             (setf (%vector-raw-bits vector i) sb-ext:most-positive-word))))))
             vector))
          (t
           ;; it's non-simple or multidimensional, or both.
           (when fill-pointer
             (unless (= array-rank 1)
               (error "Only vectors can have fill pointers."))
             (when (and (integerp fill-pointer) (> fill-pointer total-size))
               (make-array-bad-fill-pointer fill-pointer total-size "")))
           (let* ((data (or displaced-to
                            (data-vector-from-inits dimensions total-size widetag n-bits
                                                    initialize initial-data)))
                  (array (make-array-header
                          (cond ((= array-rank 1)
                                 (%complex-vector-widetag widetag))
                                (simple simple-array-widetag)
                                (t complex-array-widetag))
                          array-rank)))
             (cond (fill-pointer
                    (logior-array-flags array +array-fill-pointer-p+)
                    (setf (%array-fill-pointer array)
                          (if (eq fill-pointer t) total-size fill-pointer)))
                   (t
                    (reset-array-flags array +array-fill-pointer-p+)
                    (setf (%array-fill-pointer array) total-size)))
             (setf (%array-available-elements array) total-size)
             (setf (%array-data array) data)
             (setf (%array-displaced-from array) nil)
             (cond (displaced-to
                    (setf (%array-displacement array) (or displaced-index-offset 0))
                    (setf (%array-displaced-p array) t)
                    (when (adjustable-array-p data)
                      (%save-displaced-new-array-backpointer array data)))
                   (t
                    (setf (%array-displaced-p array) nil)))
             (populate-dimensions array dimensions array-rank)
             array)))))

(defun make-array (dimensions &rest args
                   &key (element-type t)
                        initial-element initial-contents
                        adjustable
                        fill-pointer
                        displaced-to
                        displaced-index-offset)
  (declare (ignore initial-element
                   initial-contents adjustable
                   fill-pointer displaced-to displaced-index-offset))
  (declare (explicit-check))
  (multiple-value-bind (widetag shift) (%vector-widetag-and-n-bits-shift element-type)
    (apply #'%make-array dimensions widetag shift args)))

(defun make-static-vector (length &key
                           (element-type '(unsigned-byte 8))
                           (initial-contents nil contents-p)
                           (initial-element nil element-p))
  "Allocate vector of LENGTH elements in static space. Only allocation
of specialized arrays is supported."
  ;; STEP 1: check inputs fully
  ;;
  ;; This way of doing explicit checks before the vector is allocated
  ;; is expensive, but probably worth the trouble as once we've allocated
  ;; the vector we have no way to get rid of it anymore...
  (when (eq t (upgraded-array-element-type element-type))
    (error "Static arrays of type ~/sb-impl:print-type-specifier/ not supported."
           element-type))
  (check-make-array-initargs nil) ; for effect
  (when contents-p
    (unless (= length (length initial-contents))
      (error "There are ~W elements in the :INITIAL-CONTENTS, but the ~
              vector length is ~W."
             (length initial-contents)
             length))
    (unless (every (lambda (x) (typep x element-type)) initial-contents)
      (error ":INITIAL-CONTENTS contains elements not of type ~
               ~/sb-impl:print-type-specifier/."
             element-type)))
  (when element-p
    (unless (typep initial-element element-type)
      (error ":INITIAL-ELEMENT ~S is not of type ~
               ~/sb-impl:print-type-specifier/."
             initial-element element-type)))
  ;; STEP 2
  ;;
  ;; Allocate and possibly initialize the vector.
  (multiple-value-bind (type n-bits-shift)
      (%vector-widetag-and-n-bits-shift element-type)
    (let* ((full-length
             ;; KLUDGE: add SAETP-N-PAD-ELEMENTS "by hand" since there is
             ;; but a single case involving it now.
             (+ length (if (= type simple-base-string-widetag) 1 0)))
           (vector
             (allocate-static-vector type length
                                     (vector-length-in-words full-length
                                                             n-bits-shift))))
      (cond (element-p
             (fill vector initial-element))
            (contents-p
             (replace vector initial-contents))
            (t
             vector)))))

#+darwin-jit
(defun make-static-code-vector (length initial-contents)
  "Allocate vector of LENGTH elements in static space. Only allocation
of specialized arrays is supported."
  (let ((vector (allocate-static-code-vector simple-array-unsigned-byte-8-widetag
                                             length
                                             (* length n-word-bytes))))
    (with-pinned-objects (initial-contents)
      (jit-memcpy (vector-sap vector) (vector-sap initial-contents) length))
    vector))

;;; DATA-VECTOR-FROM-INITS returns a simple rank-1 array that has the
;;; specified array characteristics. Dimensions is only used to pass
;;; to FILL-DATA-VECTOR for error checking on the structure of
;;; initial-contents.
(defun data-vector-from-inits (dimensions total-size widetag n-bits initialize initial-data)
  (declare (fixnum widetag n-bits)) ; really just that they're non-nil
  (let ((data (allocate-vector-with-widetag #+ubsan (not initialize) widetag total-size n-bits)))
    (ecase initialize
     (:initial-element
      (fill (the vector data) initial-data))
     (:initial-contents
      ;; DIMENSIONS can be supplied as a list or integer now
      (dx-let ((list-of-dims (list dimensions))) ; ok if already a list
        (fill-data-vector data
                          (if (listp dimensions) dimensions list-of-dims)
                          initial-data)))
     ((nil)))
    data))

(defun vector (&rest objects)
  "Construct a SIMPLE-VECTOR from the given objects."
  (let ((v (make-array (length objects))))
    (do-rest-arg ((x i) objects 0 v)
      (setf (aref v i) x))))


;;;; accessor/setter functions

;;; Dispatch to an optimized routine the data vector accessors for
;;; each different specialized vector type. Do dispatching by looking
;;; up the widetag in the array rather than with the typecases, which
;;; as of 1.0.5 compiles to a naive sequence of linear TYPEPs. Also
;;; provide separate versions where bounds checking has been moved
;;; from the callee to the caller, since it's much cheaper to do once
;;; the type information is available. Finally, for each of these
;;; routines also provide a slow path, taken for arrays that are not
;;; vectors or not simple.
;;; FIXME: how is this not redundant with DEFINE-ARRAY-DISPATCH?
;;; Which is to say, why did DEFINE-ARRAY-DISPATCH decide to do
;;; something different instead of figuring out how to unify the ways
;;; that we call an element of an array accessed by widetag?
(macrolet ((def (name table-name)
             `(progn
                (define-load-time-global ,table-name
                    (make-array ,(1+ widetag-mask)))
                (declaim (type (simple-array function (,(1+ widetag-mask)))
                               ,table-name))
                (defmacro ,name (array-var &optional type)
                  (if type
                      `(the function
                            (svref ,',table-name (%other-pointer-widetag
                                                  (locally (declare (optimize (safety 1)))
                                                    (the ,type ,array-var)))))
                      `(the function
                            ;; Assigning TAG to 0 initially produces slightly better
                            ;; code than would be generated by the more natural expression
                            ;;   (let ((tag (if (%other-ptr ...) (widetag ...) 0)))
                            ;; but either way is suboptimal. As expressed, if the array-var
                            ;; is known to satisfy %other-pointer-p, then it performs a
                            ;; move-immediate-to-register which is clobbered right away
                            ;; by a zero-extending load. A peephole pass could eliminate
                            ;; the first move as effectless.  If expressed the other way,
                            ;; it would produce a jump around a jump because the compiler
                            ;; is unwilling to *unconditionally* assign 0 into a register
                            ;; to begin with. It actually wants to guard an immediate load
                            ;; when it doesn't need to, as if both consequents of the IF
                            ;; have side-effects that should not happen.
                            (let ((tag 0))
                              (when (%other-pointer-p ,array-var)
                                (setf tag (%other-pointer-widetag ,array-var)))
                              (svref ,',table-name tag))))))))
  (def !find-data-vector-setter %%data-vector-setters%%)
  (def !find-data-vector-setter/check-bounds %%data-vector-setters/check-bounds%%)
  ;; Used by DO-VECTOR-DATA -- which in turn appears in DOSEQUENCE expansion,
  ;; meaning we can have post-build dependences on this.
  (def %find-data-vector-reffer %%data-vector-reffers%%)
  (def !find-data-vector-reffer/check-bounds %%data-vector-reffers/check-bounds%%))

;;; Like DOVECTOR, but more magical -- can't use this on host.
(defmacro sb-impl::do-vector-data ((elt vector &optional result) &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    (with-unique-names (index vec start end ref)
      `(with-array-data ((,vec ,vector)
                         (,start)
                         (,end)
                         :check-fill-pointer t)
         (let ((,ref (%find-data-vector-reffer ,vec)))
           (declare (function ,ref))
           (do ((,index ,start (1+ ,index)))
               ((>= ,index ,end)
                (let ((,elt nil))
                  ,@(sb-impl::filter-dolist-declarations decls)
                  ,elt
                  ,result))
             (let ((,elt (funcall ,ref ,vec ,index)))
               ,@decls
               (tagbody ,@forms))))))))

(macrolet ((%ref (accessor-getter extra-params &optional vector-check)
             `(sb-c::%funcall-no-nargs (,accessor-getter array ,vector-check) array index ,@extra-params))
           (define (accessor-name slow-accessor-name accessor-getter
                                  extra-params check-bounds)
             `(progn
                (defun ,accessor-name (array index ,@extra-params)
                  (declare (explicit-check))
                  (declare (optimize speed
                                     ;; (SAFETY 0) is ok. All calls to
                                     ;; these functions are generated by
                                     ;; the compiler, so argument count
                                     ;; checking isn't needed. Type checking
                                     ;; is done implicitly via the widetag
                                     ;; dispatch.
                                     (safety 0)))
                  (%ref ,accessor-getter ,extra-params))
                (defun ,(symbolicate 'vector- accessor-name) (array index ,@extra-params)
                  (declare (explicit-check)
                           (optimize speed (safety 0)))
                  (%ref ,accessor-getter ,extra-params vector))
                (defun ,(symbolicate 'string- accessor-name) (array index ,@extra-params)
                  (declare (explicit-check)
                           (optimize speed (safety 0)))
                  (%ref ,accessor-getter ,extra-params string))
                (defun ,slow-accessor-name (array index ,@extra-params)
                  (declare (optimize speed (safety 0))
                           (array array))
                  (if (not (%array-displaced-p array))
                      ;; The reasonably quick path of non-displaced complex
                      ;; arrays.
                      (let ((array (%array-data array)))
                        (%ref ,accessor-getter ,extra-params))
                      ;; The real slow path.
                      (with-array-data
                          ((array array)
                           (index (locally
                                      (declare (optimize (speed 1) (safety 1)))
                                    (,@check-bounds index)))
                           (end)
                           :force-inline t)
                        (declare (ignore end))
                        (%ref ,accessor-getter ,extra-params)))))))
  (define hairy-data-vector-ref slow-hairy-data-vector-ref
    %find-data-vector-reffer
    nil (progn))
  (define hairy-data-vector-set slow-hairy-data-vector-set
    !find-data-vector-setter
    (new-value) (progn))
  (define hairy-data-vector-ref/check-bounds
      slow-hairy-data-vector-ref/check-bounds
    !find-data-vector-reffer/check-bounds
    nil (check-bound array (%array-dimension array 0)))
  (define hairy-data-vector-set/check-bounds
      slow-hairy-data-vector-set/check-bounds
    !find-data-vector-setter/check-bounds
    (new-value) (check-bound array (%array-dimension array 0))))

(defun hairy-ref-error (array index &optional new-value)
  (declare (ignore index new-value)
           (optimize (sb-c:verify-arg-count 0)))
  (error 'type-error
         :datum array
         :expected-type 'vector))

(macrolet ((define-reffer (saetp check-form)
             (let* ((type (saetp-specifier saetp))
                    (atype `(simple-array ,type (*))))
               `(named-lambda (optimized-data-vector-ref ,type) (vector index)
                  (declare (optimize speed (safety 0))
                           ;; Obviously these all coerce raw words to lispobjs
                           ;; so don't keep spewing notes about it.
                           (muffle-conditions compiler-note)
                           (ignorable index))
                  ,(if type
                       `(data-vector-ref (the ,atype vector)
                                         (the index
                                              (locally
                                                  (declare (optimize (safety 1)))
                                                (,@check-form index))))
                       `(data-nil-vector-ref (the ,atype vector) index)))))
           (define-setter (saetp check-form)
             (let* ((type (saetp-specifier saetp))
                    (atype `(simple-array ,type (*))))
               `(named-lambda (optimized-data-vector-set ,type) (vector index new-value)
                  (declare (optimize speed (safety 0)))
                  ;; Impossibly setting an elt of an (ARRAY NIL)
                  ;; returns no value. And nobody cares.
                  (declare (muffle-conditions compiler-note))
                  (data-vector-set (the ,atype vector)
                                   (locally
                                       (declare (optimize (safety 1)))
                                     (the index
                                       (,@check-form index)))
                                   (locally
                                       ;; SPEED 1 needed to avoid the compiler
                                       ;; from downgrading the type check to
                                       ;; a cheaper one.
                                       (declare (optimize (speed 1)
                                                          (safety 1)))
                                     (the* (,type :context sb-c::aref-context) new-value)))
                  ;; Low-level setters return no value
                  new-value)))
           (define-reffers (symbol deffer check-form slow-path)
             `(progn
                ;; FIXME/KLUDGE: can't just FILL here, because genesis doesn't
                ;; preserve the binding, so re-initiaize as NS doesn't have
                ;; the energy to figure out to change that right now.
                (setf ,symbol (make-array (1+ widetag-mask)
                                          :initial-element #'hairy-ref-error))
                ,@(loop for widetag in '(complex-vector-widetag
                                         complex-bit-vector-widetag
                                         #+sb-unicode complex-character-string-widetag
                                         complex-base-string-widetag
                                         simple-array-widetag
                                         complex-array-widetag)
                        collect `(setf (svref ,symbol ,widetag) ,slow-path))
                ,@(loop for saetp across *specialized-array-element-type-properties*
                        for widetag = (saetp-typecode saetp)
                        collect `(setf (svref ,symbol ,widetag)
                                       (,deffer ,saetp ,check-form))))))
  (defun !hairy-data-vector-reffer-init ()
    (define-reffers %%data-vector-reffers%% define-reffer
      (progn)
      #'slow-hairy-data-vector-ref)
    (define-reffers %%data-vector-setters%% define-setter
      (progn)
      #'slow-hairy-data-vector-set)
    (define-reffers %%data-vector-reffers/check-bounds%% define-reffer
      (check-bound vector (length vector))
      #'slow-hairy-data-vector-ref/check-bounds)
    (define-reffers %%data-vector-setters/check-bounds%% define-setter
      (check-bound vector (length vector))
      #'slow-hairy-data-vector-set/check-bounds)))

;;; (Ordinary DATA-VECTOR-REF usage compiles into a vop, but
;;; DATA-VECTOR-REF is also FOLDABLE, and this ordinary function
;;; definition is needed for the compiler to use in constant folding.)
(defun data-vector-ref (array index)
  (declare (explicit-check))
  (hairy-data-vector-ref array index))

(defun data-vector-ref-with-offset (array index offset)
  (declare (explicit-check))
  (hairy-data-vector-ref array (+ index offset)))

(defun invalid-array-p (array)
  (and (array-header-p array)
       (consp (%array-displaced-p array))))

(declaim (ftype (function (array) nil) invalid-array-error))
(define-error-wrapper invalid-array-error (array)
  (aver (array-header-p array))
  ;; Array invalidation stashes the original dimensions here...
  (let ((dims (%array-displaced-p array))
        (et (array-element-type array)))
    (error 'invalid-array-error
           :datum array
           :expected-type
           (if (cdr dims)
               `(array ,et ,dims)
               `(vector ,et ,@dims)))))

(declaim (ftype (function (array t integer &optional t) nil)
                invalid-array-index-error))
(define-error-wrapper invalid-array-index-error (array index bound &optional axis)
  (if (invalid-array-p array)
      (invalid-array-error array)
      (error 'invalid-array-index-error
             :array array
             :axis axis
             :datum index
             :expected-type `(integer 0 (,bound)))))

;;; SUBSCRIPTS has a dynamic-extent list structure and is destroyed
(defun %array-row-major-index (array &rest subscripts)
  (declare (dynamic-extent subscripts)
           (array array))
  (let ((length (length subscripts)))
    (cond ((array-header-p array)
           (let ((rank (%array-rank array)))
             (unless (= rank length)
               (error "Wrong number of subscripts, ~W, for array of rank ~W."
                      length rank))
             (do ((axis (1- rank) (1- axis))
                  (chunk-size 1)
                  (result 0))
                 ((minusp axis) result)
               (declare (fixnum axis chunk-size result))
               (let ((index (fast-&rest-nth axis subscripts))
                     (dim (%array-dimension array axis)))
                 (unless (and (fixnump index) (< -1 index dim))
                   (invalid-array-index-error array index dim axis))
                 (setf result
                       (truly-the fixnum
                                  (+ result
                                     (truly-the fixnum (* chunk-size index))))
                       chunk-size (truly-the fixnum (* chunk-size dim)))))))
          ((/= length 1)
           (error "Wrong number of subscripts, ~W, for array of rank 1."
                  length))
          (t
           (let ((index (fast-&rest-nth 0 subscripts))
                 (length (length (the (simple-array * (*)) array))))
             (unless (and (fixnump index) (< -1 index length))
               (invalid-array-index-error array index length))
             index)))))

(defun array-in-bounds-p (array &rest subscripts)
  "Return T if the SUBSCRIPTS are in bounds for the ARRAY, NIL otherwise."
  (declare (dynamic-extent subscripts))
  (let ((length (length subscripts)))
    (cond ((array-header-p array)
           (let ((rank (%array-rank array)))
             (unless (= rank length)
               (error "Wrong number of subscripts, ~W, for array of rank ~W."
                      length rank))
             (loop for i below length
                   for s = (fast-&rest-nth i subscripts)
                   always (and (typep s '(and fixnum unsigned-byte))
                               (< s (%array-dimension array i))))))
          ((/= length 1)
           (error "Wrong number of subscripts, ~W, for array of rank 1."
                  length))
          (t
           (let ((subscript (fast-&rest-nth 0 subscripts)))
             (and (typep subscript '(and fixnum unsigned-byte))
                  (< subscript
                     (length (truly-the (simple-array * (*)) array)))))))))

(defun array-row-major-index (array &rest subscripts)
  (declare (dynamic-extent subscripts))
  (apply #'%array-row-major-index array subscripts))

(defun aref (array &rest subscripts)
  "Return the element of the ARRAY specified by the SUBSCRIPTS."
  (declare (dynamic-extent subscripts))
  (row-major-aref array (apply #'%array-row-major-index array subscripts)))

;;; (setf aref/bit/sbit) are implemented using setf-functions,
;;; because they have to work with (setf (apply #'aref array subscripts))
;;; All other setfs can be done using setf-functions too, but I
;;; haven't found technical advantages or disadvantages for either
;;; scheme.
(defun (setf aref) (new-value array &rest subscripts)
  (declare (dynamic-extent subscripts)
           (type array array))
  (setf (row-major-aref array (apply #'%array-row-major-index array subscripts))
        new-value))

#+(or x86-64 arm64)
(defun (cas aref) (old new array &rest subscripts)
  (let ((index (apply #'%array-row-major-index array subscripts)))
    (if (not (simple-array-p array))
        (bug "(CAS AREF) on non-simple arrays is unimplemented")
        (with-array-data ((vec array) (start) (end))
          (declare (ignore start end))
          (if (simple-vector-p vec) ; N-dimensional array of T
              (cas (svref vec index) old new)
              (with-pinned-objects (vec)
                (let ((sap (vector-sap vec)))
                  (typecase vec
                    ((simple-array (unsigned-byte 8) (*))
                     (cas (sap-ref-8 sap index) old new))
                    ((simple-array (signed-byte 8) (*))
                     (cas (signed-sap-ref-8 sap index) old new))
                    ((simple-array (unsigned-byte 16) (*))
                     (cas (sap-ref-16 sap (ash index 1)) old new))
                    ((simple-array (signed-byte 16) (*))
                     (cas (signed-sap-ref-16 sap (ash index 1)) old new))
                    ((simple-array (unsigned-byte 32) (*))
                     (cas (sap-ref-32 sap (ash index 2)) old new))
                    ((simple-array (signed-byte 32) (*))
                     (cas (signed-sap-ref-32 sap (ash index 2)) old new))
                    #+64-bit
                    ((simple-array (unsigned-byte 64) (*))
                     (cas (sap-ref-64 sap (ash index 3)) old new))
                    #+64-bit
                    ((simple-array (signed-byte 64) (*))
                     (cas (signed-sap-ref-64 sap (ash index 3)) old new))
                    #+x86-64
                    ((simple-array single-float (*))
                     (cas (sap-ref-single sap (ash index 2)) old new))
                    #+x86-64
                    ((simple-array double-float (*))
                     (cas (sap-ref-double sap (ash index 3)) old new))
                    (t
                     (bug "(CAS AREF) is not implemented on ~/sb-impl:print-type-specifier/"
                          (type-of array)))))))))))

(defun row-major-aref (array index)
  "Return the element of array corresponding to the row-major index. This is
   SETFable."
  (declare (optimize (safety 1)))
  (row-major-aref array index))

(defun %set-row-major-aref (array index new-value)
  (declare (optimize (safety 1)))
  (setf (row-major-aref array index) new-value))

(defun svref (simple-vector index)
  "Return the INDEXth element of the given Simple-Vector."
  (declare (optimize (safety 1)))
  (aref simple-vector index))

(defun %svset (simple-vector index new)
  (declare (optimize (safety 1)))
  (setf (aref simple-vector index) new))

(defun bit (bit-array &rest subscripts)
  "Return the bit from the BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (type (array bit) bit-array)
           (dynamic-extent subscripts)
           (optimize (safety 1)))
  (row-major-aref bit-array (apply #'%array-row-major-index bit-array subscripts)))

(defun (setf bit) (new-value bit-array &rest subscripts)
  (declare (type (array bit) bit-array)
           (type bit new-value)
           (dynamic-extent subscripts)
           (optimize (safety 1)))
  (setf (row-major-aref bit-array
                        (apply #'%array-row-major-index bit-array subscripts))
        new-value))

(defun sbit (simple-bit-array &rest subscripts)
  "Return the bit from SIMPLE-BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (type (simple-array bit) simple-bit-array)
           (dynamic-extent subscripts)
           (optimize (safety 1)))
  (row-major-aref simple-bit-array
                  (apply #'%array-row-major-index simple-bit-array subscripts)))

(defun (setf sbit) (new-value bit-array &rest subscripts)
  (declare (type (simple-array bit) bit-array)
           (type bit new-value)
           (dynamic-extent subscripts)
           (optimize (safety 1)))
  (setf (row-major-aref bit-array
                        (apply #'%array-row-major-index bit-array subscripts))
        new-value))

;;;; miscellaneous array properties

(define-load-time-global *saetp-widetag-ctype* (make-array 32 :initial-element (make-unbound-marker)))

(defun array-element-ctype (array)
  ;; same as (SPECIFIER-TYPE (ARRAY-ELEMENT-TYPE ARRAY)) but more efficient
  (svref *saetp-widetag-ctype*
         (- (ash (array-underlying-widetag array) -2) 32)))

(defun array-element-type (array)
  "Return the type of the elements of the array"
  (declare (explicit-check array))
  (truly-the (or list symbol)
             (widetag->element-type (array-underlying-widetag array))))

(defun array-rank (array)
  "Return the number of dimensions of ARRAY."
  (%array-rank array))

(defun array-dimension (array axis-number)
  "Return the length of dimension AXIS-NUMBER of ARRAY."
  (declare (array array) (type index axis-number))
  (cond ((not (array-header-p array))
         (unless (= axis-number 0)
           (error "Vector axis is not zero: ~S" axis-number))
         (length (the (simple-array * (*)) array)))
        ((>= axis-number (%array-rank array))
         (error "Axis number ~W is too big; ~S only has ~D dimension~:P."
                axis-number array (%array-rank array)))
        (t
         (%array-dimension array axis-number))))

(defun array-dimensions (array)
  "Return a list whose elements are the dimensions of the array"
  (declare (explicit-check))
  (cond ((array-header-p array)
         (do ((results nil (cons (%array-dimension array index) results))
              (index (1- (%array-rank array)) (1- index)))
             ((minusp index) results)))
        ((typep array 'vector)
         (list (length array)))
        (t
         (sb-c::%type-check-error/c array 'object-not-array-error nil))))

(defun array-total-size (array)
  "Return the total number of elements in the Array."
  (declare (explicit-check))
  (cond ((array-header-p array)
         (%array-available-elements array))
        ((typep array 'vector)
         (length array))
        (t
         (sb-c::%type-check-error/c array 'object-not-array-error nil))))

(defun array-displacement (array)
  "Return the values of :DISPLACED-TO and :DISPLACED-INDEX-offset
   options to MAKE-ARRAY, or NIL and 0 if not a displaced array."
  (declare (type array array))
  (if (and (array-header-p array) ; if unsimple and
           (%array-displaced-p array)) ; displaced
      (values (%array-data array) (%array-displacement array))
      (values nil 0)))

(defun adjustable-array-p (array)
  "Return T if and only if calling ADJUST-ARRAY on ARRAY will return
   the identical object."
  (declare (array array))
  ;; Note that this appears not to be a fundamental limitation.
  ;; non-vector SIMPLE-ARRAYs are in fact capable of being adjusted,
  ;; but in practice we test using ADJUSTABLE-ARRAY-P in ADJUST-ARRAY.
  ;; -- CSR, 2004-03-01.
  (not (typep array 'simple-array)))

;;;; fill pointer frobbing stuff

(setf (info :function :predicate-truth-constraint 'array-has-fill-pointer-p)
      '(and vector (not simple-array)))
(defun array-has-fill-pointer-p (array)
  "Return T if the given ARRAY has a fill pointer, or NIL otherwise."
  (array-has-fill-pointer-p array))

(define-error-wrapper fill-pointer-error (vector)
  (declare (optimize (sb-c::verify-arg-count 0)))
  (error 'simple-type-error
         :datum vector
         :expected-type '(and vector (satisfies array-has-fill-pointer-p))
         :format-control "~S is not an array with a fill pointer."
         :format-arguments (list vector)))


(defun fill-pointer (vector)
  "Return the FILL-POINTER of the given VECTOR."
  (declare (explicit-check))
  (fill-pointer vector))

(defun %set-fill-pointer (vector new)
  (declare (explicit-check))
  (cond ((not (and (arrayp vector)
                   (array-has-fill-pointer-p vector)))
         (fill-pointer-error vector))
        (t
         (let ((max (%array-available-elements vector)))
           (when (> (the (and unsigned-byte fixnum) new) max)
             (error 'simple-type-error
                    :datum new
                    :expected-type (list 'integer 0 max)
                    :format-control "The new fill pointer, ~S, is larger than the length of the vector (~S.)"
                    :format-arguments (list new max)))
           (setf (%array-fill-pointer vector) (truly-the index new))))))

#-system-tlabs
(defmacro reallocate-vector-with-widetag (old-vector &rest args)
  (declare (ignore old-vector))
  `(allocate-vector-with-widetag ,@args))

;;; This does not try to allow for resizing (ARRAY NIL) - there's no backing storage anyway.
;;; However, ADJUST-ARRAY apparently thinks it can resize non-simple arrays of
;;; element type NIL, but fails in ZAP-ARRAY-DATA-AUX. e.g.:
;;;   (adjust-array (make-array '(10 10) :element-type nil) '(20 20))
;;; allocates a non-simple 10x10 array pointing to a (SIMPLE-ARRAY NIL 100)
;;; and then gets "An attempt to access an array of element-type NIL was made"
;;; because it doesn't know not to try to copy elements.
;;; So unless we think that that is one of the most pressing issues that demands
;;; a fix, who cares how we reallocate?
;;; If you're manipulating such arrays, quite literally you deserve to lose.
;;; FIXME: does not support #+ubsan, which is fairly bit-rotted, so ... meh.
#+system-tlabs
(defun reallocate-vector-with-widetag (old-vector widetag length n-bits-shift)
  (declare (type (unsigned-byte 8) widetag)
           (type index length))
  ;; KLUDGE: add SAETP-N-PAD-ELEMENTS "by hand" since there is
  ;; but a single case involving it now.
  (let* ((full-length (+ length (if (= widetag simple-base-string-widetag) 1 0)))
         (nwords (the fixnum (vector-length-in-words full-length n-bits-shift))))
    (if (sb-vm::force-to-heap-p old-vector)
        (locally (declare (sb-c::tlab :system))
          (allocate-vector widetag length nwords))
        (allocate-vector widetag length nwords))))

(defun extend-vector (vector min-extension)
  (declare (optimize speed)
           (vector vector))
  (let* ((old-length (length vector))
         (min-extension (or min-extension
                            (min old-length
                                 (- array-dimension-limit old-length))))
         (new-length (the index (+ old-length
                                   (max 1 min-extension))))
         (fill-pointer (1+ old-length)))
    (declare (fixnum new-length min-extension))
    (with-array-data ((old-data vector) (old-start)
                      (old-end old-length))
      (let* ((widetag (%other-pointer-widetag old-data))
             (n-bits-shift (aref %%simple-array-n-bits-shifts%% widetag))
             (new-data
              ;; FIXME: mark prefix of shadow bits assigned, suffix unassigned
              (reallocate-vector-with-widetag old-data #+ubsan nil
                                              widetag new-length n-bits-shift)))
        ;; Copy the data
        (if (= widetag simple-vector-widetag) ; the most common case
            (replace (truly-the simple-vector new-data) ; transformed
                     (truly-the simple-vector old-data)
                     :start2 old-start :end2 old-end)
            (let ((copier (blt-copier-for-widetag widetag)))
              (if copier
                  (funcall (truly-the function copier) old-data old-start new-data 0 old-length)
                  (replace new-data old-data :start2 old-start :end2 old-end))))
        (setf (%array-data vector) new-data
              (%array-available-elements vector) new-length
              (%array-fill-pointer vector) fill-pointer
              (%array-displacement vector) 0
              (%array-displaced-p vector) nil)
        (%set-array-dimension vector 0 new-length)
        vector))))

(defun vector-push-extend (new-element vector &optional min-extension)
  (declare (type (or null (and index (integer 1))) min-extension))
  (declare (explicit-check))
  (let* ((fill-pointer (fill-pointer vector))
         (new-fill-pointer (1+ fill-pointer)))
    (if (= fill-pointer (%array-available-elements vector))
        (extend-vector vector min-extension)
        (setf (%array-fill-pointer vector) new-fill-pointer))
    ;; disable bounds checking
    (locally (declare (optimize (safety 0)))
      (setf (aref vector fill-pointer) new-element))
    fill-pointer))

(defun prepare-vector-push-extend (vector)
  (declare (explicit-check))
  (let* ((fill-pointer (fill-pointer vector))
         (new-fill-pointer (1+ fill-pointer)))
    (if (= fill-pointer (%array-available-elements vector))
        (extend-vector vector nil)
        (setf (%array-fill-pointer vector) new-fill-pointer))
    (multiple-value-bind (array index) (%data-vector-and-index vector fill-pointer)
      (values array index fill-pointer))))

(defun vector-pop (array)
  "Decrease the fill pointer by 1 and return the element pointed to by the
  new fill pointer."
  (declare (explicit-check))
  (let ((fill-pointer (fill-pointer array)))
    (if (zerop fill-pointer)
        (error "There is nothing left to pop.")
        ;; disable bounds checking (and any fixnum test)
        (locally (declare (optimize (safety 0)))
          (aref array
                (setf (%array-fill-pointer array)
                      (1- fill-pointer)))))))

(defun vector-push (new-element array)
  "Attempt to set the element of ARRAY designated by its fill pointer
   to NEW-ELEMENT, and increment the fill pointer by one. If the fill pointer is
   too large, NIL is returned, otherwise the index of the pushed element is
   returned."
  (declare (explicit-check))
  (let ((fill-pointer (fill-pointer array)))
    (cond ((= fill-pointer (%array-available-elements array))
           nil)
          (t
           (locally (declare (optimize (safety 0)))
             (setf (aref array fill-pointer) new-element))
           (setf (%array-fill-pointer array) (1+ fill-pointer))
           fill-pointer))))


;;;; ADJUST-ARRAY

(defun adjust-array (array dimensions &key
                           (element-type (array-element-type array) element-type-p)
                           (initial-element nil element-p)
                           (initial-contents nil contents-p)
                           fill-pointer
                           displaced-to (displaced-index-offset 0 offset-p))
  "Adjust ARRAY's dimensions to the given DIMENSIONS and stuff."
  (when (invalid-array-p array)
    (invalid-array-error array))
  (binding*
      (((rank new-total-size) (rank-and-total-size-from-dims dimensions))
       (widetag
        (let ((widetag (array-underlying-widetag array)))
          (unless (= (array-rank array) rank) ; "drive-by" check of the rank
            (error "Expected ~D new dimension~:P for array, but received ~D."
                   (array-rank array) rank))
          (if (or (not element-type-p)
                  ;; Quick pass if ELEMENT-TYPE is same as the element type based on widetag
                  (equal element-type (widetag->element-type widetag))
                  (= (%vector-widetag-and-n-bits-shift element-type) widetag))
              widetag
              (error "The new element type, ~/sb-impl:print-type-specifier/, is incompatible ~
                      with old type, ~/sb-impl:print-type-specifier/."
                     element-type (array-element-type array)))))
       (new-fill-pointer
        (cond (fill-pointer
               (unless (array-has-fill-pointer-p array)
                 (if (/= rank 1)
                     (error "Only vectors can have fill pointers.")
                     ;; I believe the sentence saying that this is an error pre-dates the removal
                     ;; of the restriction of calling ADJUST-ARRAY only on arrays that
                     ;; are actually adjustable. Making a new array should always work,
                     ;; so I think this may be a bug in the spec.
                     (fill-pointer-error array)))
               (cond ((eq fill-pointer t) new-total-size)
                     ((<= fill-pointer new-total-size) fill-pointer)
                     (t (make-array-bad-fill-pointer fill-pointer new-total-size " new"))))
              ((array-has-fill-pointer-p array)
               ;; "consequences are unspecified if array is adjusted to a size smaller than its fill pointer"
               (let ((old-fill-pointer (%array-fill-pointer array)))
                 (when (< new-total-size old-fill-pointer)
                   (error "can't adjust vector ~S to a size (~S) smaller than ~
                           its current fill pointer (~S)"
                          array new-total-size old-fill-pointer))
                 old-fill-pointer))))
       ((initialize initial-data)
        (check-make-array-initargs t element-type new-total-size))
       (n-bits-shift (aref %%simple-array-n-bits-shifts%% widetag)))

    (cond
      (displaced-to ; super easy - just repoint ARRAY to new data
       (if (adjustable-array-p array)
           (set-array-header array displaced-to new-total-size new-fill-pointer
                             displaced-index-offset dimensions t nil)
           (%make-array dimensions widetag n-bits-shift
                        :displaced-to displaced-to
                        :displaced-index-offset displaced-index-offset)))
      (contents-p ; array former contents replaced by INITIAL-CONTENTS
       (let ((array-data (data-vector-from-inits dimensions new-total-size widetag n-bits-shift
                                                 initialize initial-data)))
         (cond ((adjustable-array-p array)
                (set-array-header array array-data new-total-size new-fill-pointer
                                  0 dimensions nil nil))
               ((array-header-p array)
                ;; simple multidimensional array.
                ;; fill-pointer vectors satisfy ADJUSTABLE-ARRAY-P (in SBCL, that is)
                ;; and therefore are handled by the first stanza of the cond.
                (%make-array dimensions widetag n-bits-shift
                             :initial-contents initial-contents))
               (t
                array-data))))
      ((= rank 1)
       (let ((old-length (array-total-size array)))
         ;; Because ADJUST-ARRAY has to ignore any fill-pointer when
         ;; copying from the old data, we can't just pass ARRAY as the
         ;; second argument of REPLACE.
         (with-array-data ((old-data array) (old-start) (old-end old-length))
           (let ((new-data
                  (if (and (= new-total-size old-length)
                           (not (and (array-header-p array) (%array-displaced-p array))))
                      ;; if total size is unchanged, and it was not a displaced array,
                      ;; then this array owns the data and can retain it.
                      old-data
                      (let ((data
                             (reallocate-vector-with-widetag old-data #+ubsan t
                                                             widetag new-total-size
                                                             n-bits-shift)))
                        (replace data old-data
                                 :start1 0 :end1 new-total-size
                                 :start2 old-start :end2 old-end)
                        (when (and element-p (> new-total-size old-length))
                          (fill data initial-element :start old-length))
                        data))))
             (if (adjustable-array-p array)
                 (set-array-header array new-data new-total-size new-fill-pointer
                                   0 dimensions nil nil)
                 new-data)))))
      (t
           (let ((old-total-size (%array-available-elements array)))
             (with-array-data ((old-data array) (old-start) (old-end old-total-size))
               (declare (ignore old-end))
               (let ((new-data (if (or (and (array-header-p array)
                                            (%array-displaced-p array))
                                       (> new-total-size old-total-size)
                                       (not (adjustable-array-p array)))
                                   (data-vector-from-inits dimensions new-total-size widetag
                                                           n-bits-shift initialize initial-data)
                                   old-data)))
                 (if (or (zerop old-total-size) (zerop new-total-size))
                     (when element-p (fill new-data initial-element))
                     (zap-array-data old-data (array-dimensions array)
                                     old-start
                                     new-data dimensions new-total-size
                                     element-type initial-element
                                     element-p))
                 (if (adjustable-array-p array)
                     (set-array-header array new-data new-total-size
                                       nil 0 dimensions nil nil)
                     (let ((new-array (make-array-header simple-array-widetag rank)))
                       (set-array-header new-array new-data new-total-size
                                         nil 0 dimensions nil t))))))))))

;;; Destructively alter VECTOR, changing its length to NEW-LENGTH,
;;; which must be less than or equal to its current length. This can
;;; be called on vectors without a fill pointer but it is slightly
;;; dangerous to do so: shrinking the size of an object accessible
;;; to another thread could cause it to access an out-of-bounds element.
;;; GC should generally be fine no matter what happens, because it either
;;; reads the old length or the new length. If it reads the old length,
;;; then the whole vector is skipped if unboxed; if it reads the new length,
;;; then the next object is a filler.
;;; Exception: for SIMPLE-VECTOR we always zeroized the unused tail,
;;; because the garbage collector can scan certain pages without regard
;;; to object boundaries. The situation we need to avoid is this:
;;;       "old" #(...............|.....)
;;;       "new" #(..........)Fill|.....
;;;                              ^ page boundary
;;; where GC reads the objects on the page just after the filler
;;; because it doesn't know not to.
;;;
(defmacro make-filler (n)
  `(logior (ash ,n #+64-bit 32 #-64-bit ,n-widetag-bits) filler-widetag))
(defmacro filler-nwords (header)
  `(ash ,header #+64-bit -32 #-64-bit ,(- n-widetag-bits)))

(defun %shrink-vector (vector new-length
                              &aux (old-length (length vector))
                                   (new-length* new-length))
  (declare (vector vector))
  (cond
    ((simple-vector-p vector)
     ;; We do in fact call %SHRINK-VECTOR a lot from sequence functions
     ;; that overallocate a temporary result. In all places where that happens,
     ;; the discarded suffix was never used. So assuming pre-zeroed heap,
     ;; it kind of just worked. But I don't want to assume that.
     ;; For what it's worth, adding this assertion prior to FILL:
     ;;   (WHEN (FIND 0 VECTOR :START OLD-LENGTH :TEST #'NEQ) (BUG "No can do"))
     ;; produced no failures in the regression suite.
     (when (< new-length old-length) (fill vector 0 :start new-length)))
    ((not (or (array-header-p vector) (typep vector '(simple-array nil (*)))))
      (when (simple-base-string-p vector)
        ;; We can blindly store the hidden #\null at NEW-LENGTH, but it would
        ;; appear to be an out-of-bounds access if the length is not
        ;; changing at all. i.e. while it's safe to always do a store,
        ;; the length check has to be skipped.
        (locally (declare (optimize (sb-c:insert-array-bounds-checks 0)))
          (setf (schar vector new-length) (code-char 0)))
        ;; Now treat both the old and new lengths as if they include
        ;; the byte that holds the implicit string terminator.
        (incf old-length)
        (incf new-length*))
      (let* ((n-bits-shift (aref %%simple-array-n-bits-shifts%%
                                 (%other-pointer-widetag vector)))
             (old-nwords (ceiling (ash old-length n-bits-shift) n-word-bits))
             (new-nwords (ceiling (ash new-length* n-bits-shift) n-word-bits)))
        (when (< new-nwords old-nwords)
          (with-pinned-objects (vector)
            ;; VECTOR-SAP is only for unboxed vectors. Use the vop directly.
            (let ((data (%primitive vector-sap vector)))
              ;; There is no requirement to zeroize memory corresponding
              ;; to unused array elements.
              ;; However, it's slightly nicer if the padding word (if present) is 0.
              (when (oddp new-nwords)
                (setf (sap-ref-word data (ash new-nwords word-shift)) 0))
              (let* ((aligned-old (align-up old-nwords 2))
                     (aligned-new (align-up new-nwords 2)))
                ;; Only if physically shrunk as determined by PRIMITIVE-OBJECT-SIZE
                ;; will we need to (and have adequate room to) place a filler.
                (when (< aligned-new aligned-old)
                  (let ((diff (- aligned-old aligned-new)))
                    ;; Certainly if the vector is unboxed it can't possibly matter
                    ;; if GC sees this bit pattern prior to setting the new length;
                    ;; but even for SIMPLE-VECTOR, it's OK, it turns out.
                    (setf (sap-ref-word data (ash aligned-new word-shift))
                          (make-filler diff)))))))))))
  ;; Only arrays have fill-pointers, but vectors have their length
  ;; parameter in the same place.
  (setf (%array-fill-pointer vector) new-length)
  vector)

(defun shrink-vector (vector new-length)
  (declare (vector vector))
  (cond
    ((eq (length vector) new-length)
     vector)
    ((array-has-fill-pointer-p vector)
     (setf (%array-fill-pointer vector) new-length)
     vector)
    (t (subseq vector 0 new-length))))

;;; BIG THREAD SAFETY NOTE
;;;
;;; ADJUST-ARRAY/SET-ARRAY-HEADER, and its callees are very
;;; thread unsafe. They are nonatomic, and can mess with parallel
;;; code using the same arrays.
;;;
;;; A likely seeming fix is an additional level of indirection:
;;; ARRAY-HEADER -> ARRAY-INFO -> ... where ARRAY-HEADER would
;;; hold nothing but the pointer to ARRAY-INFO, and ARRAY-INFO
;;; would hold everything ARRAY-HEADER now holds. This allows
;;; consing up a new ARRAY-INFO and replacing it atomically in
;;; the ARRAY-HEADER.
;;;
;;; %WALK-DISPLACED-ARRAY-BACKPOINTERS is an especially nasty
;;; one: not only is it needed extremely rarely, which makes
;;; any thread safety bugs involving it look like rare random
;;; corruption, but because it walks the chain *upwards*, which
;;; may violate user expectations.

;;; Fill in array header with the provided information, and return the array.
(defun set-array-header (array data length fill-pointer displacement dimensions
                         displacedp newp)
  (labels ((%walk-displaced-array-backpointers (array new-length)
             (dolist (p (%array-displaced-from array))
               (let ((from (weak-pointer-value p)))
                 (when (and from (eq array (%array-data from)))
                   (let ((requires (+ (%array-available-elements from)
                                      (%array-displacement from))))
                     (unless (>= new-length requires)
                       ;; ANSI sayeth (ADJUST-ARRAY dictionary entry):
                       ;;
                       ;;   "If A is displaced to B, the consequences are unspecified if B is
                       ;;   adjusted in such a way that it no longer has enough elements to
                       ;;   satisfy A.
                       ;;
                       ;; since we're hanging on a weak pointer here, we can't signal an
                       ;; error right now: the array that we're looking at might be
                       ;; garbage. Instead, we set all dimensions to zero so that next
                       ;; safe access to the displaced array will trap. Additionally, we
                       ;; save the original dimensions, so we can signal a more
                       ;; understandable error when the time comes.
                       (%walk-displaced-array-backpointers from 0)
                       (setf (%array-fill-pointer from) 0
                             (%array-available-elements from) 0
                             (%array-displaced-p from) (array-dimensions array))
                       (dotimes (i (%array-rank from))
                         (%set-array-dimension from i 0)))))))))
    (if newp
        (setf (%array-displaced-from array) nil)
        (%walk-displaced-array-backpointers array length))
    (when displacedp
      (%save-displaced-array-backpointer array data))
    (setf (%array-data array) data)
    (setf (%array-available-elements array) length)
    (cond (fill-pointer
           (setf (%array-fill-pointer array) fill-pointer)
           (logior-array-flags array +array-fill-pointer-p+))
          (t
           (setf (%array-fill-pointer array) length)
           (reset-array-flags array +array-fill-pointer-p+)))
    (setf (%array-displacement array) displacement)
    (populate-dimensions array dimensions (array-rank array))
    (setf (%array-displaced-p array) displacedp)
    array))

;;; User visible extension
(declaim (ftype (sfunction (array) (simple-array * (*))) array-storage-vector))
(defun array-storage-vector (array)
  "Returns the underlying storage vector of ARRAY, which must be a non-displaced array.

In SBCL, if ARRAY is a of type \(SIMPLE-ARRAY * \(*)), it is its own storage
vector. Multidimensional arrays, arrays with fill pointers, and adjustable
arrays have an underlying storage vector with the same ARRAY-ELEMENT-TYPE as
ARRAY, which this function returns.

Important note: the underlying vector is an implementation detail. Even though
this function exposes it, changes in the implementation may cause this
function to be removed without further warning."
  ;; KLUDGE: Without TRULY-THE the system is not smart enough to figure out that
  ;; the return value is always of the known type.
  (truly-the (simple-array * (*))
             (cond ((not (array-header-p array))
                    array)
                   ((%array-displaced-p array)
                    (error "~S cannot be used with displaced arrays. Use ~S instead."
                           'array-storage-vector 'array-displacement))
                   (t
                    (%array-data array)))))


;;;; ZAP-ARRAY-DATA for ADJUST-ARRAY

;;; This does the grinding work for ADJUST-ARRAY. It zaps the data
;;; from the OLD-DATA in an arrangement specified by the OLD-DIMS to
;;; the NEW-DATA in an arrangement specified by the NEW-DIMS. OFFSET
;;; is a displaced offset to be added to computed indices of OLD-DATA.
(defun zap-array-data (old-data old-dims offset new-data new-dims new-length
                       element-type initial-element initial-element-p)
  (declare (list old-dims new-dims)
           (fixnum new-length))
  ;; OLD-DIMS comes from array-dimensions, which returns a fresh list
  ;; at least in SBCL.
  ;; NEW-DIMS comes from the user.
  (setf old-dims (nreverse old-dims)
        new-dims (reverse new-dims))
  (cond ((eq old-data new-data)
         ;; NEW-LENGTH, ELEMENT-TYPE, INITIAL-ELEMENT, and
         ;; INITIAL-ELEMENT-P are used when OLD-DATA and NEW-DATA are
         ;; EQ; in this case, a temporary must be used and filled
         ;; appropriately. specified initial-element.
         ;; FIXME: transforming this TYPEP to someting a bit faster
         ;; would be a win...
         (unless (or (not initial-element-p)
                     (typep initial-element element-type))
           (error "~S can't be used to initialize an array of type ~
                    ~/sb-impl:print-type-specifier/."
                  initial-element element-type))
         (let ((temp (if initial-element-p
                         (make-array new-length :initial-element initial-element)
                         (make-array new-length))))
           (declare (simple-vector temp))
           (zap-array-data-aux old-data old-dims offset temp new-dims)
           (dotimes (i new-length)
             (setf (aref new-data i) (aref temp i)))
           ;; Kill the temporary vector to prevent garbage retention.
           (%shrink-vector temp 0)))
        (t
         ;; When OLD-DATA and NEW-DATA are not EQ, NEW-DATA has
         ;; already been filled with any
         (zap-array-data-aux old-data old-dims offset new-data new-dims))))

(defun zap-array-data-aux (old-data old-dims offset new-data new-dims)
  (declare (fixnum offset))
  (let ((limits (mapcar (lambda (x y)
                          (declare (fixnum x y))
                          (1- (the fixnum (min x y))))
                        old-dims new-dims)))
    (macrolet ((bump-index-list (index limits)
                 `(do ((subscripts ,index (cdr subscripts))
                       (limits ,limits (cdr limits)))
                      ((null subscripts) :eof)
                    (cond ((< (the fixnum (car subscripts))
                              (the fixnum (car limits)))
                           (rplaca subscripts
                                   (1+ (the fixnum (car subscripts))))
                           (return ,index))
                          (t (rplaca subscripts 0))))))
      (do ((index (make-list (length old-dims) :initial-element 0)
                  (bump-index-list index limits)))
          ((eq index :eof))
        (setf (aref new-data (row-major-index-from-dims index new-dims))
              (aref old-data
                    (+ (the fixnum (row-major-index-from-dims index old-dims))
                       offset)))))))

;;; Figure out the row-major-order index of an array reference from a
;;; list of subscripts and a list of dimensions. This is for internal
;;; calls only, and the subscripts and dim-list variables are assumed
;;; to be reversed from what the user supplied.
(defun row-major-index-from-dims (rev-subscripts rev-dim-list)
  (do ((rev-subscripts rev-subscripts (cdr rev-subscripts))
       (rev-dim-list rev-dim-list (cdr rev-dim-list))
       (chunk-size 1)
       (result 0))
      ((null rev-dim-list) result)
    (declare (fixnum chunk-size result))
    (setq result (+ result
                    (the fixnum (* (the fixnum (car rev-subscripts))
                                   chunk-size))))
    (setq chunk-size (* chunk-size (the fixnum (car rev-dim-list))))))

;;;; some bit stuff

(defun bit-array-same-dimensions-p (array1 array2)
  (declare (type (array bit) array1 array2))
  (let ((rank (array-rank array1)))
    (and (= rank (array-rank array2))
         (if (= rank 1)
             (= (array-total-size array1)
                (array-total-size array2))
             (dotimes (index rank t)
               (when (/= (%array-dimension array1 index)
                         (%array-dimension array2 index))
                 (return nil)))))))

(defun copy-array-header (array)
  (let* ((rank (%array-rank array))
         (size (%array-available-elements array))
         (result (make-array-header simple-array-widetag
                                    rank)))
    (loop for i below rank
          do (%set-array-dimension result i
                                   (%array-dimension array i)))
    ;; fill-pointer-p defaults to 0
    (setf (%array-displaced-from result) nil
          (%array-displaced-p result) nil
          (%array-fill-pointer result) size
          (%array-available-elements result) size)
    result))

(defun pick-result-array (result-bit-array bit-array-1)
  (case result-bit-array
    ((t) bit-array-1)
    ((nil)
     (if (vectorp bit-array-1)
         (make-array (array-total-size bit-array-1)
                     :element-type 'bit
                     :initial-element 0)
         (let ((header (copy-array-header bit-array-1)))
           (setf (%array-data header)
                 (make-array (%array-available-elements bit-array-1)
                             :element-type 'bit
                             :initial-element 0))
           header)))
    (t
     (unless (bit-array-same-dimensions-p bit-array-1
                                          result-bit-array)
       (error "~S and ~S don't have the same dimensions."
              bit-array-1 result-bit-array))
     result-bit-array)))

;;; This used to be a DEFMACRO, but depending on the target's support for Unicode,
;;; it got a constant-folding-error in the FORMAT call when producing the load-time
;;; macro. CONCATENATE-FORMAT-P returns true, so then we want to know whether the
;;; result is a base-string which entails calling SB-KERNEL:SIMPLE-BASE-STRING-P
;;; which has no definition in the cross-compiler. (We could add one of course)

;;; Bit array operations are allowed to leave arbitrary values in the
;;; trailing bits of the result. Examples:
;;; * (format t "~b~%" (%vector-raw-bits (bit-not #*1001) 0))
;;;   1111111111111111111111111111111111111111111111111111111111110110
;;; * (format t "~b~%" (%vector-raw-bits (bit-nor #*1001 #*1010) 0))
;;;   1111111111111111111111111111111111111111111111111111111111110010
;;; But because reading is more common than writing, it seems that a better
;;; technique might be to enforce an invariant that the last word contain 0
;;; in all unused bits so that EQUAL and SXHASH become far simpler.

(macrolet ((def-bit-array-op (name function)
  `(defun ,name (bit-array-1 bit-array-2 &optional result-bit-array)
     ,(format nil
              "Perform a bit-wise ~A on the elements of BIT-ARRAY-1 and ~
               BIT-ARRAY-2,~%  putting the results in RESULT-BIT-ARRAY. ~
               If RESULT-BIT-ARRAY is T,~%  BIT-ARRAY-1 is used. If ~
               RESULT-BIT-ARRAY is NIL or omitted, a new array is~%  created. ~
               All the arrays must have the same rank and dimensions."
              (symbol-name function))
     (declare (type (array bit) bit-array-1 bit-array-2)
              (type (or (array bit) (member t nil)) result-bit-array))
     (unless (bit-array-same-dimensions-p bit-array-1 bit-array-2)
       (error "~S and ~S don't have the same dimensions."
              bit-array-1 bit-array-2))
     (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
       (if (and (simple-bit-vector-p bit-array-1)
                (simple-bit-vector-p bit-array-2)
                (simple-bit-vector-p result-bit-array))
           (locally (declare (optimize (speed 3) (safety 0)))
             (,name bit-array-1 bit-array-2 result-bit-array))
           (with-array-data ((data1 bit-array-1) (start1) (end1))
             (with-array-data ((data2 bit-array-2) (start2) (end2))
               (with-array-data ((data3 result-bit-array) (start3) (end3))
                 (if (and (zerop start1)
                          (zerop start2)
                          (zerop start3)
                          (= (length data1) end1)
                          (= (length data2) end2)
                          (= (length data3) end3))
                     (locally (declare (optimize (speed 3) (safety 0)))
                       (,name data1 data2 data3))
                     (do ((index-1 start1 (1+ index-1))
                          (index-2 start2 (1+ index-2))
                          (index-3 start3 (1+ index-3)))
                         ((>= index-3 end3))
                       (declare (type index index-1 index-2 index-3))
                       (setf (sbit data3 index-3)
                             (logand (,function (sbit data1 index-1)
                                                (sbit data2 index-2))
                                     1))))
                 result-bit-array))))))))

(def-bit-array-op bit-and logand)
(def-bit-array-op bit-ior logior)
(def-bit-array-op bit-xor logxor)
(def-bit-array-op bit-eqv logeqv)
(def-bit-array-op bit-nand lognand)
(def-bit-array-op bit-nor lognor)
(def-bit-array-op bit-andc1 logandc1)
(def-bit-array-op bit-andc2 logandc2)
(def-bit-array-op bit-orc1 logorc1)
(def-bit-array-op bit-orc2 logorc2)
) ; end MACROLET

(defun bit-not (bit-array &optional result-bit-array)
  "Performs a bit-wise logical NOT on the elements of BIT-ARRAY,
  putting the results in RESULT-BIT-ARRAY. If RESULT-BIT-ARRAY is T,
  BIT-ARRAY is used. If RESULT-BIT-ARRAY is NIL or omitted, a new array is
  created. Both arrays must have the same rank and dimensions."
  (declare (type (array bit) bit-array)
           (type (or (array bit) (member t nil)) result-bit-array))
  (let ((result-bit-array (pick-result-array result-bit-array bit-array)))
    (if (and (simple-bit-vector-p bit-array)
             (simple-bit-vector-p result-bit-array))
        (locally (declare (optimize (speed 3) (safety 0)))
          (bit-not bit-array result-bit-array))
        (with-array-data ((src bit-array) (src-start) (src-end))
          (with-array-data ((dst result-bit-array) (dst-start) (dst-end))
            (if (and (zerop src-start)
                     (zerop dst-start)
                     (= src-end (length src))
                     (= dst-end (length dst)))
                (locally (declare (optimize (speed 3) (safety 0)))
                  (bit-not src dst))
                (do ((src-index src-start (1+ src-index))
                     (dst-index dst-start (1+ dst-index)))
                    ((>= dst-index dst-end))
                  (declare (type index src-index dst-index))
                  (setf (sbit dst dst-index)
                        (logxor (sbit src src-index) 1))))
            result-bit-array)))))

;;;; array type dispatching

;;; Given DISPATCH-FOO as the DISPATCH-NAME argument (unevaluated),
;;; defines the functions
;;;
;;; DISPATCH-FOO/SIMPLE-BASE-STRING
;;; DISPATCH-FOO/SIMPLE-CHARACTER-STRING
;;; DISPATCH-FOO/SIMPLE-ARRAY-SINGLE-FLOAT
;;; ...
;;;
;;; PARAMS are the function parameters in the definition of each
;;; specializer function. The array being specialized must be the
;;; first parameter in PARAMS. A type declaration for this parameter
;;; is automatically inserted into the body of each function.
;;;
;;; The dispatch table %%FOO-FUNS%% is defined and populated by these
;;; functions. The table is padded by the function
;;; HAIRY-FOO-DISPATCH-ERROR, also defined by DEFINE-ARRAY-DISPATCH.
;;;
;;; Finally, the DISPATCH-FOO macro is defined which does the actual
;;; dispatching when called. It expects arguments that match PARAMS.
;;;
(defmacro sb-impl::!define-array-dispatch (style dispatch-name params nil-array &body body)
  #-(or x86 x86-64 arm64) (setq style :call)
  (let ((table-name (symbolicate "%%" dispatch-name "-FUNS%%"))
        (error-name (symbolicate "HAIRY-" dispatch-name "-ERROR")))
    (declare (ignorable table-name))
    `(progn
       (defun ,error-name (,(first params) &rest rest)
         (declare (ignore rest))
         (error 'type-error
                :datum ,(first params)
                :expected-type '(simple-array * (*))))

       ,@(ecase style
    (:call
     `((define-load-time-global ,table-name ,(sb-xc:make-array (1+ widetag-mask)))

       ;; This SUBSTITUTE call happens ** after ** all the SETFs below it.
       ;; DEFGLOBAL's initial value is dumped by genesis as a vector filled
       ;; with 0 (it would not work if the vector held function objects).
       ;; Then the SETFs happen, as cold-load can process %SVSET, which
       ;; is great, because it means that hairy sequence dispatch may occur
       ;; as early as you'd like in cold-init without regard to file order.
       ;; However when it comes to actually executing the toplevel forms
       ;; that were compiled into thunks of target code to invoke,
       ;; all the known good entries must be preserved.
       (nsubstitute #',error-name 0 ,table-name)

       ,@(loop for info across *specialized-array-element-type-properties*
               for typecode = (saetp-typecode info)
               for specifier = (saetp-specifier info)
               for primitive-type-name = (saetp-primitive-type-name info)
               collect (let ((fun-name (symbolicate (string dispatch-name)
                                                    "/" primitive-type-name)))
                         `(progn
                            (defun ,fun-name ,params
                              (declare (type (simple-array ,specifier (*))
                                             ,(first params)))
                              ,@(if (null specifier)
                                    nil-array
                                    body))
                            (setf (svref ,table-name ,typecode) #',fun-name))))
       (defmacro ,dispatch-name (&rest args)
         (aver (symbolp (first args)))
         (let ((tag (gensym "TAG")))
           `(,',(if (find '&rest params)
                    'apply
                    'funcall)
             (truly-the function
               (let ((,tag 0))
                 (when (%other-pointer-p ,(first args))
                   (setf ,tag (%other-pointer-widetag ,(first args))))
                 (svref (truly-the (simple-vector 256) (load-time-value ,',table-name t))
                        ,tag)))
             ,@args)))))
    (:jump-table
     (multiple-value-bind (body decls) (parse-body body nil)
       `((declaim (inline ,dispatch-name))
         (defun ,dispatch-name ,params
           (declare (optimize (sb-c:jump-table 3)))
           ,@decls
           (case (if (%other-pointer-p ,(first params))
                     (ash (%other-pointer-widetag ,(first params)) -2)
                     0)
             ,@(loop
                 for info across *specialized-array-element-type-properties*
                 for specifier = (saetp-specifier info)
                 collect `(,(ash (saetp-typecode info) -2)
                           (let ((,(first params)
                                   (truly-the (simple-array ,specifier (*))
                                              ,(first params))))
                             ,@(if (null specifier)
                                   nil-array
                                   body))))
             (t
              (,error-name ,@params)))))))))))

(defun sb-kernel::check-array-shape (array dimensions)
  (when (let ((dimensions dimensions))
          (dotimes (i (array-rank array))
            (unless (eql (array-dimension array i) (pop dimensions))
              (return t))))
    (error "malformed :INITIAL-CONTENTS: ~S should have dimensions ~S"
           (make-array dimensions :displaced-to (%array-data array)
                                  :element-type (array-element-type array))
           (array-dimensions array)))
  array)

;;; Horrible kludge for the "static-vectors" system
;;; which uses an internal symbol in SB-IMPL.
(import '%vector-widetag-and-n-bits-shift 'sb-impl)
