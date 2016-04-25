;;;; functions to implement arrays

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

#!-sb-fluid
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
  (def %array-fill-pointer-p)
  (def %array-available-elements)
  (def %array-data-vector)
  (def %array-displacement)
  (def %array-displaced-p)
  (def %array-displaced-from))

(defun %array-rank (array)
  (%array-rank array))

(defun %array-dimension (array axis)
  (%array-dimension array axis))

(defun %set-array-dimension (array axis value)
  (%set-array-dimension array axis value))

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
  (%with-array-data-macro array start end :check-bounds t :check-fill-pointer nil))

(defun %data-vector-and-index (array index)
  (if (array-header-p array)
      (multiple-value-bind (vector index)
          (%with-array-data array index nil)
        (values vector index))
      (values array index)))

;;;; MAKE-ARRAY
(defun %integer-vector-widetag-and-n-bits (signed high)
  (let ((unsigned-table
          #.(let ((map (make-array (1+ sb!vm:n-word-bits))))
              (loop for saetp across
                    (reverse sb!vm:*specialized-array-element-type-properties*)
                    for ctype = (sb!vm:saetp-ctype saetp)
                    when (and (numeric-type-p ctype)
                              (eq (numeric-type-class ctype) 'integer)
                              (zerop (numeric-type-low ctype)))
                    do (fill map (cons (sb!vm:saetp-typecode saetp)
                                       (sb!vm:saetp-n-bits saetp))
                             :end (1+ (integer-length (numeric-type-high ctype)))))
              map))
        (signed-table
          #.(let ((map (make-array (1+ sb!vm:n-word-bits))))
              (loop for saetp across
                    (reverse sb!vm:*specialized-array-element-type-properties*)
                    for ctype = (sb!vm:saetp-ctype saetp)
                    when (and (numeric-type-p ctype)
                              (eq (numeric-type-class ctype) 'integer)
                              (minusp (numeric-type-low ctype)))
                    do (fill map (cons (sb!vm:saetp-typecode saetp)
                                       (sb!vm:saetp-n-bits saetp))
                             :end (+ (integer-length (numeric-type-high ctype)) 2)))
              map)))
    (cond ((> high sb!vm:n-word-bits)
           (values #.sb!vm:simple-vector-widetag #.sb!vm:n-word-bits))
          (signed
           (let ((x (aref signed-table high)))
             (values (car x) (cdr x))))
          (t
           (let ((x (aref unsigned-table high)))
             (values (car x) (cdr x)))))))

;;; This is a bit complicated, but calling subtypep over all
;;; specialized types is exceedingly slow
(defun %vector-widetag-and-n-bits (type)
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
             (result (widetag)
               (let ((value (symbol-value widetag)))
                 `(values ,value
                          ,(sb!vm:saetp-n-bits
                            (find value
                                  sb!vm:*specialized-array-element-type-properties*
                                  :key #'sb!vm:saetp-typecode))))))
    (flet ((ill-type ()
             (error "Invalid type specifier: ~s" type))
           (integer-interval-widetag (low high)
             (if (minusp low)
                 (%integer-vector-widetag-and-n-bits
                  t
                  (1+ (max (integer-length low) (integer-length high))))
                 (%integer-vector-widetag-and-n-bits
                  nil
                  (max (integer-length low) (integer-length high))))))
      (let* ((consp (consp type))
             (type-name (if consp
                            (car type)
                            type)))
        (case type-name
          ((t)
           (when consp
             (ill-type))
           (result sb!vm:simple-vector-widetag))
          ((base-char standard-char #!-sb-unicode character)
           (when consp
             (ill-type))
           (result sb!vm:simple-base-string-widetag))
          #!+sb-unicode
          ((character extended-char)
           (when consp
             (ill-type))
           (result sb!vm:simple-character-string-widetag))
          (bit
           (when consp
             (ill-type))
           (result sb!vm:simple-bit-vector-widetag))
          (fixnum
           (when consp
             (ill-type))
           (result sb!vm:simple-array-fixnum-widetag))
          (unsigned-byte
           (with-parameters ((integer 1)) (high)
             (if (eq high '*)
                 (result sb!vm:simple-vector-widetag)
                 (%integer-vector-widetag-and-n-bits nil high))))
          (signed-byte
           (with-parameters ((integer 1)) (high)
             (if (eq high '*)
                 (result sb!vm:simple-vector-widetag)
                 (%integer-vector-widetag-and-n-bits t high))))
          (double-float
           (with-parameters (double-float :intervals t) (low high)
             (if (and (not (eq low '*))
                      (not (eq high '*))
                      (if (or (consp low) (consp high))
                          (>= (type-bound-number low) (type-bound-number high))
                          (> low high)))
                 (result sb!vm:simple-array-nil-widetag)
                 (result sb!vm:simple-array-double-float-widetag))))
          (single-float
           (with-parameters (single-float :intervals t) (low high)
             (if (and (not (eq low '*))
                      (not (eq high '*))
                      (if (or (consp low) (consp high))
                          (>= (type-bound-number low) (type-bound-number high))
                          (> low high)))
                 (result sb!vm:simple-array-nil-widetag)
                 (result sb!vm:simple-array-single-float-widetag))))
          (mod
           (if (and (consp type)
                    (consp (cdr type))
                    (null (cddr type))
                    (typep (cadr type) '(integer 1)))
               (%integer-vector-widetag-and-n-bits
                nil (integer-length (1- (cadr type))))
               (ill-type)))
          #!+long-float
          (long-float
           (with-parameters (long-float :intervals t) (low high)
             (if (and (not (eq low '*))
                      (not (eq high '*))
                      (if (or (consp low) (consp high))
                          (>= (type-bound-number low) (type-bound-number high))
                          (> low high)))
                 (result sb!vm:simple-array-nil-widetag)
                 (result sb!vm:simple-array-long-float-widetag))))
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
                      (result sb!vm:simple-vector-widetag))
                     ((> low high)
                      (result sb!vm:simple-array-nil-widetag))
                     (t
                      (integer-interval-widetag low high))))))
          (complex
           (with-parameters (t) (subtype)
             (if (eq subtype '*)
                 (result sb!vm:simple-vector-widetag)
                 (let ((ctype (specifier-type type)))
                   (cond ((eq ctype *empty-type*)
                          (result sb!vm:simple-array-nil-widetag))
                         ((union-type-p ctype)
                          (cond ((csubtypep ctype (specifier-type '(complex double-float)))
                                 (result
                                  sb!vm:simple-array-complex-double-float-widetag))
                                ((csubtypep ctype (specifier-type '(complex single-float)))
                                 (result
                                  sb!vm:simple-array-complex-single-float-widetag))
                                #!+long-float
                                ((csubtypep ctype (specifier-type '(complex long-float)))
                                 (result
                                  sb!vm:simple-array-complex-long-float-widetag))
                                (t
                                 (result sb!vm:simple-vector-widetag))))
                         (t
                          (case (numeric-type-format ctype)
                            (double-float
                             (result
                              sb!vm:simple-array-complex-double-float-widetag))
                            (single-float
                             (result
                              sb!vm:simple-array-complex-single-float-widetag))
                            #!+long-float
                            (long-float
                             (result
                              sb!vm:simple-array-complex-long-float-widetag))
                            (t
                             (result sb!vm:simple-vector-widetag)))))))))
          ((nil)
           (result sb!vm:simple-array-nil-widetag))
          (t
           (block nil
             (let ((ctype (type-or-nil-if-unknown type)))
               (unless ctype
                 (return (result sb!vm:simple-vector-widetag)))
               (typecase ctype
                 (union-type
                  (let ((types (union-type-types ctype)))
                    (cond ((not (every #'numeric-type-p types))
                           (result sb!vm:simple-vector-widetag))
                          ((csubtypep ctype (specifier-type 'integer))
                           (integer-interval-widetag
                            (reduce #'min types :key #'numeric-type-low)
                            (reduce #'max types :key #'numeric-type-high)))
                          ((csubtypep ctype (specifier-type 'double-float))
                           (result sb!vm:simple-array-double-float-widetag))
                          ((csubtypep ctype (specifier-type 'single-float))
                           (result sb!vm:simple-array-single-float-widetag))
                          #!+long-float
                          ((csubtypep ctype (specifier-type 'long-float))
                           (result sb!vm:simple-array-long-float-widetag))
                          (t
                           (result sb!vm:simple-vector-widetag)))))
                 (character-set-type
                  #!-sb-unicode (result sb!vm:simple-base-string-widetag)
                  #!+sb-unicode
                  (if (loop for (start . end)
                            in (character-set-type-pairs ctype)
                            always (and (< start base-char-code-limit)
                                        (< end base-char-code-limit)))
                      (result sb!vm:simple-base-string-widetag)
                      (result sb!vm:simple-character-string-widetag)))
                 (t
                  (let ((expansion (type-specifier ctype)))
                    (if (equal expansion type)
                        (result sb!vm:simple-vector-widetag)
                        (%vector-widetag-and-n-bits expansion)))))))))))))

(defun %complex-vector-widetag (widetag)
  (macrolet ((make-case ()
               `(case widetag
                  ,@(loop for saetp across sb!vm:*specialized-array-element-type-properties*
                          for complex = (sb!vm:saetp-complex-typecode saetp)
                          when complex
                          collect (list (sb!vm:saetp-typecode saetp) complex))
                  (t
                   #.sb!vm:complex-vector-widetag))))
    (make-case)))

(defglobal %%simple-array-n-bits%% (make-array (1+ sb!vm:widetag-mask)))
#.(loop for info across sb!vm:*specialized-array-element-type-properties*
        collect `(setf (aref %%simple-array-n-bits%% ,(sb!vm:saetp-typecode info))
                       ,(sb!vm:saetp-n-bits info)) into forms
        finally (return `(progn ,@forms)))

(declaim (type (simple-vector #.(1+ sb!vm:widetag-mask)) %%simple-array-n-bits%%))

(defun allocate-vector-with-widetag (widetag length &optional n-bits)
  (declare (type (unsigned-byte 8) widetag)
           (type index length))
  (let ((n-bits (or n-bits (aref %%simple-array-n-bits%% widetag))))
    (declare (type (integer 0 256) n-bits))
    (allocate-vector widetag length
                     (ceiling
                      (* (if (or (= widetag sb!vm:simple-base-string-widetag)
                                 #!+sb-unicode
                                 (= widetag
                                    sb!vm:simple-character-string-widetag))
                             (1+ length)
                             length)
                         n-bits)
                      sb!vm:n-word-bits))))

(defun array-underlying-widetag (array)
  (macrolet ((make-case ()
               `(case widetag
                  ,@(loop for saetp across sb!vm:*specialized-array-element-type-properties*
                          for complex = (sb!vm:saetp-complex-typecode saetp)
                          when complex
                          collect (list complex (sb!vm:saetp-typecode saetp)))
                  ((,sb!vm:simple-array-widetag
                    ,sb!vm:complex-vector-widetag
                    ,sb!vm:complex-array-widetag)
                   (with-array-data ((array array) (start) (end))
                     (declare (ignore start end))
                     (%other-pointer-widetag array)))
                  (t
                   widetag))))
    (let ((widetag (%other-pointer-widetag array)))
      (make-case))))

(defun make-vector-like (vector length)
  (allocate-vector-with-widetag (array-underlying-widetag vector) length))

;; Complain in various ways about wrong :INITIAL-foo arguments,
;; returning the two initialization arguments needed for DATA-VECTOR-FROM-INITS.
(defun validate-array-initargs (element-p element contents-p contents displaced)
  (cond ((and displaced (or element-p contents-p))
         (if (and element-p contents-p)
             (error "Neither :INITIAL-ELEMENT nor :INITIAL-CONTENTS ~
                     may be specified with the :DISPLACED-TO option")
             (error "~S may not be specified with the :DISPLACED-TO option"
                    (if element-p :initial-element :initial-contents))))
        ((and element-p contents-p)
         (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
        (element-p  (values :initial-element element))
        (contents-p (values :initial-contents contents))
        (t          (values nil nil))))

(declaim (inline %save-displaced-array-backpointer))
(defun %save-displaced-array-backpointer (array data)
  (flet ((purge (pointers)
           (remove-if (lambda (value)
                        (or (not value) (eq array value)))
                      pointers
                      :key #'weak-pointer-value)))
    ;; Add backpointer to the new data vector if it has a header.
    (when (array-header-p data)
      (setf (%array-displaced-from data)
            (cons (make-weak-pointer array)
                  (purge (%array-displaced-from data)))))
    ;; Remove old backpointer, if any.
    (let ((old-data (%array-data-vector array)))
      (when (and (neq data old-data) (array-header-p old-data))
        (setf (%array-displaced-from old-data)
              (purge (%array-displaced-from old-data)))))))

;;; Widetag is the widetag of the underlying vector,
;;; it'll be the same as the resulting array widetag only for simple vectors
(defun %make-array (dimensions widetag n-bits
                    &key
                      element-type
                      (initial-element nil initial-element-p)
                      (initial-contents nil initial-contents-p)
                      adjustable fill-pointer
                      displaced-to displaced-index-offset)
  (declare (ignore element-type))
  (binding* (((array-rank dimension-0)
              (if (listp dimensions)
                  (values (length dimensions)
                          (if dimensions (car dimensions) 1))
                  (values 1 dimensions)))
             ((initialize initial-data)
              (validate-array-initargs initial-element-p initial-element
                                       initial-contents-p initial-contents
                                       displaced-to))
             (simple (and (null fill-pointer)
                          (not adjustable)
                          (null displaced-to))))
    (declare (type array-rank array-rank))
    (declare (type index dimension-0))
    (cond ((and displaced-index-offset (null displaced-to))
           (error "can't specify :DISPLACED-INDEX-OFFSET without :DISPLACED-TO"))
          ((and simple (= array-rank 1))
           (let ((vector ; a (SIMPLE-ARRAY * (*))
                  (allocate-vector-with-widetag widetag dimension-0 n-bits)))
             ;; presence of at most one :INITIAL-thing keyword was ensured above
             (cond (initial-element-p
                    (fill vector initial-element))
                   (initial-contents-p
                    (let ((content-length (length initial-contents)))
                      (unless (= dimension-0 content-length)
                        (error "There are ~W elements in the :INITIAL-CONTENTS, but ~
                                the vector length is ~W."
                               content-length dimension-0)))
                    (replace vector initial-contents)))
             vector))
          ((and (arrayp displaced-to)
                (/= (array-underlying-widetag displaced-to) widetag))
           (error "Array element type of :DISPLACED-TO array does not match specified element type"))
          (t
           ;; it's non-simple or multidimensional, or both.
           (when fill-pointer
             (unless (= array-rank 1)
               (error "Only vectors can have fill pointers."))
             (when (and (integerp fill-pointer) (> fill-pointer dimension-0))
               ;; FIXME: should be TYPE-ERROR?
               (error "invalid fill-pointer ~W" fill-pointer)))
           (let* ((total-size
                   (if (consp dimensions)
                       (the index (reduce (lambda (a b) (* a (the index b)))
                                          dimensions))
                       ;; () is considered to have dimension-0 = 1.
                       ;; It avoids the REDUCE lambda being called with no args.
                       dimension-0))
                  (data (or displaced-to
                            (data-vector-from-inits
                             dimensions total-size nil widetag n-bits
                             initialize initial-data)))
                  (array (make-array-header
                          (cond ((= array-rank 1)
                                 (%complex-vector-widetag widetag))
                                (simple sb!vm:simple-array-widetag)
                                (t sb!vm:complex-array-widetag))
                          array-rank)))
             (if fill-pointer
                 (setf (%array-fill-pointer-p array) t
                       (%array-fill-pointer array)
                       (if (eq fill-pointer t) dimension-0 fill-pointer))
                 (setf (%array-fill-pointer-p array) nil
                       (%array-fill-pointer array) total-size))
             (setf (%array-available-elements array) total-size)
             ;; Terrible name for this slot - we displace to the
             ;; target array's header, if any, not the "ultimate"
             ;; vector in the chain of displacements.
             (setf (%array-data-vector array) data)
             (setf (%array-displaced-from array) nil)
             (cond (displaced-to
                    (let ((offset (or displaced-index-offset 0)))
                      (when (> (+ offset total-size)
                               (array-total-size displaced-to))
                        (error "~S doesn't have enough elements." displaced-to))
                      (setf (%array-displacement array) offset)
                      (setf (%array-displaced-p array) t)
                      (%save-displaced-array-backpointer array data)))
                   (t
                    (setf (%array-displaced-p array) nil)))
             (if (listp dimensions)
                 (let ((dims dimensions)) ; avoid "prevents use of assertion"
                   (dotimes (axis array-rank)
                     (setf (%array-dimension array axis) (pop dims))))
                 (setf (%array-dimension array 0) dimension-0))
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
  (multiple-value-bind (widetag n-bits) (%vector-widetag-and-n-bits element-type)
    (apply #'%make-array dimensions widetag n-bits args)))

(defun make-static-vector (length &key
                           (element-type '(unsigned-byte 8))
                           (initial-contents nil initial-contents-p)
                           (initial-element nil initial-element-p))
  #!+sb-doc
  "Allocate vector of LENGTH elements in static space. Only allocation
of specialized arrays is supported."
  ;; STEP 1: check inputs fully
  ;;
  ;; This way of doing explicit checks before the vector is allocated
  ;; is expensive, but probably worth the trouble as once we've allocated
  ;; the vector we have no way to get rid of it anymore...
  (when (eq t (upgraded-array-element-type element-type))
    (error "Static arrays of type ~S not supported."
           element-type))
  (validate-array-initargs initial-element-p initial-element
                           initial-contents-p initial-contents nil) ; for effect
  (when initial-contents-p
    (unless (= length (length initial-contents))
      (error "There are ~W elements in the :INITIAL-CONTENTS, but the ~
              vector length is ~W."
             (length initial-contents)
             length))
    (unless (every (lambda (x) (typep x element-type)) initial-contents)
      (error ":INITIAL-CONTENTS contains elements not of type ~S."
             element-type)))
  (when initial-element-p
    (unless (typep initial-element element-type)
      (error ":INITIAL-ELEMENT ~S is not of type ~S."
             initial-element element-type)))
  ;; STEP 2
  ;;
  ;; Allocate and possibly initialize the vector.
  (multiple-value-bind (type n-bits)
      (%vector-widetag-and-n-bits element-type)
    (let ((vector
            (allocate-static-vector type length
                                    (ceiling (* length n-bits)
                                             sb!vm:n-word-bits))))
      (cond (initial-element-p
             (fill vector initial-element))
            (initial-contents-p
             (replace vector initial-contents))
            (t
             vector)))))

;;; DATA-VECTOR-FROM-INITS returns a simple vector that has the
;;; specified array characteristics. Dimensions is only used to pass
;;; to FILL-DATA-VECTOR for error checking on the structure of
;;; initial-contents.
(defun data-vector-from-inits (dimensions total-size
                               element-type widetag n-bits
                               initialize initial-data)
    ;; FIXME: element-type can be NIL when widetag is non-nil,
    ;; and FILL will check the type, although the error will be not as nice.
    ;; (cond (typep initial-element element-type)
    ;;   (error "~S cannot be used to initialize an array of type ~S."
    ;;          initial-element element-type))
  (let ((data (if widetag
                  (allocate-vector-with-widetag widetag total-size n-bits)
                  (make-array total-size :element-type element-type))))
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
  #!+sb-doc
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
(macrolet ((def (name table-name)
             `(progn
                (defglobal ,table-name (make-array ,(1+ sb!vm:widetag-mask)))
                (declaim (type (simple-array function (,(1+ sb!vm:widetag-mask)))
                               ,table-name))
                (defmacro ,name (array-var)
                  `(the function
                     (let ((tag 0))
                       (when (sb!vm::%other-pointer-p ,array-var)
                         (setf tag (%other-pointer-widetag ,array-var)))
                       (svref ,',table-name tag)))))))
  (def !find-data-vector-setter %%data-vector-setters%%)
  (def !find-data-vector-setter/check-bounds %%data-vector-setters/check-bounds%%)
  ;; Used by DO-VECTOR-DATA -- which in turn appears in DOSEQUENCE expansion,
  ;; meaning we can have post-build dependences on this.
  (def %find-data-vector-reffer %%data-vector-reffers%%)
  (def !find-data-vector-reffer/check-bounds %%data-vector-reffers/check-bounds%%))

;;; Like DOVECTOR, but more magical -- can't use this on host.
(defmacro do-vector-data ((elt vector &optional result) &body body)
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
                  ,@(filter-dolist-declarations decls)
                  ,elt
                  ,result))
             (let ((,elt (funcall ,ref ,vec ,index)))
               ,@decls
               (tagbody ,@forms))))))))

(macrolet ((%ref (accessor-getter extra-params)
             `(funcall (,accessor-getter array) array index ,@extra-params))
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
                (defun ,slow-accessor-name (array index ,@extra-params)
                  (declare (optimize speed (safety 0)))
                  (if (not (%array-displaced-p array))
                      ;; The reasonably quick path of non-displaced complex
                      ;; arrays.
                      (let ((array (%array-data-vector array)))
                        (%ref ,accessor-getter ,extra-params))
                      ;; The real slow path.
                      (with-array-data
                          ((vector array)
                           (index (locally
                                      (declare (optimize (speed 1) (safety 1)))
                                    (,@check-bounds index)))
                           (end)
                           :force-inline t)
                        (declare (ignore end))
                        (,accessor-name vector index ,@extra-params)))))))
  (define hairy-data-vector-ref slow-hairy-data-vector-ref
    %find-data-vector-reffer
    nil (progn))
  (define hairy-data-vector-set slow-hairy-data-vector-set
    !find-data-vector-setter
    (new-value) (progn))
  (define hairy-data-vector-ref/check-bounds
      slow-hairy-data-vector-ref/check-bounds
    !find-data-vector-reffer/check-bounds
    nil (check-bound array (array-dimension array 0)))
  (define hairy-data-vector-set/check-bounds
      slow-hairy-data-vector-set/check-bounds
    !find-data-vector-setter/check-bounds
    (new-value) (check-bound array (array-dimension array 0))))

(defun hairy-ref-error (array index &optional new-value)
  (declare (ignore index new-value))
  (error 'type-error
         :datum array
         :expected-type 'vector))

(macrolet ((define-reffer (saetp check-form)
             (let* ((type (sb!vm:saetp-specifier saetp))
                    (atype `(simple-array ,type (*))))
               `(named-lambda (optimized-data-vector-ref ,type) (vector index)
                  (declare (optimize speed (safety 0))
                           ;; Obviously these all coerce raw words to lispobjs
                           ;; so don't keep spewing notes about it.
                           (muffle-conditions compiler-note)
                           (ignorable index))
                  ,(if type
                       `(data-vector-ref (the ,atype vector)
                                         (locally
                                             (declare (optimize (safety 1)))
                                           (the index
                                                (,@check-form index))))
                       `(data-nil-vector-ref (the ,atype vector) index)))))
           (define-setter (saetp check-form)
             (let* ((type (sb!vm:saetp-specifier saetp))
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
                                     (the ,type new-value)))
                  ;; For specialized arrays, the return from
                  ;; data-vector-set would have to be reboxed to be a
                  ;; (Lisp) return value; instead, we use the
                  ;; already-boxed value as the return.
                  new-value)))
           (define-reffers (symbol deffer check-form slow-path)
             `(progn
                ;; FIXME/KLUDGE: can't just FILL here, because genesis doesn't
                ;; preserve the binding, so re-initiaize as NS doesn't have
                ;; the energy to figure out to change that right now.
                (setf ,symbol (make-array (1+ sb!vm::widetag-mask)
                                          :initial-element #'hairy-ref-error))
                ,@(loop for widetag in '(sb!vm:complex-vector-widetag
                                         sb!vm:complex-vector-nil-widetag
                                         sb!vm:complex-bit-vector-widetag
                                         #!+sb-unicode sb!vm:complex-character-string-widetag
                                         sb!vm:complex-base-string-widetag
                                         sb!vm:simple-array-widetag
                                         sb!vm:complex-array-widetag)
                        collect `(setf (svref ,symbol ,widetag) ,slow-path))
                ,@(loop for saetp across sb!vm:*specialized-array-element-type-properties*
                        for widetag = (sb!vm:saetp-typecode saetp)
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
(defun invalid-array-error (array)
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
(defun invalid-array-index-error (array index bound &optional axis)
  (if (invalid-array-p array)
      (invalid-array-error array)
      (error 'invalid-array-index-error
             :array array
             :axis axis
             :datum index
             :expected-type `(integer 0 (,bound)))))

;;; SUBSCRIPTS has a dynamic-extent list structure and is destroyed
(defun %array-row-major-index (array &rest subscripts)
  (declare (truly-dynamic-extent subscripts)
           (array array))
  (let ((length (length subscripts)))
    (cond ((array-header-p array)
           (let ((rank (%array-rank array)))
             (unless (= rank length)
               (error "wrong number of subscripts, ~W, for array of rank ~W."
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
  #!+sb-doc
  "Return T if the SUBSCRIPTS are in bounds for the ARRAY, NIL otherwise."
  (declare (truly-dynamic-extent subscripts))
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
  (declare (truly-dynamic-extent subscripts))
  (apply #'%array-row-major-index array subscripts))

(defun aref (array &rest subscripts)
  #!+sb-doc
  "Return the element of the ARRAY specified by the SUBSCRIPTS."
  (declare (truly-dynamic-extent subscripts))
  (row-major-aref array (apply #'%array-row-major-index array subscripts)))

;;; (setf aref/bit/sbit) are implemented using setf-functions,
;;; because they have to work with (setf (apply #'aref array subscripts))
;;; All other setfs can be done using setf-functions too, but I
;;; haven't found technical advantages or disadvantages for either
;;; scheme.
(defun (setf aref) (new-value array &rest subscripts)
  (declare (truly-dynamic-extent subscripts)
           (type array array))
  (setf (row-major-aref array (apply #'%array-row-major-index array subscripts))
        new-value))

(defun row-major-aref (array index)
  #!+sb-doc
  "Return the element of array corresponding to the row-major index. This is
   SETFable."
  (declare (optimize (safety 1)))
  (row-major-aref array index))

(defun %set-row-major-aref (array index new-value)
  (declare (optimize (safety 1)))
  (setf (row-major-aref array index) new-value))

(defun svref (simple-vector index)
  #!+sb-doc
  "Return the INDEXth element of the given Simple-Vector."
  (declare (optimize (safety 1)))
  (aref simple-vector index))

(defun %svset (simple-vector index new)
  (declare (optimize (safety 1)))
  (setf (aref simple-vector index) new))

(defun bit (bit-array &rest subscripts)
  #!+sb-doc
  "Return the bit from the BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (type (array bit) bit-array)
           (truly-dynamic-extent subscripts)
           (optimize (safety 1)))
  (row-major-aref bit-array (apply #'%array-row-major-index bit-array subscripts)))

(defun (setf bit) (new-value bit-array &rest subscripts)
  (declare (type (array bit) bit-array)
           (type bit new-value)
           (truly-dynamic-extent subscripts)
           (optimize (safety 1)))
  (setf (row-major-aref bit-array
                        (apply #'%array-row-major-index bit-array subscripts))
        new-value))

(defun sbit (simple-bit-array &rest subscripts)
  #!+sb-doc
  "Return the bit from SIMPLE-BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (type (simple-array bit) simple-bit-array)
           (truly-dynamic-extent subscripts)
           (optimize (safety 1)))
  (row-major-aref simple-bit-array
                  (apply #'%array-row-major-index simple-bit-array subscripts)))

(defun (setf sbit) (new-value bit-array &rest subscripts)
  (declare (type (simple-array bit) bit-array)
           (type bit new-value)
           (truly-dynamic-extent subscripts)
           (optimize (safety 1)))
  (setf (row-major-aref bit-array
                        (apply #'%array-row-major-index bit-array subscripts))
        new-value))

;;;; miscellaneous array properties

(defun array-element-type (array)
  #!+sb-doc
  "Return the type of the elements of the array"
  (let ((widetag (%other-pointer-widetag array))
        (table (load-time-value
                (let ((table (make-array 256 :initial-element nil)))
                  (dotimes (i (length sb!vm:*specialized-array-element-type-properties*) table)
                    (let* ((saetp (aref sb!vm:*specialized-array-element-type-properties* i))
                           (typecode (sb!vm:saetp-typecode saetp))
                           (complex-typecode (sb!vm:saetp-complex-typecode saetp))
                           (specifier (sb!vm:saetp-specifier saetp)))
                      (aver (typep specifier '(or list symbol)))
                      (setf (aref table typecode) specifier)
                      (when complex-typecode
                        (setf (aref table complex-typecode) specifier)))))
                t)))
    (let ((result (aref table widetag)))
      (if result
          (truly-the (or list symbol) result)
          ;; (MAKE-ARRAY :ELEMENT-TYPE NIL) goes to this branch, but
          ;; gets the right answer in the end
          (with-array-data ((array array) (start) (end))
            (declare (ignore start end))
            (truly-the (or list symbol) (aref table (%other-pointer-widetag array))))))))

(defun array-rank (array)
  #!+sb-doc
  "Return the number of dimensions of ARRAY."
  (if (array-header-p array)
      (%array-rank array)
      1))

(defun array-dimension (array axis-number)
  #!+sb-doc
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
  #!+sb-doc
  "Return a list whose elements are the dimensions of the array"
  (declare (array array))
  (if (array-header-p array)
      (do ((results nil (cons (array-dimension array index) results))
           (index (1- (array-rank array)) (1- index)))
          ((minusp index) results))
      (list (array-dimension array 0))))

(defun array-total-size (array)
  #!+sb-doc
  "Return the total number of elements in the Array."
  (declare (array array))
  (if (array-header-p array)
      (%array-available-elements array)
      (length (the vector array))))

(defun array-displacement (array)
  #!+sb-doc
  "Return the values of :DISPLACED-TO and :DISPLACED-INDEX-offset
   options to MAKE-ARRAY, or NIL and 0 if not a displaced array."
  (declare (type array array))
  (if (and (array-header-p array) ; if unsimple and
           (%array-displaced-p array)) ; displaced
      (values (%array-data-vector array) (%array-displacement array))
      (values nil 0)))

(defun adjustable-array-p (array)
  #!+sb-doc
  "Return T if and only if calling ADJUST-ARRAY on ARRAY will return
   the identical object."
  (declare (array array))
  ;; Note that this appears not to be a fundamental limitation.
  ;; non-vector SIMPLE-ARRAYs are in fact capable of being adjusted,
  ;; but in practice we test using ADJUSTABLE-ARRAY-P in ADJUST-ARRAY.
  ;; -- CSR, 2004-03-01.
  (not (typep array 'simple-array)))

;;;; fill pointer frobbing stuff

(declaim (inline array-has-fill-pointer-p))
(defun array-has-fill-pointer-p (array)
  #!+sb-doc
  "Return T if the given ARRAY has a fill pointer, or NIL otherwise."
  (declare (array array))
  (and (array-header-p array) (%array-fill-pointer-p array)))

(defun fill-pointer-error (vector &optional arg)
  (cond (arg
         (aver (array-has-fill-pointer-p vector))
         (let ((max (%array-available-elements vector)))
           (error 'simple-type-error
                  :datum arg
                  :expected-type (list 'integer 0 max)
                  :format-control "The new fill pointer, ~S, is larger than the length of the vector (~S.)"
                  :format-arguments (list arg max))))
        (t
         (error 'simple-type-error
                :datum vector
                :expected-type '(and vector (satisfies array-has-fill-pointer-p))
                :format-control "~S is not an array with a fill pointer."
                :format-arguments (list vector)))))

(declaim (inline fill-pointer))
(defun fill-pointer (vector)
  #!+sb-doc
  "Return the FILL-POINTER of the given VECTOR."
  (declare (explicit-check))
  (if (array-has-fill-pointer-p vector)
      (%array-fill-pointer vector)
      (fill-pointer-error vector)))

(defun %set-fill-pointer (vector new)
  (declare (explicit-check))
  (flet ((oops (x)
           (fill-pointer-error vector x)))
    (cond ((not (array-has-fill-pointer-p vector))
           (oops nil))
          ((> new (%array-available-elements vector))
           (oops new))
          (t
            (setf (%array-fill-pointer vector) new)))))

;;; FIXME: It'd probably make sense to use a MACROLET to share the
;;; guts of VECTOR-PUSH between VECTOR-PUSH-EXTEND. Such a macro
;;; should probably be based on the VECTOR-PUSH-EXTEND code (which is
;;; new ca. sbcl-0.7.0) rather than the VECTOR-PUSH code (which dates
;;; back to CMU CL).
(defun vector-push (new-element array)
  #!+sb-doc
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

;;; Widetags of FROM and TO should be equal
(defun copy-vector-data (from to start end n-bits)
  (declare (vector from to)
           (index start end)
           ((integer 0 255) n-bits))
  (let ((from-length (length from)))
    (cond ((simple-vector-p from)
           (replace (truly-the simple-vector to)
                    (truly-the simple-vector from)
                    :start2 start :end2 end))
          ;; Vector sizes are double-word aligned and have zeros in
          ;; the extra word so it's safe to copy when the boundaries
          ;; are matching the whole vector.
          ;; A more generic routine is left for another time, even if
          ;; only handling aligned data since it will avoid consing
          ;; floats or word bignums.
          ((and (= start 0)
                (= end from-length))
           (let ((words (ceiling (* from-length n-bits)
                                 sb!vm:n-word-bits)))
             (loop for i below words
                   do (setf
                       (%vector-raw-bits to i)
                       (%vector-raw-bits from i)))))
          (t
           (replace to
                    from
                    :start2 start :end2 end)))
    to))

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
             (n-bits (aref %%simple-array-n-bits%% widetag))
             (new-data
               (allocate-vector-with-widetag widetag new-length n-bits)))
        (copy-vector-data old-data new-data old-start old-end n-bits)
        (setf (%array-data-vector vector) new-data
              (%array-available-elements vector) new-length
              (%array-fill-pointer vector) fill-pointer
              (%array-displacement vector) 0
              (%array-dimension vector 0) new-length
              (%array-displaced-p vector) nil)
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

(defun vector-pop (array)
  #!+sb-doc
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


;;;; ADJUST-ARRAY

(defun adjust-array (array dimensions &key
                           (element-type (array-element-type array) element-type-p)
                           (initial-element nil initial-element-p)
                           (initial-contents nil initial-contents-p)
                           fill-pointer
                           displaced-to displaced-index-offset)
  #!+sb-doc
  "Adjust ARRAY's dimensions to the given DIMENSIONS and stuff."
  (when (invalid-array-p array)
    (invalid-array-error array))
  (binding* ((dimensions (ensure-list dimensions))
             (array-rank (array-rank array))
             (()
              (unless (= (length dimensions) array-rank)
                (error "The number of dimensions not equal to rank of array.")))
             ((initialize initial-data)
              (validate-array-initargs initial-element-p initial-element
                                       initial-contents-p initial-contents
                                       displaced-to)))
    (cond ((and element-type-p
                (not (subtypep element-type (array-element-type array))))
           ;; This is weird. Should check upgraded type against actual
           ;; array element type I think. See lp#1331299. CLHS says that
           ;; "consequences are unspecified" so current behavior isn't wrong.
           (error "The new element type, ~S, is incompatible with old type."
                  element-type))
          ((and fill-pointer (/= array-rank 1))
           (error "Only vectors can have fill pointers."))
          ((and fill-pointer (not (array-has-fill-pointer-p array)))
           ;; This case always struck me as odd. It seems like it might mean
           ;; that the user asks that the array gain a fill-pointer if it didn't
           ;; have one, yet CLHS is clear that the argument array must have a
           ;; fill-pointer or else signal a type-error.
           (fill-pointer-error array)))
    (cond (initial-contents-p
             ;; array former contents replaced by INITIAL-CONTENTS
             (let* ((array-size (apply #'* dimensions))
                    (array-data (data-vector-from-inits
                                 dimensions array-size element-type nil nil
                                 initialize initial-data)))
               (cond ((adjustable-array-p array)
                      (set-array-header array array-data array-size
                                        (get-new-fill-pointer array array-size
                                                              fill-pointer)
                                        0 dimensions nil nil))
                     ((array-header-p array)
                      ;; simple multidimensional or single dimensional array
                       (make-array dimensions
                                   :element-type element-type
                                   :initial-contents initial-contents))
                     (t
                      array-data))))
          (displaced-to
             ;; We already established that no INITIAL-CONTENTS was supplied.
             (unless (or (eql element-type (array-element-type displaced-to))
                         (subtypep element-type (array-element-type displaced-to)))
               ;; See lp#1331299 again. Require exact match on upgraded type?
               (error "can't displace an array of type ~S into another of ~
                       type ~S"
                      element-type (array-element-type displaced-to)))
             (let ((displacement (or displaced-index-offset 0))
                   (array-size (apply #'* dimensions)))
               (declare (fixnum displacement array-size))
               (if (< (the fixnum (array-total-size displaced-to))
                      (the fixnum (+ displacement array-size)))
                   (error "The :DISPLACED-TO array is too small."))
               (if (adjustable-array-p array)
                   ;; None of the original contents appear in adjusted array.
                   (set-array-header array displaced-to array-size
                                     (get-new-fill-pointer array array-size
                                                           fill-pointer)
                                     displacement dimensions t nil)
                   ;; simple multidimensional or single dimensional array
                   (make-array dimensions
                               :element-type element-type
                               :displaced-to displaced-to
                               :displaced-index-offset
                               displaced-index-offset))))
          ((= array-rank 1)
             (let ((old-length (array-total-size array))
                   (new-length (car dimensions))
                   new-data)
               (declare (fixnum old-length new-length))
               (with-array-data ((old-data array) (old-start)
                                 (old-end old-length))
                 (cond ((or (and (array-header-p array)
                                 (%array-displaced-p array))
                            (< old-length new-length))
                        (setf new-data
                              (data-vector-from-inits
                               dimensions new-length element-type
                               (%other-pointer-widetag old-data) nil
                               initialize initial-data))
                        ;; Provide :END1 to avoid full call to LENGTH
                        ;; inside REPLACE.
                        (replace new-data old-data
                                 :end1 new-length
                                 :start2 old-start :end2 old-end))
                       (t (setf new-data
                                (shrink-vector old-data new-length))))
                 (if (adjustable-array-p array)
                     (set-array-header array new-data new-length
                                       (get-new-fill-pointer array new-length
                                                             fill-pointer)
                                       0 dimensions nil nil)
                     new-data))))
          (t
           (let ((old-length (%array-available-elements array))
                 (new-length (apply #'* dimensions)))
             (declare (fixnum old-length new-length))
             (with-array-data ((old-data array) (old-start)
                               (old-end old-length))
               (declare (ignore old-end))
               (let ((new-data (if (or (and (array-header-p array)
                                            (%array-displaced-p array))
                                       (> new-length old-length)
                                       (not (adjustable-array-p array)))
                                   (data-vector-from-inits
                                    dimensions new-length
                                    element-type
                                    (%other-pointer-widetag old-data) nil
                                    (if initial-element-p :initial-element)
                                    initial-element)
                                   old-data)))
                 (if (or (zerop old-length) (zerop new-length))
                     (when initial-element-p (fill new-data initial-element))
                     (zap-array-data old-data (array-dimensions array)
                                     old-start
                                     new-data dimensions new-length
                                     element-type initial-element
                                     initial-element-p))
                 (if (adjustable-array-p array)
                     (set-array-header array new-data new-length
                                       nil 0 dimensions nil nil)
                     (let ((new-array
                             (make-array-header
                              sb!vm:simple-array-widetag array-rank)))
                       (set-array-header new-array new-data new-length
                                         nil 0 dimensions nil t))))))))))


(defun get-new-fill-pointer (old-array new-array-size fill-pointer)
  (declare (fixnum new-array-size))
  (typecase fill-pointer
    (null
     ;; "The consequences are unspecified if array is adjusted to a
     ;;  size smaller than its fill pointer ..."
     (when (array-has-fill-pointer-p old-array)
       (when (> (%array-fill-pointer old-array) new-array-size)
         (error "cannot ADJUST-ARRAY an array (~S) to a size (~S) that is ~
                     smaller than its fill pointer (~S)"
                old-array new-array-size (fill-pointer old-array)))
       (%array-fill-pointer old-array)))
    ((eql t)
     new-array-size)
    (fixnum
     (when (> fill-pointer new-array-size)
       (error "can't supply a value for :FILL-POINTER (~S) that is larger ~
                   than the new length of the vector (~S)"
              fill-pointer new-array-size))
     fill-pointer)))

;;; Destructively alter VECTOR, changing its length to NEW-LENGTH,
;;; which must be less than or equal to its current length. This can
;;; be called on vectors without a fill pointer but it is extremely
;;; dangerous to do so: shrinking the size of an object (as viewed by
;;; the gc) makes bounds checking unreliable in the face of interrupts
;;; or multi-threading. Call it only on provably local vectors.
(defun %shrink-vector (vector new-length)
  (declare (vector vector))
  (unless (array-header-p vector)
    (macrolet ((frob (name &rest things)
                 `(etypecase ,name
                    ((simple-array nil (*)) (error 'nil-array-accessed-error))
                    ,@(mapcar (lambda (thing)
                                (destructuring-bind (type-spec fill-value)
                                    thing
                                  `(,type-spec
                                    (fill (truly-the ,type-spec ,name)
                                          ,fill-value
                                          :start new-length))))
                              things))))
      ;; Set the 'tail' of the vector to the appropriate type of zero,
      ;; "because in some cases we'll scavenge larger areas in one go,
      ;; like groups of pages that had triggered the write barrier, or
      ;; the whole static space" according to jsnell.
      #.`(frob vector
          ,@(map 'list
                 (lambda (saetp)
                   `((simple-array ,(sb!vm:saetp-specifier saetp) (*))
                     ,(if (or (eq (sb!vm:saetp-specifier saetp) 'character)
                              #!+sb-unicode
                              (eq (sb!vm:saetp-specifier saetp) 'base-char))
                          *default-init-char-form*
                          (sb!vm:saetp-initial-element-default saetp))))
                 (remove-if-not
                  #'sb!vm:saetp-specifier
                  sb!vm:*specialized-array-element-type-properties*)))))
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
                 (when (and from (eq array (%array-data-vector from)))
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
                         (setf (%array-dimension from i) 0)))))))))
    (if newp
        (setf (%array-displaced-from array) nil)
        (%walk-displaced-array-backpointers array length))
    (when displacedp
      (%save-displaced-array-backpointer array data))
    (setf (%array-data-vector array) data)
    (setf (%array-available-elements array) length)
    (cond (fill-pointer
           (setf (%array-fill-pointer array) fill-pointer)
           (setf (%array-fill-pointer-p array) t))
          (t
           (setf (%array-fill-pointer array) length)
           (setf (%array-fill-pointer-p array) nil)))
    (setf (%array-displacement array) displacement)
    (if (listp dimensions)
        (dotimes (axis (array-rank array))
          (declare (type index axis))
          (setf (%array-dimension array axis) (pop dimensions)))
        (setf (%array-dimension array 0) dimensions))
    (setf (%array-displaced-p array) displacedp)
    array))

;;; User visible extension
(declaim (ftype (sfunction (array) (simple-array * (*))) array-storage-vector))
(defun array-storage-vector (array)
  #!+sb-doc
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
                    (%array-data-vector array)))))


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
           (error "~S can't be used to initialize an array of type ~S."
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
  (and (= (array-rank array1)
          (array-rank array2))
       (dotimes (index (array-rank array1) t)
         (when (/= (array-dimension array1 index)
                   (array-dimension array2 index))
           (return nil)))))

(defun pick-result-array (result-bit-array bit-array-1)
  (case result-bit-array
    ((t) bit-array-1)
    ((nil) (make-array (array-dimensions bit-array-1)
                       :element-type 'bit
                       :initial-element 0))
    (t
     (unless (bit-array-same-dimensions-p bit-array-1
                                          result-bit-array)
       (error "~S and ~S don't have the same dimensions."
              bit-array-1 result-bit-array))
     result-bit-array)))

(defmacro def-bit-array-op (name function)
  `(defun ,name (bit-array-1 bit-array-2 &optional result-bit-array)
     #!+sb-doc
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
             (declare (ignore end1))
             (with-array-data ((data2 bit-array-2) (start2) (end2))
               (declare (ignore end2))
               (with-array-data ((data3 result-bit-array) (start3) (end3))
                 (do ((index-1 start1 (1+ index-1))
                      (index-2 start2 (1+ index-2))
                      (index-3 start3 (1+ index-3)))
                     ((>= index-3 end3) result-bit-array)
                   (declare (type index index-1 index-2 index-3))
                   (setf (sbit data3 index-3)
                         (logand (,function (sbit data1 index-1)
                                            (sbit data2 index-2))
                                 1))))))))))

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

(defun bit-not (bit-array &optional result-bit-array)
  #!+sb-doc
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
          (declare (ignore src-end))
          (with-array-data ((dst result-bit-array) (dst-start) (dst-end))
            (do ((src-index src-start (1+ src-index))
                 (dst-index dst-start (1+ dst-index)))
                ((>= dst-index dst-end) result-bit-array)
              (declare (type index src-index dst-index))
              (setf (sbit dst dst-index)
                    (logxor (sbit src src-index) 1))))))))

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
(defmacro !define-array-dispatch (dispatch-name params &body body)
  (let ((table-name (symbolicate "%%" dispatch-name "-FUNS%%"))
        (error-name (symbolicate "HAIRY-" dispatch-name "-ERROR")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defun ,error-name (&rest args)
           (error 'type-error
                  :datum (first args)
                  :expected-type '(simple-array * (*)))))
       (!defglobal ,table-name (make-array ,(1+ sb!vm:widetag-mask)
                                          :initial-element #',error-name))
       ,@(loop for info across sb!vm:*specialized-array-element-type-properties*
               for typecode = (sb!vm:saetp-typecode info)
               for specifier = (sb!vm:saetp-specifier info)
               for primitive-type-name = (sb!vm:saetp-primitive-type-name info)
               collect (let ((fun-name (symbolicate (string dispatch-name)
                                                    "/" primitive-type-name)))
                         `(progn
                            (defun ,fun-name ,params
                              (declare (type (simple-array ,specifier (*))
                                             ,(first params)))
                              ,@body)
                            (setf (svref ,table-name ,typecode) #',fun-name))))
       (defmacro ,dispatch-name (&rest args)
         (check-type (first args) symbol)
         (let ((tag (gensym "TAG")))
           `(funcall
             (the function
               (let ((,tag 0))
                 (when (sb!vm::%other-pointer-p ,(first args))
                   (setf ,tag (%other-pointer-widetag ,(first args))))
                 (svref ,',table-name ,tag)))
             ,@args))))))
