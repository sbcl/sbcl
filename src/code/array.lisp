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
  (def %array-diplaced-from))

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
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro pick-vector-type (type &rest specs)
    `(cond ,@(mapcar (lambda (spec)
                       `(,(if (eq (car spec) t)
                              t
                              `(subtypep ,type ',(car spec)))
                         ,@(cdr spec)))
                     specs))))

;;; These functions are used in the implementation of MAKE-ARRAY for
;;; complex arrays. There are lots of transforms to simplify
;;; MAKE-ARRAY for various easy cases, but not for all reasonable
;;; cases, so e.g. as of sbcl-0.6.6 we still make full calls to
;;; MAKE-ARRAY for any non-simple array. Thus, there's some value to
;;; making this somewhat efficient, at least not doing full calls to
;;; SUBTYPEP in the easy cases.
(defun %vector-widetag-and-n-bits (type)
  (case type
    ;; Pick off some easy common cases.
    ;;
    ;; (Perhaps we should make a much more exhaustive table of easy
    ;; common cases here. Or perhaps the effort would be better spent
    ;; on smarter compiler transforms which do the calculation once
    ;; and for all in any reasonable user programs.)
    ((t)
     (values #.sb!vm:simple-vector-widetag #.sb!vm:n-word-bits))
    ((base-char standard-char #!-sb-unicode character)
     (values #.sb!vm:simple-base-string-widetag #.sb!vm:n-byte-bits))
    #!+sb-unicode
    ((character)
     (values #.sb!vm:simple-character-string-widetag #.sb!vm:n-word-bits))
    ((bit)
     (values #.sb!vm:simple-bit-vector-widetag 1))
    ;; OK, we have to wade into SUBTYPEPing after all.
    (t
     (unless *type-system-initialized*
       (bug "SUBTYPEP dispatch for MAKE-ARRAY before the type system is ready"))
     #.`(pick-vector-type type
         ,@(map 'list
                (lambda (saetp)
                  `(,(sb!vm:saetp-specifier saetp)
                    (values ,(sb!vm:saetp-typecode saetp)
                            ,(sb!vm:saetp-n-bits saetp))))
                sb!vm:*specialized-array-element-type-properties*)))))

(defun %complex-vector-widetag (type)
  (case type
    ;; Pick off some easy common cases.
    ((t)
     #.sb!vm:complex-vector-widetag)
    ((base-char #!-sb-unicode character)
     #.sb!vm:complex-base-string-widetag)
    #!+sb-unicode
    ((character)
     #.sb!vm:complex-character-string-widetag)
    ((nil)
     #.sb!vm:complex-vector-nil-widetag)
    ((bit)
     #.sb!vm:complex-bit-vector-widetag)
    ;; OK, we have to wade into SUBTYPEPing after all.
    (t
     (pick-vector-type type
       (nil #.sb!vm:complex-vector-nil-widetag)
       #!-sb-unicode
       (character #.sb!vm:complex-base-string-widetag)
       #!+sb-unicode
       (base-char #.sb!vm:complex-base-string-widetag)
       #!+sb-unicode
       (character #.sb!vm:complex-character-string-widetag)
       (bit #.sb!vm:complex-bit-vector-widetag)
       (t #.sb!vm:complex-vector-widetag)))))

(defun make-array (dimensions &key
                              (element-type t)
                              (initial-element nil initial-element-p)
                              (initial-contents nil initial-contents-p)
                              adjustable fill-pointer
                              displaced-to displaced-index-offset)
  (let* ((dimensions (if (listp dimensions) dimensions (list dimensions)))
         (array-rank (length (the list dimensions)))
         (simple (and (null fill-pointer)
                      (not adjustable)
                      (null displaced-to))))
    (declare (fixnum array-rank))
    (when (and displaced-index-offset (null displaced-to))
      (error "can't specify :DISPLACED-INDEX-OFFSET without :DISPLACED-TO"))
    (when (and displaced-to
               (arrayp displaced-to)
               (not (equal (array-element-type displaced-to)
                           (upgraded-array-element-type element-type))))
      (error "Array element type of :DISPLACED-TO array does not match specified element type"))
    (if (and simple (= array-rank 1))
        ;; it's a (SIMPLE-ARRAY * (*))
        (multiple-value-bind (type n-bits)
            (%vector-widetag-and-n-bits element-type)
          (declare (type (unsigned-byte 8) type)
                   (type (integer 0 256) n-bits))
          (let* ((length (car dimensions))
                 (array (allocate-vector
                         type
                         length
                         (ceiling
                          (* (if (or (= type sb!vm:simple-base-string-widetag)
                                     #!+sb-unicode
                                     (= type
                                        sb!vm:simple-character-string-widetag))
                                 (1+ length)
                                 length)
                             n-bits)
                          sb!vm:n-word-bits))))
            (declare (type index length))
            (when initial-element-p
              (fill array initial-element))
            (when initial-contents-p
              (when initial-element-p
                (error "can't specify both :INITIAL-ELEMENT and ~
                       :INITIAL-CONTENTS"))
              (unless (= length (length initial-contents))
                (error "There are ~W elements in the :INITIAL-CONTENTS, but ~
                       the vector length is ~W."
                       (length initial-contents)
                       length))
              (replace array initial-contents))
            array))
        ;; it's either a complex array or a multidimensional array.
        (let* ((total-size (reduce #'* dimensions))
               (data (or displaced-to
                         (data-vector-from-inits
                          dimensions total-size element-type
                          initial-contents initial-contents-p
                          initial-element initial-element-p)))
               (array (make-array-header
                       (cond ((= array-rank 1)
                              (%complex-vector-widetag element-type))
                             (simple sb!vm:simple-array-widetag)
                             (t sb!vm:complex-array-widetag))
                       array-rank)))
          (cond (fill-pointer
                 (unless (= array-rank 1)
                   (error "Only vectors can have fill pointers."))
                 (let ((length (car dimensions)))
                   (declare (fixnum length))
                   (setf (%array-fill-pointer array)
                     (cond ((eq fill-pointer t)
                            length)
                           (t
                            (unless (and (fixnump fill-pointer)
                                         (>= fill-pointer 0)
                                         (<= fill-pointer length))
                              ;; FIXME: should be TYPE-ERROR?
                              (error "invalid fill-pointer ~W"
                                     fill-pointer))
                            fill-pointer))))
                 (setf (%array-fill-pointer-p array) t))
                (t
                 (setf (%array-fill-pointer array) total-size)
                 (setf (%array-fill-pointer-p array) nil)))
          (setf (%array-available-elements array) total-size)
          (setf (%array-data-vector array) data)
          (setf (%array-displaced-from array) nil)
          (cond (displaced-to
                 (when (or initial-element-p initial-contents-p)
                   (error "Neither :INITIAL-ELEMENT nor :INITIAL-CONTENTS ~
                   can be specified along with :DISPLACED-TO"))
                 (let ((offset (or displaced-index-offset 0)))
                   (when (> (+ offset total-size)
                            (array-total-size displaced-to))
                     (error "~S doesn't have enough elements." displaced-to))
                   (setf (%array-displacement array) offset)
                   (setf (%array-displaced-p array) t)
                   (%save-displaced-array-backpointer array data)))
                (t
                 (setf (%array-displaced-p array) nil)))
          (let ((axis 0))
            (dolist (dim dimensions)
              (setf (%array-dimension array axis) dim)
              (incf axis)))
          array))))

(defun make-static-vector (length &key
                           (element-type '(unsigned-byte 8))
                           (initial-contents nil initial-contents-p)
                           (initial-element nil initial-element-p))
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
  (when initial-contents-p
    (when initial-element-p
      (error "can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
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
      (sb!impl::%vector-widetag-and-n-bits element-type)
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
(defun data-vector-from-inits (dimensions total-size element-type
                               initial-contents initial-contents-p
                               initial-element initial-element-p)
  (when (and initial-contents-p initial-element-p)
    (error "cannot supply both :INITIAL-CONTENTS and :INITIAL-ELEMENT to
            either MAKE-ARRAY or ADJUST-ARRAY."))
  (let ((data (if initial-element-p
                  (make-array total-size
                              :element-type element-type
                              :initial-element initial-element)
                  (make-array total-size
                              :element-type element-type))))
    (cond (initial-element-p
           (unless (simple-vector-p data)
             (unless (typep initial-element element-type)
               (error "~S cannot be used to initialize an array of type ~S."
                      initial-element element-type))
             (fill (the vector data) initial-element)))
          (initial-contents-p
           (fill-data-vector data dimensions initial-contents)))
    data))

(defun vector (&rest objects)
  #!+sb-doc
  "Construct a SIMPLE-VECTOR from the given objects."
  (coerce (the list objects) 'simple-vector))


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
                (defmacro ,name (array-var)
                  `(the function
                     (let ((tag 0))
                       (when (sb!vm::%other-pointer-p ,array-var)
                         (setf tag (%other-pointer-widetag ,array-var)))
                       (svref ,',table-name tag)))))))
  (def !find-data-vector-setter %%data-vector-setters%%)
  (def !find-data-vector-setter/check-bounds %%data-vector-setters/check-bounds%%)
  (def !find-data-vector-reffer %%data-vector-reffers%%)
  (def !find-data-vector-reffer/check-bounds %%data-vector-reffers/check-bounds%%))

(macrolet ((%ref (accessor-getter extra-params)
             `(funcall (,accessor-getter array) array index ,@extra-params))
           (define (accessor-name slow-accessor-name accessor-getter
                                  extra-params check-bounds)
             `(progn
                (defun ,accessor-name (array index ,@extra-params)
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
    !find-data-vector-reffer
    nil (progn))
  (define hairy-data-vector-set slow-hairy-data-vector-set
    !find-data-vector-setter
    (new-value) (progn))
  (define hairy-data-vector-ref/check-bounds
      slow-hairy-data-vector-ref/check-bounds
    !find-data-vector-reffer/check-bounds
    nil (%check-bound array (array-dimension array 0)))
  (define hairy-data-vector-set/check-bounds
      slow-hairy-data-vector-set/check-bounds
    !find-data-vector-setter/check-bounds
    (new-value) (%check-bound array (array-dimension array 0))))

(defun hairy-ref-error (array index &optional new-value)
  (declare (ignore index new-value))
  (error 'type-error
         :datum array
         :expected-type 'vector))

;;; Populate the dispatch tables.
(macrolet ((define-reffer (saetp check-form)
             (let* ((type (sb!vm:saetp-specifier saetp))
                    (atype `(simple-array ,type (*))))
               `(named-lambda optimized-data-vector-ref (vector index)
                  (declare (optimize speed (safety 0)))
                  (data-vector-ref (the ,atype vector)
                                   (locally
                                       (declare (optimize (safety 1)))
                                     (the index
                                       (,@check-form index)))))))
           (define-setter (saetp check-form)
             (let* ((type (sb!vm:saetp-specifier saetp))
                    (atype `(simple-array ,type (*))))
               `(named-lambda optimized-data-vector-set (vector index new-value)
                  (declare (optimize speed (safety 0)))
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
      (%check-bound vector (length vector))
      #'slow-hairy-data-vector-ref/check-bounds)
    (define-reffers %%data-vector-setters/check-bounds%% define-setter
      (%check-bound vector (length vector))
      #'slow-hairy-data-vector-set/check-bounds)))

;;; (Ordinary DATA-VECTOR-REF usage compiles into a vop, but
;;; DATA-VECTOR-REF is also FOLDABLE, and this ordinary function
;;; definition is needed for the compiler to use in constant folding.)
(defun data-vector-ref (array index)
  (hairy-data-vector-ref array index))

(defun data-vector-ref-with-offset (array index offset)
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

(declaim (ftype (function (array integer integer &optional t) nil)
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
(defun %array-row-major-index (array subscripts
                                     &optional (invalid-index-error-p t))
  (declare (array array)
           (list subscripts))
  (let ((rank (array-rank array)))
    (unless (= rank (length subscripts))
      (error "wrong number of subscripts, ~W, for array of rank ~W"
             (length subscripts) rank))
    (if (array-header-p array)
        (do ((subs (nreverse subscripts) (cdr subs))
             (axis (1- (array-rank array)) (1- axis))
             (chunk-size 1)
             (result 0))
            ((null subs) result)
          (declare (list subs) (fixnum axis chunk-size result))
          (let ((index (car subs))
                (dim (%array-dimension array axis)))
            (declare (fixnum dim))
            (unless (and (fixnump index) (< -1 index dim))
              (if invalid-index-error-p
                  (invalid-array-index-error array index dim axis)
                  (return-from %array-row-major-index nil)))
            (incf result (* chunk-size (the fixnum index)))
            (setf chunk-size (* chunk-size dim))))
        (let ((index (first subscripts))
              (length (length (the (simple-array * (*)) array))))
          (unless (and (fixnump index) (< -1 index length))
            (if invalid-index-error-p
                (invalid-array-index-error array index length)
                (return-from %array-row-major-index nil)))
          index))))

(defun array-in-bounds-p (array &rest subscripts)
  #!+sb-doc
  "Return T if the SUBSCRIPTS are in bounds for the ARRAY, NIL otherwise."
  (if (%array-row-major-index array subscripts nil)
      t))

(defun array-row-major-index (array &rest subscripts)
  (declare (truly-dynamic-extent subscripts))
  (%array-row-major-index array subscripts))

(defun aref (array &rest subscripts)
  #!+sb-doc
  "Return the element of the ARRAY specified by the SUBSCRIPTS."
  (declare (truly-dynamic-extent subscripts))
  (row-major-aref array (%array-row-major-index array subscripts)))

(defun %aset (array &rest stuff)
  (declare (truly-dynamic-extent stuff))
  (let ((subscripts (butlast stuff))
        (new-value (car (last stuff))))
    (setf (row-major-aref array (%array-row-major-index array subscripts))
          new-value)))

;;; FIXME: What's supposed to happen with functions
;;; like AREF when we (DEFUN (SETF FOO) ..) when
;;; DEFSETF FOO is also defined? It seems as though the logical
;;; thing to do would be to nuke the macro definition for (SETF FOO)
;;; and replace it with the (SETF FOO) function, issuing a warning,
;;; just as for ordinary functions
;;;  * (LISP-IMPLEMENTATION-VERSION)
;;;  "18a+ release x86-linux 2.4.7 6 November 1998 cvs"
;;;  * (DEFMACRO ZOO (X) `(+ ,X ,X))
;;;  ZOO
;;;  * (DEFUN ZOO (X) (* 3 X))
;;;  Warning: ZOO previously defined as a macro.
;;;  ZOO
;;; But that doesn't seem to be what happens in CMU CL.
;;;
;;; KLUDGE: this is probably because ANSI, in its wisdom (CLHS
;;; 5.1.2.5) requires implementations to support
;;;   (SETF (APPLY #'AREF ...) ...)
;;; [and also #'BIT and #'SBIT].  Yes, this is terrifying, and it's
;;; also terrifying that this sequence of definitions causes it to
;;; work.
;;;
;;; Also, it would be nice to make DESCRIBE FOO tell whether a symbol
;;; has a setf expansion and/or a setf function defined.

#!-sb-fluid (declaim (inline (setf aref)))
(defun (setf aref) (new-value array &rest subscripts)
  (declare (truly-dynamic-extent subscripts))
  (declare (type array array))
  (setf (row-major-aref array (%array-row-major-index array subscripts))
        new-value))

(defun row-major-aref (array index)
  #!+sb-doc
  "Return the element of array corressponding to the row-major index. This is
   SETF'able."
  (declare (optimize (safety 1)))
  (row-major-aref array index))

(defun %set-row-major-aref (array index new-value)
  (declare (optimize (safety 1)))
  (setf (row-major-aref array index) new-value))

(defun svref (simple-vector index)
  #!+sb-doc
  "Return the INDEX'th element of the given Simple-Vector."
  (declare (optimize (safety 1)))
  (aref simple-vector index))

(defun %svset (simple-vector index new)
  (declare (optimize (safety 1)))
  (setf (aref simple-vector index) new))

(defun bit (bit-array &rest subscripts)
  #!+sb-doc
  "Return the bit from the BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (type (array bit) bit-array) (optimize (safety 1)))
  (row-major-aref bit-array (%array-row-major-index bit-array subscripts)))

(defun %bitset (bit-array &rest stuff)
  (declare (type (array bit) bit-array) (optimize (safety 1)))
  (let ((subscripts (butlast stuff))
        (new-value (car (last stuff))))
    (setf (row-major-aref bit-array
                          (%array-row-major-index bit-array subscripts))
          new-value)))

#!-sb-fluid (declaim (inline (setf bit)))
(defun (setf bit) (new-value bit-array &rest subscripts)
  (declare (type (array bit) bit-array) (optimize (safety 1)))
  (setf (row-major-aref bit-array
                        (%array-row-major-index bit-array subscripts))
        new-value))

(defun sbit (simple-bit-array &rest subscripts)
  #!+sb-doc
  "Return the bit from SIMPLE-BIT-ARRAY at the specified SUBSCRIPTS."
  (declare (type (simple-array bit) simple-bit-array) (optimize (safety 1)))
  (row-major-aref simple-bit-array
                  (%array-row-major-index simple-bit-array subscripts)))

;;; KLUDGE: Not all these things (%SET-ROW-MAJOR-AREF, %SET-FILL-POINTER,
;;; %SET-FDEFINITION, %SCHARSET, %SBITSET..) seem to deserve separate names.
;;; Could we just DEFUN (SETF SBIT) etc. and get rid of the non-ANSI names?
;;; -- WHN 19990911
(defun %sbitset (simple-bit-array &rest stuff)
  (declare (type (simple-array bit) simple-bit-array) (optimize (safety 1)))
  (let ((subscripts (butlast stuff))
        (new-value (car (last stuff))))
    (setf (row-major-aref simple-bit-array
                          (%array-row-major-index simple-bit-array subscripts))
          new-value)))

#!-sb-fluid (declaim (inline (setf sbit)))
(defun (setf sbit) (new-value bit-array &rest subscripts)
  (declare (type (simple-array bit) bit-array) (optimize (safety 1)))
  (setf (row-major-aref bit-array
                        (%array-row-major-index bit-array subscripts))
        new-value))

;;;; miscellaneous array properties

(defun array-element-type (array)
  #!+sb-doc
  "Return the type of the elements of the array"
  (let ((widetag (widetag-of array)))
    (macrolet ((pick-element-type (&rest stuff)
                 `(cond ,@(mapcar (lambda (stuff)
                                    (cons
                                     (let ((item (car stuff)))
                                       (cond ((eq item t)
                                              t)
                                             ((listp item)
                                              (cons 'or
                                                    (mapcar (lambda (x)
                                                              `(= widetag ,x))
                                                            item)))
                                             (t
                                              `(= widetag ,item))))
                                     (cdr stuff)))
                                  stuff))))
      #.`(pick-element-type
          ,@(map 'list
                 (lambda (saetp)
                   `(,(if (sb!vm:saetp-complex-typecode saetp)
                          (list (sb!vm:saetp-typecode saetp)
                                (sb!vm:saetp-complex-typecode saetp))
                          (sb!vm:saetp-typecode saetp))
                     ',(sb!vm:saetp-specifier saetp)))
                 sb!vm:*specialized-array-element-type-properties*)
          ((sb!vm:simple-array-widetag
            sb!vm:complex-vector-widetag
            sb!vm:complex-array-widetag)
           (with-array-data ((array array) (start) (end))
             (declare (ignore start end))
             (array-element-type array)))
          (t
           (error 'type-error :datum array :expected-type 'array))))))

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
  "Return T if (ADJUST-ARRAY ARRAY...) would return an array identical
   to the argument, this happens for complex arrays."
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

(defun fill-pointer-error (vector arg)
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
  (if (array-has-fill-pointer-p vector)
      (%array-fill-pointer vector)
      (fill-pointer-error vector nil)))

(defun %set-fill-pointer (vector new)
  (flet ((oops (x)
           (fill-pointer-error vector x)))
    (if (array-has-fill-pointer-p vector)
        (if (> new (%array-available-elements vector))
            (oops new)
            (setf (%array-fill-pointer vector) new))
        (oops nil))))

;;; FIXME: It'd probably make sense to use a MACROLET to share the
;;; guts of VECTOR-PUSH between VECTOR-PUSH-EXTEND. Such a macro
;;; should probably be based on the VECTOR-PUSH-EXTEND code (which is
;;; new ca. sbcl-0.7.0) rather than the VECTOR-PUSH code (which dates
;;; back to CMU CL).
(defun vector-push (new-el array)
  #!+sb-doc
  "Attempt to set the element of ARRAY designated by its fill pointer
   to NEW-EL, and increment the fill pointer by one. If the fill pointer is
   too large, NIL is returned, otherwise the index of the pushed element is
   returned."
  (let ((fill-pointer (fill-pointer array)))
    (declare (fixnum fill-pointer))
    (cond ((= fill-pointer (%array-available-elements array))
           nil)
          (t
           (locally (declare (optimize (safety 0)))
             (setf (aref array fill-pointer) new-el))
           (setf (%array-fill-pointer array) (1+ fill-pointer))
           fill-pointer))))

(defun vector-push-extend (new-element
                           vector
                           &optional
                           (min-extension
                            (let ((length (length vector)))
                              (min (1+ length)
                                   (- array-dimension-limit length)))))
  (declare (fixnum min-extension))
  (let ((fill-pointer (fill-pointer vector)))
    (declare (fixnum fill-pointer))
    (when (= fill-pointer (%array-available-elements vector))
      (adjust-array vector (+ fill-pointer (max 1 min-extension))))
    ;; disable bounds checking
    (locally (declare (optimize (safety 0)))
      (setf (aref vector fill-pointer) new-element))
    (setf (%array-fill-pointer vector) (1+ fill-pointer))
    fill-pointer))

(defun vector-pop (array)
  #!+sb-doc
  "Decrease the fill pointer by 1 and return the element pointed to by the
  new fill pointer."
  (let ((fill-pointer (fill-pointer array)))
    (declare (fixnum fill-pointer))
    (if (zerop fill-pointer)
        (error "There is nothing left to pop.")
        ;; disable bounds checking (and any fixnum test)
        (locally (declare (optimize (safety 0)))
          (aref array
                (setf (%array-fill-pointer array)
                      (1- fill-pointer)))))))


;;;; ADJUST-ARRAY

(defun adjust-array (array dimensions &key
                           (element-type (array-element-type array))
                           (initial-element nil initial-element-p)
                           (initial-contents nil initial-contents-p)
                           fill-pointer
                           displaced-to displaced-index-offset)
  #!+sb-doc
  "Adjust ARRAY's dimensions to the given DIMENSIONS and stuff."
  (when (invalid-array-p array)
    (invalid-array-error array))
  (let ((dimensions (if (listp dimensions) dimensions (list dimensions))))
    (cond ((/= (the fixnum (length (the list dimensions)))
               (the fixnum (array-rank array)))
           (error "The number of dimensions not equal to rank of array."))
          ((not (subtypep element-type (array-element-type array)))
           (error "The new element type, ~S, is incompatible with old type."
                  element-type))
          ((and fill-pointer (not (array-has-fill-pointer-p array)))
           (error 'type-error
                  :datum array
                  :expected-type '(satisfies array-has-fill-pointer-p))))
    (let ((array-rank (length (the list dimensions))))
      (declare (fixnum array-rank))
      (unless (= array-rank 1)
        (when fill-pointer
          (error "Only vectors can have fill pointers.")))
      (cond (initial-contents-p
             ;; array former contents replaced by INITIAL-CONTENTS
             (if (or initial-element-p displaced-to)
                 (error "INITIAL-CONTENTS may not be specified with ~
                         the :INITIAL-ELEMENT or :DISPLACED-TO option."))
             (let* ((array-size (apply #'* dimensions))
                    (array-data (data-vector-from-inits
                                 dimensions array-size element-type
                                 initial-contents initial-contents-p
                                 initial-element initial-element-p)))
               (if (adjustable-array-p array)
                   (set-array-header array array-data array-size
                                 (get-new-fill-pointer array array-size
                                                       fill-pointer)
                                 0 dimensions nil nil)
                   (if (array-header-p array)
                       ;; simple multidimensional or single dimensional array
                       (make-array dimensions
                                   :element-type element-type
                                   :initial-contents initial-contents)
                       array-data))))
            (displaced-to
             ;; We already established that no INITIAL-CONTENTS was supplied.
             (when initial-element
               (error "The :INITIAL-ELEMENT option may not be specified ~
                       with :DISPLACED-TO."))
             (unless (subtypep element-type (array-element-type displaced-to))
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
                               initial-contents initial-contents-p
                               initial-element initial-element-p))
                        (replace new-data old-data
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
                                         (> new-length old-length))
                                     (data-vector-from-inits
                                      dimensions new-length
                                      element-type () nil
                                      initial-element initial-element-p)
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
                                           nil 0 dimensions nil t)))))))))))


(defun get-new-fill-pointer (old-array new-array-size fill-pointer)
  (cond ((not fill-pointer)
         (when (array-has-fill-pointer-p old-array)
           (when (> (%array-fill-pointer old-array) new-array-size)
             (error "cannot ADJUST-ARRAY an array (~S) to a size (~S) that is ~
                     smaller than its fill pointer (~S)"
                    old-array new-array-size (fill-pointer old-array)))
           (%array-fill-pointer old-array)))
        ((not (array-has-fill-pointer-p old-array))
         (error "cannot supply a non-NIL value (~S) for :FILL-POINTER ~
                 in ADJUST-ARRAY unless the array (~S) was originally ~
                 created with a fill pointer"
                fill-pointer
                old-array))
        ((numberp fill-pointer)
         (when (> fill-pointer new-array-size)
           (error "can't supply a value for :FILL-POINTER (~S) that is larger ~
                   than the new length of the vector (~S)"
                  fill-pointer new-array-size))
         fill-pointer)
        ((eq fill-pointer t)
         new-array-size)
        (t
         (error "bogus value for :FILL-POINTER in ADJUST-ARRAY: ~S"
                fill-pointer))))

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

(defun %walk-displaced-array-backpointers (array new-length)
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
              (setf (%array-dimension from i) 0))))))))

;;; Fill in array header with the provided information, and return the array.
(defun set-array-header (array data length fill-pointer displacement dimensions
                         displacedp newp)
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
  array)

;;; User visible extension
(declaim (ftype (function (array) (values (simple-array * (*)) &optional))
                array-storage-vector))
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
             (if (array-header-p array)
                 (if (%array-displaced-p array)
                     (error "~S cannot be used with displaced arrays. Use ~S instead."
                            'array-storage-vector 'array-displacement)
                     (%array-data-vector array))
                 array)))


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
         (when initial-element-p
           ;; FIXME: transforming this TYPEP to someting a bit faster
           ;; would be a win...
           (unless (typep initial-element element-type)
             (error "~S can't be used to initialize an array of type ~S."
                    initial-element element-type)))
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
