;;;; array-specific optimizers and transforms

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; utilities for optimizing array operations

;;; Return UPGRADED-ARRAY-ELEMENT-TYPE for LVAR, or do
;;; GIVE-UP-IR1-TRANSFORM if the upgraded element type can't be
;;; determined.
(defun upgraded-element-type-specifier-or-give-up (lvar)
  (let* ((element-ctype (extract-upgraded-element-type lvar))
         (element-type-specifier (type-specifier element-ctype)))
    (if (eq element-type-specifier '*)
        (give-up-ir1-transform
         "upgraded array element type not known at compile time")
        element-type-specifier)))

;;; Array access functions return an object from the array, hence its
;;; type is going to be the array upgraded element type.
(defun extract-upgraded-element-type (array)
  (let ((type (lvar-type array)))
    ;; Note that this IF mightn't be satisfied even if the runtime
    ;; value is known to be a subtype of some specialized ARRAY, because
    ;; we can have values declared e.g. (AND SIMPLE-VECTOR UNKNOWN-TYPE),
    ;; which are represented in the compiler as INTERSECTION-TYPE, not
    ;; array type.
    (if (array-type-p type)
        (array-type-specialized-element-type type)
        ;; KLUDGE: there is no good answer here, but at least
        ;; *wild-type* won't cause HAIRY-DATA-VECTOR-{REF,SET} to be
        ;; erroneously optimized (see generic/vm-tran.lisp) -- CSR,
        ;; 2002-08-21
        *wild-type*)))

(defun extract-declared-element-type (array)
  (let ((type (lvar-type array)))
    (if (array-type-p type)
        (array-type-element-type type)
        *wild-type*)))

;;; The ``new-value'' for array setters must fit in the array, and the
;;; return type is going to be the same as the new-value for SETF
;;; functions.
(defun assert-new-value-type (new-value array)
  (let ((type (lvar-type array)))
    (when (array-type-p type)
      (assert-lvar-type
       new-value
       (array-type-specialized-element-type type)
       (lexenv-policy (node-lexenv (lvar-dest new-value))))))
  (lvar-type new-value))

(defun assert-array-complex (array)
  (assert-lvar-type
   array
   (make-array-type :complexp t
                    :element-type *wild-type*)
   (lexenv-policy (node-lexenv (lvar-dest array))))
  nil)

;;; Return true if ARG is NIL, or is a constant-lvar whose
;;; value is NIL, false otherwise.
(defun unsupplied-or-nil (arg)
  (declare (type (or lvar null) arg))
  (or (not arg)
      (and (constant-lvar-p arg)
           (not (lvar-value arg)))))

;;;; DERIVE-TYPE optimizers

;;; Array operations that use a specific number of indices implicitly
;;; assert that the array is of that rank.
(defun assert-array-rank (array rank)
  (assert-lvar-type
   array
   (specifier-type `(array * ,(make-list rank :initial-element '*)))
   (lexenv-policy (node-lexenv (lvar-dest array)))))

(defoptimizer (array-in-bounds-p derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  *universal-type*)

(defoptimizer (aref derive-type) ((array &rest indices) node)
  (assert-array-rank array (length indices))
  (extract-upgraded-element-type array))

(defoptimizer (%aset derive-type) ((array &rest stuff))
  (assert-array-rank array (1- (length stuff)))
  (assert-new-value-type (car (last stuff)) array))

(defoptimizer (hairy-data-vector-ref derive-type) ((array index))
  (extract-upgraded-element-type array))
(defoptimizer (data-vector-ref derive-type) ((array index))
  (extract-upgraded-element-type array))

(defoptimizer (data-vector-set derive-type) ((array index new-value))
  (assert-new-value-type new-value array))
(defoptimizer (hairy-data-vector-set derive-type) ((array index new-value))
  (assert-new-value-type new-value array))

;;; Figure out the type of the data vector if we know the argument
;;; element type.
(defoptimizer (%with-array-data derive-type) ((array start end))
  (let ((atype (lvar-type array)))
    (when (array-type-p atype)
      (specifier-type
       `(simple-array ,(type-specifier
                       (array-type-specialized-element-type atype))
                     (*))))))

(defoptimizer (array-row-major-index derive-type) ((array &rest indices))
  (assert-array-rank array (length indices))
  *universal-type*)

(defoptimizer (row-major-aref derive-type) ((array index))
  (extract-upgraded-element-type array))

(defoptimizer (%set-row-major-aref derive-type) ((array index new-value))
  (assert-new-value-type new-value array))

(defoptimizer (make-array derive-type)
              ((dims &key initial-element element-type initial-contents
                adjustable fill-pointer displaced-index-offset displaced-to))
  (let ((simple (and (unsupplied-or-nil adjustable)
                     (unsupplied-or-nil displaced-to)
                     (unsupplied-or-nil fill-pointer))))
    (or (careful-specifier-type
         `(,(if simple 'simple-array 'array)
            ,(cond ((not element-type) t)
                   ((constant-lvar-p element-type)
                    (let ((ctype (careful-specifier-type
                                  (lvar-value element-type))))
                      (cond
                        ((or (null ctype) (unknown-type-p ctype)) '*)
                        (t (sb!xc:upgraded-array-element-type
                            (lvar-value element-type))))))
                   (t
                    '*))
            ,(cond ((constant-lvar-p dims)
                    (let* ((val (lvar-value dims))
                           (cdims (if (listp val) val (list val))))
                      (if simple
                          cdims
                          (length cdims))))
                   ((csubtypep (lvar-type dims)
                               (specifier-type 'integer))
                    '(*))
                   (t
                    '*))))
        (specifier-type 'array))))

;;; Complex array operations should assert that their array argument
;;; is complex.  In SBCL, vectors with fill-pointers are complex.
(defoptimizer (fill-pointer derive-type) ((vector))
  (assert-array-complex vector))
(defoptimizer (%set-fill-pointer derive-type) ((vector index))
  (declare (ignorable index))
  (assert-array-complex vector))

(defoptimizer (vector-push derive-type) ((object vector))
  (declare (ignorable object))
  (assert-array-complex vector))
(defoptimizer (vector-push-extend derive-type)
    ((object vector &optional index))
  (declare (ignorable object index))
  (assert-array-complex vector))
(defoptimizer (vector-pop derive-type) ((vector))
  (assert-array-complex vector))

;;;; constructors

;;; Convert VECTOR into a MAKE-ARRAY followed by SETFs of all the
;;; elements.
(define-source-transform vector (&rest elements)
  (let ((len (length elements))
        (n -1))
    (once-only ((n-vec `(make-array ,len)))
      `(progn
         ,@(mapcar (lambda (el)
                     (once-only ((n-val el))
                       `(locally (declare (optimize (safety 0)))
                                 (setf (svref ,n-vec ,(incf n))
                                       ,n-val))))
                   elements)
         ,n-vec))))

;;; Just convert it into a MAKE-ARRAY.
(deftransform make-string ((length &key
                                   (element-type 'character)
                                   (initial-element
                                    #.*default-init-char-form*)))
  `(the simple-string (make-array (the index length)
                       :element-type element-type
                       ,@(when initial-element
                           '(:initial-element initial-element)))))

(deftransform make-array ((dims &key initial-element element-type
                                     adjustable fill-pointer)
                          (t &rest *))
  (when (null initial-element)
    (give-up-ir1-transform))
  (let* ((eltype (cond ((not element-type) t)
                       ((not (constant-lvar-p element-type))
                        (give-up-ir1-transform
                         "ELEMENT-TYPE is not constant."))
                       (t
                        (lvar-value element-type))))
         (eltype-type (ir1-transform-specifier-type eltype))
         (saetp (find-if (lambda (saetp)
                           (csubtypep eltype-type (sb!vm:saetp-ctype saetp)))
                         sb!vm:*specialized-array-element-type-properties*))
         (creation-form `(make-array dims
                          :element-type ',(type-specifier (sb!vm:saetp-ctype saetp))
                          ,@(when fill-pointer
                                  '(:fill-pointer fill-pointer))
                          ,@(when adjustable
                                  '(:adjustable adjustable)))))

    (unless saetp
      (give-up-ir1-transform "ELEMENT-TYPE not found in *SAETP*: ~S" eltype))

    (cond ((and (constant-lvar-p initial-element)
                (eql (lvar-value initial-element)
                     (sb!vm:saetp-initial-element-default saetp)))
           creation-form)
          (t
           ;; error checking for target, disabled on the host because
           ;; (CTYPE-OF #\Null) is not possible.
           #-sb-xc-host
           (when (constant-lvar-p initial-element)
             (let ((value (lvar-value initial-element)))
               (cond
                 ((not (ctypep value (sb!vm:saetp-ctype saetp)))
                  ;; this case will cause an error at runtime, so we'd
                  ;; better WARN about it now.
                  (warn 'array-initial-element-mismatch
                        :format-control "~@<~S is not a ~S (which is the ~
                                         ~S of ~S).~@:>"
                        :format-arguments
                        (list
                         value
                         (type-specifier (sb!vm:saetp-ctype saetp))
                         'upgraded-array-element-type
                         eltype)))
                 ((not (ctypep value eltype-type))
                  ;; this case will not cause an error at runtime, but
                  ;; it's still worth STYLE-WARNing about.
                  (compiler-style-warn "~S is not a ~S."
                                       value eltype)))))
           `(let ((array ,creation-form))
             (multiple-value-bind (vector)
                 (%data-vector-and-index array 0)
               (fill vector initial-element))
             array)))))

;;; The integer type restriction on the length ensures that it will be
;;; a vector. The lack of :ADJUSTABLE, :FILL-POINTER, and
;;; :DISPLACED-TO keywords ensures that it will be simple; the lack of
;;; :INITIAL-ELEMENT relies on another transform to deal with that
;;; kind of initialization efficiently.
(deftransform make-array ((length &key element-type)
                          (integer &rest *))
  (let* ((eltype (cond ((not element-type) t)
                       ((not (constant-lvar-p element-type))
                        (give-up-ir1-transform
                         "ELEMENT-TYPE is not constant."))
                       (t
                        (lvar-value element-type))))
         (len (if (constant-lvar-p length)
                  (lvar-value length)
                  '*))
         (eltype-type (ir1-transform-specifier-type eltype))
         (result-type-spec
          `(simple-array
            ,(if (unknown-type-p eltype-type)
                 (give-up-ir1-transform
                  "ELEMENT-TYPE is an unknown type: ~S" eltype)
                 (sb!xc:upgraded-array-element-type eltype))
            (,len)))
         (saetp (find-if (lambda (saetp)
                           (csubtypep eltype-type (sb!vm:saetp-ctype saetp)))
                         sb!vm:*specialized-array-element-type-properties*)))
    (unless saetp
      (give-up-ir1-transform
       "cannot open-code creation of ~S" result-type-spec))
    #-sb-xc-host
    (unless (ctypep (sb!vm:saetp-initial-element-default saetp) eltype-type)
      ;; This situation arises e.g. in (MAKE-ARRAY 4 :ELEMENT-TYPE
      ;; '(INTEGER 1 5)) ANSI's definition of MAKE-ARRAY says "If
      ;; INITIAL-ELEMENT is not supplied, the consequences of later
      ;; reading an uninitialized element of new-array are undefined,"
      ;; so this could be legal code as long as the user plans to
      ;; write before he reads, and if he doesn't we're free to do
      ;; anything we like. But in case the user doesn't know to write
      ;; elements before he reads elements (or to read manuals before
      ;; he writes code:-), we'll signal a STYLE-WARNING in case he
      ;; didn't realize this.
      (compiler-style-warn "The default initial element ~S is not a ~S."
                           (sb!vm:saetp-initial-element-default saetp)
                           eltype))
    (let* ((n-bits-per-element (sb!vm:saetp-n-bits saetp))
           (typecode (sb!vm:saetp-typecode saetp))
           (n-pad-elements (sb!vm:saetp-n-pad-elements saetp))
           (padded-length-form (if (zerop n-pad-elements)
                                   'length
                                   `(+ length ,n-pad-elements)))
           (n-words-form
            (cond
              ((= n-bits-per-element 0) 0)
              ((>= n-bits-per-element sb!vm:n-word-bits)
               `(* ,padded-length-form
                 (the fixnum ; i.e., not RATIO
                   ,(/ n-bits-per-element sb!vm:n-word-bits))))
              (t
               (let ((n-elements-per-word (/ sb!vm:n-word-bits
                                             n-bits-per-element)))
                 (declare (type index n-elements-per-word)) ; i.e., not RATIO
                 `(ceiling ,padded-length-form ,n-elements-per-word))))))
      (values
       `(truly-the ,result-type-spec
         (allocate-vector ,typecode length ,n-words-form))
       '((declare (type index length)))))))

;;; The list type restriction does not ensure that the result will be a
;;; multi-dimensional array. But the lack of adjustable, fill-pointer,
;;; and displaced-to keywords ensures that it will be simple.
;;;
;;; FIXME: should we generalize this transform to non-simple (though
;;; non-displaced-to) arrays, given that we have %WITH-ARRAY-DATA to
;;; deal with those? Maybe when the DEFTRANSFORM
;;; %DATA-VECTOR-AND-INDEX in the VECTOR case problem is solved? --
;;; CSR, 2002-07-01
(deftransform make-array ((dims &key element-type)
                          (list &rest *))
  (unless (or (null element-type) (constant-lvar-p element-type))
    (give-up-ir1-transform
     "The element-type is not constant; cannot open code array creation."))
  (unless (constant-lvar-p dims)
    (give-up-ir1-transform
     "The dimension list is not constant; cannot open code array creation."))
  (let ((dims (lvar-value dims)))
    (unless (every #'integerp dims)
      (give-up-ir1-transform
       "The dimension list contains something other than an integer: ~S"
       dims))
    (if (= (length dims) 1)
        `(make-array ',(car dims)
                     ,@(when element-type
                         '(:element-type element-type)))
        (let* ((total-size (reduce #'* dims))
               (rank (length dims))
               (spec `(simple-array
                       ,(cond ((null element-type) t)
                              ((and (constant-lvar-p element-type)
                                    (ir1-transform-specifier-type
                                     (lvar-value element-type)))
                               (sb!xc:upgraded-array-element-type
                                (lvar-value element-type)))
                              (t '*))
                           ,(make-list rank :initial-element '*))))
          `(let ((header (make-array-header sb!vm:simple-array-widetag ,rank)))
             (setf (%array-fill-pointer header) ,total-size)
             (setf (%array-fill-pointer-p header) nil)
             (setf (%array-available-elements header) ,total-size)
             (setf (%array-data-vector header)
                   (make-array ,total-size
                               ,@(when element-type
                                   '(:element-type element-type))))
             (setf (%array-displaced-p header) nil)
             ,@(let ((axis -1))
                 (mapcar (lambda (dim)
                           `(setf (%array-dimension header ,(incf axis))
                                  ,dim))
                         dims))
             (truly-the ,spec header))))))

;;;; miscellaneous properties of arrays

;;; Transforms for various array properties. If the property is know
;;; at compile time because of a type spec, use that constant value.

;;; Most of this logic may end up belonging in code/late-type.lisp;
;;; however, here we also need the -OR-GIVE-UP for the transforms, and
;;; maybe this is just too sloppy for actual type logic.  -- CSR,
;;; 2004-02-18
(defun array-type-dimensions-or-give-up (type)
  (typecase type
    (array-type (array-type-dimensions type))
    (union-type
     (let ((types (union-type-types type)))
       ;; there are at least two types, right?
       (aver (> (length types) 1))
       (let ((result (array-type-dimensions-or-give-up (car types))))
         (dolist (type (cdr types) result)
           (unless (equal (array-type-dimensions-or-give-up type) result)
             (give-up-ir1-transform))))))
    ;; FIXME: intersection type [e.g. (and (array * (*)) (satisfies foo)) ]
    (t (give-up-ir1-transform))))

(defun conservative-array-type-complexp (type)
  (typecase type
    (array-type (array-type-complexp type))
    (union-type
     (let ((types (union-type-types type)))
       (aver (> (length types) 1))
       (let ((result (conservative-array-type-complexp (car types))))
         (dolist (type (cdr types) result)
           (unless (eq (conservative-array-type-complexp type) result)
             (return-from conservative-array-type-complexp :maybe))))))
    ;; FIXME: intersection type
    (t :maybe)))

;;; If we can tell the rank from the type info, use it instead.
(deftransform array-rank ((array))
  (let ((array-type (lvar-type array)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
      (if (not (listp dims))
          (give-up-ir1-transform
           "The array rank is not known at compile time: ~S"
           dims)
          (length dims)))))

;;; If we know the dimensions at compile time, just use it. Otherwise,
;;; if we can tell that the axis is in bounds, convert to
;;; %ARRAY-DIMENSION (which just indirects the array header) or length
;;; (if it's simple and a vector).
(deftransform array-dimension ((array axis)
                               (array index))
  (unless (constant-lvar-p axis)
    (give-up-ir1-transform "The axis is not constant."))
  (let ((array-type (lvar-type array))
        (axis (lvar-value axis)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
      (unless (listp dims)
        (give-up-ir1-transform
         "The array dimensions are unknown; must call ARRAY-DIMENSION at runtime."))
      (unless (> (length dims) axis)
        (abort-ir1-transform "The array has dimensions ~S, ~W is too large."
                             dims
                             axis))
      (let ((dim (nth axis dims)))
        (cond ((integerp dim)
               dim)
              ((= (length dims) 1)
               (ecase (conservative-array-type-complexp array-type)
                 ((t)
                  '(%array-dimension array 0))
                 ((nil)
                  '(length array))
                 ((:maybe)
                  (give-up-ir1-transform
                   "can't tell whether array is simple"))))
              (t
               '(%array-dimension array axis)))))))

;;; If the length has been declared and it's simple, just return it.
(deftransform length ((vector)
                      ((simple-array * (*))))
  (let ((type (lvar-type vector)))
    (let ((dims (array-type-dimensions-or-give-up type)))
      (unless (and (listp dims) (integerp (car dims)))
        (give-up-ir1-transform
         "Vector length is unknown, must call LENGTH at runtime."))
      (car dims))))

;;; All vectors can get their length by using VECTOR-LENGTH. If it's
;;; simple, it will extract the length slot from the vector. It it's
;;; complex, it will extract the fill pointer slot from the array
;;; header.
(deftransform length ((vector) (vector))
  '(vector-length vector))

;;; If a simple array with known dimensions, then VECTOR-LENGTH is a
;;; compile-time constant.
(deftransform vector-length ((vector))
  (let ((vtype (lvar-type vector)))
    (let ((dim (first (array-type-dimensions-or-give-up vtype))))
      (when (eq dim '*)
        (give-up-ir1-transform))
      (when (conservative-array-type-complexp vtype)
        (give-up-ir1-transform))
      dim)))

;;; Again, if we can tell the results from the type, just use it.
;;; Otherwise, if we know the rank, convert into a computation based
;;; on array-dimension. We can wrap a TRULY-THE INDEX around the
;;; multiplications because we know that the total size must be an
;;; INDEX.
(deftransform array-total-size ((array)
                                (array))
  (let ((array-type (lvar-type array)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
      (unless (listp dims)
        (give-up-ir1-transform "can't tell the rank at compile time"))
      (if (member '* dims)
          (do ((form 1 `(truly-the index
                                   (* (array-dimension array ,i) ,form)))
               (i 0 (1+ i)))
              ((= i (length dims)) form))
          (reduce #'* dims)))))

;;; Only complex vectors have fill pointers.
(deftransform array-has-fill-pointer-p ((array))
  (let ((array-type (lvar-type array)))
    (let ((dims (array-type-dimensions-or-give-up array-type)))
      (if (and (listp dims) (not (= (length dims) 1)))
          nil
          (ecase (conservative-array-type-complexp array-type)
            ((t)
             t)
            ((nil)
             nil)
            ((:maybe)
             (give-up-ir1-transform
              "The array type is ambiguous; must call ~
               ARRAY-HAS-FILL-POINTER-P at runtime.")))))))

;;; Primitive used to verify indices into arrays. If we can tell at
;;; compile-time or we are generating unsafe code, don't bother with
;;; the VOP.
(deftransform %check-bound ((array dimension index) * * :node node)
  (cond ((policy node (and (> speed safety) (= safety 0)))
         'index)
        ((not (constant-lvar-p dimension))
         (give-up-ir1-transform))
        (t
         (let ((dim (lvar-value dimension)))
           `(the (integer 0 (,dim)) index)))))

;;;; WITH-ARRAY-DATA

;;; This checks to see whether the array is simple and the start and
;;; end are in bounds. If so, it proceeds with those values.
;;; Otherwise, it calls %WITH-ARRAY-DATA. Note that %WITH-ARRAY-DATA
;;; may be further optimized.
;;;
;;; Given any ARRAY, bind DATA-VAR to the array's data vector and
;;; START-VAR and END-VAR to the start and end of the designated
;;; portion of the data vector. SVALUE and EVALUE are any start and
;;; end specified to the original operation, and are factored into the
;;; bindings of START-VAR and END-VAR. OFFSET-VAR is the cumulative
;;; offset of all displacements encountered, and does not include
;;; SVALUE.
;;;
;;; When FORCE-INLINE is set, the underlying %WITH-ARRAY-DATA form is
;;; forced to be inline, overriding the ordinary judgment of the
;;; %WITH-ARRAY-DATA DEFTRANSFORMs. Ordinarily the DEFTRANSFORMs are
;;; fairly picky about their arguments, figuring that if you haven't
;;; bothered to get all your ducks in a row, you probably don't care
;;; that much about speed anyway! But in some cases it makes sense to
;;; do type testing inside %WITH-ARRAY-DATA instead of outside, and
;;; the DEFTRANSFORM can't tell that that's going on, so it can make
;;; sense to use FORCE-INLINE option in that case.
(def!macro with-array-data (((data-var array &key offset-var)
                             (start-var &optional (svalue 0))
                             (end-var &optional (evalue nil))
                             &key force-inline)
                            &body forms)
  (once-only ((n-array array)
              (n-svalue `(the index ,svalue))
              (n-evalue `(the (or index null) ,evalue)))
    `(multiple-value-bind (,data-var
                           ,start-var
                           ,end-var
                           ,@(when offset-var `(,offset-var)))
         (if (not (array-header-p ,n-array))
             (let ((,n-array ,n-array))
               (declare (type (simple-array * (*)) ,n-array))
               ,(once-only ((n-len `(length ,n-array))
                            (n-end `(or ,n-evalue ,n-len)))
                  `(if (<= ,n-svalue ,n-end ,n-len)
                       ;; success
                       (values ,n-array ,n-svalue ,n-end 0)
                       (failed-%with-array-data ,n-array
                                                ,n-svalue
                                                ,n-evalue))))
             (,(if force-inline '%with-array-data-macro '%with-array-data)
              ,n-array ,n-svalue ,n-evalue))
       ,@forms)))

;;; This is the fundamental definition of %WITH-ARRAY-DATA, for use in
;;; DEFTRANSFORMs and DEFUNs.
(def!macro %with-array-data-macro (array
                                   start
                                   end
                                   &key
                                   (element-type '*)
                                   unsafe?
                                   fail-inline?)
  (with-unique-names (size defaulted-end data cumulative-offset)
    `(let* ((,size (array-total-size ,array))
            (,defaulted-end
              (cond (,end
                     (unless (or ,unsafe? (<= ,end ,size))
                       ,(if fail-inline?
                            `(error 'bounding-indices-bad-error
                              :datum (cons ,start ,end)
                              :expected-type `(cons (integer 0 ,',size)
                                                    (integer ,',start ,',size))
                              :object ,array)
                            `(failed-%with-array-data ,array ,start ,end)))
                     ,end)
                    (t ,size))))
       (unless (or ,unsafe? (<= ,start ,defaulted-end))
         ,(if fail-inline?
              `(error 'bounding-indices-bad-error
                :datum (cons ,start ,end)
                :expected-type `(cons (integer 0 ,',size)
                                      (integer ,',start ,',size))
                :object ,array)
              `(failed-%with-array-data ,array ,start ,end)))
       (do ((,data ,array (%array-data-vector ,data))
            (,cumulative-offset 0
                                (+ ,cumulative-offset
                                   (%array-displacement ,data))))
           ((not (array-header-p ,data))
            (values (the (simple-array ,element-type 1) ,data)
                    (the index (+ ,cumulative-offset ,start))
                    (the index (+ ,cumulative-offset ,defaulted-end))
                    (the index ,cumulative-offset)))
         (declare (type index ,cumulative-offset))))))

(deftransform %with-array-data ((array start end)
                                ;; It might very well be reasonable to
                                ;; allow general ARRAY here, I just
                                ;; haven't tried to understand the
                                ;; performance issues involved. --
                                ;; WHN, and also CSR 2002-05-26
                                ((or vector simple-array) index (or index null))
                                *
                                :node node
                                :policy (> speed space))
  "inline non-SIMPLE-vector-handling logic"
  (let ((element-type (upgraded-element-type-specifier-or-give-up array)))
    `(%with-array-data-macro array start end
                             :unsafe? ,(policy node (= safety 0))
                             :element-type ,element-type)))

;;;; array accessors

;;; We convert all typed array accessors into AREF and %ASET with type
;;; assertions on the array.
(macrolet ((define-bit-frob (reffer setter simplep)
             `(progn
                (define-source-transform ,reffer (a &rest i)
                  `(aref (the (,',(if simplep 'simple-array 'array)
                                  bit
                                  ,(mapcar (constantly '*) i))
                           ,a) ,@i))
                (define-source-transform ,setter (a &rest i)
                  `(%aset (the (,',(if simplep 'simple-array 'array)
                                   bit
                                   ,(cdr (mapcar (constantly '*) i)))
                            ,a) ,@i)))))
  (define-bit-frob sbit %sbitset t)
  (define-bit-frob bit %bitset nil))
(macrolet ((define-frob (reffer setter type)
             `(progn
                (define-source-transform ,reffer (a i)
                  `(aref (the ,',type ,a) ,i))
                (define-source-transform ,setter (a i v)
                  `(%aset (the ,',type ,a) ,i ,v)))))
  (define-frob svref %svset simple-vector)
  (define-frob schar %scharset simple-string)
  (define-frob char %charset string))

(macrolet (;; This is a handy macro for computing the row-major index
           ;; given a set of indices. We wrap each index with a call
           ;; to %CHECK-BOUND to ensure that everything works out
           ;; correctly. We can wrap all the interior arithmetic with
           ;; TRULY-THE INDEX because we know the resultant
           ;; row-major index must be an index.
           (with-row-major-index ((array indices index &optional new-value)
                                  &rest body)
             `(let (n-indices dims)
                (dotimes (i (length ,indices))
                  (push (make-symbol (format nil "INDEX-~D" i)) n-indices)
                  (push (make-symbol (format nil "DIM-~D" i)) dims))
                (setf n-indices (nreverse n-indices))
                (setf dims (nreverse dims))
                `(lambda (,',array ,@n-indices
                                   ,@',(when new-value (list new-value)))
                   (let* (,@(let ((,index -1))
                              (mapcar (lambda (name)
                                        `(,name (array-dimension
                                                 ,',array
                                                 ,(incf ,index))))
                                      dims))
                            (,',index
                             ,(if (null dims)
                                  0
                                (do* ((dims dims (cdr dims))
                                      (indices n-indices (cdr indices))
                                      (last-dim nil (car dims))
                                      (form `(%check-bound ,',array
                                                           ,(car dims)
                                                           ,(car indices))
                                            `(truly-the
                                              index
                                              (+ (truly-the index
                                                            (* ,form
                                                               ,last-dim))
                                                 (%check-bound
                                                  ,',array
                                                  ,(car dims)
                                                  ,(car indices))))))
                                    ((null (cdr dims)) form)))))
                     ,',@body)))))

  ;; Just return the index after computing it.
  (deftransform array-row-major-index ((array &rest indices))
    (with-row-major-index (array indices index)
      index))

  ;; Convert AREF and %ASET into a HAIRY-DATA-VECTOR-REF (or
  ;; HAIRY-DATA-VECTOR-SET) with the set of indices replaced with the an
  ;; expression for the row major index.
  (deftransform aref ((array &rest indices))
    (with-row-major-index (array indices index)
      (hairy-data-vector-ref array index)))
  (deftransform %aset ((array &rest stuff))
    (let ((indices (butlast stuff)))
      (with-row-major-index (array indices index new-value)
        (hairy-data-vector-set array index new-value)))))

;;; Just convert into a HAIRY-DATA-VECTOR-REF (or
;;; HAIRY-DATA-VECTOR-SET) after checking that the index is inside the
;;; array total size.
(deftransform row-major-aref ((array index))
  `(hairy-data-vector-ref array
                          (%check-bound array (array-total-size array) index)))
(deftransform %set-row-major-aref ((array index new-value))
  `(hairy-data-vector-set array
                          (%check-bound array (array-total-size array) index)
                          new-value))

;;;; bit-vector array operation canonicalization
;;;;
;;;; We convert all bit-vector operations to have the result array
;;;; specified. This allows any result allocation to be open-coded,
;;;; and eliminates the need for any VM-dependent transforms to handle
;;;; these cases.

(macrolet ((def (fun)
             `(progn
               (deftransform ,fun ((bit-array-1 bit-array-2
                                                &optional result-bit-array)
                                   (bit-vector bit-vector &optional null) *
                                   :policy (>= speed space))
                 `(,',fun bit-array-1 bit-array-2
                   (make-array (array-dimension bit-array-1 0) :element-type 'bit)))
               ;; If result is T, make it the first arg.
               (deftransform ,fun ((bit-array-1 bit-array-2 result-bit-array)
                                   (bit-vector bit-vector (eql t)) *)
                 `(,',fun bit-array-1 bit-array-2 bit-array-1)))))
  (def bit-and)
  (def bit-ior)
  (def bit-xor)
  (def bit-eqv)
  (def bit-nand)
  (def bit-nor)
  (def bit-andc1)
  (def bit-andc2)
  (def bit-orc1)
  (def bit-orc2))

;;; Similar for BIT-NOT, but there is only one arg...
(deftransform bit-not ((bit-array-1 &optional result-bit-array)
                       (bit-vector &optional null) *
                       :policy (>= speed space))
  '(bit-not bit-array-1
            (make-array (array-dimension bit-array-1 0) :element-type 'bit)))
(deftransform bit-not ((bit-array-1 result-bit-array)
                       (bit-vector (eql t)))
  '(bit-not bit-array-1 bit-array-1))

;;; Pick off some constant cases.
(defoptimizer (array-header-p derive-type) ((array))
  (let ((type (lvar-type array)))
    (cond ((not (array-type-p type))
           ;; FIXME: use analogue of ARRAY-TYPE-DIMENSIONS-OR-GIVE-UP
           nil)
          (t
           (let ((dims (array-type-dimensions type)))
             (cond ((csubtypep type (specifier-type '(simple-array * (*))))
                    ;; no array header
                    (specifier-type 'null))
                   ((and (listp dims) (/= (length dims) 1))
                    ;; multi-dimensional array, will have a header
                    (specifier-type '(eql t)))
                   ((eql (array-type-complexp type) t)
                    (specifier-type '(eql t)))
                   (t
                    nil)))))))
