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

;;; FIXME: It would be good to implement SB!XC:DEFCONSTANT, and use
;;; use that here, so that the compiler is born knowing this value.
;;; FIXME: Add a comment telling whether this holds for all vectors
;;; or only for vectors based on simple arrays (non-adjustable, etc.).
(defconstant vector-data-bit-offset
  (* sb!vm:vector-data-offset sb!vm:word-bits))

;;; We need to define these predicates, since the TYPEP source
;;; transform picks whichever predicate was defined last when there
;;; are multiple predicates for equivalent types.
(def-source-transform short-float-p (x) `(single-float-p ,x))
#!-long-float
(def-source-transform long-float-p (x) `(double-float-p ,x))

(def-source-transform compiled-function-p (x)
  `(functionp ,x))

(def-source-transform char-int (x)
  `(char-code ,x))

(deftransform abs ((x) (rational))
  '(if (< x 0) (- x) x))

;;; The layout is stored in slot 0.
(def-source-transform %instance-layout (x)
  `(truly-the layout (%instance-ref ,x 0)))
(def-source-transform %set-instance-layout (x val)
  `(%instance-set ,x 0 (the layout ,val)))

;;;; character support

;;; In our implementation there are really only BASE-CHARs.
(def-source-transform characterp (obj)
  `(base-char-p ,obj))

;;;; simplifying HAIRY-DATA-VECTOR-REF and HAIRY-DATA-VECTOR-SET

(deftransform hairy-data-vector-ref ((array index) (array t) * :important t)
  "avoid runtime dispatch on array element type"
  (let ((element-ctype (extract-upgraded-element-type array)))
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
	   ;; FIXME: All this noise should move into a
	   ;; %DATA-VECTOR-AND-INDEX function, and there should be
	   ;; DEFTRANSFORMs for %DATA-VECTOR-AND-INDEX to optimize the
	   ;; function call away when the array is known to be simple,
	   ;; and to specialize to
	   ;; %DATA-VECTOR-AND-INDEX-IN-VECTOR-CASE when the array is
	   ;; known to have only one dimension.
	   (if (array-header-p array)
	       (%with-array-data array index nil)
	       (let ((array array))
		 (declare (type (simple-array ,element-type-specifier 1)
				array))
		 (%check-bound array 0 index)
		 (values array index)))
	 (declare (type (simple-array ,element-type-specifier 1) array))
	 (data-vector-ref array index)))))

;;; MNA: open-coded-simple-array patch
(deftransform data-vector-ref ((array index)
                               (simple-array t))
  (let ((array-type (continuation-type array)))
    ;; FIXME: How could this happen? Doesn't the limitation to arg
    ;; type SIMPLE-ARRAY guarantee that ARRAY-TYPE is an ARRAY-TYPE?
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (when (and (consp dims) (= (length dims) 1))
        (give-up-ir1-transform))
      (let* ((el-type (array-type-element-type array-type))
             (total-size (if (or (atom dims) (member '* dims))
			     '*
			     (reduce #'* dims)))
             (type-sp `(simple-array ,(type-specifier el-type)
                        (,total-size))))
        (if (atom dims)
          `(let ((a (truly-the ,type-sp (%array-simp array))))
            (data-vector-ref a index))
          `(let ((a (truly-the ,type-sp (%array-data-vector array))))
            (data-vector-ref a index)))))))

(deftransform hairy-data-vector-set ((array index new-value)
				     (array t t)
				     *
				     :important t)
  "avoid runtime dispatch on array element type"
  (let ((element-ctype (extract-upgraded-element-type array)))
    (declare (type ctype element-ctype))
    (when (eq *wild-type* element-ctype)
      (give-up-ir1-transform
       "Upgraded element type of array is not known at compile time."))
    (let ((element-type-specifier (type-specifier element-ctype)))
      `(multiple-value-bind (array index)
	   ;; FIXME: All this noise should move into a
	   ;; %DATA-VECTOR-AND-INDEX function, and there should be
	   ;; DEFTRANSFORMs for %DATA-VECTOR-AND-INDEX to optimize the
	   ;; function call away when the array is known to be simple,
	   ;; and to specialize to
	   ;; %DATA-VECTOR-AND-INDEX-IN-VECTOR-CASE when the array is
	   ;; known to have only one dimension.
	   (if (array-header-p array)
	       (%with-array-data array index nil)
	       (let ((array array))
		 (declare (type (simple-array ,element-type-specifier 1)
				array))
		 (%check-bound array 0 index)
		 (values array index)))
	 (data-vector-set (truly-the (simple-array ,element-type-specifier 1)
				     array)
			  index
			  new-value)))))

;;; MNA: open-coded-simple-array patch
(deftransform data-vector-set ((array index new-value)
			       (simple-array t t))
  (let ((array-type (continuation-type array)))
    ;; FIXME: How could this happen? Doesn't the limitation to arg
    ;; type SIMPLE-ARRAY guarantee that ARRAY-TYPE is an ARRAY-TYPE?
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (when (and (consp dims) (= (length dims) 1))
	(give-up-ir1-transform))
      (let* ((el-type (array-type-element-type array-type))
             (total-size (if (or (atom dims) (member '* dims))
			     '*
			     (reduce #'* dims)))
             (type-sp `(simple-array ,(type-specifier el-type)
                        (,total-size))))
	(if (atom dims)
	    `(let ((a (truly-the ,type-sp (%array-simp array))))
	       (data-vector-set a index new-value))
	    `(let ((a (truly-the ,type-sp (%array-data-vector array))))
	       (data-vector-set a index new-value)))))))

;;; transforms for getting at simple arrays of (UNSIGNED-BYTE N) when (< N 8)
;;;
;;; FIXME: In CMU CL, these were commented out with #+NIL. Why? Should
;;; we fix them or should we delete them? (Perhaps these definitions
;;; predate the various DATA-VECTOR-REF-FOO VOPs which have
;;; (:TRANSLATE DATA-VECTOR-REF), and are redundant now?)
#+nil
(macrolet
    ((frob (type bits)
       (let ((elements-per-word (truncate sb!vm:word-bits bits)))
	 `(progn
	    (deftransform data-vector-ref ((vector index)
					   (,type *))
	      `(multiple-value-bind (word bit)
		   (floor index ,',elements-per-word)
		 (ldb ,(ecase sb!vm:target-byte-order
			 (:little-endian '(byte ,bits (* bit ,bits)))
			 (:big-endian '(byte ,bits (- sb!vm:word-bits
						      (* (1+ bit) ,bits)))))
		      (%raw-bits vector (+ word sb!vm:vector-data-offset)))))
	    (deftransform data-vector-set ((vector index new-value)
					   (,type * *))
	      `(multiple-value-bind (word bit)
		   (floor index ,',elements-per-word)
		 (setf (ldb ,(ecase sb!vm:target-byte-order
			       (:little-endian '(byte ,bits (* bit ,bits)))
			       (:big-endian
				'(byte ,bits (- sb!vm:word-bits
						(* (1+ bit) ,bits)))))
			    (%raw-bits vector (+ word sb!vm:vector-data-offset)))
		       new-value)))))))
  (frob simple-bit-vector 1)
  (frob (simple-array (unsigned-byte 2) (*)) 2)
  (frob (simple-array (unsigned-byte 4) (*)) 4))

;;;; bit vector hackery

;;; SIMPLE-BIT-VECTOR bit-array operations are transformed to a word loop that
;;; does 32 bits at a time.
;;;
;;; FIXME: This is a lot of repeatedly macroexpanded code. It should be a
;;; function call instead. And do it with DEF-FROB instead of DOLIST.
(dolist (x '((bit-and 32bit-logical-and)
	     (bit-ior 32bit-logical-or)
	     (bit-xor 32bit-logical-xor)
	     (bit-eqv 32bit-logical-eqv)
	     (bit-nand 32bit-logical-nand)
	     (bit-nor 32bit-logical-nor)
	     (bit-andc1 32bit-logical-andc1)
	     (bit-andc2 32bit-logical-andc2)
	     (bit-orc1 32bit-logical-orc1)
	     (bit-orc2 32bit-logical-orc2)))
  (destructuring-bind (bitfun wordfun) x
    (deftransform bitfun
		  ((bit-array-1 bit-array-2 result-bit-array)
		   '(simple-bit-vector simple-bit-vector simple-bit-vector) '*
		   :eval-name t :node node :policy (>= speed space))
      `(progn
	 ,@(unless (policy node (zerop safety))
	     '((unless (= (length bit-array-1) (length bit-array-2)
			  (length result-bit-array))
		 (error "Argument and/or result bit arrays are not the same length:~
			 ~%  ~S~%  ~S  ~%  ~S"
			bit-array-1 bit-array-2 result-bit-array))))
	 (do ((index sb!vm:vector-data-offset (1+ index))
	      (end (+ sb!vm:vector-data-offset
		      (truncate (the index
				     (+ (length bit-array-1)
					sb!vm:word-bits -1))
				sb!vm:word-bits))))
	     ((= index end) result-bit-array)
	   (declare (optimize (speed 3) (safety 0))
		    (type index index end))
	   (setf (%raw-bits result-bit-array index)
		 (,wordfun (%raw-bits bit-array-1 index)
			   (%raw-bits bit-array-2 index))))))))

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
     (do ((index sb!vm:vector-data-offset (1+ index))
	  (end (+ sb!vm:vector-data-offset
		  (truncate (the index
				 (+ (length bit-array)
				    (1- sb!vm:word-bits)))
			    sb!vm:word-bits))))
	 ((= index end) result-bit-array)
       (declare (optimize (speed 3) (safety 0))
		(type index index end))
       (setf (%raw-bits result-bit-array index)
	     (32bit-logical-not (%raw-bits bit-array index))))))

;;;; primitive translator for BYTE-BLT

(def-primitive-translator byte-blt (src src-start dst dst-start dst-end)
  `(let ((src ,src)
	 (src-start (* ,src-start sb!vm:byte-bits))
	 (dst ,dst)
	 (dst-start (* ,dst-start sb!vm:byte-bits))
	 (dst-end (* ,dst-end sb!vm:byte-bits)))
     (let ((length (- dst-end dst-start)))
       (etypecase src
	 (system-area-pointer
	  (etypecase dst
	    (system-area-pointer
	     (system-area-copy src src-start dst dst-start length))
	    ((simple-unboxed-array (*))
	     (copy-from-system-area src src-start
				    dst (+ dst-start ,vector-data-bit-offset)
				    length))))
	 ((simple-unboxed-array (*))
	  (etypecase dst
	    (system-area-pointer
	     (copy-to-system-area src (+ src-start ,vector-data-bit-offset)
				  dst dst-start
				  length))
	    ((simple-unboxed-array (*))
	     (bit-bash-copy src (+ src-start ,vector-data-bit-offset)
			    dst (+ dst-start ,vector-data-bit-offset)
			    length))))))))

;;;; transforms for EQL of floating point values

(deftransform eql ((x y) (single-float single-float))
  '(= (single-float-bits x) (single-float-bits y)))

(deftransform eql ((x y) (double-float double-float))
  '(and (= (double-float-low-bits x) (double-float-low-bits y))
	(= (double-float-high-bits x) (double-float-high-bits y))))

