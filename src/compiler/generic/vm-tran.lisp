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
  (* sb!vm:vector-data-offset sb!vm:n-word-bits))

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

(deftransform data-vector-ref ((array index)
                               (simple-array t))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (when (or (atom dims) (= (length dims) 1))
        (give-up-ir1-transform))
      (let ((el-type (array-type-element-type array-type))
            (total-size (if (member '* dims)
                            '*
                            (reduce #'* dims))))
        `(data-vector-ref (truly-the (simple-array ,(type-specifier el-type)
                                                   (,total-size))
                                     (%array-data-vector array))
                          index)))))

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

(deftransform data-vector-set ((array index new-value)
                               (simple-array t t))
  (let ((array-type (continuation-type array)))
    (unless (array-type-p array-type)
      (give-up-ir1-transform))
    (let ((dims (array-type-dimensions array-type)))
      (when (or (atom dims) (= (length dims) 1))
        (give-up-ir1-transform))
      (let ((el-type (array-type-element-type array-type))
            (total-size (if (member '* dims)
                            '*
                            (reduce #'* dims))))
        `(data-vector-set (truly-the (simple-array ,(type-specifier el-type)
                                                   (,total-size))
                                     (%array-data-vector array))
                          index
                          new-value)))))

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
					sb!vm:n-word-bits -1))
				sb!vm:n-word-bits))))
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
				    (1- sb!vm:n-word-bits)))
			    sb!vm:n-word-bits))))
	 ((= index end) result-bit-array)
       (declare (optimize (speed 3) (safety 0))
		(type index index end))
       (setf (%raw-bits result-bit-array index)
	     (32bit-logical-not (%raw-bits bit-array index))))))

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
     nil))

;;;; transforms for EQL of floating point values

(deftransform eql ((x y) (single-float single-float))
  '(= (single-float-bits x) (single-float-bits y)))

(deftransform eql ((x y) (double-float double-float))
  '(and (= (double-float-low-bits x) (double-float-low-bits y))
	(= (double-float-high-bits x) (double-float-high-bits y))))

