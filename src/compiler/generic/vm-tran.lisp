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
  `(functionp ,x))

(define-source-transform char-int (x)
  `(char-code ,x))

(deftransform abs ((x) (rational))
  '(if (< x 0) (- x) x))

;;; The layout is stored in slot 0.
(define-source-transform %instance-layout (x)
  `(truly-the layout (%instance-ref ,x 0)))
(define-source-transform %set-instance-layout (x val)
  `(%instance-set ,x 0 (the layout ,val)))

;;;; character support

;;; In our implementation there are really only BASE-CHARs.
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
	  ((simple-array nil (*)) (data-vector-ref string index))))))

(deftransform hairy-data-vector-ref ((array index) (array t) * :important t)
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
	  ((simple-array nil (*))
	   (data-vector-set string index new-value))))))

(deftransform hairy-data-vector-set ((array index new-value)
				     (array t t)
				     *
				     :important t)
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
				      *
				      :important t)
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
			    ((= index end-1)
			     (setf (%raw-bits result-bit-array index)
				   (,',wordfun (%raw-bits bit-array-1 index)
					       (%raw-bits bit-array-2 index)))
			     result-bit-array)
			  (declare (optimize (speed 3) (safety 0))
				   (type index index end-1))
			  (setf (%raw-bits result-bit-array index)
				(,',wordfun (%raw-bits bit-array-1 index)
					    (%raw-bits bit-array-2 index))))))))))
 (def bit-and 32bit-logical-and)
 (def bit-ior 32bit-logical-or)
 (def bit-xor 32bit-logical-xor)
 (def bit-eqv 32bit-logical-eqv)
 (def bit-nand 32bit-logical-nand)
 (def bit-nor 32bit-logical-nor)
 (def bit-andc1 32bit-logical-andc1)
 (def bit-andc2 32bit-logical-andc2)
 (def bit-orc1 32bit-logical-orc1)
 (def bit-orc2 32bit-logical-orc2))

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
	  ;; We avoid doing anything to 0-length bit-vectors, or
	  ;; rather, the memory that follows them. Other
	  ;; divisible-by-32 cases are handled by the (1- length),
	  ;; below.  CSR, 2002-04-24
	  result-bit-array
	  (do ((index sb!vm:vector-data-offset (1+ index))
	       (end-1 (+ sb!vm:vector-data-offset
			 ;; bit-vectors of length 1-32 need precisely
			 ;; one (SETF %RAW-BITS), done here in the
			 ;; epilogue. - CSR, 2002-04-24
			 (truncate (truly-the index (1- length))
				   sb!vm:n-word-bits))))
	      ((= index end-1)
	       (setf (%raw-bits result-bit-array index)
		     (32bit-logical-not (%raw-bits bit-array index)))
	       result-bit-array)
	    (declare (optimize (speed 3) (safety 0))
		     (type index index end-1))
	    (setf (%raw-bits result-bit-array index)
		  (32bit-logical-not (%raw-bits bit-array index))))))))

(deftransform bit-vector-= ((x y) (simple-bit-vector simple-bit-vector))
  `(and (= (length x) (length y))
        (let ((length (length x)))
	  (or (= length 0)
	      (do* ((i sb!vm:vector-data-offset (+ i 1))
		    (end-1 (+ sb!vm:vector-data-offset
			      (floor (1- length) sb!vm:n-word-bits))))
		   ((= i end-1)
		    (let* ((extra (mod length sb!vm:n-word-bits))
			   (mask (1- (ash 1 extra)))
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
		      (declare (type (integer 0 31) extra)
			       (type (unsigned-byte 32) mask numx numy))
		      (= numx numy)))
		(declare (type index i end-1))
		(let ((numx (%raw-bits x i))
		      (numy (%raw-bits y i)))
		  (declare (type (unsigned-byte 32) numx numy))
		  (unless (= numx numy)
		    (return nil))))))))

(deftransform fill ((sequence item) (simple-bit-vector bit) *
		    :policy (>= speed space))
  (let ((value (if (constant-lvar-p item)
		   (if (= (lvar-value item) 0)
		       0
		       #.(1- (ash 1 32)))
		   `(if (= item 0) 0 #.(1- (ash 1 32))))))
    `(let ((length (length sequence))
	   (value ,value))
       (if (= length 0)
	   sequence
	   (do ((index sb!vm:vector-data-offset (1+ index))
		(end-1 (+ sb!vm:vector-data-offset
			  ;; bit-vectors of length 1-32 need precisely
			  ;; one (SETF %RAW-BITS), done here in the
			  ;; epilogue. - CSR, 2002-04-24
			  (truncate (truly-the index (1- length))
				    sb!vm:n-word-bits))))
	       ((= index end-1)
		(setf (%raw-bits sequence index) value)
		sequence)
	     (declare (optimize (speed 3) (safety 0))
		      (type index index end-1))
	     (setf (%raw-bits sequence index) value))))))

(deftransform fill ((sequence item) (simple-base-string base-char) *
		    :policy (>= speed space))
  (let ((value (if (constant-lvar-p item)
		   (let* ((char (lvar-value item))
			  (code (sb!xc:char-code char)))
		     (logior code (ash code 8) (ash code 16) (ash code 24)))
		   `(let ((code (sb!xc:char-code item)))
		     (logior code (ash code 8) (ash code 16) (ash code 24))))))
    `(let ((length (length sequence))
	   (value ,value))
      (multiple-value-bind (times rem)
	  (truncate length 4)
	(do ((index sb!vm:vector-data-offset (1+ index))
	     (end (+ times sb!vm:vector-data-offset)))
	    ((= index end)
	     (let ((place (* times 4)))
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


;;;; 32-bit operations
(define-good-modular-fun logand)
(define-good-modular-fun logior)
;;; FIXME: XOR? ANDC1, ANDC2?  -- CSR, 2003-09-16

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
    (labels ((tub32 (x) `(truly-the (unsigned-byte 32) ,x))
	     (add (next-factor)
	       (setf result
		     (tub32
		      (if result
			  (progn (incf adds) `(+ ,result ,(tub32 next-factor)))
			  next-factor)))))
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
			 `(- ,(tub32 `(ash ,arg ,bitpos))
			     ,(tub32 `(ash ,arg ,first-one))))))
	      (setf first-one nil))
	    (when (logbitp bitpos num)
	      (setf first-one bitpos))))
      (when first-one
	(cond ((= first-one 31))
	      ((= first-one 30) (incf shifts) (add `(ash ,arg 30)))
	      (t
	       (incf shifts 2)
	       (incf adds)
	       (add `(- ,(tub32 `(ash ,arg 31)) 
			,(tub32 `(ash ,arg ,first-one))))))
	(incf shifts)
	(add `(ash ,arg 31))))
    (values result adds shifts)))
