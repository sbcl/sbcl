;;;; that part of the implementation of HASH-TABLE which lives solely
;;;; on the target system, not on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant max-hash sb!xc:most-positive-fixnum))

(deftype hash ()
  `(integer 0 ,max-hash))

;;; FIXME: Does this always make a nonnegative FIXNUM? If so, then
;;; explain why. If not (or if the reason it always makes a
;;; nonnegative FIXNUM is only the accident that pointers in supported
;;; architectures happen to be in the lower half of the address
;;; space), then fix it.
#!-sb-fluid (declaim (inline pointer-hash))
(defun pointer-hash (key)
  (declare (values hash))
  (truly-the hash (%primitive sb!c:make-fixnum key)))

#!-sb-fluid (declaim (inline eq-hash))
(defun eq-hash (key)
  (declare (values hash (member t nil)))
  (values (pointer-hash key)
	  (oddp (get-lisp-obj-address key))))

#!-sb-fluid (declaim (inline equal-hash))
(defun equal-hash (key)
  (declare (values hash (member t nil)))
  (values (sxhash key) nil))

#!-sb-fluid (declaim (inline eql-hash))
(defun eql-hash (key)
  (declare (values hash (member t nil)))
  (if (numberp key)
      (equal-hash key)
      (eq-hash key)))

(defun equalp-hash (key)
  (declare (values hash (member t nil)))
  (values (psxhash key) nil))

(defun almost-primify (num)
  (declare (type index num))
  #!+sb-doc
  "Return an almost prime number greater than or equal to NUM."
  (if (= (rem num 2) 0)
      (setq num (+ 1 num)))
  (if (= (rem num 3) 0)
      (setq num (+ 2 num)))
  (if (= (rem num 7) 0)
      (setq num (+ 4 num)))
  num)

;;;; user-defined hash table tests

(defvar *hash-table-tests* nil)

(defun define-hash-table-test (name test-fun hash-fun)
  #!+sb-doc
  "Define a new kind of hash table test."
  (declare (type symbol name)
	   (type function test-fun hash-fun))
  (setf *hash-table-tests*
	(cons (list name test-fun hash-fun)
	      (remove name *hash-table-tests* :test #'eq :key #'car)))
  name)

;;;; construction and simple accessors

(defconstant +min-hash-table-size+ 16)
(defconstant +min-hash-table-rehash-threshold+ (float 1/16 1.0))
;; as explained by pmai on openprojects #lisp IRC 2002-07-30: #x80000000
;; is bigger than any possible nonEQ hash value, and thus indicates an
;; empty slot; and EQ hash tables don't use HASH-TABLE-HASH-VECTOR
(defconstant +magic-hash-vector-value+ #x80000000)

(defun make-hash-table (&key (test 'eql)
			     (size +min-hash-table-size+)
			     (rehash-size 1.5)
			     (rehash-threshold 1)
			     (weak-p nil))
  #!+sb-doc
  "Create and return a new hash table. The keywords are as follows:
     :TEST -- Indicates what kind of test to use.
     :SIZE -- A hint as to how many elements will be put in this hash
       table.
     :REHASH-SIZE -- Indicates how to expand the table when it fills up.
       If an integer, add space for that many elements. If a floating
       point number (which must be greater than 1.0), multiply the size
       by that amount.
     :REHASH-THRESHOLD -- Indicates how dense the table can become before
       forcing a rehash. Can be any positive number <=1, with density
       approaching zero as the threshold approaches 0. Density 1 means an
       average of one entry per bucket.
     :WEAK-P -- (This is an extension from CMU CL, not currently supported
       in SBCL 0.6.6, but perhaps supported in a future version.) If T,
       don't keep entries if the key would otherwise be garbage."
  (declare (type (or function symbol) test))
  (declare (type unsigned-byte size))
  (when weak-p
    (error "stub: unsupported WEAK-P option"))
  (multiple-value-bind (test test-fun hash-fun)
      (cond ((or (eq test #'eq) (eq test 'eq))
	     (values 'eq #'eq #'eq-hash))
	    ((or (eq test #'eql) (eq test 'eql))
	     (values 'eql #'eql #'eql-hash))
	    ((or (eq test #'equal) (eq test 'equal))
	     (values 'equal #'equal #'equal-hash))
	    ((or (eq test #'equalp) (eq test 'equalp))
	     (values 'equalp #'equalp #'equalp-hash))
	    (t
	     ;; FIXME: I'd like to remove *HASH-TABLE-TESTS* stuff.
	     ;; Failing that, I'd like to rename it to
	     ;; *USER-HASH-TABLE-TESTS*.
	     (dolist (info *hash-table-tests*
			   (error "unknown :TEST for MAKE-HASH-TABLE: ~S"
				  test))
	       (destructuring-bind (test-name test-fun hash-fun) info
		 (when (or (eq test test-name) (eq test test-fun))
		   (return (values test-name test-fun hash-fun)))))))
    (let* ((size (max +min-hash-table-size+
		      (min size
			   ;; SIZE is just a hint, so if the user asks
			   ;; for a SIZE which'd be too big for us to
 			   ;; easily implement, we bump it down.
			   (floor array-dimension-limit 1024))))
	   (rehash-size (if (integerp rehash-size)
			    rehash-size
			    (float rehash-size 1.0)))
	   ;; FIXME: Original REHASH-THRESHOLD default should be 1.0,
	   ;; not 1, to make it easier for the compiler to avoid
	   ;; boxing.
	   (rehash-threshold (max +min-hash-table-rehash-threshold+
				  (float rehash-threshold 1.0)))
	   (size+1 (1+ size))		; The first element is not usable.
           ;; KLUDGE: The most natural way of expressing the below is
           ;; (round (/ (float size+1) rehash-threshold)), and indeed
           ;; it was expressed like that until 0.7.0. However,
           ;; MAKE-HASH-TABLE is called very early in cold-init, and
           ;; the SPARC has no primitive instructions for rounding,
           ;; but only for truncating; therefore, we fudge this issue
           ;; a little. The other uses of truncate, below, similarly
           ;; used to be round. -- CSR, 2002-10-01
	   ;;
	   ;; Note that this has not yet been audited for
	   ;; correctness. It just seems to work. -- CSR, 2002-11-02
	   (scaled-size (truncate (/ (float size+1) rehash-threshold)))
	   (length (almost-primify (max scaled-size
					(1+ +min-hash-table-size+))))
	   (index-vector (make-array length
				     :element-type '(unsigned-byte 32)
				     :initial-element 0))
	   ;; needs to be the same length as the KV vector
           ;; (FIXME: really?  why doesn't the code agree?)
	   (next-vector (make-array size+1
				    :element-type '(unsigned-byte 32)))
	   (kv-vector (make-array (* 2 size+1)
				  :initial-element +empty-ht-slot+))
	   (table (%make-hash-table
		   :test test
		   :test-fun test-fun
		   :hash-fun hash-fun
		   :rehash-size rehash-size
		   :rehash-threshold rehash-threshold
		   :rehash-trigger size
		   :table kv-vector
		   :weak-p weak-p
		   :index-vector index-vector
		   :next-vector next-vector
		   :hash-vector (unless (eq test 'eq)
				  (make-array size+1
					      :element-type '(unsigned-byte 32)
					      :initial-element +magic-hash-vector-value+)))))
      (declare (type index size+1 scaled-size length))
      ;; Set up the free list, all free. These lists are 0 terminated.
      (do ((i 1 (1+ i)))
	  ((>= i size))
	(setf (aref next-vector i) (1+ i)))
      (setf (aref next-vector size) 0)
      (setf (hash-table-next-free-kv table) 1)
      (setf (hash-table-needing-rehash table) 0)
      (setf (aref kv-vector 0) table)
      table)))

(defun hash-table-count (hash-table)
  #!+sb-doc
  "Return the number of entries in the given HASH-TABLE."
  (declare (type hash-table hash-table)
	   (values index))
  (hash-table-number-entries hash-table))

#!+sb-doc
(setf (fdocumentation 'hash-table-rehash-size 'function)
      "Return the rehash-size HASH-TABLE was created with.")

#!+sb-doc
(setf (fdocumentation 'hash-table-rehash-threshold 'function)
      "Return the rehash-threshold HASH-TABLE was created with.")

(defun hash-table-size (hash-table)
  #!+sb-doc
  "Return a size that can be used with MAKE-HASH-TABLE to create a hash
   table that can hold however many entries HASH-TABLE can hold without
   having to be grown."
  (hash-table-rehash-trigger hash-table))

#!+sb-doc
(setf (fdocumentation 'hash-table-test 'function)
      "Return the test HASH-TABLE was created with.")

#!+sb-doc
(setf (fdocumentation 'hash-table-weak-p 'function)
      "Return T if HASH-TABLE will not keep entries for keys that would
   otherwise be garbage, and NIL if it will.")

;;;; accessing functions

;;; Make new vectors for the table, extending the table based on the
;;; rehash-size.
(defun rehash (table)
  (declare (type hash-table table))
  (let* ((old-kv-vector (hash-table-table table))
	 (old-next-vector (hash-table-next-vector table))
	 (old-hash-vector (hash-table-hash-vector table))
	 (old-size (length old-next-vector))
	 (new-size
	  (let ((rehash-size (hash-table-rehash-size table)))
	    (etypecase rehash-size
	      (fixnum
	       (+ rehash-size old-size))
	      (float
	       (the index (truncate (* rehash-size old-size)))))))
	 (new-kv-vector (make-array (* 2 new-size)
				    :initial-element +empty-ht-slot+))
	 (new-next-vector (make-array new-size
				      :element-type '(unsigned-byte 32)
				      :initial-element 0))
	 (new-hash-vector (when old-hash-vector
			    (make-array new-size
					:element-type '(unsigned-byte 32)
					:initial-element +magic-hash-vector-value+)))
	 (old-index-vector (hash-table-index-vector table))
	 (new-length (almost-primify
		      (truncate (/ (float new-size)
				(hash-table-rehash-threshold table)))))
	 (new-index-vector (make-array new-length
				       :element-type '(unsigned-byte 32)
				       :initial-element 0)))
    (declare (type index new-size new-length old-size))

    ;; Disable GC tricks on the OLD-KV-VECTOR.
    (set-header-data old-kv-vector sb!vm:vector-normal-subtype)

    ;; FIXME: here and in several other places in the hash table code,
    ;; loops like this one are used when FILL or REPLACE would be
    ;; appropriate.  why are standard CL functions not used?
    ;; Performance issues?  General laziness?  -- NJF, 2004-03-10

    ;; Copy over the kv-vector. The element positions should not move
    ;; in case there are active scans.
    (dotimes (i (* old-size 2))
      (declare (type index i))
      (setf (aref new-kv-vector i) (aref old-kv-vector i)))

    ;; Copy over the hash-vector.
    (when old-hash-vector
      (dotimes (i old-size)
	(setf (aref new-hash-vector i) (aref old-hash-vector i))))

    (setf (hash-table-next-free-kv table) 0)
    (setf (hash-table-needing-rehash table) 0)
    ;; Rehash all the entries; last to first so that after the pushes
    ;; the chains are first to last.
    (do ((i (1- new-size) (1- i)))
	((zerop i))
      (let ((key (aref new-kv-vector (* 2 i)))
	    (value (aref new-kv-vector (1+ (* 2 i)))))
	(cond ((and (eq key +empty-ht-slot+)
		    (eq value +empty-ht-slot+))
	       ;; Slot is empty, push it onto the free list.
	       (setf (aref new-next-vector i)
		     (hash-table-next-free-kv table))
	       (setf (hash-table-next-free-kv table) i))
	      ((and new-hash-vector
		    (not (= (aref new-hash-vector i) +magic-hash-vector-value+)))
	       ;; Can use the existing hash value (not EQ based)
	       (let* ((hashing (aref new-hash-vector i))
		      (index (rem hashing new-length))
		      (next (aref new-index-vector index)))
		 (declare (type index index)
			  (type hash hashing))
		 ;; Push this slot into the next chain.
		 (setf (aref new-next-vector i) next)
		 (setf (aref new-index-vector index) i)))
	      (t
	       ;; EQ base hash.
	       ;; Enable GC tricks.
	       (set-header-data new-kv-vector
				sb!vm:vector-valid-hashing-subtype)
	       (let* ((hashing (pointer-hash key))
		      (index (rem hashing new-length))
		      (next (aref new-index-vector index)))
		 (declare (type index index)
			  (type hash hashing))
		 ;; Push this slot onto the next chain.
		 (setf (aref new-next-vector i) next)
		 (setf (aref new-index-vector index) i))))))
    (setf (hash-table-table table) new-kv-vector)
    (setf (hash-table-index-vector table) new-index-vector)
    (setf (hash-table-next-vector table) new-next-vector)
    (setf (hash-table-hash-vector table) new-hash-vector)
    ;; Shrink the old vectors to 0 size to help the conservative GC.
    (shrink-vector old-kv-vector 0)
    (shrink-vector old-index-vector 0)
    (shrink-vector old-next-vector 0)
    (when old-hash-vector
      (shrink-vector old-hash-vector 0))
    (setf (hash-table-rehash-trigger table) new-size))
  (values))

;;; Use the same size as before, re-using the vectors.
(defun rehash-without-growing (table)
  (declare (type hash-table table))
  (let* ((kv-vector (hash-table-table table))
	 (next-vector (hash-table-next-vector table))
	 (hash-vector (hash-table-hash-vector table))
	 (size (length next-vector))
	 (index-vector (hash-table-index-vector table))
	 (length (length index-vector)))
    (declare (type index size length))

    ;; Disable GC tricks, they will be re-enabled during the re-hash
    ;; if necesary.
    (set-header-data kv-vector sb!vm:vector-normal-subtype)

    ;; Rehash all the entries.
    (setf (hash-table-next-free-kv table) 0)
    (setf (hash-table-needing-rehash table) 0)
    (dotimes (i size)
      (setf (aref next-vector i) 0))
    (dotimes (i length)
      (setf (aref index-vector i) 0))
    (do ((i (1- size) (1- i)))
	((zerop i))
      (let ((key (aref kv-vector (* 2 i)))
	    (value (aref kv-vector (1+ (* 2 i)))))
	(cond ((and (eq key +empty-ht-slot+)
		    (eq value +empty-ht-slot+))
	       ;; Slot is empty, push it onto free list.
	       (setf (aref next-vector i) (hash-table-next-free-kv table))
	       (setf (hash-table-next-free-kv table) i))
	      ((and hash-vector (not (= (aref hash-vector i) +magic-hash-vector-value+)))
	       ;; Can use the existing hash value (not EQ based)
	       (let* ((hashing (aref hash-vector i))
		      (index (rem hashing length))
		      (next (aref index-vector index)))
		 (declare (type index index))
		 ;; Push this slot into the next chain.
		 (setf (aref next-vector i) next)
		 (setf (aref index-vector index) i)))
	      (t
	       ;; EQ base hash.
	       ;; Enable GC tricks.
	       (set-header-data kv-vector sb!vm:vector-valid-hashing-subtype)
	       (let* ((hashing (pointer-hash key))
		      (index (rem hashing length))
		      (next (aref index-vector index)))
		 (declare (type index index)
			  (type hash hashing))
		 ;; Push this slot into the next chain.
		 (setf (aref next-vector i) next)
		 (setf (aref index-vector index) i)))))))
  (values))

(defun flush-needing-rehash (table)
  (let* ((kv-vector (hash-table-table table))
	 (index-vector (hash-table-index-vector table))
	 (next-vector (hash-table-next-vector table))
	 (length (length index-vector)))
    (do ((next (hash-table-needing-rehash table)))
	((zerop next))
      (declare (type index next))
      (let* ((key (aref kv-vector (* 2 next)))
	     (hashing (pointer-hash key))
	     (index (rem hashing length))
	     (temp (aref next-vector next)))
	(setf (aref next-vector next) (aref index-vector index))
	(setf (aref index-vector index) next)
	(setf next temp))))
  (setf (hash-table-needing-rehash table) 0)
  (values))

(defun gethash (key hash-table &optional default)
  #!+sb-doc
  "Finds the entry in HASH-TABLE whose key is KEY and returns the associated
   value and T as multiple values, or returns DEFAULT and NIL if there is no
   such entry. Entries can be added using SETF."
  (declare (type hash-table hash-table)
	   (values t (member t nil)))
  (without-gcing
   (cond ((= (get-header-data (hash-table-table hash-table))
	     sb!vm:vector-must-rehash-subtype)
	  (rehash-without-growing hash-table))
	 ((not (zerop (hash-table-needing-rehash hash-table)))
	  (flush-needing-rehash hash-table)))
   ;; Search for key in the hash table.
   (multiple-value-bind (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (declare (type hash hashing))
     (let* ((index-vector (hash-table-index-vector hash-table))
	    (length (length index-vector))
	    (index (rem hashing length))
	    (next (aref index-vector index))
	    (table (hash-table-table hash-table))
	    (next-vector (hash-table-next-vector hash-table))
	    (hash-vector (hash-table-hash-vector hash-table))
	    (test-fun (hash-table-test-fun hash-table)))
       (declare (type index index))
       ;; Search next-vector chain for a matching key.
       (if (or eq-based (not hash-vector))
	   (do ((next next (aref next-vector next)))
	       ((zerop next) (values default nil))
	     (declare (type index next))
	     (when (eq key (aref table (* 2 next)))
	       (return (values (aref table (1+ (* 2 next))) t))))
	   (do ((next next (aref next-vector next)))
	       ((zerop next) (values default nil))
	     (declare (type index next))
	     (when (and (= hashing (aref hash-vector next))
			(funcall test-fun key (aref table (* 2 next))))
	       ;; Found.
	       (return (values (aref table (1+ (* 2 next))) t)))))))))

;;; so people can call #'(SETF GETHASH)
(defun (setf gethash) (new-value key table &optional default)
  (declare (ignore default))
  (%puthash key table new-value))

(defun %puthash (key hash-table value)
  (declare (type hash-table hash-table))
  (aver (hash-table-index-vector hash-table))
  (without-gcing
   ;; We need to rehash here so that a current key can be found if it
   ;; exists. Check that there is room for one more entry. May not be
   ;; needed if the key is already present.
   (cond ((zerop (hash-table-next-free-kv hash-table))
	  (rehash hash-table))
	 ((= (get-header-data (hash-table-table hash-table))
	     sb!vm:vector-must-rehash-subtype)
	  (rehash-without-growing hash-table))
	 ((not (zerop (hash-table-needing-rehash hash-table)))
	  (flush-needing-rehash hash-table)))

   ;; Search for key in the hash table.
   (multiple-value-bind (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (declare (type hash hashing))
     (let* ((index-vector (hash-table-index-vector hash-table))
	    (length (length index-vector))
	    (index (rem hashing length))
	    (next (aref index-vector index))
	    (kv-vector (hash-table-table hash-table))
	    (next-vector (hash-table-next-vector hash-table))
	    (hash-vector (hash-table-hash-vector hash-table))
	    (test-fun (hash-table-test-fun hash-table)))
       (declare (type index index))

       (cond ((or eq-based (not hash-vector))
	      (when eq-based
		(set-header-data kv-vector sb!vm:vector-valid-hashing-subtype))

	      ;; Search next-vector chain for a matching key.
	      (do ((next next (aref next-vector next)))
		  ((zerop next))
		(declare (type index next))
		(when (eq key (aref kv-vector (* 2 next)))
		  ;; Found, just replace the value.
		  (setf (aref kv-vector (1+ (* 2 next))) value)
		  (return-from %puthash value))))
	     (t
	      ;; Search next-vector chain for a matching key.
	      (do ((next next (aref next-vector next)))
		  ((zerop next))
		(declare (type index next))
		(when (and (= hashing (aref hash-vector next))
			   (funcall test-fun key
				    (aref kv-vector (* 2 next))))
		  ;; Found, just replace the value.
		  (setf (aref kv-vector (1+ (* 2 next))) value)
		  (return-from %puthash value)))))

       ;; Pop a KV slot off the free list
       (let ((free-kv-slot (hash-table-next-free-kv hash-table)))
	 ;; Double-check for overflow.
	 (aver (not (zerop free-kv-slot)))
	 (setf (hash-table-next-free-kv hash-table)
	       (aref next-vector free-kv-slot))
	 (incf (hash-table-number-entries hash-table))

	 (setf (aref kv-vector (* 2 free-kv-slot)) key)
	 (setf (aref kv-vector (1+ (* 2 free-kv-slot))) value)

	 ;; Setup the hash-vector if necessary.
	 (when hash-vector
	   (if (not eq-based)
	       (setf (aref hash-vector free-kv-slot) hashing)
	       (aver (= (aref hash-vector free-kv-slot) +magic-hash-vector-value+))))

	 ;; Push this slot into the next chain.
	 (setf (aref next-vector free-kv-slot) next)
	 (setf (aref index-vector index) free-kv-slot)))))
  value)

(defun remhash (key hash-table)
  #!+sb-doc
  "Remove the entry in HASH-TABLE associated with KEY. Return T if there
   was such an entry, or NIL if not."
  (declare (type hash-table hash-table)
	   (values (member t nil)))
  (without-gcing
   ;; We need to rehash here so that a current key can be found if it
   ;; exists.
   (cond ((= (get-header-data (hash-table-table hash-table))
	     sb!vm:vector-must-rehash-subtype)
	  (rehash-without-growing hash-table))
	 ((not (zerop (hash-table-needing-rehash hash-table)))
	  (flush-needing-rehash hash-table)))

   ;; Search for key in the hash table.
   (multiple-value-bind (hashing eq-based)
       (funcall (hash-table-hash-fun hash-table) key)
     (declare (type hash hashing))
     (let* ((index-vector (hash-table-index-vector hash-table))
	    (length (length index-vector))
	    (index (rem hashing length))
	    (next (aref index-vector index))
	    (table (hash-table-table hash-table))
	    (next-vector (hash-table-next-vector hash-table))
	    (hash-vector (hash-table-hash-vector hash-table))
	    (test-fun (hash-table-test-fun hash-table)))
       (declare (type index index next))
       (flet ((clear-slot (chain-vector prior-slot-location slot-location)
                ;; Mark slot as empty.
                (setf (aref table (* 2 slot-location)) +empty-ht-slot+
                      (aref table (1+ (* 2 slot-location))) +empty-ht-slot+)
                ;; Update the prior pointer in the chain to skip this.
                (setf (aref chain-vector prior-slot-location)
                      (aref next-vector slot-location))
                ;; Push KV slot onto free chain.
                (setf (aref next-vector slot-location)
                      (hash-table-next-free-kv hash-table))
                (setf (hash-table-next-free-kv hash-table) slot-location)
                (when hash-vector
                  (setf (aref hash-vector slot-location) +magic-hash-vector-value+))
                (decf (hash-table-number-entries hash-table))
                t))
         (cond ((zerop next)
                nil)
               ((if (or eq-based (not hash-vector))
                    (eq key (aref table (* 2 next)))
                    (and (= hashing (aref hash-vector next))
                         (funcall test-fun key (aref table (* 2 next)))))
                (clear-slot index-vector index next))
               ;; Search next-vector chain for a matching key.
               ((or eq-based (not hash-vector))
                ;; EQ based
                (do ((prior next next)
                     (next (aref next-vector next) (aref next-vector next)))
                    ((zerop next) nil)
                  (declare (type index next))
                  (when (eq key (aref table (* 2 next)))
                    (return-from remhash (clear-slot next-vector prior next)))))
               (t
                ;; not EQ based
                (do ((prior next next)
                     (next (aref next-vector next) (aref next-vector next)))
                    ((zerop next) nil)
                  (declare (type index next))
                  (when (and (= hashing (aref hash-vector next))
                             (funcall test-fun key (aref table (* 2 next))))
                    (return-from remhash (clear-slot next-vector prior next)))))))))))

(defun clrhash (hash-table)
  #!+sb-doc
  "This removes all the entries from HASH-TABLE and returns the hash table
   itself."
  (declare (optimize speed))
  (let* ((kv-vector (hash-table-table hash-table))
	 (next-vector (hash-table-next-vector hash-table))
	 (hash-vector (hash-table-hash-vector hash-table))
	 (size (length next-vector))
	 (index-vector (hash-table-index-vector hash-table)))
    ;; Disable GC tricks.
    (set-header-data kv-vector sb!vm:vector-normal-subtype)
    ;; Mark all slots as empty by setting all keys and values to magic
    ;; tag.
    (aver (eq (aref kv-vector 0) hash-table))
    (fill kv-vector +empty-ht-slot+ :start 2)
    ;; Set up the free list, all free.
    (do ((i 1 (1+ i)))
	((>= i (1- size)))
      (setf (aref next-vector i) (1+ i)))
    (setf (aref next-vector (1- size)) 0)
    (setf (hash-table-next-free-kv hash-table) 1)
    (setf (hash-table-needing-rehash hash-table) 0)
    ;; Clear the index-vector.
    (fill index-vector 0)
    ;; Clear the hash-vector.
    (when hash-vector
      (fill hash-vector +magic-hash-vector-value+)))
  (setf (hash-table-number-entries hash-table) 0)
  hash-table)

;;;; MAPHASH

;;; FIXME: This should be made into a compiler transform for two reasons:
;;;   1. It would then be available for compiling the entire system,
;;;      not only parts of the system which are defined after DEFUN MAPHASH.
;;;   2. It could be conditional on compilation policy, so that
;;;      it could be compiled as a full call instead of an inline
;;;      expansion when SPACE>SPEED.
(declaim (inline maphash))
(defun maphash (function-designator hash-table)
  #!+sb-doc
  "For each entry in HASH-TABLE, call the designated two-argument function
   on the key and value of the entry. Return NIL."
  (let ((fun (%coerce-callable-to-fun function-designator))
	(size (length (hash-table-next-vector hash-table))))
    (declare (type function fun))
    (do ((i 1 (1+ i)))
	((>= i size))
      (declare (type index i))
      (let* ((kv-vector (hash-table-table hash-table))
	     (key (aref kv-vector (* 2 i)))
	     (value (aref kv-vector (1+ (* 2 i)))))
	(unless (and (eq key +empty-ht-slot+)
		     (eq value +empty-ht-slot+))
	  (funcall fun key value))))))

;;;; methods on HASH-TABLE

;;; Return a list of keyword args and values to use for MAKE-HASH-TABLE
;;; when reconstructing HASH-TABLE.
(defun %hash-table-ctor-args (hash-table)
  (when (hash-table-weak-p hash-table)
    ;; FIXME: This might actually work with no trouble, but as of
    ;; sbcl-0.6.12.10 when this code was written, weak hash tables
    ;; weren't working yet, so I couldn't test it. When weak hash
    ;; tables are supported again, this should be fixed.
    (error "can't dump weak hash tables readably")) ; defensive programming..
  `(:test             ',(hash-table-test             hash-table)
    :size             ',(hash-table-size             hash-table)
    :rehash-size      ',(hash-table-rehash-size      hash-table)
    :rehash-threshold ',(hash-table-rehash-threshold hash-table)))

;;; Return an association list representing the same data as HASH-TABLE.
(defun %hash-table-alist (hash-table)
  (let ((result nil))
    (maphash (lambda (key value)
	       (push (cons key value) result))
	     hash-table)
    result))

;;; Stuff an association list into HASH-TABLE. Return the hash table,
;;; so that we can use this for the *PRINT-READABLY* case in
;;; PRINT-OBJECT (HASH-TABLE T) without having to worry about LET
;;; forms and readable gensyms and stuff.
(defun %stuff-hash-table (hash-table alist)
  (dolist (x alist)
    (setf (gethash (car x) hash-table) (cdr x)))
  hash-table)

(def!method print-object ((hash-table hash-table) stream)
  (declare (type stream stream))
  (cond ((not *print-readably*)
	 (print-unreadable-object (hash-table stream :type t :identity t)
	   (format stream
		   ":TEST ~S :COUNT ~S"
		   (hash-table-test hash-table)
		   (hash-table-count hash-table))))
	((not *read-eval*)
	 (error "can't print hash tables readably without *READ-EVAL*"))
	(t
	 (with-standard-io-syntax
	  (format stream
		  "#.~W"
		  `(%stuff-hash-table (make-hash-table ,@(%hash-table-ctor-args
							  hash-table))
				     ',(%hash-table-alist hash-table)))))))

(def!method make-load-form ((hash-table hash-table) &optional environment)
  (declare (ignore environment))
  (values `(make-hash-table ,@(%hash-table-ctor-args hash-table))
	  `(%stuff-hash-table ,hash-table ',(%hash-table-alist hash-table))))
