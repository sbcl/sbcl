;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;; 
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "CL-USER")

(locally
  (declare (notinline mapcar))
  (mapcar (lambda (args)
	    (destructuring-bind (obj type-spec result) args
	      (flet ((matches-result? (x)
		       (eq (if x t nil) result)))
		(assert (matches-result? (typep obj type-spec)))
		(assert (matches-result? (sb-kernel:ctypep
					  obj
					  (sb-kernel:specifier-type
					   type-spec)))))))
	  '((nil (or null vector)              t)
	    (nil (or number vector)            nil)
	    (12  (or null vector)              nil)
	    (12  (and (or number vector) real) t))))


;;; This test is motivated by bug #195, which previously had (THE REAL
;;; #(1 2 3)) give an error which prints as "This is not a (OR
;;; SINGLE-FLOAT DOUBLE-FLOAT RATIONAL)".  We ideally want all of the
;;; defined-by-ANSI types to unparse as themselves or at least
;;; something similar (e.g. CHARACTER can unparse to BASE-CHAR, since
;;; the types are equivalent in current SBCL).
(let ((standard-types '(;; from table 4-2 in section 4.2.3 in the
			;; CLHS.
			arithmetic-error
			function
			simple-condition           
			array
			generic-function
			simple-error
			;; (NOT CONS)
			;; atom
			hash-table
			simple-string              
			base-char
			integer
			simple-type-error          
			base-string
			keyword
			simple-vector              
			bignum
			list
			simple-warning             
			bit
			logical-pathname
			single-float               
			bit-vector
			long-float
			;; MEMBER-TYPE #\a #\b ...
			;; standard-char              
			broadcast-stream
			method
			standard-class             
			built-in-class
			method-combination
			standard-generic-function  
			cell-error
			nil
			standard-method            
			character
			null
			standard-object            
			class
			number
			storage-condition          
			compiled-function
			package
			stream                     
			complex
			package-error
			stream-error               
			concatenated-stream
			parse-error
			string                     
			condition
			pathname
			;; OR STRING-INPUT-STREAM STRING-OUTPUT-STREAM
			;; FILL-POINTER-OUTPUT-STREAM
			;; string-stream
			cons
			print-not-readable
			structure-class            
			control-error
			program-error
			structure-object           
			division-by-zero
			random-state
			style-warning              
			double-float
			ratio
			symbol                     
			echo-stream
			rational
			synonym-stream             
			end-of-file
			reader-error
			t                          
			error
			readtable
			two-way-stream
			;; This one's hard: (AND BASE-CHAR (NOT BASE-CHAR))
			;;
			;; This is because it looks like
			;;   (AND CHARACTER (NOT BASE-CHAR))
			;; but CHARACTER is equivalent to
			;; BASE-CHAR. So if we fix intersection of
			;; obviously disjoint types and then do (the
			;; extended-char foo), we'll get back FOO is
			;; not a NIL. -- CSR, 2002-09-16.
			;; extended-char
			real
			type-error                 
			file-error
			restart
			unbound-slot               
			file-stream
			;; (OR CONS NULL VECTOR)
			;; sequence
			unbound-variable           
			fixnum
			serious-condition
			undefined-function         
			float
			short-float
			unsigned-byte              
			floating-point-inexact
			signed-byte
			vector                     
			floating-point-invalid-operation
			simple-array
			warning                    
			floating-point-overflow
			simple-base-string                             
			floating-point-underflow
			simple-bit-vector)))
  (dolist (type standard-types)
    (format t "~&~S~%" type)
    (assert (not (sb-kernel:unknown-type-p (sb-kernel:specifier-type type))))
    (assert (atom (sb-kernel:type-specifier (sb-kernel:specifier-type type))))))
