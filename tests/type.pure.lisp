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
;;; the types are equivalent in current SBCL, and EXTENDED-CHAR can
;;; unparse to NIL, since there are no EXTENDED-CHARs currently).
(let ((standard-types '(;; from table 4-2 in section 4.2.3 in the
			;; CLHS.
			arithmetic-error
			function
			simple-condition           
			array
			generic-function
			simple-error
			;; so it might seem easy to change the HAIRY
			;; :UNPARSE method to recognize that (NOT
			;; CONS) should unparse as ATOM. However, we
			;; then lose the nice (SUBTYPEP '(NOT ATOM)
			;; 'CONS) => T,T behaviour that we get from
			;; simplifying (NOT ATOM) -> (NOT (NOT CONS))
			;; -> CONS. So, for now, we leave this
			;; commented out.
			;;
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
			standard-char              
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
			string-stream
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
			extended-char
			real
			type-error                 
			file-error
			restart
			unbound-slot               
			file-stream
			sequence
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

;;; a bug underlying the reported bug #221: The SB-KERNEL type code
;;; signalled an error on this expression.
(subtypep '(function (fixnum) (values package boolean))
	  '(function (t) (values package boolean)))

;;; bug reported by Valtteri Vuorik
(compile nil '(lambda () (member (char "foo" 0) '(#\. #\/) :test #'char=)))
(assert (not (equal (multiple-value-list
                     (subtypep '(function ()) '(function (&rest t))))
                    '(nil t))))

(assert (not (equal (multiple-value-list
                     (subtypep '(function (&rest t)) '(function ())))
                    '(t t))))

(assert (subtypep '(function)
                  '(function (&optional * &rest t))))
(assert (equal (multiple-value-list
                (subtypep '(function)
                          '(function (t &rest t))))
               '(nil t)))
#+nil
(assert (and (subtypep 'function '(function))
             (subtypep '(function) 'function)))

;;; Absent any exciting generalizations of |R, the type RATIONAL is
;;; partitioned by RATIO and INTEGER.  Ensure that the type system
;;; knows about this.  [ the type system is permitted to return NIL,
;;; NIL for these, so if future maintenance breaks these tests that
;;; way, that's fine.  What the SUBTYPEP calls are _not_ allowed to
;;; return is NIL, T, because that's completely wrong. ]
(assert (subtypep '(or integer ratio) 'rational))
(assert (subtypep 'rational '(or integer ratio)))
;;; Likewise, these are allowed to return NIL, NIL, but shouldn't
;;; return NIL, T:
(assert (subtypep t '(or real (not real))))
(assert (subtypep t '(or keyword (not keyword))))
(assert (subtypep '(and cons (not (cons symbol integer)))
		  '(or (cons (not symbol) *) (cons * (not integer)))))
(assert (subtypep '(or (cons (not symbol) *) (cons * (not integer)))
		  '(and cons (not (cons symbol integer)))))
(assert (subtypep '(or (eql 0) (rational (0) 10))
		  '(rational 0 10)))
(assert (subtypep '(rational 0 10)
		  '(or (eql 0) (rational (0) 10))))
