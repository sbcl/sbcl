;;;; tests of the type system, intended to be executed as soon as 
;;;; the cross-compiler is built

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

(in-package "SB!KERNEL")

(/show "beginning tests/type.before-xc.lisp")

(assert (type= (specifier-type '(and fixnum (satisfies foo)))
	       (specifier-type '(and (satisfies foo) fixnum))))
(assert (type= (specifier-type '(member 1 2 3))
	       (specifier-type '(member 2 3 1))))
(assert (type= (specifier-type '(and (member 1.0 2 3) single-float))
	       (specifier-type '(member 1.0))))

(assert (sb-xc:typep #(1 2 3) 'simple-vector))
(assert (sb-xc:typep #(1 2 3) 'vector))
(assert (not (sb-xc:typep '(1 2 3) 'vector)))
(assert (not (sb-xc:typep 1 'vector)))

(assert (sb-xc:typep '(1 2 3) 'list))
(assert (sb-xc:typep '(1 2 3) 'cons))
(assert (not (sb-xc:typep '(1 2 3) 'null)))
(assert (not (sb-xc:typep "1 2 3" 'list)))
(assert (not (sb-xc:typep 1 'list)))

(assert (sb-xc:typep nil 'null))
(assert (sb-xc:typep nil '(member nil)))
(assert (sb-xc:typep nil '(member 1 2 nil 3)))
(assert (not (sb-xc:typep nil '(member 1 2 3))))

(assert (type= *empty-type*
	       (type-intersection (specifier-type 'list)
				  (specifier-type 'vector))))
(assert (eql *empty-type*
	     (type-intersection (specifier-type 'list)
				(specifier-type 'vector))))
(assert (type= (specifier-type 'null)
	       (type-intersection (specifier-type 'list)
				  (specifier-type '(or vector null)))))
(assert (type= (specifier-type 'null)
	       (type-intersection (specifier-type 'sequence)
				  (specifier-type 'symbol))))
(assert (type= (specifier-type 'cons)
	       (type-intersection (specifier-type 'sequence)
				  (specifier-type '(or cons number)))))
(assert (eql *empty-type*
	     (type-intersection (specifier-type '(satisfies keywordp))
				*empty-type*)))

;;; Identities should be identities.
(dolist (type-specifier '(nil
			  t
			  null
			  (satisfies keywordp) 
			  (satisfies foo) 
			  (not fixnum)
			  (not null)
			  (and symbol (satisfies foo))
			  (and (satisfies foo) string)
			  (or symbol sequence)
			  (or single-float character)
			  (or float (satisfies bar))
			  integer (integer 0 1)
			  character standard-char
			  (member 1 2 3)))
  (/show type-specifier)
  (let ((ctype (specifier-type type-specifier)))

    (assert (eql *empty-type* (type-intersection ctype *empty-type*)))
    (assert (eql *empty-type* (type-intersection *empty-type* ctype)))
    (assert (eql *empty-type* (type-intersection2 ctype *empty-type*)))
    (assert (eql *empty-type* (type-intersection2 *empty-type* ctype)))

    (assert (type= ctype (type-intersection ctype *universal-type*)))
    (assert (type= ctype (type-intersection *universal-type* ctype)))
    (assert (type= ctype (type-intersection2 ctype *universal-type*)))
    (assert (type= ctype (type-intersection2 *universal-type* ctype)))
      
    ;; FIXME: TYPE-UNION still acts CMU-CL-ish as of 0.6.11.13, so
    ;; e.g. (TYPE-UNION #<HAIRY-TYPE (SATISFIES KEYWORDP)> *EMPTY-TYPE*)
    ;; returns a UNION-TYPE instead of the HAIRY-TYPE. When that's
    ;; fixed, these tests should be enabled.
    ;;(assert (eql ctype (type-union ctype *empty-type*)))
    ;;(assert (eql ctype (type-union *empty-type* ctype)))

    ;; FIXME: TYPE-UNION2 is not defined yet as of 0.6.11.13, and when
    ;; it's defined, these tests should be enabled.
    ;;(assert (eql *empty-type* (type-union2 ctype *empty-type*)))
    ;;(assert (eql *empty-type* (type-union2 *empty-type* ctype)))

    ;;(assert (eql *universal-type* (type-union ctype *universal-type*)))
    ;;(assert (eql *universal-type* (type-union *universal-type* ctype)))
    ;;(assert (eql ctype (type-union2 ctype *universal-type*)))
    ;;(assert (eql ctype (type-union2 *universal-type* ctype)))

    (assert (csubtypep *empty-type* ctype))
    (assert (csubtypep ctype *universal-type*))))
(/show "done with identities-should-be-identities block")

(assert (sb-xc:subtypep 'simple-vector 'vector))
(assert (sb-xc:subtypep 'simple-vector 'simple-array))
(assert (sb-xc:subtypep 'vector 'array))
(assert (not (sb-xc:subtypep 'vector 'simple-vector)))
(assert (not (sb-xc:subtypep 'vector 'simple-array)))

(macrolet ((assert-secondnil (expr) `(assert (null (nth-value 1 ,expr)))))
  (assert-secondnil (sb-xc:subtypep t '(satisfies foo)))
  (assert-secondnil (sb-xc:subtypep t '(and (satisfies foo) (satisfies bar))))
  (assert-secondnil (sb-xc:subtypep t '(or (satisfies foo) (satisfies bar))))
  ;; FIXME: Enable these tests when bug 84 is fixed.
  #|
  (assert-secondnil (sb-xc:subtypep '(satisfies foo) nil))
  (assert-secondnil (sb-xc:subtypep '(and (satisfies foo) (satisfies bar))
				    nil))
  (assert-secondnil (sb-xc:subtypep '(or (satisfies foo) (satisfies bar))
				    nil))
  |#)

;;; various dead bugs
(assert (union-type-p (type-intersection (specifier-type 'list)
					 (specifier-type '(or list vector)))))
(assert (type= (type-intersection (specifier-type 'list)
				  (specifier-type '(or list vector)))
	       (specifier-type 'list)))
(assert (array-type-p (type-intersection (specifier-type 'vector)
					 (specifier-type '(or list vector)))))
(assert (type= (type-intersection (specifier-type 'vector)
				  (specifier-type '(or list vector)))
	       (specifier-type 'vector)))
(assert (type= (type-intersection (specifier-type 'number)
				  (specifier-type 'integer))
	       (specifier-type 'integer)))
(assert (null (type-intersection2 (specifier-type 'symbol)
				  (specifier-type '(satisfies foo)))))
(assert (intersection-type-p (specifier-type '(and symbol (satisfies foo)))))

(/show "done with tests/type.before-xc.lisp")
