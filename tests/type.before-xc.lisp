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

(assert (type= (specifier-type 'list)
	       (type-union (specifier-type 'cons) (specifier-type 'null))))
(assert (type= (specifier-type 'list)
	       (type-union (specifier-type 'null) (specifier-type 'cons))))
(assert (type= (specifier-type 'sequence)
	       (type-union (specifier-type 'list) (specifier-type 'vector))))
(assert (type= (specifier-type 'sequence)
	       (type-union (specifier-type 'vector) (specifier-type 'list))))
(assert (type= (specifier-type 'list)
	       (type-union (specifier-type 'cons) (specifier-type 'list))))
(assert (not (csubtypep (type-union (specifier-type 'list)
				    (specifier-type '(satisfies foo)))
			(specifier-type 'list))))
(assert (csubtypep (specifier-type 'list)
		   (type-union (specifier-type 'list)
			       (specifier-type '(satisfies foo)))))

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
      
    (assert (eql *universal-type* (type-union ctype *universal-type*)))
    (assert (eql *universal-type* (type-union *universal-type* ctype)))
    (assert (eql *universal-type* (type-union2 ctype *universal-type*)))
    (assert (eql *universal-type* (type-union2 *universal-type* ctype)))

    (assert (type= ctype (type-union ctype *empty-type*)))
    (assert (type= ctype (type-union *empty-type* ctype)))
    (assert (type= ctype (type-union2 ctype *empty-type*)))
    (assert (type= ctype (type-union2 *empty-type* ctype)))

    (assert (csubtypep *empty-type* ctype))
    (assert (csubtypep ctype *universal-type*))))
(/show "finished with identities-should-be-identities block")

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

;;; tests of 2-value quantifieroids FOO/TYPE
(macrolet ((2= (v1 v2 expr2)
             (let ((x1 (gensym))
		   (x2 (gensym)))
	       `(multiple-value-bind (,x1 ,x2) ,expr2
		  (unless (and (eql ,x1 ,v1) (eql ,x2 ,v2))
		    (error "mismatch for EXPR2=~S" ',expr2))))))
  (flet (;; SUBTYPEP running in the cross-compiler
	 (xsubtypep (x y)
	   (csubtypep (specifier-type x)
		      (specifier-type y))))
    (2=   t   t (any/type   #'xsubtypep 'fixnum '(real integer)))
    (2=   t   t (any/type   #'xsubtypep 'fixnum '(real cons)))
    (2= nil   t (any/type   #'xsubtypep 'fixnum '(cons vector)))
    (2= nil nil (any/type   #'xsubtypep 'fixnum '(cons some-unknown-type-foo)))
    (2= nil nil (any/type   #'xsubtypep 'fixnum '(some-unknown-type-foo cons)))
    (2=   t   t (any/type   #'xsubtypep 'fixnum '(some-unknown-type-foo real)))
    (2=   t   t (any/type   #'xsubtypep 'fixnum '(real some-unknown-type-foo)))
    (2= nil   t (any/type   #'xsubtypep 'fixnum '()))
    (2=   t   t (every/type #'xsubtypep 'fixnum '()))
    (2= nil nil (every/type #'xsubtypep 'fixnum '(real some-unknown-type-foo)))
    (2= nil nil (every/type #'xsubtypep 'fixnum '(some-unknown-type-foo real)))
    (2= nil   t (every/type #'xsubtypep 'fixnum '(some-unknown-type-foo cons)))
    (2= nil   t (every/type #'xsubtypep 'fixnum '(cons some-unknown-type-foo)))
    (2=   t   t (every/type #'xsubtypep 'fixnum '(real integer)))
    (2= nil   t (every/type #'xsubtypep 'fixnum '(real cons)))
    (2= nil   t (every/type #'xsubtypep 'fixnum '(cons vector)))))

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
;; FIXME: As of sbcl-0.6.11.17, the system doesn't know how to do the
;; type simplifications which would let these tests work. (bug 88)
#|
(let* ((type1 (specifier-type '(member :x86)))
       (type2 (specifier-type '(or keyword null)))
       (isect (type-intersection type1 type2)))
  (assert (type= isect (type-intersection type2 type1)))
  (assert (type= isect type1))
  (assert (type= isect (type-intersection type2 type1 type2)))
  (assert (type= isect (type-intersection type1 type1 type2 type1)))
  (assert (type= isect (type-intersection type1 type2 type1 type2))))
|#

(/show "done with tests/type.before-xc.lisp")
