;;;; definitions of types for the target (output of the compiler)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; Now that DEFTYPE is set up, any pending requests for it can
;;;; be honored.

#+sb-xc-host
(progn
  (/show "about to force delayed DEF!TYPEs")
  (force-delayed-def!types)
  (/show "done forcing delayed DEF!TYPEs"))

;;;; standard types

(sb!xc:deftype boolean () '(member t nil))

(sb!xc:deftype mod (n)
  (unless (and (integerp n) (> n 0))
    (error "bad modulus specified for MOD type specifier: ~S" n))
  `(integer 0 ,(1- n)))

(sb!xc:deftype signed-byte (&optional s)
  (cond ((eq s '*) 'integer)
	((and (integerp s) (> s 1))
	 (let ((bound (ash 1 (1- s))))
	   `(integer ,(- bound) ,(1- bound))))
	(t
	 (error "bad size specified for SIGNED-BYTE type specifier: ~S" s))))

(sb!xc:deftype unsigned-byte (&optional s)
  (cond ((eq s '*) '(integer 0))
	((and (integerp s) (> s 0))
	 `(integer 0 ,(1- (ash 1 s))))
	(t
	 (error "bad size specified for UNSIGNED-BYTE type specifier: ~S" s))))

(sb!xc:deftype bit () '(integer 0 1))

(sb!xc:deftype compiled-function () 'function)

(sb!xc:deftype atom () '(not cons))

(sb!xc:deftype extended-char ()
  #!+sb-doc
  "Type of characters that aren't base-char's. None in CMU CL."
  '(and character (not base-char)))

(sb!xc:deftype standard-char ()
  #!+sb-doc
  "Type corresponding to the characters required by the standard."
  '(member
    #\NEWLINE #\SPACE #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\,
    #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\=
    #\> #\?  #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\]
    #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{
    #\| #\} #\~))

;;; FIXME: Would type inference be able to get more traction on this
;;; if it were defined as (AND SYMBOL (SATISFIES KEYWORDP))?
(sb!xc:deftype keyword ()
  #!+sb-doc
  "Type for any keyword symbol."
  '(satisfies keywordp))

(sb!xc:deftype eql (n) `(member ,n))

(sb!xc:deftype vector (&optional element-type size)
  `(array ,element-type (,size)))

(sb!xc:deftype simple-vector (&optional size)
  `(simple-array t (,size)))

(sb!xc:deftype base-string (&optional size)
  `(array base-char (,size)))
(sb!xc:deftype simple-base-string (&optional size)
  `(simple-array base-char (,size)))
(sb!xc:deftype string (&optional size)
  `(or (array character (,size))
	    (base-string ,size)))
(sb!xc:deftype simple-string (&optional size)
  `(or (simple-array character (,size))
	    (simple-base-string ,size)))

(sb!xc:deftype bit-vector (&optional size)
  `(array bit (,size)))

(sb!xc:deftype simple-bit-vector (&optional size)
  `(simple-array bit (,size)))

;;;; some private types that we use in defining the standard functions

;;; a type specifier
(sb!xc:deftype type-specifier () '(or list symbol sb!xc:class))

;;; array rank, total size...
(sb!xc:deftype array-rank () `(integer 0 (,sb!xc:array-rank-limit)))
(sb!xc:deftype array-total-size ()
  `(integer 0 (,sb!xc:array-total-size-limit)))

;;; something legal in an evaluated context
;;; FIXME: could probably go away
(sb!xc:deftype form () 't)

;;; Maclisp compatibility...
;;; FIXME: should be STRING-DESIGNATOR (the term used in the ANSI spec)
(sb!xc:deftype stringable () '(or string symbol character))

;;; a thing legal in places where we want the name of a file
(sb!xc:deftype filename () '(or string pathname))

;;; legal args to pathname functions
(sb!xc:deftype pathname-designator ()
  '(or string pathname stream))
(sb!xc:deftype logical-host-designator ()
  '(or host string))

;;; a thing returned by the irrational functions. We assume that they
;;; never compute a rational result.
(sb!xc:deftype irrational ()
  '(or float (complex float)))

;;; character components
(sb!xc:deftype char-code () `(integer 0 (,char-code-limit)))

;;; a consed sequence result. If a vector, is a simple array.
(sb!xc:deftype consed-sequence () '(or list (simple-array * (*))))

;;; the :END arg to a sequence
(sb!xc:deftype sequence-end () '(or null index))

;;; a valid argument to a stream function
;;;
;;; FIXME: should probably be STREAM-DESIGNATOR, after the term
;;; used in the ANSI spec (if this is in fact exactly the same thing)
(sb!xc:deftype streamlike () '(or stream (member nil t)))

;;; a thing that can be passed to FUNCALL & friends
;;;
;;; FIXME: should be FUNCTION-DESIGNATOR?
(sb!xc:deftype callable () '(or function symbol))

;;; decomposing floats into integers
(sb!xc:deftype single-float-exponent ()
  `(integer ,(- sb!vm:single-float-normal-exponent-min
		sb!vm:single-float-bias
		sb!vm:single-float-digits)
	    ,(- sb!vm:single-float-normal-exponent-max
		sb!vm:single-float-bias)))
(sb!xc:deftype double-float-exponent ()
  `(integer ,(- sb!vm:double-float-normal-exponent-min
		sb!vm:double-float-bias
		sb!vm:double-float-digits)
	    ,(- sb!vm:double-float-normal-exponent-max
		sb!vm:double-float-bias)))
(sb!xc:deftype single-float-int-exponent ()
  `(integer ,(- sb!vm:single-float-normal-exponent-min
		sb!vm:single-float-bias
		(* sb!vm:single-float-digits 2))
	    ,(- sb!vm:single-float-normal-exponent-max
		sb!vm:single-float-bias
		sb!vm:single-float-digits)))
(sb!xc:deftype double-float-int-exponent ()
  `(integer ,(- sb!vm:double-float-normal-exponent-min sb!vm:double-float-bias
		(* sb!vm:double-float-digits 2))
	    ,(- sb!vm:double-float-normal-exponent-max sb!vm:double-float-bias
		sb!vm:double-float-digits)))
(sb!xc:deftype single-float-significand ()
  `(integer 0 (,(ash 1 sb!vm:single-float-digits))))
(sb!xc:deftype double-float-significand ()
  `(integer 0 (,(ash 1 sb!vm:double-float-digits))))
