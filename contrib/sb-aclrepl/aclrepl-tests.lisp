;; Tests for sb-aclrepl 

(defpackage #:aclrepl-tests (:use #:sb-aclrepl #:cl))
(in-package #:aclrepl-tests)

(import '(sb-aclrepl::inspected-parts sb-aclrepl::inspected-description
	  sb-aclrepl::inspected-elements sb-aclrepl::parts-count
	  sb-aclrepl::parts-seq-type sb-aclrepl::find-object-part-with-id
	  sb-aclrepl::element-at sb-aclrepl::label-at
	  sb-aclrepl::display-inspected-parts
	  sb-aclrepl::display-labelled-element
	  sb-aclrepl::*inspect-unbound-object-marker*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'regression-test)
    (load (sb-aclrepl::compile-file-as-needed "rt.lisp"))))
(use-package :regression-test)
(setf regression-test::*catch-errors* nil)

(rem-all-tests)

(deftest hook.1 (boundp 'sb-impl::*inspect-fun*) t)
(deftest hook.2 (boundp 'sb-int:*repl-prompt-fun*) t)
(deftest hook.3 (boundp 'sb-int:*repl-read-form-fun*) t)
;(deftest (boundp 'sb-debug::*invoke-debugger-fun*) t)

;;; Inspector tests

(defclass empty-class ()
  ())
(defparameter *empty-class* (make-instance 'empty-class))

(defclass empty-class ()
  ())

(defclass simple-class ()
  ((a)
   (second :initform 0)
   (really-long-slot-name :initform "abc")))

(defstruct empty-struct
  )

(defstruct simple-struct
  (first)
  (slot-2 'a-value)
  (really-long-struct-slot-name "defg"))

(defparameter *empty-class* (make-instance 'empty-class))
(defparameter *simple-class* (make-instance 'simple-class))
(defparameter *empty-struct* (make-empty-struct))
(defparameter *simple-struct* (make-simple-struct))
(defparameter *normal-list* '(a b 3))
(defparameter *dotted-list* '(a b . 3))
(defparameter *cons-pair* '(#c(1 2) . a-symbol))
(defparameter *complex* #c(1 2))
(defparameter *ratio* 22/7)
(defparameter *array* (make-array '(3 3 2) :initial-element nil))
(defparameter *vector* (make-array '(20):initial-contents
			     '(0 1 2 3 4 5 6 7 8 9
			       10 11 12 13 14 15 16 17 18 19)))

(defun find-position (object id)
    (nth-value 0 (find-object-part-with-id object id)))
(defun parts (object)
    (inspected-parts object))
(defun description (object)
  (inspected-description object))
(defun elements (object &optional print skip)
  (nth-value 0 (inspected-elements object print skip)))
(defun elements-labels (object &optional print skip)
  (nth-value 1 (inspected-elements object print skip)))
(defun elements-count (object &optional print skip)
  (nth-value 2 (inspected-elements object print skip)))

(defun labelled-element (object pos &optional print skip)
  (with-output-to-string (strm)
    (display-labelled-element (aref (elements object print skip) pos)
			      (aref (elements-labels object print skip) pos)
			      strm)))

(deftest find.list.0 (find-position *normal-list* 0) 0)
(deftest find.list.1 (find-position *normal-list* 0) 0)
(deftest find.list.2 (find-position *normal-list* 1) 1)
(deftest find.list.3 (find-position *normal-list* 2) 2)
(deftest parts.list.1 (parts-count (parts *normal-list*)) 3)
(deftest parts.list.2 (element-at (parts *normal-list*) 0) a)
(deftest parts.list.3 (element-at (parts *normal-list*) 1) b)
(deftest parts.list.4 (element-at (parts *normal-list*) 2) 3)
(deftest parts.list.5 (label-at (parts *normal-list*) 0) 0)
(deftest parts.list.6 (label-at (parts *normal-list*) 1) 1)
(deftest parts.list.7 (label-at (parts *normal-list*) 2) 2)
(deftest parts.list.8 (parts-seq-type (parts *normal-list*)) :list)

(deftest elem.list.0 (elements-count *normal-list*) 3)
(deftest elem.list.1 (elements *normal-list*) #(a b 3))
(deftest elem.list.2 (elements-labels *normal-list*) #(0 1 2))

(deftest elem.dotted.0 (elements-count *dotted-list*) 3)
(deftest elem.dotted.1 (elements *dotted-list*) #(a b 3))
(deftest elem.dotted.2 (elements-labels *dotted-list*) #(0 1 :tail))

(deftest elem.consp.0 (elements-count *cons-pair*) 2)
(deftest elem.consp.1 (elements *cons-pair*) #(#c(1 2) a-symbol))
(deftest elem.consp.2 (elements-labels *cons-pair*)
  #((0 . "car") (1 . "cdr")))

(deftest elem.complex.0 (elements-count *complex*) 2)
(deftest elem.complex.1 (elements *complex*) #(1 2))
(deftest elem.complex.2 (elements-labels *complex*)
  #((0 . "real") (1 . "imag")))

(deftest elem.ratio.0 (elements-count *ratio*) 2)
(deftest elem.ratio.1 (elements *ratio*) #(22 7))
(deftest elem.ratio.2 (elements-labels *ratio*)
  #((0 . "numerator") (1 . "denominator")))

(deftest elem.vector.0 (elements-count *vector*) 20)
(deftest elem.vector.1 (elements *vector*)
  #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
(deftest elem.vector.2 (elements-labels *vector*)
  #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))

(deftest elem.vector.skip1.0 (elements-count *vector* nil 3) 18)
(deftest elem.vector.skip1.1 (elements *vector* nil 3) 
  #(nil 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
(deftest elem.vector.skip1.2 (elements-labels *vector* nil 3)
  #(:ellipses 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))

(deftest elem.vector.skip2.0 (elements-count *vector* 10 3) 13)
(deftest elem.vector.skip2.1 (elements *vector* 10 3) 
  #(nil 3 4 5 6 7 8 9 10 11 12 nil 19))
(deftest elem.vector.skip2.2 (elements-labels *vector* 10 3)
  #(:ellipses 3 4 5 6 7 8 9 10 11 12 :ellipses 19))

(deftest elem.vector.skip3.0 (elements-count *vector* 5 16) 5)
(deftest elem.vector.skip3.1 (elements *vector* 5 16) 
  #(nil 16 17 18 19))
(deftest elem.vector.skip3.2 (elements-labels *vector* 5 16)
  #(:ellipses 16 17 18 19))

(deftest elem.vector.skip4.0 (elements-count *vector* 2 16) 5)
(deftest elem.vector.skip4.1 (elements *vector* 2 16) 
  #(nil 16 17 18 19))
(deftest elem.vector.skip4.2 (elements-labels *vector* 2 16)
  #(:ellipses 16 17 18 19))

(deftest elem.vector.skip5.0 (elements-count *vector* 2 15) 5)
(deftest elem.vector.skip5.1 (elements *vector* 2 15) 
  #(nil 15 16 nil 19))
(deftest elem.vector.skip5.2 (elements-labels *vector* 2 15)
  #(:ellipses 15 16 :ellipses 19))

(deftest elem.array.0 (elements-count *array*) 18)
(deftest elem.array.1 (elements *array*)
   #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
                NIL NIL))
(deftest elem.array.2 (elements-labels *array*)
  #((0 . "[0,0,0]") (1 . "[0,0,1]") (2 . "[0,1,0]") (3 . "[0,1,1]")
    (4 . "[0,2,0]") (5 . "[0,2,1]") (6 . "[1,0,0]") (7 . "[1,0,1]")
    (8 . "[1,1,0]") (9 . "[1,1,1]") (10 . "[1,2,0]")
    (11 . "[1,2,1]") (12 . "[2,0,0]") (13 . "[2,0,1]")
    (14 . "[2,1,0]") (15 . "[2,1,1]") (16 . "[2,2,0]")
    (17 . "[2,2,1]")))


(deftest empty.class.0 (elements-count *empty-class*) 0)
(deftest empty.class.1 (elements *empty-class*) nil)
(deftest empty.class.2 (elements-labels *empty-class*) nil)

(deftest simple.class.0 (elements-count *simple-class*) 3)
(deftest simple.class.1 (elements *simple-class*)
  #(#.*inspect-unbound-object-marker* 0 "abc"))
(deftest simple.class.2 (elements-labels *simple-class*)
  #((0 . A) (1 . SECOND) (2 . REALLY-LONG-SLOT-NAME)))

(deftest empty.struct.0 (elements-count *empty-struct*) 0)
(deftest empty.struct.1 (elements *empty-struct*) nil)
(deftest empty.struct.2 (elements-labels *empty-struct*) nil)

(deftest simple.struct.0 (elements-count *simple-struct*) 3)
(deftest simple.struct.1 (elements *simple-struct*)
  #(nil a-value "defg"))
(deftest simple.struct.2 (elements-labels *simple-struct*)
  #((0 . "FIRST") (1 . "SLOT-2")
    (2 . "REALLY-LONG-STRUCT-SLOT-NAME")))

(deftest display.simple-struct.0
    (labelled-element *simple-struct* 0)
  "   0 FIRST ----------> the symbol NIL")
(deftest display.simple-struct.1
    (labelled-element *simple-struct* 1)
  "   1 SLOT-2 ---------> the symbol A-VALUE")
(deftest display.simple-struct.2
    (labelled-element *simple-struct* 2)
  "   2 REALLY-LONG-STRUCT-SLOT-NAME -> a simple-string (4) \"defg\"")

(deftest display.simple-class.0
    (labelled-element *simple-class* 0)
  "   0 A --------------> ..unbound..")
(deftest display.simple-class.1
    (labelled-element *simple-class* 1)
  "   1 SECOND ---------> fixnum 0")
(deftest display.simple-class.2
    (labelled-element *simple-class* 2)
  "   2 REALLY-LONG-SLOT-NAME -> a simple-string (3) \"abc\"")

(deftest display.complex.0
    (labelled-element *complex* 0)
  "   0 real -----------> fixnum 1")
(deftest display.complex.1
    (labelled-element *complex* 1)
  "   1 imag -----------> fixnum 2")

(deftest display.ratio.0
    (labelled-element *ratio* 0)
  "   0 numerator ------> fixnum 22")
(deftest display.ratio.1
    (labelled-element *ratio* 1)
  "   1 denominator ----> fixnum 7")

(deftest display.dotted-list.0
    (labelled-element *dotted-list* 0)
  "   0-> the symbol A")
(deftest display.dotted-list.1
    (labelled-element *dotted-list* 1)
  "   1-> the symbol B")
(deftest display.dotted-list.2
    (labelled-element *dotted-list* 2)
  "tail-> fixnum 3")

(deftest display.normal-list.0
    (labelled-element *normal-list* 0)
  "   0-> the symbol A")
(deftest display.normal-list.1
    (labelled-element *normal-list* 1)
  "   1-> the symbol B")
(deftest display.normal-list.2
    (labelled-element *normal-list* 2)
  "   2-> fixnum 3")


(deftest display.vector.0
    (labelled-element *vector* 0)
  "   0-> fixnum 0")
(deftest display.vector.1
    (labelled-element *vector* 1)
  "   1-> fixnum 1")

(deftest display.vector.skip1.0
    (labelled-element *vector* 0 nil 2)
  "   ...")
(deftest display.vector.skip1.1
    (labelled-element *vector* 1 nil 2)
  "   2-> fixnum 2")

(deftest display.consp.0
    (labelled-element *cons-pair* 0)
  "   0 car ------------> complex number #C(1 2)")
(deftest display.consp.1
    (labelled-element *cons-pair* 1)
  "   1 cdr ------------> the symbol A-SYMBOL")

(do-tests)

(when (pending-tests)
  (error "Some tests failed."))

