;; Tests for sb-aclrepl

(defpackage #:aclrepl-tests
  (:use #:sb-aclrepl #:cl #:sb-rt))
(in-package #:aclrepl-tests)

(declaim (special sb-aclrepl::*skip-address-display*
                  sb-aclrepl::*inspect-unbound-object-marker*))

(setf sb-rt::*catch-errors* nil)

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

(defstruct tiny-struct
  (first 10))

(defstruct simple-struct
  (first)
  (slot-2 'a-value)
  (really-long-struct-slot-name "defg"))

(defparameter *empty-class* (make-instance 'empty-class))
(defparameter *simple-class* (make-instance 'simple-class))
(defparameter *empty-struct* (make-empty-struct))
(defparameter *tiny-struct* (make-tiny-struct))
(defparameter *simple-struct* (make-simple-struct))
(defparameter *normal-list* '(a b 3))
(defparameter *dotted-list* '(a b . 3))
(defparameter *cons-pair* '(#c(1 2) . a-symbol))
(defparameter *complex* #c(1 2))
(defparameter *ratio* 22/7)
(defparameter *double* 5.5d0)
(defparameter *bignum* 4938271560493827156)
(defparameter *array* (make-array '(3 3 2) :initial-element nil))
(defparameter *vector* (make-array '(20):initial-contents
                             '(0 1 2 3 4 5 6 7 8 9
                               10 11 12 13 14 15 16 17 18 19)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *circle-list1* '(a))
  (setf (car *circle-list1*) *circle-list1*)
  (defparameter *circle-list2* '(b))
  (setf (cdr *circle-list2*) *circle-list2*)
  (defparameter *circle-list3* '(a b c))
  (setf (car *circle-list3*) *circle-list3*)
  (defparameter *circle-list4* '(a b c))
  (setf (second *circle-list4*) *circle-list4*)
  (defparameter *circle-list5* '(a b c))
  (setf (cddr *circle-list5*) *circle-list5*))

(defun find-position (object id)
    (nth-value 0 (sb-aclrepl::find-part-id object id)))
(defun parts (object)
  (let ((sb-aclrepl::*skip-address-display* t))
    (sb-aclrepl::inspected-parts object)))
(defun description (object)
  (let ((sb-aclrepl::*skip-address-display* t))
    (sb-aclrepl::inspected-description object)))
(defun elements (object &optional print (skip 0))
  (let ((sb-aclrepl::*skip-address-display* t))
    (sb-aclrepl::inspected-elements object print skip)))
(defun elements-components (object &optional print (skip 0))
    (nth-value 0 (elements object print skip )))
(defun elements-labels (object &optional print (skip 0))
    (nth-value 1 (elements object print skip)))
(defun elements-count (object &optional print (skip 0))
  (nth-value 2 (elements object print skip)))

(defun labeled-element (object pos &optional print (skip 0))
  (with-output-to-string (strm)
    (let ((sb-aclrepl::*skip-address-display* t))
      (sb-aclrepl::display-labeled-element
       (aref (the simple-vector (elements-components object print skip)) pos)
       (aref (the simple-vector (elements-labels object print skip)) pos)
       strm))))

(defun display (object &optional print (skip 0))
  (with-output-to-string (strm)
    (let ((sb-aclrepl::*skip-address-display* t))
      (sb-aclrepl::display-inspect object strm print skip))))

(defun istep (args)
  (with-output-to-string (strm)
    (let ((sb-aclrepl::*skip-address-display* t))
      (sb-aclrepl::istep args strm))))

(deftest find.list.0 (find-position *normal-list* 0) 0)
(deftest find.list.1 (find-position *normal-list* 0) 0)
(deftest find.list.2 (find-position *normal-list* 1) 1)
(deftest find.list.3 (find-position *normal-list* 2) 2)
(deftest parts.list.1 (sb-aclrepl::parts-count (parts *normal-list*)) 3)
(deftest parts.list.2 (sb-aclrepl::component-at (parts *normal-list*) 0) a)
(deftest parts.list.3 (sb-aclrepl::component-at (parts *normal-list*) 1) b)
(deftest parts.list.4 (sb-aclrepl::component-at (parts *normal-list*) 2) 3)
(deftest parts.list.5 (sb-aclrepl::label-at (parts *normal-list*) 0) 0)
(deftest parts.list.6 (sb-aclrepl::label-at (parts *normal-list*) 1) 1)
(deftest parts.list.7 (sb-aclrepl::label-at (parts *normal-list*) 2) 2)
(deftest parts.list.8 (sb-aclrepl::parts-seq-type (parts *normal-list*)) :list)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun basename (id &optional print (skip 0))
    (let ((name (typecase id
                  (symbol (symbol-name id))
                  (string (string-upcase id))
                  (t (format nil "~A" id)))))
      (format nil "~A~A~A"
              (string-left-trim "*" (string-right-trim "*" name))
              (if print (format nil ".P~D" print) "")
              (if (not (zerop skip)) (format nil ".S~D" skip) ""))))

  (defun elements-tests-name (id ext print skip)
    (intern (format nil "ELEM.~A.~A" (basename id print skip) ext))))

(defmacro def-elements-tests (object count components labels
                          &optional (print nil) (skip 0))
  `(progn
    (deftest ,(elements-tests-name object "COUNT" print skip)
        (elements-count ,object ,print ,skip) ,count)
    (unless (eq ,components :dont-check)
      (deftest ,(elements-tests-name object "COMPONENTS" print skip)
          (elements-components ,object ,print ,skip) ,components))
    (deftest ,(elements-tests-name object "LABELS" print skip)
        (elements-labels ,object ,print ,skip) ,labels)))

(def-elements-tests *normal-list* 3 #(a b 3) #(0 1 2))
(def-elements-tests *dotted-list* 3 #(a b 3) #(0 1 :tail))

(def-elements-tests *circle-list1* 2 :dont-check #((0 . "car") (1 . "cdr")))
(def-elements-tests *circle-list2* 2 :dont-check #(0 :tail))
(def-elements-tests *circle-list3* 3 :dont-check #(0 1 2))
(def-elements-tests *circle-list4* 3 :dont-check #(0 1 2))
(def-elements-tests *circle-list5* 3 :dont-check #(0 1 :tail))

(deftest circle-list1-components
    (aref (elements-components *circle-list1*) 0) #.*circle-list1*)
(deftest circle-list2-components.0
    (aref (elements-components *circle-list2*) 0) b)
(deftest circle-list2-components.1
    (aref (elements-components *circle-list2*) 1) #.*circle-list2*)
(deftest circle-list3-components.0
    (aref (elements-components *circle-list3*) 0) #.*circle-list3*)
(deftest circle-list3-components.1
    (aref (elements-components *circle-list3*) 1) b)
(deftest circle-list3-components.2
    (aref (elements-components *circle-list3*) 2) c)
(deftest circle-list4-components.0
    (aref (elements-components *circle-list4*) 0) a)
(deftest circle-list4-components.1
    (aref (elements-components *circle-list4*) 1) #.*circle-list4*)
(deftest circle-list4-components.2
    (aref (elements-components *circle-list4*) 2) c)
(deftest circle-list5-components.0
    (aref (elements-components *circle-list5*) 0) a)
(deftest circle-list5-components.1
    (aref (elements-components *circle-list5*) 1) b)
(deftest circle-list5-components.2
    (aref (elements-components *circle-list5*) 2) #.*circle-list5*)

(def-elements-tests *cons-pair* 2 #(#c(1 2) a-symbol)
                #((0 . "car") (1 . "cdr")))
(def-elements-tests *complex* 2 #(1 2) #((0 . "real") (1 . "imag")))
(def-elements-tests *ratio* 2 #(22 7)
                #((0 . "numerator") (1 . "denominator")))
(case sb-vm:n-word-bits
  (32
   (def-elements-tests *bignum* 2
     #(4154852436 1149780945)
     #((0 . :HEX32) (1 . :HEX32))))
  (64
   (def-elements-tests *bignum* 1
     #(4938271560493827156)
     #((0 . :HEX64)))))

(def-elements-tests *vector* 20
                #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
                #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
(def-elements-tests *vector* 18
  #(nil 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
  #(:ellipses 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
  nil 3)
(def-elements-tests *vector* 13
  #(nil 3 4 5 6 7 8 9 10 11 12 nil 19)
  #(:ellipses 3 4 5 6 7 8 9 10 11 12 :ellipses 19)
  10 3)
(def-elements-tests *vector* 5
  #(nil 16 17 18 19)
  #(:ellipses 16 17 18 19)
  5 16)
(def-elements-tests *vector* 5
  #(nil 16 17 18 19)
  #(:ellipses 16 17 18 19)
  2 16)
(def-elements-tests *vector* 5
  #(nil 15 16 nil 19)
  #(:ellipses 15 16 :ellipses 19)
  2 15)
(def-elements-tests *array* 18
   #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
                NIL NIL)
  #((0 . "[0,0,0]") (1 . "[0,0,1]") (2 . "[0,1,0]") (3 . "[0,1,1]")
    (4 . "[0,2,0]") (5 . "[0,2,1]") (6 . "[1,0,0]") (7 . "[1,0,1]")
    (8 . "[1,1,0]") (9 . "[1,1,1]") (10 . "[1,2,0]")
    (11 . "[1,2,1]") (12 . "[2,0,0]") (13 . "[2,0,1]")
    (14 . "[2,1,0]") (15 . "[2,1,1]") (16 . "[2,2,0]")
    (17 . "[2,2,1]")))

(def-elements-tests *empty-class* 0 nil nil)
#+ignore ;; FIXME
(def-elements-tests *simple-class* 3
  #(#.sb-aclrepl::*inspect-unbound-object-marker* 0 "abc")
  #((0 . "A") (1 . "SECOND") (2 . "REALLY-LONG-SLOT-NAME")))
(def-elements-tests *empty-struct* 0 nil nil)
(def-elements-tests *simple-struct* 3
  #(nil a-value "defg")
  #((0 . "FIRST") (1 . "SLOT-2")
    (2 . "REALLY-LONG-STRUCT-SLOT-NAME")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun label-test-name (name pos &optional print (skip 0))
    (intern (format nil "LABEL.~A.~D" (basename name print skip) pos))))

(defmacro def-label-test (object pos label &optional print (skip 0))
  `(deftest ,(label-test-name object pos print skip)
    (labeled-element ,object ,pos ,print ,skip) ,label))

(def-label-test *simple-struct* 0
  "   0 FIRST ----------> the symbol NIL")
(def-label-test *simple-struct* 1
  "   1 SLOT-2 ---------> the symbol A-VALUE")
(def-label-test *simple-struct* 2
 "   2 REALLY-LONG-STRUCT-SLOT-NAME -> a simple-string (4) \"defg\"")
(def-label-test *simple-class* 0
  "   0 A --------------> ..unbound..")
(def-label-test *simple-class* 1
  "   1 SECOND ---------> fixnum 0")
(def-label-test *simple-class* 2
  "   2 REALLY-LONG-SLOT-NAME -> a simple-string (3) \"abc\"")

(def-label-test *complex* 0 "   0 real -----------> fixnum 1")
(def-label-test *complex* 1 "   1 imag -----------> fixnum 2")

(def-label-test *ratio* 0 "   0 numerator ------> fixnum 22")
(def-label-test *ratio* 1 "   1 denominator ----> fixnum 7")

(def-label-test *dotted-list* 0 "   0-> the symbol A")
(def-label-test *dotted-list* 1 "   1-> the symbol B")
(def-label-test *dotted-list* 2 "tail-> fixnum 3")

(def-label-test *normal-list* 0 "   0-> the symbol A")
(def-label-test *normal-list* 1 "   1-> the symbol B")
(def-label-test *normal-list* 2 "   2-> fixnum 3")

(def-label-test *vector* 0 "   0-> fixnum 0")
(def-label-test *vector* 1 "   1-> fixnum 1")
(def-label-test *vector* 0 "   ..." nil 2)
(def-label-test *vector* 1"   2-> fixnum 2" nil 2)

(def-label-test *cons-pair* 0
    "   0 car ------------> complex number #C(1 2)")
(def-label-test *cons-pair* 1
  "   1 cdr ------------> the symbol A-SYMBOL")

(deftest nil.parts.0 (elements-count nil) 5)

(def-elements-tests *tiny-struct* 1 #(10) #((0 . "FIRST")))
(def-elements-tests *tiny-struct* 1
                    #(nil) #(:ellipses) nil 1)
(def-elements-tests *tiny-struct* 1
                    #(nil) #(:ellipses) nil 2)

(def-elements-tests *double* 0 nil nil)
(def-elements-tests *double* 0 nil nil nil 1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun display-test-name (name print skip)
    (intern (format nil "DISPLAY.~A" (basename name print skip)))))

(defmacro def-display-test (object string &optional print (skip 0))
  `(deftest ,(display-test-name object print skip)
    (display ,object ,print ,skip) ,string))

(def-display-test *cons-pair*
  "a cons cell
   0 car ------------> complex number #C(1 2)
   1 cdr ------------> the symbol A-SYMBOL")

(def-display-test *simple-struct*
 "#<STRUCTURE-CLASS ACLREPL-TESTS::SIMPLE-STRUCT>
   0 FIRST ----------> the symbol NIL
   1 SLOT-2 ---------> the symbol A-VALUE
   2 REALLY-LONG-STRUCT-SLOT-NAME -> a simple-string (4) \"defg\"")

(def-display-test *simple-struct*
  "#<STRUCTURE-CLASS ACLREPL-TESTS::SIMPLE-STRUCT>
   ...
   2 REALLY-LONG-STRUCT-SLOT-NAME -> a simple-string (4) \"defg\""
  nil 2)

(case sb-vm:n-word-bits
  (32
   (def-display-test *bignum*
       "bignum 4938271560493827156 with 2 32-bit words
   0-> #xF7A60454
   1-> #x448843D1"))
  (64
   (def-display-test *bignum*
       "bignum 4938271560493827156 with 1 64-bit word
   0-> #x448843D1F7A60454"
     )))

(def-display-test *vector*
  "a simple T vector (20)
   ...
   6-> fixnum 6
   7-> fixnum 7
   8-> fixnum 8
   9-> fixnum 9
  10-> fixnum 10
   ...
  19-> fixnum 19"
  5 6)

(def-display-test *circle-list1*
"a cons cell
   0 car ------------> a cons cell
   1 cdr ------------> the symbol NIL")
(def-display-test *circle-list2*
"a cyclic list with 1 element+tail
   0-> the symbol B
tail-> a cyclic list with 1 element+tail")
(def-display-test *circle-list3*
"a normal list with 3 elements
   0-> a normal list with 3 elements
   1-> the symbol B
   2-> the symbol C")
(def-display-test *circle-list4*
"a normal list with 3 elements
   0-> the symbol A
   1-> a normal list with 3 elements
   2-> the symbol C")
(def-display-test *circle-list5*
  "a cyclic list with 2 elements+tail
   0-> the symbol A
   1-> the symbol B
tail-> a cyclic list with 2 elements+tail")


;;; Inspector traversal tests
(deftest inspect.0 (progn (setq * *simple-struct*)
                          (istep '("*")))
  "#<STRUCTURE-CLASS ACLREPL-TESTS::SIMPLE-STRUCT>
   0 FIRST ----------> the symbol NIL
   1 SLOT-2 ---------> the symbol A-VALUE
   2 REALLY-LONG-STRUCT-SLOT-NAME -> a simple-string (4) \"defg\"")

(deftest istep.0 (progn (setq * *simple-struct*)
                          (istep '("*"))
                          (istep '("=")))
  "#<STRUCTURE-CLASS ACLREPL-TESTS::SIMPLE-STRUCT>
   0 FIRST ----------> the symbol NIL
   1 SLOT-2 ---------> the symbol A-VALUE
   2 REALLY-LONG-STRUCT-SLOT-NAME -> a simple-string (4) \"defg\"")


(deftest istep.1 (progn (setq * *simple-struct*)
                        (istep '("*"))
                        (istep '("first")))
"the symbol NIL
   0 NAME -----------> a simple-string (3) \"NIL\"
   1 PACKAGE --------> the COMMON-LISP package
   2 VALUE ----------> the symbol NIL
   3 FUNCTION -------> ..unbound..
   4 PLIST ----------> the symbol NIL")


(deftest istep.2  (progn (setq * *simple-struct*)
                         (istep '("*"))
                         (istep '("first"))
                         (istep '(">")))
"the symbol A-VALUE
   0 NAME -----------> a simple-string (7) \"A-VALUE\"
   1 PACKAGE --------> the ACLREPL-TESTS package
   2 VALUE ----------> ..unbound..
   3 FUNCTION -------> ..unbound..
   4 PLIST ----------> the symbol NIL")

(deftest istep.3  (progn (setq * *simple-struct*)
                         (istep '("*"))
                         (istep '("first"))
                         (istep '(">"))
                         (istep '("<")))
"the symbol NIL
   0 NAME -----------> a simple-string (3) \"NIL\"
   1 PACKAGE --------> the COMMON-LISP package
   2 VALUE ----------> the symbol NIL
   3 FUNCTION -------> ..unbound..
   4 PLIST ----------> the symbol NIL")

(deftest istep.4  (progn (setq * *simple-struct*)
                         (istep '("*"))
                         (istep '("first"))
                         (istep '(">"))
                         (istep '("<"))
                         (istep '("tree")))
"The current object is:
the symbol NIL, which was selected by FIRST
#<STRUCTURE-CLASS ACLREPL-TESTS::SIMPLE-STRUCT>, which was selected by (inspect *)
")

(deftest istep.5  (progn (setq * *simple-struct*)
                         (istep '("*"))
                         (istep '("first"))
                         (istep '(">"))
                         (istep '("<"))
                         (istep '("-")))
  "#<STRUCTURE-CLASS ACLREPL-TESTS::SIMPLE-STRUCT>
   0 FIRST ----------> the symbol NIL
   1 SLOT-2 ---------> the symbol A-VALUE
   2 REALLY-LONG-STRUCT-SLOT-NAME -> a simple-string (4) \"defg\"")

(deftest istep.6 (progn (setq * *dotted-list*)
                        (istep '("*"))
                        (istep '("tail")))
"fixnum 3")

(deftest istep.7 (progn (setq * *dotted-list*)
                        (istep '("*"))
                        (istep '("2")))
"fixnum 3")

(deftest istep.8 (progn (setq * 5.5d0)
                        (istep '("*")))
  "double-float 5.5d0")

(deftest istep.9 (progn (setq * 5.5d0)
                        (istep '("-")))
  "Object has no parent
")
