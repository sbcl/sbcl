;;;; This file defines MAKE-INSTANCE optimization mechanisms.
;;;;
;;;; KLUDGE: I removed the old DEFCONSTRUCTOR, MAKE-CONSTRUCTOR, and
;;;; LOAD-CONSTRUCTOR families of definitions in sbcl-0.pre7.99, since
;;;; it was clear from a few minutes with egrep that they were dead
;;;; code, but I suspect more dead code remains in this file. (Maybe
;;;; it's all dead?) -- WHN 2001-12-26

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;; The actual constructor objects.
(defclass constructor (funcallable-standard-object)
     ((class					;The class with which this
	:initarg :class				;constructor is associated.
	:reader constructor-class)		;The actual class object,
						;not the class name.

      (name					;The name of this constructor.
	:initform nil				;This is the symbol in whose
	:initarg :name				;function cell the constructor
	:reader constructor-name)		;usually sits. Of course, this
						;is optional. The old
					        ;DEFCONSTRUCTOR macro made
						;named constructors, but
						;it is possible to manipulate
						;anonymous constructors also.

      (supplied-initarg-names			;The names of the initargs this
	:initarg :supplied-initarg-names	;constructor supplies when it
	:reader					;"calls" make-instance.
	   constructor-supplied-initarg-names)	;

      (code-generators				;Generators for the different
	:initarg :code-generators		;types of code this constructor
	:reader constructor-code-generators))	;could use.
  (:metaclass funcallable-standard-class))

(defmethod describe-object ((constructor constructor) stream)
  (format stream
	  "~S is a constructor for the class ~S.~%"
	  constructor (constructor-class constructor)))

;;;; Here is the actual smarts for making the code generators and then
;;;; trying each generator to get constructor code. This extensible
;;;; mechanism allows new kinds of constructor code types to be added.
;;;; A programmer defining a specialization of the constructor class
;;;; can use this mechanism to define new code types.
;;;;
;;;; original PCL comment from before dead COMPUTE-CONSTRUCTOR-CODE
;;;; was deleted:
;;;;    When compute-constructor-code is called, it first performs
;;;;    basic checks to make sure that the basic assumptions common to
;;;;    all the code types are valid. (For details see method
;;;;    definition). If any of the tests fail, the fallback
;;;;    constructor code type is used. If none of the tests fail, the
;;;;    constructor code generators are called in order. They receive
;;;;    5 arguments:
;;;;
;;;;   CLASS	the class the constructor is making instances of
;;;;   WRAPPER      that class's wrapper
;;;;   DEFAULTS     the result of calling class-default-initargs on class
;;;;   INITIALIZE   the applicable methods on initialize-instance
;;;;   SHARED       the applicable methosd on shared-initialize
;;;;
;;;; The first code generator to return code is used. The code
;;;; generators are called in reverse order of definition, so forms
;;;; which define better code should appear after ones that define
;;;; less good code. The fallback code type appears first. Note that
;;;; redefining a code type does not change its position in the list.
;;;; To do that, define a new type at the end with the behavior.

;;;; helper functions and utilities that are shared by all of the code
;;;; types

(defvar *standard-initialize-instance-method*
	(get-method #'initialize-instance
		    ()
		    (list *the-class-slot-object*)))

(defvar *standard-shared-initialize-method*
	(get-method #'shared-initialize
		    ()
		    (list *the-class-slot-object* *the-class-t*)))

(defun non-pcl-initialize-instance-methods-p (methods)
  (notevery #'(lambda (m) (eq m *standard-initialize-instance-method*))
	    methods))

(defun non-pcl-shared-initialize-methods-p (methods)
  (notevery #'(lambda (m) (eq m *standard-shared-initialize-method*))
	    methods))

(defun non-pcl-or-after-initialize-instance-methods-p (methods)
  (notevery #'(lambda (m) (or (eq m *standard-initialize-instance-method*)
			      (equal '(:after) (method-qualifiers m))))
	    methods))

(defun non-pcl-or-after-shared-initialize-methods-p (methods)
  (notevery #'(lambda (m) (or (eq m *standard-shared-initialize-method*)
			      (equal '(:after) (method-qualifiers m))))
	    methods))
