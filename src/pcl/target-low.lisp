;;;; This file contains portable versions of low-level functions and macros
;;;; which are ripe for implementation specific customization. None of the code
;;;; in this file *has* to be customized for a particular Common Lisp
;;;; implementation. Moreover, in some implementations it may not make any
;;;; sense to customize some of this code.
;;;;
;;;; The original version was intended to support portable customization to
;;;; lotso different Lisp implementations. This functionality is gone in the
;;;; current version, and it now runs only under SBCL. (Now that ANSI Common
;;;; Lisp has mixed CLOS into the insides of the system (e.g. error handling
;;;; and printing) so deeply that it's not very meaningful to bootstrap Common
;;;; Lisp without CLOS, the old functionality is of dubious use. -- WHN
;;;; 19981108)
;;;; 
;;;; To make this work properly in SBCL's build, this file contains
;;;; only those functions and structure definitions that are not
;;;; portable to an arbitrary ANSI Common Lisp host.

;;;; This software is part of the SBCL system. See the README file for more
;;;; information.

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

(in-package "SB!PCL")

;;;; PCL's view of funcallable instances

(sb!kernel:!defstruct-with-alternate-metaclass pcl-funcallable-instance
  ;; KLUDGE: Note that neither of these slots is ever accessed by its
  ;; accessor name as of sbcl-0.pre7.63. Presumably everything works
  ;; by puns based on absolute locations. Fun fun fun.. -- WHN 2001-10-30
  :slot-names (clos-slots name hash-code)
  :boa-constructor %make-pcl-funcallable-instance
  :superclass-name sb!kernel:funcallable-instance
  :metaclass-name sb!kernel:random-pcl-class
  :metaclass-constructor sb!kernel:make-random-pcl-class
  :dd-type sb!kernel:funcallable-structure
  ;; Only internal implementation code will access these, and these
  ;; accesses (slot readers in particular) could easily be a
  ;; bottleneck, so it seems reasonable to suppress runtime type
  ;; checks.
  ;;
  ;; (Except note KLUDGE above that these accessors aren't used at all
  ;; (!) as of sbcl-0.pre7.63, so for now it's academic.)
  :runtime-type-checks-p nil)

(defun set-funcallable-instance-fun (fin new-value)
  (declare (type function new-value))
  (aver (sb!kernel:funcallable-instance-p fin))
  (setf (sb!kernel:funcallable-instance-fun fin) new-value))

;;; CMU CL comment:
;;;   We define this as STANDARD-INSTANCE, since we're going to
;;;   clobber the layout with some standard-instance layout as soon as
;;;   we make it, and we want the accessor to still be type-correct.
#|
(defstruct (standard-instance
	    (:predicate nil)
	    (:constructor %%allocate-instance--class ())
	    (:copier nil)
	    (:alternate-metaclass sb-kernel:instance
				  cl:standard-class
				  sb-kernel:make-standard-class))
  (slots nil))
|#
(sb!kernel:!defstruct-with-alternate-metaclass standard-instance
  :slot-names (slots hash-code)
  :boa-constructor %make-standard-instance
  :superclass-name sb!kernel:instance
  :metaclass-name cl:standard-class
  :metaclass-constructor sb!kernel:make-standard-class
  :dd-type structure
  :runtime-type-checks-p nil)

