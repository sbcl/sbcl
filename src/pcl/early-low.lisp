;;;; some code pulled out of CMU CL's low.lisp to solve build order problems,
;;;; and some other stuff that just plain needs to be done early

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

(/show "starting early-low.lisp")

;;; FIXME: The PCL package is internal and is used by code in potential
;;; bottlenecks. Access to it might be faster through #.(find-package "SB-PCL")
;;; than through *PCL-PACKAGE*. And since it's internal, no one should be
;;; doing things like deleting and recreating it in a running target Lisp.
;;; So perhaps we should replace it uses of *PCL-PACKAGE* with uses of
;;; (PCL-PACKAGE), and make PCL-PACKAGE a macro which expands into
;;; the SB-PCL package itself. Maybe we should even use this trick for
;;; COMMON-LISP and KEYWORD, too. (And the definition of PCL-PACKAGE etc.
;;; could be made less viciously brittle when SB-FLUID.)
;;; (Or perhaps just define a macro
;;;   (DEFMACRO PKG (NAME)
;;;     #-SB-FLUID (FIND-PACKAGE NAME)
;;;     #+SB-FLUID `(FIND-PACKAGE ,NAME))
;;; and use that to replace all three variables.)
(defvar *pcl-package*		     (find-package "SB-PCL"))
(defvar *slot-accessor-name-package* (find-package "SB-SLOT-ACCESSOR-NAME"))

;;; This excludes structure types created with the :TYPE option to
;;; DEFSTRUCT. It also doesn't try to deal with types created by
;;; hairy DEFTYPEs, e.g.
;;;   (DEFTYPE CACHE-STRUCTURE (SIZE)
;;;     (IF (> SIZE 11) 'BIG-CS 'SMALL-CS)).
;;; KLUDGE: In fact, it doesn't seem to deal with DEFTYPEs at all. Perhaps
;;; it needs a more mnemonic name. -- WHN 19991204
(defun structure-type-p (type)
  (and (symbolp type)
       (let ((class  (cl:find-class type nil)))
	 (and class
	      (typep (sb-kernel:layout-info (sb-kernel:class-layout class))
		     'sb-kernel:defstruct-description)))))

(/show "finished with early-low.lisp")
