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
(defvar *pcl-package*                (find-package "SB-PCL"))

(declaim (inline defstruct-classoid-p))
(defun defstruct-classoid-p (classoid)
  ;; It is non-obvious to me why STRUCTURE-CLASSOID-P doesn't
  ;; work instead of this. -- NS 2008-03-14
  (typep (layout-info (classoid-layout classoid)) 'defstruct-description))

;;; This excludes structure types created with the :TYPE option to
;;; DEFSTRUCT. It also doesn't try to deal with types created by
;;; hairy DEFTYPEs, e.g.
;;;   (DEFTYPE CACHE-STRUCTURE (SIZE)
;;;     (IF (> SIZE 11) 'BIG-CS 'SMALL-CS)).
;;; KLUDGE: In fact, it doesn't seem to deal with DEFTYPEs at all. Perhaps
;;; it needs a more mnemonic name. -- WHN 19991204
(defun structure-type-p (type)
  (and (symbolp type)
       (let ((classoid (find-classoid type nil)))
         (and classoid
              (not (condition-classoid-p classoid))
              (defstruct-classoid-p classoid)))))

;;; Symbol contruction utilities
(defun format-symbol (package format-string &rest format-arguments)
  (without-package-locks
   (intern (apply #'format nil format-string format-arguments) package)))

(defun make-class-symbol (class-name)
  (format-symbol *pcl-package* "*THE-CLASS-~A*" (symbol-name class-name)))

(defun make-wrapper-symbol (class-name)
  (format-symbol *pcl-package* "*THE-WRAPPER-~A*" (symbol-name class-name)))

(defun condition-type-p (type)
  (and (symbolp type)
       (condition-classoid-p (find-classoid type nil))))

(declaim (special *the-class-t*
                  *the-class-vector* *the-class-symbol*
                  *the-class-string* *the-class-sequence*
                  *the-class-rational* *the-class-ratio*
                  *the-class-number* *the-class-null* *the-class-list*
                  *the-class-integer* *the-class-float* *the-class-cons*
                  *the-class-complex* *the-class-character*
                  *the-class-bit-vector* *the-class-array*
                  *the-class-stream* *the-class-file-stream*
                  *the-class-string-stream*

                  *the-class-slot-object*
                  *the-class-structure-object*
                  *the-class-standard-object*
                  *the-class-funcallable-standard-object*
                  *the-class-class*
                  *the-class-generic-function*
                  *the-class-built-in-class*
                  *the-class-slot-class*
                  *the-class-condition-class*
                  *the-class-structure-class*
                  *the-class-std-class*
                  *the-class-standard-class*
                  *the-class-funcallable-standard-class*
                  *the-class-forward-referenced-class*
                  *the-class-method*
                  *the-class-standard-method*
                  *the-class-standard-reader-method*
                  *the-class-standard-writer-method*
                  *the-class-standard-boundp-method*
                  *the-class-global-reader-method*
                  *the-class-global-writer-method*
                  *the-class-global-boundp-method*
                  *the-class-standard-generic-function*
                  *the-class-standard-effective-slot-definition*

                  *the-eslotd-standard-class-slots*
                  *the-eslotd-funcallable-standard-class-slots*))

(declaim (special *the-wrapper-of-t*
                  *the-wrapper-of-vector* *the-wrapper-of-symbol*
                  *the-wrapper-of-string* *the-wrapper-of-sequence*
                  *the-wrapper-of-rational* *the-wrapper-of-ratio*
                  *the-wrapper-of-number* *the-wrapper-of-null*
                  *the-wrapper-of-list* *the-wrapper-of-integer*
                  *the-wrapper-of-float* *the-wrapper-of-cons*
                  *the-wrapper-of-complex* *the-wrapper-of-character*
                  *the-wrapper-of-bit-vector* *the-wrapper-of-array*))

(/show "finished with early-low.lisp")
