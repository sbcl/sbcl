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

(in-package "SB!PCL")

(declaim (type (member nil early braid complete) **boot-state**))
(defglobal **boot-state** nil)

(/show0 "starting early-low.lisp")

;;; The PCL package is internal and is used by code in potential
;;; bottlenecks. And since it's internal, no one should be
;;; doing things like deleting and recreating it in a running target Lisp.
;;; By the time we get to compiling the rest of PCL,
;;; the package will have been renamed,
;;; so subsequently compiled code should refer to "SB-PCL", not "SB!PCL".
(define-symbol-macro *pcl-package* (load-time-value (find-package "SB-PCL") t))

(declaim (inline class-classoid))
(defun class-classoid (class)
  (layout-classoid (class-wrapper class)))

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
  ;; Reference a package that is now SB!PCL but later SB-PCL
  (format-symbol (load-time-value (find-package "SB!PCL") t)
                 "*THE-CLASS-~A*" (symbol-name class-name)))

(defun make-wrapper-symbol (class-name)
  ;; Reference a package that is now SB!PCL but later SB-PCL
  (format-symbol (load-time-value (find-package "SB!PCL") t)
                 "*THE-WRAPPER-~A*" (symbol-name class-name)))

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
                  *the-class-system-class*
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
                  *the-class-standard-direct-slot-definition*
                  *the-class-standard-effective-slot-definition*
                  *the-class-standard-specializer*

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

;;;; PCL instances

(sb!kernel::!defstruct-with-alternate-metaclass standard-instance
  :slot-names (slots hash-code)
  :boa-constructor %make-standard-instance
  :superclass-name t
  :metaclass-name standard-classoid
  :metaclass-constructor make-standard-classoid
  :dd-type structure
  :runtime-type-checks-p nil)

(sb!kernel::!defstruct-with-alternate-metaclass standard-funcallable-instance
  ;; KLUDGE: Note that neither of these slots is ever accessed by its
  ;; accessor name as of sbcl-0.pre7.63. Presumably everything works
  ;; by puns based on absolute locations. Fun fun fun.. -- WHN 2001-10-30
  :slot-names (clos-slots hash-code)
  :boa-constructor %make-standard-funcallable-instance
  :superclass-name function
  :metaclass-name standard-classoid
  :metaclass-constructor make-standard-classoid
  :dd-type funcallable-structure
  ;; Only internal implementation code will access these, and these
  ;; accesses (slot readers in particular) could easily be a
  ;; bottleneck, so it seems reasonable to suppress runtime type
  ;; checks.
  ;;
  ;; (Except note KLUDGE above that these accessors aren't used at all
  ;; (!) as of sbcl-0.pre7.63, so for now it's academic.)
  :runtime-type-checks-p nil)

(defconstant std-instance-hash-slot-index 2)
(defconstant fsc-instance-hash-slot-index 2)

(/show0 "finished with early-low.lisp")
