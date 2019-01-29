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

(declaim (type (member nil early braid complete) **boot-state**))
(define-load-time-global **boot-state** nil)


;;; The PCL package is internal and is used by code in potential
;;; bottlenecks. And since it's internal, no one should be
;;; doing things like deleting and recreating it in a running target Lisp.
(define-symbol-macro *pcl-package* #.(find-package "SB-PCL"))

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
   (intern (possibly-base-stringize
            (apply #'format nil format-string format-arguments))
           package)))

(defun condition-type-p (type)
  (and (symbolp type)
       (condition-classoid-p (find-classoid type nil))))

;;;; PCL instances

(sb-kernel::!defstruct-with-alternate-metaclass standard-instance
  ;; KLUDGE: arm64 needs to have CAS-HEADER-DATA-HIGH implemented
  :slot-names (slots #-(and compact-instance-header x86-64) hash-code)
  :constructor %make-standard-instance
  :superclass-name t
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type structure)

;;; Note: for x8-64 with #+immobile-code there are 2 additional raw slots which
;;; hold machine instructions to load the funcallable-instance-fun and jump to
;;; it, so that funcallable-instances can act like simple-funs, in as much as
;;; there's an address you can jump to without loading a register.
(sb-kernel::!defstruct-with-alternate-metaclass standard-funcallable-instance
  :slot-names (clos-slots #-compact-instance-header hash-code)
  :constructor %make-standard-funcallable-instance
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)
