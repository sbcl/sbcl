;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; Has the type system been properly initialized? (I.e. is it OK to
;;; use it?)
(defvar *type-system-initialized* #+sb-xc-host nil) ; (set in cold load)

(defvar *wild-type*)
(defvar *empty-type*)
(defvar *universal-type*)
(defvar *universal-fun-type*)
(defvar *instance-type*)
(defvar *funcallable-instance-type*)
(defvar *extended-sequence-type*)

;;; a vector that maps type codes to layouts, used for quickly finding
;;; the layouts of built-in classes
(defvar *built-in-class-codes*) ; initialized in cold load
(defvar *null-classoid-layout*)
(declaim (type simple-vector *built-in-class-codes*))
