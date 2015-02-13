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
(!defglobal *type-system-initialized* nil)

;; These are set by cold-init-forms in 'late-type' (look for "macrolet frob").
;; It is a requirement of the type machinery that there be
;; exactly one instance of each of these, which is to say,
;; any type named T is exactly EQ to *UNIVERSAL-TYPE*, etc.
(defglobal *wild-type* -1)
(defglobal *empty-type* -1)
(defglobal *universal-type* -1)
(defglobal *instance-type* -1)
(defglobal *funcallable-instance-type* -1)
(defglobal *extended-sequence-type* -1)

;; Unlike the above, this one is not a NAMED-TYPE, and as such
;; does not have to be a singleton, but it improves efficiency.
;; (Except that we never really need it for anything)
(defglobal *universal-fun-type* -1)

;;; a vector that maps type codes to layouts, used for quickly finding
;;; the layouts of built-in classes
(defglobal **built-in-class-codes** #()) ; initialized in cold load
(declaim (type simple-vector **built-in-class-codes**))
