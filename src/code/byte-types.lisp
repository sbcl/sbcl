;;;; types which are needed to implement byte-compiled functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; types

(deftype stack-pointer ()
  `(integer 0 ,(1- sb!vm:*target-most-positive-fixnum*)))

;;; KLUDGE: bare numbers, no documentation, ick.. -- WHN 19990701
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant max-pc (1- (ash 1 24))))

(deftype pc ()
  `(integer 0 ,max-pc))

(deftype return-pc ()
  `(integer ,(- max-pc) ,max-pc))

;;;; byte functions

;;; This abstract class represents any type of byte-compiled function.
(defstruct (byte-function-or-closure
	    (:alternate-metaclass funcallable-instance
				  funcallable-structure-class
				  make-funcallable-structure-class)
	    (:type funcallable-structure)
	    (:constructor nil)
	    (:copier nil)))

;;; a byte-compiled closure
(defstruct (byte-closure
	    (:include byte-function-or-closure)
	    (:constructor make-byte-closure (function data))
	    (:type funcallable-structure)
	    (:print-object
	     (lambda (x stream)
	       (print-unreadable-object (x stream :type t :identity t)
		 (prin1 (byte-function-name (byte-closure-function x))
			stream))))
	    (:copier nil))
  ;; the byte function that we call
  (function (required-argument) :type byte-function)
  ;; the closure data vector
  (data (required-argument) :type simple-vector))

;;; any non-closure byte function (including the hidden function
;;; object for a closure)
(defstruct (byte-function (:include byte-function-or-closure)
			  (:type funcallable-structure)
			  (:constructor nil)
			  (:copier nil))
  ;; The component that this XEP is an entry point into. NIL until
  ;; LOAD or MAKE-CORE-BYTE-COMPONENT fills it in. They count on this
  ;; being the first slot.
  (component nil :type (or null code-component))
  ;; Debug name of this function.
  (name nil))
(def!method print-object ((x byte-function) stream)
  ;; FIXME: I think functions should probably print either as
  ;; #<FUNCTION ..> or as #<COMPILED-FUNCTION ..>, since those are
  ;; their user-visible types. (And this should be true for
  ;; BYTE-CLOSURE objects too.)
  (print-unreadable-object (x stream :identity t)
    (format stream "byte function ~S" (byte-function-name x))))

;;; fixed-argument byte function
(defstruct (simple-byte-function (:include byte-function)
				 (:type funcallable-structure)
				 (:copier nil))
  ;; The number of arguments expected.
  (num-args 0 :type (integer 0 #.call-arguments-limit))
  ;; The start of the function.
  (entry-point 0 :type index))

;;; variable-arg-count byte function
(defstruct (hairy-byte-function (:include byte-function)
				(:type funcallable-structure)
				(:copier nil))
  ;; The minimum and maximum number of args, ignoring &REST and &KEY.
  (min-args 0 :type (integer 0 #.call-arguments-limit))
  (max-args 0 :type (integer 0 #.call-arguments-limit))
  ;; List of the entry points for min-args, min-args+1, ... max-args.
  (entry-points nil :type list)
  ;; The entry point to use when there are more than max-args. Only
  ;; filled in where okay. In other words, only when &REST or &KEY is
  ;; specified.
  (more-args-entry-point nil :type (or null (unsigned-byte 24)))
  ;; The number of ``more-arg'' args.
  (num-more-args 0 :type (integer 0 #.call-arguments-limit))
  ;; True if there is a rest-arg.
  (rest-arg-p nil :type (member t nil))
  ;; True if there are keywords. Note: keywords might still be NIL
  ;; because having &KEY with no keywords is valid and should result
  ;; in &ALLOW-OTHER-KEYS processing. If :ALLOW-OTHERS, then allow
  ;; other keys.
  (keywords-p nil :type (member t nil :allow-others))
  ;; list of &KEY arguments. Each element is a list of:
  ;; key, default, supplied-p.
  (keywords nil :type list))

#!-sb-fluid (declaim (freeze-type byte-function-or-closure))
