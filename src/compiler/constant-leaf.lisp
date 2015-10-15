;;;; more structures for the first intermediate representation in the
;;;; compiler, IR1

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; This file was split out from 'node' because it contains a reference to
;;; an IR2 structure and has no explicit references from IR1 structures
;;; (in terms of the :TYPE option to defstruct slots).
;;; The other IR1 types involved in dependency cycles are always going
;;; to cause a problem unless the compiler is enhanced to deal with cycles.

;;; The CONSTANT structure is used to represent known constant values.
;;; Since the same constant leaf may be shared between named and anonymous
;;; constants, %SOURCE-NAME is never used.
(def!struct (constant (:constructor make-constant (value
                                                   &aux
                                                   (type (ctype-of value))
                                                   (%source-name '.anonymous.)
                                                   (where-from :defined)))
                      (:include leaf))
  ;; the value of the constant
  (value (missing-arg) :type t)
  ;; Boxed TN for this constant, if any.
  (boxed-tn nil :type (or null tn)))
(defprinter (constant :identity t)
  value)

#!-sb-fluid
(declaim (freeze-type leaf))
