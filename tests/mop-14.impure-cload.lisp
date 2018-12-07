;;;; miscellaneous side-effectful tests of the MOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;; this file attempts to test the computation of final discriminating
;;; functions for slot-valuish generic functions in the presence of
;;; large hierarchies of slot definitions with a forward-referenced
;;; superclass.  (This used to fail in cache-filling code: see reports
;;; from Levente Mészáros sbcl-devel 2006-04-19)

(defclass dwim-slot-definition
  (sb-mop:standard-slot-definition)
  ())

(defclass dwim-direct-slot-definition
  (sb-mop:standard-direct-slot-definition dwim-slot-definition)
  ())

(defclass dwim-effective-slot-definition
  (extra-effective-slot-definition
   sb-mop:standard-effective-slot-definition dwim-slot-definition)
  ())
(defclass dwim-attribute-slot-definition
  (dwim-slot-definition)
  ())

(defclass dwim-attribute-effective-slot-definition
  (dwim-effective-slot-definition dwim-attribute-slot-definition)
  ())

(defclass dwim-attribute-direct-slot-definition
  (dwim-direct-slot-definition dwim-attribute-slot-definition)
  ())

(defclass extra-effective-slot-definition ()
  ())
