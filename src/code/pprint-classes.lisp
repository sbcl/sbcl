;;;; Common Lisp pretty printer

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PRETTY")

;;; Ancestral types
(defstruct (queued-op (:constructor nil) (:copier nil))
  (posn 0 :type posn))

(defstruct (block-end (:include queued-op) (:copier nil))
  (suffix nil :type (or null simple-string)))

(defstruct (section-start (:include queued-op)
                          (:constructor nil)
                          (:copier nil))
  (depth 0 :type index)
  (section-end nil :type (or null newline block-end)))

(defstruct (newline (:include section-start) (:copier nil))
  (kind (missing-arg)
        :type (member :linear :fill :miser :literal :mandatory)))
(declaim (freeze-type newline))

(defstruct (indentation (:include queued-op)
                        (:copier nil))
  (kind (missing-arg) :type (member :block :current))
  (amount 0 :type fixnum))
(declaim (freeze-type indentation))

(defstruct (block-start (:include section-start)
                        (:copier nil))
  (block-end nil :type (or null block-end))
  (prefix nil :type (or null simple-string))
  (suffix nil :type (or null simple-string)))
(declaim (freeze-type block-start))

(defstruct (tab (:include queued-op)
                (:copier nil))
  (sectionp nil :type (member t nil))
  (relativep nil :type (member t nil))
  (colnum 0 :type column)
  (colinc 0 :type column))
(declaim (freeze-type queued-op)) ; and all subtypes
