;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(defvar *alien-type-classes* (make-hash-table :test 'eq))

(defvar *new-auxiliary-types* nil)

;;; the list of record types that have already been unparsed. This is
;;; used to keep from outputting the slots again if the same structure
;;; shows up twice.
(defvar *record-types-already-unparsed*)
