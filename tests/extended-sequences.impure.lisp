;;;; Tests related to extended sequences.

;;;; This file is impure because we want to be able to define methods
;;;; implementing the extended sequence protocol.

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

(load "test-util.lisp")
(load "assertoid.lisp")

;;; For the following situation:
;;; - result type is a type specifier designating a DEFTYPEd type
;;; - the type expands to a the name of a user-defined sequence class
;;; - not all mandatory sequence protocol methods are define for the
;;;   user-define sequence class
;;; MAKE-SEQUENCE used to signal a SIMPLE-TYPE-ERROR referring to the
;;; unexpanded type specifier, instead of signaling a
;;; SEQUENCE:PROTOCOL-UNIMPLEMENTED error.
(defclass bug-1315846-simple-sequence (sequence) ())

(deftype bug-1315846-sequence ()
  'bug-1315846-simple-sequence)

(with-test (:name (make-sequence deftype :bug-1315846))
  (assert-error (make-sequence 'bug-1315846-sequence 10)
                sequence::protocol-unimplemented))
