;;;; Common Lisp pretty printer definitions that need to be on the
;;;; host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PRETTY")

;; This comes early so that fndb can use PPRINT-DISPATCH-TABLE as
;; a type-specifier.
(sb-xc:defstruct (pprint-dispatch-table
                  (:conc-name pp-dispatch-)
                  (:constructor make-pprint-dispatch-table
                      (entries number-matchable-p only-initial-entries))
                  (:copier nil) ; needs a deep copy
                  (:predicate nil))
  ;; A list of all the entries (except for CONS entries below) in highest
  ;; to lowest priority.
  (entries #() :type simple-vector)
  ;; A hash table mapping things to entries for type specifiers of the
  ;; form (CONS (MEMBER <thing>)). If the type specifier is of this form,
  ;; we put it in this hash table instead of the regular entries table.
  (cons-entries (make-hash-table :test 'eql) :read-only t)
  ;; NIL if this this table can't match any numeric type.
  ;; The conservative value is T.
  (number-matchable-p nil)
  (only-initial-entries nil :type boolean))

(declaim (freeze-type pprint-dispatch-table))
