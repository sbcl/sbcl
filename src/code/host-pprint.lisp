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

(in-package "SB!PRETTY")

;; This comes early so that fndb can use PPRINT-DISPATCH-TABLE as
;; a type-specifier.
(sb!xc:defstruct (pprint-dispatch-table
                  (:constructor make-pprint-dispatch-table (&optional entries))
                  (:copier nil) ; needs a deep copy
                  (:predicate nil))
  ;; A list of all the entries (except for CONS entries below) in highest
  ;; to lowest priority.
  (entries nil :type list)
  ;; A hash table mapping things to entries for type specifiers of the
  ;; form (CONS (MEMBER <thing>)). If the type specifier is of this form,
  ;; we put it in this hash table instead of the regular entries table.
  (cons-entries (make-hash-table :test 'eql) :read-only t))
#+sb-xc
(def!method print-object ((table pprint-dispatch-table) stream)
  (print-unreadable-object (table stream :type t :identity t)))

;;; Structures defined here only because we use DEF!STRUCT as a crutch
;;; to solve the problem of mutually referential structures not being
;;; efficiently compiled with regard to their type checks.

(def!type posn () 'fixnum)

(def!struct (queued-op (:constructor nil)
                       (:copier nil))
  (posn 0 :type posn))

(def!struct (block-end (:include queued-op)
                       (:copier nil))
  (suffix nil :type (or null simple-string)))

(def!struct (section-start (:include queued-op)
                           (:constructor nil)
                           (:copier nil))
  (depth 0 :type index)
  (section-end nil :type (or null newline block-end)))

(def!struct (newline (:include section-start)
                     (:copier nil))
  (kind (missing-arg)
        :type (member :linear :fill :miser :literal :mandatory)))


