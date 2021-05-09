;;;; stuff that knows how to load compiled code directly into core

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Unique number assigned into high 4 bytes of 64-bit code size slot
;;; so that we can sort the contents of varyobj space in a more-or-less
;;; predictable manner based on the order in which code was loaded.
;;; This wraps around at 32 bits, but it's still deterministic.
(define-load-time-global *code-serialno* 0)
(declaim (fixnum *code-serialno*))

;;; A CORE-OBJECT structure holds the state needed to resolve cross-component
;;; references during in-core compilation.
(defstruct (core-object
            (:constructor make-core-object (ephemeral))
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s :type t :identity t))))
            (:copier nil))
  ephemeral
  ;; A hashtable translating ENTRY-INFO structures to the corresponding actual
  ;; FUNCTIONs for functions in this compilation.
  (entry-table (make-hash-table :test 'eq) :type hash-table)
  ;; A hashtable translating ENTRY-INFO structures to a list of pairs
  ;; (<code object> . <offset>) describing the places that need to be
  ;; backpatched to point to the function for ENTRY-INFO.
  (patch-table (make-hash-table :test 'eq) :type hash-table)
  ;; A list of all the DEBUG-INFO objects created, kept so that we can
  ;; backpatch with the source info.
  (debug-info () :type list))

;;; Call the top level lambda function dumped for ENTRY, returning the
;;; values. ENTRY may be a :TOPLEVEL-XEP functional.
(defun core-call-toplevel-lambda (entry object)
  (declare (type functional entry) (type core-object object))
  (funcall (or (gethash (leaf-info entry)
                        (core-object-entry-table object))
               (error "Unresolved forward reference."))))
