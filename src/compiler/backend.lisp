;;;; This file contains backend-specific data. The original intent, in
;;;; CMU CL, was to allow compilation using different backends, as a
;;;; way of mutating a running CMU CL into a hybrid system which could
;;;; emit code for a different architecture. In SBCL, this is not
;;;; needed, since we have a cross-compiler which runs as an ordinary
;;;; Lisp program under SBCL or other Lisps. However, it still seems
;;;; reasonable to have all backendish things here in a single file.
;;;;
;;;; FIXME: Perhaps someday the vmdef.lisp and/or meta-vmdef.lisp stuff can
;;;; merged into this file, and/or the metaness can go away or at least be
;;;; radically simplified.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; miscellaneous backend properties

;;; the number of references that a TN must have to offset the
;;; overhead of saving the TN across a call
(defglobal *backend-register-save-penalty* 3)
(declaim (type index *backend-register-save-penalty*))

;;; the byte order of the target machine. :BIG-ENDIAN has the MSB first (e.g.
;;; IBM RT), :LITTLE-ENDIAN has the MSB last (e.g. DEC VAX).
(defglobal *backend-byte-order*
  #+little-endian :little-endian
  #+big-endian :big-endian)
(declaim (type (member nil :little-endian :big-endian) *backend-byte-order*))

;;; translation from SC numbers to SC info structures. SC numbers are always
;;; used instead of names at run time, so changing this vector changes all the
;;; references.
(defglobal *backend-sc-numbers* (make-array sb-vm:sc-number-limit :initial-element nil))
(declaim (type sc-vector *backend-sc-numbers*))

;;; a vector of all the SBs defined, so that we can easily iterate over them
(defglobal *backend-sbs* #())
(declaim (type simple-vector *backend-sbs*))

;;; hashtables mapping from SC and SB names to the corresponding structures
(defglobal *backend-sc-names* (make-hash-table))
(declaim (type hash-table *backend-sc-names*))

;;; translations from primitive type names to the corresponding
;;; primitive-type structure.
(defglobal *backend-primitive-type-names* (make-hash-table)) ; keys are symbols
(declaim (type hash-table *backend-primitive-type-names*))

;;; This establishes a convenient handle on primitive type unions, or
;;; whatever. These names can only be used as the :ARG-TYPES or
;;; :RESULT-TYPES for VOPs and can map to anything else that can be
;;; used as :ARG-TYPES or :RESULT-TYPES (e.g. :OR, :CONSTANT).
(defglobal *backend-primitive-type-aliases* nil)
(declaim (type list *backend-primitive-type-aliases*))

;;; The primitive type T is somewhat magical, in that it is the only
;;; primitive type that overlaps with other primitive types. An object
;;; of primitive-type T is in the canonical descriptor (boxed or pointer)
;;; representation.
;;;
;;; The T primitive-type is kept in this variable so that people who
;;; have to special-case it can get at it conveniently.
(define-symbol-macro *backend-t-primitive-type*
    (the primitive-type (load-time-value (primitive-type-or-lose t) t)))

;;; a hashtable translating from VOP names to the corresponding VOP-PARSE
;;; structures. This information is only used at meta-compile time.
(defglobal *backend-parsed-vops* (make-hash-table)) ; keys are symbols
(declaim (type hash-table *backend-parsed-vops*))

;;; Mapping of predicate name (a symbol) to a type.
;;; This is an EQL table so that it uses symbol-hash.
(defglobal *backend-predicate-types* (make-hash-table))
;;; mappings between CTYPE structures and the corresponding predicate.
;;; The type->predicate mapping is implemented as an alist because
;;; there is no such thing as a TYPE= hash table.
;;; (But if we could make all the types use as keys in this table
;;; into interned types, could it become an EQ table perhaps?)
(defglobal *backend-type-predicates* nil)
(declaim (type hash-table *backend-predicate-types*))
(declaim (type list *backend-type-predicates*))

;;;; VM support routines which backends need to implement

;;; from vm.lisp
;;; immediate-constant-sc
;;; location-print-name
;;; combination-implementation-style
;;; convert-conditional-move-p
;;; boxed-immediate-sc-p

;;; from c-call.lisp
;;; make-call-out-tns

;;; from call.lisp
;;; make-return-pc-passing-location
;;; make-old-fp-passing-location
;;; make-old-fp-save-location
;;; make-return-pc-save-location
;;; make-arg-count-location

;;; from nlx.lisp
;;; make-nlx-entry-arg-start-location

;;; from pred.lisp
;;; convert-conditional-move-p

;;; from support.lisp
;;; generate-call-sequence
;;; generate-return-sequence

;;; for use with scheduler
;;; emit-nop
;;; location-number


;;;; This is a prototype interface to support Christophe Rhodes' new
;;;; (sbcl-0.pre7.57) VOP :GUARD clauses for implementations which
;;;; depend on CPU variants, e.g. the differences between I486,
;;;; Pentium, and Pentium Pro, or the differences between different
;;;; SPARC versions.

;;;; Christophe Rhodes' longer explanation (cut and pasted
;;;; from CLiki SBCL internals site 2001-10-12):
#|
In CMUCL, the :guard argument to VOPs provided a way of disallowing
the use of a particular VOP in compiled code. As an example, from the
SPARC code in CMUCL,

(DEFINE-VOP? (FAST-V8-TRUNCATE/SIGNED=>SIGNED? FAST-SAFE-ARITH-OP?)
  (:TRANSLATE TRUNCATE?)
  ...
  (:GUARD (OR (BACKEND-FEATUREP :SPARC-V8)
              (AND (BACKEND-FEATUREP :SPARC-V9)
                   (NOT (BACKEND-FEATUREP :SPARC-64)))))
  ...)

and at the IR2 translation stage, the function #'`(LAMBDA () ,GUARD) would be called.

Until SBCL-0.7pre57, this is translated as
  (:GUARD #+(OR :SPARC-V8 (AND :SPARC-V9 (NOT :SPARC-64))) T
          #-(OR :SPARC-V8 (AND :SPARC-V9 (NOT :SPARC-64))) NIL)
which means that whether this VOP will ever be used is determined at
compiler compile-time depending on the contents of SB-XC:*FEATURES*.

As of SBCL-0.7pre57, a new special variable,
SB-C:*BACKEND-SUBFEATURES*?, is introduced. As of that version, only
VOPs translating %log1p? query it, and :PENTIUM-STYLE-FYL2XP1 is the
only useful value to be pushed onto that list, for x86. This is not
yet an ideal interface, but it does allow for compile-time
conditionalization.
|#

;;; The default value of NIL means use only unguarded VOPs. The
;;; initial value is customizeable via customize-backend-subfeatures.lisp
(defvar *backend-subfeatures* '#.(sort sb-cold:backend-subfeatures #'string<))
#-sb-xc-host (declaim (always-bound *backend-subfeatures*))

;;; possible *BACKEND-SUBFEATURES* values:
;;;
;;; :PENTIUM-STYLE-FYL2XP1 is a useful value for x86 SBCLs to have on
;;; SB-C:*BACKEND-SUBFEATURES*?; it enables the use of the
;;; %flog1p-pentium? VOP rather than the %flog1p? VOP, which is a few
;;; instructions longer.
