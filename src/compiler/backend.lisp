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

(in-package "SB!C")

;;;; miscellaneous backend properties

;;; the number of references that a TN must have to offset the
;;; overhead of saving the TN across a call
(defvar *backend-register-save-penalty* 0)
(declaim (type index *backend-register-save-penalty*))

;;; the byte order of the target machine. :BIG-ENDIAN has the MSB first (e.g.
;;; IBM RT), :LITTLE-ENDIAN has the MSB last (e.g. DEC VAX).
(defvar *backend-byte-order*)
(declaim (type (member nil :little-endian :big-endian) *backend-byte-order*))

;;; translation from SC numbers to SC info structures. SC numbers are always
;;; used instead of names at run time, so changing this vector changes all the
;;; references.
(defvar *backend-sc-numbers* (make-array sc-number-limit :initial-element nil))
(declaim (type sc-vector *backend-sc-numbers*))

;;; a list of all the SBs defined, so that we can easily iterate over them
(defvar *backend-sb-list* ())
(declaim (type list *backend-sb-list*))

;;; translation from template names to template structures
(defvar *backend-template-names* (make-hash-table :test 'eq))
(declaim (type hash-table *backend-template-names*))

;;; hashtables mapping from SC and SB names to the corresponding structures
;;;
;;; CMU CL comment:
;;;   The META versions are only used at meta-compile and load times,
;;;   so the defining macros can change these at meta-compile time
;;;   without breaking the compiler.
;;; FIXME: Couldn't the META versions go away in SBCL now that we don't
;;; have to worry about metacompiling and breaking the compiler?
(defvar *backend-sc-names* (make-hash-table :test 'eq))
(defvar *backend-sb-names* (make-hash-table :test 'eq))
(defvar *backend-meta-sc-names* (make-hash-table :test 'eq))
(defvar *backend-meta-sb-names* (make-hash-table :test 'eq))
(declaim (type hash-table
               *backend-sc-names*
               *backend-sb-names*
               *backend-meta-sc-names*
               *backend-meta-sb-names*))


;;; like *SC-NUMBERS*, but updated at meta-compile time
;;;
;;; FIXME: As per *BACKEND-META-SC-NAMES* and *BACKEND-META-SB-NAMES*,
;;; couldn't we get rid of this in SBCL?
(defvar *backend-meta-sc-numbers*
  (make-array sc-number-limit :initial-element nil))
(declaim (type sc-vector *backend-meta-sc-numbers*))

;;; translations from primitive type names to the corresponding
;;; primitive-type structure.
(defvar *backend-primitive-type-names*
  (make-hash-table :test 'eq))
(declaim (type hash-table *backend-primitive-type-names*))

;;; This establishes a convenient handle on primitive type unions, or
;;; whatever. These names can only be used as the :ARG-TYPES or
;;; :RESULT-TYPES for VOPs and can map to anything else that can be
;;; used as :ARG-TYPES or :RESULT-TYPES (e.g. :OR, :CONSTANT).
(defvar *backend-primitive-type-aliases* (make-hash-table :test 'eq))
(declaim (type hash-table *backend-primitive-type-aliases*))

;;; meta-compile time translation from names to primitive types
;;;
;;; FIXME: As per *BACKEND-META-SC-NAMES* and *BACKEND-META-SB-NAMES*,
;;; couldn't we get rid of this in SBCL?
(defvar *backend-meta-primitive-type-names* (make-hash-table :test 'eq))
(declaim (type hash-table *meta-primitive-type-names*))

;;; The primitive type T is somewhat magical, in that it is the only
;;; primitive type that overlaps with other primitive types. An object
;;; of primitive-type T is in the canonical descriptor (boxed or pointer)
;;; representation.
;;;
;;; The T primitive-type is kept in this variable so that people who
;;; have to special-case it can get at it conveniently. This variable
;;; has to be set by the machine-specific VM definition, since the
;;; !DEF-PRIMITIVE-TYPE for T must specify the SCs that boxed objects
;;; can be allocated in.
(defvar *backend-t-primitive-type*)
(declaim (type primitive-type *backend-t-primitive-type*))

;;; a hashtable translating from VOP names to the corresponding VOP-PARSE
;;; structures. This information is only used at meta-compile time.
(defvar *backend-parsed-vops* (make-hash-table :test 'eq))
(declaim (type hash-table *backend-parsed-vops*))

;;; support for the assembler
(defvar *backend-instruction-formats* (make-hash-table :test 'eq))
(defvar *backend-instruction-flavors* (make-hash-table :test 'equal))
(defvar *backend-special-arg-types* (make-hash-table :test 'eq))
(declaim (type hash-table
               *backend-instruction-formats*
               *backend-instruction-flavors*
               *backend-special-arg-types*))

;;; mappings between CTYPE structures and the corresponding predicate.
;;; The type->predicate mapping is implemented as an alist because
;;; there is no such thing as a TYPE= hash table.
(defvar *backend-predicate-types* (make-hash-table :test 'eq))
(defvar *backend-type-predicates* nil)
(declaim (type hash-table *backend-predicate-types*))
(declaim (type list *backend-type-predicates*))

;;; a vector of the internal errors defined for this backend, or NIL if
;;; they haven't been installed yet
(defvar *backend-internal-errors* nil)
(declaim (type (or simple-vector null) *backend-internal-errors*))

;;;; VM support routines

;;; FIXME: Do we need this kind of indirection for the VM support
;;; routines any more?

;;; forward declaration
(defvar *backend-support-routines*)

(macrolet ((def-vm-support-routines (&rest routines)
             `(progn
                (eval-when (:compile-toplevel :load-toplevel :execute)
                  (defparameter *vm-support-routines* ',routines))
                (defstruct (vm-support-routines (:copier nil))
                  ,@(mapcar (lambda (routine)
                              `(,routine nil :type (or function null)))
                            routines))
                ,@(mapcar
                   (lambda (name)
                     `(defun ,name (&rest args)
                        (apply (or (,(symbolicate "VM-SUPPORT-ROUTINES-"
                                                  name)
                                    *backend-support-routines*)
                                   (error "machine-specific support ~S ~
                                           routine undefined"
                                          ',name))
                               args)))
                   routines))))

  (def-vm-support-routines

    ;; from vm.lisp
    immediate-constant-sc
    location-print-name
    combination-implementation-style

    ;; from primtype.lisp
    primitive-type-of
    primitive-type

    ;; from c-call.lisp
    make-call-out-tns

    ;; from call.lisp
    standard-arg-location
    make-return-pc-passing-location
    make-old-fp-passing-location
    make-old-fp-save-location
    make-return-pc-save-location
    make-arg-count-location
    make-nfp-tn
    make-stack-pointer-tn
    make-number-stack-pointer-tn
    make-unknown-values-locations
    select-component-format

    ;; from nlx.lisp
    make-nlx-sp-tn
    make-dynamic-state-tns
    make-nlx-entry-arg-start-location

    ;; from pred.lisp
    convert-conditional-move-p

    ;; from support.lisp
    generate-call-sequence
    generate-return-sequence

    ;; for use with scheduler
    emit-nop
    location-number))

(defprinter (vm-support-routines))

(defmacro !def-vm-support-routine (name ll &body body)
  (unless (member (intern (string name) (find-package "SB!C"))
                  *vm-support-routines*)
    (warn "unknown VM support routine: ~A" name))
  (let ((local-name (symbolicate "IMPL-OF-VM-SUPPORT-ROUTINE-" name)))
    `(progn
       (defun ,local-name ,ll ,@body)
       (setf (,(intern (concatenate 'simple-string
                                    "VM-SUPPORT-ROUTINES-"
                                    (string name))
                       (find-package "SB!C"))
              *backend-support-routines*)
             #',local-name))))

;;; the VM support routines
(defvar *backend-support-routines* (make-vm-support-routines))
(declaim (type vm-support-routines *backend-support-routines*))

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
  (:GUARD #!+(OR :SPARC-V8 (AND :SPARC-V9 (NOT :SPARC-64))) T
          #!-(OR :SPARC-V8 (AND :SPARC-V9 (NOT :SPARC-64))) NIL)
which means that whether this VOP will ever be used is determined at
compiler compile-time depending on the contents of
*SHEBANG-FEATURES*?.

As of SBCL-0.7pre57, a new special variable,
SB-C:*BACKEND-SUBFEATURES*?, is introduced. As of that version, only
VOPs translating %log1p? query it, and :PENTIUM-STYLE-FYL2XP1 is the
only useful value to be pushed onto that list, for x86. This is not
yet an ideal interface, but it does allow for compile-time
conditionalization.
|#

;;; The default value of NIL means use only unguarded VOPs. The
;;; initial value is customizeable via
;;; customize-backend-subfeatures.lisp
(defvar *backend-subfeatures* '#.sb-cold:*shebang-backend-subfeatures*)

;;; possible *BACKEND-SUBFEATURES* values:
;;;
;;; :PENTIUM-STYLE-FYL2XP1 is a useful value for x86 SBCLs to have on
;;; SB-C:*BACKEND-SUBFEATURES*?; it enables the use of the
;;; %flog1p-pentium? VOP rather than the %flog1p? VOP, which is a few
;;; instructions longer.
