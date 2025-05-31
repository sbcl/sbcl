;;; This is a semi-machine-generated file

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; These are the most commonly occurring structure-object subtypes sorted in
;;; in descending dynamic frequency of occurrence of the typep test of the type.
;;; The list was obtained from a regression run containing a modified
;;; 'structure-is-a' vop which counts executions in the manner of :SB-DYNCOUNT.
;;; Presumably this correlates well with the number of "static" insertions of
;;; each type test also. So it can decrease code size and increase instruction
;;; decode throughput since these all get IDs that encode as imm8 for x86.

;;; [Some manual editing here was necessary due to not having all the packages
;;; in all build configurations.]
;;; Also note that some types are here to force consistent assignment of
;;; a layout ID between both genesis passes. In particular, without HASHSET
;;; we'd get the rarely-seen header diff failure:
;;;   //testing for consistency of first and second GENESIS passes
;;;   diff -r src/runtime/genesis/instance.h output/genesis-2/instance.h
;;;   62,63c62,63
;;;   < #define HASHSET_LAYOUT_ID 135
;;;   < #define HASHSET_STORAGE_LAYOUT_ID 136
;;;   ---
;;;   > #define HASHSET_LAYOUT_ID 283
;;;   > #define HASHSET_STORAGE_LAYOUT_ID 284
;;;
(defparameter *popular-structure-types* (mapcar 'list '(
SB-KERNEL:CTYPE
HASH-TABLE
PACKAGE
SB-IMPL::ROBINHOOD-HASHSET
SB-IMPL::ROBINHOOD-HASHSET-STORAGE
SB-IMPL::GENERAL-HASH-TABLE
SB-LOCKLESS::LINKED-LIST
SB-LOCKLESS::FINALIZER-NODE
SB-C::NODE
SB-C::GLOBAL-CONFLICTS
SB-C::GLOBAL-VAR
SB-C::FUNCTIONAL
SB-C::LEAF
SB-KERNEL:ANSI-STREAM
SB-C::IR2-BLOCK
SB-C::VALUED-NODE
RANDOM-STATE
CAST
SB-KERNEL::TYPE-CONTEXT
SB-KERNEL:VALUES-TYPE
SB-SYS:FD-STREAM
SB-C::BASIC-COMBINATION
SB-INT:SSET-ELEMENT
SB-C:TN-REF
SB-KERNEL:ARGS-TYPE
SB-C::VOP
SB-C:STORAGE-BASE
SB-C:STORAGE-CLASS
SB-KERNEL:LEXENV
SB-ASSEM::ANNOTATION
SB-KERNEL:INTERSECTION-TYPE
SB-C:PRIMITIVE-TYPE
#+sb-fasteval SB-INTERPRETER:BASIC-ENV
SB-KERNEL:NUMERIC-TYPE
SB-KERNEL:CLASSOID
SB-KERNEL:UNION-TYPE
SB-C::CONSTRAINT
SB-C::TEMPLATE
SB-ASSEM:LABEL
SB-C::IR2-COMPONENT
SB-C::VOP-INFO
SB-C::IR2-LVAR
SB-IMPL::INFO-HASHTABLE
SB-INT:FORM-TRACKING-STREAM
SB-PRETTY:PRETTY-STREAM
SB-C::CONSET
SB-KERNEL:ARRAY-TYPE
SB-KERNEL:COMPOUND-TYPE
SB-KERNEL:NEGATION-TYPE
SB-REGALLOC::VERTEX
SB-THREAD:THREAD
SB-THREAD::AVLNODE
SB-C::ABSTRACT-LEXENV
SB-KERNEL:UNKNOWN-TYPE
SB-KERNEL:CONS-TYPE
SB-C::IR2-ENVIRONMENT
SB-C::BASIC-VAR
SB-KERNEL:FUN-DESIGNATOR-TYPE
SB-PRETTY::QUEUED-OP
SB-KERNEL:MEMBER-TYPE
SB-C::FUN-INFO
SB-C::COMPILED-DEBUG-FUN
SB-C::LOCATION-INFO
SB-C::TRANSFORM
SB-PRETTY::SECTION-START
SB-KERNEL:NAMED-TYPE
SB-C::CORE-OBJECT
SB-C::EQUALITY-CONSTRAINT
SB-C::ENTRY-INFO
SB-DI::COMPILED-CODE-LOCATION
SB-C::RETURN-INFO
SB-C::SOURCE-INFO
SB-KERNEL:HOST
TWO-WAY-STREAM
SB-ALIEN-INTERNALS:ALIEN-TYPE
SB-DI:FRAME
SB-C::COMPILED-DEBUG-INFO
SB-DI::COMPILED-DEBUG-FUN
SB-C::FUN-TYPE-ANNOTATION
SB-C::CLOOP
SB-DI:DEBUG-FUN
SB-C::COMPILED-DEBUG-FUN-OPTIONAL
SB-C::COMPILED-DEBUG-FUN-MORE
SB-C::COMPILED-DEBUG-FUN-EXTERNAL
SB-ALIEN::ALIEN-TYPE-CLASS
SB-PCL::CACHE
SB-KERNEL::CONDITION-SLOT
SB-PCL::CLASS-PRECEDENCE-DESCRIPTION
SB-KERNEL:FUN-TYPE
SB-C::FILE-INFO
SB-KERNEL:CHARACTER-SET-TYPE
SB-IMPL::PATTERN
SB-C::LVAR-ANNOTATION
SB-DI::COMPILED-DEBUG-BLOCK
SB-C::ARG-INFO
SB-C::COMPILED-DEBUG-FUN-TOPLEVEL
SB-C::COMPILED-DEBUG-FUN-CLEANUP
SB-DI::COMPILED-FRAME
SB-DISASSEM:SEGMENT
SB-ALIEN-INTERNALS:ALIEN-POINTER-TYPE
SB-C::DEBUG-SOURCE
SB-C::DEBUG-INFO
SB-ALIEN-INTERNALS:ALIEN-RECORD-TYPE
SB-C::LVAR-PROPER-SEQUENCE-ANNOTATION
SB-DI:CODE-LOCATION
SB-KERNEL:STRUCTURE-CLASSOID
SB-C::LVAR-FUNCTION-DESIGNATOR-ANNOTATION
SB-C::LVAR-MODIFIED-ANNOTATION
SB-DI::BOGUS-DEBUG-FUN
#+sb-simd-pack SB-KERNEL:SIMD-PACK-TYPE
#+sb-simd-pack-256 SB-KERNEL:SIMD-PACK-256-TYPE
#+sb-fasteval SB-INTERPRETER::SEXPR
SB-C::MODULAR-CLASS
SB-DI:DEBUG-BLOCK
SB-C::LVAR-HOOK
SB-KERNEL:HAIRY-TYPE
#+sb-fasteval SB-INTERPRETER::FRAME
SB-C::LOCAL-CALL-CONTEXT
SB-C::IR2-NLX-INFO
SB-C::LVAR-FUNCTION-ANNOTATION
SB-C::CORE-DEBUG-SOURCE
SB-REGALLOC::INTERFERENCE-GRAPH
SB-C::RESTART-LOCATION
#+sb-fasteval SB-INTERPRETER::LAMBDA-FRAME
SB-C::LVAR-LAMBDA-VAR-ANNOTATION
SB-KERNEL::UNDEFINED-CLASSOID
SB-ALIEN-INTERNALS:ALIEN-INTEGER-TYPE
SB-C::LVAR-TYPE-ANNOTATION
SB-C:DEFINITION-SOURCE-LOCATION
SB-DI:DEBUG-VAR
SB-DI::COMPILED-DEBUG-VAR
SB-ALIEN-INTERNALS:ALIEN-FUN-TYPE
SB-PCL::DFUN-INFO
TIMER
#+sb-fasteval SB-INTERPRETER::DECL-SCOPE
SB-C::UNDEFINED-WARNING
SB-C::MODULAR-FUN-INFO
SB-KERNEL:DEFSTRUCT-DESCRIPTION
SB-PCL::ACCESSOR-DFUN-INFO
SB-C::COMPILER-ERROR-CONTEXT
SB-C::DEFINITION-SOURCE-LOCATION+PLIST
SB-KERNEL:ALIEN-TYPE-TYPE
SB-KERNEL:DEFSTRUCT-SLOT-DESCRIPTION
SB-VM:PRIMITIVE-OBJECT
SB-PCL::ONE-INDEX-DFUN-INFO
SB-C::DEBUG-FUN
SB-ALIEN::ALIEN-C-STRING-TYPE
SB-DISASSEM:INSTRUCTION
SB-C::APPROXIMATE-KEY-INFO
SB-ALIEN-INTERNALS:ALIEN-RECORD-FIELD
SB-KERNEL:CONSTANT-TYPE
SB-ALIEN-INTERNALS:ALIEN-FLOAT-TYPE
SB-C::APPROXIMATE-FUN-TYPE
SB-ALIEN-INTERNALS:LOCAL-ALIEN-INFO
SB-ALIEN-INTERNALS:ALIEN-ARRAY-TYPE
SB-KERNEL::CONDITION-CLASSOID
SB-IMPL::ENCAPSULATION-INFO
SB-C::VOP-PARSE
SB-ALIEN-INTERNALS:ALIEN-VALUES-TYPE
SB-DISASSEM::SOURCE-FORM-CACHE
SB-IMPL::SHARP-EQUAL-WRAPPER
SB-DISASSEM::LOCATION-GROUP
SB-FASL::CIRCULARITY
SB-LOOP::LOOP-COLLECTOR
SB-IMPL::COMMA
SB-ALIEN-INTERNALS:ALIEN-SINGLE-FLOAT-TYPE
SB-DEBUG::TRACE-INFO
SB-ALIEN-INTERNALS:ALIEN-DOUBLE-FLOAT-TYPE
SB-VM::SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES
SB-DI:BREAKPOINT
SB-KERNEL:LOGICAL-HOST
RESTART
SB-ALIEN-INTERNALS:HEAP-ALIEN-INFO
SB-DI::BREAKPOINT-DATA
SB-PRETTY::PPRINT-DISPATCH-ENTRY
SB-IMPL::EXTERNAL-FORMAT
SB-PCL::METHOD-COMBINATION-INFO
SB-ALIEN-INTERNALS:ALIEN-ENUM-TYPE
#+sb-fasteval SB-INTERPRETER::SYMBOL-MACRO-SCOPE
SB-INT:DEPRECATION-INFO
SB-DI::FUN-END-COOKIE
SB-ALIEN::SHARED-OBJECT
SB-PCL::FAST-METHOD-CALL
SB-C::DXABLE-ARGS
)))

;;; The rationale for using (signed-byte 8) for small IDs on the x86
;;; is that imm8 operands are sign-extended.
;;; The rationale for using (unsigned-byte 8) for small IDs on ARM
;;; is that imm8 operands are NOT sign-extended.
;;; This condition should probably be x86[-64] and everybody else.
;;; I suspect that nobody else benefits from sign-extended immediates.

(defconstant layout-id-type
  #+(or arm mips) 'unsigned-byte
  #-(or arm mips) 'signed-byte)

;;; There are a few wired IDs:
;;; (I doubt that these numbers need to be hand-chosen now. It should be enough
;;; to list them in compiler/generic/layout-ids)
;;;   0 = T
;;;   1 = STRUCTURE-OBJECT
;;;   2 = unused : FIXME
;;;   3 = LAYOUT
;;;   4 = SB-LOCKLESS::LIST-NODE
;;;   5 = SB-BROTHERTREE::UNARY-NODE
(ecase layout-id-type
  (unsigned-byte
   ;; Assign all the above an (UNSIGNED-BYTE 8) layout-id.
   (let ((id 5)) ; pre-increment when using
     (dolist (item *popular-structure-types*)
       ;; Because of (MAPCAR #'LIST ...) it is ok to modify this list.
       (rplacd item (incf id)))))
  (signed-byte
   ;; Assign all the above a (SIGNED-BYTE 8) layout-id.
   ;; There is room for more types that are encodable as one byte.
   ;; It might be interesting to figure out a way to allow user code
   ;; to avail itself of some of that encoding space.
   (let ((id -128))
     (dolist (item *popular-structure-types*)
       ;; Because of (MAPCAR #'LIST ...) it is ok to modify this list.
       (rplacd item id)
       (setq id (if (= id -1) 6 ; hop over the wired IDs
                    (1+ id)))))))

(defglobal *general-layout-uniqueid-counter*  ; incremented before use
  (ecase sb-kernel::layout-id-type
    (signed-byte 127) ; predefined IDs range from -128 to 127
    (unsigned-byte 255))) ; all IDs are unsigned integers
;;; Conditions are numbered from -128 downward,
;;; but only if layout IDs can be negative.
(defglobal *condition-layout-uniqueid-counter* -128) ; decremented before use

(defun choose-layout-id (name conditionp)
  (case name
    ((t) 0)
    (structure-object 1)
    (layout 3)
    (sb-lockless::list-node 4)
    (sb-brothertree::unary-node 5)
    (t (or (cdr (assq name sb-kernel::*popular-structure-types*))
           (ecase sb-kernel::layout-id-type
             (unsigned-byte
              (incf *general-layout-uniqueid-counter*))
             (signed-byte
              (if conditionp
                  ;; It doesn't really matter what ID is assigned to a CONDITION subtype
                  ;; because we don't use the IDs for type testing. Nor for standard-object.
                  ;; But I'd like to a have a quick visual scan of the IDs assigned during
                  ;; genesis by giving them negative values which can't otherwise occur.
                  (decf *condition-layout-uniqueid-counter*)
                  (incf *general-layout-uniqueid-counter*))))))))
