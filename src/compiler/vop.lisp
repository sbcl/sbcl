;;;; structures for the second (virtual machine) intermediate
;;;; representation in the compiler, IR2

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; the largest number of TNs whose liveness changes that we can have
;;; in any block
(def!constant local-tn-limit 64)

(deftype local-tn-number () `(integer 0 (,local-tn-limit)))
(deftype local-tn-count () `(integer 0 ,local-tn-limit))
(deftype local-tn-vector () `(simple-vector ,local-tn-limit))
(deftype local-tn-bit-vector () `(simple-bit-vector ,local-tn-limit))

;;; type of an SC number
(deftype sc-number () `(integer 0 (,sc-number-limit)))

;;; types for vectors indexed by SC numbers
(deftype sc-vector () `(simple-vector ,sc-number-limit))
(deftype sc-bit-vector () `(simple-bit-vector ,sc-number-limit))

;;; the different policies we can use to determine the coding strategy
(deftype ltn-policy ()
  '(member :safe :small :fast :fast-safe))

;;;; PRIMITIVE-TYPEs

;;; A PRIMITIVE-TYPE is used to represent the aspects of type
;;; interesting to the VM. Selection of IR2 translation templates is
;;; done on the basis of the primitive types of the operands, and the
;;; primitive type of a value is used to constrain the possible
;;; representations of that value.
(defstruct (primitive-type (:copier nil))
  ;; the name of this PRIMITIVE-TYPE
  (name nil :type symbol :read-only t)
  ;; a list of the SC numbers for all the SCs that a TN of this type
  ;; can be allocated in
  (scs nil :type list :read-only t)
  ;; the Lisp type equivalent to this type. If this type could never be
  ;; returned by PRIMITIVE-TYPE, then this is the NIL (or empty) type.
  ;; TYPE-SPECIFIER is too general - this doesn't allow CLASS/CLASSOID.
  (specifier (missing-arg) :type (or symbol list) :read-only t))

(defprinter (primitive-type)
  name)

;;;; IR1 annotations used for IR2 conversion

;;; BLOCK-INFO
;;;    Holds the IR2-BLOCK structure. If there are overflow blocks,
;;;    then this points to the first IR2-BLOCK. The BLOCK-INFO of the
;;;    dummy component head and tail are dummy IR2 blocks that begin
;;;    and end the emission order thread.
;;;
;;; COMPONENT-INFO
;;;    Holds the IR2-COMPONENT structure.
;;;
;;; LVAR-INFO
;;;    Holds the IR2-LVAR structure. LVARs whose values aren't used
;;;    won't have any. XXX
;;;
;;; CLEANUP-INFO
;;;    If non-null, then a TN in which the affected dynamic
;;;    environment pointer should be saved after the binding is
;;;    instantiated.
;;;
;;; PHYSENV-INFO
;;;    Holds the IR2-PHYSENV structure.
;;;
;;; TAIL-SET-INFO
;;;    Holds the RETURN-INFO structure.
;;;
;;; NLX-INFO-INFO
;;;    Holds the IR2-NLX-INFO structure.
;;;
;;; LEAF-INFO
;;;    If a non-set lexical variable, the TN that holds the value in
;;;    the home environment. If a constant, then the corresponding
;;;    constant TN. If an XEP lambda, then the corresponding
;;;    Entry-Info structure.
;;;
;;; BASIC-COMBINATION-INFO
;;;    The template chosen by LTN, or
;;;     :FULL if this is definitely a full call.
;;;     :FUNNY if this is an oddball thing with IR2-convert.
;;;     :LOCAL if this is a local call.
;;;
;;; NODE-TAIL-P
;;;    After LTN analysis, this is true only in combination nodes that are
;;;    truly tail recursive.

;;; An IR2-BLOCK holds information about a block that is used during
;;; and after IR2 conversion. It is stored in the BLOCK-INFO slot for
;;; the associated block.
(defstruct (ir2-block (:include block-annotation)
                      (:constructor make-ir2-block (block))
                      (:copier nil))
  ;; the IR2-BLOCK's number, which differs from BLOCK's BLOCK-NUMBER
  ;; if any blocks are split. This is assigned by lifetime analysis.
  (number nil :type (or index null))
  ;; information about unknown-values LVARs that is used by stack
  ;; analysis to do stack simulation. An UNKNOWN-VALUES LVAR is PUSHED
  ;; if its DEST is in another block. Similarly, a LVAR is POPPED if
  ;; its DEST is in this block but has its uses elsewhere. The LVARs
  ;; are in the order that are pushed/popped in the block. Note that
  ;; the args to a single MV-COMBINATION appear reversed in POPPED,
  ;; since we must effectively pop the last argument first. All pops
  ;; must come before all pushes (although internal MV uses may be
  ;; interleaved.) POPPED is computed by LTN, and PUSHED is computed
  ;; by stack analysis.
  (pushed () :type list)
  (popped () :type list)
  ;; the result of stack analysis: lists of all the unknown-values
  ;; LVARs on the stack at the block start and end, topmost LVAR
  ;; first.
  (start-stack () :type list)
  (end-stack () :type list)
  ;; the first and last VOP in this block. If there are none, both
  ;; slots are null.
  (start-vop nil :type (or vop null))
  (last-vop nil :type (or vop null))
  ;; the number of local TNs actually allocated
  (local-tn-count 0 :type local-tn-count)
  ;; a vector that maps local TN numbers to TNs. Some entries may be
  ;; NIL, indicating that that number is unused. (This allows us to
  ;; delete local conflict information without compressing the LTN
  ;; numbers.)
  ;;
  ;; If an entry is :MORE, then this block contains only a single VOP.
  ;; This VOP has so many more arguments and/or results that they
  ;; cannot all be assigned distinct LTN numbers. In this case, we
  ;; assign all the more args one LTN number, and all the more results
  ;; another LTN number. We can do this, since more operands are
  ;; referenced simultaneously as far as conflict analysis is
  ;; concerned. Note that all these :MORE TNs will be global TNs.
  (local-tns (make-array local-tn-limit) :type local-tn-vector)
  ;; Bit-vectors used during lifetime analysis to keep track of
  ;; references to local TNs. When indexed by the LTN number, the
  ;; index for a TN is non-zero in WRITTEN if it is ever written in
  ;; the block, and in LIVE-OUT if the first reference is a read.
  (written (make-array local-tn-limit :element-type 'bit
                       :initial-element 0)
           :type local-tn-bit-vector)
  (live-out (make-array local-tn-limit :element-type 'bit)
            :type local-tn-bit-vector)
  ;; This is similar to the above, but is updated by lifetime flow
  ;; analysis to have a 1 for LTN numbers of TNs live at the end of
  ;; the block. This takes into account all TNs that aren't :LIVE.
  (live-in (make-array local-tn-limit :element-type 'bit :initial-element 0)
           :type local-tn-bit-vector)
  ;; a thread running through the global-conflicts structures for this
  ;; block, sorted by TN number
  (global-tns nil :type (or global-conflicts null))
  ;; the assembler label that points to the beginning of the code for
  ;; this block, or NIL when we haven't assigned a label yet
  (%label nil)
  ;; the assembler label that points to the trampoline for this block,
  ;; or NIL if unassigned yet. Only meaningful for local call targets.
  (%trampoline-label nil)
  ;; T if the preceding block assumes it can drop thru to %label
  (dropped-thru-to nil)
  ;; list of LOCATION-INFO structures describing all the interesting
  ;; (to the debugger) locations in this block
  (locations nil :type list))

(defprinter (ir2-block)
  (pushed :test pushed)
  (popped :test popped)
  (start-vop :test start-vop)
  (last-vop :test last-vop)
  (local-tn-count :test (not (zerop local-tn-count)))
  (%label :test %label))

;;; An IR2-LVAR structure is used to annotate LVARs that are used as a
;;; function result LVARs or that receive MVs.
(defstruct (ir2-lvar
            (:constructor make-ir2-lvar (primitive-type))
            (:copier nil))
  ;; If this is :DELAYED, then this is a single value LVAR for which
  ;; the evaluation of the use is to be postponed until the evaluation
  ;; of destination. This can be done for ref nodes or predicates
  ;; whose destination is an IF.
  ;;
  ;; If this is :FIXED, then this LVAR has a fixed number of values,
  ;; with the TNs in LOCS.
  ;;
  ;; If this is :UNKNOWN, then this is an unknown-values LVAR, using
  ;; the passing locations in LOCS.
  ;;
  ;; If this is :UNUSED, then this LVAR should never actually be used
  ;; as the destination of a value: it is only used tail-recursively.
  (kind :fixed :type (member :delayed :fixed :unknown :unused))
  ;; The primitive-type of the first value of this LVAR. This is
  ;; primarily for internal use during LTN, but it also records the
  ;; type restriction on delayed references. In multiple-value
  ;; contexts, this is null to indicate that it is meaningless. This
  ;; is always (primitive-type (lvar-type cont)), which may be more
  ;; restrictive than the tn-primitive-type of the value TN. This is
  ;; becase the value TN must hold any possible type that could be
  ;; computed (before type checking.) XXX
  (primitive-type nil :type (or primitive-type null))
  ;; Locations used to hold the values of the LVAR. If the number of
  ;; values if fixed, then there is one TN per value. If the number of
  ;; values is unknown, then this is a two-list of TNs holding the
  ;; start of the values glob and the number of values. Note that
  ;; since type checking is the responsibility of the values receiver,
  ;; these TNs primitive type is only based on the proven type
  ;; information.
  (locs nil :type list)
  (stack-pointer nil :type (or tn null)))

(defprinter (ir2-lvar)
  kind
  primitive-type
  locs)

;;; An IR2-COMPONENT serves mostly to accumulate non-code information
;;; about the component being compiled.
(def!struct (ir2-component (:copier nil))
  ;; the counter used to allocate global TN numbers
  (global-tn-counter 0 :type index)
  ;; NORMAL-TNS is the head of the list of all the normal TNs that
  ;; need to be packed, linked through the Next slot. We place TNs on
  ;; this list when we allocate them so that Pack can find them.
  ;;
  ;; RESTRICTED-TNS are TNs that must be packed within a finite SC. We
  ;; pack these TNs first to ensure that the restrictions will be
  ;; satisfied (if possible).
  ;;
  ;; WIRED-TNs are TNs that must be packed at a specific location. The
  ;; SC and OFFSET are already filled in.
  ;;
  ;; CONSTANT-TNs are non-packed TNs that represent constants.
  (normal-tns nil :type (or tn null))
  (restricted-tns nil :type (or tn null))
  (wired-tns nil :type (or tn null))
  (constant-tns nil :type (or tn null))
  ;; a list of all the :COMPONENT TNs (live throughout the component).
  ;; These TNs will also appear in the {NORMAL,RESTRICTED,WIRED} TNs
  ;; as appropriate to their location.
  (component-tns () :type list)
  ;; If this component has a NFP, then this is it.
  (nfp nil :type (or tn null))
  ;; a list of the explicitly specified save TNs (kind
  ;; :SPECIFIED-SAVE). These TNs will also appear in the
  ;; {NORMAL,RESTRICTED,WIRED} TNs as appropriate to their location.
  (specified-save-tns () :type list)
  ;; a list of all the blocks whose IR2-BLOCK has a non-null value for
  ;; POPPED. This slot is initialized by LTN-ANALYZE as an input to
  ;; STACK-ANALYZE.
  (values-receivers nil :type list)
  ;; an adjustable vector that records all the constants in the
  ;; constant pool. A non-immediate :CONSTANT TN with offset 0 refers
  ;; to the constant in element 0, etc. Normal constants are
  ;; represented by the placing the CONSTANT leaf in this vector. A
  ;; load-time constant is distinguished by being a cons (KIND .
  ;; WHAT). KIND is a keyword indicating how the constant is computed,
  ;; and WHAT is some context.
  ;;
  ;; These load-time constants are recognized:
  ;;
  ;; (:entry . <function>)
  ;;    Is replaced by the code pointer for the specified function.
  ;;    This is how compiled code (including DEFUN) gets its hands on
  ;;    a function. <function> is the XEP lambda for the called
  ;;    function; its LEAF-INFO should be an ENTRY-INFO structure.
  ;;
  ;; (:label . <label>)
  ;;    Is replaced with the byte offset of that label from the start
  ;;    of the code vector (including the header length.)
  ;;
  ;; A null entry in this vector is a placeholder for implementation
  ;; overhead that is eventually stuffed in somehow.
  (constants (make-array 10 :fill-pointer 0 :adjustable t) :type vector)
  ;; some kind of info about the component's run-time representation.
  ;; This is filled in by the VM supplied SELECT-COMPONENT-FORMAT function.
  format
  ;; a list of the ENTRY-INFO structures describing all of the entries
  ;; into this component. Filled in by entry analysis.
  (entries nil :type list)
  ;; head of the list of :ALIAS TNs in this component, threaded by TN-NEXT
  (alias-tns nil :type (or tn null))
  ;; SPILLED-VOPS is a hashtable translating from "interesting" VOPs
  ;; to a list of the TNs spilled at that VOP. This is used when
  ;; computing debug info so that we don't consider the TN's value to
  ;; be valid when it is in fact somewhere else. SPILLED-TNS has T for
  ;; every "interesting" TN that is ever spilled, providing a
  ;; representation that is more convenient some places.
  (spilled-vops (make-hash-table :test 'eq) :type hash-table)
  (spilled-tns (make-hash-table :test 'eq) :type hash-table)
  ;; dynamic vop count info. This is needed by both ir2-convert and
  ;; setup-dynamic-count-info. (But only if we are generating code to
  ;; collect dynamic statistics.)
  #!+sb-dyncount
  (dyncount-info nil :type (or null dyncount-info)))

;;; An ENTRY-INFO condenses all the information that the dumper needs
;;; to create each XEP's function entry data structure. ENTRY-INFO
;;; structures are sometimes created before they are initialized,
;;; since IR2 conversion may need to compile a forward reference. In
;;; this case the slots aren't actually initialized until entry
;;; analysis runs.
(defstruct (entry-info (:copier nil))
  ;; TN, containing closure (if needed) for this function in the home
  ;; environment.
  (closure-tn nil :type (or null tn))
  ;; a label pointing to the entry vector for this function, or NIL
  ;; before ENTRY-ANALYZE runs
  (offset nil :type (or label null))
  ;; If this function was defined using DEFUN, then this is the name
  ;; of the function, a symbol or (SETF <symbol>). Otherwise, this is
  ;; some string that is intended to be informative.
  (name "<not computed>" :type (or simple-string list symbol))
  ;; the argument list that the function was defined with.
  (arguments nil :type list)
  ;; a function type specifier representing the arguments and results
  ;; of this function
  (type 'function :type (or list (member function)))
  ;; docstring and/or xref information for the XEP
  (info nil :type (or null simple-vector string (cons string simple-vector))))

;;; An IR2-PHYSENV is used to annotate non-LET LAMBDAs with their
;;; passing locations. It is stored in the PHYSENV-INFO.
(defstruct (ir2-physenv (:copier nil))
  ;; TN info for closed-over things within the function: an alist
  ;; mapping from NLX-INFOs and LAMBDA-VARs to TNs holding the
  ;; corresponding thing within this function
  ;;
  ;; Elements of this list have a one-to-one correspondence with
  ;; elements of the PHYSENV-CLOSURE list of the PHYSENV object that
  ;; links to us.
  (closure (missing-arg) :type list :read-only t)
  ;; the TNs that hold the OLD-FP and RETURN-PC within the function.
  ;; We always save these so that the debugger can do a backtrace,
  ;; even if the function has no return (and thus never uses them).
  ;; Null only temporarily.
  (old-fp nil :type (or tn null))
  (return-pc nil :type (or tn null))
  ;; The passing location for the RETURN-PC. The return PC is treated
  ;; differently from the other arguments, since in some
  ;; implementations we may use a call instruction that requires the
  ;; return PC to be passed in a particular place.
  (return-pc-pass (missing-arg) :type tn :read-only t)
  ;; True if this function has a frame on the number stack. This is
  ;; set by representation selection whenever it is possible that some
  ;; function in our tail set will make use of the number stack.
  (number-stack-p nil :type boolean)
  ;; a list of all the :ENVIRONMENT TNs live in this environment
  (live-tns nil :type list)
  ;; a list of all the :DEBUG-ENVIRONMENT TNs live in this environment
  (debug-live-tns nil :type list)
  ;; a label that marks the start of elsewhere code for this function,
  ;; or null until this label is assigned by codegen. Used for
  ;; maintaining the debug source map.
  (elsewhere-start nil :type (or label null))
  ;; a label that marks the first location in this function at which
  ;; the environment is properly initialized, i.e. arguments moved
  ;; from their passing locations, etc. This is the start of the
  ;; function as far as the debugger is concerned.
  (environment-start nil :type (or label null))
  (closure-save-tn nil :type (or tn null))
  #!+unwind-to-frame-and-call-vop
  (bsp-save-tn nil :type (or tn null)))

(defprinter (ir2-physenv)
  closure
  old-fp
  return-pc
  return-pc-pass
  closure-save-tn)

;;; A RETURN-INFO is used by GTN to represent the return strategy and
;;; locations for all the functions in a given TAIL-SET. It is stored
;;; in the TAIL-SET-INFO.
(defstruct (return-info (:copier nil))
  ;; The return convention used:
  ;; -- If :UNKNOWN, we use the standard return convention.
  ;; -- If :FIXED, we use the known-values convention.
  (kind (missing-arg) :type (member :fixed :unknown))
  ;; the number of values returned, or :UNKNOWN if we don't know.
  ;; COUNT may be known when KIND is :UNKNOWN, since we may choose the
  ;; standard return convention for other reasons.
  (count (missing-arg) :type (or index (member :unknown)))
  ;; If count isn't :UNKNOWN, then this is a list of the
  ;; primitive-types of each value.
  (types () :type list)
  ;; If kind is :FIXED, then this is the list of the TNs that we
  ;; return the values in.
  (locations () :type list))
(defprinter (return-info)
  kind
  count
  types
  locations)

(defstruct (ir2-nlx-info (:copier nil))
  ;; If the kind is :ENTRY (a lexical exit), then in the home
  ;; environment, this holds a VALUE-CELL object containing the unwind
  ;; block pointer. In the other cases nobody directly references the
  ;; unwind-block, so we leave this slot null.
  (home nil :type (or tn null))
  ;; the saved control stack pointer
  (save-sp (missing-arg) :type tn)
  ;; the list of dynamic state save TNs
  (dynamic-state (list* (make-stack-pointer-tn)
                        (make-dynamic-state-tns))
                 :type list)
  ;; the target label for NLX entry
  (target (gen-label) :type label))
(defprinter (ir2-nlx-info)
  home
  save-sp
  dynamic-state)

;;;; VOPs and templates

;;; A VOP is a Virtual Operation. It represents an operation and the
;;; operands to the operation.
(def!struct (vop (:constructor make-vop (block node info args results))
                 (:copier nil))
  ;; VOP-INFO structure containing static info about the operation
  (info nil :type (or vop-info null))
  ;; the IR2-BLOCK this VOP is in
  (block (missing-arg) :type ir2-block)
  ;; VOPs evaluated after and before this one. Null at the
  ;; beginning/end of the block, and temporarily during IR2
  ;; translation.
  (next nil :type (or vop null))
  (prev nil :type (or vop null))
  ;; heads of the TN-REF lists for operand TNs, linked using the
  ;; ACROSS slot
  (args nil :type (or tn-ref null))
  (results nil :type (or tn-ref null))
  ;; head of the list of write refs for each explicitly allocated
  ;; temporary, linked together using the ACROSS slot
  (temps nil :type (or tn-ref null))
  ;; head of the list of all TN-REFs for references in this VOP,
  ;; linked by the NEXT-REF slot. There will be one entry for each
  ;; operand and two (a read and a write) for each temporary.
  (refs nil :type (or tn-ref null))
  ;; stuff that is passed uninterpreted from IR2 conversion to
  ;; codegen. The meaning of this slot is totally dependent on the VOP.
  codegen-info
  ;; the node that generated this VOP, for keeping track of debug info
  (node nil :type (or node null))
  ;; LOCAL-TN-BIT-VECTOR representing the set of TNs live after args
  ;; are read and before results are written. This is only filled in
  ;; when VOP-INFO-SAVE-P is non-null.
  (save-set nil :type (or local-tn-bit-vector null)))

;;; A TN-REF object contains information about a particular reference
;;; to a TN. The information in TN-REFs largely determines how TNs are
;;; packed.
(def!struct (tn-ref (:constructor make-tn-ref (tn write-p))
                    (:copier nil))
  ;; the TN referenced
  (tn (missing-arg) :type tn)
  ;; Is this is a write reference? (as opposed to a read reference)
  (write-p nil :type boolean)
  ;; the link for a list running through all TN-REFs for this TN of
  ;; the same kind (read or write)
  (next nil :type (or tn-ref null))
  ;; the VOP where the reference happens, or NIL temporarily
  (vop nil :type (or vop null))
  ;; the link for a list of all TN-REFs in VOP, in reverse order of
  ;; reference
  (next-ref nil :type (or tn-ref null))
  ;; the link for a list of the TN-REFs in VOP of the same kind
  ;; (argument, result, temp)
  (across nil :type (or tn-ref null))
  ;; If true, this is a TN-REF also in VOP whose TN we would like
  ;; packed in the same location as our TN. Read and write refs are
  ;; always paired: TARGET in the read points to the write, and
  ;; vice-versa.
  (target nil :type (or null tn-ref))
  ;; the load TN allocated for this operand, if any
  (load-tn nil :type (or tn null)))

;;; A TEMPLATE object represents a particular IR2 coding strategy for
;;; a known function.
(def!struct (template (:constructor nil)
                      #-sb-xc-host (:pure t))
  ;; the symbol name of this VOP. This is used when printing the VOP
  ;; and is also used to provide a handle for definition and
  ;; translation.
  (name nil :type symbol)
  ;; the arg/result type restrictions. We compute this from the
  ;; PRIMITIVE-TYPE restrictions to make life easier for IR1 phases
  ;; that need to anticipate LTN's template selection.
  (type (missing-arg) :type ctype)
  ;; lists of restrictions on the argument and result types. A
  ;; restriction may take several forms:
  ;; -- The restriction * is no restriction at all.
  ;; -- A restriction (:OR <primitive-type>*) means that the operand
  ;;    must have one of the specified primitive types.
  ;; -- A restriction (:CONSTANT <predicate> <type-spec>) means that the
  ;;    argument (not a result) must be a compile-time constant that
  ;;    satisfies the specified predicate function. In this case, the
  ;;    constant value will be passed as an info argument rather than
  ;;    as a normal argument. <type-spec> is a Lisp type specifier for
  ;;    the type tested by the predicate, used when we want to represent
  ;;    the type constraint as a Lisp function type.
  ;;
  ;; If RESULT-TYPES is :CONDITIONAL, then this is an IF-FOO style
  ;; conditional that yields its result as a control transfer. The
  ;; emit function takes two info arguments: the target label and a
  ;; boolean flag indicating whether to negate the sense of the test.
  ;;
  ;; If RESULT-TYPES is a cons whose car is :CONDITIONAL, then this is
  ;; a flag-setting VOP. The rest is a list of condition descriptors to
  ;; be interpreted by the BRANCH-IF VOP (see $ARCH/pred.lisp).
  (arg-types nil :type list)
  (result-types nil :type (or list (member :conditional) (cons (eql :conditional))))
  ;; the primitive type restriction applied to each extra argument or
  ;; result following the fixed operands. If NIL, no extra
  ;; args/results are allowed. Otherwise, either * or a (:OR ...) list
  ;; as described for the {ARG,RESULT}-TYPES.
  (more-args-type nil :type (or (member nil *) cons))
  (more-results-type nil :type (or (member nil *) cons))
  ;; If true, this is a function that is called with no arguments to
  ;; see whether this template can be emitted. This is used to
  ;; conditionally compile for different target hardware
  ;; configuarations (e.g. FP hardware.)
  (guard nil :type (or function null))
  ;; the policy under which this template is the best translation.
  ;; Note that LTN might use this template under other policies if it
  ;; can't figure out anything better to do.
  (ltn-policy (missing-arg) :type ltn-policy)
  ;; the base cost for this template, given optimistic assumptions
  ;; such as no operand loading, etc.
  (cost (missing-arg) :type index)
  ;; If true, then this is a short noun-like phrase describing what
  ;; this VOP "does", i.e. the implementation strategy. This is for
  ;; use in efficiency notes.
  (note nil :type (or string null))
  ;; the number of trailing arguments to VOP or %PRIMITIVE that we
  ;; bundle into a list and pass into the emit function. This provides
  ;; a way to pass uninterpreted stuff directly to the code generator.
  (info-arg-count 0 :type index))
(defprinter (template)
  name
  arg-types
  result-types
  (more-args-type :test more-args-type :prin1 more-args-type)
  (more-results-type :test more-results-type :prin1 more-results-type)
  ltn-policy
  cost
  (note :test note)
  (info-arg-count :test (not (zerop info-arg-count))))

;;; A VOP-INFO object holds the constant information for a given
;;; virtual operation. We include TEMPLATE so that functions with a
;;; direct VOP equivalent can be translated easily.
(def!struct (vop-info
             (:include template)
             (:make-load-form-fun ignore-it))
  ;; side effects of this VOP and side effects that affect the value
  ;; of this VOP
  (effects (missing-arg) :type attributes)
  (affected (missing-arg) :type attributes)
  ;; If true, causes special casing of TNs live after this VOP that
  ;; aren't results:
  ;; -- If T, all such TNs that are allocated in a SC with a defined
  ;;    save-sc will be saved in a TN in the save SC before the VOP
  ;;    and restored after the VOP. This is used by call VOPs. A bit
  ;;    vector representing the live TNs is stored in the VOP-SAVE-SET.
  ;; -- If :FORCE-TO-STACK, all such TNs will made into :ENVIRONMENT TNs
  ;;    and forced to be allocated in SCs without any save-sc. This is
  ;;    used by NLX entry vops.
  ;; -- If :COMPUTE-ONLY, just compute the save set, don't do any saving.
  ;;    This is used to get the live variables for debug info.
  (save-p nil :type (member t nil :force-to-stack :compute-only))
  ;; info for automatic emission of move-arg VOPs by representation
  ;; selection. If NIL, then do nothing special. If non-null, then
  ;; there must be a more arg. Each more arg is moved to its passing
  ;; location using the appropriate representation-specific MOVE-ARG
  ;; VOP. The first (fixed) argument must be the control-stack frame
  ;; pointer for the frame to move into. The first info arg is the
  ;; list of passing locations.
  ;;
  ;; Additional constraints depend on the value:
  ;;
  ;; :FULL-CALL
  ;;     None.
  ;;
  ;; :LOCAL-CALL
  ;;     The second (fixed) arg is the NFP for the called function (from
  ;;     ALLOCATE-FRAME.)
  ;;
  ;; :KNOWN-RETURN
  ;;     If needed, the old NFP is computed using COMPUTE-OLD-NFP.
  (move-args nil :type (member nil :full-call :local-call :known-return))
  ;; a list of sc-vectors representing the loading costs of each fixed
  ;; argument and result
  (arg-costs nil :type list)
  (result-costs nil :type list)
  ;; if true, SC-VECTORs representing the loading costs for any more
  ;; args and results
  (more-arg-costs nil :type (or sc-vector null))
  (more-result-costs nil :type (or sc-vector null))
  ;; lists of SC-VECTORs mapping each SC to the SCs that we can load
  ;; into. If a SC is directly acceptable to the VOP, then the entry
  ;; is T. Otherwise, it is a list of the SC numbers of all the SCs
  ;; that we can load into. This list will be empty if there is no
  ;; load function which loads from that SC to an SC allowed by the
  ;; operand SC restriction.
  (arg-load-scs nil :type list)
  (result-load-scs nil :type list)
  ;; a function that emits assembly code for a use of this VOP when it
  ;; is called with the VOP structure. This is null if this VOP has no
  ;; specified generator (i.e. if it exists only to be inherited by
  ;; other VOPs).
  (generator-function nil :type (or function null))
  ;; a list of things that are used to parameterize an inherited
  ;; generator. This allows the same generator function to be used for
  ;; a group of VOPs with similar implementations.
  (variant nil :type list)
  ;; the number of arguments and results. Each regular arg/result
  ;; counts as one, and all the more args/results together count as 1.
  (num-args 0 :type index)
  (num-results 0 :type index)
  ;; a vector of the temporaries the vop needs. See EMIT-VOP
  ;; in vmdef for information on how the temps are encoded.
  (temps nil :type (or null (simple-array (unsigned-byte 16) 1)))
  ;; the order all the refs for this vop should be put in. Each
  ;; operand is assigned a number in the following ordering: args,
  ;; more-args, results, more-results, temps. This vector represents
  ;; the order the operands should be put into in the next-ref link.
  (ref-ordering nil :type (or null (simple-array (unsigned-byte 8) 1)))
  ;; a vector of the various targets that should be done. Each element
  ;; encodes the source ref (shifted 8, it is also encoded in
  ;; MAX-VOP-TN-REFS) and the dest ref index.
  (targets nil :type (or null (simple-array (unsigned-byte 16) 1))))

;; These printers follow the definition of VOP-INFO because they
;; want to inline VOP-INFO-NAME, and it's less code to move them here
;; than to move the defstructs of VOP-INFO and TEMPLATE.
(defprinter (vop)
  (info :prin1 (vop-info-name info))
  args
  results
  (codegen-info :test codegen-info))
(defprinter (tn-ref)
  tn
  write-p
  (vop :test vop :prin1 (vop-info-name (vop-info vop))))


;;;; SBs and SCs

;;; copied from docs/internals/retargeting.tex by WHN 19990707:
;;;
;;; A Storage Base represents a physical storage resource such as a
;;; register set or stack frame. Storage bases for non-global
;;; resources such as the stack are relativized by the environment
;;; that the TN is allocated in. Packing conflict information is kept
;;; in the storage base, but non-packed storage resources such as
;;; closure environments also have storage bases.
;;;
;;; Some storage bases:
;;;     General purpose registers
;;;     Floating point registers
;;;     Boxed (control) stack environment
;;;     Unboxed (number) stack environment
;;;     Closure environment
;;;
;;; A storage class is a potentially arbitrary set of the elements in
;;; a storage base. Although conceptually there may be a hierarchy of
;;; storage classes such as "all registers", "boxed registers", "boxed
;;; scratch registers", this doesn't exist at the implementation
;;; level. Such things can be done by specifying storage classes whose
;;; locations overlap. A TN shouldn't have lots of overlapping SC's as
;;; legal SC's, since time would be wasted repeatedly attempting to
;;; pack in the same locations.
;;;
;;; ...
;;;
;;; Some SCs:
;;;     Reg: any register (immediate objects)
;;;     Save-Reg: a boxed register near r15 (registers easily saved in a call)
;;;     Boxed-Reg: any boxed register (any boxed object)
;;;     Unboxed-Reg: any unboxed register (any unboxed object)
;;;     Float-Reg, Double-Float-Reg: float in FP register.
;;;     Stack: boxed object on the stack (on control stack)
;;;     Word: any 32bit unboxed object on nstack.
;;;     Double: any 64bit unboxed object on nstack.

;;; The SB structure represents the global information associated with
;;; a storage base.
(def!struct (sb (:make-load-form-fun just-dump-it-normally))
  ;; name, for printing and reference
  (name nil :type symbol)
  ;; the kind of storage base (which determines the packing
  ;; algorithm)
  (kind :non-packed :type (member :finite :unbounded :non-packed))
  ;; the number of elements in the SB. If finite, this is the total
  ;; size. If unbounded, this is the size that the SB is initially
  ;; allocated at.
  (size 0 :type index))
(defprinter (sb)
  name)

;;; A FINITE-SB holds information needed by the packing algorithm for
;;; finite SBs.
(def!struct (finite-sb (:include sb))
  ;; the minimum number of location by which to grow this SB
  ;; if it is :unbounded
  (size-increment 1 :type index)
  ;; current-size must always be a multiple of this. It is assumed
  ;; to be a power of two.
  (size-alignment 1 :type index)
  ;; the number of locations currently allocated in this SB
  (current-size 0 :type index)
  ;; the last location packed in, used by pack to scatter TNs to
  ;; prevent a few locations from getting all the TNs, and thus
  ;; getting overcrowded, reducing the possibilities for targeting.
  (last-offset 0 :type index)
  ;; a vector containing, for each location in this SB, a vector
  ;; indexed by IR2 block numbers, holding local conflict bit vectors.
  ;; A TN must not be packed in a given location within a particular
  ;; block if the LTN number for that TN in that block corresponds to
  ;; a set bit in the bit-vector.
  (conflicts '#() :type simple-vector)
  ;; a vector containing, for each location in this SB, a bit-vector
  ;; indexed by IR2 block numbers. If the bit corresponding to a block
  ;; is set, then the location is in use somewhere in the block, and
  ;; thus has a conflict for always-live TNs.
  (always-live '#() :type simple-vector)
  (always-live-count '#() :type simple-vector)
  ;; a vector containing the TN currently live in each location in the
  ;; SB, or NIL if the location is unused. This is used during load-tn pack.
  (live-tns '#() :type simple-vector)
  ;; the number of blocks for which the ALWAYS-LIVE and CONFLICTS
  ;; might not be virgin, and thus must be reinitialized when PACK
  ;; starts. Less then the length of those vectors when not all of the
  ;; length was used on the previously packed component.
  (last-block-count 0 :type index))

;;; the SC structure holds the storage base that storage is allocated
;;; in and information used to select locations within the SB
(def!struct (sc (:copier nil))
  ;; name, for printing and reference
  (name nil :type symbol)
  ;; the number used to index SC cost vectors
  (number 0 :type sc-number)
  ;; the storage base that this SC allocates storage from
  (sb nil :type (or sb null))
  ;; the size of elements in this SC, in units of locations in the SB
  (element-size 0 :type index)
  ;; if our SB is finite, a list of the locations in this SC
  (locations nil :type list)
  ;; a list of the alternate (save) SCs for this SC
  (alternate-scs nil :type list)
  ;; a list of the constant SCs that can me moved into this SC
  (constant-scs nil :type list)
  ;; true if the values in this SC needs to be saved across calls
  (save-p nil :type boolean)
  ;; vectors mapping from SC numbers to information about how to load
  ;; from the index SC to this one. MOVE-FUNS holds the names of
  ;; the functions used to do loading, and LOAD-COSTS holds the cost
  ;; of the corresponding move functions. If loading is impossible,
  ;; then the entries are NIL. LOAD-COSTS is initialized to have a 0
  ;; for this SC.
  (move-funs (make-array sc-number-limit :initial-element nil)
             :type sc-vector)
  (load-costs (make-array sc-number-limit :initial-element nil)
              :type sc-vector)
  ;; a vector mapping from SC numbers to possibly
  ;; representation-specific move and coerce VOPs. Each entry is a
  ;; list of VOP-INFOs for VOPs that move/coerce an object in the
  ;; index SC's representation into this SC's representation. This
  ;; vector is filled out with entries for all SCs that can somehow be
  ;; coerced into this SC, not just those VOPs defined to directly
  ;; move into this SC (i.e. it allows for operand loading on the move
  ;; VOP's operands.)
  ;;
  ;; When there are multiple applicable VOPs, the template arg and
  ;; result type restrictions are used to determine which one to use.
  ;; The list is sorted by increasing cost, so the first applicable
  ;; VOP should be used.
  ;;
  ;; Move (or move-arg) VOPs with descriptor results shouldn't have
  ;; TNs wired in the standard argument registers, since there may
  ;; already be live TNs wired in those locations holding the values
  ;; that we are setting up for unknown-values return.
  (move-vops (make-array sc-number-limit :initial-element nil)
             :type sc-vector)
  ;; the costs corresponding to the MOVE-VOPS. Separate because this
  ;; info is needed at meta-compile time, while the MOVE-VOPs don't
  ;; exist till load time. If no move is defined, then the entry is
  ;; NIL.
  (move-costs (make-array sc-number-limit :initial-element nil)
              :type sc-vector)
  ;; similar to Move-VOPs, except that we only ever use the entries
  ;; for this SC and its alternates, since we never combine complex
  ;; representation conversion with argument passing.
  (move-arg-vops (make-array sc-number-limit :initial-element nil)
                 :type sc-vector)
  ;; true if this SC or one of its alternates in in the NUMBER-STACK SB.
  (number-stack-p nil :type boolean)
  ;; alignment restriction. The offset must be an even multiple of this.
  ;; this must be a power of two.
  (alignment 1 :type (and index (integer 1)))
  ;; a list of locations that we avoid packing in during normal
  ;; register allocation to ensure that these locations will be free
  ;; for operand loading. This prevents load-TN packing from thrashing
  ;; by spilling a lot.
  (reserve-locations nil :type list))
(defprinter (sc)
  name)

;;;; TNs

(def!struct (tn (:include sset-element)
               (:constructor make-random-tn)
               (:constructor make-tn (number kind primitive-type sc))
               (:copier nil))
  ;; The kind of TN this is:
  ;;
  ;;   :NORMAL
  ;;    A normal, non-constant TN, representing a variable or temporary.
  ;;    Lifetime information is computed so that packing can be done.
  ;;
  ;;   :ENVIRONMENT
  ;;    A TN that has hidden references (debugger or NLX), and thus must be
  ;;    allocated for the duration of the environment it is referenced in.
  ;;
  ;;   :DEBUG-ENVIRONMENT
  ;;    Like :ENVIRONMENT, but is used for TNs that we want to be able to
  ;;    target to/from and that don't absolutely have to be live
  ;;    everywhere. These TNs are live in all blocks in the environment
  ;;    that don't reference this TN.
  ;;
  ;;   :COMPONENT
  ;;    A TN that implicitly conflicts with all other TNs. No conflict
  ;;    info is computed.
  ;;
  ;;   :SAVE
  ;;   :SAVE-ONCE
  ;;    A TN used for saving a :NORMAL TN across function calls. The
  ;;    lifetime information slots are unitialized: get the original
  ;;    TN out of the SAVE-TN slot and use it for conflicts. SAVE-ONCE
  ;;    is like :SAVE, except that it is only save once at the single
  ;;    writer of the original TN.
  ;;
  ;;   :SPECIFIED-SAVE
  ;;    A TN that was explicitly specified as the save TN for another TN.
  ;;    When we actually get around to doing the saving, this will be
  ;;    changed to :SAVE or :SAVE-ONCE.
  ;;
  ;;   :LOAD
  ;;    A load-TN used to compute an argument or result that is
  ;;    restricted to some finite SB. Load TNs don't have any conflict
  ;;    information. Load TN pack uses a special local conflict
  ;;    determination method.
  ;;
  ;;   :CONSTANT
  ;;    Represents a constant, with TN-LEAF a CONSTANT leaf. Lifetime
  ;;    information isn't computed, since the value isn't allocated by
  ;;    pack, but is instead generated as a load at each use. Since
  ;;    lifetime analysis isn't done on :CONSTANT TNs, they don't have
  ;;    LOCAL-NUMBERs and similar stuff.
  ;;
  ;;   :ALIAS
  ;;    A special kind of TN used to represent initialization of local
  ;;    call arguments in the caller. It provides another name for the
  ;;    argument TN so that lifetime analysis doesn't get confused by
  ;;    self-recursive calls. Lifetime analysis treats this the same
  ;;    as :NORMAL, but then at the end merges the conflict info into
  ;;    the original TN and replaces all uses of the alias with the
  ;;    original TN. SAVE-TN holds the aliased TN.
  (kind (missing-arg)
        :type (member :normal :environment :debug-environment
                      :save :save-once :specified-save :load :constant
                      :component :alias))
  ;; the primitive-type for this TN's value. Null in restricted or
  ;; wired TNs.
  (primitive-type nil :type (or primitive-type null))
  ;; If this TN represents a variable or constant, then this is the
  ;; corresponding LEAF.
  (leaf nil :type (or leaf null))
  ;; thread that links TNs together so that we can find them
  (next nil :type (or tn null))
  ;; head of TN-REF lists for reads and writes of this TN
  (reads nil :type (or tn-ref null))
  (writes nil :type (or tn-ref null))
  ;; a link we use when building various temporary TN lists
  (next* nil :type (or tn null))
  ;; some block that contains a reference to this TN, or NIL if we
  ;; haven't seen any reference yet. If the TN is local, then this is
  ;; the block it is local to.
  (local nil :type (or ir2-block null))
  ;; If a local TN, the block relative number for this TN. Global TNs
  ;; whose liveness changes within a block are also assigned a local
  ;; number during the conflicts analysis of that block. If the TN has
  ;; no local number within the block, then this is NIL.
  (local-number nil :type (or local-tn-number null))
  ;; If this object is a local TN, this slot is a bit-vector with 1
  ;; for the local-number of every TN that we conflict with.
  (local-conflicts (make-array local-tn-limit
                               :element-type 'bit
                               :initial-element 0)
                   :type local-tn-bit-vector)
  ;; head of the list of GLOBAL-CONFLICTS structures for a global TN.
  ;; This list is sorted by block number (i.e. reverse DFO), allowing
  ;; the intersection between the lifetimes for two global TNs to be
  ;; easily found. If null, then this TN is a local TN.
  ;; KLUDGE: The defstructs for TN and GLOBAL-CONFLICTS are mutually
  ;; referential, and absent a block-compilation feature, one or the other
  ;; of the structures can't inline type checks for its referent.
  ;; Stating this type as (OR NULL GLOBAL-CONFLICTS) instead of the reverse
  ;; avoids a forward-reference that would crash cold-init.
  ;; But since TYPEP short-circuits, the constructor is happy with NIL.
  ;; [See the comments in LAYOUT-OF for further detail]
  (global-conflicts nil :type (or null global-conflicts))
  ;; During lifetime analysis, this is used as a pointer into the
  ;; conflicts chain, for scanning through blocks in reverse DFO.
  (current-conflict nil)
  ;; In a :SAVE TN, this is the TN saved. In a :NORMAL or :ENVIRONMENT
  ;; TN, this is the associated save TN. In TNs with no save TN, this
  ;; is null.
  (save-tn nil :type (or tn null))
  ;; After pack, the SC we packed into. Beforehand, the SC we want to
  ;; pack into, or null if we don't know.
  (sc nil :type (or sc null))
  ;; the offset within the SB that this TN is packed into. This is what
  ;; indicates that the TN is packed
  (offset nil :type (or index null))
  ;; some kind of info about how important this TN is
  (cost 0 :type fixnum)
  ;; If a :ENVIRONMENT or :DEBUG-ENVIRONMENT TN, this is the
  ;; physical environment that the TN is live throughout.
  (physenv nil :type (or physenv null))
  ;; The depth of the deepest loop that this TN is used in.
  (loop-depth 0 :type fixnum))
(declaim (freeze-type tn))
(def!method print-object ((tn tn) stream)
  (print-unreadable-object (tn stream :type t)
    ;; KLUDGE: The distinction between PRINT-TN and PRINT-OBJECT on TN is
    ;; not very mnemonic. -- WHN 20000124
    (print-tn-guts tn stream)))

;;; The GLOBAL-CONFLICTS structure represents the conflicts for global
;;; TNs. Each global TN has a list of these structures, one for each
;;; block that it is live in. In addition to representing the result of
;;; lifetime analysis, the global conflicts structure is used during
;;; lifetime analysis to represent the set of TNs live at the start of
;;; the IR2 block.
(def!struct (global-conflicts
            (:constructor make-global-conflicts (kind tn block number))
            (:copier nil))
  ;; the IR2-BLOCK that this structure represents the conflicts for
  (block (missing-arg) :type ir2-block)
  ;; thread running through all the GLOBAL-CONFLICTSs for BLOCK. This
  ;; thread is sorted by TN number
  (next-blockwise nil :type (or global-conflicts null))
  ;; the way that TN is used by BLOCK
  ;;
  ;;   :READ
  ;;     The TN is read before it is written. It starts the block live,
  ;;     but is written within the block.
  ;;
  ;;   :WRITE
  ;;     The TN is written before any read. It starts the block dead,
  ;;     and need not have a read within the block.
  ;;
  ;;   :READ-ONLY
  ;;     The TN is read, but never written. It starts the block live,
  ;;     and is not killed by the block. Lifetime analysis will promote
  ;;     :READ-ONLY TNs to :LIVE if they are live at the block end.
  ;;
  ;;   :LIVE
  ;;     The TN is not referenced. It is live everywhere in the block.
  (kind :read-only :type (member :read :write :read-only :live))
  ;; a local conflicts vector representing conflicts with TNs live in
  ;; BLOCK. The index for the local TN number of each TN we conflict
  ;; with in this block is 1. To find the full conflict set, the :LIVE
  ;; TNs for BLOCK must also be included. This slot is not meaningful
  ;; when KIND is :LIVE.
  (conflicts (make-array local-tn-limit
                         :element-type 'bit
                         :initial-element 0)
             :type local-tn-bit-vector)
  ;; the TN we are recording conflicts for.
  (tn (missing-arg) :type tn)
  ;; thread through all the GLOBAL-CONFLICTSs for TN
  (next-tnwise nil :type (or global-conflicts null))
  ;; TN's local TN number in BLOCK. :LIVE TNs don't have local numbers.
  (number nil :type (or local-tn-number null)))
(defprinter (global-conflicts)
  tn
  block
  kind
  (number :test number))
