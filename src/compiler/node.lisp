;;;; structures for the first intermediate representation in the
;;;; compiler, IR1

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; The front-end data structure (IR1) is composed of nodes and
;;; continuations. The general idea is that continuations contain
;;; top-down information and nodes contain bottom-up, derived
;;; information. A continuation represents a place in the code, while
;;; a node represents code that does something.
;;;
;;; This representation is more of a flow-graph than an augmented
;;; syntax tree. The evaluation order is explicitly represented in the
;;; linkage by continuations, rather than being implicit in the nodes
;;; which receive the the results of evaluation. This allows us to
;;; decouple the flow of results from the flow of control. A
;;; continuation represents both, but the continuation can represent
;;; the case of a discarded result by having no DEST.

(def!struct (continuation
	     (:make-load-form-fun ignore-it)
	     (:constructor make-continuation (&optional dest)))
  ;; An indication of the way that this continuation is currently used:
  ;;
  ;; :UNUSED
  ;;	A continuation for which all control-related slots have the default
  ;;	values. A continuation is unused during IR1 conversion until it is
  ;;	assigned a block, and may be also be temporarily unused during
  ;;	later manipulations of IR1. In a consistent state there should
  ;;	never be any mention of :UNUSED continuations. Next can have a
  ;;	non-null value if the next node has already been determined.
  ;;
  ;; :DELETED
  ;;	A continuation that has been deleted from IR1. Any pointers into
  ;;	IR1 are cleared. There are two conditions under which a deleted
  ;;	continuation may appear in code:
  ;;	 -- The CONT of the LAST node in a block may be a deleted
  ;;	    continuation when the original receiver of the continuation's
  ;;	    value was deleted. Note that DEST in a deleted continuation is
  ;;	    null, so it is easy to know not to attempt delivering any
  ;;	    values to the continuation.
  ;;	 -- Unreachable code that hasn't been deleted yet may receive
  ;;	    deleted continuations. All such code will be in blocks that
  ;;	    have DELETE-P set. All unreachable code is deleted by control
  ;;	    optimization, so the backend doesn't have to worry about this.
  ;;
  ;; :BLOCK-START
  ;;	The continuation that is the START of BLOCK. This is the only kind
  ;;	of continuation that can have more than one use. The BLOCK's
  ;;	START-USES is a list of all the uses.
  ;;
  ;; :DELETED-BLOCK-START
  ;;	Like :BLOCK-START, but BLOCK has been deleted. A block starting
  ;;	continuation is made into a deleted block start when the block is
  ;;	deleted, but the continuation still may have value semantics.
  ;;	Since there isn't any code left, next is null.
  ;;
  ;; :INSIDE-BLOCK
  ;;	A continuation that is the CONT of some node in BLOCK.
  (kind :unused :type (member :unused :deleted :inside-block :block-start
			      :deleted-block-start))
  ;; The node which receives this value, if any. In a deleted continuation,
  ;; this is null even though the node that receives this continuation may not
  ;; yet be deleted.
  (dest nil :type (or node null))
  ;; If this is a NODE, then it is the node which is to be evaluated next.
  ;; This is always null in :DELETED and :UNUSED continuations, and will be
  ;; null in a :INSIDE-BLOCK continuation when this is the CONT of the LAST.
  (next nil :type (or node null))
  ;; An assertion on the type of this continuation's value.
  (asserted-type *wild-type* :type ctype)
  ;; Cached type of this continuation's value. If NIL, then this must be
  ;; recomputed: see CONTINUATION-DERIVED-TYPE.
  (%derived-type nil :type (or ctype null))
  ;; Node where this continuation is used, if unique. This is always null in
  ;; :DELETED and :UNUSED continuations, and is never null in :INSIDE-BLOCK
  ;; continuations. In a :BLOCK-START continuation, the Block's START-USES
  ;; indicate whether NIL means no uses or more than one use.
  (use nil :type (or node null))
  ;; Basic block this continuation is in. This is null only in :DELETED and
  ;; :UNUSED continuations. Note that blocks that are unreachable but still in
  ;; the DFO may receive deleted continuations, so it isn't o.k. to assume that
  ;; any continuation that you pick up out of its DEST node has a BLOCK.
  (block nil :type (or cblock null))
  ;; Set to true when something about this continuation's value has changed.
  ;; See REOPTIMIZE-CONTINUATION. This provides a way for IR1 optimize to
  ;; determine which operands to a node have changed. If the optimizer for
  ;; this node type doesn't care, it can elect not to clear this flag.
  (reoptimize t :type boolean)
  ;; An indication of what we have proven about how this contination's type
  ;; assertion is satisfied:
  ;;
  ;; NIL
  ;;    No type check is necessary (proven type is a subtype of the assertion.)
  ;;
  ;; T
  ;;    A type check is needed.
  ;;
  ;; :DELETED
  ;;    Don't do a type check, but believe (intersect) the assertion. A T
  ;;    check can be changed to :DELETED if we somehow prove the check is
  ;;    unnecessary, or if we eliminate it through a policy decision.
  ;;
  ;; :NO-CHECK
  ;;    Type check generation sets the slot to this if a check is called for,
  ;;    but it believes it has proven that the check won't be done for
  ;;    policy reasons or because a safe implementation will be used. In the
  ;;    latter case, LTN must ensure that a safe implementation *is* be used.
  ;;
  ;; :ERROR
  ;;    There is a compile-time type error in some use of this continuation. A
  ;;    type check should still be generated, but be careful.
  ;;
  ;; This is computed lazily by CONTINUATION-DERIVED-TYPE, so use
  ;; CONTINUATION-TYPE-CHECK instead of the %'ed slot accessor.
  (%type-check t :type (member t nil :deleted :no-check :error))
  ;; Something or other that the back end annotates this continuation with.
  (info nil))
(def!method print-object ((x continuation) stream)
  (print-unreadable-object (x stream :type t :identity t)))

(defstruct (node (:constructor nil))
  ;; The bottom-up derived type for this node. This does not take into
  ;; consideration output type assertions on this node (actually on its CONT).
  (derived-type *wild-type* :type ctype)
  ;; True if this node needs to be optimized. This is set to true whenever
  ;; something changes about the value of a continuation whose DEST is this
  ;; node.
  (reoptimize t :type boolean)
  ;; The continuation which receives the value of this node. This also
  ;; indicates what we do controlwise after evaluating this node. This may be
  ;; null during IR1 conversion.
  (cont nil :type (or continuation null))
  ;; The continuation that this node is the next of. This is null during
  ;; IR1 conversion when we haven't linked the node in yet or in nodes that
  ;; have been deleted from the IR1 by UNLINK-NODE.
  (prev nil :type (or continuation null))
  ;; The lexical environment this node was converted in.
  (lexenv *lexenv* :type lexenv)
  ;; A representation of the source code responsible for generating this node.
  ;;
  ;; For a form introduced by compilation (does not appear in the original
  ;; source), the path begins with a list of all the enclosing introduced
  ;; forms. This list is from the inside out, with the form immediately
  ;; responsible for this node at the head of the list.
  ;;
  ;; Following the introduced forms is a representation of the location of the
  ;; enclosing original source form. This transition is indicated by the magic
  ;; ORIGINAL-SOURCE-START marker. The first element of the orignal source is
  ;; the "form number", which is the ordinal number of this form in a
  ;; depth-first, left-to-right walk of the truly top-level form in which this
  ;; appears.
  ;;
  ;; Following is a list of integers describing the path taken through the
  ;; source to get to this point:
  ;;     (K L M ...) => (NTH K (NTH L (NTH M ...)))
  ;;
  ;; The last element in the list is the top-level form number, which is the
  ;; ordinal number (in this call to the compiler) of the truly top-level form
  ;; containing the orignal source.
  (source-path *current-path* :type list)
  ;; If this node is in a tail-recursive position, then this is set to T. At
  ;; the end of IR1 (in environment analysis) this is computed for all nodes
  ;; (after cleanup code has been emitted). Before then, a non-null value
  ;; indicates that IR1 optimization has converted a tail local call to a
  ;; direct transfer.
  ;;
  ;; If the back-end breaks tail-recursion for some reason, then it can null
  ;; out this slot.
  (tail-p nil :type boolean))

;;; Flags that are used to indicate various things about a block, such as what
;;; optimizations need to be done on it:
;;; -- REOPTIMIZE is set when something interesting happens the uses of a
;;;    continuation whose Dest is in this block. This indicates that the
;;;    value-driven (forward) IR1 optimizations should be done on this block.
;;; -- FLUSH-P is set when code in this block becomes potentially flushable,
;;;    usually due to a continuation's DEST becoming null.
;;; -- TYPE-CHECK is true when the type check phase should be run on this
;;;    block. IR1 optimize can introduce new blocks after type check has
;;;    already run. We need to check these blocks, but there is no point in
;;;    checking blocks we have already checked.
;;; -- DELETE-P is true when this block is used to indicate that this block
;;;    has been determined to be unreachable and should be deleted. IR1
;;;    phases should not attempt to  examine or modify blocks with DELETE-P
;;;    set, since they may:
;;;     - be in the process of being deleted, or
;;;     - have no successors, or
;;;     - receive :DELETED continuations.
;;; -- TYPE-ASSERTED, TEST-MODIFIED
;;;    These flags are used to indicate that something in this block might be
;;;    of interest to constraint propagation. TYPE-ASSERTED is set when a
;;;    continuation type assertion is strengthened. TEST-MODIFIED is set
;;;    whenever the test for the ending IF has changed (may be true when there
;;;    is no IF.)
(def-boolean-attribute block
  reoptimize flush-p type-check delete-p type-asserted test-modified)

(macrolet ((frob (slot)
	     `(defmacro ,(symbolicate "BLOCK-" slot) (block)
		`(block-attributep (block-flags ,block) ,',slot))))
  (frob reoptimize)
  (frob flush-p)
  (frob type-check)
  (frob delete-p)
  (frob type-asserted)
  (frob test-modified))

;;; The CBLOCK structure represents a basic block. We include SSET-ELEMENT so
;;; that we can have sets of blocks. Initially the SSET-ELEMENT-NUMBER is
;;; null, DFO analysis numbers in reverse DFO. During IR2 conversion, IR1
;;; blocks are re-numbered in forward emit order. This latter numbering also
;;; forms the basis of the block numbering in the debug-info (though that is
;;; relative to the start of the function.)
(defstruct (cblock (:include sset-element)
		   (:constructor make-block (start))
		   (:constructor make-block-key)
		   (:conc-name block-)
		   (:predicate block-p)
		   (:copier copy-block))
  ;; A list of all the blocks that are predecessors/successors of this block.
  ;; In well-formed IR1, most blocks will have one successor. The only
  ;; exceptions are:
  ;;  1. component head blocks (any number)
  ;;  2. blocks ending in an IF (1 or 2)
  ;;  3. blocks with DELETE-P set (zero)
  (pred nil :type list)
  (succ nil :type list)
  ;; The continuation which heads this block (either a :Block-Start or
  ;; :Deleted-Block-Start.)  Null when we haven't made the start continuation
  ;; yet (and in the dummy component head and tail blocks.)
  (start nil :type (or continuation null))
  ;; A list of all the nodes that have Start as their Cont.
  (start-uses nil :type list)
  ;; The last node in this block. This is null when we are in the process of
  ;; building a block (and in the dummy component head and tail blocks.)
  (last nil :type (or node null))
  ;; The forward and backward links in the depth-first ordering of the blocks.
  ;; These slots are null at beginning/end.
  (next nil :type (or null cblock))
  (prev nil :type (or null cblock))
  ;; This block's attributes: see above.
  (flags (block-attributes reoptimize flush-p type-check type-asserted
			   test-modified)
	 :type attributes)
  ;; Some sets used by constraint propagation.
  (kill nil)
  (gen nil)
  (in nil)
  (out nil)
  ;; The component this block is in. Null temporarily during IR1 conversion
  ;; and in deleted blocks.
  (component *current-component* :type (or component null))
  ;; A flag used by various graph-walking code to determine whether this block
  ;; has been processed already or what. We make this initially NIL so that
  ;; Find-Initial-DFO doesn't have to scan the entire initial component just to
  ;; clear the flags.
  (flag nil)
  ;; Some kind of info used by the back end.
  (info nil)
  ;; If true, then constraints that hold in this block and its successors by
  ;; merit of being tested by its IF predecessor.
  (test-constraint nil :type (or sset null)))
(def!method print-object ((cblock cblock) stream)
  (print-unreadable-object (cblock stream :type t :identity t)
    (format stream ":START c~D" (cont-num (block-start cblock)))))

;;; The Block-Annotation structure is shared (via :include) by different
;;; block-info annotation structures so that code (specifically control
;;; analysis) can be shared.
(defstruct (block-annotation (:constructor nil))
  ;; The IR1 block that this block is in the Info for.
  (block (required-argument) :type cblock)
  ;; The next and previous block in emission order (not DFO). This determines
  ;; which block we drop though to, and also used to chain together overflow
  ;; blocks that result from splitting of IR2 blocks in lifetime analysis.
  (next nil :type (or block-annotation null))
  (prev nil :type (or block-annotation null)))

;;; The Component structure provides a handle on a connected piece of the flow
;;; graph. Most of the passes in the compiler operate on components rather
;;; than on the entire flow graph.
(defstruct component
  ;; The kind of component:
  ;;
  ;; NIL
  ;;     An ordinary component, containing non-top-level code.
  ;;
  ;; :Top-Level
  ;;     A component containing only load-time code.
  ;;
  ;; :Complex-Top-Level
  ;;     A component containing both top-level and run-time code.
  ;;
  ;; :Initial
  ;;     The result of initial IR1 conversion, on which component analysis has
  ;;     not been done.
  ;;
  ;; :Deleted
  ;;     Debris left over from component analysis.
  (kind nil :type (member nil :top-level :complex-top-level :initial :deleted))
  ;; The blocks that are the dummy head and tail of the DFO.
  ;; Entry/exit points have these blocks as their
  ;; predecessors/successors. Null temporarily. The start and return
  ;; from each non-deleted function is linked to the component head
  ;; and tail. Until environment analysis links NLX entry stubs to the
  ;; component head, every successor of the head is a function start
  ;; (i.e. begins with a Bind node.)
  (head nil :type (or null cblock))
  (tail nil :type (or null cblock))
  ;; A list of the CLambda structures for all functions in this
  ;; component. Optional-Dispatches are represented only by their XEP
  ;; and other associated lambdas. This doesn't contain any deleted or
  ;; let lambdas.
  (lambdas () :type list)
  ;; A list of Functional structures for functions that are newly
  ;; converted, and haven't been local-call analyzed yet. Initially
  ;; functions are not in the Lambdas list. LOCAL-CALL-ANALYZE moves
  ;; them there (possibly as LETs, or implicitly as XEPs if an
  ;; OPTIONAL-DISPATCH.) Between runs of LOCAL-CALL-ANALYZE there may
  ;; be some debris of converted or even deleted functions in this
  ;; list.
  (new-functions () :type list)
  ;; If true, then there is stuff in this component that could benefit
  ;; from further IR1 optimization.
  (reoptimize t :type boolean)
  ;; If true, then the control flow in this component was messed up by
  ;; IR1 optimizations. The DFO should be recomputed.
  (reanalyze nil :type boolean)
  ;; String that is some sort of name for the code in this component.
  (name "<unknown>" :type simple-string)
  ;; Some kind of info used by the back end.
  (info nil)
  ;; The Source-Info structure describing where this component was
  ;; compiled from.
  (source-info *source-info* :type source-info)
  ;; Count of the number of inline expansions we have done while
  ;; compiling this component, to detect infinite or exponential
  ;; blowups.
  (inline-expansions 0 :type index)
  ;; A hashtable from combination nodes to things describing how an
  ;; optimization of the node failed. The value is an alist (Transform
  ;; . Args), where Transform is the structure describing the
  ;; transform that failed, and Args is either a list of format
  ;; arguments for the note, or the FUNCTION-TYPE that would have
  ;; enabled the transformation but failed to match.
  (failed-optimizations (make-hash-table :test 'eq) :type hash-table)
  ;; Similar to NEW-FUNCTIONS, but is used when a function has already
  ;; been analyzed, but new references have been added by inline
  ;; expansion. Unlike NEW-FUNCTIONS, this is not disjoint from
  ;; COMPONENT-LAMBDAS.
  (reanalyze-functions nil :type list))
(defprinter (component)
  name
  (reanalyze :test reanalyze))

;;; The Cleanup structure represents some dynamic binding action.
;;; Blocks are annotated with the current cleanup so that dynamic
;;; bindings can be removed when control is transferred out of the
;;; binding environment. We arrange for changes in dynamic bindings to
;;; happen at block boundaries, so that cleanup code may easily be
;;; inserted. The "mess-up" action is explicitly represented by a
;;; funny function call or Entry node.
;;;
;;; We guarantee that cleanups only need to be done at block boundaries
;;; by requiring that the exit continuations initially head their
;;; blocks, and then by not merging blocks when there is a cleanup
;;; change.
(defstruct cleanup
  ;; The kind of thing that has to be cleaned up.
  (kind (required-argument)
	:type (member :special-bind :catch :unwind-protect :block :tagbody))
  ;; The node that messes things up. This is the last node in the
  ;; non-messed-up environment. Null only temporarily. This could be
  ;; deleted due to unreachability.
  (mess-up nil :type (or node null))
  ;; A list of all the NLX-Info structures whose NLX-Info-Cleanup is
  ;; this cleanup. This is filled in by environment analysis.
  (nlx-info nil :type list))
(defprinter (cleanup)
  kind
  mess-up
  (nlx-info :test nlx-info))

;;; The Environment structure represents the result of Environment analysis.
(defstruct environment
  ;; The function that allocates this environment.
  (function (required-argument) :type clambda)
  ;; A list of all the Lambdas that allocate variables in this environment.
  (lambdas nil :type list)
  ;; A list of all the lambda-vars and NLX-Infos needed from enclosing
  ;; environments by code in this environment.
  (closure nil :type list)
  ;; A list of NLX-Info structures describing all the non-local exits into this
  ;; environment.
  (nlx-info nil :type list)
  ;; Some kind of info used by the back end.
  (info nil))
(defprinter (environment)
  function
  (closure :test closure)
  (nlx-info :test nlx-info))

;;; The Tail-Set structure is used to accmumlate information about
;;; tail-recursive local calls. The "tail set" is effectively the transitive
;;; closure of the "is called tail-recursively by" relation.
;;;
;;; All functions in the same tail set share the same Tail-Set structure.
;;; Initially each function has its own Tail-Set, but when IR1-OPTIMIZE-RETURN
;;; notices a tail local call, it joins the tail sets of the called function
;;; and the calling function.
;;;
;;; The tail set is somewhat approximate, because it is too early to be sure
;;; which calls will be TR. Any call that *might* end up TR causes tail-set
;;; merging.
(defstruct tail-set
  ;; A list of all the lambdas in this tail set.
  (functions nil :type list)
  ;; Our current best guess of the type returned by these functions. This is
  ;; the union across all the functions of the return node's Result-Type.
  ;; excluding local calls.
  (type *wild-type* :type ctype)
  ;; Some info used by the back end.
  (info nil))
(defprinter (tail-set)
  functions
  type
  (info :test info))

;;; The NLX-Info structure is used to collect various information about
;;; non-local exits. This is effectively an annotation on the Continuation,
;;; although it is accessed by searching in the Environment-Nlx-Info.
(def!struct (nlx-info (:make-load-form-fun ignore-it))
  ;; The cleanup associated with this exit. In a catch or unwind-protect, this
  ;; is the :Catch or :Unwind-Protect cleanup, and not the cleanup for the
  ;; escape block. The Cleanup-Kind of this thus provides a good indication of
  ;; what kind of exit is being done.
  (cleanup (required-argument) :type cleanup)
  ;; The continuation exited to (the CONT of the EXIT nodes.)  If this exit is
  ;; from an escape function (CATCH or UNWIND-PROTECT), then environment
  ;; analysis deletes the escape function and instead has the %NLX-ENTRY use
  ;; this continuation.
  ;;
  ;; This slot is primarily an indication of where this exit delivers its
  ;; values to (if any), but it is also used as a sort of name to allow us to
  ;; find the NLX-Info that corresponds to a given exit. For this purpose, the
  ;; Entry must also be used to disambiguate, since exits to different places
  ;; may deliver their result to the same continuation.
  (continuation (required-argument) :type continuation)
  ;; The entry stub inserted by environment analysis. This is a block
  ;; containing a call to the %NLX-Entry funny function that has the original
  ;; exit destination as its successor. Null only temporarily.
  (target nil :type (or cblock null))
  ;; Some kind of info used by the back end.
  info)
(defprinter (nlx-info)
  continuation
  target
  info)

;;;; LEAF structures

;;; Variables, constants and functions are all represented by LEAF
;;; structures. A reference to a LEAF is indicated by a REF node. This
;;; allows us to easily substitute one for the other without actually
;;; hacking the flow graph.
(def!struct (leaf (:make-load-form-fun ignore-it)
		  (:constructor nil))
  ;; Some name for this leaf. The exact significance of the name
  ;; depends on what kind of leaf it is. In a Lambda-Var or
  ;; Global-Var, this is the symbol name of the variable. In a
  ;; functional that is from a DEFUN, this is the defined name. In
  ;; other functionals, this is a descriptive string.
  (name nil :type t)
  ;; The type which values of this leaf must have.
  (type *universal-type* :type ctype)
  ;; Where the Type information came from:
  ;;  :DECLARED, from a declaration.
  ;;  :ASSUMED, from uses of the object.
  ;;  :DEFINED, from examination of the definition.
  ;; FIXME: This should be a named type. (LEAF-WHERE-FROM?)
  (where-from :assumed :type (member :declared :assumed :defined))
  ;; List of the Ref nodes for this leaf.
  (refs () :type list)
  ;; True if there was ever a Ref or Set node for this leaf. This may
  ;; be true when Refs and Sets are null, since code can be deleted.
  (ever-used nil :type boolean)
  ;; Some kind of info used by the back end.
  (info nil))

;;; The Constant structure is used to represent known constant values.
;;; If Name is not null, then it is the name of the named constant
;;; which this leaf corresponds to, otherwise this is an anonymous
;;; constant.
(def!struct (constant (:include leaf))
  ;; The value of the constant.
  (value nil :type t))
(defprinter (constant)
  (name :test name)
  value)

;;; The Basic-Var structure represents information common to all
;;; variables which don't correspond to known local functions.
(def!struct (basic-var (:include leaf) (:constructor nil))
  ;; Lists of the set nodes for this variable.
  (sets () :type list))

;;; The Global-Var structure represents a value hung off of the symbol
;;; Name. We use a :Constant Var when we know that the thing is a
;;; constant, but don't know what the value is at compile time.
(def!struct (global-var (:include basic-var))
  ;; Kind of variable described.
  (kind (required-argument)
	:type (member :special :global-function :constant :global)))
(defprinter (global-var)
  name
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  kind)

;;; The Slot-Accessor structure represents slot accessor functions. It
;;; is a subtype of Global-Var to make it look more like a normal
;;; function.
(def!struct (slot-accessor (:include global-var
				     (where-from :defined)
				     (kind :global-function)))
  ;; The description of the structure that this is an accessor for.
  (for (required-argument) :type sb!xc:class)
  ;; The slot description of the slot.
  (slot (required-argument)))
(defprinter (slot-accessor)
  name
  for
  slot)

;;; The Defined-Function structure represents functions that are
;;; defined in the same compilation block, or that have inline
;;; expansions, or have a non-NIL INLINEP value. Whenever we change
;;; the INLINEP state (i.e. an inline proclamation) we copy the
;;; structure so that former inlinep values are preserved.
(def!struct (defined-function (:include global-var
					(where-from :defined)
					(kind :global-function)))
  ;; The values of INLINEP and INLINE-EXPANSION initialized from the
  ;; global environment.
  (inlinep nil :type inlinep)
  (inline-expansion nil :type (or cons null))
  ;; The block-local definition of this function (either because it
  ;; was semi-inline, or because it was defined in this block.) If
  ;; this function is not an entry point, then this may be deleted or
  ;; let-converted. Null if we haven't converted the expansion yet.
  (functional nil :type (or functional null)))
(defprinter (defined-function)
  name
  inlinep
  (functional :test functional))

;;;; function stuff

;;; We default the WHERE-FROM and TYPE slots to :DEFINED and FUNCTION.
;;; We don't normally manipulate function types for defined functions,
;;; but if someone wants to know, an approximation is there.
(def!struct (functional (:include leaf
				  (where-from :defined)
				  (type (specifier-type 'function))))
  ;; Some information about how this function is used. These values are
  ;; meaningful:
  ;;
  ;;    Nil
  ;;	An ordinary function, callable using local call.
  ;;
  ;;    :Let
  ;;	A lambda that is used in only one local call, and has in effect
  ;;	been substituted directly inline. The return node is deleted, and
  ;;	the result is computed with the actual result continuation for the
  ;;	call.
  ;;
  ;;    :MV-Let
  ;;	Similar to :Let, but the call is an MV-Call.
  ;;
  ;;    :Assignment
  ;;	Similar to a let, but can have other than one call as long as there
  ;;	is at most one non-tail call.
  ;;
  ;;    :Optional
  ;;	A lambda that is an entry-point for an optional-dispatch. Similar
  ;;	to NIL, but requires greater caution, since local call analysis may
  ;;	create new references to this function. Also, the function cannot
  ;;	be deleted even if it has *no* references. The Optional-Dispatch
  ;;	is in the LAMDBA-OPTIONAL-DISPATCH.
  ;;
  ;;    :External
  ;;	An external entry point lambda. The function it is an entry for is
  ;;	in the Entry-Function.
  ;;
  ;;    :Top-Level
  ;;	A top-level lambda, holding a compiled top-level form. Compiled
  ;;	very much like NIL, but provides an indication of top-level
  ;;	context. A top-level lambda should have *no* references. Its
  ;;	Entry-Function is a self-pointer.
  ;;
  ;;    :Top-Level-XEP
  ;;	After a component is compiled, we clobber any top-level code
  ;;	references to its non-closure XEPs with dummy FUNCTIONAL structures
  ;;	having this kind. This prevents the retained top-level code from
  ;;	holding onto the IR for the code it references.
  ;;
  ;;    :Escape
  ;;    :Cleanup
  ;;	Special functions used internally by Catch and Unwind-Protect.
  ;;	These are pretty much like a normal function (NIL), but are treated
  ;;	specially by local call analysis and stuff. Neither kind should
  ;;	ever be given an XEP even though they appear as args to funny
  ;;	functions. An :Escape function is never actually called, and thus
  ;;	doesn't need to have code generated for it.
  ;;
  ;;    :Deleted
  ;;	This function has been found to be uncallable, and has been
  ;;	marked for deletion.
  (kind nil :type (member nil :optional :deleted :external :top-level :escape
			  :cleanup :let :mv-let :assignment
			  :top-level-xep))
  ;; In a normal function, this is the external entry point (XEP)
  ;; lambda for this function, if any. Each function that is used
  ;; other than in a local call has an XEP, and all of the
  ;; non-local-call references are replaced with references to the
  ;; XEP.
  ;;
  ;; In an XEP lambda (indicated by the :External kind), this is the
  ;; function that the XEP is an entry-point for. The body contains
  ;; local calls to all the actual entry points in the function. In a
  ;; :Top-Level lambda (which is its own XEP) this is a self-pointer.
  ;;
  ;; With all other kinds, this is null.
  (entry-function nil :type (or functional null))
  ;; The value of any inline/notinline declaration for a local function.
  (inlinep nil :type inlinep)
  ;; If we have a lambda that can be used as in inline expansion for this
  ;; function, then this is it. If there is no source-level lambda
  ;; corresponding to this function then this is Null (but then INLINEP will
  ;; always be NIL as well.)
  (inline-expansion nil :type list)
  ;; The lexical environment that the inline-expansion should be converted in.
  (lexenv *lexenv* :type lexenv)
  ;; The original function or macro lambda list, or :UNSPECIFIED if this is a
  ;; compiler created function.
  (arg-documentation nil :type (or list (member :unspecified)))
  ;; Various rare miscellaneous info that drives code generation & stuff.
  (plist () :type list))
(defprinter (functional)
  name)

;;; The Lambda only deals with required lexical arguments. Special,
;;; optional, keyword and rest arguments are handled by transforming
;;; into simpler stuff.
(def!struct (clambda (:include functional)
		     (:conc-name lambda-)
		     (:predicate lambda-p)
		     (:constructor make-lambda)
		     (:copier copy-lambda))
  ;; List of lambda-var descriptors for args.
  (vars nil :type list)
  ;; If this function was ever a :OPTIONAL function (an entry-point
  ;; for an optional-dispatch), then this is that optional-dispatch.
  ;; The optional dispatch will be :DELETED if this function is no
  ;; longer :OPTIONAL.
  (optional-dispatch nil :type (or optional-dispatch null))
  ;; The Bind node for this Lambda. This node marks the beginning of
  ;; the lambda, and serves to explicitly represent the lambda binding
  ;; semantics within the flow graph representation. Null in deleted
  ;; functions, and also in LETs where we deleted the call & bind
  ;; (because there are no variables left), but have not yet actually
  ;; deleted the lambda yet.
  (bind nil :type (or bind null))
  ;; The Return node for this Lambda, or NIL if it has been deleted.
  ;; This marks the end of the lambda, receiving the result of the
  ;; body. In a let, the return node is deleted, and the body delivers
  ;; the value to the actual continuation. The return may also be
  ;; deleted if it is unreachable.
  (return nil :type (or creturn null))
  ;; If this is a let, then the Lambda whose Lets list we are in,
  ;; otherwise this is a self-pointer.
  (home nil :type (or clambda null))
  ;; A list of all the all the lambdas that have been let-substituted
  ;; in this lambda. This is only non-null in lambdas that aren't
  ;; lets.
  (lets () :type list)
  ;; A list of all the Entry nodes in this function and its lets. Null
  ;; an a let.
  (entries () :type list)
  ;; A list of all the functions directly called from this function
  ;; (or one of its lets) using a non-let local call. May include
  ;; deleted functions because nobody bothers to clear them out.
  (calls () :type list)
  ;; The Tail-Set that this lambda is in. Null during creation and in
  ;; let lambdas.
  (tail-set nil :type (or tail-set null))
  ;; The structure which represents the environment that this
  ;; Function's variables are allocated in. This is filled in by
  ;; environment analysis. In a let, this is EQ to our home's
  ;; environment.
  (environment nil :type (or environment null))
  ;; In a LET, this is the NODE-LEXENV of the combination node. We
  ;; retain it so that if the let is deleted (due to a lack of vars),
  ;; we will still have caller's lexenv to figure out which cleanup is
  ;; in effect.
  (call-lexenv nil :type (or lexenv null)))
(defprinter (clambda :conc-name lambda-)
  name
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  (vars :prin1 (mapcar #'leaf-name vars)))

;;; The Optional-Dispatch leaf is used to represent hairy lambdas. It
;;; is a Functional, like Lambda. Each legal number of arguments has a
;;; function which is called when that number of arguments is passed.
;;; The function is called with all the arguments actually passed. If
;;; additional arguments are legal, then the LEXPR style More-Entry
;;; handles them. The value returned by the function is the value
;;; which results from calling the Optional-Dispatch.
;;;
;;; The theory is that each entry-point function calls the next entry
;;; point tail-recursively, passing all the arguments passed in and
;;; the default for the argument the entry point is for. The last
;;; entry point calls the real body of the function. In the presence
;;; of supplied-p args and other hair, things are more complicated. In
;;; general, there is a distinct internal function that takes the
;;; supplied-p args as parameters. The preceding entry point calls
;;; this function with NIL filled in for the supplied-p args, while
;;; the current entry point calls it with T in the supplied-p
;;; positions.
;;;
;;; Note that it is easy to turn a call with a known number of
;;; arguments into a direct call to the appropriate entry-point
;;; function, so functions that are compiled together can avoid doing
;;; the dispatch.
(def!struct (optional-dispatch (:include functional))
  ;; The original parsed argument list, for anyone who cares.
  (arglist nil :type list)
  ;; True if &ALLOW-OTHER-KEYS was supplied.
  (allowp nil :type boolean)
  ;; True if &KEY was specified. (Doesn't necessarily mean that there
  ;; are any keyword arguments...)
  (keyp nil :type boolean)
  ;; The number of required arguments. This is the smallest legal
  ;; number of arguments.
  (min-args 0 :type unsigned-byte)
  ;; The total number of required and optional arguments. Args at
  ;; positions >= to this are rest, key or illegal args.
  (max-args 0 :type unsigned-byte)
  ;; List of the Lambdas which are the entry points for non-rest,
  ;; non-key calls. The entry for Min-Args is first, Min-Args+1
  ;; second, ... Max-Args last. The last entry-point always calls the
  ;; main entry; in simple cases it may be the main entry.
  (entry-points nil :type list)
  ;; An entry point which takes Max-Args fixed arguments followed by
  ;; an argument context pointer and an argument count. This entry
  ;; point deals with listifying rest args and parsing keywords. This
  ;; is null when extra arguments aren't legal.
  (more-entry nil :type (or clambda null))
  ;; The main entry-point into the function, which takes all arguments
  ;; including keywords as fixed arguments. The format of the
  ;; arguments must be determined by examining the arglist. This may
  ;; be used by callers that supply at least Max-Args arguments and
  ;; know what they are doing.
  (main-entry nil :type (or clambda null)))
(defprinter (optional-dispatch)
  name
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  arglist
  allowp
  keyp
  min-args
  max-args
  (entry-points :test entry-points)
  (more-entry :test more-entry)
  main-entry)

;;; The Arg-Info structure allows us to tack various information onto
;;; Lambda-Vars during IR1 conversion. If we use one of these things,
;;; then the var will have to be massaged a bit before it is simple
;;; and lexical.
(def!struct arg-info
  ;; True if this arg is to be specially bound.
  (specialp nil :type boolean)
  ;; The kind of argument being described. Required args only have arg
  ;; info structures if they are special.
  (kind (required-argument) :type (member :required :optional :keyword :rest
					  :more-context :more-count))
  ;; If true, the Var for supplied-p variable of a keyword or optional
  ;; arg. This is true for keywords with non-constant defaults even
  ;; when there is no user-specified supplied-p var.
  (supplied-p nil :type (or lambda-var null))
  ;; The default for a keyword or optional, represented as the
  ;; original Lisp code. This is set to NIL in keyword arguments that
  ;; are defaulted using the supplied-p arg.
  (default nil :type t)
  ;; The actual keyword for a keyword argument.
  (keyword nil :type (or keyword null)))
(defprinter (arg-info)
  (specialp :test specialp)
  kind
  (supplied-p :test supplied-p)
  (default :test default)
  (keyword :test keyword))

;;; The Lambda-Var structure represents a lexical lambda variable.
;;; This structure is also used during IR1 conversion to describe
;;; lambda arguments which may ultimately turn out not to be simple
;;; and lexical.
;;;
;;; Lambda-Vars with no Refs are considered to be deleted; environment
;;; analysis isn't done on these variables, so the back end must check
;;; for and ignore unreferenced variables. Note that a deleted
;;; lambda-var may have sets; in this case the back end is still
;;; responsible for propagating the Set-Value to the set's Cont.
(def!struct (lambda-var (:include basic-var))
  ;; True if this variable has been declared Ignore.
  (ignorep nil :type boolean)
  ;; The Lambda that this var belongs to. This may be null when we are
  ;; building a lambda during IR1 conversion.
  (home nil :type (or null clambda))
  ;; This is set by environment analysis if it chooses an indirect
  ;; (value cell) representation for this variable because it is both
  ;; set and closed over.
  (indirect nil :type boolean)
  ;; The following two slots are only meaningful during IR1 conversion
  ;; of hairy lambda vars:
  ;;
  ;; The Arg-Info structure which holds information obtained from
  ;; &keyword parsing.
  (arg-info nil :type (or arg-info null))
  ;; If true, the Global-Var structure for the special variable which
  ;; is to be bound to the value of this argument.
  (specvar nil :type (or global-var null))
  ;; Set of the CONSTRAINTs on this variable. Used by constraint
  ;; propagation. This is left null by the lambda pre-pass if it
  ;; determine that this is a set closure variable, and is thus not a
  ;; good subject for flow analysis.
  (constraints nil :type (or sset null)))
(defprinter (lambda-var)
  name
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  (ignorep :test ignorep)
  (arg-info :test arg-info)
  (specvar :test specvar))

;;;; basic node types

;;; A Ref represents a reference to a leaf. Ref-Reoptimize is
;;; initially (and forever) NIL, since Refs don't receive any values
;;; and don't have any IR1 optimizer.
(defstruct (ref (:include node (:reoptimize nil))
		(:constructor make-ref (derived-type leaf)))
  ;; The leaf referenced.
  (leaf nil :type leaf))
(defprinter (ref)
  leaf)

;;; Naturally, the IF node always appears at the end of a block.
;;; Node-Cont is a dummy continuation, and is there only to keep
;;; people happy.
(defstruct (cif (:include node)
		(:conc-name if-)
		(:predicate if-p)
		(:constructor make-if)
		(:copier copy-if))
  ;; Continuation for the predicate.
  (test (required-argument) :type continuation)
  ;; The blocks that we execute next in true and false case,
  ;; respectively (may be the same.)
  (consequent (required-argument) :type cblock)
  (alternative (required-argument) :type cblock))
(defprinter (cif :conc-name if-)
  (test :prin1 (continuation-use test))
  consequent
  alternative)

(defstruct (cset (:include node
			   (derived-type *universal-type*))
		 (:conc-name set-)
		 (:predicate set-p)
		 (:constructor make-set)
		 (:copier copy-set))
  ;; Descriptor for the variable set.
  (var (required-argument) :type basic-var)
  ;; Continuation for the value form.
  (value (required-argument) :type continuation))
(defprinter (cset :conc-name set-)
  var
  (value :prin1 (continuation-use value)))

;;; The Basic-Combination structure is used to represent both normal
;;; and multiple value combinations. In a local function call, this
;;; node appears at the end of its block and the body of the called
;;; function appears as the successor. The NODE-CONT remains the
;;; continuation which receives the value of the call.
(defstruct (basic-combination (:include node)
			      (:constructor nil))
  ;; Continuation for the function.
  (fun (required-argument) :type continuation)
  ;; List of continuations for the args. In a local call, an argument
  ;; continuation may be replaced with NIL to indicate that the
  ;; corresponding variable is unreferenced, and thus no argument
  ;; value need be passed.
  (args nil :type list)
  ;; The kind of function call being made. :LOCAL means that this is a
  ;; local call to a function in the same component, and that argument
  ;; syntax checking has been done, etc. Calls to known global
  ;; functions are represented by storing the FUNCTION-INFO for the
  ;; function in this slot. :FULL is a call to an (as yet) unknown
  ;; function. :ERROR is like :FULL, but means that we have discovered
  ;; that the call contains an error, and should not be reconsidered
  ;; for optimization.
  (kind :full :type (or (member :local :full :error) function-info))
  ;; Some kind of information attached to this node by the back end.
  (info nil))

;;; The COMBINATION node represents all normal function calls,
;;; including FUNCALL. This is distinct from BASIC-COMBINATION so that
;;; an MV-COMBINATION isn't COMBINATION-P.
(defstruct (combination (:include basic-combination)
			(:constructor make-combination (fun))))
(defprinter (combination)
  (fun :prin1 (continuation-use fun))
  (args :prin1 (mapcar #'(lambda (x)
			   (if x
			       (continuation-use x)
			       "<deleted>"))
		       args)))

;;; An MV-Combination is to Multiple-Value-Call as a Combination is to
;;; Funcall. This is used to implement all the multiple-value
;;; receiving forms.
(defstruct (mv-combination (:include basic-combination)
			   (:constructor make-mv-combination (fun))))
(defprinter (mv-combination)
  (fun :prin1 (continuation-use fun))
  (args :prin1 (mapcar #'continuation-use args)))

;;; The Bind node marks the beginning of a lambda body and represents
;;; the creation and initialization of the variables.
(defstruct (bind (:include node))
  ;; The lambda we are binding variables for. Null when we are
  ;; creating the Lambda during IR1 translation.
  (lambda nil :type (or clambda null)))
(defprinter (bind)
  lambda)

;;; The Return node marks the end of a lambda body. It collects the
;;; return values and represents the control transfer on return. This
;;; is also where we stick information used for Tail-Set type
;;; inference.
(defstruct (creturn (:include node)
		    (:conc-name return-)
		    (:predicate return-p)
		    (:constructor make-return)
		    (:copier copy-return))
  ;; The lambda we are returning from. Null temporarily during
  ;; ir1tran.
  (lambda nil :type (or clambda null))
  ;; The continuation which yields the value of the lambda.
  (result (required-argument) :type continuation)
  ;; The union of the node-derived-type of all uses of the result
  ;; other than by a local call, intersected with the result's
  ;; asserted-type. If there are no non-call uses, this is
  ;; *empty-type*.
  (result-type *wild-type* :type ctype))
(defprinter (creturn :conc-name return-)
  lambda
  result-type)

;;;; non-local exit support
;;;;
;;;; In IR1, we insert special nodes to mark potentially non-local
;;;; lexical exits.

;;; The Entry node serves to mark the start of the dynamic extent of a
;;; lexical exit. It is the mess-up node for the corresponding :Entry
;;; cleanup.
(defstruct (entry (:include node))
  ;; All of the Exit nodes for potential non-local exits to this point.
  (exits nil :type list)
  ;; The cleanup for this entry. Null only temporarily.
  (cleanup nil :type (or cleanup null)))
(defprinter (entry))

;;; The Exit node marks the place at which exit code would be emitted,
;;; if necessary. This is interposed between the uses of the exit
;;; continuation and the exit continuation's DEST. Instead of using
;;; the returned value being delivered directly to the exit
;;; continuation, it is delivered to our Value continuation. The
;;; original exit continuation is the exit node's CONT.
(defstruct (exit (:include node))
  ;; The Entry node that this is an exit for. If null, this is a
  ;; degenerate exit. A degenerate exit is used to "fill" an empty
  ;; block (which isn't allowed in IR1.) In a degenerate exit, Value
  ;; is always also null.
  (entry nil :type (or entry null))
  ;; The continuation yeilding the value we are to exit with. If NIL,
  ;; then no value is desired (as in GO).
  (value nil :type (or continuation null)))
(defprinter (exit)
  (entry :test entry)
  (value :test value))

;;;; miscellaneous IR1 structures

(defstruct (undefined-warning
	    #-no-ansi-print-object
	    (:print-object (lambda (x s)
			     (print-unreadable-object (x s :type t)
			       (prin1 (undefined-warning-name x) s)))))
  ;; The name of the unknown thing.
  (name nil :type (or symbol list))
  ;; The kind of reference to Name.
  (kind (required-argument) :type (member :function :type :variable))
  ;; The number of times this thing was used.
  (count 0 :type unsigned-byte)
  ;; A list of COMPILER-ERROR-CONTEXT structures describing places
  ;; where this thing was used. Note that we only record the first
  ;; *UNDEFINED-WARNING-LIMIT* calls.
  (warnings () :type list))

;;;; Freeze some structure types to speed type testing.

#!-sb-fluid
(declaim (freeze-type node leaf lexenv continuation cblock component cleanup
		      environment tail-set nlx-info))
