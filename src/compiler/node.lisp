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
  ;; an indication of the way that this continuation is currently used
  ;;
  ;; :UNUSED
  ;;	A continuation for which all control-related slots have the
  ;;	default values. A continuation is unused during IR1 conversion
  ;;	until it is assigned a block, and may be also be temporarily
  ;;	unused during later manipulations of IR1. In a consistent
  ;;	state there should never be any mention of :UNUSED
  ;;	continuations. Next can have a non-null value if the next node
  ;;	has already been determined.
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
  ;;	Like :BLOCK-START, but BLOCK has been deleted. A block
  ;;	starting continuation is made into a deleted block start when
  ;;	the block is deleted, but the continuation still may have
  ;;	value semantics. Since there isn't any code left, next is
  ;;	null.
  ;;
  ;; :INSIDE-BLOCK
  ;;	A continuation that is the CONT of some node in BLOCK.
  (kind :unused :type (member :unused :deleted :inside-block :block-start
			      :deleted-block-start))
  ;; The node which receives this value, if any. In a deleted
  ;; continuation, this is null even though the node that receives
  ;; this continuation may not yet be deleted.
  (dest nil :type (or node null))
  ;; If this is a NODE, then it is the node which is to be evaluated
  ;; next. This is always null in :DELETED and :UNUSED continuations,
  ;; and will be null in a :INSIDE-BLOCK continuation when this is the
  ;; CONT of the LAST.
  (next nil :type (or node null))
  ;; an assertion on the type of this continuation's value
  (asserted-type *wild-type* :type ctype)
  ;; cached type of this continuation's value. If NIL, then this must
  ;; be recomputed: see CONTINUATION-DERIVED-TYPE.
  (%derived-type nil :type (or ctype null))
  ;; the node where this continuation is used, if unique. This is always
  ;; null in :DELETED and :UNUSED continuations, and is never null in
  ;; :INSIDE-BLOCK continuations. In a :BLOCK-START continuation, the
  ;; Block's START-USES indicate whether NIL means no uses or more
  ;; than one use.
  (use nil :type (or node null))
  ;; the basic block this continuation is in. This is null only in
  ;; :DELETED and :UNUSED continuations. Note that blocks that are
  ;; unreachable but still in the DFO may receive deleted
  ;; continuations, so it isn't o.k. to assume that any continuation
  ;; that you pick up out of its DEST node has a BLOCK.
  (block nil :type (or cblock null))
  ;; set to true when something about this continuation's value has
  ;; changed. See REOPTIMIZE-CONTINUATION. This provides a way for IR1
  ;; optimize to determine which operands to a node have changed. If
  ;; the optimizer for this node type doesn't care, it can elect not
  ;; to clear this flag.
  (reoptimize t :type boolean)
  ;; an indication of what we have proven about how this contination's
  ;; type assertion is satisfied:
  ;;
  ;; NIL
  ;;    No type check is necessary (proven type is a subtype of the assertion.)
  ;;
  ;; T
  ;;    A type check is needed.
  ;;
  ;; :DELETED
  ;;    Don't do a type check, but believe (intersect) the assertion.
  ;;    A T check can be changed to :DELETED if we somehow prove the
  ;;    check is unnecessary, or if we eliminate it through a policy
  ;;    decision.
  ;;
  ;; :NO-CHECK
  ;;    Type check generation sets the slot to this if a check is
  ;;    called for, but it believes it has proven that the check won't
  ;;    be done for policy reasons or because a safe implementation
  ;;    will be used. In the latter case, LTN must ensure that a safe
  ;;    implementation *is* used.
  ;;
  ;; :ERROR
  ;;    There is a compile-time type error in some use of this
  ;;    continuation. A type check should still be generated, but be
  ;;    careful.
  ;;
  ;; This is computed lazily by CONTINUATION-DERIVED-TYPE, so use
  ;; CONTINUATION-TYPE-CHECK instead of the %'ed slot accessor.
  (%type-check t :type (member t nil :deleted :no-check :error))
  ;; something or other that the back end annotates this continuation with
  (info nil)
  ;; uses of this continuation in the lexical environment. They are
  ;; recorded so that when one continuation is substituted for another
  ;; the environment may be updated properly.
  (lexenv-uses nil :type list))

(def!method print-object ((x continuation) stream)
  (print-unreadable-object (x stream :type t :identity t)))

(defstruct (node (:constructor nil)
		 (:copier nil))
  ;; the bottom-up derived type for this node. This does not take into
  ;; consideration output type assertions on this node (actually on its CONT).
  (derived-type *wild-type* :type ctype)
  ;; True if this node needs to be optimized. This is set to true
  ;; whenever something changes about the value of a continuation
  ;; whose DEST is this node.
  (reoptimize t :type boolean)
  ;; the continuation which receives the value of this node. This also
  ;; indicates what we do controlwise after evaluating this node. This
  ;; may be null during IR1 conversion.
  (cont nil :type (or continuation null))
  ;; the continuation that this node is the next of. This is null
  ;; during IR1 conversion when we haven't linked the node in yet or
  ;; in nodes that have been deleted from the IR1 by UNLINK-NODE.
  (prev nil :type (or continuation null))
  ;; the lexical environment this node was converted in
  (lexenv *lexenv* :type lexenv)
  ;; a representation of the source code responsible for generating
  ;; this node
  ;;
  ;; For a form introduced by compilation (does not appear in the
  ;; original source), the path begins with a list of all the
  ;; enclosing introduced forms. This list is from the inside out,
  ;; with the form immediately responsible for this node at the head
  ;; of the list.
  ;;
  ;; Following the introduced forms is a representation of the
  ;; location of the enclosing original source form. This transition
  ;; is indicated by the magic ORIGINAL-SOURCE-START marker. The first
  ;; element of the original source is the "form number", which is the
  ;; ordinal number of this form in a depth-first, left-to-right walk
  ;; of the truly-top-level form in which this appears.
  ;;
  ;; Following is a list of integers describing the path taken through
  ;; the source to get to this point:
  ;;     (K L M ...) => (NTH K (NTH L (NTH M ...)))
  ;;
  ;; The last element in the list is the top level form number, which
  ;; is the ordinal number (in this call to the compiler) of the truly
  ;; top level form containing the original source.
  (source-path *current-path* :type list)
  ;; If this node is in a tail-recursive position, then this is set to
  ;; T. At the end of IR1 (in physical environment analysis) this is
  ;; computed for all nodes (after cleanup code has been emitted).
  ;; Before then, a non-null value indicates that IR1 optimization has
  ;; converted a tail local call to a direct transfer.
  ;;
  ;; If the back-end breaks tail-recursion for some reason, then it
  ;; can null out this slot.
  (tail-p nil :type boolean))

;;; Flags that are used to indicate various things about a block, such
;;; as what optimizations need to be done on it:
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
;;;    These flags are used to indicate that something in this block
;;;    might be of interest to constraint propagation. TYPE-ASSERTED
;;;    is set when a continuation type assertion is strengthened.
;;;    TEST-MODIFIED is set whenever the test for the ending IF has
;;;    changed (may be true when there is no IF.)
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

;;; The CBLOCK structure represents a basic block. We include
;;; SSET-ELEMENT so that we can have sets of blocks. Initially the
;;; SSET-ELEMENT-NUMBER is null, DFO analysis numbers in reverse DFO.
;;; During IR2 conversion, IR1 blocks are re-numbered in forward emit
;;; order. This latter numbering also forms the basis of the block
;;; numbering in the debug-info (though that is relative to the start
;;; of the function.)
(defstruct (cblock (:include sset-element)
		   (:constructor make-block (start))
		   (:constructor make-block-key)
		   (:conc-name block-)
		   (:predicate block-p)
		   (:copier copy-block))
  ;; a list of all the blocks that are predecessors/successors of this
  ;; block. In well-formed IR1, most blocks will have one successor.
  ;; The only exceptions are:
  ;;  1. component head blocks (any number)
  ;;  2. blocks ending in an IF (1 or 2)
  ;;  3. blocks with DELETE-P set (zero)
  (pred nil :type list)
  (succ nil :type list)
  ;; the continuation which heads this block (either a :BLOCK-START or
  ;; :DELETED-BLOCK-START), or NIL when we haven't made the start
  ;; continuation yet (and in the dummy component head and tail
  ;; blocks)
  (start nil :type (or continuation null))
  ;; a list of all the nodes that have START as their CONT
  (start-uses nil :type list)
  ;; the last node in this block. This is NIL when we are in the
  ;; process of building a block (and in the dummy component head and
  ;; tail blocks.)
  (last nil :type (or node null))
  ;; the forward and backward links in the depth-first ordering of the
  ;; blocks. These slots are NIL at beginning/end.
  (next nil :type (or null cblock))
  (prev nil :type (or null cblock))
  ;; This block's attributes: see above.
  (flags (block-attributes reoptimize flush-p type-check type-asserted
			   test-modified)
	 :type attributes)
  ;; CMU CL had a KILL slot here, documented as "set used by
  ;; constraint propagation", which was used in constraint propagation
  ;; as a list of LAMBDA-VARs killed, and in copy propagation as an
  ;; SSET, representing I dunno what. I (WHN) found this confusing,
  ;; and furthermore it caused type errors when I was trying to make
  ;; the compiler produce fully general LAMBDA functions directly
  ;; (instead of doing as CMU CL always did, producing extra little
  ;; functions which return the LAMDBA you need) and therefore taking
  ;; a new path through the compiler. So I split this into two:
  ;;   KILL-LIST = list of LAMBDA-VARs killed, used in constraint propagation
  ;;   KILL-SSET = an SSET value, used in copy propagation
  (kill-list nil :type list)
  (kill-sset nil :type (or sset null))
  ;; other sets used in constraint propagation and/or copy propagation
  (gen nil)
  (in nil)
  (out nil)
  ;; the component this block is in, or NIL temporarily during IR1
  ;; conversion and in deleted blocks
  (component (progn
	       (aver-live-component *current-component*)
	       *current-component*)
	     :type (or component null))
  ;; a flag used by various graph-walking code to determine whether
  ;; this block has been processed already or what. We make this
  ;; initially NIL so that FIND-INITIAL-DFO doesn't have to scan the
  ;; entire initial component just to clear the flags.
  (flag nil)
  ;; some kind of info used by the back end
  (info nil)
  ;; If true, then constraints that hold in this block and its
  ;; successors by merit of being tested by its IF predecessor.
  (test-constraint nil :type (or sset null)))
(def!method print-object ((cblock cblock) stream)
  (print-unreadable-object (cblock stream :type t :identity t)
    (format stream ":START c~W" (cont-num (block-start cblock)))))

;;; The BLOCK-ANNOTATION class is inherited (via :INCLUDE) by
;;; different BLOCK-INFO annotation structures so that code
;;; (specifically control analysis) can be shared.
(defstruct (block-annotation (:constructor nil)
			     (:copier nil))
  ;; The IR1 block that this block is in the INFO for.
  (block (missing-arg) :type cblock)
  ;; the next and previous block in emission order (not DFO). This
  ;; determines which block we drop though to, and is also used to
  ;; chain together overflow blocks that result from splitting of IR2
  ;; blocks in lifetime analysis.
  (next nil :type (or block-annotation null))
  (prev nil :type (or block-annotation null)))

;;; A COMPONENT structure provides a handle on a connected piece of
;;; the flow graph. Most of the passes in the compiler operate on
;;; COMPONENTs rather than on the entire flow graph.
;;;
;;; According to the CMU CL internals/front.tex, the reason for
;;; separating compilation into COMPONENTs is
;;;   to increase the efficiency of large block compilations. In
;;;   addition to improving locality of reference and reducing the
;;;   size of flow analysis problems, this allows back-end data
;;;   structures to be reclaimed after the compilation of each
;;;   component.
(defstruct (component (:copier nil))
  ;; the kind of component
  ;;
  ;; (The terminology here is left over from before
  ;; sbcl-0.pre7.34.flaky5.2, when there was no such thing as
  ;; FUNCTIONAL-HAS-EXTERNAL-REFERENCES-P, so that Python was
  ;; incapable of building standalone :EXTERNAL functions, but instead
  ;; had to implement things like #'CL:COMPILE as FUNCALL of a little
  ;; toplevel stub whose sole purpose was to return an :EXTERNAL
  ;; function.)
  ;;
  ;; The possibilities are:
  ;;   NIL
  ;;     an ordinary component, containing non-top-level code
  ;;   :TOPLEVEL
  ;;     a component containing only load-time code
  ;;   :COMPLEX-TOPLEVEL
  ;;     In the old system, before FUNCTIONAL-HAS-EXTERNAL-REFERENCES-P
  ;;     was defined, this was necessarily a component containing both
  ;;     top level and run-time code. Now this state is also used for
  ;;     a component with HAS-EXTERNAL-REFERENCES-P functionals in it.
  ;;   :INITIAL
  ;;     the result of initial IR1 conversion, on which component
  ;;     analysis has not been done
  ;;   :DELETED
  ;;     debris left over from component analysis
  ;;
  ;; See also COMPONENT-TOPLEVELISH-P.
  (kind nil :type (member nil :toplevel :complex-toplevel :initial :deleted))
  ;; the blocks that are the dummy head and tail of the DFO
  ;;
  ;; Entry/exit points have these blocks as their
  ;; predecessors/successors. Null temporarily. The start and return
  ;; from each non-deleted function is linked to the component head
  ;; and tail. Until physical environment analysis links NLX entry
  ;; stubs to the component head, every successor of the head is a
  ;; function start (i.e. begins with a BIND node.)
  (head nil :type (or null cblock))
  (tail nil :type (or null cblock))
  ;; This becomes a list of the CLAMBDA structures for all functions
  ;; in this component. OPTIONAL-DISPATCHes are represented only by
  ;; their XEP and other associated lambdas. This doesn't contain any
  ;; deleted or LET lambdas.
  ;;
  ;; Note that logical associations between CLAMBDAs and COMPONENTs
  ;; seem to exist for a while before this is initialized. See e.g.
  ;; the NEW-FUNS slot. In particular, I got burned by writing some
  ;; code to use this value to decide which components need
  ;; LOCALL-ANALYZE-COMPONENT, when it turns out that
  ;; LOCALL-ANALYZE-COMPONENT had a role in initializing this value
  ;; (and DFO stuff does too, maybe). Also, even after it's
  ;; initialized, it might change as CLAMBDAs are deleted or merged.
  ;; -- WHN 2001-09-30
  (lambdas () :type list)
  ;; a list of FUNCTIONALs for functions that are newly converted, and
  ;; haven't been local-call analyzed yet. Initially functions are not
  ;; in the LAMBDAS list. Local call analysis moves them there
  ;; (possibly as LETs, or implicitly as XEPs if an OPTIONAL-DISPATCH.)
  ;; Between runs of local call analysis there may be some debris of
  ;; converted or even deleted functions in this list.
  (new-funs () :type list)
  ;; If this is true, then there is stuff in this component that could
  ;; benefit from further IR1 optimization.
  (reoptimize t :type boolean)
  ;; If this is true, then the control flow in this component was
  ;; messed up by IR1 optimizations, so the DFO should be recomputed.
  (reanalyze nil :type boolean)
  ;; some sort of name for the code in this component
  (name "<unknown>" :type simple-string)
  ;; When I am a child, this is :NO-IR2-YET.
  ;; In my adulthood, IR2 stores notes to itself here.
  ;; After I have left the great wheel and am staring into the GC, this
  ;;   is set to :DEAD to indicate that it's a gruesome error to operate
  ;;   on me (e.g. by using me as *CURRENT-COMPONENT*, or by pushing
  ;;   LAMBDAs onto my NEW-FUNS, as in sbcl-0.pre7.115).
  (info :no-ir2-yet :type (or ir2-component (member :no-ir2-yet :dead)))
  ;; the SOURCE-INFO structure describing where this component was
  ;; compiled from
  (source-info *source-info* :type source-info)
  ;; count of the number of inline expansions we have done while
  ;; compiling this component, to detect infinite or exponential
  ;; blowups
  (inline-expansions 0 :type index)
  ;; a map from combination nodes to things describing how an
  ;; optimization of the node failed. The description is an alist
  ;; (TRANSFORM . ARGS), where TRANSFORM is the structure describing
  ;; the transform that failed, and ARGS is either a list of format
  ;; arguments for the note, or the FUN-TYPE that would have
  ;; enabled the transformation but failed to match.
  (failed-optimizations (make-hash-table :test 'eq) :type hash-table)
  ;; This is similar to NEW-FUNS, but is used when a function has
  ;; already been analyzed, but new references have been added by
  ;; inline expansion. Unlike NEW-FUNS, this is not disjoint from
  ;; COMPONENT-LAMBDAS.
  (reanalyze-funs nil :type list))
(defprinter (component :identity t)
  name
  (reanalyze :test reanalyze))

;;; Check that COMPONENT is suitable for roles which involve adding
;;; new code. (gotta love imperative programming with lotso in-place
;;; side-effects...)
(defun aver-live-component (component)
  ;; FIXME: As of sbcl-0.pre7.115, we're asserting that
  ;; COMPILE-COMPONENT hasn't happened yet. Might it be even better
  ;; (certainly stricter, possibly also correct...) to assert that
  ;; IR1-FINALIZE hasn't happened yet?
  (aver (not (eql (component-info component) :dead))))

;;; Before sbcl-0.7.0, there were :TOPLEVEL things which were magical
;;; in multiple ways. That's since been refactored into the orthogonal
;;; properties "optimized for locall with no arguments" and "externally
;;; visible/referenced (so don't delete it)". The code <0.7.0 did a lot
;;; of tests a la (EQ KIND :TOP_LEVEL) in the "don't delete it?" sense;
;;; this function is a sort of literal translation of those tests into
;;; the new world.
;;;
;;; FIXME: After things settle down, bare :TOPLEVEL might go away, at
;;; which time it might be possible to replace the COMPONENT-KIND
;;; :TOPLEVEL mess with a flag COMPONENT-HAS-EXTERNAL-REFERENCES-P
;;; along the lines of FUNCTIONAL-HAS-EXTERNAL-REFERENCES-P.
(defun lambda-toplevelish-p (clambda)
  (or (eql (lambda-kind clambda) :toplevel)
      (lambda-has-external-references-p clambda)))
(defun component-toplevelish-p (component)
  (member (component-kind component)
	  '(:toplevel :complex-toplevel)))

;;; A CLEANUP structure represents some dynamic binding action. Blocks
;;; are annotated with the current CLEANUP so that dynamic bindings
;;; can be removed when control is transferred out of the binding
;;; environment. We arrange for changes in dynamic bindings to happen
;;; at block boundaries, so that cleanup code may easily be inserted.
;;; The "mess-up" action is explicitly represented by a funny function
;;; call or ENTRY node.
;;;
;;; We guarantee that CLEANUPs only need to be done at block boundaries
;;; by requiring that the exit continuations initially head their
;;; blocks, and then by not merging blocks when there is a cleanup
;;; change.
(defstruct (cleanup (:copier nil))
  ;; the kind of thing that has to be cleaned up
  (kind (missing-arg)
	:type (member :special-bind :catch :unwind-protect :block :tagbody))
  ;; the node that messes things up. This is the last node in the
  ;; non-messed-up environment. Null only temporarily. This could be
  ;; deleted due to unreachability.
  (mess-up nil :type (or node null))
  ;; a list of all the NLX-INFO structures whose NLX-INFO-CLEANUP is
  ;; this cleanup. This is filled in by physical environment analysis.
  (nlx-info nil :type list))
(defprinter (cleanup :identity t)
  kind
  mess-up
  (nlx-info :test nlx-info))

;;; A PHYSENV represents the result of physical environment analysis.
;;;
;;; As far as I can tell from reverse engineering, this IR1 structure
;;; represents the physical environment (which is probably not the
;;; standard Lispy term for this concept, but I dunno what is the
;;; standard term): those things in the lexical environment which a
;;; LAMBDA actually interacts with. Thus in
;;;   (DEFUN FROB-THINGS (THINGS)
;;;     (DOLIST (THING THINGS)
;;;       (BLOCK FROBBING-ONE-THING
;;;         (MAPCAR (LAMBDA (PATTERN)
;;;                   (WHEN (FITS-P THING PATTERN)
;;;                     (RETURN-FROM FROB-THINGS (LIST :FIT THING PATTERN))))
;;;                 *PATTERNS*))))
;;; the variables THINGS, THING, and PATTERN and the block names
;;; FROB-THINGS and FROBBING-ONE-THING are all in the inner LAMBDA's
;;; lexical environment, but of those only THING, PATTERN, and
;;; FROB-THINGS are in its physical environment. In IR1, we largely
;;; just collect the names of these things; in IR2 an IR2-PHYSENV
;;; structure is attached to INFO and used to keep track of
;;; associations between these names and less-abstract things (like
;;; TNs, or eventually stack slots and registers). -- WHN 2001-09-29
(defstruct (physenv (:copier nil))
  ;; the function that allocates this physical environment
  (lambda (missing-arg) :type clambda :read-only t)
  #| ; seems not to be used as of sbcl-0.pre7.51
  ;; a list of all the lambdas that allocate variables in this
  ;; physical environment
  (lambdas nil :type list)
  |#
  ;; This ultimately converges to a list of all the LAMBDA-VARs and
  ;; NLX-INFOs needed from enclosing environments by code in this
  ;; physical environment. In the meantime, it may be
  ;;   * NIL at object creation time
  ;;   * a superset of the correct result, generated somewhat later
  ;;   * smaller and smaller sets converging to the correct result as
  ;;     we notice and delete unused elements in the superset
  (closure nil :type list)
  ;; a list of NLX-INFO structures describing all the non-local exits
  ;; into this physical environment
  (nlx-info nil :type list)
  ;; some kind of info used by the back end
  (info nil))
(defprinter (physenv :identity t)
  lambda
  (closure :test closure)
  (nlx-info :test nlx-info))

;;; An TAIL-SET structure is used to accumulate information about
;;; tail-recursive local calls. The "tail set" is effectively the
;;; transitive closure of the "is called tail-recursively by"
;;; relation.
;;;
;;; All functions in the same tail set share the same TAIL-SET
;;; structure. Initially each function has its own TAIL-SET, but when
;;; IR1-OPTIMIZE-RETURN notices a tail local call, it joins the tail
;;; sets of the called function and the calling function.
;;;
;;; The tail set is somewhat approximate, because it is too early to
;;; be sure which calls will be tail-recursive. Any call that *might*
;;; end up tail-recursive causes TAIL-SET merging.
(defstruct (tail-set)
  ;; a list of all the LAMBDAs in this tail set
  (funs nil :type list)
  ;; our current best guess of the type returned by these functions.
  ;; This is the union across all the functions of the return node's
  ;; RESULT-TYPE, excluding local calls.
  (type *wild-type* :type ctype)
  ;; some info used by the back end
  (info nil))
(defprinter (tail-set :identity t)
  funs
  type
  (info :test info))

;;; The NLX-Info structure is used to collect various information
;;; about non-local exits. This is effectively an annotation on the
;;; CONTINUATION, although it is accessed by searching in the
;;; PHYSENV-NLX-INFO.
(def!struct (nlx-info (:make-load-form-fun ignore-it))
  ;; the cleanup associated with this exit. In a catch or
  ;; unwind-protect, this is the :CATCH or :UNWIND-PROTECT cleanup,
  ;; and not the cleanup for the escape block. The CLEANUP-KIND of
  ;; this thus provides a good indication of what kind of exit is
  ;; being done.
  (cleanup (missing-arg) :type cleanup)
  ;; the continuation exited to (the CONT of the EXIT nodes). If this
  ;; exit is from an escape function (CATCH or UNWIND-PROTECT), then
  ;; physical environment analysis deletes the escape function and
  ;; instead has the %NLX-ENTRY use this continuation.
  ;;
  ;; This slot is primarily an indication of where this exit delivers
  ;; its values to (if any), but it is also used as a sort of name to
  ;; allow us to find the NLX-Info that corresponds to a given exit.
  ;; For this purpose, the Entry must also be used to disambiguate,
  ;; since exits to different places may deliver their result to the
  ;; same continuation.
  (continuation (missing-arg) :type continuation)
  ;; the entry stub inserted by physical environment analysis. This is
  ;; a block containing a call to the %NLX-Entry funny function that
  ;; has the original exit destination as its successor. Null only
  ;; temporarily.
  (target nil :type (or cblock null))
  ;; some kind of info used by the back end
  info)
(defprinter (nlx-info :identity t)
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
  ;; (For public access to this slot, use LEAF-SOURCE-NAME.)
  ;;
  ;; the name of LEAF as it appears in the source, e.g. 'FOO or '(SETF
  ;; FOO) or 'N or '*Z*, or the special .ANONYMOUS. value if there's
  ;; no name for this thing in the source (as can happen for
  ;; FUNCTIONALs, e.g. for anonymous LAMBDAs or for functions for
  ;; top-level forms; and can also happen for anonymous constants) or
  ;; perhaps also if the match between the name and the thing is
  ;; skewed enough (e.g. for macro functions or method functions) that
  ;; we don't want to have that name affect compilation
  ;;
  ;; The value of this slot in can affect ordinary runtime behavior,
  ;; e.g. of special variables and known functions, not just debugging.
  ;;
  ;; See also the LEAF-DEBUG-NAME function and the
  ;; FUNCTIONAL-%DEBUG-NAME slot.
  (%source-name (missing-arg)
		:type (or symbol (and cons (satisfies legal-fun-name-p)))
		:read-only t)
  ;; the type which values of this leaf must have
  (type *universal-type* :type ctype)
  ;; where the TYPE information came from:
  ;;  :DECLARED, from a declaration.
  ;;  :ASSUMED, from uses of the object.
  ;;  :DEFINED, from examination of the definition.
  ;; FIXME: This should be a named type. (LEAF-WHERE-FROM? Or
  ;; perhaps just WHERE-FROM, since it's not just used in LEAF,
  ;; but also in various DEFINE-INFO-TYPEs in globaldb.lisp,
  ;; and very likely elsewhere too.)
  (where-from :assumed :type (member :declared :assumed :defined))
  ;; list of the REF nodes for this leaf
  (refs () :type list)
  ;; true if there was ever a REF or SET node for this leaf. This may
  ;; be true when REFS and SETS are null, since code can be deleted.
  (ever-used nil :type boolean)
  ;; some kind of info used by the back end
  (info nil))

;;; LEAF name operations
;;;
;;; KLUDGE: wants CLOS..
(defun leaf-has-source-name-p (leaf)
  (not (eq (leaf-%source-name leaf)
	   '.anonymous.)))
(defun leaf-source-name (leaf)
  (aver (leaf-has-source-name-p leaf))
  (leaf-%source-name leaf))
(defun leaf-debug-name (leaf)
  (if (functional-p leaf)
      ;; FUNCTIONALs have additional %DEBUG-NAME behavior.
      (functional-debug-name leaf)
      ;; Other objects just use their source name.
      ;;
      ;; (As of sbcl-0.pre7.85, there are a few non-FUNCTIONAL
      ;; anonymous objects, (anonymous constants..) and those would
      ;; fail here if we ever tried to get debug names from them, but
      ;; it looks as though it's never interesting to get debug names
      ;; from them, so it's moot. -- WHN)
      (leaf-source-name leaf)))

;;; The CONSTANT structure is used to represent known constant values.
;;; If NAME is not null, then it is the name of the named constant
;;; which this leaf corresponds to, otherwise this is an anonymous
;;; constant.
(def!struct (constant (:include leaf))
  ;; the value of the constant
  (value nil :type t))
(defprinter (constant :identity t)
  (%source-name :test %source-name)
  value)

;;; The BASIC-VAR structure represents information common to all
;;; variables which don't correspond to known local functions.
(def!struct (basic-var (:include leaf)
		       (:constructor nil))
  ;; Lists of the set nodes for this variable.
  (sets () :type list))

;;; The GLOBAL-VAR structure represents a value hung off of the symbol
;;; NAME. We use a :CONSTANT VAR when we know that the thing is a
;;; constant, but don't know what the value is at compile time.
(def!struct (global-var (:include basic-var))
  ;; kind of variable described
  (kind (missing-arg)
	:type (member :special :global-function :global)))
(defprinter (global-var :identity t)
  %source-name
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  kind)

;;; The SLOT-ACCESSOR structure represents slot accessor functions. It
;;; is a subtype of GLOBAL-VAR to make it look more like a normal
;;; function.
(def!struct (slot-accessor (:include global-var
				     (where-from :defined)
				     (kind :global-function)))
  ;; The description of the structure that this is an accessor for.
  (for (missing-arg) :type sb!xc:class)
  ;; The slot description of the slot.
  (slot (missing-arg)))
(defprinter (slot-accessor :identity t)
  %source-name
  for
  slot)

;;; A DEFINED-FUN represents a function that is defined in the same
;;; compilation block, or that has an inline expansion, or that has a
;;; non-NIL INLINEP value. Whenever we change the INLINEP state (i.e.
;;; an inline proclamation) we copy the structure so that former
;;; INLINEP values are preserved.
(def!struct (defined-fun (:include global-var
				   (where-from :defined)
				   (kind :global-function)))
  ;; The values of INLINEP and INLINE-EXPANSION initialized from the
  ;; global environment.
  (inlinep nil :type inlinep)
  (inline-expansion nil :type (or cons null))
  ;; the block-local definition of this function (either because it
  ;; was semi-inline, or because it was defined in this block). If
  ;; this function is not an entry point, then this may be deleted or
  ;; LET-converted. Null if we haven't converted the expansion yet.
  (functional nil :type (or functional null)))
(defprinter (defined-fun :identity t)
  %source-name
  inlinep
  (functional :test functional))

;;;; function stuff

;;; We default the WHERE-FROM and TYPE slots to :DEFINED and FUNCTION.
;;; We don't normally manipulate function types for defined functions,
;;; but if someone wants to know, an approximation is there.
(def!struct (functional (:include leaf
				  (%source-name '.anonymous.)
				  (where-from :defined)
				  (type (specifier-type 'function))))
  ;; (For public access to this slot, use LEAF-DEBUG-NAME.)
  ;;
  ;; the name of FUNCTIONAL for debugging purposes, or NIL if we
  ;; should just let the SOURCE-NAME fall through
  ;; 
  ;; Unlike the SOURCE-NAME slot, this slot's value should never
  ;; affect ordinary code behavior, only debugging/diagnostic behavior.
  ;;
  ;; The value of this slot can be anything, except that it shouldn't
  ;; be a legal function name, since otherwise debugging gets
  ;; confusing. (If a legal function name is a good name for the
  ;; function, it should be in %SOURCE-NAME, and then we shouldn't
  ;; need a %DEBUG-NAME.) In SBCL as of 0.pre7.87, it's always a
  ;; string unless it's NIL, since that's how CMU CL represented debug
  ;; names. However, eventually I (WHN) think it we should start using
  ;; list values instead, since they have much nicer print properties
  ;; (abbreviation, skipping package prefixes when unneeded, and
  ;; renaming package prefixes when we do things like renaming SB!EXT
  ;; to SB-EXT).
  ;;
  ;; E.g. for the function which implements (DEFUN FOO ...), we could
  ;; have
  ;;   %SOURCE-NAME=FOO
  ;;   %DEBUG-NAME=NIL
  ;; for the function which implements the top level form
  ;; (IN-PACKAGE :FOO) we could have
  ;;   %SOURCE-NAME=NIL
  ;;   %DEBUG-NAME="top level form (IN-PACKAGE :FOO)"
  ;; for the function which implements FOO in
  ;;   (DEFUN BAR (...) (FLET ((FOO (...) ...)) ...))
  ;; we could have
  ;;   %SOURCE-NAME=FOO
  ;;   %DEBUG-NAME="FLET FOO in BAR"
  ;; and for the function which implements FOO in
  ;;   (DEFMACRO FOO (...) ...)
  ;; we could have
  ;;   %SOURCE-NAME=FOO (or maybe .ANONYMOUS.?)
  ;;   %DEBUG-NAME="DEFMACRO FOO"
  (%debug-name nil
	       :type (or null (not (satisfies legal-fun-name-p)))
	       :read-only t)
  ;; some information about how this function is used. These values
  ;; are meaningful:
  ;;
  ;;    NIL
  ;;	an ordinary function, callable using local call
  ;;
  ;;    :LET
  ;;	a lambda that is used in only one local call, and has in
  ;;	effect been substituted directly inline. The return node is
  ;;	deleted, and the result is computed with the actual result
  ;;	continuation for the call.
  ;;
  ;;    :MV-LET
  ;;	Similar to :LET, but the call is an MV-CALL.
  ;;
  ;;    :ASSIGNMENT
  ;;	similar to a LET, but can have other than one call as long as
  ;;	there is at most one non-tail call.
  ;;
  ;;    :OPTIONAL
  ;;	a lambda that is an entry-point for an optional-dispatch.
  ;;	Similar to NIL, but requires greater caution, since local call
  ;;	analysis may create new references to this function. Also, the
  ;;	function cannot be deleted even if it has *no* references. The
  ;;	OPTIONAL-DISPATCH is in the LAMDBA-OPTIONAL-DISPATCH.
  ;;
  ;;    :EXTERNAL
  ;;	an external entry point lambda. The function it is an entry
  ;;	for is in the ENTRY-FUN slot.
  ;;
  ;;    :TOPLEVEL
  ;;	a top level lambda, holding a compiled top level form.
  ;;	Compiled very much like NIL, but provides an indication of
  ;;	top level context. A :TOPLEVEL lambda should have *no*
  ;;	references. Its ENTRY-FUN is a self-pointer.
  ;;
  ;;    :TOPLEVEL-XEP
  ;;	After a component is compiled, we clobber any top level code
  ;;	references to its non-closure XEPs with dummy FUNCTIONAL
  ;;	structures having this kind. This prevents the retained
  ;;	top level code from holding onto the IR for the code it
  ;;	references.
  ;;
  ;;    :ESCAPE
  ;;    :CLEANUP
  ;;	special functions used internally by CATCH and UNWIND-PROTECT.
  ;;	These are pretty much like a normal function (NIL), but are
  ;;	treated specially by local call analysis and stuff. Neither
  ;;	kind should ever be given an XEP even though they appear as
  ;;	args to funny functions. An :ESCAPE function is never actually
  ;;	called, and thus doesn't need to have code generated for it.
  ;;
  ;;    :DELETED
  ;;	This function has been found to be uncallable, and has been
  ;;	marked for deletion.
  (kind nil :type (member nil :optional :deleted :external :toplevel
			  :escape :cleanup :let :mv-let :assignment
			  :toplevel-xep))
  ;; Is this a function that some external entity (e.g. the fasl dumper)
  ;; refers to, so that even when it appears to have no references, it
  ;; shouldn't be deleted? In the old days (before
  ;; sbcl-0.pre7.37.flaky5.2) this was sort of implicitly true when
  ;; KIND was :TOPLEVEL. Now it must be set explicitly, both for
  ;; :TOPLEVEL functions and for any other kind of functions that we
  ;; want to dump or return from #'CL:COMPILE or whatever.
  (has-external-references-p nil) 
  ;; In a normal function, this is the external entry point (XEP)
  ;; lambda for this function, if any. Each function that is used
  ;; other than in a local call has an XEP, and all of the
  ;; non-local-call references are replaced with references to the
  ;; XEP.
  ;;
  ;; In an XEP lambda (indicated by the :EXTERNAL kind), this is the
  ;; function that the XEP is an entry-point for. The body contains
  ;; local calls to all the actual entry points in the function. In a
  ;; :TOPLEVEL lambda (which is its own XEP) this is a self-pointer.
  ;;
  ;; With all other kinds, this is null.
  (entry-fun nil :type (or functional null))
  ;; the value of any inline/notinline declaration for a local function
  (inlinep nil :type inlinep)
  ;; If we have a lambda that can be used as in inline expansion for
  ;; this function, then this is it. If there is no source-level
  ;; lambda corresponding to this function then this is Null (but then
  ;; INLINEP will always be NIL as well.)
  (inline-expansion nil :type list)
  ;; the lexical environment that the inline-expansion should be converted in
  (lexenv *lexenv* :type lexenv)
  ;; the original function or macro lambda list, or :UNSPECIFIED if
  ;; this is a compiler created function
  (arg-documentation nil :type (or list (member :unspecified)))
  ;; various rare miscellaneous info that drives code generation & stuff
  (plist () :type list))
(defprinter (functional :identity t)
  %source-name
  %debug-name)

;;; FUNCTIONAL name operations
(defun functional-debug-name (functional)
  ;; FUNCTIONAL-%DEBUG-NAME takes precedence over FUNCTIONAL-SOURCE-NAME
  ;; here because we want different debug names for the functions in
  ;; DEFUN FOO and FLET FOO even though they have the same source name.
  (or (functional-%debug-name functional)
      ;; Note that this will cause an error if the function is
      ;; anonymous. In SBCL (as opposed to CMU CL) we make all
      ;; FUNCTIONALs have debug names. The CMU CL code didn't bother
      ;; in many FUNCTIONALs, especially those which were likely to be
      ;; optimized away before the user saw them. However, getting 
      ;; that right requires a global understanding of the code,
      ;; which seems bad, so we just require names for everything.
      (leaf-source-name functional)))

;;; The CLAMBDA only deals with required lexical arguments. Special,
;;; optional, keyword and rest arguments are handled by transforming
;;; into simpler stuff.
(def!struct (clambda (:include functional)
		     (:conc-name lambda-)
		     (:predicate lambda-p)
		     (:constructor make-lambda)
		     (:copier copy-lambda))
  ;; list of LAMBDA-VAR descriptors for arguments
  (vars nil :type list :read-only t)
  ;; If this function was ever a :OPTIONAL function (an entry-point
  ;; for an OPTIONAL-DISPATCH), then this is that OPTIONAL-DISPATCH.
  ;; The optional dispatch will be :DELETED if this function is no
  ;; longer :OPTIONAL.
  (optional-dispatch nil :type (or optional-dispatch null))
  ;; the BIND node for this LAMBDA. This node marks the beginning of
  ;; the lambda, and serves to explicitly represent the lambda binding
  ;; semantics within the flow graph representation. This is null in
  ;; deleted functions, and also in LETs where we deleted the call and
  ;; bind (because there are no variables left), but have not yet
  ;; actually deleted the LAMBDA yet.
  (bind nil :type (or bind null))
  ;; the RETURN node for this LAMBDA, or NIL if it has been deleted.
  ;; This marks the end of the lambda, receiving the result of the
  ;; body. In a LET, the return node is deleted, and the body delivers
  ;; the value to the actual continuation. The return may also be
  ;; deleted if it is unreachable.
  (return nil :type (or creturn null))
  ;; If this CLAMBDA is a LET, then this slot holds the LAMBDA whose
  ;; LETS list we are in, otherwise it is a self-pointer.
  (home nil :type (or clambda null))
  ;; all the lambdas that have been LET-substituted in this lambda.
  ;; This is only non-null in lambdas that aren't LETs.
  (lets nil :type list)
  ;; all the ENTRY nodes in this function and its LETs, or null in a LET
  (entries nil :type list)
  ;; CLAMBDAs which are locally called by this lambda, and other
  ;; objects (closed-over LAMBDA-VARs and XEPs) which this lambda
  ;; depends on in such a way that DFO shouldn't put them in separate
  ;; components.
  (calls-or-closes nil :type list)
  ;; the TAIL-SET that this LAMBDA is in. This is null during creation.
  ;;
  ;; In CMU CL, and old SBCL, this was also NILed out when LET
  ;; conversion happened. That caused some problems, so as of
  ;; sbcl-0.pre7.37.flaky5.2 when I was trying to get the compiler to
  ;; emit :EXTERNAL functions directly, and so now the value
  ;; is no longer NILed out in LET conversion, but instead copied
  ;; (so that any further optimizations on the rest of the tail
  ;; set won't modify the value) if necessary.
  (tail-set nil :type (or tail-set null))
  ;; the structure which represents the phsical environment that this
  ;; function's variables are allocated in. This is filled in by
  ;; physical environment analysis. In a LET, this is EQ to our home's
  ;; physical environment.
  (physenv nil :type (or physenv null))
  ;; In a LET, this is the NODE-LEXENV of the combination node. We
  ;; retain it so that if the LET is deleted (due to a lack of vars),
  ;; we will still have caller's lexenv to figure out which cleanup is
  ;; in effect.
  (call-lexenv nil :type (or lexenv null)))
(defprinter (clambda :conc-name lambda- :identity t)
  %source-name
  %debug-name
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  (vars :prin1 (mapcar #'leaf-source-name vars)))

;;; The OPTIONAL-DISPATCH leaf is used to represent hairy lambdas. It
;;; is a FUNCTIONAL, like LAMBDA. Each legal number of arguments has a
;;; function which is called when that number of arguments is passed.
;;; The function is called with all the arguments actually passed. If
;;; additional arguments are legal, then the LEXPR style MORE-ENTRY
;;; handles them. The value returned by the function is the value
;;; which results from calling the OPTIONAL-DISPATCH.
;;;
;;; The theory is that each entry-point function calls the next entry
;;; point tail-recursively, passing all the arguments passed in and
;;; the default for the argument the entry point is for. The last
;;; entry point calls the real body of the function. In the presence
;;; of SUPPLIED-P args and other hair, things are more complicated. In
;;; general, there is a distinct internal function that takes the
;;; SUPPLIED-P args as parameters. The preceding entry point calls
;;; this function with NIL filled in for the SUPPLIED-P args, while
;;; the current entry point calls it with T in the SUPPLIED-P
;;; positions.
;;;
;;; Note that it is easy to turn a call with a known number of
;;; arguments into a direct call to the appropriate entry-point
;;; function, so functions that are compiled together can avoid doing
;;; the dispatch.
(def!struct (optional-dispatch (:include functional))
  ;; the original parsed argument list, for anyone who cares
  (arglist nil :type list)
  ;; true if &ALLOW-OTHER-KEYS was supplied
  (allowp nil :type boolean)
  ;; true if &KEY was specified (which doesn't necessarily mean that
  ;; there are any &KEY arguments..)
  (keyp nil :type boolean)
  ;; the number of required arguments. This is the smallest legal
  ;; number of arguments.
  (min-args 0 :type unsigned-byte)
  ;; the total number of required and optional arguments. Args at
  ;; positions >= to this are &REST, &KEY or illegal args.
  (max-args 0 :type unsigned-byte)
  ;; list of the LAMBDAs which are the entry points for non-rest,
  ;; non-key calls. The entry for MIN-ARGS is first, MIN-ARGS+1
  ;; second, ... MAX-ARGS last. The last entry-point always calls the
  ;; main entry; in simple cases it may be the main entry.
  (entry-points nil :type list)
  ;; an entry point which takes MAX-ARGS fixed arguments followed by
  ;; an argument context pointer and an argument count. This entry
  ;; point deals with listifying rest args and parsing keywords. This
  ;; is null when extra arguments aren't legal.
  (more-entry nil :type (or clambda null))
  ;; the main entry-point into the function, which takes all arguments
  ;; including keywords as fixed arguments. The format of the
  ;; arguments must be determined by examining the arglist. This may
  ;; be used by callers that supply at least MAX-ARGS arguments and
  ;; know what they are doing.
  (main-entry nil :type (or clambda null)))
(defprinter (optional-dispatch :identity t)
  %source-name
  %debug-name
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

;;; The ARG-INFO structure allows us to tack various information onto
;;; LAMBDA-VARs during IR1 conversion. If we use one of these things,
;;; then the var will have to be massaged a bit before it is simple
;;; and lexical.
(def!struct arg-info
  ;; true if this arg is to be specially bound
  (specialp nil :type boolean)
  ;; the kind of argument being described. Required args only have arg
  ;; info structures if they are special.
  (kind (missing-arg)
	:type (member :required :optional :keyword :rest
		      :more-context :more-count))
  ;; If true, this is the VAR for SUPPLIED-P variable of a keyword or
  ;; optional arg. This is true for keywords with non-constant
  ;; defaults even when there is no user-specified supplied-p var.
  (supplied-p nil :type (or lambda-var null))
  ;; the default for a keyword or optional, represented as the
  ;; original Lisp code. This is set to NIL in &KEY arguments that are
  ;; defaulted using the SUPPLIED-P arg.
  (default nil :type t)
  ;; the actual key for a &KEY argument. Note that in ANSI CL this is
  ;; not necessarily a keyword: (DEFUN FOO (&KEY ((BAR BAR))) ...).
  (key nil :type symbol))
(defprinter (arg-info :identity t)
  (specialp :test specialp)
  kind
  (supplied-p :test supplied-p)
  (default :test default)
  (key :test key))

;;; The LAMBDA-VAR structure represents a lexical lambda variable.
;;; This structure is also used during IR1 conversion to describe
;;; lambda arguments which may ultimately turn out not to be simple
;;; and lexical.
;;;
;;; LAMBDA-VARs with no REFs are considered to be deleted; physical
;;; environment analysis isn't done on these variables, so the back
;;; end must check for and ignore unreferenced variables. Note that a
;;; deleted LAMBDA-VAR may have sets; in this case the back end is
;;; still responsible for propagating the SET-VALUE to the set's CONT.
(def!struct (lambda-var (:include basic-var))
  ;; true if this variable has been declared IGNORE
  (ignorep nil :type boolean)
  ;; the CLAMBDA that this var belongs to. This may be null when we are
  ;; building a lambda during IR1 conversion.
  (home nil :type (or null clambda))
  ;; This is set by physical environment analysis if it chooses an
  ;; indirect (value cell) representation for this variable because it
  ;; is both set and closed over.
  (indirect nil :type boolean)
  ;; The following two slots are only meaningful during IR1 conversion
  ;; of hairy lambda vars:
  ;;
  ;; The ARG-INFO structure which holds information obtained from
  ;; &keyword parsing.
  (arg-info nil :type (or arg-info null))
  ;; if true, the GLOBAL-VAR structure for the special variable which
  ;; is to be bound to the value of this argument
  (specvar nil :type (or global-var null))
  ;; Set of the CONSTRAINTs on this variable. Used by constraint
  ;; propagation. This is left null by the lambda pre-pass if it
  ;; determine that this is a set closure variable, and is thus not a
  ;; good subject for flow analysis.
  (constraints nil :type (or sset null)))
(defprinter (lambda-var :identity t)
  %source-name
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  (ignorep :test ignorep)
  (arg-info :test arg-info)
  (specvar :test specvar))

;;;; basic node types

;;; A REF represents a reference to a LEAF. REF-REOPTIMIZE is
;;; initially (and forever) NIL, since REFs don't receive any values
;;; and don't have any IR1 optimizer.
(defstruct (ref (:include node (:reoptimize nil))
		(:constructor make-ref (derived-type leaf))
		(:copier nil))
  ;; The leaf referenced.
  (leaf nil :type leaf))
(defprinter (ref :identity t)
  leaf)

;;; Naturally, the IF node always appears at the end of a block.
;;; NODE-CONT is a dummy continuation, and is there only to keep
;;; people happy.
(defstruct (cif (:include node)
		(:conc-name if-)
		(:predicate if-p)
		(:constructor make-if)
		(:copier copy-if))
  ;; CONTINUATION for the predicate
  (test (missing-arg) :type continuation)
  ;; the blocks that we execute next in true and false case,
  ;; respectively (may be the same)
  (consequent (missing-arg) :type cblock)
  (alternative (missing-arg) :type cblock))
(defprinter (cif :conc-name if- :identity t)
  (test :prin1 (continuation-use test))
  consequent
  alternative)

(defstruct (cset (:include node
			   (derived-type *universal-type*))
		 (:conc-name set-)
		 (:predicate set-p)
		 (:constructor make-set)
		 (:copier copy-set))
  ;; descriptor for the variable set
  (var (missing-arg) :type basic-var)
  ;; continuation for the value form
  (value (missing-arg) :type continuation))
(defprinter (cset :conc-name set- :identity t)
  var
  (value :prin1 (continuation-use value)))

;;; The BASIC-COMBINATION structure is used to represent both normal
;;; and multiple value combinations. In a local function call, this
;;; node appears at the end of its block and the body of the called
;;; function appears as the successor. The NODE-CONT remains the
;;; continuation which receives the value of the call.
(defstruct (basic-combination (:include node)
			      (:constructor nil)
			      (:copier nil))
  ;; continuation for the function
  (fun (missing-arg) :type continuation)
  ;; list of CONTINUATIONs for the args. In a local call, an argument
  ;; continuation may be replaced with NIL to indicate that the
  ;; corresponding variable is unreferenced, and thus no argument
  ;; value need be passed.
  (args nil :type list)
  ;; the kind of function call being made. :LOCAL means that this is a
  ;; local call to a function in the same component, and that argument
  ;; syntax checking has been done, etc. Calls to known global
  ;; functions are represented by storing the FUNCTION-INFO for the
  ;; function in this slot. :FULL is a call to an (as yet) unknown
  ;; function. :ERROR is like :FULL, but means that we have discovered
  ;; that the call contains an error, and should not be reconsidered
  ;; for optimization.
  (kind :full :type (or (member :local :full :error) function-info))
  ;; some kind of information attached to this node by the back end
  (info nil))

;;; The COMBINATION node represents all normal function calls,
;;; including FUNCALL. This is distinct from BASIC-COMBINATION so that
;;; an MV-COMBINATION isn't COMBINATION-P.
(defstruct (combination (:include basic-combination)
			(:constructor make-combination (fun))
			(:copier nil)))
(defprinter (combination :identity t)
  (fun :prin1 (continuation-use fun))
  (args :prin1 (mapcar (lambda (x)
			 (if x
			     (continuation-use x)
			     "<deleted>"))
		       args)))

;;; An MV-COMBINATION is to MULTIPLE-VALUE-CALL as a COMBINATION is to
;;; FUNCALL. This is used to implement all the multiple-value
;;; receiving forms.
(defstruct (mv-combination (:include basic-combination)
			   (:constructor make-mv-combination (fun))
			   (:copier nil)))
(defprinter (mv-combination)
  (fun :prin1 (continuation-use fun))
  (args :prin1 (mapcar #'continuation-use args)))

;;; The BIND node marks the beginning of a lambda body and represents
;;; the creation and initialization of the variables.
(defstruct (bind (:include node)
		 (:copier nil))
  ;; the lambda we are binding variables for. Null when we are
  ;; creating the LAMBDA during IR1 translation.
  (lambda nil :type (or clambda null)))
(defprinter (bind)
  lambda)

;;; The RETURN node marks the end of a lambda body. It collects the
;;; return values and represents the control transfer on return. This
;;; is also where we stick information used for TAIL-SET type
;;; inference.
(defstruct (creturn (:include node)
		    (:conc-name return-)
		    (:predicate return-p)
		    (:constructor make-return)
		    (:copier copy-return))
  ;; the lambda we are returning from. Null temporarily during
  ;; ir1tran.
  (lambda nil :type (or clambda null))
  ;; the continuation which yields the value of the lambda
  (result (missing-arg) :type continuation)
  ;; the union of the node-derived-type of all uses of the result
  ;; other than by a local call, intersected with the result's
  ;; asserted-type. If there are no non-call uses, this is
  ;; *EMPTY-TYPE*
  (result-type *wild-type* :type ctype))
(defprinter (creturn :conc-name return- :identity t)
  lambda
  result-type)

;;;; non-local exit support
;;;;
;;;; In IR1, we insert special nodes to mark potentially non-local
;;;; lexical exits.

;;; The ENTRY node serves to mark the start of the dynamic extent of a
;;; lexical exit. It is the mess-up node for the corresponding :Entry
;;; cleanup.
(defstruct (entry (:include node)
		  (:copier nil))
  ;; All of the Exit nodes for potential non-local exits to this point.
  (exits nil :type list)
  ;; The cleanup for this entry. NULL only temporarily.
  (cleanup nil :type (or cleanup null)))
(defprinter (entry :identity t))

;;; The EXIT node marks the place at which exit code would be emitted,
;;; if necessary. This is interposed between the uses of the exit
;;; continuation and the exit continuation's DEST. Instead of using
;;; the returned value being delivered directly to the exit
;;; continuation, it is delivered to our VALUE continuation. The
;;; original exit continuation is the exit node's CONT.
(defstruct (exit (:include node)
		 (:copier nil))
  ;; the ENTRY node that this is an exit for. If null, this is a
  ;; degenerate exit. A degenerate exit is used to "fill" an empty
  ;; block (which isn't allowed in IR1.) In a degenerate exit, Value
  ;; is always also null.
  (entry nil :type (or entry null))
  ;; the continuation yielding the value we are to exit with. If NIL,
  ;; then no value is desired (as in GO).
  (value nil :type (or continuation null)))
(defprinter (exit :identity t)
  (entry :test entry)
  (value :test value))

;;;; miscellaneous IR1 structures

(defstruct (undefined-warning
	    #-no-ansi-print-object
	    (:print-object (lambda (x s)
			     (print-unreadable-object (x s :type t)
			       (prin1 (undefined-warning-name x) s))))
	    (:copier nil))
  ;; the name of the unknown thing
  (name nil :type (or symbol list))
  ;; the kind of reference to NAME
  (kind (missing-arg) :type (member :function :type :variable))
  ;; the number of times this thing was used
  (count 0 :type unsigned-byte)
  ;; a list of COMPILER-ERROR-CONTEXT structures describing places
  ;; where this thing was used. Note that we only record the first
  ;; *UNDEFINED-WARNING-LIMIT* calls.
  (warnings () :type list))

;;; a helper for the POLICY macro, defined late here so that the
;;; various type tests can be inlined
(declaim (ftype (function ((or list lexenv node functional)) list)
		%coerce-to-policy))
(defun %coerce-to-policy (thing)
  (let ((result (etypecase thing
		  (list thing)
		  (lexenv (lexenv-policy thing))
		  (node (lexenv-policy (node-lexenv thing)))
		  (functional (lexenv-policy (functional-lexenv thing))))))
    ;; Test the first element of the list as a rudimentary sanity
    ;; that it really does look like a valid policy.
    (aver (or (null result) (policy-quality-name-p (caar result))))
    ;; Voila.
    result))

;;;; Freeze some structure types to speed type testing.

#!-sb-fluid
(declaim (freeze-type node leaf lexenv continuation cblock component cleanup
		      physenv tail-set nlx-info))
