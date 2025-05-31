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

(in-package "SB-C")

(declaim (special *lexenv*))

;;; The LEXENV represents the lexical environment used for IR1 conversion.
;;; (This is also what shows up as an ENVIRONMENT value in macroexpansion.)
(declaim (inline internal-make-lexenv))
(defstruct (lexenv
            (:include abstract-lexenv)
             (:print-function
              (lambda (lexenv stream depth)
                (if (null-lexenv-p lexenv)
                    (print-unreadable-object (lexenv stream)
                      (write-string "NULL-LEXENV" stream))
                    (default-structure-print lexenv stream depth))))
             (:copier nil)
             (:constructor make-null-lexenv ())
             (:constructor make-almost-null-lexenv
                           (%policy handled-conditions flushable lambda parent
                            &optional user-data))
             (:constructor make-package-lock-lexenv
                           (disabled-package-locks %policy
                            &aux (handled-conditions nil)))
             (:constructor internal-make-lexenv
                           (funs vars blocks tags
                            type-restrictions
                            flushable
                            lambda cleanup handled-conditions
                            disabled-package-locks %policy user-data
                            parent)))
  ;; an alist of (NAME . WHAT), where WHAT is either a FUNCTIONAL (a
  ;; local function), a DEFINED-FUN, representing an
  ;; INLINE/NOTINLINE declaration, or a list (MACRO . <function>) (a
  ;; local macro, with the specifier expander). Note that NAME may be
  ;; a (SETF <name>) list, not necessarily a single symbol.
  (funs nil :type list)
  ;; an alist translating variable names to LEAF structures. A special
  ;; binding is indicated by a :SPECIAL GLOBAL-VAR leaf. Each special
  ;; binding within the code gets a distinct leaf structure, as does
  ;; the current "global" value on entry to the code compiled.
  ;; (locally (special ...)) is handled by adding the most recent
  ;; special binding to the front of the list.
  ;;
  ;; If the CDR is (MACRO . <exp>), then <exp> is the expansion of a
  ;; symbol macro.
  (vars nil :type list)
  ;; BLOCKS and TAGS are alists from block and go-tag names to 2-lists
  ;; of the form (<entry> <continuation>), where <continuation> is the
  ;; continuation to exit to, and <entry> is the corresponding ENTRY
  ;; node.
  (blocks nil :type list)
  (tags nil :type list)
  ;; an alist (THING . CTYPE) which is used to keep track of
  ;; "pervasive" type declarations. When THING is a leaf, this is for
  ;; type declarations that pertain to the type in a syntactic extent
  ;; which does not correspond to a binding of the affected name.
  (type-restrictions nil :type list)
  ;; the lexically enclosing lambda, if any
  (lambda nil :type (or clambda null))
  ;; the lexically enclosing cleanup, or NIL if none enclosing within LAMBDA
  (cleanup nil :type (or cleanup null))
  ;; condition types we handle with a handler around the compiler
  (handled-conditions *handled-conditions*)
  ;; lexically disabled package locks (list of symbols)
  (disabled-package-locks *disabled-package-locks*)
  ;; the current OPTIMIZE policy. this is null in the null environment,
  ;; and the global policy is stored in *POLICY*. (Because we want to
  ;; be able to affect it from :WITH-COMPILATION-UNIT.) NIL here also
  ;; works as a convenient null-lexenv identifier.
  (%policy nil :type (or null policy))
  ;; A list associating extra user info to symbols.
  ;; sb-cltl2 entries are of the form (:declare name . value),
  ;; (:variable name key . value), or (:function name key . value)
  ;; The NO-COMPILER-MACRO declartion is also stored here (because it
  ;; can't be attached to a function, as compiler macros can be
  ;; defined on macros).
  (user-data nil :type list)
  (parent nil)
  ;; Cache of all visible variables, including the ones coming from
  ;; (call-lexenv lambda)
  ;; Used for LEAF-VISIBLE-TO-DEBUGGER-P
  (var-cache nil :type (or null hash-table))
  ;; A list of functions that can be removed when unused.
  ;; Similar to the FLUSHABLE attribute in DEFKNOWN, but can applied
  ;; locally to things that are generally not flushable but can be
  ;; flushed in some circumstances.
  (flushable nil :type list))

#+sb-eval
(defstruct (sb-eval::eval-lexenv
            (:include lexenv)
            (:copier nil)
            (:constructor sb-eval::make-eval-lexenv
                (funs vars blocks tags
                 type-restrictions
                 flushable
                 lambda cleanup handled-conditions
                 disabled-package-locks %policy user-data
                 parent))))

(defun lexenv-policy (lexenv)
  (or (lexenv-%policy lexenv) *policy*))

(defun null-lexenv-p (lexenv)
  (not (lexenv-%policy lexenv)))

;;; an object suitable for input to standard functions that accept
;;; "environment objects" (of the ANSI glossary)
(def!type lexenv-designator () '(or abstract-lexenv null))

;;; support for the idiom (in MACROEXPAND and elsewhere) that NIL is
;;; to be taken as a null lexical environment.
;;; Of course this is a mostly pointless "idiom" because NIL *is*
;;; an environment, as far as most environment inquiry functions care.
(defun coerce-to-lexenv (x)
  (etypecase x
    (null (make-null-lexenv))
    (lexenv x)
    #+(and sb-fasteval (not sb-xc-host))
    (sb-interpreter:basic-env (sb-interpreter:lexenv-from-env x))))

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

;;; Note: Continuations have been split into CTRANs and LVARs. Control
;;; transfers inside a block are represented with CTRANs; data
;;; transfers are represented with LVARs. However, many of the
;;; comments and names have not been updated to reflect this, and it
;;; is easy to find references to the old way of doing things with
;;; continuations throughout the compiler.

;;; "Lead-in" Control TRANsfer [to some node]
(defstruct (ctran (:constructor make-ctran (&optional kind)) (:copier nil))
  ;; an indication of the way that this continuation is currently used
  ;;
  ;; :UNUSED
  ;;    A continuation for which all control-related slots have the
  ;;    default values. A continuation is unused during IR1 conversion
  ;;    until it is assigned a block, and may be also be temporarily
  ;;    unused during later manipulations of IR1. In a consistent
  ;;    state there should never be any mention of :UNUSED
  ;;    continuations. NEXT can have a non-null value if the next node
  ;;    has already been determined.
  ;;
  ;; :BLOCK-START
  ;;    The continuation that is the START of BLOCK.
  ;;
  ;; :INSIDE-BLOCK
  ;;    A continuation that is the NEXT of some node in BLOCK.
  (kind :unused :type (member :unused :inside-block :block-start))
  ;; A NODE which is to be evaluated next. Null only temporary.
  (next nil :type (or node null))
  ;; the node where this CTRAN is used, if unique. This is always null
  ;; in :UNUSED and :BLOCK-START CTRANs, and is never null in
  ;; :INSIDE-BLOCK continuations.
  (use nil :type (or node null))
  ;; the basic block this continuation is in. This is null only in
  ;; :UNUSED continuations.
  (block nil :type (or cblock null))
  ;; Use for reporting notes for the following node,
  ;; which can be transformed and lose its original source code.
  (source-path nil :type list))

(defmethod print-object ((x ctran) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (when (boundp '*compilation*)
      (format stream "~D" (cont-num x)))))

;;; Linear VARiable. Multiple-value (possibly of unknown number)
;;; temporal storage.
(defstruct (lvar (:constructor make-lvar (&optional dest))
                 (:copier nil))
  ;; The node which receives this value. NIL only temporarily.
  (dest nil :type (or node null))
  ;; cached type of this lvar's value. If NIL, then this must be
  ;; recomputed: see LVAR-DERIVED-TYPE.
  (%derived-type nil :type (or ctype null))
  ;; the node (if unique) or a list of nodes where this lvar is used.
  (uses nil :type (or node list))
  ;; set to true when something about this lvar's value has
  ;; changed. See REOPTIMIZE-LVAR. This provides a way for IR1
  ;; optimize to determine which operands to a node have changed. If
  ;; the optimizer for this node type doesn't care, it can elect not
  ;; to clear this flag.
  (reoptimize t :type boolean)
  ;; the DYNAMIC-EXTENT of this lvar
  (dynamic-extent nil :type (or null cdynamic-extent))
  ;; something or other that the back end annotates this lvar with
  (info nil)
  ;; Nodes to reoptimize together with the lvar
  (dependent-nodes nil)
  (annotations nil)
  (dependent-annotations nil))
(!set-load-form-method lvar (:xc :target) :ignore-it)

;;; These are used for annotating a LVAR with information that can't
;;; be expressed using types or if the CAST semantics are undesirable
;;; (type derivation, runtime errors).
;;; Right now it's basically used for tracking constants and checking
;;; them for things like proper sequence, or valid type specifier.
(defstruct (lvar-annotation
            (:constructor nil))
  (source-path nil :type list)
  lexenv
  fired)

(defprinter (lvar-annotation)
  fired)

(defstruct (lvar-modified-annotation
            (:include lvar-annotation)
            (:copier nil))
  caller)

(defstruct (lvar-hook
            (:include lvar-annotation)
            (:copier nil))
  (hook #'missing-arg :type function))

(defstruct (lvar-type-spec-annotation
            (:include lvar-hook)
            (:copier nil)))

(defstruct (lvar-proper-sequence-annotation
            (:include lvar-annotation)
            (:copier nil))
  (kind 'proper-sequence :type (member proper-list proper-sequence
                                       proper-or-circular-list proper-or-dotted-list)))

(defstruct (lvar-dependent-annotation
            (:include lvar-annotation)
            (:copier nil))
    (deps nil :type list))

(defstruct (lvar-function-designator-annotation
            (:include lvar-dependent-annotation)
            (:copier nil))
  (caller nil :type symbol)
  (arg-specs nil :type list)
  (result-specs nil :type list)
  type)

(defstruct (lvar-sequence-bounds-annotation
            (:include lvar-dependent-annotation)
            (:copier nil)))

(defstruct (lvar-type-annotation
            (:include lvar-annotation)
            (:copier nil))
  type
  context)

(defstruct (lvar-function-annotation
            (:include lvar-type-annotation)
            (:copier nil)))

(defstruct (lvar-lambda-var-annotation
            (:include lvar-annotation)
            (:copier nil))
  lambda-var)

(defmethod print-object ((x lvar) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (when (boundp '*compilation*)
      (format stream "~D" (cont-num x)))))

(declaim (inline lvar-has-single-use-p))
(defun lvar-has-single-use-p (lvar)
  (typep (lvar-uses lvar) '(not list)))

;;; Return the unique node, delivering a value to LVAR.
(declaim (inline lvar-use))
(defun lvar-use (lvar)
  (the (not list) (lvar-uses lvar)))

(declaim (inline lvar-derived-type))
(defun lvar-derived-type (lvar)
  (declare (type lvar lvar))
  (or (lvar-%derived-type lvar)
      (setf (lvar-%derived-type lvar)
            (%lvar-derived-type lvar))))

(defstruct (node (:constructor nil)
                 (:include sset-element
                           (number (when (boundp '*compilation*)
                                     (incf (sset-counter *compilation*)))))
                 (:copier nil))
  ;; True if this node needs to be optimized. This is set to true
  ;; whenever something changes about the value of an lvar whose DEST
  ;; is this node.
  (reoptimize t :type boolean)
  ;; the ctran indicating what we do controlwise after evaluating this
  ;; node. This is null if the node is the last in its block.
  (next nil :type (or ctran null))
  ;; the ctran that this node is the NEXT of. This is null during IR1
  ;; conversion when we haven't linked the node in yet or in nodes
  ;; that have been deleted from the IR1 by UNLINK-NODE.
  (prev nil :type (or ctran null))
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
  ;; T. At the end of IR1 (in environment analysis) this is computed
  ;; for all nodes (after cleanup code has been emitted).  Before
  ;; then, a non-null value indicates that IR1 optimization has
  ;; converted a tail local call to a direct transfer.
  ;;
  ;; If the back-end breaks tail-recursion for some reason, then it
  ;; can null out this slot.
  (tail-p nil :type boolean))

(declaim (inline node-block))
(defun node-block (node)
  (ctran-block (node-prev node)))

(declaim (inline node-component))
(defun node-component (node)
  (declare (type node node))
  (block-component (node-block node)))

(defstruct (valued-node (:conc-name node-)
                        (:include node)
                        (:constructor nil)
                        (:copier nil))
  ;; the bottom-up derived type for this node.
  (derived-type *wild-type* :type ctype)
  ;; Lvar, receiving the values, produced by this node. May be NIL if
  ;; the value is unused.
  (lvar nil :type (or lvar null)))

(declaim (inline node-dest))
(defun node-dest (node)
  (awhen (node-lvar node) (lvar-dest it)))

;;; Flags that are used to indicate various things about a block, such
;;; as what optimizations need to be done on it:
;;; -- REOPTIMIZE is set when something interesting happens the uses of a
;;;    lvar whose DEST is in this block. This indicates that the
;;;    value-driven (forward) IR1 optimizations should be done on this block.
;;; -- FLUSH-P is set when code in this block becomes potentially flushable,
;;;    usually due to an lvar's DEST becoming null.
;;; -- TYPE-CHECK is true when the type check phase should be run on this
;;;    block. IR1 optimize can introduce new blocks after type check has
;;;    already run. We need to check these blocks, but there is no point in
;;;    checking blocks we have already checked.
;;; -- DELETE-P is true when this block is used to indicate that this block
;;;    has been determined to be unreachable and should be deleted. IR1
;;;    phases should not attempt to examine or modify blocks with DELETE-P
;;;    set, since they may:
;;;     - be in the process of being deleted, or
;;;     - have no successors.
(!def-boolean-attribute block
  reoptimize flush-p type-check delete-p)

(macrolet ((defattr (block-slot)
             `(defmacro ,block-slot (block)
                `(block-attributep
                  (block-flags ,block)
                  ,(symbolicate (subseq (string ',block-slot) 6))))))
  (defattr block-reoptimize)
  (defattr block-flush-p)
  (defattr block-type-check)
  (defattr block-delete-p))

;;; The LOOP structure holds information about a loop.
(defstruct (cloop (:conc-name loop-)
                  (:predicate nil)
                  (:constructor make-loop (kind head &optional tail))
                  (:copier nil))
  ;; The kind of loop that this is.  These values are legal:
  ;;
  ;;    :OUTER
  ;;        This is the outermost loop structure, and represents all the
  ;;        code in a component.
  ;;
  ;;    :NATURAL
  ;;        A normal loop with only one entry.
  ;;
  ;;    :STRANGE
  ;;        A segment of a "strange loop" in a non-reducible flow graph.
  (kind (missing-arg) :type (member :outer :natural :strange))
  ;; The first and last blocks in the loop.  There may be more than one tail,
  ;; since there may be multiple back branches to the same head.
  (head (missing-arg) :type cblock :read-only t)
  (tail nil :type list)
  ;; A list of all the blocks in this loop or its inferiors that have a
  ;; successor outside of the loop.
  (exits nil :type list)
  ;; The loop that this loop is nested within.  This is null in the outermost
  ;; loop structure.
  (superior nil :type (or cloop null))
  ;; A list of the loops nested directly within this one.
  (inferiors nil :type list)
  (depth 0 :type fixnum)
  ;; The head of the list of blocks directly within this loop.  We must recurse
  ;; on INFERIORS to find all the blocks.
  (blocks nil :type (or null cblock))
  ;; Backend saves the first emitted block of each loop here.
  (info nil))

(defprinter (cloop :conc-name loop-)
  kind
  head
  tail
  exits
  depth)

;;; The CBLOCK structure represents a basic block. We include
;;; SSET-ELEMENT so that we can have sets of blocks. Initially the
;;; SSET-ELEMENT-NUMBER is null, DFO analysis numbers in reverse DFO.
;;; During IR2 conversion, IR1 blocks are re-numbered in forward emit
;;; order. This latter numbering also forms the basis of the block
;;; numbering in the debug-info (though that is relative to the start
;;; of the function.)
(defstruct (cblock (:include sset-element)
                   (:constructor make-block (start))
                   (:constructor make-block-key (&key component pred succ start last))
                   (:copier nil)
                   (:conc-name block-)
                   (:predicate block-p))
  ;; a list of all the blocks that are predecessors/successors of this
  ;; block. In well-formed IR1, most blocks will have one successor.
  ;; The only exceptions are:
  ;;  1. component head blocks (any number)
  ;;  2. blocks ending in an IF (1 or 2)
  ;;  3. blocks with DELETE-P set (zero)
  (pred nil :type list)
  (succ nil :type list)
  ;; the ctran which heads this block (a :BLOCK-START), or NIL when we
  ;; haven't made the start ctran yet (and in the dummy component head
  ;; and tail blocks)
  (start nil :type (or ctran null))
  ;; the last node in this block. This is NIL when we are in the
  ;; process of building a block (and in the dummy component head and
  ;; tail blocks.)
  (last nil :type (or node null))
  ;; the forward and backward links in the depth-first ordering of the
  ;; blocks. These slots are NIL at beginning/end.
  (next nil :type (or null cblock))
  (prev nil :type (or null cblock))
  ;; This block's attributes: see above.
  (flags #.(block-attributes reoptimize flush-p type-check)
         :type attributes)
  ;; in constraint propagation: list of LAMBDA-VARs killed in this block
  ;; in copy propagation: list of killed TNs
  (kill nil)
  ;; other sets used in constraint propagation and/or copy propagation
  (in nil)
  (out nil)
  ;; The Loop structure for the innermost loop that contains this
  ;; block. Null only temporarily.
  (loop nil :type (or cloop null))
  ;; A link that holds together the list of blocks within Loop. Null
  ;; at the end or when we haven't determined it yet.
  (loop-next nil :type (or null cblock))
  ;; The immediate dominator of this block.
  (dominator nil :type (or null cblock))
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
  ;; what macroexpansions and source transforms happened "in" this block, used
  ;; for xref
  (xrefs nil :type list)
  ;; Cache the environment of a block during lifetime analysis. :NONE
  ;; if no cached value has been stored yet.
  (environment-cache :none :type (or null environment (member :none))))
(defmethod print-object ((cblock cblock) stream)
  (if (boundp '*compilation*)
      (print-unreadable-object (cblock stream :type t :identity t)
        (let ((component (block-component cblock)))
          (cond ((and component
                      (eq cblock
                          (component-head component)))
                 (format stream "~W :HEAD" (block-number cblock)))
                ((and component
                      (eq cblock
                          (component-tail component)))
                 (format stream "~W :TAIL" (block-number cblock)))
                (t
                 (format stream "~W :START c~W"
                         (block-number cblock)
                         (cont-num (block-start cblock)))))))
      (print-unreadable-object (cblock stream :type t :identity t))))

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
(defstruct (component (:copier nil)
                      (:constructor make-component
                       (head tail &aux (last-block tail)
                                       (outer-loop
                                        (make-loop :outer head (list tail)))))
                      #-sb-xc-host :no-constructor-defun)
  ;; space where this component will be allocated in
  ;; :auto won't make any codegen optimizations pertinent to immobile space,
  ;; but will place the code there given sufficient available space.
  (%mem-space nil :type (member nil :dynamic :immobile :auto))
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
  ;; predecessors/successors. The start and return from each
  ;; non-deleted function is linked to the component head and
  ;; tail. Until environment analysis links NLX entry stubs to the
  ;; component head, every successor of the head is a function start
  ;; (i.e. begins with a BIND node.)
  (head (missing-arg) :type cblock :read-only t)
  (tail (missing-arg) :type cblock :read-only t)
  ;; New blocks are inserted before this.
  (last-block (missing-arg) :type cblock)
  ;; This becomes a list of the CLAMBDA structures for all functions
  ;; in this component. OPTIONAL-DISPATCHes are represented only by
  ;; their XEP and other associated lambdas. This doesn't contain any
  ;; deleted or LET lambdas.
  ;;
  ;; Note that logical associations between CLAMBDAs and COMPONENTs
  ;; seem to exist for a while before this is initialized. See e.g.
  ;; the NEW-FUNCTIONALS slot. In particular, I got burned by writing
  ;; some code to use this value to decide which components need
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
  (new-functionals () :type list)
  ;; If this is :MAYBE, then there is stuff in this component that
  ;; could benefit from further IR1 optimization. T means that
  ;; reoptimization is necessary.
  (reoptimize t :type (member nil :maybe t))
  ;; For delay-ir1-transform to know when to delay
  (reoptimize-counter 0 :type fixnum)
  (phase-counter 0 :type fixnum)
  ;; If this is true, then the control flow in this component was
  ;; messed up by IR1 optimizations, so the DFO should be recomputed.
  (reanalyze nil :type boolean)
  ;; some sort of name for the code in this component
  (name "<unknown>" :type t)
  ;; some kind of info used by the back end.
  (info nil)
  ;; a map from combination nodes to things describing how an
  ;; optimization of the node failed. The description is an alist
  ;; (TRANSFORM . ARGS), where TRANSFORM is the structure describing
  ;; the transform that failed, and ARGS is either a list of format
  ;; arguments for the note, or the FUN-TYPE that would have
  ;; enabled the transformation but failed to match.
  (failed-optimizations (make-hash-table :test 'eq) :type hash-table)
  ;; This is similar to NEW-FUNCTIONALS, but is used when a function
  ;; has already been analyzed, but new references have been added by
  ;; inline expansion. Unlike NEW-FUNCTIONALS, this is not disjoint
  ;; from COMPONENT-LAMBDAS.
  (reanalyze-functionals nil :type list)
  (delete-blocks nil :type list)
  ;; The default LOOP in the component.
  (outer-loop (missing-arg) :type cloop)
  (renumber-p nil))

(defprinter (component :identity t)
  name
  (reanalyze :test reanalyze))

(declaim (inline reoptimize-component))
(defun reoptimize-component (component kind)
  (declare (type component component)
           (type (member :maybe t) kind))
  (unless (eq (component-reoptimize component) t)
    (setf (component-reoptimize component) kind)))

;;; Check that COMPONENT is suitable for roles which involve adding
;;; new code. (gotta love imperative programming with lotso in-place
;;; side effects...)
(defun aver-live-component (component)
  ;; FIXME: As of sbcl-0.pre7.115, we're asserting that
  ;; COMPILE-COMPONENT hasn't happened yet. Might it be even better
  ;; (certainly stricter, possibly also correct...) to assert that
  ;; IR1-FINALIZE hasn't happened yet?
  (aver (not (eql (component-info component) :dead))))

;;; A CLEANUP structure represents some dynamic binding action. Blocks
;;; are annotated with the current CLEANUP so that dynamic bindings
;;; can be removed when control is transferred out of the binding
;;; environment. We arrange for changes in dynamic bindings to happen
;;; at block boundaries, so that cleanup code may easily be inserted.
;;; The "mess-up" action is explicitly represented by a funny function
;;; call or ENTRY node.
;;;
;;; We guarantee that CLEANUPs only need to be done at block
;;; boundaries by requiring that the exit ctrans initially head their
;;; blocks, and then by not merging blocks when there is a cleanup
;;; change.
(defstruct (cleanup (:copier nil)
                    (:constructor make-cleanup
                        (kind &optional mess-up)))
  ;; the kind of thing that has to be cleaned up
  (kind (missing-arg)
        :type (member :special-bind :catch :unwind-protect
                      :block :tagbody :dynamic-extent
                      #-c-stack-is-control-stack :restore-nsp))
  ;; the node that messes things up. This is the last node in the
  ;; non-messed-up environment. Null only temporarily. This could be
  ;; deleted due to unreachability.
  (mess-up nil :type (or node null))
  ;; a list of all the NLX-INFO structures whose NLX-INFO-CLEANUP is
  ;; this cleanup.
  (nlx-info nil :type list)
  ;; Lexenv entries for the BLOCK special form, to be fixed up when
  ;; deleting blocks to allow inlining to find it again.
  (block nil))
(defprinter (cleanup :identity t)
  kind
  mess-up
  (nlx-info :test nlx-info))

;;; The ENVIRONMENT structure represents the result of environment analysis.
(defstruct (environment (:copier nil)
                        (:constructor make-environment (lambda))
                        #-sb-xc-host :no-constructor-defun)
  ;; the function that allocates this environment
  (lambda (missing-arg) :type clambda :read-only t)
  ;; a list of all the LAMBDA-VARs and NLX-INFOs needed from enclosing
  ;; environments by code in this environment.
  (closure nil :type list)
  ;; a list of NLX-INFO structures describing all the non-local exits
  ;; into this environment
  (nlx-info nil :type list)
  ;; some kind of info used by the back end
  (info nil))
(defprinter (environment :identity t)
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
(defstruct (tail-set
            (:constructor make-tail-set (funs)))
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

;;; An NLX-INFO structure is used to collect various information about
;;; non-local exits. This is effectively an annotation on the
;;; continuation, although it is accessed by searching in the
;;; ENVIRONMENT-NLX-INFO.
(defstruct (nlx-info
            (:copier nil)
            (:constructor make-nlx-info (cleanup block))
            #-sb-xc-host :no-constructor-defun)
  ;; the cleanup associated with this exit. In a catch or
  ;; unwind-protect, this is the :CATCH or :UNWIND-PROTECT cleanup,
  ;; and not the cleanup for the escape block. The CLEANUP-KIND of
  ;; this thus provides a good indication of what kind of exit is
  ;; being done.
  (cleanup (missing-arg) :type cleanup)
  ;; the continuation exited to. If this exit is from an escape function (CATCH or
  ;; UNWIND-PROTECT), then environment analysis deletes the
  ;; escape function and instead has the %NLX-ENTRY use this
  ;; continuation.
  ;;
  ;; This slot is used as a sort of name to allow us to find the
  ;; NLX-INFO that corresponds to a given exit. For this purpose, the
  ;; ENTRY must also be used to disambiguate, since exits to different
  ;; places may deliver their result to the same continuation.
  (block (missing-arg) :type cblock)
  ;; the entry stub inserted by environment analysis. This is a block
  ;; containing a call to the %NLX-ENTRY funny function that has the
  ;; original exit destination as its successor. Null only
  ;; temporarily.
  (target nil :type (or cblock null))
  ;; for a lexical exit it determines whether tag existence check is
  ;; needed
  (safe-p nil :type boolean)
  ;; some kind of info used by the back end
  (info nil))
(defprinter (nlx-info :identity t)
  block
  target
  info)
(!set-load-form-method nlx-info (:xc :target) :ignore-it)


;;;; LEAF structures

;;; Variables, constants and functions are all represented by LEAF
;;; structures. A reference to a LEAF is indicated by a REF node. This
;;; allows us to easily substitute one for the other without actually
;;; hacking the flow graph.
(defstruct (leaf (:include sset-element
                           (number (when (boundp '*compilation*)
                                     (incf (sset-counter *compilation*)))))
                 (:copier nil)
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
  ;; (We use .ANONYMOUS. here more or less the way we'd ordinarily use
  ;; NIL, but we're afraid to use NIL because it's a symbol which could
  ;; be the name of a leaf, if only the constant named NIL.)
  ;;
  ;; The value of this slot in can affect ordinary runtime behavior,
  ;; e.g. of special variables and known functions, not just debugging.
  ;;
  ;; See also the LEAF-DEBUG-NAME function and the
  ;; FUNCTIONAL-%DEBUG-NAME slot.
  (%source-name (missing-arg)
                ;; I guess we state the type this way to avoid calling
                ;; LEGAL-FUN-NAME-P unless absolutely necessary,
                ;; but this seems a bit of a premature optimization.
                :type (or symbol (and cons #-host-quirks-cmu (satisfies legal-fun-name-p)))
                :read-only t)
  ;; the type which values of this leaf must have
  (type *universal-type* :type ctype)
  ;; the type which values of this leaf have last been defined to have
  ;; (but maybe won't have in future, in case of redefinition)
  (defined-type *universal-type* :type ctype)
  ;; where the TYPE information came from (in order, from strongest to weakest):
  ;;  :DECLARED, from a declaration.
  ;;  :DECLARED-VERIFY
  ;;  :DEFINED-HERE, from examination of the definition in the same file.
  ;;  :DEFINED, from examination of the definition elsewhere.
  ;;  :DEFINED-METHOD, implicit, piecemeal declarations from CLOS.
  ;;  :ASSUMED, from uses of the object.
  (where-from :assumed :type (member :declared :declared-verify :assumed :defined-here :defined :defined-method))
  ;; list of the REF nodes for this leaf
  (refs () :type list)
  ;; For tracking whether to warn about unused variables:
  ;; NIL if there was never a REF or SET.
  ;; SET if there was a set but no REF.
  ;; T if there was a REF.
  ;; This may be non-nil when REFS and SETS are null, since code can be deleted.
  (ever-used nil :type (member nil set t initial-unused))
  ;; True if declared dynamic-extent.
  (dynamic-extent nil :type boolean)
  ;; some kind of info used by the back end
  (info nil))
(!set-load-form-method leaf (:xc :target) :ignore-it)

;;; LEAF name operations
(defun leaf-has-source-name-p (leaf)
  (not (eq (leaf-%source-name leaf)
           '.anonymous.)))
(defun leaf-source-name (leaf)
  (aver (leaf-has-source-name-p leaf))
  (leaf-%source-name leaf))

;;; The CONSTANT structure is used to represent known constant values.
;;; If NAME is not null, then it is the name of the named constant
;;; which this leaf corresponds to, otherwise this is an anonymous
;;; constant.
(defstruct (constant (:constructor make-constant (value
                                                  &optional
                                                  (type (ctype-of value))
                                                  (%source-name '.anonymous.)
                                                  &aux
                                                  (where-from :defined)))
                     (:copier nil)
                     (:include leaf))
  ;; the value of the constant
  (value (missing-arg) :type t))
(defprinter (constant :identity t)
  value (%source-name :test (neq %source-name '.anonymous.)))

;;; The BASIC-VAR structure represents information common to all
;;; variables which don't correspond to known local functions.
(defstruct (basic-var (:include leaf)
                      (:copier nil)
                      (:constructor nil))
  ;; Lists of the set nodes for this variable.
  (sets () :type list))

;;; The GLOBAL-VAR structure represents a value hung off of the symbol
;;; NAME.
(defstruct (global-var (:include basic-var)
                       (:copier nil)
                       (:constructor make-global-var (kind %source-name
                                                      &optional where-from
                                                                type defined-type)))
  ;; kind of variable described
  (kind (missing-arg)
        :type (member :special :global-function :global :unknown)))

(defun pretty-print-global-var (var stream)
  (let ((name (leaf-source-name var)))
    (princ (if (eq (global-var-kind var) :global-function)
               `(function ,name)
               name)
           stream)))

(defprinter (global-var :identity t
             :pretty-ir-printer
             (pretty-print-global-var structure stream))
  %source-name
  (type :test (not (eq type *universal-type*)))
  (defined-type :test (not (eq defined-type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  kind)

(defun fun-locally-defined-p (name env)
  (typecase env
    (null nil)
    #+(and sb-fasteval (not sb-xc-host))
    (sb-interpreter:basic-env
     (values (sb-interpreter:find-lexical-fun env name)))
    (t
     (let ((fun (cdr (assoc name (lexenv-funs env) :test #'equal))))
       (and fun (not (global-var-p fun)))))))

;;; A DEFINED-FUN represents a function that is defined in the same
;;; compilation block, or that has an inline expansion, or that has a
;;; non-NIL INLINEP value. Whenever we change the INLINEP state (i.e.
;;; an inline proclamation) we copy the structure so that former
;;; INLINEP values are preserved.
(defstruct (defined-fun (:include global-var
                         (where-from :defined)
                         (kind :global-function))
               (:constructor make-defined-fun
                   (%source-name type &optional where-from
                    &key kind
                         inline-expansion inlinep
                         same-block-p functional))
               (:copier nil))
  ;; The values of INLINEP and INLINE-EXPANSION initialized from the
  ;; global environment.
  (inlinep nil :type inlinep)
  (inline-expansion nil :type (or cons null))
  ;; Was the function defined in this compilation block?
  (same-block-p nil :type boolean)
  ;; The block-local definition of this function (either because it
  ;; was semi-inline, or because it was defined in this block). If
  ;; this function is not an entry point, then this may be deleted or
  ;; LET-converted. NULL if we haven't converted the expansion yet.
  (functional nil :type (or functional null)))
(defprinter (defined-fun :identity t
             :pretty-ir-printer (pretty-print-global-var structure stream))
  %source-name
  inlinep
  same-block-p
  (functional :test functional))

;;;; function stuff
(!def-boolean-attribute functional-kind
  nil optional deleted external toplevel
  escape cleanup let mv-let assignment
  zombie toplevel-xep)

(defmacro functional-kind-eq (functional &rest kinds)
  `(,(if (cdr kinds)
         'logtest
         'eql)
    (functional-kind ,functional)
    (functional-kind-attributes ,@kinds)))

(defmacro functional-kind-ecase (functional &body cases)
  (let ((kind (gensym)))
    `(let ((,kind (functional-kind ,functional)))
       (cond
         ,@(loop for (case* . forms) in cases
                 for case = (ensure-list case*)
                 collect
                 `((,(if (singleton-p case)
                         'eql
                         'logtest) ,kind (functional-kind-attributes ,@case))
                   ,@forms))
         (t (error "Unhandled functional-kind ~a"
                   (decode-functional-kind-attributes ,kind)))))))

(defmacro functional-kind-case (functional &body cases)
  (let ((kind (gensym)))
    `(let ((,kind (functional-kind ,functional)))
       (cond
         ,@(loop for (case* . forms) in cases
                 for case = (ensure-list case*)
                 collect
                 (if (eq case* t)
                     `(t ,@forms)
                     `((,(if (singleton-p case)
                             'eql
                             'logtest) ,kind (functional-kind-attributes ,@case))
                       ,@forms)))))))

;;; We default the WHERE-FROM and TYPE slots to :DEFINED and FUNCTION.
;;; We don't normally manipulate function types for defined functions,
;;; but if someone wants to know, an approximation is there.
(defstruct (functional (:include leaf
                        (%source-name '.anonymous.)
                        (where-from :defined)
                        (type (specifier-type 'function)))
                       (:constructor make-functional (&key kind info
                                                           %source-name %debug-name lexenv))
                       (:copier nil))
  ;; (For public access to this slot, use LEAF-DEBUG-NAME.)
  ;;
  ;; the name of FUNCTIONAL for debugging purposes, or NIL if we
  ;; should just let the SOURCE-NAME fall through
  ;;
  ;; Unlike the SOURCE-NAME slot, this slot's value should never
  ;; affect ordinary code behavior, only debugging/diagnostic behavior.
  ;;
  ;; Ha.  Ah, the starry-eyed idealism of the writer of the above
  ;; paragraph.  FUNCTION-LAMBDA-EXPRESSION's behaviour, as of
  ;; sbcl-0.7.11.x, differs if the name of the a function is a string
  ;; or not, as if it is a valid function name then it can look for an
  ;; inline expansion.
  ;;
  ;; E.g. for the function which implements (DEFUN FOO ...), we could
  ;; have
  ;;   %SOURCE-NAME=FOO
  ;;   %DEBUG-NAME=NIL
  ;; for the function which implements the top level form
  ;; (IN-PACKAGE :FOO) we could have
  ;;   %SOURCE-NAME=NIL
  ;;   %DEBUG-NAME=(TOP-LEVEL-FORM (IN-PACKAGE :FOO)
  ;; for the function which implements FOO in
  ;;   (DEFUN BAR (...) (FLET ((FOO (...) ...)) ...))
  ;; we could have
  ;;   %SOURCE-NAME=FOO
  ;;   %DEBUG-NAME=(FLET FOO)
  ;; and for the function which implements FOO in
  ;;   (DEFMACRO FOO (...) ...)
  ;; we could have
  ;;   %SOURCE-NAME=FOO (or maybe .ANONYMOUS.?)
  ;;   %DEBUG-NAME=(MACRO-FUNCTION FOO)
  (%debug-name nil
   :type (or null (not (satisfies legal-fun-name-p)))
   :read-only t)
  ;; some information about how this function is used. These values
  ;; are meaningful:
  ;;
  ;;    NIL
  ;;    an ordinary function, callable using local call
  ;;
  ;;    :LET
  ;;    a lambda that is used in only one local call, and has in
  ;;    effect been substituted directly inline. The return node is
  ;;    deleted, and the result is computed with the actual result
  ;;    lvar for the call.
  ;;
  ;;    :MV-LET
  ;;    Similar to :LET (as per FUNCTIONAL-LETLIKE-P), but the call
  ;;    is an MV-CALL.
  ;;
  ;;    :ASSIGNMENT
  ;;    similar to a LET (as per FUNCTIONAL-SOMEWHAT-LETLIKE-P), but
  ;;    can have more than one call as long as the calls all return to
  ;;    the same place.
  ;;
  ;;    :OPTIONAL
  ;;    a lambda that is an entry point for an OPTIONAL-DISPATCH.
  ;;    Similar to NIL, but requires greater caution, since local call
  ;;    analysis may create new references to this function. Also, the
  ;;    function cannot be deleted even if it has *no* references. The
  ;;    OPTIONAL-DISPATCH is in the LAMBDA-OPTIONAL-DISPATCH.
  ;;
  ;;    :EXTERNAL
  ;;    an external entry point lambda. The function it is an entry
  ;;    for is in the ENTRY-FUN slot.
  ;;
  ;;    :TOPLEVEL
  ;;    a top level lambda, holding a compiled top level form.
  ;;    Compiled very much like NIL, but provides an indication of
  ;;    top level context. A :TOPLEVEL lambda should have *no*
  ;;    references. Its ENTRY-FUN is a self-pointer.
  ;;
  ;;    :TOPLEVEL-XEP
  ;;    After a component is compiled, we clobber any top level code
  ;;    references to its non-closure XEPs with dummy FUNCTIONAL
  ;;    structures having this kind. This prevents the retained
  ;;    top level code from holding onto the IR for the code it
  ;;    references.
  ;;
  ;;    :ESCAPE
  ;;    :CLEANUP
  ;;    special functions used internally by CATCH and UNWIND-PROTECT.
  ;;    These are pretty much like a normal function (NIL), but are
  ;;    treated specially by local call analysis and stuff. Neither
  ;;    kind should ever be given an XEP even though they appear as
  ;;    args to funny functions. An :ESCAPE function is never actually
  ;;    called, and thus doesn't need to have code generated for it.
  ;;
  ;;    :DELETED
  ;;    This function has been found to be uncallable, and has been
  ;;    marked for deletion.
  ;;
  ;;    :ZOMBIE
  ;;    Effectless [MV-]LET; has no BIND node.
  (kind #.(functional-kind-attributes nil) :type attributes)
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
  ;; the value of any inline/notinline declaration for a local
  ;; function (or NIL in any case if no inline expansion is available)
  (inlinep nil :type inlinep)
  ;; If we have a lambda that can be used as in inline expansion for
  ;; this function, then this is it. If there is no source-level
  ;; lambda corresponding to this function then this is null (but then
  ;; INLINEP will always be NIL as well.)
  (inline-expansion nil :type list)
  ;; the lexical environment that the INLINE-EXPANSION should be converted in
  (lexenv *lexenv* :type lexenv)
  ;; the original function or macro lambda list, or :UNSPECIFIED if
  ;; this is a compiler created function
  (arg-documentation nil :type (or list (member :unspecified)))
  ;; the documentation string for the lambda
  (documentation nil :type (or null string))
  ;; the enclose node allocating the closure for this lambda. May be
  ;; NIL when we are sure that no closure is needed.
  (enclose nil :type (or null enclose))
  ;; various rare miscellaneous info that drives code generation & stuff
  (plist () :type list)
  ;; xref information for this functional (only used for functions with an
  ;; XEP)
  (xref () :type list)
  ;; True if this functional was created from an inline expansion.
  (inline-expanded nil :type boolean)
  ;; Is it coming from a top-level NAMED-LAMBDA?
  (top-level-defun-p nil)
  (ignore nil)
  (reanalyze nil))

(defun pretty-print-functional (functional stream)
  (let ((name (functional-debug-name functional)))
    (prin1 `(function
             ,(if (typep name '(cons (member xep tl-xep)))
                  (cadr name)
                  name))
           stream)))

(defprinter (functional :identity t
             :pretty-ir-printer (pretty-print-functional structure stream))
  %source-name
  %debug-name)

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

;;; Is FUNCTIONAL LET-converted? (where we're indifferent to whether
;;; it returns one value or multiple values)
(defmacro functional-letlike-p (functional)
  `(functional-kind-eq ,functional let mv-let))

;;; Is FUNCTIONAL sorta LET-converted? (where even an :ASSIGNMENT counts)
;;;
;;; FIXME: I (WHN) don't understand this one well enough to give a good
;;; definition or even a good function name, it's just a literal copy
;;; of a CMU CL idiom. Does anyone have a better name or explanation?
(defmacro functional-somewhat-letlike-p (functional)
  `(functional-kind-eq ,functional let mv-let assignment))

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
(defstruct (clambda (:include functional)
                    (:conc-name lambda-)
                    (:predicate lambda-p)
                    (:constructor make-clambda
                        (&key vars kind bind home %debug-name %source-name
                              lexenv allow-instrumenting))
                    (:copier nil))
  ;; list of LAMBDA-VAR descriptors for arguments
  (vars nil :type list)
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
  ;; the RETURN node for this LAMBDA, or NIL if it has been
  ;; deleted. This marks the end of the lambda, receiving the result
  ;; of the body. In a LET, the return node is deleted, and the body
  ;; delivers the value to the actual lvar. The return may also be
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
  ;; all the DYNAMIC-EXTENT nodes in this function and its LETs, or
  ;; null in a LET.
  (dynamic-extents nil :type list)
  ;; CLAMBDAs which are locally called by this lambda, and other
  ;; objects (closed-over LAMBDA-VARs and XEPs) which this lambda
  ;; depends on in such a way that DFO shouldn't put them in separate
  ;; components.
  (calls-or-closes (make-sset) :type (or null sset))
  ;; the TAIL-SET that this LAMBDA is in. This is null during creation
  ;; and in let lambdas.
  (tail-set nil :type (or tail-set null))
  ;; the structure which represents the phsical environment that this
  ;; function's variables are allocated in. This is filled in by
  ;; environment analysis. In a LET, this is EQ to our home's
  ;; environment.
  (environment nil :type (or environment null))
  ;; In a LET, this is the NODE-LEXENV of the combination node. We
  ;; retain it so that if the LET is deleted (due to a lack of vars),
  ;; we will still have caller's lexenv to figure out which cleanup is
  ;; in effect.
  (call-lexenv nil :type (or lexenv null))
  (allow-instrumenting *allow-instrumenting* :type boolean))
(defprinter (clambda :conc-name lambda- :identity t
             :pretty-ir-printer (pretty-print-functional structure stream))
  %source-name
  %debug-name
  (kind :princ (decode-functional-kind-attributes kind))
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  (vars :prin1 (mapcar #'leaf-source-name vars)))

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
  (or (functional-kind-eq clambda toplevel)
      (lambda-has-external-references-p clambda)))
(defun component-toplevelish-p (component)
  (member (component-kind component)
          '(:toplevel :complex-toplevel)))

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
(defstruct (optional-dispatch
            (:include functional) (:copier nil)
            (:constructor make-optional-dispatch
                (arglist allowp keyp %source-name %debug-name source-path))
            #-sb-xc-host :no-constructor-defun)
  ;; the original parsed argument list, for anyone who cares
  (arglist nil :type list)
  ;; true if &ALLOW-OTHER-KEYS was supplied
  (allowp nil :type boolean)
  ;; true if &KEY was specified (which doesn't necessarily mean that
  ;; there are any &KEY arguments..)
  (keyp nil :type boolean)
  (source-path)
  ;; the number of required arguments. This is the smallest legal
  ;; number of arguments.
  (min-args 0 :type unsigned-byte)
  ;; the total number of required and optional arguments. Args at
  ;; positions >= to this are &REST, &KEY or illegal args.
  (max-args 0 :type unsigned-byte)
  ;; list of the (maybe delayed) LAMBDAs which are the entry points
  ;; for non-rest, non-key calls. The entry for MIN-ARGS is first,
  ;; MIN-ARGS+1 second, ... MAX-ARGS last. The last entry-point always
  ;; calls the main entry; in simple cases it may be the main entry.
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
(defprinter (optional-dispatch :identity t
             :pretty-ir-printer (pretty-print-functional structure stream))
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
(defstruct (arg-info (:copier nil)
                     (:constructor make-arg-info (kind)))
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
  ;; NIL if supplied-p is only used for directing evaluation of init forms
  (supplied-used-p t :type boolean)
  ;; the default for a keyword or optional, represented as the
  ;; original Lisp code. This is set to NIL in &KEY arguments that are
  ;; defaulted using the SUPPLIED-P arg.
  ;;
  ;; For &REST arguments this may contain information about more context
  ;; the rest list comes from.
  (default nil :type t)
  (default-p nil :type boolean)
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
;;; LAMBDA-VARs with no REFs are considered to be deleted; environment
;;; analysis isn't done on these variables, so the back end must check
;;; for and ignore unreferenced variables. Note that a deleted
;;; LAMBDA-VAR may have sets; in this case the back end is still
;;; responsible for propagating the SET-VALUE to the set's CONT.
(!def-boolean-attribute lambda-var
  ;; true if this variable has been declared IGNORE
  ignore
  ;; This is set by environment analysis if it chooses an indirect
  ;; (value cell) representation for this variable because it is both
  ;; set and closed over.
  indirect
  ;; true if the last reference has been deleted (and new references
  ;; should not be made)
  deleted
  ;; This is set by environment analysis if, should it be an indirect
  ;; lambda-var, an actual value cell object must be allocated for
  ;; this variable because one or more of the closures that refer to
  ;; it are not dynamic-extent.  Note that both attributes must be set
  ;; for the value-cell object to be created.
  explicit-value-cell
  ;; Do not propagate constraints for this var
  no-constraints
  ;; Does it hold a constant that shouldn't be destructively modified
  constant
  unused-initial-value
  ;; Instruct constraint propagation to do some work.
  compute-same-refs
  no-debug)

(defstruct (lambda-var
            (:include basic-var) (:copier nil)
            (:constructor make-lambda-var
              (%source-name &key type where-from specvar source-form arg-info)))
  (flags #.(lambda-var-attributes) :type attributes)
  ;; the CLAMBDA that this var belongs to. This may be null when we are
  ;; building a lambda during IR1 conversion.
  (home nil :type (or null clambda))
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
  (constraints nil :type (or null t #| FIXME: conset |#))
  ;; Content-addressed indices for the CONSTRAINTs on this variable.
  ;; These are solely used by FIND-CONSTRAINT
  (ctype-constraints nil :type (or null hash-table))
  (eq-constraints    nil :type (or null hash-table))
  ;; sorted sets of constraints we like to iterate over
  (eql-var-constraints     nil :type (or null (vector t)))
  (inheritable-constraints nil :type (or null (vector t)))
  (equality-constraints    nil :type (or null (vector t)))
  (equality-constraints-hash nil :type (or null hash-table))
  source-form)

(defprinter (lambda-var :identity t)
  %source-name
  (type :test (not (eq type *universal-type*)))
  (where-from :test (not (eq where-from :assumed)))
  (flags :test (not (zerop flags))
         :prin1 (decode-lambda-var-attributes flags))
  (arg-info :test arg-info)
  (specvar :test specvar))

(defmacro lambda-var-ignorep (var)
  `(lambda-var-attributep (lambda-var-flags ,var) ignore))
(defmacro lambda-var-indirect (var)
  `(lambda-var-attributep (lambda-var-flags ,var) indirect))
(defmacro lambda-var-deleted (var)
  `(lambda-var-attributep (lambda-var-flags ,var) deleted))
(defmacro lambda-var-explicit-value-cell (var)
  `(lambda-var-attributep (lambda-var-flags ,var) explicit-value-cell))
(defmacro lambda-var-no-constraints (var)
  `(lambda-var-attributep (lambda-var-flags ,var) no-constraints))
(defmacro lambda-var-constant (var)
  `(lambda-var-attributep (lambda-var-flags ,var) constant))
(defmacro lambda-var-unused-initial-value (var)
  `(lambda-var-attributep (lambda-var-flags ,var) unused-initial-value))
(defmacro lambda-var-no-debug (var)
  `(lambda-var-attributep (lambda-var-flags ,var) no-debug))
(defmacro lambda-var-compute-same-refs (var)
  `(lambda-var-attributep (lambda-var-flags ,var) compute-same-refs))


;;;; basic node types

;;; A REF represents a reference to a LEAF. REF-REOPTIMIZE is
;;; initially (and forever) NIL, since REFs don't receive any values
;;; and don't have any IR1 optimizer.
(defstruct (ref (:include valued-node (reoptimize nil))
                (:constructor make-ref
                              (leaf
                               &optional (%source-name '.anonymous.)
                               &aux (leaf-type (leaf-type leaf))
                                    (derived-type
                                     (make-single-value-type leaf-type))))
                (:copier nil))
  ;; The leaf referenced.
  (leaf nil :type leaf)
  ;; KLUDGE: This is supposed to help with keyword debug messages somehow.
  (%source-name (missing-arg) :type symbol :read-only t)
  ;; An cons added by constraint-propagate to all REFs that have the
  ;; same value when referencing a lambda-var with sets.
  (same-refs nil :type (or null cons)))
(defprinter (ref :identity t)
  (%source-name :test (neq %source-name '.anonymous.))
  leaf)

(defstruct (multiple-successors-node
            (:constructor nil)
            (:include node)
            (:copier nil)
            (:conc-name node-)))

;;; Naturally, the IF node always appears at the end of a block.
(defstruct (cif (:include multiple-successors-node)
                (:conc-name if-)
                (:predicate if-p)
                (:constructor make-if (test consequent alternative))
                (:copier nil))
  ;; LVAR for the predicate
  (test (missing-arg) :type lvar)
  ;; the blocks that we execute next in true and false case,
  ;; respectively (may be the same)
  (consequent (missing-arg) :type cblock)
  (consequent-constraints nil :type (or null t #| FIXME: conset |#))
  (alternative (missing-arg) :type cblock)
  (alternative-constraints nil :type (or null t #| FIXME: conset |#)))
(defprinter (cif :conc-name if- :identity t)
  (test :prin1 (lvar-uses test))
  consequent
  alternative)

(defstruct (jump-table (:include multiple-successors-node)
                       (:constructor make-jump-table (index))
                       (:copier nil)
                       #-sb-xc-host :no-constructor-defun)
  (index (missing-arg) :type lvar)
  (targets nil :type list))

(defprinter (jump-table :identity t)
  index
  targets)

(defstruct (cset (:include valued-node
                           (derived-type (make-single-value-type *universal-type*)))
                 (:conc-name set-)
                 (:predicate set-p)
                 (:constructor make-set (var value))
                 (:copier nil)
                 #-sb-xc-host :no-constructor-defun)
  ;; descriptor for the variable set
  (var (missing-arg) :type basic-var)
  ;; LVAR for the value form
  (value (missing-arg) :type lvar))
(defprinter (cset :conc-name set- :identity t)
  var
  (value :prin1 (lvar-uses value)))

(defvar *inline-expansion-limit* 50
  "an upper limit on the number of inline function calls that will be expanded
   in any given code object (single function or block compilation)")

(defvar *inline-expansions* nil)
(declaim (list *inline-expansions*)
         (always-bound *inline-expansions*))

;;; The BASIC-COMBINATION structure is used to represent both normal
;;; and multiple value combinations. In a let-like function call, this
;;; node appears at the end of its block and the body of the called
;;; function appears as the successor; the NODE-LVAR is null.
(defstruct (basic-combination (:include valued-node)
                              (:constructor nil)
                              (:copier nil))
  ;; LVAR for the function
  (fun (missing-arg) :type lvar)
  ;; list of LVARs for the args. In a local call, an argument lvar may
  ;; be replaced with NIL to indicate that the corresponding variable
  ;; is unreferenced, and thus no argument value need be passed.
  (args nil :type list)
  ;; the kind of function call being made. :LOCAL means that this is a
  ;; local call to a function in the same component, and that argument
  ;; syntax checking has been done, etc.  Calls to known global
  ;; functions are represented by storing :KNOWN in this slot and the
  ;; FUN-INFO for that function in the FUN-INFO slot.  :FULL is a call
  ;; to an (as yet) unknown function, or to a known function declared
  ;; NOTINLINE. :ERROR is like :FULL, but means that we have
  ;; discovered that the call contains an error, and should not be
  ;; reconsidered for optimization.
  (kind :full :type (member :local :full :error :known
                            :unknown-keys))
  ;; if a call to a known global function, contains the FUN-INFO.
  (fun-info nil :type (or fun-info null))
  ;; Untrusted type we have asserted for this combination.
  (type-validated-for-leaf nil)
  ;; some kind of information attached to this node by the back end
  ;; or by CHECK-IMPORTANT-RESULT, or by anything else that may need to.
  (info nil)
  (step-info nil)
  ;; A plist of inline expansions
  (inline-expansions *inline-expansions* :type list :read-only t)
  ;; Current COMPONENT-REOPTIMIZE-COUNTER set when calling delay-ir1-transform.
  (delay-to -1 :type fixnum)
  (constraints-in)
  #+() (constraints-out))

;;; The COMBINATION node represents all normal function calls,
;;; including FUNCALL. This is distinct from BASIC-COMBINATION so that
;;; an MV-COMBINATION isn't COMBINATION-P.
(defstruct (combination (:include basic-combination)
                        (:constructor make-combination (fun))
                        (:copier nil))
  (pass-nargs t :type boolean)
  (or-chain-computed nil :type boolean))
(defprinter (combination :identity t)
  (fun :prin1 (lvar-uses fun))
  (args :prin1 (mapcar (lambda (x)
                         (if x
                             (lvar-uses x)
                             "<deleted>"))
                       args)))

;;; An MV-COMBINATION is to MULTIPLE-VALUE-CALL as a COMBINATION is to
;;; FUNCALL. This is used to implement all the multiple-value
;;; receiving forms.
(defstruct (mv-combination (:include basic-combination)
                           (:constructor make-mv-combination (fun))
                           (:copier nil)
                           #-sb-xc-host :no-constructor-defun))
(defprinter (mv-combination)
  (fun :prin1 (lvar-uses fun))
  (args :prin1 (mapcar #'lvar-uses args)))

;;; The BIND node marks the beginning of a lambda body and represents
;;; the creation and initialization of the variables.
(defstruct (bind (:include node)
                 (:copier nil)
                 (:constructor make-bind ()))
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
                    (:constructor make-return (result lambda))
                    (:copier nil)
                    #-sb-xc-host :no-constructor-defun)
  ;; the lambda we are returning from. Null temporarily during
  ;; ir1tran.
  (lambda nil :type (or clambda null))
  ;; the lvar which yields the value of the lambda
  (result (missing-arg) :type lvar)
  ;; the union of the node-derived-type of all uses of the result
  ;; other than by a local call, intersected with the result's
  ;; asserted-type. If there are no non-call uses, this is
  ;; *EMPTY-TYPE*.
  (result-type *wild-type* :type ctype))
(defprinter (creturn :conc-name return- :identity t)
  lambda
  result-type)

;;; The CAST node represents type assertions. The check for
;;; TYPE-TO-CHECK is performed and then the VALUE is declared to be of
;;; type ASSERTED-TYPE.
(defstruct (cast (:include valued-node)
                 (:copier nil)
                 (:constructor %make-cast
                     (asserted-type type-to-check value derived-type context))
                 #-sb-xc-host :no-constructor-defun)
  (asserted-type (missing-arg) :type ctype)
  (type-to-check (missing-arg) :type ctype)
  ;; an indication of what we have proven about how this type
  ;; assertion is satisfied:
  ;;
  ;; NIL
  ;;    No type check is necessary (proven type of VALUE is a subtype of the TYPE-TO-CHECK.)
  ;;
  ;; :EXTERNAL
  ;;    A type check will be performed by NODE-DEST.
  ;;
  ;; T
  ;;    A type check is needed.
  (%type-check t :type (member t :external nil))
  ;; the lvar which is checked
  (value (missing-arg) :type lvar)
  (context nil)
  ;; Avoid compile time type conflict warnings.
  ;; Used by things that expand into ETYPECASE.
  (silent-conflict nil :type (or boolean (eql :style-warning))))
(defprinter (cast :identity t)
  %type-check
  value
  asserted-type
  type-to-check)

;;; The DELAY node is interposed between a VALUE's USE and its DEST in
;;; order to allow the value to be immediately used. This allows us to
;;; do substitution of lvars without doing flow analysis to check the
;;; validity of the substitution in certain cases.
(defstruct (delay (:include cast
                   (%type-check nil)
                   (asserted-type *wild-type*)
                   (type-to-check *wild-type*))
                  (:constructor make-delay (value))
                  (:copier nil)))

;;; Inserted by ARRAY-CALL-TYPE-DERIVER so that it can be later deleted
(defstruct (array-index-cast
            (:include cast) (:copier nil)
            (:constructor make-array-index-cast
                (asserted-type type-to-check value derived-type))))

;;;; non-local exit support
;;;;
;;;; In IR1, we insert special nodes to mark potentially non-local
;;;; lexical exits.

;;; The ENTRY node serves to mark the start of the dynamic extent of a
;;; lexical exit. It is the mess-up node for the corresponding :ENTRY
;;; cleanup.
(defstruct (entry (:include node)
                  (:copier nil)
                  (:constructor make-entry ()))
  ;; All of the EXIT nodes for potential non-local exits to this point.
  (exits nil :type list)
  ;; The cleanup for this entry. NULL only temporarily.
  (cleanup nil :type (or cleanup null)))
(defprinter (entry :identity t))

;;; The EXIT node marks the place at which exit code would be emitted,
;;; if necessary. This is interposed between the uses of the exit
;;; continuation and the exit continuation's DEST. Instead of using
;;; the returned value being delivered directly to the exit
;;; continuation, it is delivered to our VALUE lvar. The original exit
;;; lvar is the exit node's LVAR; environment analysis also makes it
;;; the lvar of %NLX-ENTRY call.
(defstruct (exit (:include valued-node)
                 (:copier nil)
                 (:constructor make-exit (&optional entry value)))
  ;; the ENTRY node that this is an exit for. If null, this is a
  ;; degenerate exit. A degenerate exit is used to "fill" an empty
  ;; block (which isn't allowed in IR1.) In a degenerate exit, Value
  ;; is always also null.
  (entry nil :type (or entry null))
  ;; the lvar yielding the value we are to exit with. If NIL, then no
  ;; value is desired (as in GO).
  (value nil :type (or lvar null))
  (nlx-info nil :type (or nlx-info null)))
(defprinter (exit :identity t)
  (entry :test entry)
  (value :test value))

;;; The ENCLOSE node marks the place at which closure allocation code
;;; would be emitted, if necessary.
(defstruct (enclose (:include node)
                    (:copier nil)
                    (:constructor make-enclose (funs)))
  ;; The list of functionals that this ENCLOSE node allocates.
  (funs nil :type list)
  ;; The dynamic extent for this enclose if any of its functionals are
  ;; declared dynamic extent.
  (dynamic-extent nil :type (or null cdynamic-extent))
  ;; The union of all dynamic extents inferred by the compiler for
  ;; this enclose's functionals. NULL when there is a declared dynamic
  ;; extent.
  (derived-dynamic-extents () :type list))
(defprinter (enclose :identity t)
  funs)

;;; The DYNAMIC-EXTENT node is used to accumulate information about a
;;; dynamic extent declaration. It is the mess-up for the
;;; corresponding :DYNAMIC-EXTENT cleanup.
(defstruct (cdynamic-extent (:include node)
                            (:conc-name dynamic-extent-)
                            (:predicate dynamic-extent-p)
                            (:constructor make-dynamic-extent (&optional values))
                            (:copier nil))
  ;; the values explicitly declared with this dynamic extent.
  (values nil :type list)
  ;; the cleanup for this extent.
  (cleanup nil :type (or cleanup null))
  ;; some kind of info used by the back end
  (info nil))

(defprinter (cdynamic-extent :conc-name dynamic-extent-
                             :identity t)
  values
  (info :test info))


;;;; miscellaneous IR1 structures

(defstruct (undefined-warning
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

(declaim (freeze-type undefined-warning))

(defstruct (argument-mismatch-warning
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s :type t)
                               (prin1 (argument-mismatch-warning-name x) s))))
            (:copier nil))
  (name nil :type (or symbol list))
  ;; a list of (KEYS . COMPILER-ERROR-CONTEXT)
  (warnings () :type list))


;;; a helper for the POLICY macro, defined late here so that the
;;; various type tests can be inlined
;;; You might think that NIL as a policy becomes *POLICY*,
;;; but no, NIL was always an empty alist representing no qualities,
;;; which is a valid policy that makes each quality read as 1.
;;; In contrast, a LEXENV with NIL policy _does_ become *POLICY*.
;;; The reason for NIL mapping to baseline is that all nodes are annotated
;;; with a LEXENV, and the only object type that can be a LEXENV is LEXENV.
;;; An indicator is needed that a LEXENV is devoid of a policy, so this is
;;; what the NIL is for in lexenv-policy. But sometimes the compiler needs
;;; a policy without reference to an IR object - which is weird - and in that
;;; case it has nothing better to go with but the baseline policy.
;;; It still seems like a bug though.
(defun %coerce-to-policy (thing)
  (typecase thing
    (policy thing)
    #+(and sb-fasteval (not sb-xc-host))
    (sb-interpreter:basic-env (sb-interpreter:env-policy thing))
    ;; Why not *policy*?
    (null **baseline-policy**)
    (t (lexenv-policy (etypecase thing
                        (lexenv thing)
                        (node (node-lexenv thing))
                        (functional (functional-lexenv thing)))))))

;;;; Freeze some structure types to speed type testing.

(declaim (freeze-type node lexenv ctran lvar cblock component cleanup
                      environment tail-set nlx-info leaf))
