;;;; Define IR1 boolean attributes and the FUN-INFO structure

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; IR1 boolean function attributes
;;;
;;; There are a number of boolean attributes of known functions which
;;; we like to have in IR1. This information is mostly side effect
;;; information of a sort, but it is different from the kind of
;;; information we want in IR2. We aren't interested in a fine
;;; breakdown of side effects, since we do very little code motion on
;;; IR1. We are interested in some deeper semantic properties such as
;;; whether it is safe to pass stack closures to.
;;;
;;; FIXME: This whole notion of "bad" explicit attributes is bad for
;;; maintenance. How confident are we that we have no defknowns for functions
;;; with functional arguments that are missing the CALL attribute? Much better
;;; to have NO-CALLS, as it is much less likely to break accidentally.
(!def-boolean-attribute ir1
  ;; may call functions that are passed as arguments. In order to
  ;; determine what other effects are present, we must find the
  ;; effects of all arguments that may be functions.
  call
  ;; may fail to return during correct execution. Errors are O.K.
  ;; UNUSED, BEWARE OF BITROT.
  unwind
  ;; the (default) worst case. Includes all the other bad things, plus
  ;; any other possible bad thing. If this is present, the above bad
  ;; attributes will be explicitly present as well.
  any
  ;; all arguments are safe for dynamic extent.
  ;; (We used to have an UNSAFE attribute, which was basically the inverse
  ;; of this, but it was unused and bitrotted, so when we started making
  ;; use of the information we flipped the name and meaning the safe way
  ;; around.)
  dx-safe
  ;; may be constant-folded. The function has no side effects, but may
  ;; be affected by side effects on the arguments. e.g. SVREF, MAPC.
  ;; Functions that side-effect their arguments are not considered to
  ;; be foldable. Although it would be "legal" to constant fold them
  ;; (since it "is an error" to modify a constant), we choose not to
  ;; mark these functions as foldable in this database.
  foldable
  ;; may be eliminated if value is unused. The function has no side
  ;; effects except possibly cons. If a function might signal errors,
  ;; then it is not flushable even if it is movable, foldable or
  ;; unsafely-flushable. Implies UNSAFELY-FLUSHABLE. (In safe code
  ;; type checking of arguments is always performed by the caller, so
  ;; a function which SHOULD signal an error if arguments are not of
  ;; declared types may be FLUSHABLE.)
  flushable
  ;; unsafe call may be eliminated if value is unused. The function
  ;; has no side effects except possibly cons and signalling an error
  ;; in the safe code. If a function MUST signal errors, then it is
  ;; not unsafely-flushable even if it is movable or foldable.
  unsafely-flushable
  ;; return value is important, and ignoring it is probably a mistake.
  ;; Unlike the other attributes, this is used only for style
  ;; warnings and has no effect on optimization.
  important-result
  ;; may be moved with impunity. Has no side effects except possibly
  ;; consing, and is affected only by its arguments.
  ;; UNUSED, BEWARE OF BITROT.
  movable
  ;; The function is a true predicate likely to be open-coded. Convert
  ;; any non-conditional uses into (IF <pred> T NIL). Not usually
  ;; specified to DEFKNOWN, since this is implementation dependent,
  ;; and is usually automatically set by the DEFINE-VOP :CONDITIONAL
  ;; option.
  predicate
  ;; Inhibit any warning for compiling a recursive definition.
  ;; (Normally the compiler warns when compiling a recursive
  ;; definition for a known function, since it might be a botched
  ;; interpreter stub.)
  recursive
  ;; The function should always be translated by a VOP (i.e. it should
  ;; should never be converted into a full call).  This is used strictly
  ;; as a consistency checking mechanism inside the compiler during IR2
  ;; transformation.
  always-translatable
  ;; If a function is called with two arguments and the first one is a
  ;; constant, then the arguments will be swapped.
  commutative
  ;; Reoptimize this function if the node that follows it gets unlinked.
  reoptimize-when-unlinking
  ;; The function does not verify the arg count and must be always
  ;; called with the right arguments and can avoid passing NARGS.
  no-verify-arg-count
  ;; Arguments are can be passed unboxed, no type checking on entry is
  ;; performed, and the number of arguments passed in registers can be
  ;; greater than the standard number. Only fixed arguments can be used.
  fixed-args
  unboxed-return)

(defstruct (fun-info (:copier nil)
                     #-sb-xc-host (:pure t))
  ;; boolean attributes of this function.
  (attributes (missing-arg) :type attributes)
  ;; TRANSFORM structures describing transforms for this function
  (transforms () :type list)
  ;; a function which computes the derived type for a call to this
  ;; function by examining the arguments. This is null when there is
  ;; no special method for this function.
  (derive-type nil :type (or function null))
  ;; a function that does various unspecified code transformations by
  ;; directly hacking the IR. Returns true if further optimizations of
  ;; the call shouldn't be attempted.
  ;;
  ;; KLUDGE: This return convention (non-NIL if you shouldn't do
  ;; further optimiz'ns) is backwards from the return convention for
  ;; transforms. -- WHN 19990917
  (optimizer nil :type (or function null))
  ;; If true, a special-case LTN annotation method that is used in
  ;; place of the standard type/policy template selection. It may use
  ;; arbitrary code to choose a template, decide to do a full call, or
  ;; conspire with the IR2-CONVERT method to do almost anything. The
  ;; COMBINATION node is passed as the argument.
  (ltn-annotate nil :type (or function null))
  ;; If true, the special-case IR2 conversion method for this
  ;; function. This deals with funny functions, and anything else that
  ;; can't be handled using the template mechanism. The COMBINATION
  ;; node and the IR2-BLOCK are passed as arguments.
  (ir2-convert nil :type (or function null))
  ;; Called before IR2 conversion, just like IR2-CONVERT above
  ;; Currently used for issuing warnings so that it doesn't intefere
  ;; with things like CALL-FULL-LIKE-P due to IR2-CONVERT.
  (ir2-hook nil :type (or function null))
  ;; If true, the function can stack-allocate the result. The
  ;; COMBINATION node is passed as an argument.
  (stack-allocate-result nil :type (or function null))
  ;; If true, the function can add flow-sensitive type information
  ;; about the state of the world after its execution. The COMBINATION
  ;; node is passed as an argument, along with the current set of
  ;; active constraints for the block.  The function returns a
  ;; sequence of constraints; a constraint is a triplet of a
  ;; constraint kind (a symbol, see (defstruct (constraint ...)) in
  ;; constraint.lisp) and arguments, either LVARs, LAMBDA-VARs, or
  ;; CTYPEs.  If any of these arguments is NIL, the constraint is
  ;; skipped. This simplifies integration with OK-LVAR-LAMBDA-VAR,
  ;; which maps LVARs to LAMBDA-VARs.  An optional fourth value in
  ;; each constraint flips the meaning of the constraint if it is
  ;; non-NIL.
  (constraint-propagate nil :type (or function null))
  ;; Propagating stuff back to the arguments based on the constraints on
  ;; the result of this combination.
  (constraint-propagate-back nil :type (or function null))
  (constraint-propagate-result nil :type (or function null))
  ;; If true, the function can add flow-sensitive type information
  ;; depending on the truthiness of its return value.  Returns two
  ;; values, a LVAR and a CTYPE. The LVAR is of that CTYPE iff the
  ;; function returns true.
  ;; It may also return additional third and fourth values. Each is
  ;; a sequence of constraints (see CONSTRAINT-PROPAGATE), for the
  ;; consequent and alternative branches, respectively.
  (constraint-propagate-if nil :type (or function null))
  (equality-constraint nil :type (or function null))
  ;; all the templates that could be used to translate this function
  ;; into IR2, sorted by increasing cost.
  (templates nil :type list)
  ;; If non-null, then this function is a unary type predicate for
  ;; this type.
  (predicate-type nil :type (or ctype null))
  ;; If non-null, the index of the argument which becomes the result
  ;; of the function.
  (result-arg nil :type (or index null))
  ;; Customizing behavior of ASSERT-CALL-TYPE
  (call-type-deriver nil :type (or function null))
  annotation
  ;; For functions with unboxed args/returns
  (folder nil :type (or function null))
  (externally-checkable-type nil :type (or function null))
  (constants nil :type (or function null)))

(defprinter (fun-info)
  (attributes :test (not (zerop attributes))
              :prin1 (decode-ir1-attributes attributes))
  (transforms :test transforms)
  (derive-type :test derive-type)
  (optimizer :test optimizer)
  (ltn-annotate :test ltn-annotate)
  (ir2-convert :test ir2-convert)
  (templates :test templates)
  (predicate-type :test predicate-type))
