;;;; This file contains stuff for maintaining a database of special
;;;; information about functions known to the compiler. This includes
;;;; semantic information such as side effects and type inference
;;;; functions as well as transforms and IR2 translators.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(/show0 "knownfun.lisp 17")

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
  ;; The function does explicit argument type checking, so the
  ;; declared type should not be asserted when a definition is
  ;; compiled.
  explicit-check
  ;; The function should always be translated by a VOP (i.e. it should
  ;; should never be converted into a full call).  This is used strictly
  ;; as a consistency checking mechanism inside the compiler during IR2
  ;; transformation.
  always-translatable
  ;; If a function is called with two arguments and the first one is a
  ;; constant, then the arguments will be swapped.
  commutative)

(defstruct (fun-info #-sb-xc-host (:pure t))
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
  ;; a function computing the constant or literal arguments which are
  ;; destructively modified by the call.
  (destroyed-constant-args nil :type (or function null))
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
  ;; If true, the function can add flow-sensitive type information
  ;; depending on the truthiness of its return value.  Returns two
  ;; values, a LVAR and a CTYPE. The LVAR is of that CTYPE iff the
  ;; function returns true.
  ;; It may also return additional third and fourth values. Each is
  ;; a sequence of constraints (see CONSTRAINT-PROPAGATE), for the
  ;; consequent and alternative branches, respectively.
  (constraint-propagate-if nil :type (or function null))
  ;; all the templates that could be used to translate this function
  ;; into IR2, sorted by increasing cost.
  (templates nil :type list)
  ;; If non-null, then this function is a unary type predicate for
  ;; this type.
  (predicate-type nil :type (or ctype null))
  ;; If non-null, the index of the argument which becomes the result
  ;; of the function.
  (result-arg nil :type (or index null)))

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

;;;; interfaces to defining macros

;;; an IR1 transform
(defstruct (transform (:copier nil))
  ;; the function type which enables this transform.
  ;;
  ;; (Note that declaring this :TYPE FUN-TYPE probably wouldn't
  ;; work because some function types, like (SPECIFIER-TYPE 'FUNCTION0
  ;; itself, are represented as BUILT-IN-TYPE, and at least as of
  ;; sbcl-0.pre7.54 or so, that's inconsistent with being a
  ;; FUN-TYPE.)
  (type (missing-arg) :type ctype)
  ;; the transformation function. Takes the COMBINATION node and
  ;; returns a lambda expression, or throws out.
  (function (missing-arg) :type function)
  ;; string used in efficiency notes
  (note (missing-arg) :type string)
  ;; T if we should emit a failure note even if SPEED=INHIBIT-WARNINGS.
  (important nil :type (member t nil)))

(defprinter (transform) type note important)

;;; Grab the FUN-INFO and enter the function, replacing any old
;;; one with the same type and note.
(declaim (ftype (function (t list function &optional (or string null)
                             (member t nil))
                          *)
                %deftransform))
(defun %deftransform (name type fun &optional note important)
  (let* ((ctype (specifier-type type))
         (note (or note "optimize"))
         (info (fun-info-or-lose name))
         (old (find-if (lambda (x)
                         (and (type= (transform-type x) ctype)
                              (string-equal (transform-note x) note)
                              (eq (transform-important x) important)))
                       (fun-info-transforms info))))
    (cond (old
           (style-warn 'redefinition-with-deftransform
                       :transform old)
           (setf (transform-function old) fun
                 (transform-note old) note))
          (t
           (push (make-transform :type ctype :function fun :note note
                                 :important important)
                 (fun-info-transforms info))))
    name))

;;; Make a FUN-INFO structure with the specified type, attributes
;;; and optimizers.
(declaim (ftype (function (list list attributes &key
                                (:derive-type (or function null))
                                (:optimizer (or function null))
                                (:destroyed-constant-args (or function null))
                                (:result-arg (or index null))
                                (:overwrite-fndb-silently boolean))
                          *)
                %defknown))
(defun %defknown (names type attributes
                  &key derive-type optimizer destroyed-constant-args result-arg
                    overwrite-fndb-silently)
  (let ((ctype (specifier-type type))
        (info (make-fun-info :attributes attributes
                             :derive-type derive-type
                             :optimizer optimizer
                             :destroyed-constant-args destroyed-constant-args
                             :result-arg result-arg)))
    (dolist (name names)
      (unless overwrite-fndb-silently
        (let ((old-fun-info (info :function :info name)))
          (when old-fun-info
            ;; This is handled as an error because it's generally a bad
            ;; thing to blow away all the old optimization stuff. It's
            ;; also a potential source of sneaky bugs:
            ;;    DEFKNOWN FOO
            ;;    DEFTRANSFORM FOO
            ;;    DEFKNOWN FOO ; possibly hidden inside some macroexpansion
            ;;    ; Now the DEFTRANSFORM doesn't exist in the target Lisp.
            ;; However, it's continuable because it might be useful to do
            ;; it when testing new optimization stuff interactively.
            (cerror "Go ahead, overwrite it."
                    "~@<overwriting old FUN-INFO ~2I~_~S ~I~_for ~S~:>"
                    old-fun-info name))))
      (setf (info :function :type name) ctype)
      (setf (info :function :where-from name) :declared)
      (setf (info :function :kind name) :function)
      (setf (info :function :info name) info)))
  names)

;;; Return the FUN-INFO for NAME or die trying. Since this is
;;; used by callers who want to modify the info, and the info may be
;;; shared, we copy it. We don't have to copy the lists, since each
;;; function that has generators or transforms has already been
;;; through here.
;;;
;;; Note that this operation is somewhat garbage-producing in the current
;;; globaldb implementation.  Setting a piece of INFO for a name makes
;;; a shallow copy of the name's info-vector. FUN-INFO-OR-LOSE sounds
;;; like a data reader, and you might be disinclined to think that it
;;; copies at all, but:
;;;   (TIME (LOOP REPEAT 1000 COUNT (FUN-INFO-OR-LOSE '*)))
;;;   294,160 bytes consed
;;; whereas just copying the info per se is not half as bad:
;;;   (LET ((X (INFO :FUNCTION :INFO '*)))
;;;     (TIME (LOOP REPEAT 1000 COUNT (COPY-FUN-INFO X))))
;;;   130,992 bytes consed
;;;
(declaim (ftype (sfunction (t) fun-info) fun-info-or-lose))
(defun fun-info-or-lose (name)
    (let ((old (info :function :info name)))
      (unless old (error "~S is not a known function." name))
      (setf (info :function :info name) (copy-fun-info old))))

;;;; generic type inference methods

;;; Derive the type to be the type of the xxx'th arg. This can normally
;;; only be done when the result value is that argument.
(defun result-type-first-arg (call)
  (declare (type combination call))
  (let ((lvar (first (combination-args call))))
    (when lvar (lvar-type lvar))))
(defun result-type-last-arg (call)
  (declare (type combination call))
  (let ((lvar (car (last (combination-args call)))))
    (when lvar (lvar-type lvar))))

;;; Derive the result type according to the float contagion rules, but
;;; always return a float. This is used for irrational functions that
;;; preserve realness of their arguments.
(defun result-type-float-contagion (call)
  (declare (type combination call))
  (reduce #'numeric-contagion (combination-args call)
          :key #'lvar-type
          :initial-value (specifier-type 'single-float)))

;;; Return a closure usable as a derive-type method for accessing the
;;; N'th argument. If arg is a list, result is a list. If arg is a
;;; vector, result is a vector with the same element type.
(defun sequence-result-nth-arg (n)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when lvar
        (let ((type (lvar-type lvar)))
          (if (array-type-p type)
              (specifier-type
               `(vector ,(type-specifier (array-type-element-type type))))
              (let ((ltype (specifier-type 'list)))
                (when (csubtypep type ltype)
                  ltype))))))))

;;; Derive the type to be the type specifier which is the Nth arg.
(defun result-type-specifier-nth-arg (n)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when (and lvar (constant-lvar-p lvar))
        (careful-specifier-type (lvar-value lvar))))))

;;; Derive the type to be the type specifier which is the Nth arg,
;;; with the additional restriptions noted in the CLHS for STRING and
;;; SIMPLE-STRING, defined to specialize on CHARACTER, and for VECTOR
;;; (under the page for MAKE-SEQUENCE).
(defun creation-result-type-specifier-nth-arg (n)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when (and lvar (constant-lvar-p lvar))
        (let* ((specifier (lvar-value lvar))
               (lspecifier (if (atom specifier) (list specifier) specifier)))
          (cond
            ((eq (car lspecifier) 'string)
             (destructuring-bind (string &rest size)
                 lspecifier
               (declare (ignore string))
               (careful-specifier-type
                `(vector character ,@(when size size)))))
            ((eq (car lspecifier) 'simple-string)
             (destructuring-bind (simple-string &rest size)
                 lspecifier
               (declare (ignore simple-string))
               (careful-specifier-type
                `(simple-array character ,@(if size (list size) '((*)))))))
            (t
             (let ((ctype (careful-specifier-type specifier)))
               (if (and (array-type-p ctype)
                        (eq (array-type-specialized-element-type ctype)
                            *wild-type*))
                   ;; I don't think I'm allowed to modify what I get
                   ;; back from SPECIFIER-TYPE; it is, after all,
                   ;; cached.  Better copy it, then.
                   (let ((real-ctype (copy-structure ctype)))
                     (setf (array-type-element-type real-ctype)
                           *universal-type*
                           (array-type-specialized-element-type real-ctype)
                           *universal-type*)
                     real-ctype)
                   ctype)))))))))

(defun remove-non-constants-and-nils (fun)
  (lambda (list)
    (remove-if-not #'lvar-value
                   (remove-if-not #'constant-lvar-p (funcall fun list)))))

;;; FIXME: bad name (first because it uses 1-based indexing; second
;;; because it doesn't get the nth constant arguments)
(defun nth-constant-args (&rest indices)
  (lambda (list)
    (let (result)
      (do ((i 1 (1+ i))
           (list list (cdr list))
           (indices indices))
          ((null indices) (nreverse result))
        (when (= i (car indices))
          (when (constant-lvar-p (car list))
            (push (car list) result))
          (setf indices (cdr indices)))))))

;;; FIXME: a number of the sequence functions not only do not destroy
;;; their argument if it is empty, but also leave it alone if :start
;;; and :end bound a null sequence, or if :count is 0.  This test is a
;;; bit complicated to implement, verging on the impossible, but for
;;; extra points (fill #\1 "abc" :start 0 :end 0) should not cause a
;;; warning.
(defun nth-constant-nonempty-sequence-args (&rest indices)
  (lambda (list)
    (let (result)
      (do ((i 1 (1+ i))
           (list list (cdr list))
           (indices indices))
          ((null indices) (nreverse result))
        (when (= i (car indices))
          (when (constant-lvar-p (car list))
            (let ((value (lvar-value (car list))))
              (unless (or (typep value 'null)
                          (typep value '(vector * 0)))
                (push (car list) result))))
          (setf indices (cdr indices)))))))

(defun read-elt-type-deriver (skip-arg-p element-type-spec no-hang)
  (lambda (call)
    (let* ((element-type (specifier-type element-type-spec))
           (null-type (specifier-type 'null))
           (err-args (if skip-arg-p ; for PEEK-CHAR, skip 'peek-type' + 'stream'
                         (cddr (combination-args call))
                         (cdr (combination-args call)))) ; else just 'stream'
           (eof-error-p (first err-args))
           (eof-value (second err-args))
           (unexceptional-type ; the normally returned thing
            (if (and eof-error-p
                     (types-equal-or-intersect (lvar-type eof-error-p)
                                               null-type))
                ;; (READ-elt stream nil <x>) returns (OR (EQL <x>) elt-type)
                (type-union (if eof-value (lvar-type eof-value) null-type)
                            element-type)
                ;; If eof-error is unsupplied, or was but couldn't be nil
                element-type)))
      (if no-hang
          (type-union unexceptional-type null-type)
          unexceptional-type))))

(/show0 "knownfun.lisp end of file")
