;;;; cross-compiler-only versions of TYPEP, TYPE-OF, and related functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(define-condition cross-type-warning (warning)
  ((call :initarg :call :reader cross-type-warning-call)
   (message :reader cross-type-warning-message))
  (:report (lambda (c s)
             (format
              s
              "cross-compiler type ambiguity in ~S:~%~A"
              (cross-type-warning-call c)
              (cross-type-warning-message c)))))

;;; This warning is signaled when giving up on a type calculation
;;; during cross-compilation.
(define-condition cross-type-giving-up (cross-type-warning)
  ((message :initform "giving up conservatively")))

;; Return T if SYMBOL is a predicate acceptable for use in a SATISFIES type
;; specifier. We assume that anything in CL: is allowed.
(defvar *seen-xtypep-preds* nil)
(defun acceptable-cross-typep-pred (symbol usage)
  #+nil
  (cond ((not (fboundp symbol))
         (format t "~&XTYPEP: ~S is not fboundp~%" symbol))
        ((not (member symbol *seen-xtypep-preds*))
         (format t "~&XTYPEP: checking applicability of ~S for ~S~%"
                 symbol usage)
         (push symbol *seen-xtypep-preds*)))
  ;; For USAGE = TYPEP, always call the predicate.
  ;; For USAGE = CTYPEP, call it only if it is a foldable function.
  ;;
  ;; Exceptions:
  ;; 1. CLOSUREP and SIMPLE-FUN-P. These can get called from WEAKEN-TYPE with
  ;;    a symbol as the argument. It's ok to call either one in that case.
  ;;
  ;; 2. UNBOUND-MARKER-P is needed when compiling CLOSURE-EXTRA-VALUES
  ;;    and maybe some other things too.
  ;;
  ;; 3. VECTOR-WITH-FILL-POINTER-P - as long as the argument is not a vector
  ;;    we can safely say that the answer is NIL.
  ;;
  ;; 4. LEGAL-FUN-NAME-P and EXTENDED-FUNCTION-DESIGNATOR-P.
  ;;    These are harmless enough to call, so why not.
  ;;
  ;; 5. Anything else needed by a particular backend.
  (or (member symbol
              '(closurep simple-fun-p unbound-marker-p
                sb-impl::vector-with-fill-pointer-p
                legal-fun-name-p extended-function-designator-p))
      (member symbol sb-vm::*backend-cross-foldable-predicates*)
      (and (eq (sb-xc:symbol-package symbol) *cl-package*)
           (or (eq usage 'sb-xc:typep)
               (awhen (info :function :info symbol)
                 (sb-c::ir1-attributep (sb-c::fun-info-attributes it)
                                       sb-c:foldable))))))

;;; Return true of any SBCL-internal package.
;;; Defined here so that CROSS-TYPEP can use it.
;;; The target variant of this is in src/code/package.
(defmacro system-package-p (package)
  `(eql (mismatch "SB-" (cl:package-name ,package)) 3))

;;; This is like TYPEP, except that it asks whether OBJ (a host object acting
;;; as a proxy for some logically equivalent object in the target sytem)
;;; would be of type TYPE when instantiated on the target SBCL. Since this
;;; is hard to determine in some cases, and since in other cases we
;;; just haven't bothered to try, it needs to return two values, just
;;; like SUBTYPEP: the first value for its conservative opinion (never
;;; T unless it's certain) and the second value to tell whether it's
;;; certain.
;;; The logic is a mixture of the code for CTYPEP and %%TYPEP
;;; because it handles both.
;;; The order of clauses is fairly symmetrical with that of %%TYPEP.
(defvar *xtypep-uncertainty-action* #-sb-devel 'warn #+sb-devel nil) ; {BREAK WARN STYLE-WARN ERROR NIL}
(macrolet ((unimplemented ()
             '(bug "Incomplete implementation of ~S ~S ~S" caller obj type))
           (uncertain ()
             '(values nil nil)))
(defun cross-typep (caller obj type)
  (declare (type (member sb-xc:typep ctypep) caller))
  (when (or (cl:floatp obj) (cl:complexp obj))
    (error "Can't happen"))
  (multiple-value-bind (answer certain)
     (typep-impl-macro (obj)
       (named-type
        (ecase (named-type-name type)
          ((t) (values t t)) ; universal supertype
          ((instance) (values (%instancep obj) t))
          ((nil extended-sequence funcallable-instance)
           (values nil t)))) ; nothing could be these
       (numeric-type
        (values (number-typep obj type) t))
       (array-type
        ;; Array types correspond fairly closely between host and target, but
        ;; asking whether an array is definitely non-simple is a nonsensical
        ;; operation for the cross-compiler. The host can disagree with us,
        ;; reporting that all arrays are simple. So ensure we don't ask.
        ;; Otherwise the specialized array registry would need to track
        ;; our idea of whether the array is non-simple.
        (when (and (arrayp obj) (eq (array-type-complexp type) t))
          (bug "Should not call cross-typep with definitely-non-simple array type"))
        ;; This is essentially just the ARRAY-TYPE case of %%TYPEP
        ;; using SB-XC:ARRAY-ELEMENT-TYPE, not CL:ARRAY-ELEMENT-TYPE,
        ;; and disregarding simple-ness.
        (values (and (arrayp obj)
                     (or (eq (array-type-element-type type) *wild-type*)
                         (type= (specifier-type (sb-xc:array-element-type obj))
                                (array-type-specialized-element-type type)))
                     (or (eq (array-type-dimensions type) '*)
                         (and (= (length (array-type-dimensions type))
                                 (array-rank obj))
                              (every (lambda (required actual)
                                       (or (eq required '*) (eql required actual)))
                                     (array-type-dimensions type)
                                     (array-dimensions obj)))))
                t))
       (member-type
        (values (if (member-type-member-p obj type) t) t))
       ;; An empty (OR) produces a warning under CCL, and the warning as worded
       ;; is a tad wrong: "Clause ((OR) ...) shadowed by (CONS-TYPE ...)".
       ;; "Shadowed by" would imply that if you match the shadowing clause, then
       ;; you will match (and not execute) the shadowed clause.
       ;; But the empty (OR) should match nothing, so, what's up with that?
       ;; Maybe we can define host-side types named simd-pack-blah deftyped to NIL?
       ((or #+sb-simd-pack simd-pack-type
            #+sb-simd-pack-256 simd-pack-256-type)
        (values nil t))
       (character-set-type
        ;; provided that CHAR-CODE doesn't fail, the answer is certain
        (values (test-character-type type) t))
       (classoid ; = {built-in,structure,condition,standard,static}-classoid
        (let ((name (classoid-name type)))
          (if (built-in-classoid-p type)
              (ecase name
                (symbol (values (symbolp obj) t)) ; 1:1 correspondence with host
                (function
                 (if (functionp obj)
                     (uncertain)
                     ;; probably not a function. What about FMT-CONTROL instances?
                     (values nil t)))
                ((system-area-pointer stream fdefn weak-pointer file-stream
                  code-component lra pathname logical-pathname)
                 (values nil t)))
              (cond ((eq name 'pathname)
                     (values (pathnamep obj) t))
                    ((not (%instancep obj))
                     (values nil t)) ; false certainly
                    (t
                     (if (cl:find-class name nil) ; see if the host knows the type
                         (values (cl:typep obj name) t)
                         (unimplemented)))))))
       (fun-type
        (if (fun-designator-type-p type)
             (values (typep obj '(or symbol function)) t)
             ;; FUNCTION is not a specifier suitable for discrimination,
             ;; thus TYPEP is not allowed to determine whether an object
             ;; is in a particular subtype of function.
             ;; But be lenient when the object is not a function.
             (if (and (functionp obj) (eq caller 'sb-xc:typep))
                 (error "TYPEP called with function type")
                 (values (functionp obj) t))))
       (alien-type-type (if (symbolp obj) (values nil t) (unimplemented)))
       ;; Test UNKNOWN before falling into the HAIRY case
       (unknown-type
        (let ((spec (unknown-type-specifier type)))
          ;; Apparently we ask whether some objects are CLASSOIDS and other things
          ;; before we've made the respective type known to our type system,
          ;; but after the type is known by the host as one of our defstructs.
          ;; It happens during initialization of the type system, but later too.
          ;; Ideally we'd restrict this case to initialization only.
          ;; The post-initialization occurrences are for computing DD-BITMAP
          ;; of structures, which requires testing whether (TYPEP NIL X) is
          ;; true for various X that are not known yet.
          (cond ((and (symbolp spec)
                      (cl:find-class spec nil)
                      ;; See if the host knows our DEFSTRUCT yet
                      (cl:subtypep spec 'instance))
                 (values (cl:typep obj spec) t))
                ;; Sometimes we try to test a forward-referenced type
                ;; that was unknown at the point of creation but has
                ;; been resolved at the point of testing via block
                ;; compilation. Retry with the now resolved type if
                ;; this is the case.
                ((and (symbolp spec)
                      (not (unknown-type-p (specifier-type spec))))
                 (cross-typep caller obj (specifier-type spec)))
                ;; Special-case a few forward-referenced instance types.
                ;; Some are picked off by the quick exit case in (DEFUN SB-XC:TYPEP)
                ;; but a bunch of other cases arise within the type methods per se
                ;; which tend not to call via SB-XC:TYPEP but rather CTYPEP.
                ;; So you might think to pick off (%CROSS-TYPEP NIL AN-UNKNOWN-TYPE)
                ;; in this function, but then you have a new problem: bootstrap falls down
                ;; an infinite hole because simplifying unions and intersections often involves
                ;; asking whether NIL is a member of a type. The stub code we formerly had
                ;; for SB-XC:TYPEP tried to avoid parsing, and CROSS-TYPEP was almost entirely
                ;; implemented in terms of unparsed specifiers, not parsed specifiers,
                ;; which sort of accidentally worked for things like
                ;; (typep nil '(or (member null ...) blah)). That was a solution, but unprincipled
                ;; and not very testable, which was a problem considering how much it diverged
                ;; from the real implementations of CTYPEP and %%TYPEP.
                ;; So unfortunately we still munst handle a few cases without parsing,
                ;; but fewer than before, and then some after parsing.
                ((and (symbolp obj)
                      (member
                       spec
                       (if (member :sb-xc sb-xc:*features*)
                           ;; If :sb-xc is present, then we're cross-compiling.
                           ;; CROSS-TYPEP should not see any unknowns.
                           '()
                           ;; If :sb-xc is absent, then we're either running CL:COMPILE
                           ;; or CL:LOAD in make-host-1.
                           ;; It is permissible to make forward references to the following
                           ;; subtypes of structure-object in make-host-1.
                           '(hash-table lexenv sb-c::abstract-lexenv
                             condition restart
                             pathname sb-impl::host sb-impl::pattern
                             synonym-stream
                             ;; why on earth is LABEL needed here?
                             sb-assem:label))))
                 #+nil (format t "~&(XTYPEP '~S '~S) -> (NIL T)~%" obj spec)
                 (values nil t))
                (t
                 (uncertain)))))
       (hairy-type
        (let ((spec (hairy-type-specifier type)))
          (if (cl:typep spec '(cons (eql satisfies) (cons symbol null)))
              (let ((predicate (cadr spec)))
                ;; Keep in sync with KEYWORDP test in src/code/target-type
                (cond ((eq predicate 'keywordp)
                       (test-keywordp))
                      ;; These are needed in order to compile the predicates
                      ;; for the initial pprint dispatch table.
                      ((and (eq obj nil) (member predicate '(fboundp macro-function)))
                       (values nil t))
                      ((acceptable-cross-typep-pred predicate caller)
                       (values (funcall predicate obj) t))
                      (t
                       (uncertain))))
              (unimplemented)))))
    (when (and (not certain) *xtypep-uncertainty-action*
               ;; KLUDGE: Allow some slack while block compiling, as we are
               ;; hoping that forward referenced types get resolved.
               (not (and (boundp 'sb-c::*compilation*)
                         (eq (sb-c::block-compile sb-c::*compilation*) t))))
      ;; can't even backtrace if the printing of a something involving
      ;; uncertainty involves uncertainty.
      (let* ((action *xtypep-uncertainty-action*)
             (*xtypep-uncertainty-action* nil))
        (funcall action "CROSS-TYPEP uncertain: ~S ~S ~S" caller obj type)))
    (values answer certain))))

;;; This is an incomplete TYPEP which runs at cross-compile time to
;;; tell whether OBJECT is the host Lisp representation of a target
;;; SBCL type specified by TARGET-TYPE-SPEC. It need make no pretense
;;; to completeness, since it need only handle the cases which arise
;;; when building SBCL itself, e.g. testing that range limits FOO and
;;; BAR in (INTEGER FOO BAR) are INTEGERs.
(defun sb-xc:typep (host-object target-type-spec &optional (env nil env-p))
  (declare (ignore env))
  (aver (null env-p)) ; 'cause we're too lazy to think about it

  ;; Special case which avoids having to add more more hardcoded types
  ;; to the list of allowed unknown in cross-typep.
  ;; This is often used for deciding whether a structure slot can be raw.
  ;; If NIL is in the type, then it isn't raw.
  ;; The problem this solves is that even so much as parsing a type
  ;; can involve asking whether NIL is a member of the type,
  ;; and thus we need a way to break the endless cycle of not knowing
  ;; which is problem during self-build. The target system will make
  ;; any slot non-raw if its type is unknown, but during bootstrap
  ;; we're trying to be extremeley pedantic by disallowing unknowns.
  (if (and (null host-object)
           (typep target-type-spec '(cons (eql or)))
           (member 'null (cdr target-type-spec)))
      t
      (multiple-value-bind (opinion certain-p)
          (cross-typep 'sb-xc:typep host-object (specifier-type target-type-spec))
    ;; A program that calls TYPEP doesn't want uncertainty and
    ;; probably can't handle it.
        (if certain-p
            opinion
            (error "uncertain in SB-XC:TYPEP ~S ~S" host-object target-type-spec)))))

;;; This is an incomplete, portable implementation for use at
;;; cross-compile time only.
(defun ctypep (obj ctype)
  (declare (type ctype ctype))
  (multiple-value-bind (answer certain) (cross-typep 'ctypep obj ctype)
    ;; FIXME: I think we can (AVER CERTAIN) at this point.
    (if (or certain
            ;; Allow uncertainty only if the type contains a SATISFIES
            (block contains-satisfies
              (map-type (lambda (type)
                          (when (and (hairy-type-p type)
                                     (typep (hairy-type-specifier type)
                                            '(cons (eql satisfies))))
                            (return-from contains-satisfies t)))
                        ctype))
            ;; KLUDGE: Allow uncertainty while block compiling.
            (and (boundp 'sb-c::*compilation*)
                 (eq (sb-c::block-compile sb-c::*compilation*) t)))
        (values answer certain)
        #-sb-devel
        (warn 'cross-type-giving-up :call `(ctypep ,obj ,ctype)))))

;; TODO: would it be feasible to unify this definition with that in src/code/typep ?
(defun ctype-of (x)
  (typecase x
    (function
     (if (typep x 'generic-function)
         ;; Since at cross-compile time we build a CLOS-free bootstrap
         ;; version of SBCL, it's unclear how to explain to it what a
         ;; generic function is.
         (error "not implemented: cross CTYPE-OF generic function")
         ;; There's no ANSI way to find out what the function is
         ;; declared to be, so we just return the CTYPE for the
         ;; most-general function.
         *universal-fun-type*))
    (symbol
     (make-eql-type x))
    (number
     (ctype-of-number x))
    (array
     ;; It is critical not to inquire of the host for the array's element type.
     (let ((etype (specifier-type (array-element-type x))))
       (make-array-type (array-dimensions x)
                        ;; complexp relies on the host implementation,
                        ;; but in practice any array for which we need to
                        ;; call ctype-of will be a simple-array.
                        :complexp (not (typep x 'simple-array))
                        :element-type etype
                        :specialized-element-type etype)))
    (cons (specifier-type 'cons))
    (character
     (cond ((typep x 'standard-char)
            (specifier-type 'base-char))
           (t
            ;; Beyond this, there seems to be no portable correspondence.
            (error "can't map host Lisp CHARACTER ~S to target Lisp" x))))
    (instance
     (let ((type (type-of x)))
       (if (eq type 'sb-format::fmt-control-proxy)
           ;; These are functions, but they're weird. We don't want any IR1 transform
           ;; on FORMAT to kick in and try to convert to FUNCALL on the thing.
           (specifier-type '(or string function))
           ;; The structure may not be defined on the target yet.
           (or (find-classoid type nil)
               (find-classoid 'structure-object)))))
    (t
     ;; There might be more cases which we could handle with
     ;; sufficient effort; since all we *need* to handle are enough
     ;; cases for bootstrapping, we don't try to be complete here,. If
     ;; future maintainers make the bootstrap code more complicated,
     ;; they can also add new cases here to handle it. -- WHN 2000-11-11
     (error "can't handle ~S in cross CTYPE-OF" x))))

(defun sb-pcl::class-has-a-forward-referenced-superclass-p (x)
  (declare (ignore x))
  nil)

(defun non-null-symbol-p (x) (and x (symbolp x)))
;; these two functions don't need to be fully general
(defun pointerp (x)
  (aver (or (symbolp x) (fixnump x)))
  (symbolp x))
;; Use of non-ASCII during build occurs no sooner than make-target-2,
;; therefore _every_ character satisfies BASE-CHAR-P prior to that.
#+sb-unicode (defun base-char-p (x) (characterp x))
