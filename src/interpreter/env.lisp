;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTERPRETER")

;;; The policy in *VACUOUS-DECLS* specifies neither primary nor dependent
;;; qualities. By this we can determine that the env is to be skipped over
;;; when looking for the current policy.
(defglobal *vacuous-decls* (make-decl-scope nil (sb-c::make-policy 0 0)))

;;; One environment subtype exists for each kind of binding form.
;;; Structurally they are all identical, except that LAMBDA adds
;;; a block name, as an optimization wherein we attempt to elide
;;; creation of two lexical contours - one for the variables and
;;; one for the block - by creating only one contour when there
;;; is no chance that user code could detect that the block's scope
;;; was "widened" to encompass the variable bindings.
;;;
;;; LET and LET* use the same ENV type. The major difference with LAMBDA
;;; is that it has a fancy way to compute initial values,
;;; but internally the bindings look like they came from LET*.

(macrolet ((def-subtype (type &optional more-slots)
             (let ((constructor (symbolicate "MAKE-" type)))
               `(progn
                  (defstruct
                      (,type
                        (:include basic-env) (:copier nil)
                        (:constructor ,constructor
                         (parent payload symbols contour ,@more-slots)))
                    ,@more-slots)
                  (declaim (freeze-type ,type))
                  (declaim (inline ,constructor))))))
  (def-subtype function-env)
  (def-subtype var-env)
  (def-subtype macro-env)
  (def-subtype symbol-macro-env)
  (def-subtype block-env)
  (def-subtype tagbody-env)
  ;; LAMBDA-ENV is a theoretical subtype of both VAR-ENV and BLOCK-ENV
  ;; but implementationally all subtypes are pairwise disjoint.
  ;; Defining them thusly is a small efficiency win for the compiler,
  ;; because TYPEP has an optimization for sealed classes with no subclass,
  ;; but not for a sealed class with a fixed number of sealed subclasses.
  ;; Plus since structs don't support multiple inheritance anyway,
  ;; it would be arbitrary whether lambda-env were defined as a LET
  ;; with a block name, or a BLOCK and some bindings.
  (def-subtype lambda-env (block)))

;;; This implementation of lexical environments takes a fairly unconventional
;;; approach to dealing with sequential (LET*) binding. [Anything said here
;;; about LET* pertains to LAMBDA as well]. The conceptual model for LET* is
;;; that it is a sequence of nested LET forms, each creating one binding.
;;; This is often implemented by allocating a new environment structure per
;;; variable, exactly as implied by the equivalence with nested LET forms.
;;;
;;; An implementation that realizes LET* as such is quite inefficient though,
;;; especially if the LET* did not really need to be a LET* and was
;;; "just written that way". Assuming that the speed overhead is at least
;;; the space overhead - allocating N words takes N time - we can easily
;;; estimate the space overhead alone based on the structures above.
;;; One VAR-ENV takes up 6 words, plus a simple-vector for the variable values.
;;; Binding 10 variables in a LET consumes 18 words: 6 fixed, plus 12 words
;;; for a 10-vector. In contrast, 10 bindings if nested would take 100 words
;;; which is 10 * (6 + 4). This is nearly 6 times more space.
;;; [The smallest non-empty simple-vector is 4 words]
;;;
;;; So instead of using nested environments, this interpreter uses a single
;;; vector for storing all the variables of a LET*, and a quasi-fill-pointer
;;; (essentially a fill-pointer, but not stored as part an adjustable vector)
;;; indicating the effective end. Any operation on the vector should simply
;;; pretend that bindings beyond the effective end do not exist. Furthermore
;;; the parts of the environment pertaining to the body forms of the LET*,
;;; such as TYPE declarations for other than the bound variables, and SPECIAL
;;; declarations, do not exist until all variables have been sequentially bound.
;;; This is dealt with by having the declaration accessors return NIL until such
;;; time as the fill-pointer has reached its limit, at which point the extra
;;; declarations in the environment (SPECIAL,OPTIMIZE,etc.) become visible.

;;; An environment is termed "mutable" if it has not reached the end of its
;;; bindings - it is still subject to change by binding additional variables
;;; beyond the last which is currently bound. But once the last variable has
;;; been bound, the environment becomes "immutable" and it exposes all its
;;; variables plus the free declarations and such.
;;; LET (specifically, not LET*) environments are never mutable because
;;; no new environment is materialized until after all variable initialization
;;; values have been computed. Only the value vector is made in the interim.
;;;
;;; But mutability causes a problem for lexical closures - a closure needs a
;;; fixed view of the environment.  The solution is to place the burden upon
;;; closure allocation to capture the fixed view.
;;; The FUNCTION special operator is responsible for looking at the environment
;;; that it closes over, and deciding whether the environment is in flux.
;;; If it is, then FUNCTION needs to "freeze" the mutable environment by sharing
;;; all value cells and storing a fixed value for the fill-pointer.
;;; This is a recursive operation - you can close over an immutable environment
;;; whose parent is mutable, so that a deep copy occurs "upwardly"
;;; e.g. in (LET* ((f (...)) (g (let ((x 3)) (lambda () ...))) (h ..)))
;;; the LAMBDA's innermost contour contains an immutable environment for X,
;;; whose ancestor is a mutable environment in which F is seen but not G or H.
;;;
;;; In practice it makes a lot of sense to place the burden on closure creation.
;;; to avoid work during environment construction. As shown by the example,
;;; you must write fairly contrived code to cause a closure to see only part
;;; of a LET* environment. Contrived as that it though, there is indeed a
;;; use-case for it. The PCL defmethod expander exercises this uncommon
;;; pattern when making its fast method function:
;;;   (LET* ((FMF (NAMED-LAMBDA () ...)) (MF (%MAKE-METHOD-FUNCTION FMF NIL))))
;;; This NAMED-LAMBDA should see no symbols in the LET* environment.
;;; But in fact this usage is a degenerate example in which there is no need
;;; to freeze the environment as long as the parent did not need to be frozen.
;;; Since nothing was bound in the LET*, it is as if it had not happened at all,
;;; as far as the closure is concerned. Is is likely that scenarios in which
;;; freezing must do a non-trivial amount of work are quite rare.

;;; All that said, the representation of "mutable" is that instead of
;;; a vector of symbols, we have a cons of a fixnum and a vector, so ...
;;; Return T if the ENV is currently undergoing sequential variable binding.
(declaim (inline env-mutable-p))
(defun env-mutable-p (env) (consp (env-symbols env)))

;;; Code should only inquire of N-VARS when in the midst of a LET*
;;; or LAMBDA sequential binding operation, not LET, and not a completely
;;; bound LET* or LAMBDA.  (You can't take CAR of symbols in that case)
(declaim (inline env-n-vars))
(defun env-n-vars (env) (car (env-symbols env)))

;;; Each subtype of ENV points to subtype of a DECL-SCOPE.

(defstruct (local-scope (:include decl-scope)
                        (:constructor %make-local-scope
                                      (declarations %policy body specials))
                        (:predicate nil)
                        (:copier nil))
  (specials nil :read-only t)
  (body nil :read-only t))

(defstruct (local-fn-scope
             (:include local-scope)
             (:constructor %make-local-fn-scope
                           (declarations %policy funs body specials))
             (:predicate nil)
             (:copier nil))
  (funs nil :type simple-vector :read-only t))

(defstruct (symbol-macro-scope
             (:conc-name symbol-macro-)
             (:include decl-scope)
             (:constructor %make-symbol-macro-scope
                           (declarations %policy symbols expansions body))
             (:predicate nil)
             (:copier nil))
  (symbols nil :type simple-vector :read-only t)
  (expansions nil :type simple-vector :read-only t)
  (body nil :read-only t))

;;; "scope" and "frame" are basically synonymous here.
;;; The existence of both terms is a minor accident.

(defmethod print-object ((self frame) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(declaim (inline frame-size))
(defun frame-size (frame) (length (frame-values frame)))

;;; BASIC-ENV stores a policy for its body, but if evaluation has not reached
;;; the body forms of a LET*, then the old policy is in effect. This is due to
;;; the need for presenting a consistent view of the policy, but a frozen ENV
;;; does not get a different static environment - it sees the same object
;;; that the body forms see.
(defun env-policy (env)
  (cond ((not env) sb-c::*policy*)
        ((env-mutable-p env) (env-policy (env-parent env)))
        (t (let ((policy (%policy (env-contour env))))
             ;; If the policy contains no qualities, look to the parent env.
             ;; This happens with a BLOCK or TAGBODY, as well as the
             ;; environment in which LABELS are defined.
             (if (eql (sb-c::policy-presence-bits policy) 0)
                 (env-policy (env-parent env))
                 policy)))))

(defconstant +restp-bit+  #b100)
(defconstant +keyp-bit+   #b010)
(defconstant +allowp-bit+ #b001)

(declaim (inline make-keyword-bits keyword-bits-n-keys keyword-bits-allowp))
(defun make-keyword-bits (n-keys restp keyp allowp)
  (logior (ash n-keys 3)
          (if restp  +restp-bit+  0)
          (if keyp   +keyp-bit+   0)
          (if allowp +allowp-bit+ 0)))
(defun keyword-bits-n-keys (bits) (ash bits -3))
(defun keyword-bits-allowp (bits) (logtest bits +allowp-bit+))

(defun lambda-frame-max-args (frame) ; NIL if no explicit upper limit
  (if (logtest (lambda-frame-keyword-bits frame)
               (logior +restp-bit+ +keyp-bit+))
      nil
      (truly-the index (+ (lambda-frame-min-args frame)
                          (lambda-frame-n-optional frame)))))

;;; Return T if the innermost package-lock-related declaration pertaining
;;; to SYMBOL disables its package lock. Don't scan backwards through
;;; a lambda frame. (See remarks at CAPTURE-TOPLEVEL-ENV)
(defun lexically-unlocked-symbol-p (symbol env)
  (named-let recurse ((env env) (globalp t))
    (do-decl-spec (declaration
                   (env-declarations env)
                   (acond ((env-parent env)
                           (recurse it (and globalp (not (lambda-env-p env)))))
                          (globalp
                           (member symbol sb-c::*disabled-package-locks*))))
      ;; Ambiguous case: both in the same decl list. Oh well...
      (case (car declaration)
        (disable-package-locks
         (when (member symbol (cdr declaration)) (return t)))
        (enable-package-locks
         (when (member symbol (cdr declaration)) (return nil)))))))

;;; Do like above, but materialize the complete list of unlocked symbols,
;;; including those from sb-c::*disabled-package-locks* if applicable.
(defun env-disabled-package-locks (env &aux list)
  (named-let recurse ((env env) (globalp t))
    (acond ((env-parent env)
            (recurse it (and globalp (not (lambda-env-p env)))))
           (globalp
            (setq list (copy-list sb-c::*disabled-package-locks*))))
    (do-decl-spec (declaration (env-declarations env) list)
      (when (member (car declaration)
                    '(disable-package-locks enable-package-locks))
        (setq list (sb-c::process-package-lock-decl declaration list))))))

;;; Return the declarations which are currently effective in ENV.
;;; If ENV is a sequential binding environment which has not reached
;;; its body forms, return NIL.  This is not recursive,
;;; because declarations are a local aspect of the ENV.
(defun env-declarations (env)
  (if (env-mutable-p env) nil (declarations (env-contour env))))

;;; Everything in the interpreter is compiled without safety,
;;; because when thing work as they should, no mistaken assumptions are made
;;; about the internals. For user code, safety is effectively imparted by the
;;; strict checking of argument list arity in interpreted APPLY,
;;; and all system functions are safe when invoked through their public API.
;;; Whether maximally strict checking of types is performed in user code
;;; has nothing to do with how the interpreter is compiled.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Of course, errors are possible in the interpreter itself,
  ;; so in that case it helps to define this as '() for debugging.
  #+nil(defparameter +handler-optimize+ '(optimize))
  (defparameter +handler-optimize+ '(optimize (speed 2) (debug 2) (safety 0))))

;;; We represent a pointer to a symbol in an environment by a FRAME-PTR
;;; which is a packed integer containing the "up" and "across" indices.
(declaim (inline make-frame-ptr frame-ptr-depth frame-ptr-cell-index))
;;; This provides constant-time access to lexical variables within a frame.
(defun make-frame-ptr (across &optional (up 0))
  (declare (type (unsigned-byte #.+frame-depth-bits+) up)
           (type (unsigned-byte #.+frame-size-bits+) across)
           #.+handler-optimize+)
  (logior (ash across +frame-depth-bits+) up))

(defun frame-ptr-depth (frame-ptr) ; "up"
  (declare (fixnum frame-ptr))
  (ldb (byte +frame-depth-bits+ 0) frame-ptr))

(defun frame-ptr-cell-index (frame-ptr) ; "across"
  (declare (fixnum frame-ptr))
  (ash frame-ptr (- +frame-depth-bits+)))

;;; Older frames take O(depth) to locate.
(declaim (inline env-ancestor))
(defun env-ancestor (env frame-ptr)
  (declare (fixnum frame-ptr) #.+handler-optimize+)
  (do ((i (frame-ptr-depth frame-ptr) (1- i)))
      ((zerop i) env)
    (declare (fixnum i))
    (setq env (env-parent env))))

;;; Hide an implementation detail of a partially bound environment,
;;; that it is a vector and a count.
;;; END indicates the length of the effective portion of the value vector.
;;; LENGTH of a simple-vector is in the same location as CAR of a cons,
;;; so regardless of whether ENV-SYMBOLS currently hold a cons or a vector
;;; we can always use CAR to read the length.
(defmacro with-environment-vars ((symbols end) env &body body)
  `(awhen (env-symbols ,env)
     (let ((,symbols (truly-the simple-vector (if (listp it) (cdr it) it)))
           (,end #+(or ubsan ppc64) (if (listp it) (car it) (sb-c::vector-length it))
                 #-(or ubsan ppc64) (locally (declare (optimize (safety 0))) (car it))))
       (declare (index-or-minus-1 ,end))
       ,@body)))
#-ppc64
(eval-when (:compile-toplevel)
  ;; Assert that the claims made in the above comment remain true.
  (assert (= (- (* sb-vm:n-word-bytes sb-vm:cons-car-slot)
                sb-vm:list-pointer-lowtag)
             (- (* sb-vm:n-word-bytes sb-vm:vector-length-slot)
                sb-vm:other-pointer-lowtag))))

(defmacro %cell-ref (env frame-ptr)
  `(svref (env-payload (env-ancestor ,env ,frame-ptr))
          (frame-ptr-cell-index ,frame-ptr)))

;;; Return the symbol that FRAME-PTR represents in ENV (or an ancestor of it).
;;; The symbol vector is a vector of (CONS SYMBOL (OR FUNCTION CTYPE)).
(defun frame-symbol (env frame-ptr)
  (let ((symbols (env-symbols (env-ancestor env frame-ptr))))
    (car (svref (if (listp symbols) (cdr symbols) symbols)
                (frame-ptr-cell-index frame-ptr)))))

(defun %enforce-types (typechecks env)
  (do ((i 0 (+ i 2))
       (n (length typechecks)))
      ((eq i n))
    (declare (index i n))
    (let ((ref (svref typechecks i)))
      (when (fixnump ref)
        (do ((i i (+ i 2)))
            ((eq i n) (return-from %enforce-types))
          (let* ((frame-ptr (svref typechecks i))
                 (val (%cell-ref env frame-ptr))
                 (type (svref typechecks (logior i 1))))
            (unless (itypep val type)
              (typecheck-fail/ref (frame-symbol env frame-ptr) val type)))))
      (when (boundp ref)
        (let ((val (symbol-value ref))
              (type (svref typechecks (logior i 1))))
          (unless (itypep val type)
            (typecheck-fail/ref ref val type)))))))

(defun must-freeze-p (env)
  (and env
       (or (env-mutable-p env)
           (must-freeze-p (env-parent env)))))

;; This is an important operation for creation of interpreted lexical closures.
;; It should execute as fast as possible.
(defun freeze-env (env)
  (declare (instance env)) ; just rule out NIL
  (labels ((recurse (env)
             (let* ((parent-copy (awhen (env-parent (truly-the basic-env env))
                                   (recurse it)))
                    ;; The reason we're grabbing ENV-SYMBOLS here is
                    ;; to ensure that the slot is accessed exactly once.
                    ;; (CONSP symbols) is the same as (mutable-p env).
                    ;; See comment in 'macros' about figuring out
                    ;; whether this is safe. Maybe I'm just paranoid?
                    ;; Otoh, maybe I'm not, since the concurrency tests
                    ;; are randomly hitting lose("Feh.") in gencgc.
                    (symbols (env-symbols env))
                    (mutable (consp symbols)))
               ;; PARENT-COPY might not actually be a copy
               (if (or mutable (neq parent-copy (env-parent env)))
                   (let ((new (copy-structure env)))
                     (setf (env-parent new) parent-copy)
                     (when mutable
                       (setf (env-symbols new)
                             (cons (car (truly-the list symbols))
                                   (cdr symbols))))
                     new)
                   env))))
    (recurse env)))

;;; SBCL currently takes declarations affecting policy as if they were "hoisted"
;;; outside the form containing them, so that they apply to initialization forms
;;; as well as body forms. This is in direct contradiction to the X3J13 decision.
;;; This flag says to be conveniently bug-for-bug compatible with the compiler.
;;; See https://bugs.launchpad.net/sbcl/+bug/309125
;;; FIXME: is this used anywhere?
(declaim (boolean *hoist-optimize-declarations*))
(defvar *hoist-optimize-declarations* t)

;;; Return a new policy based on the existing policy, augmented by DECLS.
;;; FIXME: this looks like it duplicates code that exists elsewhere.
;;; Maybe SB-C::PROCESS-OPTIMIZE-DECL ?
(defun new-policy (env decls)
  (let ((policy (env-policy env)) (copy-on-write t))
    (do-decl-spec (decl-spec decls policy)
      (when (eq (car decl-spec) 'optimize)
        (dolist (qual+val (cdr decl-spec))
          (multiple-value-bind (qual val)
              (if (atom qual+val)
                  (values qual+val 3)
                  (values (car qual+val) (cadr qual+val)))
            (let ((index (sb-c::policy-quality-name-p qual)))
              (when (and index
                         (typep val 'sb-c::policy-quality)
                         ;; Read the unadjusted value from the origin policy.
                         ;; If we're not changing that, don't do anything.
                         (/= val (sb-c::%%policy-quality policy index)))
                (when copy-on-write
                  (setq policy (copy-structure policy)
                        copy-on-write nil))
                (sb-c::alter-policy policy index val)))))))))

;;;; Function stuff that's not in 'function.lisp'
;;;; because cross-compilation does not need it.

;; If a function's name slot does not hold a proper name,
;; then its name is itself.
(defun name-for-fun (fun)
  (let ((name (fun-name fun)))
    (if (and (not (eql name 0)) (legal-fun-name-p name) (fboundp name))
        name
        fun)))

(defmethod print-object ((obj interpreted-function) stream)
  ;; Do not try to directly print 'NAME-FOR-FUN', which returns OBJ
  ;; itself if it has no proper name.
  (if (or (eql (interpreted-function-%proto-fn obj) 0)
          (eql (fun-name obj) 0))
      ;; To avoid an extra space between type and identity, the body must
      ;; be empty, so we need two cases, because emptiness is compile-time
      ;; determined, not based on whether the body actually printed anything.
      (print-unreadable-object (obj stream :type t :identity t))
      ;; show name whenever NAME it is not 0, even if not OBJ's proper name.
      (print-unreadable-object (obj stream :type t)
        (prin1 (fun-name obj) stream))))

;;; Return approximately a type specifier for LAMBDA-LIST.
;;; e.g. after doing (DEFUN FOO (A B) ...), you want (FUNCTION (T T) *)
;;; This is mainly to get accurate information from DESCRIBE
;;; when properly hooked in.
;;; FIXME: this returns T for all &OPTIONAL and &KEY args.
(defun approximate-proto-fn-type (lambda-list bound-symbols)
  (declare (notinline member cons))
  (labels ((recurse (list var-index &aux (elt (car list)))
             (unless (or (eq elt '&aux) (null list))
               (let ((ll-keyword-p (member elt lambda-list-keywords))
                     (rest (cdr list)))
                 (cons (cond (ll-keyword-p elt)
                             ((not var-index) 't)
                             (t
                              (acond ((cdr (svref bound-symbols var-index))
                                      (type-specifier it))
                                     (t t))))
                       (if (eq elt '&key)
                           (keys rest)
                           (recurse rest
                                    (and var-index
                                         (not ll-keyword-p)
                                         (1+ var-index))))))))
           (keys (list &aux (elt (car list)))
             (unless (or (eq elt '&aux) (null list))
               (cons (cond ((member elt lambda-list-keywords) elt)
                           (t `(,(parse-key-arg-spec elt) t)))
                     (keys (cdr list))))))
    `(function ,(recurse lambda-list 0) *)))

(declaim (type boolean *hook-all-functions*))
(defvar *hook-all-functions-p* nil)

(declaim (ftype function interpreter-trampoline interpreter-hooked-trampoline))

(defun make-function (proto-fn env)
  (declare (type interpreted-fun-prototype proto-fn))
  (let ((function (%make-interpreted-function proto-fn env nil nil)))
    ;; Hooking all functions, makes them somewhat slower,
    ;; but allows for really nifty introspection,
    ;; such as discovering what calls are made by read-time evals.
    (setf (%funcallable-instance-fun function)
          (if *hook-all-functions-p*
              (lambda (&rest args)
                (apply #'interpreter-hooked-trampoline function args))
              (lambda (&rest args)
                (apply #'interpreter-trampoline function args))))
    function))

;; When globaldb info changes, this counter can be bumped to force interpreted
;; functions to discard memoized data on their next application. For example
;; if a function gets called before a global SPECIAL proclamation has been made
;; regarding one of its lambda variables, this can be corrected by touching the
;; globaldb cookie. On-stack functions will not see the change though.
;; What remains is to hook the setting of some of the globaldb info-types.
;;
(declaim (fixnum *globaldb-cookie*))
(defglobal *globaldb-cookie* most-positive-fixnum)

;; instrumentation of macro cache flushes, mostly for testing
(declaim (fixnum *invalidation-count*))
(defglobal *invalidation-count* 0)

;; Return two values: FRAME and COOKIE, recomputing if cookie doesn't match
;; globaldb, otherwise return the previously computed information.
(declaim (inline proto-fn-frame))
(defun proto-fn-frame (proto-fn env)
  (if (eq (proto-fn-cookie proto-fn) *globaldb-cookie*)
      (values (proto-fn-%frame proto-fn) (proto-fn-cookie proto-fn))
      (digest-lambda env proto-fn)))

(defun %fun-ftype (fun)
  (let ((proto-fn (fun-proto-fn fun)))
    (or (proto-fn-type proto-fn)
        (setf (proto-fn-type proto-fn)
              (approximate-proto-fn-type
               (proto-fn-lambda-list proto-fn)
               (frame-symbols
                (proto-fn-frame (fun-proto-fn fun)
                                (interpreted-function-env fun))))))))

;; This is just a rename of DESTRUCTURING-BIND
;; Should it do anything magic?
(defmacro with-subforms (lambda-list arg-form &body body)
  `(destructuring-bind ,lambda-list ,arg-form ,@body))

;  (let ((arg-list-name (gensym "ARG-LIST-")))
;    (multiple-value-bind (body local-decls)
;        (parse-defmacro lambda-list arg-list-name body nil
;                        'program-destructuring-bind
;                        :anonymousp t
;                        :doc-string-allowed nil
;                        :wrap-block nil
;                        :error-fun 'sb-eval::arg-count-program-error)
;      `(let ((,arg-list-name ,arg-list))
;         ,@local-decls
;         ,body))))

(declaim (ftype (sfunction (integer t) function) local-fdefinition)
         (maybe-inline local-fdefinition))

;; If DECL is a declaration that affects variables, return the kind of
;; variable-affecting declaration it is.
(defun applies-to-variables-p (decl)
  (let ((id (car decl)))
    (or (find id '(ignorable ignore type special dynamic-extent))
        (if (or (listp id) ; it must be a type-specifier (including NIL)
                (info :type :kind id))
            'type))))

(defglobal *unary-functions* nil)
(defglobal *binary-functions* nil)

(defun collect-progv-symbols (symbols n mask)
  (loop for i below n
        when (logbitp i mask)
        collect (car (svref symbols i))))

;;; DECLS-LIST is a list of lists of declarations. The original structure
;;; is preserved, so this necessitates a triply-nested loop.
;;; e.g. (LET () (DECLARE (SPECIAL X) (SPECIAL Y)) (DECLARE (SPECIAL Z W)))
;;; has decls-list (((SPECIAL X) (SPECIAL Y)) ((SPECIAL Z W)))
;;;
(defun declared-specials (decls-list)
  (let ((count 0))
    (declare (fixnum count))
    (collect ((specials))
      (do-decl-spec (decl-spec decls-list)
        (when (eql (car decl-spec) 'special)
          (dolist (var (cdr decl-spec))
            (unless (memq var (specials))
              (incf count)
              (specials var)))))
      (values (specials) count))))

;;; See if all SYMBOLS can be declare special.
;;; This applies to both free and bound variables.
(defun assert-declarable-as-special (env symbols)
  (declare (ignore env))
  (dolist (name symbols)
    (unless (symbolp name)
      (%program-error "~A is not a symbol" name))
    ;; Same logic as SB-C::PROCESS-SPECIAL-DECL
    (let ((kind (info :variable :kind name)))
      (unless (member kind '(:special :unknown))
        (error "Can't declare ~(~A~) variable locally special: ~S" kind name)))
    (program-assert-symbol-home-package-unlocked
     :eval name "declaring ~A special")))

;; Given that all SPECIAL declarations in DECLS pertain to free specials,
;; return a vector to supply as the SYMBOLS for an environment constructor.
;; This should not be used for LET/LET*/LAMBDA binding handlers,
;; which have their own way of creating the free specials along with
;; bound variables.
(defun free-specials (env decls)
  (multiple-value-bind (symbols n) (declared-specials decls)
    (when symbols
      (with-package-lock-context (env)
        (assert-declarable-as-special env symbols))
      (let ((a (make-array n)))
        ;; If any special declaration exposes a bound special
        ;; from an enclosing scope, the original binding cell
        ;; is made visible in this binding scope.
        ;; This causes any type declaration to be carried forward.
        (dotimes (i n a)
          (setf (aref a i) (find-special-binding env (pop symbols))))))))

(defmacro specially-bind-p (symbol lexically-special-p)
  ;; Don't signal errors here: allow the interpreter to attempt to bind
  ;; as special if so indicated, and let the native PROGV complain.
  `(or (memq (info :variable :kind ,symbol) '(:constant :global :special))
       ,lexically-special-p))

(defun mark-bound-specials (env declared-specials symbols n-bound)
  (declare (simple-vector symbols))
  (with-package-lock-context (env)
    (assert-declarable-as-special env declared-specials))
  (let ((special-b 0))
    ;; Every time I look at this and think that it makes more sense to run the
    ;; outer loop over declared and the inner loop over bound, so that there
    ;; are fewer iterations, I have to remember why that is wrong -
    ;; it would miss global proclamations.
    (dotimes (i n-bound special-b)
      (let ((sym (the symbol (car (svref symbols i)))))
        ;; Given: (let* ((x (foo)) (x (fn x))) (declare (special x)) ...
        ;; only the second X is special. This mimics the compiler exactly.
        (when (specially-bind-p
               sym (and (memq sym declared-specials)
                        (not (find sym symbols :start (1+ i) :end n-bound
                                   :key #'car))))
          (setf (logbitp i special-b) t))))))

(defun make-proto-fn (lambda-expression &optional (silent t))
  (multiple-value-bind (name lambda-list body)
      (if (memq (car lambda-expression) '(named-lambda))
          (with-subforms (name lambda-list . body) (cdr lambda-expression)
            (values name lambda-list body))
          (with-subforms (lambda-list . body) (cdr lambda-expression)
            (values 0 lambda-list body)))
    ;; Choke now if the list can't be parsed.
    ;; If lexical environment is NIL, :silent will be passed as NIL,
    ;; and we can warn about "suspicious variables" and such.
    (parse-lambda-list lambda-list :silent silent)
    (multiple-value-bind (forms decls docstring) (parse-body body t t)
      (%make-proto-fn name lambda-list decls forms docstring
                      (do-decl-spec (spec decls lambda-list)
                        (when (eq (car spec) 'sb-c::lambda-list)
                          (return (cadr spec))))))))

;; Find function named by FNAME in ENV or an ancestor, returning three values:
;;  * KIND = {:MACRO,:FUNCTION}
;;  * DEF  = the definition
;;  * FRAME-PTR
(defun find-lexical-fun (env fname)
  (flet ((fname (x) (second (fun-name x))))
    (do ((test (if (atom fname) #'eq #'equal))
         (env env (env-parent env))
         (level 0 (1+ level)))
        ((null env) (values nil nil nil))
      (declare (type (unsigned-byte #.+frame-depth-bits+) level))
     (when (and (env-payload env) ; quick check before using type predicates
                (or (function-env-p env) (macro-env-p env)))
       (multiple-value-bind (definition index)
           (%find-position fname (the simple-vector (env-payload env))
                           nil 0 nil #'fname test)
         (when index
           (return (values (if (macro-env-p env) :macro :function)
                           definition (make-frame-ptr index level)))))))))

;;; Retrieve the function/macro binding of the symbol NAME in
;;; environment ENV, with the global definition as a fallback.
;;; The second return value is T if NAME names a macro.
;;;
(defun get-function (fname env)
  (multiple-value-bind (kind definition) (find-lexical-fun env fname)
    (acond (definition (values definition (eq kind :macro)))
           ((and (symbolp fname) (macro-function fname)) (values it t))
           ;; FDEFINITION strips encapsulations, %COERCE-NAME-TO-FUN doesn't.
           ;; There's a test in 'eval.impure.lisp' asserting that encapsulations
           ;; aren't stripped, but frankly all bets are off when tracing.
           (t (values (%coerce-name-to-fun fname) nil)))))

;; Find SYM in ENV or an ancestor and return four values:
;;  * CELL  = a cons of the symbol and its CTYPE
;;  * KIND  = {:NORMAL,:MACRO,:SPECIAL}
;;  * FRAME-PTR
;;  * VALUE = the value, if KIND is :NORMAL, else the macroexpansion
;;
;; Bindings are in parallel symbol/value vectors left-to-right as appearing
;; in source. Scanning is right-to-left so that later LET* bindings shadow
;; earlier ones of the same name. The end pointer in a LET* environment
;; constrains the usable length of the symbol vector.
;; Unbound ("free") special variables have no entry in the value vector.
;;
(defun find-lexical-var (env sym)
  (do ((env env (env-parent env))
       (level 0 (1+ level)))
      ((null env) (values nil nil nil))
    (declare (type (unsigned-byte #.+frame-depth-bits+) level))
    (with-environment-vars (symbols index) env ; skipped if no symbols
      ;; Emulate find/position with :FROM-END T here, but faster.
      (loop
       (when (minusp (decf index)) (return))
       (let ((cell (svref symbols index)))
         (when (eq (binding-symbol cell)  sym)
           (multiple-value-bind (kind value)
               (cond ((or (var-env-p env) (lambda-env-p env))
                      (let ((values (the simple-vector (env-payload env))))
                        (if (or (>= index (length values))
                                (logbitp index
                                         (frame-special-b (env-contour env))))
                            :special
                            (values :normal (svref values index)))))
                     ((symbol-macro-env-p env)
                      (let ((values (the simple-vector (env-payload env))))
                        (if (>= index (length values))
                            :special
                            (values :macro (svref values index)))))
                     (t ; function-env, macro-env, basic-env (locally).
                      :special)) ; no symbol is bound
             (return-from find-lexical-var
               (values cell kind (make-frame-ptr index level) value)))))))))

;;; Search ENV for BINDING and return a frame-pointer if the variable
;;; is a lexical var, or :SPECIAL or :MACRO if it is one of those.
;;; Lexically visible special bindings return :SPECIAL.
;;; This is similar to FIND-LEXICAL-VAR in its operation,
;;; but simpler, as bindings are unique objects.
(defun find-binding (env binding)
  (do ((env env (env-parent env))
       (level 0 (1+ level)))
      ((null env) nil)
    (declare (type (unsigned-byte #.+frame-depth-bits+) level))
    (with-environment-vars (bindings index) env ; skipped if no symbols
      (declare (ignore index))
      (let ((payload (env-payload env))
            (index (position binding bindings)))
        (when index
          (return
            (cond ((or (var-env-p env) (lambda-env-p env))
                   (if (or (>= index (length (the simple-vector payload)))
                           (logbitp index (frame-special-b (env-contour env))))
                       :special
                       (make-frame-ptr index level)))
                  ((symbol-macro-env-p env)
                   (if (>= index (length (the simple-vector payload)))
                       :special
                       :macro))
                  (t ; function-env, macro-env, basic-env (locally).
                   :special)))))))) ; no symbol is bound

;;; Similar to the above, but only return a lexically visible special binding.
;;; This is required to locate the intended binding in cases such as this:
#|
 (let ((a 3)) ; special A[1]
   (declare (special a))
   (symbol-macrolet ((a x)) ; macro A[2]
     (let ((a (foo))) ; lexical A[3]
       (macrolet ((foo () ...))
         (declare (real a) (special a)) ; declares the type of A[1]
         A)))) ; references A[1]
|#
(defun find-special-binding (env sym)
  (do ((env env (env-parent env)))
      ((null env) sym) ; Return just SYM if no binding found.
    ;; Only a LET-like frames can create special bindings.
    (when (or (var-env-p env) (lambda-env-p env))
      (with-environment-vars (symbols index) env ; skipped if no symbols
        (let ((index (position sym symbols
                               :end (min index (length (env-payload env)))
                               :key #'car :test #'eq :from-end t)))
          (when (and index (logbitp index (frame-special-b (env-contour env))))
            (return (svref symbols index))))))))

;;; BINDING is either a cell or a symbol (if a free special).
(defun find-type-restriction (env binding)
  (do ((env env (env-parent env)))
      ((null env)
       (if (listp binding)
           (or (cdr binding) *universal-type*)
           *universal-type*))
    (unless (env-mutable-p env)
      (awhen (assq binding (type-restrictions (env-contour env)))
        (return (cdr it))))))

;;; Update DECL-SCOPE with type restrictions based on its declarations.
;;; Restrictions are separated into those which apply to bindings made by
;;; this scope - each being pertinent as soon as the variable to which it
;;; applies is bound - and those which apply to the body forms.
;;; Bound lexical and special variables in the new scope have the CTYPE
;;; as stored in the CDR of the binding cell altered to reflect the
;;; restriction, other restrictions go into the TYPE-RESTRICTIONS slot.
;;; A special falls into the latter category if the binding is lexically
;;; visible but did not occur in exactly this scope. This avoids stomping
;;; on the type that was stored in the scope which made the binding.
(defun process-typedecls (decl-scope env n-var-bindings symbols
                          &aux new-restrictions)
  ;; First compute the effective set of type restrictions.
  (with-package-lock-context (env)
    (do-decl-spec (decl (declarations decl-scope))
      (cond
       ((eq (applies-to-variables-p decl) 'type)
        (binding* (((type-spec names)
                    (if (eq (car decl) 'type)
                        (values (cadr decl) (cddr decl))
                        (values (car decl) (cdr decl))))
                   (ctype (specifier-type type-spec)))
          (dolist (symbol names)
            (when (and (symbolp symbol) (boundp symbol))
              (program-assert-symbol-home-package-unlocked
               :eval symbol "declaring the type of ~A"))
            (unless (eq ctype *universal-type*)
              (multiple-value-bind (binding index)
                  (%find-position symbol symbols t 0 nil #'binding-symbol #'eq)
                (if (and index (< index n-var-bindings))
                    ;; Any kind of binding created in directly this frame.
                    ;; Type restrictions aren't pervasive downward,
                    ;; so this case doesn't intersect the new type with a
                    ;; prevailing restriction. Global proclamations are
                    ;; pervasive, but violations are caught by the runtime.
                    (rplacd binding
                            (acond ((cdr binding) (type-intersection it ctype))
                                   (t ctype)))
                    ;; Three possibilities now:
                    ;; 1. INDEX was past the number of bindings in this scope.
                    ;;    This is either a locally declared free special,
                    ;;    or a special declaration that exposes a special
                    ;;    var bound in some containing scope, possibly with
                    ;;    intervening non-special bindings of the same name.
                    ;; 2. A binding from an earlier scope not covered by case 1.
                    ;; 3. Something global: an assumed or proclaimed special,
                    ;;    or a global symbol-macro.
                    (let* ((thing (or binding ; case 1
                                      (find-lexical-var env symbol) ; case 2
                                      symbol)) ; case 3
                           (restriction (assq thing new-restrictions)))
                      (if restriction
                          (rplacd restriction
                                  (type-intersection (cdr restriction) ctype))
                          (let* ((old-type (find-type-restriction env thing))
                                 (new-type (type-intersection old-type ctype)))
                            (unless (type= old-type new-type)
                              (push (cons thing new-type)
                                    new-restrictions)))))))))))
       ((eq (car decl) 'ftype)
        (dolist (name (cddr decl))
          (when (and (legal-fun-name-p name) (fboundp name))
            (program-assert-symbol-home-package-unlocked
             :eval name "declaring the ftype of ~A")))))))

  (setf (type-restrictions decl-scope) new-restrictions)
  ;; Done computing effective restrictions.
  ;; If the enclosing policy - not the new policy - demands typechecks,
  ;; then insert assertions for all variables bound by this scope.
  (when (and (policy env (>= safety 1))
             (find-if #'cdr symbols :end n-var-bindings))
    (let ((checks (make-array n-var-bindings :initial-element 0)))
      (dotimes (i n-var-bindings (setf (binding-typechecks decl-scope) checks))
        (awhen (cdr (svref symbols i))
          (setf (svref checks i) (type-checker it))))))
  ;; If a nested scope re-declares a variable to be of a more constrained
  ;; type "for efficiency" it does not really help the interpreter any,
  ;; so don't do those checks unless SAFETY exceeds 2.
  ;; This is a somewhat arbitrary but reasonable stance to take.
  (when (and (policy env (>= safety 2)) new-restrictions)
    (collect ((lexical-var-checks) (special-var-checks))
      (dolist (check new-restrictions)
        (let ((binding (car check))
              (checkfun (type-checker (cdr check))))
          (if (consp binding) ; some kind of binding, not sure what
              (let ((frame-ptr (find-binding env binding)))
                (case frame-ptr
                  (:macro) ; ignore it
                  (:special (special-var-checks (car binding) checkfun))
                  (t (lexical-var-checks frame-ptr checkfun))))
              (special-var-checks binding checkfun))))
      ;; Restrictions could pertain to symbol-macros only,
      ;; which are not checked on entry to the scope.
      (when (or (lexical-var-checks) (special-var-checks))
        (setf (extra-typechecks decl-scope)
              (coerce (nconc (special-var-checks) (lexical-var-checks))
                      'vector)))))
  decl-scope)

;;; Return the thing that should be asserted about VARIABLE's type.
;;; If there is nothing to check - no declared type, or the current policy
;;; does not require type-checking, then return NIL.
;;; These are the distinct things that might want different controls:
;;;  - initial bindings
;;;  - entrance to new scope with stronger constraint
;;;  - writes (SETQ)
;;;  - reads
;;;  - THE
;;; Only the writes and reads are controlled here,
;;; the others are kind of spread out. It might be nice to consolidate
;;; the policy-related decisions somewhere.
;;;
(defun var-type-assertion (env symbol binding op)
  ;; ** these criteria are subject to change. Not sure they're the best.
  (when (ecase op
          (:write (policy env (>= safety 1)))
          (:read  (policy env (and (= safety 3) (= speed 0)))))
    (let ((type (find-type-restriction env (or binding symbol))))
      (if (eq type *universal-type*) nil (type-checker type)))))

;;; Convert compiler env to interpreter env if possible,
;;; otherwise return :COMPILE.
;;; A lexical environment which contains only macros and decls is ok.
;;; Locally [not]inline declarations about global functions are ok.
(defun env-from-lexenv (lexenv &aux (compiler-funs (sb-c::lexenv-funs lexenv))
                                    (compiler-vars (sb-c::lexenv-vars lexenv)))
  (flet ((harmless-defined-fun-p (thing)
           (and (typep thing 'sb-c::defined-fun)
                (eq (sb-c::defined-fun-kind thing) :global-function)))
         (macro-p (thing)
           (typep thing '(cons (eql sb-sys:macro)))))
    (if (or (sb-c::lexenv-blocks lexenv)
            (sb-c::lexenv-tags lexenv)
            ;; FIXME: why test -LAMBDA here? Isn't that a book-keeping thing,
            ;; not a semantics thing? (See also similar code in 'full-eval')
            (sb-c::lexenv-lambda lexenv)
            (sb-c::lexenv-cleanup lexenv)
            (sb-c::lexenv-type-restrictions lexenv)
            (find-if-not (lambda (x)
                           (or (macro-p x) (harmless-defined-fun-p x)))
                         compiler-funs :key #'cdr)
            (find-if-not #'macro-p compiler-vars :key #'cdr))
        :compile
        (let*
            ((disabled-package-locks
              `((declare (disabled-package-locks
                          ,@(sb-c::lexenv-disabled-package-locks lexenv)))))
             (macro-env
              ;; Macros in an interpreter environment must look like interpreted
              ;; functions due to use of FUN-NAME to extract their name as a key
              ;; rather than also needing an alist mapping names to objects.
              ;; For each compiled expander, wrap it in a trivial interpreted fun.
              (make-macro-env
               nil
               (map 'vector
                    (lambda (cell)
                      (let ((expander (cddr cell)))
                        (if (typep expander 'interpreted-function)
                            expander
                            (make-function
                             (%make-proto-fn `(macrolet ,(car cell)) '(form env)
                                             nil ; decls
                                             `((funcall ,expander form env)) nil)
                             nil)))) ; environment for the interpreted fun
                    (remove-if-not #'macro-p compiler-funs :key #'cdr))
               nil ; no free specials vars
               ;; FIXME: type-restrictions, handled-conditions.
               (make-decl-scope (if compiler-vars nil disabled-package-locks)
                                (sb-c::lexenv-policy lexenv)))))
        (if (not compiler-vars)
            macro-env
            (make-symbol-macro-env
             macro-env
             (map 'vector #'cddr compiler-vars)
             (map 'vector (lambda (x) (list (car x))) compiler-vars)
             (make-decl-scope disabled-package-locks
                              (sb-c::lexenv-policy lexenv))))))))

;;; Enclose PROTO-FN in the environment ENV.
;;; SEXPR is provided only because this is a HANDLER.
(defun enclose (proto-fn env sexpr)
  (declare (ignore sexpr))
  (make-function proto-fn env))

;;; If ENV is a LET* environment that is not yet complete, then make a copy.
;;; Recurse up and see if copy needs to be done on any parent ENV - so not exactly
;;; a shallow copy. It boggles the mind to think of reasons a parent ENV would
;;; be getting sequentially bound and a descendant ENV is captured, but of course
;;; that must be dealt with properly.
;;; It seems unlikely that there would be much benefit from memoizing a
;;; frozen ENV - multiple closures over the same mutable ENV could get the
;;; same immutable copy, but where would I stash it?
(defun enclose-freeze (proto-fn env sexpr)
  (declare (ignore sexpr))
  (make-function proto-fn (freeze-env env)))

;;; Try to compile an interpreted function. If the environment
;;; contains local functions we'll punt on compiling it.
;;; Lexical vars are OK but not specials of either the bound or free variety.
;;; LEXENV-FROM-ENV fakes up a GLOBAL-VAR which is fine for
;;; limited kinds of analysis but not for really compiling (at least I
;;; think it isn't), so must be avoided. This conservatively prevents
;;; compilation when it shouldn't, but is more liberal than before.
;;; An example of an environment in which compilation should occur but won't:
;;;  (locally (declare (special x)) (symbol-macrolet ((x 3)) (defun f ...
;;;
;;; Bound specials that were proclaimed special, not locally declared,
;;; should be allowed because we don't have to inject any SPECIAL declaration
;;; or otherwise manipulate the lexenv to have the compiler know about
;;; the specialness.
;;; Free specials could be made to work fairly easily by inserting
;;; a (declare (special ...)) into the lambda, provided that there are no
;;; &optional/&key/&aux arguments. If there any hairy arguments, we don't know
;;; if some argument's defaulting form would have needed to know about the
;;; specialness of a symbol.
;;;
;;; Compiled code can access lexical vars in interpreted closures.
;;; It is achieved by changing each lexical var into a symbol macro
;;; that accesses its storage location.
;;; In practice it's likely that an interpreted closure would be "too complex"
;;; for other reasons, usually due to surrounding BLOCK. It would be somewhat
;;; nifty to walk the code and find that the block is never used.
(defun sb-c::prepare-for-compile (function &aux nullify-lexenv)
  (if (named-let too-complex-p ((env (interpreted-function-env function)))
        (when (null env)
          (return-from too-complex-p nil))
        (when (or (typep env '(or function-env block-env tagbody-env))
                  (and (typep env 'lambda-env)
                       ;; a block makes it too complex
                       (neq (lambda-frame-block-name (env-contour env)) 0)))
          (return-from too-complex-p t))
        #+nil
        (with-environment-vars (symbols end) env
          (when (or (and (= end (length symbols)) ; all symbols are accessible
                         ;; More symbols then values means free specials.
                         (> (length symbols) (length (env-payload env))))
                    (let ((frame (env-contour env)))
                      (and (typep env '(or var-env lambda-env))
                           ;; any bound specials must be proclaimed special
                           ;; so that we can just drop the binding
                           (ldb-test (byte end 0) (frame-special-b frame)))))
            (return-from too-complex-p t)))
        ;; Decls inserted into the lambda would give them a scope that is
        ;; technically "too small". This is fundamentally a problem in the
        ;; NO-HOISTING vote of X3J13. Using syntactic constructs there is
        ;; no way to guarantee a safe entry to functions if the prevailing
        ;; policy was unsafe:
        ;;   (declaim (optimize (safety 0)))
        ;;   (compile nil '(lambda (arg) (declare (optimize safety)) ...))
        ;; [They could have voted to hoist OPTIMIZE if nothing else].
        ;; But if the only decls in ENV pertain to a POLICY, then simply
        ;; propagating that policy into the resulting LEXENV is exactly right.
        (do-decl-spec (spec (env-declarations env))
          (unless (member (car spec) '(optimize))
            (return-from too-complex-p t)))
        (too-complex-p (env-parent env))) ; recurse
      ;; KLUDGE: if for no other reason than to make some assertions pass,
      ;; we'll recognize the case where the function body does not actually
      ;; need its lexical environment.
      (let ((forms (proto-fn-forms (fun-proto-fn function))))
        ;; Happily our CONSTANTP is smart enough to look into a BLOCK.
        (if (and (singleton-p forms) (constantp (car forms)))
            (setq nullify-lexenv t)
            (error 'interpreter-environment-too-complex-error
                   :format-control
                   "~@<Lexical environment of ~S is too complex to compile.~:@>"
                   :format-arguments (list function)))))
  (values (fun-lambda-expression function)
          (acond ((and (not nullify-lexenv) (interpreted-function-env function))
                  (lexenv-from-env it 'compile))
                 (t
                  (make-null-lexenv)))))

;;; Convert ENV from an interpreter environment to a compiler environment,
;;; i.e. one which is acceptable to various environment inquiry functions
;;; that do not understand interpreter environments.
;;; (Things like MACROEXPAND do understand interpreter environments)
;;; If REASON is COMPILE, then symbols which refer to the interpreter's
;;; lexical variables are changed to symbol-macros which access the
;;; interpreter variables.
;;;
;;; NB: This is not the only way to attack the problem of having the
;;; compiler deal with subtypes of ABSTRACT-LEXENV. Another way would
;;; make all access functions just do the right thing to begin with,
;;; though it might slow down the compiler a little bit, since it really
;;; likes to manipulate LEXENVs a lot.
;;; Also the parts for sb-cltl2 are fairly odious.

(defun lexenv-from-env (env &optional reason)
  (let ((lexenv (%lexenv-from-env (make-hash-table :test 'eq) env reason)))
    (setf (sb-c::lexenv-%policy lexenv) (%policy (env-contour env))
          (sb-c::lexenv-disabled-package-locks lexenv)
          (env-disabled-package-locks env))
    lexenv))

(defun %lexenv-from-env (var-map env &optional reason)
  (let ((lexenv (acond ((env-parent env) (%lexenv-from-env var-map it reason))
                       ;; The null-lexenv has to be copied into a new
                       ;; lexenv to get a snapshot of *POLICY*.
                       (t (sb-c::make-lexenv :default (make-null-lexenv)))))
        (payload (env-payload env)))
    (flet ((specialize (binding) ; = make a global var, not make less general
             (let ((sym (binding-symbol binding)))
               (cons sym (sb-c::make-global-var :special sym :declared))))
           (macroize (name thing) (list* name 'sb-sys:macro thing))
           (fname (f) (second (fun-name f))))
      (multiple-value-bind (vars funs)
          (typecase env
            ((or var-env lambda-env)
             (with-environment-vars (symbols end) env
               (loop for i fixnum from (1- end) downto 0
                     for binding = (svref symbols i)
                     for sym = (binding-symbol binding)
                  collect
                  (cond ((or (>= i (length payload))
                             (logbitp i (frame-special-b (env-contour env))))
                         (specialize binding))
                        ((eq reason 'compile)
                         ;; access interpreter's lexical vars
                         ;; Prevent SETF on the variable from getting
                         ;; "Destructive function (SETF SVREF) called on constant data"
                         (macroize sym `(svref (load-time-value ,payload) ,i)))
                        (t
                         (let ((leaf (sb-c::make-lambda-var
                                      sym
                                      :type (or (cdr binding) *universal-type*))))
                           (setf (gethash binding var-map) leaf)
                           (cons sym leaf)))))))
            (symbol-macro-env
             (nconc (map 'list
                         (lambda (cell expansion)
                           (list* (car cell) 'sb-sys:macro
                                  (acond ((cdr cell) `(the ,it ,expansion))
                                         (t expansion))))
                         (env-symbols env) payload)
                    ;; symbols without values are free specials
                    (map 'list #'specialize
                         (subseq (env-symbols env) (length payload)))))
            ((or function-env macro-env)
             ;; all symbols are free specials
             (values (map 'list #'specialize (env-symbols env))
                     (map 'list (if (macro-env-p env)
                                    (lambda (f) (macroize (fname f) f))
                                    (lambda (f &aux (name (fname f)))
                                      (cons name
                                            (sb-c::make-functional
                                             :%source-name name
                                             ;; LEXENV is given only because of
                                             ;; type-checking. Value is bogus.
                                             :lexenv (sb-kernel:make-null-lexenv)))))
                          payload)))
            (basic-env ; as in (LOCALLY (DECLARE ..))
             (values (map 'list #'specialize (env-symbols env)) nil)))
        ;; FIXME: This is a rather inefficient, and particularly ugly.
        ;; Since all the data that are needed by SB-CLTL2 are already
        ;; present in the interpreter ENV, it should just look there directly.
        ;; The saving grace is that most decls don't have a decl handler.
        (do-decl-spec (spec (env-declarations env))
          (case (car spec)
            (ignore
             ;; In (LET* ((X (F)) (X (G X))) (declare (ignore x)) ...)
             ;; it's the second X that is ignored. Does this code reflect that?
             (dolist (sym (cdr spec))
               (let ((var (cdr (assoc sym vars))))
                 (when (sb-c::lambda-var-p var)
                   (setf (sb-c::lambda-var-flags var) 1)))))
            ((inline notinline)
             ;; This is just enough to get sb-cltl2 tests to pass.
             (let ((inlinep (car spec)))
               (dolist (fname (cdr spec))
                 (let ((fun (cdr (assoc fname funs :test 'equal))))
                   (typecase fun
                     (sb-c::functional
                      (setf (sb-c::functional-inlinep fun) inlinep))
                     (null
                      (let ((defined-fun
                             (sb-c::make-defined-fun
                              fname
                              (sb-int:global-ftype fname))))
                        (setf (sb-c::defined-fun-inlinep defined-fun) inlinep)
                        (push (cons fname defined-fun) funs))))))))
            (ftype
             ;; As usual, just enough to get sb-cltl2 to pass tests.
             (let ((ctype (specifier-type (cadr spec))))
               (dolist (fname (cddr spec))
                 (let ((fun (cdr (assoc fname funs :test 'equal))))
                   (typecase fun
                     (sb-c::functional
                      (setf (sb-c::leaf-type fun) ctype)))))))
            ((ignorable type optimize special dynamic-extent)
             )
            (t
             (let ((fn (info :declaration :known (first spec))))
               (when (functionp fn)
                 (setq lexenv
                       (funcall
                        fn lexenv spec
                        ;; This is surely wrong. And as the comment above says,
                        ;; it's ridiculous that these undergo conversion at all.
                        (mapcar
                         (lambda (x)
                           (let ((thing (cdr x)))
                             (typecase thing
                               (cons x) ; symbol-macro
                               (sb-c::lambda-var thing)
                               (sb-c::global-var (sb-c::make-lambda-var (car x)
                                                                        :specvar thing)))))
                         vars)
                        ;; And surely this is wrong...
                        funs)))))))

        ;; type-restrictions are represented in the same way essentially.
        (dolist (restriction (type-restrictions (env-contour env)))
          (let ((binding (car restriction)))
            (if (consp binding)
                (push (cons (gethash binding var-map) (cdr restriction))
                      (sb-c::lexenv-type-restrictions lexenv)))))
        ;;
        (setf (sb-c::lexenv-vars lexenv) (nconc vars (sb-c::lexenv-vars lexenv))
              (sb-c::lexenv-funs lexenv) (nconc funs (sb-c::lexenv-funs lexenv))
              ;; FIXME: handled conditions
              )))
    lexenv))

(defun inline-syntactic-closure-lambda (lambda env)
  (labels ((frob (env decls)
             (unless (or (basic-env-p env)
                         (macro-env-p env)
                         (symbol-macro-env-p env))
               (return-from inline-syntactic-closure-lambda nil))
             (let ((parent (env-parent env)))
               (dolist (decl-spec (env-declarations env))
                 (dolist (decl (cdr decl-spec))
                   (push decl decls)))
               (if parent
                   (frob parent decls)
                   decls))))
    (let ((decls (frob env '()))
          (expansion (sb-walker:macroexpand-all lambda env)))
      (if decls
          `(sb-c::lambda-with-lexenv ((declare ,@decls)) ,@(cdr expansion))
          expansion))))

;;; Return INLINE or NOTINLINE if FNAME has a lexical declaration,
;;; otherwise NIL for no information.
;;; FIXME: obviously this does nothing
(defun fun-lexically-notinline-p (fname env)
  (declare (ignore fname env))
  nil)
