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
                  (declaim (inline ,constructor))
                  (defstruct
                      (,type
                        (:include basic-env) (:copier nil)
                        (:constructor ,constructor
                         (parent payload symbols contour ,@more-slots)))
                    ,@more-slots)
                  (declaim (freeze-type ,type))))))
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

;;; This implementation of lexical environments takes a fairly unconvential
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
;;; a vector of sybols, we have a cons of a fixnum and a vector, so ...
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

;; Binding frame specification for LET and LET*
;; This is a prototype stack-frame rather than a runtime stack frame in that
;; one exists per syntactic form, not per dynamic invocation of same.
(defstruct (frame (:include decl-scope)
                  (:copier nil) (:predicate nil)
                  (:constructor make-let-frame
                                (declarations %policy
                                 symbols special-b values sexpr specials)))
  ;; If more symbols exist than values, the remainder are free specials.
  (symbols    nil :read-only t :type simple-vector)
  ;; Bitmask over symbols. 1 in bit N means bind the Nth symbol as special.
  (special-b  nil :read-only t :type integer)
  ;; A let frame can't have zero values. (It would be converted to LOCALLY)
  (values nil :read-only t :type simple-vector)
  (sexpr      nil :read-only t) ; code to execute
  ;; To avoid reconstituting the first PROGV operand from the special-b mask
  ;; and vector of all bound symbols, store the bound specials as follows:
  ;;   for LET    - a list of all bound specials
  ;;   for LET*   - a list of singleton lists of bound specials
  ;;   for LAMBDA - possibly both of the preceding: a list for mandatory args
  ;;                and lists of singleton lists for &optional/&rest/&key.
  (specials   nil :read-only t))

(defmethod print-object ((self frame) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(declaim (inline frame-size))
(defun frame-size (frame) (length (frame-values frame)))

;;; A LET* frame is just like a LET frame, but needs two policies.
(defstruct (let*-frame (:include frame) (:predicate nil) (:copier nil)
                       (:constructor make-let*-frame
                                     (declarations %old-policy %policy
                                      symbols special-b values sexpr specials)))
  (%old-policy nil :type sb-c::policy :read-only t))

;;; BASIC-ENV stores a policy for its body, but if evaluation has not reached
;;; the body forms of a LET*, then the old policy is in effect. This is due to
;;; the need for presenting a consistent view of the policy, but a frozen ENV
;;; does not get a different static environment - it sees the same object
;;; that the body forms see.
(defun env-policy (env)
  (cond ((not env) sb-c::*policy*)
        ((env-mutable-p env) (let*-frame-%old-policy (env-contour env)))
        (t (let ((policy (%policy (env-contour env))))
             ;; If the policy contains no qualities, look to the parent env.
             ;; This happens with a BLOCK or TAGBODY, as well as the
             ;; environment in which LABELS are defined.
             (if (eql (sb-c::policy-presence-bits policy) 0)
                 (env-policy (env-parent env))
                 policy)))))

;; Fancy binding frame specification
(defstruct (lambda-frame (:include let*-frame) (:predicate nil) (:copier nil))
  ;; Unlike for a LET* frame the count of bound values can not be determined
  ;; from the length of the VALUES vector, which contains various extra markers
  ;; dictating how the arguments are to be parsed.
  (n-bound-vars  0   :read-only t :type fixnum)
  ;; Number of mandatory and optional arguments.
  (min-args      0   :read-only t :type fixnum)
  (n-optional    0   :read-only t :type fixnum)
  ;; Packed flags indicating presence of &REST/&KEY/&ALLOW-OTHER-KEYS.
  (keyword-bits  0   :read-only t :type fixnum)
  ;; A BLOCK name in which to wrap the lambda's evaluable forms.
  ;; This behaves exactly the same as using a block-env around the forms,
  ;; however a lambda-env consumes only 8 words plus 2 for the freshly consed
  ;; catch tag; whereas a var-env + block-env would consume 6 + 6 + 2, which
  ;; entails 40% more overhead to enter the most trivial interpreted function.
  (block-name    0   :read-only t :type (or (eql 0) symbol))
  ;; SHARE-BLOCK-P is T if BLOCK-NAME can be created concurrently
  ;; with variable bindings. If NIL when a block-name is present,
  ;; then another environment is allocated to enclose the block,
  ;; which is not as big a win as combining the block-env and var-env,
  ;; but still beneficial as it avoids separately calling the block handler.
  (share-block-p nil :read-only t :type boolean))

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

;;; Computation of package locks is done lazily and usually memoized,
;;; but not not memoized if the env is mutable.
;;; This is not as critical to performance as an efficient implementation
;;; of ENV-POLICY so we don't memoize the "old" value. Computing it might
;;; have to bubble up to the null lexenv. But that seems ok for two reasons:
;;; - the only likely way that a closure would see a "wrong" value for this
;;;   is if the special variable SB-C::*DISABLED-PACKAGE-LOCKS* were
;;;   changed at an inopportune time. It doesn't seem worth worry about.
;;; - even when unmemoized, this is only computed once per binding form,
;;;   whereas a valid POLICY is required at each read or write of a lexical
;;;   variable. So the binding forms scale much better - binding 20 variables
;;;   only compute the package locks once, at parse time of the form.
(defun env-disabled-package-locks (env)
  (cond ((not env) sb-c::*disabled-package-locks*)
        ((not (env-mutable-p env))
         (let ((list (%disabled-package-locks (env-contour env))))
           (when (eq list :uncomputed)
             (setq list (env-disabled-package-locks (env-parent env)))
             (do-decl-spec (declaration (env-declarations env))
               (case (car declaration)
                 ((disable-package-locks enable-package-locks)
                  (setq list
                        (sb-c::process-package-lock-decl declaration list)))))
             (setf (%disabled-package-locks (env-contour env)) list))
           list))
        (t (env-disabled-package-locks (env-parent env)))))

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
           (,end (locally (declare (optimize (safety 0))) (car it))))
       (declare (index-or-minus-1 ,end))
       ,@body)))
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
  (loop for i fixnum from 0 below (length typechecks) by 2
        for frame-ptr = (svref typechecks i)
        ;; FIXME: fast accessor for small values of FRAME-PTR ?
        for val = (%cell-ref env frame-ptr)
        for type = (svref typechecks (logior i 1))
        unless (itypep val type)
        do (typecheck-fail/ref (frame-symbol env frame-ptr) val type)))

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
  (let ((name (fun-name obj)))
    (if (eql name 0)
        ;; To avoid an extra space between type and identity, the body must
        ;; be empty, so we need two cases, because emptiness is compile-time
        ;; determined, not based on whether the body actually printed anything.
        (print-unreadable-object (obj stream :type t :identity t))
        ;; show name whenever NAME it is not 0, even if not OBJ's proper name.
        (print-unreadable-object (obj stream :type t)
          (prin1 name stream)))))

;; Return approximately a type specifier for LAMBDA-LIST.
;; e.g. after doing (DEFUN FOO (A B) ...), you want (FUNCTION (T T) *)
;; This is mainly to get accurate information from DESCRIBE
;; when properly hooked in. Attempting to intercept %FUN-TYPE caused
;; some problems, pending investigation.
(defun approximate-proto-fn-type (lambda-list bound-symbols)
  (declare (notinline member cons))
  (labels ((recurse (list var-index)
             (unless (or (endp list) (eq (car list) '&aux))
               (let* ((elt (car list))
                      (ll-keyword-p (member elt lambda-list-keywords))
                      (rest (cdr list)))
                 (cons (cond (ll-keyword-p elt)
                             ((not var-index) 't)
                             (t (type-specifier
                                 (cdr (svref bound-symbols var-indeX)))))
                       (if (eq elt '&key)
                           (keys rest)
                           (recurse rest
                                    (and var-index
                                         (not ll-keyword-p)
                                         (1+ var-index))))))))
           (keys (list)
             (unless (or (endp list) (eq (car list) '&aux))
               (let ((elt (car list)))
                 (cons (if (member elt lambda-list-keywords)
                           elt
                           (let ((sym (if (listp elt) (car elt) elt)))
                             `(,(if (atom sym) (keywordicate sym) (car sym))
                               t)))
                       (keys (cdr list)))))))
    `(function ,(recurse lambda-list 0) *)))

(declaim (type boolean *hook-all-functions*))
(defvar *hook-all-functions-p* nil)

(declaim (ftype function interpreter-trampoline interpreter-hooked-trampoline))

(defun make-function (proto-fn env)
  (let ((function (%make-interpreted-function proto-fn env nil nil)))
    ;; Hooking all functions, makes them somewhat slower,
    ;; but allows for really nifty introspection,
    ;; such as discovering what calls are made by read-time evals.
    (setf (funcallable-instance-fun function)
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

(defun flush-macros () (atomic-incf *globaldb-cookie*))

;; Return two values: FRAME and COOKIE, recomputing if cookie doesn't match
;; globaldb, otherwise return the previously computed information.
(declaim (inline proto-fn-frame))
(defun proto-fn-frame (proto-fn env)
  (if (eq (proto-fn-cookie proto-fn) *globaldb-cookie*)
      (values (proto-fn-%frame proto-fn) (proto-fn-cookie proto-fn))
      (digest-lambda env proto-fn)))

(defun %fun-type (fun)
  (let ((proto-fn (interpreted-function-proto-fn fun)))
    (or (proto-fn-type proto-fn)
        (setf (proto-fn-type proto-fn)
              (approximate-proto-fn-type
               (proto-fn-lambda-list proto-fn)
               (frame-symbols
                (proto-fn-frame (interpreted-function-proto-fn fun)
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
    (or (find id '(ignorable ignore type special
                   dynamic-extent truly-dynamic-extent))
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
  (let ((specials nil))
    (do-decl-spec (decl-spec decls-list)
      (when (eql (car decl-spec) 'special)
        (dolist (var (cdr decl-spec))
          (pushnew var specials :test #'eq))))
    specials))

;;; See if all SYMBOLS can be declare special.
;;; This applies to both free and bound variables.
(defun assert-declarable-as-special (env symbols)
  (declare (ignore env))
  (dolist (name symbols)
    (unless (symbolp name)
      (ip-error "~A is not a symbol" name))
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
  (let ((symbols (declared-specials decls)))
    (when symbols
      (with-package-lock-context (env)
        (assert-declarable-as-special env symbols))
      (map 'vector #'list (the list symbols)))))

;;; Return the type of a lexically bound lexical var.
;;; Doesn't work for specially bound or free special.
(defun binding-type (cell) (cdr cell))

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

;;; Split off the declarations (and the docstring, if DOCSTRING-ALLOWED
;;; is true) from the actual forms of BODY.
;;; Also do some rudimentary checks on the declarations (if any).
;;; Return three values: the cons in BODY containing the first evaluable
;;; subform, a list of the declarations, and a docstring if present.
;;; KLUDGE: would be nice to share the compiler's PARSE-BODY,
;;; except that because this is called potentially a lot more than
;;; once per input form, we don't want the "DECLAIM where DECLARE"
;;; style-warning every time. It's only a warning, not an error.
;;; KLUDGE: Genesis can't shadow parse-body, so name it differently.
(defun iparse-body (body &optional docstring-allowed)
  (let (decls-list docstring)
    (flet ((quick-validate-decls (subexpr &aux (specs (cdr subexpr)))
             (unless (and (proper-list-p specs) (every #'consp specs))
               (ip-error "malformed declaration ~S" subexpr))
             ;; It's faster to store the list of DECLARE expressions
             ;; sans initial DECLARE rather than append together
             ;; into a list of just the decl-specs of all of them.
             (dolist (decl specs)
               (when (eq (car decl) 'optimize)
                 (dolist (element (cdr decl))
                   (let ((quality
                           (if (atom element)
                               element
                               (with-subforms (quality value) element
                                 (declare (ignore value))
                                 quality))))
                     (unless (sb-c::policy-quality-name-p quality)
                       (warn "ignoring unknown optimization quality ~S in ~S"
                             quality subexpr))))))
             (push specs decls-list)))
      (macrolet
          ((scan (checking-docstring)
             `(loop
               (let ((form (car body)))
                 (cond ((listp form)
                        (cond ((eql (car form) 'declare)
                               (quick-validate-decls form)
                               (pop body))
                              (t
                               (return))))
                       ((and ,checking-docstring (stringp form))
                        ;; CLHS 3.4.11
                        (cond ((not (cdr body)) (return))
                              (docstring ; "consequences are unspecified"
                               (ip-error "~@<Duplicate doc string ~S.~:@>"
                                         form))
                              (t
                               (pop body) (setf docstring form))))
                       (t (return)))))))
        ;; slight optimization: two variations of the loop
        (if docstring-allowed (scan t) (scan nil))
        (values body (nreverse decls-list) docstring)))))

(defun make-proto-fn (lambda-expression &optional (silent t))
  (multiple-value-bind (name lambda-list body)
      (if (eq (car lambda-expression) 'named-lambda)
          (with-subforms (name lambda-list . body) (cdr lambda-expression)
            (values name lambda-list body))
          (with-subforms (lambda-list . body) (cdr lambda-expression)
            (values 0 lambda-list body)))
    ;; Choke now if the list can't be parsed.
    ;; If lexical environment is NIL, :silent will be passed as NIL,
    ;; and we can warn about "suspcious variables" and such.
    (parse-lambda-list lambda-list :silent silent)
    (multiple-value-bind (forms decls docstring) (iparse-body body t)
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
         (when definition
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
       (let ((cell (truly-the cons (svref symbols index))))
         (when (eq (car cell) sym)
           (multiple-value-bind (kind value)
               (cond ((or (var-env-p env) (lambda-env-p env))
                      (let ((values (the simple-vector (env-payload env))))
                        (if (and (< index (length values))
                                 (not (logbitp index
                                               (frame-special-b
                                                (env-contour env)))))
                            (values :normal (svref values index))
                            :special)))
                     ((symbol-macro-env-p env)
                      (let ((values (the simple-vector (env-payload env))))
                        (if (< index (length values))
                            (values :macro (svref values index))
                            :special)))
                     (t ; function-env, macro-env, basic-env (locally).
                      :special)) ; no symbol is bound
             (return-from find-lexical-var
               (values cell kind (make-frame-ptr index level) value)))))))))

;;; Similar to the above, but only return a lexically visible special binding.
;;; This is required to locate the intended binding in cases such as this:
#|
 (let ((a 3)) ; special A
   (declare (special a))
   (symbol-macrolet ((a x)) ; macro A
     (let ((a (foo))) ; lexical A
       (macrolet ((foo () ...))
         (declare (real a) (special a)) ; declares the type of special A
|#
(defun find-bound-special (env sym)
  (do ((env env (env-parent env))
       (level 0 (1+ level)))
      ((null env) (values nil nil nil))
    (declare (type (unsigned-byte #.+frame-depth-bits+) level))
    ;; Only LET-like frames can create special bindings.
    (when (or (var-env-p env) (lambda-env-p env))
      (with-environment-vars (symbols index) env ; skipped if no symbols
        (loop
         (when (minusp (decf index)) (return))
         (let ((cell (truly-the cons (svref symbols index))))
           (when (eq (car cell) sym)
             ;; If this frame declare SYM as a free special,
             ;; it can't also bind it, so bail out now.
             (unless (cdr cell) (return))
             (when (logbitp index (frame-special-b (env-contour env)))
               (return-from find-bound-special cell))
             ;; If it was not special, then there can be no other match
             ;; in this frame - if duplicately named bindings exists,
             ;; at most the last one could be special.
             (return))))))))

;;; Look backwards for any type restriction pertinent to BINDING.
;;; If none is found then return the restriction in BINDING itself.
;;; We skip over LET* (or LAMBDA) frames whose body has not been
;;; entered but which are present in the environment chain.
;;; This potentially scans backwards beyond the frame in which
;;; BINDING is made, but no harm can come of it,
;;; since bindings are unique objects - you can't find the wrong one.
(defun find-bound-type-restriction (env binding)
  (do ((env env (env-parent env))
       (level 0 (1+ level)))
      ((null env) (cdr binding))
    (declare (type (unsigned-byte #.+frame-depth-bits+) level))
    (unless (env-mutable-p env)
      (awhen (assq binding
                   (bound-var-type-restrictions (env-contour env)))
        (return (cdr it))))))

;;; As above but look for SYMBOL in the free type restrictions,
;;; and if not found, return *UNIVERSAL-TYPE*.
(defun find-free-type-restriction (env symbol)
  (do ((env env (env-parent env))
       (level 0 (1+ level)))
      ((null env) *universal-type*)
    (declare (type (unsigned-byte #.+frame-depth-bits+) level))
    (unless (env-mutable-p env)
      (awhen (assq symbol (free-var-type-restrictions (env-contour env)))
        (return (cdr it))))))

;;; Update DECL-SCOPE with type restrictions based on its declarations.
;;; Restrictions are separated into those which apply to bindings made by
;;; this scope - each being pertinent as soon as the variable to which it
;;; applies is bound - and those which apply to the body forms.
;;; The latter are further subdivided into restrictions which apply to
;;; a lexically visible binding (possibly special) versus a free declaration.
;;;
;;; Bound lexical and special variables in the new scope have the CTYPE
;;; as stored in the CDR of the binding cell altered to reflect the
;;; restriction. Bound variables of containing scopes get a new restriction
;;; placed in the BOUND-VAR-TYPE-RESTRICTIONS of this new scope.
;;; Free declarations go in FREE-VAR-TYPE-RESTRICTIONS.
;;;
;;; FIXME: global DECLAIM that affects the interpreter's TYPE-CHECK policy
;;; needs to kick the globaldb so that all intepreted function re-consider
;;; whether to perform checks.
(defun process-typedecls (decl-scope env symbols)
  (let (binding-types-p ; T if any binding made in this scope has a type declaration
        new-lexical-bound-type-constraints
        new-special-bound-type-constraints
        new-free-type-constraints
        (symbols (if symbols (the simple-vector symbols) #())))
    ;; Intersect all the declarations made about a single symbol in the
    ;; current scope with the prevailing type from enclosing context.
    (flet ((add-bound-type-constraint (binding ctype list)
             (let ((found (assq binding list)))
               (if found
                   (progn (rplacd found (type-intersection (cdr found) ctype))
                          list)
                   (let* ((old-type (find-bound-type-restriction env binding))
                          (new-type (type-intersection ctype old-type)))
                     (if (type= old-type new-type)
                         list ; don't create a new irrelevant constraint
                         (acons binding new-type list)))))))
      ;; First we compute the effective set of type restrictions.
      (do-decl-spec (decl (declarations decl-scope))
        (when (eq (applies-to-variables-p decl) 'type)
          (multiple-value-bind (type-spec names)
              (if (eq (car decl) 'type)
                  (values (cadr decl) (cddr decl))
                  (values (car decl) (cdr decl)))
            (let ((ctype (specifier-type type-spec)))
              (unless (eq ctype *universal-type*)
                (dolist (symbol names)
                  (block done
                    (acond ((find (the symbol symbol) symbols
                                  :test #'eq :key #'car :from-end t)
                            ;; Type restrictions aren't pervasive downward,
                            ;; so this is the only case that doesn't have to
                            ;; search backward for the most recent restriction.
                            ;; [Global declamations are pervasive downward,
                            ;; but violations will be caught by the runtime]
                            (when (cdr it) ; binding is established by this scope
                              (return-from done
                                (setf (cdr it) (type-intersection (cdr it) ctype)
                                      binding-types-p t)))
                            ;; The symbol is declared special in this scope.
                            ;; Either it exposes a special binding that was
                            ;; hidden by a non-special, or it's a free special.
                            (awhen (find-bound-special env symbol)
                              (return-from done
                                (setq new-special-bound-type-constraints
                                      (add-bound-type-constraint
                                       it ctype new-special-bound-type-constraints)))))
                           (t ; otherwise search in enclosing scopes
                            (multiple-value-bind (binding kind)
                                (find-lexical-var env symbol)
                              (when (cdr binding) ; found a lexical or special bound var
                                (case kind
                                  (:normal
                                   (setq new-lexical-bound-type-constraints
                                         (add-bound-type-constraint
                                          binding ctype new-lexical-bound-type-constraints)))
                                  (:special
                                   (setq new-special-bound-type-constraints
                                         (add-bound-type-constraint
                                          binding ctype new-special-bound-type-constraints))))
                                (return-from done)))))
                    ;; Add a free constraint
                    (let ((found (assq symbol new-free-type-constraints)))
                      (if found
                          (rplacd found (type-intersection (cdr found) ctype))
                          (let* ((old-type (find-free-type-restriction env symbol))
                                 (new-type (type-intersection ctype old-type)))
                            (unless (type= old-type new-type)
                              (push (cons symbol new-type)
                                    new-free-type-constraints)))))))))))))

    (setf (bound-var-type-restrictions decl-scope)
          (nconc new-lexical-bound-type-constraints
                 new-special-bound-type-constraints
                 (bound-var-type-restrictions decl-scope)))
    (setf (free-var-type-restrictions decl-scope)
          (nconc new-free-type-constraints
                 (free-var-type-restrictions decl-scope)))

    ;; Now if the enclosing policy - not the new policy - demands typechecks,
    ;; then insert assertions for all variables bound by this scope,
    ;; as well as bound lexicals declared to have a more restrictive type
    ;; in this scope than in the prevailing scope. Symbol-macros do not generate
    ;; checks, nor do free specials nor bound specials from enclosing scopes.
    ;; In the macro case it would entail arbitrary evaluation, and specials
    ;; might err with an unbound symbol. Symbol-macros are checked when storing
    ;; through them though.
    (when (and binding-types-p (policy env (>= safety 1)))
      (let ((checks (make-array (1+ (position-if #'cdr symbols :from-end t)))))
        (dotimes (i (length checks)
                    (setf (binding-typechecks decl-scope) checks))
          (let ((ctype (cdr (svref symbols i))))
            (when (neq ctype *universal-type*)
              (setf (svref checks i) (type-checker ctype)))))))
    ;; If a nested scope re-declares a variable to be of a more constrained
    ;; type "for efficiency" it does not really help the interpreter any,
    ;; so don't do those checks unless SAFETY exceeds 2.
    ;; This is a somewhat arbitrary but reasonable stance to take.
    (when (and new-lexical-bound-type-constraints (policy env (>= safety 2)))
      (let (extra)
        (dolist (check new-lexical-bound-type-constraints)
          ;; FIXME: is this right for a LET* frame?
          (setf extra
                ;; (NTH-VALUE 2) is the frame pointer.
                (list* (nth-value 2 (find-lexical-var env (caar check)))
                       (type-checker (cdr check)) extra)))
        (setf (extra-typechecks decl-scope) (coerce extra 'vector)))))
  decl-scope)

;;; Return the innermost element of bound-var-type-restrictions in ENV
;;; that refers to BINDING. This is only for cltl2.
(defun var-type-restriction (env binding)
  (do ((env env (env-parent env)))
      ((null env) (cdr binding))
    (awhen (assq binding (bound-var-type-restrictions (env-contour env)))
      (return (cdr it)))))

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
;;; VARIABLE is a cell, if a lexical variable binding,
;;; or just a symbol, if a lexical symbol-macro.
;;; The FRAME-PTR should be supplied as an optimization when VARIABLE is a cell
;;; (the answer doesn't depend on this) but is required for correct semantics
;;; when VARIABLE is a symbol. It prevents scanning backwards too far.
(defun var-type-assertion (env frame-ptr variable op)
  (unless variable
    (return-from var-type-assertion nil))
  ;; ** these criteria are subject to change. Not sure they're the best.
  (when (ecase op
          (:write (policy env (>= safety 1)))
          (:read  (policy env (and (= safety 3) (= speed 0)))))
    (if frame-ptr
        (do ((env env (env-parent env))
             (n-iterations (frame-ptr-depth frame-ptr)))
            ((null env) (bug "can't get here"))
          (when (zerop n-iterations)
            (let ((answer (cdr variable)))
              (return (unless (eq answer *universal-type*) answer))))
          (awhen (assq variable
                       (bound-var-type-restrictions (env-contour env)))
            (return (cdr it)))
          (decf n-iterations))
        (do ((env env (env-parent env)))
            ((null env) nil)
          (awhen (assq variable
                       (free-var-type-restrictions (env-contour env)))
            (return (cdr it)))))))

;; Convert compiler env to interpreter env
(defun env-from-lexenv (lexenv)
  (when (sb-c::null-lexenv-p lexenv)
    (return-from env-from-lexenv nil))
  ;; a lexical environment which includes locally [not]inline
  ;; declarations about global functions is ok
  (flet ((harmless-defined-fun-p (thing)
           (and (typep thing 'sb-c::defined-fun)
                (eq (sb-c::defined-fun-kind thing) :global-function)))
         (macro-p (thing)
           (and (consp thing) (eq (car thing) 'sb-sys:macro))))
    (let ((native-funs (sb-c::lexenv-funs lexenv))
          (native-vars (sb-c::lexenv-vars lexenv)))
      (when (or (sb-c::lexenv-blocks lexenv)
                (sb-c::lexenv-tags lexenv)
                (sb-c::lexenv-lambda lexenv)
                (sb-c::lexenv-cleanup lexenv)
                (sb-c::lexenv-type-restrictions lexenv)
                (find-if-not (lambda (x) (or (macro-p x)
                                             (harmless-defined-fun-p x)))
                             native-funs :key #'cdr)
                (find-if-not #'macro-p native-vars :key #'cdr))
        (error 'compiler-environment-too-complex-error
               :format-control
               "~@<Lexical environment is too complex to evaluate in: ~S~:@>"
               :format-arguments (list lexenv)))
      (let ((macro-env
             ;; Macros in an interpreter environment must look like interpreted
             ;; functions due to use of FUN-NAME to extract their name as a key
             ;; rather than also needing an alist mapping names to objects.
             ;; For each compiled expander, wrap it in a trivial interpreted fun.
             (make-macro-env
              nil
              (map 'vector
                   (lambda (cell)
                     (let ((expander (cddr cell)))
                       (if (interpreted-function-p expander)
                           expander
                           (make-function
                            (%make-proto-fn `(macrolet ,(car cell)) '(form env)
                                            nil ; decls
                                            `((funcall ,expander form env)) nil)
                            nil)))) ; environment for the interpreted fun
                   (remove-if-not #'macro-p native-funs :key #'cdr))
              nil ; no free specials vars
              ;; FIXME: type-restrictions, package-locks, handled-conditions.
              (make-decl-scope nil (sb-c::lexenv-policy lexenv)))))
        (if native-vars
            (make-symbol-macro-env macro-env
                                   (map 'vector #'cddr native-vars)
                                   (map 'vector #'car native-vars)
                                   (make-decl-scope
                                    nil (sb-c::lexenv-policy lexenv)))
            macro-env)))))

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
(defun prepare-for-compile (function &aux nullify-lexenv)
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
      (let ((forms (proto-fn-forms (interpreted-function-proto-fn function))))
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
  (let ((lexenv (acond ((env-parent env) (lexenv-from-env it))
                       ;; The null-lexenv has to be copied into a new
                       ;; lexenv to get a snapshot of *POLICY*.
                       (t (sb-c::make-lexenv :default (make-null-lexenv)))))
        (payload (env-payload env)))
    (flet ((specialize (sym) ; = make a global var, not make less general
             (let ((sym (car sym)))
               (cons sym (make-global-var :%source-name sym
                                          :kind :special
                                          :where-from :declared))))
           (macroize (name thing) (list* name 'sb-sys:macro thing))
           (fname (f) (second (fun-name f))))
      (multiple-value-bind (vars funs)
          (typecase env
            ((or var-env lambda-env)
             (with-environment-vars (symbols end) env
               (loop for i fixnum from (1- end) downto 0
                     for cell = (svref symbols i)
                     for sym = (car cell)
                  collect
                  (cond ((or (>= i (length payload))
                             (logbitp i (frame-special-b (env-contour env))))
                         (specialize cell))
                        ((eq reason 'compile)
                         ;; access interpreter's lexical vars
                         (macroize sym `(svref ,payload ,i)))
                        (t
                         ;; The type of the var is returned in a sort of
                         ;; cheating way - we put the type restriction
                         ;; into the leaf type even though a LEXENV would
                         ;; ordinarily represent the leaf-type and separately
                         ;; the type constraint, just as the interpreter does.
                         ;; But sb-cltl2 is just going intersect those anyway.
                         (cons sym (make-lambda-var
                                    :%source-name sym
                                    :type (var-type-restriction env cell))))))))
            (symbol-macro-env
             (nconc (map 'list
                         (lambda (cell expansion)
                           (let ((type (cdr cell)))
                             (list* (car cell)
                                    'sb-sys:macro
                                    (if (eq type *universal-type*)
                                        expansion
                                        `(the ,type ,expansion)))))
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
             (let ((inlinep (case (car spec)
                              (inline :inline)
                              (notinline :notinline))))
               (dolist (fname (cdr spec))
                 (let ((fun (cdr (assoc fname funs :test 'equal))))
                   (typecase fun
                     (sb-c::functional
                      (setf (sb-c::functional-inlinep fun) inlinep))
                     (null
                      (let ((defined-fun
                             (sb-c::make-defined-fun
                              :%source-name fname
                              :type (sb-int:proclaimed-ftype fname))))
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
             (let ((fn (info :declaration :handler (first spec))))
               (when fn
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
                               (sb-c::global-var (make-lambda-var
                                                  :specvar thing
                                                  :%source-name (car x))))))
                         vars)
                        ;; And surely this is wrong...
                        funs)))))))
        ;;
        (setf (sb-c::lexenv-vars lexenv) (nconc vars (sb-c::lexenv-vars lexenv))
              (sb-c::lexenv-funs lexenv) (nconc funs (sb-c::lexenv-funs lexenv))
              (sb-c::lexenv-%policy lexenv) (%policy (env-contour env))
              ;; FIXME: package locks, handled conditions
              )))
    lexenv))

;;; Produce the source representation expected by :INLINE-EXPANSION-DESIGNATOR.
;;; This is less capable than the correspoding logic in RECONSTRUCT-LEXENV
;;; but should not be too difficult to enhance to match.
(defun reconstruct-syntactic-closure-env (env)
  (flet ((externalize (env nest)
           (typecase env
             (symbol-macro-env
              (let ((symbols (env-symbols env))
                    (expansions (env-payload env)))
                (when (and (null (env-declarations env))
                           ;; More symbols than expansions = free specials.
                           (= (length symbols) (length expansions)))
                  (list* :symbol-macro
                         (map 'list (lambda (x y) (list (car x) y))
                              symbols expansions)
                         nest))))
             (macro-env
              (when (and (null (env-declarations env))
                         (null (env-symbols env)))
                (list*
                 :macro
                 (map 'list
                      (lambda (f)
                        ;; The name of each macro is (MACROLET symbol).
                        (cons (second (fun-name f))
                              (fun-lambda-expression f)))
                      (env-payload env))
                 nest))))))
    (let ((nest nil))
      (loop
       (let ((sexpr (externalize env nest)))
         (if sexpr
             (acond ((env-parent env) (setq nest (list sexpr) env it))
                    (t (return sexpr)))
             (return nil)))))))

;;; Return :INLINE or :NOTINLINE if FNAME has a lexical declaration,
;;; otherwise NIL for no information.
;;; FIXME: obviously this does nothing
(defun fun-lexically-notinline-p (fname env)
  (declare (ignore fname env))
  nil)
