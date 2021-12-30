;;;; This file implements the IR1 finalize phase, which checks for
;;;; various semantic errors.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Give the user grief about optimizations that we weren't able to
;;; do. It is assumed that the user wants to hear about this, or there
;;; wouldn't be any entries in the table. If the node has been deleted
;;; or is no longer a known call, then do nothing; some other
;;; optimization must have gotten to it.
(defun note-failed-optimization (node failures)
  (declare (type combination node) (list failures))
  (unless (or (node-deleted node)
              (not (eq :known (combination-kind node))))
    (let ((*compiler-error-context* node))
      (dolist (failure failures)
        (let ((what (cdr failure))
              (note (transform-note (car failure))))
          (cond
           ((consp what)
            (compiler-notify "~@<unable to ~2I~_~A ~I~_because: ~2I~_~?~:>"
                             note (first what) (rest what)))
           ((valid-fun-use node what
                           :argument-test #'types-equal-or-intersect
                           :result-test #'values-types-equal-or-intersect)
            (collect ((messages))
              (flet ((give-grief (string &rest stuff)
                       (messages string)
                       (messages stuff)))
                (valid-fun-use node what
                               :unwinnage-fun #'give-grief
                               :lossage-fun #'give-grief))
              (compiler-notify "~@<unable to ~
                                ~2I~_~A ~
                                ~I~_due to type uncertainty: ~
                                ~2I~_~{~?~^~@:_~}~:>"
                             note (messages))))
           ;; As best I can guess, it's OK to fall off the end here
           ;; because if it's not a VALID-FUNCTION-USE, the user
           ;; doesn't want to hear about it. The things I caught when
           ;; I put ERROR "internal error: unexpected FAILURE=~S" here
           ;; didn't look like things we need to report. -- WHN 2001-02-07
           ))))))

;;; For each named function with an XEP, note the definition of that
;;; name, and add derived type information to the INFO environment. We
;;; also delete the FUNCTIONAL from (FREE-FUNS *IR1-NAMESPACE*) to eliminate the
;;; possibility that new references might be converted to it.
(defun finalize-xep-definition (fun)
  (let* ((leaf (functional-entry-fun fun))
         (ns *ir1-namespace*)
         (defined-ftype (definition-type leaf)))
    (setf (leaf-type leaf) defined-ftype)
    (when (and (leaf-has-source-name-p leaf)
               (eq (leaf-source-name leaf) (functional-debug-name leaf))
               (functional-top-level-defun-p leaf))
      (let ((source-name (leaf-source-name leaf)))
        (let* ((where (info :function :where-from source-name))
               (*compiler-error-context* (lambda-bind (main-entry leaf)))
               (global-def (gethash source-name (free-funs ns)))
               (global-p (defined-fun-p global-def)))
          (note-name-defined source-name :function)
          (when global-p
            (remhash source-name (free-funs ns)))
          (ecase where
            (:assumed
             (let ((approx-type (info :function :assumed-type source-name)))
               (when (and approx-type (fun-type-p defined-ftype))
                 (valid-approximate-type approx-type defined-ftype))
               ;; globaldb can't enforce invariants such as :assumed-type and
               ;; :type being mutually exclusive. For that reason it would have
               ;; made sense to use a single info-type holding either a true
               ;; function type or an approximate-fun-type. Regardless, it is
               ;; slightly preferable to clear the old before setting the new.
               (clear-info :function :assumed-type source-name)
               (setf (info :function :type source-name) defined-ftype))
             (setf (info :function :where-from source-name) :defined))
            ((:declared :defined-method)
             (let ((declared-ftype (global-ftype source-name)))
               (unless (defined-ftype-matches-declared-ftype-p
                         defined-ftype declared-ftype)
                 (compiler-style-warn
                  "~@<The previously declared FTYPE~
                   ~2I ~_~/sb-impl:print-type/~I ~_~
                   conflicts with the definition type ~
                   ~2I~_~/sb-impl:print-type/~:>"
                  declared-ftype defined-ftype))))
            (:defined
             (setf (info :function :type source-name) defined-ftype)))))))
  (values))

;;; Find all calls in COMPONENT to assumed functions and update the
;;; assumed type information. This is delayed until now so that we
;;; have the best possible information about the actual argument
;;; types.
(defun note-assumed-types (component name var)
  (when (and (eq (leaf-where-from var) :assumed)
             (not (and (defined-fun-p var)
                       (eq (defined-fun-inlinep var) 'notinline)))
             (eq (info :function :where-from name) :assumed)
             (eq (info :function :kind name) :function))
    (let ((atype (info :function :assumed-type name)))
      (dolist (ref (leaf-refs var))
        (let ((dest (node-dest ref)))
          (when (and (eq (node-component ref) component)
                     (combination-p dest)
                     (eq (lvar-uses (basic-combination-fun dest)) ref))
            (setq atype (note-fun-use dest atype)))))
      (setf (info :function :assumed-type name) atype))))

;;; Merge CASTs with preceding/following nodes.
(defun ir1-merge-casts (component)
  (do-blocks-backwards (block component)
    (do-nodes-backwards (node lvar block :restart-p t)
      (let ((dest (when lvar (lvar-dest lvar))))
        (cond ((and (cast-p dest)
                    (not (cast-type-check dest))
                    (almost-immediately-used-p lvar node))
               (let ((dtype (node-derived-type node))
                     (atype (node-derived-type dest)))
                 (when (values-types-equal-or-intersect
                        dtype atype)
                   ;; FIXME: We do not perform pathwise CAST->type-error
                   ;; conversion, and type errors can later cause
                   ;; backend failures. On the other hand, this version
                   ;; produces less efficient code.
                   ;;
                   ;; This is sorta DERIVE-NODE-TYPE, but does not try
                   ;; to optimize the node.
                   (setf (node-derived-type node)
                         (values-type-intersection dtype atype)))))
              ((and (cast-p node)
                    (eq (cast-type-check node) :external))
               (aver (basic-combination-p dest))
               (delete-filter node lvar (cast-value node))))))))

(defglobal *two-arg-functions*
    `((* two-arg-* (,(specifier-type 'fixnum) ,(specifier-type 'fixnum)) multiply-fixnums)
      (+ two-arg-+)
      (- two-arg--)
      (/ two-arg-/ (,(specifier-type 'integer) ,(specifier-type 'integer)) sb-kernel::integer-/-integer)
      (< two-arg-<)
      (= two-arg-=)
      (> two-arg->)
      (char-equal two-arg-char-equal)
      (char-greaterp two-arg-char-greaterp)
      (char-lessp two-arg-char-lessp)
      (char-not-equal two-arg-char-not-equal)
      (char-not-greaterp two-arg-char-not-greaterp)
      (char-not-lessp two-arg-char-not-lessp)
      (gcd two-arg-gcd)
      (lcm two-arg-lcm)
      (logand two-arg-and)
      (logior two-arg-ior)
      (logxor two-arg-xor)
      (logeqv two-arg-eqv)
      (string= two-arg-string=)
      (string-equal two-arg-string-equal)
      (string< two-arg-string<)
      (string> two-arg-string>)
      (string<= two-arg-string<=)
      (string>= two-arg-string>=)
      (string/= two-arg-string/=)
      (string-lessp two-arg-string-lessp)
      (string-greaterp two-arg-string-greaterp)
      (string-not-lessp two-arg-string-not-lessp)
      (string-not-greaterp two-arg-string-not-greaterp)
      (string-not-equal two-arg-string-not-equal)))

(defmacro def-two-arg-fun (types function)
  (let ((name (symbolicate 'two-arg- function)))
    `(progn
       (defknown ,name ,types boolean ())
       (defun ,name (a b)
         (,function a b))
       (pushnew (list ',function ',name) *two-arg-functions* :key #'car))))

(defmacro def-two-arg-funs (types &body functions)
  `(progn
     ,@(loop for fun in functions
             collect `(def-two-arg-fun ,types ,fun))))

(def-two-arg-funs (character character)
  char= char/= char< char> char<= char>=)
(def-two-arg-funs (number number)
  >= <= /=)

;;; A list of function which always call their functional argument correctly,
;;; meaning that no arg-count-error can occur in the callee
;;; and therefore the callee can skip the check.
(dolist (fun '(sb-thread::call-with-mutex
               sb-thread::call-with-recursive-lock
               sb-thread::call-with-system-mutex
               sb-thread::call-with-system-mutex/allow-with-interrupts
               sb-thread::call-with-system-mutex/without-gcing
               sb-thread::call-with-recursive-system-lock
               ;; wish we had a little more commonality to these names
               sb-impl::%with-standard-io-syntax
               sb-impl::%with-rebound-io-syntax
               sb-debug::funcall-with-debug-io-syntax
               sb-impl::call-with-sane-io-syntax
               ;; there are others, maybe %with-compilation-unit
               sb-impl::%print-unreadable-object
               ))
  (let ((info (info :function :info fun)))
    (when info
      (setf (ir1-attributep (fun-info-attributes info) callee-omit-arg-count-check)
            t))))

;;; Convert function designators to functions in calls to known functions
;;; Also convert to TWO-ARG- variants
(defun ir1-optimize-functional-arguments (component)
  (do-blocks (block component)
    (do-nodes (node nil block)
      (when (and (combination-p node)
                 (eq (combination-kind node) :known)
                 ;; REDUCE can call with zero arguments.
                 (neq (lvar-fun-name (combination-fun node) t) 'reduce))
        (map-callable-arguments
         (lambda (lvar args results &key no-function-conversion &allow-other-keys)
           (declare (ignore results))
           ;; Process annotations while the original values are still there.
           (process-annotations lvar)
           (unless no-function-conversion
             (let ((ref (lvar-uses lvar))
                   (arg-count (length args)))
               (labels ((translate-two-args (name)
                          (and (eql arg-count 2)
                               (not (fun-lexically-notinline-p name (node-lexenv node)))
                               (cadr (assoc name *two-arg-functions*))))
                        (translate (ref)
                          (let* ((leaf (ref-leaf ref))
                                 (fun-name (and (constant-p leaf)
                                                (constant-value leaf)))
                                 (replacement
                                   (cond ((and fun-name (symbolp fun-name))
                                          (or (translate-two-args fun-name)
                                              (and (not (memq (info :function :kind fun-name)
                                                              '(:macro :special-form)))
                                                   fun-name)))
                                         ((and (global-var-p leaf)
                                               (eq (global-var-kind leaf) :global-function))
                                          (translate-two-args (global-var-%source-name leaf)))))
                                 (*compiler-error-context* node))
                            (and replacement
                                 (find-global-fun replacement t)))))
                 (cond ((ref-p ref)
                        (let ((replacement (translate ref)))
                          (when replacement
                            (change-ref-leaf ref replacement))))
                       ((cast-p ref)
                        (let* ((cast ref)
                               (ref (lvar-uses (cast-value cast))))
                          (when (ref-p ref)
                            (let ((replacement (translate ref)))
                              (when replacement
                                (change-ref-leaf ref replacement :recklessly t)
                                (setf (node-derived-type cast)
                                      (lvar-derived-type (cast-value cast)))))))))))))
         node)
        ;; One more thing: builtin higher-order functions utilized by builtin macros
        ;; can impart a policy change to the callee, but it can't (easily) be done
        ;; strictly lexically, because the policy would leak downward.
        ;; e.g. (WITH-SOME-STUFF () ..) ->
        ;; -> (DX-FLET ((THUNK () <user-code>)) (CALL-WITH-STUFF #'THUNK))
        ;; should not inject (OPTIMIZE (VERIFY-ARG-COUNT 0)) at the top of <user-code>
        ;; because every lambda therein would omit the arg-count check.
        (when (ir1-attributep (fun-info-attributes (combination-fun-info node))
                              callee-omit-arg-count-check)
          (let* ((args (combination-args node))
                 (arg (if (eq (lvar-fun-name (combination-fun node) t)
                              'sb-impl::%print-unreadable-object)
                          (fourth args)
                          (first args)))
                 (ref (and arg (lvar-uses arg))))
            (when (and (ref-p ref) (lambda-p (ref-leaf ref)))
              ;; It seems impolite (un-debuggable too) to alter the lexenv-policy
              ;; of functional-lexenv, so annotate this differently.
              (setf (getf (functional-plist (ref-leaf ref)) 'verify-arg-count)
                    nil))))))))

(defun rewrite-full-call (combination)
  (let ((combination-name (lvar-fun-name (combination-fun combination) t))
        (args (combination-args combination)))
    (let ((two-arg (assoc combination-name *two-arg-functions*))
          (ref (lvar-uses (combination-fun combination))))
      (when (and two-arg
                 (ref-p ref)
                 (= (length args) 2)
                 (not (fun-lexically-notinline-p combination-name
                                                 (node-lexenv combination))))
        (destructuring-bind (name two-arg &optional types typed-two-arg) two-arg
          (declare (ignore name))
          (when (and types
                     (loop for arg in args
                           for type in types
                           always (csubtypep (lvar-type arg) type)))
            (setf two-arg typed-two-arg))
          (change-ref-leaf
           ref
           (find-free-fun two-arg "rewrite-full-call")))))))

;;; Do miscellaneous things that we want to do once all optimization
;;; has been done:
;;;  -- Record the derived result type before the back-end trashes the
;;;     flow graph.
;;;  -- Note definition of any entry points.
;;;  -- Note any failed optimizations.
(defun ir1-finalize (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (case (functional-kind fun)
      (:external
       (finalize-xep-definition fun))
      ((nil :toplevel)
       (setf (leaf-type fun) (definition-type fun)))))

  (maphash #'note-failed-optimization
           (component-failed-optimizations component))

  (maphash (lambda (k v)
             (note-assumed-types component k v))
           (free-funs *ir1-namespace*))

  (ir1-merge-casts component)
  (ir1-optimize-functional-arguments component)
  (values))
