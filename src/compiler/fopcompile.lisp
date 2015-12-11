;;;; A compiler from simple top-level forms to FASL operations.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; SBCL has no proper byte compiler (having ditched the rather
;;; ambitious and slightly flaky byte compiler inherited from CMU CL)
;;; but its FOPs are a sort of byte code which is expressive enough
;;; that we can compile some simple toplevel forms directly to them,
;;; including very common operations like the forms that DEFVARs and
;;; DECLAIMs macroexpand into.
;;;
;;; FIXME: The expexnasion problem.
;;; FOPCOMPILE and FOPCOMPILABLE-P cause multiple expansion of macros,
;;; which may be problematic with side-effecting macros. When
;;; FOPCOMPILABLE-P succeeds, FOPCOMPILE is called, resulting in
;;; double macroexpansion. When FOPCOMPILABLE-P fails,
;;; IR1-CONVERT-FUNCTOID expands already expanded macros for a second
;;; time.
;;; And an edge case, when the top-level call has a complier-macro
;;; which returns &whole it gets expanded three times, two times by
;;; FOPCOMPILABLE-P and FOPCOMPILE, and one time by
;;; PROCESS-TOPLEVEL-FORM, because unlike other macros, the expanded
;;; form is still a macro-form. That's what the EXPAND optional
;;; parameter solves, PROCESS-TOPLEVEL-FORM passes NIL, expanding
;;; compiler macros at most once.
;;; The instances of double expansion still remain, e.g. (fun (macro)),
;;; since PROCESS-TOPLEVEL-FORM only expands the macros at the first
;;; position.

(flet ((setq-fopcompilable-p (args)
         (loop for (name value) on args by #'cddr
               always (and (symbolp name)
                           (member (info :variable :kind name)
                                   '(:special :global))
                           (fopcompilable-p value))))
       (cold-svset-fopcompilable-p (args)
         (destructuring-bind (thing index value) args
           (and (symbolp thing)
                (integerp index)
                (eq (info :variable :kind thing) :global)
                (typep value '(cons (eql function) (cons symbol null)))))))
 (defun fopcompilable-p (form &optional (expand t))
  ;; We'd like to be able to handle
  ;;   -- simple funcalls, nested recursively, e.g.
  ;;      (SET '*PACKAGE* (FIND-PACKAGE "CL-USER"))
  ;;   -- common self-evaluating forms like strings and keywords and
  ;;      fixnums, which are important for terminating
  ;;      the recursion of the simple funcalls above
  ;;   -- quoted lists (which are important for PROCLAIMs, which are
  ;;      common toplevel forms)
  ;;   -- fopcompilable stuff wrapped around non-fopcompilable expressions,
  ;;      e.g.
  ;;        (%DEFUN 'FOO (LAMBDA () ...) ...)
  ;;   -- the IF special form, to support things like (DEFVAR *X* 0)
  ;;      expanding into (UNLESS (BOUNDP '*X*) (SET '*X* 0))
  ;;
  ;; Special forms which we don't currently handle, but might consider
  ;; supporting in the future are LOCALLY (with declarations),
  ;; MACROLET, SYMBOL-MACROLET and THE.
  ;; Also, if (FLET ((F () ...)) (DEFUN A () ...) (DEFUN B () ...))
  ;; were handled, then it would probably automatically work in
  ;; the cold loader too, providing definitions for A and B before
  ;; executing all other toplevel forms.
  #+sb-xc-host
  (and expand
       (or (and (self-evaluating-p form)
                (constant-fopcompilable-p form))
           (and (listp form)
                (let ((function (car form)))
                  ;; It is assumed that uses of recognized functions are
                  ;; carefully controlled, and recursion on fopcompilable-p
                  ;; would say "yes".
                  (or (member function '(sb!impl::%defun
                                         sb!impl::%defsetf
                                         sb!kernel::%defstruct))
                      (and (symbolp function) ; no ((lambda ...) ...)
                           (get-properties (symbol-plist function)
                                           '(:sb-cold-funcall-handler/for-effect
                                             :sb-cold-funcall-handler/for-value)))
                      (and (eq function 'setf)
                           (fopcompilable-p (%macroexpand form *lexenv*)))
                      (and (eq function 'sb!kernel:%svset)
                           (cold-svset-fopcompilable-p (cdr form)))
                      (and (eq function 'setq)
                           (setq-fopcompilable-p (cdr form))))))))
  #-sb-xc-host
  (flet ((expand (form)
           (if expand
               (%macroexpand form *lexenv*)
               (values form nil)))
         (expand-cm (form)
           (if expand
               (expand-compiler-macro form)
               (values form nil))))
    (or (and (self-evaluating-p form)
             (constant-fopcompilable-p form))
        (and (symbolp form)
             (multiple-value-bind (macroexpansion macroexpanded-p)
                 (expand form)
               (if macroexpanded-p
                   (fopcompilable-p macroexpansion)
                   ;; Punt on :ALIEN variables
                   (let ((kind (info :variable :kind form)))
                     (member kind '(:special :constant :global :unknown))))))
        (and (listp form)
             (ignore-errors (list-length form))
             (let ((macroexpansion (expand-cm form)))
               (if (neq macroexpansion form)
                   (return-from fopcompilable-p (fopcompilable-p macroexpansion))
                   t))
             (multiple-value-bind (macroexpansion macroexpanded-p)
                 (expand form)
               (if macroexpanded-p
                   (fopcompilable-p macroexpansion)
                   (destructuring-bind (operator &rest args) form
                     (case operator
                       ;; Special operators that we know how to cope with
                       ((progn)
                        (every #'fopcompilable-p args))
                       ((quote)
                        (and (= (length args) 1)
                             (constant-fopcompilable-p (car args))))
                       ((function)
                        (and (= (length args) 1)
                             ;; #'(LAMBDA ...), #'(NAMED-LAMBDA ...), etc. These
                             ;; are not fopcompileable as such, but we can compile
                             ;; the lambdas with the real compiler, and the rest
                             ;; of the expression with the fop-compiler.
                             (or (and (lambda-form-p (car args))
                                      ;; The lambda might be closing over some
                                      ;; variable, punt. As a further improvement,
                                      ;; we could analyze the lambda body to
                                      ;; see whether it really closes over any
                                      ;; variables. One place where even simple
                                      ;; analysis would be useful are the PCL
                                      ;; slot-definition type-check-functions
                                      ;;   -- JES, 2007-01-13
                                      (notany (lambda (binding)
                                                (lambda-var-p (cdr binding)))
                                              (lexenv-vars *lexenv*)))
                                 ;; #'FOO, #'(SETF FOO), etc
                                 (legal-fun-name-p (car args)))))
                       ((if)
                        (and (<= 2 (length args) 3)
                             (every #'fopcompilable-p args)))
                       ;; Allow SETQ only on special or global variables
                       ((setq)
                        (setq-fopcompilable-p args))
                       ;; The real toplevel form processing has already been
                       ;; done, so EVAL-WHEN handling will be easy.
                       ((eval-when)
                        (and (>= (length args) 1)
                             (eq (set-difference (car args)
                                                 '(:compile-toplevel
                                                   compile
                                                   :load-toplevel
                                                   load
                                                   :execute
                                                   eval))
                                 nil)
                             (every #'fopcompilable-p (cdr args))))
                       ;; A LET or LET* that introduces only lexical
                       ;; bindings might be fopcompilable, depending on
                       ;; whether something closes over the bindings.
                       ;; (And whether there are declarations in the body,
                       ;; see below)
                       ((let let*)
                        (let-fopcompilable-p operator args))
                       ((locally)
                        (every #'fopcompilable-p args))
                       (otherwise
                        ;; ordinary function calls
                        (and (symbolp operator)
                             ;; If a LET/LOCALLY tries to introduce
                             ;; declarations, we'll detect it here, and
                             ;; disallow fopcompilation.  This is safe,
                             ;; since defining a function/macro named
                             ;; DECLARE would violate a package lock.
                             (not (eq operator 'declare))
                             (not (special-operator-p operator))
                             (not (macro-function operator)) ; redundant check
                             ;; We can't FOP-FUNCALL with more than 255
                             ;; parameters. (We could theoretically use
                             ;; APPLY, but then we'd need to construct
                             ;; the parameter list for APPLY without
                             ;; calling LIST, which is probably more
                             ;; trouble than it's worth).
                             (<= (length args) 255)
                             (every #'fopcompilable-p args))))))))))))

(defun let-fopcompilable-p (operator args)
  (when (>= (length args) 1)
    (multiple-value-bind (body decls) (parse-body (cdr args) nil)
      (declare (ignore body))
      (let* ((orig-lexenv *lexenv*)
             (*lexenv* (make-lexenv)))
        ;; We need to check for declarations
        ;; first. Otherwise the fake lexenv we're
        ;; constructing might be invalid.
        (and (null decls)
             (loop for binding in (car args)
                   for name = (if (consp binding)
                                  (first binding)
                                  binding)
                   for value = (if (consp binding)
                                   (second binding)
                                   nil)
                   ;; Only allow binding locals, since special bindings can't
                   ;; be easily expressed with fops.
                   always (and (eq (info :variable :kind name)
                                   :unknown)
                               (let ((*lexenv* (ecase operator
                                                 (let orig-lexenv)
                                                 (let* *lexenv*))))
                                 (fopcompilable-p value)))
                   do (progn
                        (setf *lexenv* (make-lexenv))
                        (push (cons name
                                    (make-lambda-var :%source-name name))
                              (lexenv-vars *lexenv*))))
             (every #'fopcompilable-p (cdr args)))))))

(defun lambda-form-p (form)
  (and (consp form)
       (member (car form)
               '(lambda named-lambda lambda-with-lexenv))))

;;; Check that a literal form is fopcompilable. It would not be, for example,
;;; when the form contains structures with funny MAKE-LOAD-FORMS.
(defun constant-fopcompilable-p (constant)
  (let ((xset (alloc-xset)))
    (labels ((grovel (value)
               ;; Unless VALUE is an object which which obviously
               ;; can't contain other objects
               ;; FIXME: OAOOM. See MAYBE-EMIT-MAKE-LOAD-FORMS.
               (unless (typep value
                              '(or #-sb-xc-host unboxed-array
                                symbol
                                number
                                character
                                string))
                 (if (xset-member-p value xset)
                     (return-from grovel nil)
                     (add-to-xset value xset))
                 (typecase value
                   (cons
                    (grovel (car value))
                    (grovel (cdr value)))
                   (simple-vector
                    (dotimes (i (length value))
                      (grovel (svref value i))))
                   ((vector t)
                    (dotimes (i (length value))
                      (grovel (aref value i))))
                   ((simple-array t)
                    ;; Even though the (ARRAY T) branch does the exact
                    ;; same thing as this branch we do this separately
                    ;; so that the compiler can use faster versions of
                    ;; array-total-size and row-major-aref.
                    (dotimes (i (array-total-size value))
                      (grovel (row-major-aref value i))))
                   ((array t)
                    (dotimes (i (array-total-size value))
                      (grovel (row-major-aref value i))))
                   ;; This is the same kludge as appears in EMIT-MAKE-LOAD-FORM
                   ;; which informs the xc that LAYOUTs are leaf-like nodes.
                   ;; This case was never reached before because cross-compiling
                   ;; used to generate target machine code for everything.
                   #+sb-xc-host (layout)
                   (instance
                    (multiple-value-bind (creation-form init-form)
                        (handler-case
                            (sb!xc:make-load-form value (make-null-lexenv))
                          (error (condition)
                            (compiler-error condition)))
                      (declare (ignore init-form))
                      (case creation-form
                        (:sb-just-dump-it-normally
                         ;; FIXME: Why is this needed? If the constant
                         ;; is deemed fopcompilable, then when we dump
                         ;; it we bind *dump-only-valid-structures* to
                         ;; NIL.
                         (fasl-validate-structure value *compile-object*)
                         ;; The above FIXME notwithstanding,
                         ;; there's never a need to grovel a layout.
                         (do-instance-tagged-slot (i value)
                           (grovel (%instance-ref value i))))
                        (:ignore-it)
                        (t
                         (return-from constant-fopcompilable-p nil)))))
                   (t
                    (return-from constant-fopcompilable-p nil))))))
      (grovel constant))
    t))

;;; FOR-VALUE-P is true if the value will be used (i.e., pushed onto
;;; FOP stack), or NIL if any value will be discarded. FOPCOMPILABLE-P
;;; has already ensured that the form can be fopcompiled.
;;;
;;; See the expansion problem FIXME above fopcompilable-p.
(defun fopcompile (form path for-value-p &optional (expand t))
  (let ((path (or (get-source-path form) (cons form path)))
        (fasl *compile-object*))
   (flet ((expand (form)
            (if expand
                (%macroexpand form *lexenv*)
                (values form nil)))
          (expand-cm (form)
            (if expand
                (expand-compiler-macro form)
                (values form nil))))
     (cond ((self-evaluating-p form)
            (fopcompile-constant fasl form for-value-p))
           ((symbolp form)
            (multiple-value-bind (macroexpansion macroexpanded-p)
                (expand form)
              (if macroexpanded-p
                  ;; Symbol macro
                  (fopcompile macroexpansion path for-value-p)
                  (let ((kind (info :variable :kind form)))
                    (cond
                      ((eq :special kind)
                       ;; Special variable
                       (fopcompile `(symbol-value ',form) path for-value-p))

                      ((member kind '(:global :constant))
                       ;; Global variable or constant.
                       (fopcompile `(symbol-global-value ',form) path for-value-p))
                      (t
                       ;; Lexical
                       (let* ((lambda-var (cdr (assoc form (lexenv-vars *lexenv*))))
                              (handle (when lambda-var
                                        (lambda-var-fop-value lambda-var))))
                         (cond (handle
                                (setf (lambda-var-ever-used lambda-var) t)
                                (when for-value-p
                                  (sb!fasl::dump-push handle fasl)))
                               (t
                                ;; Undefined variable. Signal a warning, and
                                ;; treat it as a special variable reference, like
                                ;; the real compiler does -- do not elide even if
                                ;; the value is unused.
                                (note-undefined-reference form :variable)
                                (fopcompile `(symbol-value ',form)
                                            path
                                            for-value-p))))))))))
           ((listp form)
            (let ((macroexpansion (expand-cm form)))
              (if (neq macroexpansion form)
                  ;; could expand into an atom, so start from the top
                  (return-from fopcompile
                    (fopcompile macroexpansion path for-value-p))))
            (multiple-value-bind (macroexpansion macroexpanded-p)
                (expand form)
              (if macroexpanded-p
                  (fopcompile macroexpansion path for-value-p)
                  (destructuring-bind (operator &rest args) form
                    (case operator
                      ;; The QUOTE special operator is worth handling: very
                      ;; easy and very common at toplevel.
                      ((quote)
                       (fopcompile-constant fasl (second form) for-value-p))
                      ;; A FUNCTION needs to be compiled properly, but doesn't
                      ;; need to prevent the fopcompilation of the whole form.
                      ;; We just compile it, and emit an instruction for pushing
                      ;; the function handle on the FOP stack.
                      ((function)
                       (fopcompile-function fasl (second form) path for-value-p))
                      ;; KLUDGE! SB!C:SOURCE-LOCATION calls are normally handled
                      ;; by a compiler-macro. But if SPACE > DEBUG we choose not
                      ;; to record locations, which is strange because the main
                      ;; compiler does not have similar logic afaict.
                      ((source-location)
                       ;; FIXME: since the fopcompiler expands compiler-macros,
                       ;; this case should probably be killed. It can't execute.
                       (if (policy *policy* (and (> space 1)
                                                 (> space debug)))
                           (fopcompile-constant fasl nil for-value-p)
                           (fopcompile (let ((*current-path* path))
                                         (make-definition-source-location))
                                       path
                                       for-value-p)))
                      ((if)
                       (fopcompile-if fasl args path for-value-p))
                      ((progn locally)
                       (if (and for-value-p (endp args))
                           (fopcompile nil path t)
                           (loop for (arg . next) on args
                             do (fopcompile arg path
                                            (if next nil for-value-p)))))
                      ((setq)
                       (if (and for-value-p (endp args))
                           (fopcompile nil path t)
                           (loop for (name value . next) on args by #'cddr
                             do (fopcompile `(set ',name ,value) path
                                            (if next nil for-value-p)))))
                      ((eval-when)
                       (destructuring-bind (situations &body body) args
                         (if (or (member :execute situations)
                                 (member 'eval situations))
                             (fopcompile (cons 'progn body) path for-value-p)
                             (fopcompile nil path for-value-p))))
                      ((let let*)
                       (let ((orig-lexenv *lexenv*)
                             (*lexenv* (make-lexenv :default *lexenv*))
                             vars)
                         (loop for binding in (car args)
                               for name = (if (consp binding)
                                              (first binding)
                                              binding)
                               for value = (if (consp binding)
                                               (second binding)
                                               nil)
                               do
                               (let ((*lexenv* (if (eql operator 'let)
                                                   orig-lexenv
                                                   *lexenv*)))
                                 (fopcompile value path t))
                               (let* ((obj (sb!fasl::dump-pop fasl))
                                      (var (make-lambda-var
                                            :%source-name name
                                            :fop-value obj)))
                                 (push var vars)
                                 (setf *lexenv*
                                       (make-lexenv
                                        :vars (list (cons name var))))))
                         (fopcompile (cons 'progn (cdr args)) path for-value-p)
                         (when (and vars
                                    (and *source-info* path))
                           (let* ((tlf (source-path-tlf-number path))
                                  (file-info (source-info-file-info *source-info*))
                                  (*compiler-error-context*
                                    (make-compiler-error-context
                                     :original-source (stringify-form form)
                                     :file-name (file-info-name file-info)
                                     :file-position
                                     (nth-value 1 (find-source-root tlf *source-info*))
                                     :original-source-path (source-path-original-source path)
                                     :lexenv *lexenv*)))
                             (note-unreferenced-vars vars *policy*)))))
                      ;; Otherwise it must be an ordinary funcall.
                      (otherwise
                       (cond
                         ;; Special hack: there's already a fop for
                         ;; find-undeleted-package-or-lose, so use it.
                         ;; (We could theoretically do the same for
                         ;; other operations, but I don't see any good
                         ;; candidates in a quick read-through of
                         ;; src/code/fop.lisp.)
                         ((and (eq operator
                                   'sb!int:find-undeleted-package-or-lose)
                               (= 1 (length args))
                               for-value-p)
                          (fopcompile (first args) path t)
                          (sb!fasl::dump-fop 'sb!fasl::fop-package fasl))
                         (t
                          (when (eq (info :function :where-from operator) :assumed)
                            (note-undefined-reference operator :function))
                          (fopcompile-constant fasl operator t)
                          (dolist (arg args)
                            (fopcompile arg path t))
                          (if for-value-p
                              (sb!fasl::dump-fop 'sb!fasl::fop-funcall fasl)
                              (sb!fasl::dump-fop 'sb!fasl::fop-funcall-for-effect
                                                 fasl))
                          (let ((n-args (length args)))
                            ;; stub: FOP-FUNCALL isn't going to be usable
                            ;; to compile more than this, since its count
                            ;; is a single byte. Maybe we should just punt
                            ;; to the ordinary compiler in that case?
                            (aver (<= n-args 255))
                            (sb!fasl::dump-byte n-args fasl))))))))))
           (t
            (bug "looks unFOPCOMPILEable: ~S" form))))))

(defun fopcompile-function (fasl form path for-value-p)
  (cond ((lambda-form-p form)
          ;; Lambda forms are compiled with the real compiler
         (let ((handle (%compile form fasl :path path)))
           (when for-value-p
             (sb!fasl::dump-push handle fasl))))
          ;; While function names are translated to a call to FDEFINITION.
        ((legal-fun-name-p form)
         (fopcompile `(fdefinition ',form) path for-value-p))
        (t
         (compiler-error "~S is not a legal function name." form))))

(defun fopcompile-if (fasl args path for-value-p)
  (destructuring-bind (condition then &optional else)
      args
    (let ((else-label (incf *fopcompile-label-counter*))
          (end-label (incf *fopcompile-label-counter*)))
      (sb!fasl::dump-integer else-label fasl)
      (fopcompile condition path t)
      ;; If condition was false, skip to the ELSE
      (sb!fasl::dump-fop 'sb!fasl::fop-skip-if-false fasl)
      (fopcompile then path for-value-p)
      ;; The THEN branch will have produced a value even if we were
      ;; currently skipping to the ELSE branch (or over this whole
      ;; IF). This is done to ensure that the stack effects are
      ;; balanced properly when dealing with operations that are
      ;; executed even when skipping over code. But this particular
      ;; value will be bogus, so we drop it.
      (when for-value-p
        (sb!fasl::dump-fop 'sb!fasl::fop-drop-if-skipping fasl))
      ;; Now skip to the END
      (sb!fasl::dump-integer end-label fasl)
      (sb!fasl::dump-fop 'sb!fasl::fop-skip fasl)
      ;; Start of the ELSE branch
      (sb!fasl::dump-integer else-label fasl)
      (sb!fasl::dump-fop 'sb!fasl::fop-maybe-stop-skipping fasl)
      (fopcompile else path for-value-p)
      ;; As before
      (when for-value-p
        (sb!fasl::dump-fop 'sb!fasl::fop-drop-if-skipping fasl))
      ;; End of IF
      (sb!fasl::dump-integer end-label fasl)
      (sb!fasl::dump-fop 'sb!fasl::fop-maybe-stop-skipping fasl)
      ;; If we're still skipping, we must've triggered both of the
      ;; drop-if-skipping fops. To keep the stack balanced, push a
      ;; dummy value if needed.
      (when for-value-p
        (sb!fasl::dump-fop 'sb!fasl::fop-push-nil-if-skipping fasl)))))

(defun fopcompile-constant (fasl form for-value-p)
  (when for-value-p
    ;; FIXME: Without this binding the dumper chokes on unvalidated
    ;; structures: CONSTANT-FOPCOMPILABLE-P validates the structure
    ;; about to be dumped, not its load-form. Compare and contrast
    ;; with EMIT-MAKE-LOAD-FORM.
    (let ((sb!fasl::*dump-only-valid-structures* nil))
      (dump-object form fasl))))

;; Return CLASS if CREATION-FORM is `(allocate-instance (find-class ',CLASS))
(defun canonical-instance-maker-form-p (creation-form)
  (let ((arg (and (typep creation-form
                         '(cons (eql allocate-instance) (cons t null)))
                  (cadr creation-form))))
    (when (and arg (typep arg '(cons (eql find-class) (cons t null))))
      (let ((class (cadr arg)))
        (when (typep class '(cons (eql quote) (cons symbol null)))
          (cadr class))))))

;; If FORM can be implemented by FOP-ALLOCATE-INSTANCE,
;; then fopcompile it and return a table index, otherwise return NIL.
(defun fopcompile-allocate-instance (fasl form)
  (let ((class-name (canonical-instance-maker-form-p form)))
    (when class-name
        (dump-object class-name fasl)
        (sb!fasl::dump-fop 'sb!fasl::fop-allocate-instance fasl)
        (let ((index (sb!fasl::fasl-output-table-free fasl)))
          (setf (sb!fasl::fasl-output-table-free fasl) (1+ index))
          index))))

;; If FORM is one that we recognize as coming from MAKE-LOAD-FORM-SAVING-SLOTS,
;; then return 3 values: the instance being affected, a slot name, and a value.
;; Otherwise return three NILs.
(defun trivial-load-form-initform-args (form)
  (multiple-value-bind (args const)
      ;; these expressions suck, but here goes...
      (cond ((typep form
                    '(cons
                      (eql setf)
                      (cons (cons (eql slot-value)
                                  (cons instance
                                        (cons (cons (eql quote) (cons symbol null))
                                              null)))
                            (cons (cons (eql quote) (cons t null)) null))))
             (values (cdadr form) (second (third form))))
            ((typep form
                    '(cons
                      (eql slot-makunbound)
                      (cons instance
                            (cons (cons (eql quote) (cons symbol null)) null))))
             (values (cdr form) sb!pcl:+slot-unbound+)))
    (if args
        (values (car args) (cadadr args) const)
        (values nil nil nil))))

;; If FORMS contains exactly one PROGN with an expected shape,
;; then dump it using fops and return T. Otherwise return NIL.
(defun fopcompile-constant-init-forms (fasl forms)
  ;; It should be possible to extend this to allow FORMS to have
  ;; any number of forms in the requisite shape.
  (when (and (singleton-p forms)
             (typep (car forms)
                    '(cons (eql progn) (satisfies list-length))))
    (let ((forms (cdar forms))
          (instance)
          (slot-names)
          (values))
      (dolist (form forms
               (progn
                 (mapc (lambda (x) (dump-object x fasl)) (nreverse values))
                 (dump-object (cons (length slot-names) (nreverse slot-names))
                              fasl)
                 (dump-object instance fasl)
                 (sb!fasl::dump-fop 'sb!fasl::fop-initialize-instance fasl)
                 t))
        (multiple-value-bind (obj slot val)
            (trivial-load-form-initform-args form)
          (unless (if instance
                      (eq obj instance)
                      (typep (setq instance obj) 'instance))
            (return nil))
          ;; invoke recursive MAKE-LOAD-FORM stuff as necessary
          (find-constant val)
          (push slot slot-names)
          (push val values))))))
