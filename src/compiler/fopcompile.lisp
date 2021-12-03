;;;; A compiler from simple top-level forms to FASL operations.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

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
                           (fopcompilable-p value)))))

 #-sb-xc-host
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
  (flet ((expand (form)
           (if expand
               (handler-case
                   (%macroexpand form *lexenv*)
                 (error () (return-from fopcompilable-p)))
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
                             (every #'fopcompilable-p args)))))))))))

 ;; Special version of FOPCOMPILABLE-P which recognizes toplevel calls
 ;; that the cold loader is able to perform in the host to create the
 ;; desired effect upon the target core.
 ;; If an effect should occur "sooner than cold-init",
 ;; this is probably where you need to make it happen.
 #+sb-xc-host
 (defun fopcompilable-p (form &optional (expand t))
   (declare (ignore expand))
   (or (and (self-evaluating-p form)
            (constant-fopcompilable-p form))
       ;; Arbitrary computed constants aren't supported because we don't know
       ;; where in FOPCOMPILE's recursion it should stop recursing and just dump
       ;; whatever the constant piece is. For example in (cons `(a ,(+ 1 2)) (f))
       ;; the CAR is built wholly from foldable operators but the CDR is not.
       ;; Constant symbols and QUOTE forms are generally fine to use though.
       (and (symbolp form)
            (eq (info :variable :kind form) :constant))
       (and (typep form '(cons (eql quote) (cons t null)))
            (constant-fopcompilable-p (constant-form-value form)))
       (and (listp form)
            (let ((function (car form)))
              ;; Certain known functions have a special way of checking
              ;; their fopcompilability in the cross-compiler.
              (or (member function '(sb-pcl::!trivial-defmethod))
                  ;; allow DEFCONSTANT only if the value form is ok
                  (and (member function '(sb-impl::%defconstant))
                       (fopcompilable-p (third form))))))))
) ; end FLET

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

;;; Return T if and only if OBJ's nature as an externalizable thing renders
;;; it a leaf for dumping purposes. Symbols are leaflike despite havings slots
;;; containing pointers; similarly (COMPLEX RATIONAL) and RATIO.
(defun dumpable-leaflike-p (obj)
  (or (sb-xc:typep obj '(or symbol number character
                            ;; (ARRAY NIL) is not included in UNBOXED-ARRAY
                            (or unboxed-array (array nil))
                            system-area-pointer
                            #+sb-simd-pack simd-pack
                            #+sb-simd-pack-256 simd-pack-256))
      (cl:typep obj 'debug-name-marker)
      ;; STANDARD-OBJECT layouts use MAKE-LOAD-FORM, but all other layouts
      ;; have the same status as symbols - composite objects but leaflike.
      (and (typep obj 'wrapper) (not (layout-for-pcl-obj-p obj)))
      ;; PACKAGEs are also leaflike.
      (cl:typep obj 'package)
      ;; The cross-compiler wants to dump CTYPE instances as leaves,
      ;; but CLASSOIDs are excluded since they have a MAKE-LOAD-FORM method.
      #+sb-xc-host (cl:typep obj '(and ctype (not classoid)))
      ;; FIXME: The target compiler wants to dump NAMED-TYPE instances,
      ;; or maybe it doesn't, but we're forgetting to OPAQUELY-QUOTE them.
      ;; For the moment I've worked around this with a backward-compatibility
      ;; hack in FIND-CONSTANT which causes anonymous uses of #<named-type t>
      ;; to be dumped as *UNIVERSAL-TYPE*.
      ;; #+sb-xc (named-type-p obj)
      ))

;;; Check that a literal form is fopcompilable. It would not be, for example,
;;; when the form contains structures with funny MAKE-LOAD-FORMS.
;;; In particular, pathnames are not trivially dumpable because the HOST slot
;;; might need to be dumped as a reference to the *PHYSICAL-HOST* symbol.
;;; This function is nowhere near as OAOO-violating as it once was - it no
;;; longer has local knowledge of the set of leaf types, nor how to test for
;;; non-trivial instances. Sharing more code with MAYBE-EMIT-MAKE-LOAD-FORMS
;;; might be a nice goal, but it seems relatively impossible to achieve.
(defun constant-fopcompilable-p (constant)
  (declare (optimize (debug 1))) ;; TCO
  (let ((xset (alloc-xset))
        (dumpable-instances))
    (named-let grovel ((value constant))
      ;; Unless VALUE is an object which which obviously
      ;; can't contain other objects
      (unless (dumpable-leaflike-p value)
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
          (instance
           ;; Almost always, a make-load-form method will call
           ;; MAKE-LOAD-FORM-SAVING-SLOTS for all slots. If so,
           ;; then this structure is amenable to FOPCOMPILE.
           ;; If not, then it isn't, which is not strictly true-
           ;; the method might have returned a fopcompilable
           ;; creation form and no init form. (Handling of
           ;; circularity is best left to the main compiler.)
           (unless (sb-fasl:load-form-is-default-mlfss-p value)
             (return-from constant-fopcompilable-p nil))
           (do-instance-tagged-slot (i value)
             (grovel (%instance-ref value i)))
           (push value dumpable-instances))
          (t
           (return-from constant-fopcompilable-p nil)))))
    (dolist (instance dumpable-instances)
      (fasl-note-dumpable-instance instance *compile-object*))
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
                       (let* ((var (cdr (assoc form (lexenv-vars *lexenv*))))
                              (handle (and (lambda-var-p var)
                                           (leaf-info var))))
                         (cond (handle
                                (setf (lambda-var-ever-used var) t)
                                (when for-value-p
                                  (sb-fasl::dump-push handle fasl)))
                               (t
                                (unless var
                                  ;; Undefined variable. Signal a warning, and
                                  ;; treat it as a special variable reference, like
                                  ;; the real compiler does -- do not elide even if
                                  ;; the value is unused.
                                  (note-undefined-reference form :variable))
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
                      ;; KLUDGE! SB-C:SOURCE-LOCATION calls are normally handled
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
                               (let* ((obj (sb-fasl::dump-pop fasl))
                                      (var (make-lambda-var
                                            :%source-name name
                                            :info obj)))
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
                                     :original-form form
                                     :file-name (file-info-truename file-info)
                                     :initialized t
                                     :file-position
                                     (nth-value 1 (find-source-root tlf *source-info*))
                                     :original-source-path (source-path-original-source path)
                                     :handled-conditions
                                     (lexenv-handled-conditions *lexenv*))))
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
                         ((and (eq operator 'find-undeleted-package-or-lose)
                               (= 1 (length args))
                               for-value-p)
                          (fopcompile (first args) path t)
                          (dump-fop 'sb-fasl::fop-package fasl))
                         (t
                          (when (eq (info :function :where-from operator) :assumed)
                            (note-undefined-reference operator :function))
                          (fopcompile-constant fasl operator t)
                          (let ((n 0))
                            (dolist (arg args)
                              (incf n)
                              (fopcompile arg path t))
                            (if for-value-p
                                (dump-fop 'sb-fasl::fop-funcall fasl n)
                                (dump-fop 'sb-fasl::fop-funcall-for-effect
                                          fasl n)))))))))))
           (t
            (bug "looks unFOPCOMPILEable: ~S" form))))))

(defun fopcompile-function (fasl form path for-value-p)
  (cond ((lambda-form-p form)
          ;; Lambda forms are compiled with the real compiler
         (let ((handle (%compile form fasl :path path)))
           (when for-value-p
             (sb-fasl::dump-push handle fasl))))
          ;; While function names are translated to a call to FDEFINITION.
        ((legal-fun-name-p form)
         (fopcompile `(fdefinition ',form) path for-value-p))
        (t
         (compiler-error "~S is not a legal function name." form))))

(defun fopcompile-if (fasl args path for-value-p)
  (destructuring-bind (condition then &optional else) args
    (let ((else-label (incf *fopcompile-label-counter*))
          (end-label (incf *fopcompile-label-counter*)))
      (fopcompile condition path t)
      ;; If condition was false, skip to the ELSE
      (dump-fop 'sb-fasl::fop-skip-if-false fasl else-label)
      (fopcompile then path for-value-p)
      ;; The THEN branch will have produced a value even if we were
      ;; currently skipping to the ELSE branch (or over this whole
      ;; IF). This is done to ensure that the stack effects are
      ;; balanced properly when dealing with operations that are
      ;; executed even when skipping over code. But this particular
      ;; value will be bogus, so we drop it.
      (when for-value-p
        (dump-fop 'sb-fasl::fop-drop-if-skipping fasl))
      ;; Now skip to the END
      (dump-fop 'sb-fasl::fop-skip fasl end-label)
      ;; Start of the ELSE branch
      (dump-fop 'sb-fasl::fop-maybe-stop-skipping fasl else-label)
      (fopcompile else path for-value-p)
      ;; As before
      (when for-value-p
        (dump-fop 'sb-fasl::fop-drop-if-skipping fasl))
      ;; End of IF
      (dump-fop 'sb-fasl::fop-maybe-stop-skipping fasl end-label)
      ;; If we're still skipping, we must've triggered both of the
      ;; drop-if-skipping fops. To keep the stack balanced, push a
      ;; dummy value if needed.
      (when for-value-p
        (dump-fop 'sb-fasl::fop-push-nil-if-skipping fasl)))))

(defun fopcompile-constant (fasl form for-value-p)
  (when for-value-p
    (dump-object form fasl)))
