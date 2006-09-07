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
(defun fopcompilable-p (form)
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
  #+sb-xc-host
  nil
  #-sb-xc-host
  (or (and (self-evaluating-p form)
           (constant-fopcompilable-p form))
      (and (symbolp form)
           (multiple-value-bind (macroexpansion macroexpanded-p)
               (macroexpand form)
             (if macroexpanded-p
                 (fopcompilable-p macroexpansion)
                 ;; Punt on :ALIEN variables
                 (let ((kind (info :variable :kind form)))
                   (or (eq kind :special)
                       (eq kind :constant))))))
      (and (listp form)
           (ignore-errors (list-length form))
           (multiple-value-bind (macroexpansion macroexpanded-p)
               (macroexpand form)
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
                           (or (lambda-form-p (car args))
                               ;; #'FOO, #'(SETF FOO), etc
                               (legal-fun-name-p (car args)))))
                     ((if)
                      (and (<= 2 (length args) 3)
                           (every #'fopcompilable-p args)))
                     ;; Allow SETQ only on special variables
                     ((setq)
                      (loop for (name value) on args by #'cddr
                            unless (and (symbolp name)
                                        (let ((kind (info :variable :kind name)))
                                          (eq kind :special))
                                        (fopcompilable-p value))
                            return nil
                            finally (return t)))
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
                     ;; A LET or LET* that introduces no bindings or
                     ;; declarations is trivially fopcompilable. Forms
                     ;; with no bindings but with declarations could also
                     ;; be handled, but we're currently punting on any
                     ;; lexenv manipulation.
                     ((let let*)
                      (and (>= (length args) 1)
                           (null (car args))
                           (every #'fopcompilable-p (cdr args))))
                     ;; Likewise for LOCALLY
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
                           (not (macro-function operator))
                           ;; We can't FOP-FUNCALL with more than 255
                           ;; parameters. (We could theoretically use
                           ;; APPLY, but then we'd need to construct
                           ;; the parameter list for APPLY without
                           ;; calling LIST, which is probably more
                           ;; trouble than it's worth).
                           (<= (length args) 255)
                           (every #'fopcompilable-p args))))))))))

(defun lambda-form-p (form)
  (and (consp form)
       (member (car form)
               '(lambda named-lambda instance-lambda lambda-with-lexenv))))

;;; Check that a literal form is fopcompilable. It would not for example
;;; when the form contains structures with funny MAKE-LOAD-FORMS.
(defun constant-fopcompilable-p (constant)
  (let ((things-processed nil)
        (count 0))
    (declare (type (or list hash-table) things-processed)
             (type (integer 0 #.(1+ list-to-hash-table-threshold)) count)
             (inline member))
    (labels ((grovel (value)
               ;; Unless VALUE is an object which which obviously
               ;; can't contain other objects
               (unless (typep value
                              '(or unboxed-array
                                symbol
                                number
                                character
                                string))
                 (etypecase things-processed
                   (list
                    (when (member value things-processed :test #'eq)
                      (return-from grovel nil))
                    (push value things-processed)
                    (incf count)
                    (when (> count list-to-hash-table-threshold)
                      (let ((things things-processed))
                        (setf things-processed
                              (make-hash-table :test 'eq))
                        (dolist (thing things)
                          (setf (gethash thing things-processed) t)))))
                   (hash-table
                    (when (gethash value things-processed)
                      (return-from grovel nil))
                    (setf (gethash value things-processed) t)))
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
                    (multiple-value-bind (creation-form init-form)
                        (handler-case
                            (sb!xc:make-load-form value (make-null-lexenv))
                          (error (condition)
                            (compiler-error condition)))
                      (declare (ignore init-form))
                      (case creation-form
                        (:sb-just-dump-it-normally
                         (fasl-validate-structure constant *compile-object*)
                         (dotimes (i (- (%instance-length value)
                                        (layout-n-untagged-slots
                                         (%instance-ref value 0))))
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
(defun fopcompile (form path for-value-p)
  (cond ((self-evaluating-p form)
         (fopcompile-constant form for-value-p))
        ((symbolp form)
         (multiple-value-bind (macroexpansion macroexpanded-p)
             (macroexpand form)
           (if macroexpanded-p
               ;; Symbol macro
               (fopcompile macroexpansion path for-value-p)
               ;; Special variable
               (fopcompile `(symbol-value ',form) path for-value-p))))
        ((listp form)
         (multiple-value-bind (macroexpansion macroexpanded-p)
             (macroexpand form)
           (if macroexpanded-p
               (fopcompile macroexpansion path for-value-p)
               (destructuring-bind (operator &rest args) form
                 (case operator
                   ;; The QUOTE special operator is worth handling: very
                   ;; easy and very common at toplevel.
                   ((quote)
                    (fopcompile-constant (second form) for-value-p))
                   ;; A FUNCTION needs to be compiled properly, but doesn't
                   ;; need to prevent the fopcompilation of the whole form.
                   ;; We just compile it, and emit an instruction for pushing
                   ;; the function handle on the FOP stack.
                   ((function)
                    (fopcompile-function (second form) path for-value-p))
                   ;; KLUDGE! SB!C:SOURCE-LOCATION calls are normally handled
                   ;; by a compiler-macro. Doing general compiler-macro
                   ;; expansion in the fopcompiler is probably not sensible,
                   ;; so we'll just special-case it.
                   ((source-location)
                    (if (policy *policy* (and (> space 1)
                                              (> space debug)))
                        (fopcompile-constant nil for-value-p)
                        (fopcompile (let ((*current-path* path))
                                      (make-definition-source-location))
                                    path
                                    for-value-p)))
                   ((if)
                    (fopcompile-if args path for-value-p))
                   ((progn)
                     (loop for (arg . next) on args
                           do (fopcompile arg
                                          path (if next
                                                   nil
                                                   for-value-p))))
                   ((setq)
                    (loop for (name value . next) on args by #'cddr
                          do (fopcompile `(set ',name ,value) path
                                         (if next
                                             nil
                                             for-value-p))))
                   ((eval-when)
                    (destructuring-bind (situations &body body) args
                      (if (or (member :execute situations)
                              (member 'eval situations))
                          (fopcompile (cons 'progn body) path for-value-p)
                          (fopcompile nil path for-value-p))))
                   ((let let*)
                     (fopcompile (cons 'progn (cdr args)) path for-value-p))
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
                       (sb!fasl::dump-fop 'sb!fasl::fop-package
                                          *compile-object*))
                      (t
                       (fopcompile-constant operator t)
                       (dolist (arg args)
                         (fopcompile arg path t))
                       (if for-value-p
                           (sb!fasl::dump-fop 'sb!fasl::fop-funcall
                                              *compile-object*)
                           (sb!fasl::dump-fop 'sb!fasl::fop-funcall-for-effect
                                              *compile-object*))
                       (let ((n-args (length args)))
                         ;; stub: FOP-FUNCALL isn't going to be usable
                         ;; to compile more than this, since its count
                         ;; is a single byte. Maybe we should just punt
                         ;; to the ordinary compiler in that case?
                         (aver (<= n-args 255))
                         (sb!fasl::dump-byte n-args *compile-object*))))))))))
        (t
         (bug "looks unFOPCOMPILEable: ~S" form))))

(defun fopcompile-function (form path for-value-p)
  (flet ((dump-fdefinition (name)
           (fopcompile `(fdefinition ',name) path for-value-p)))
    (if (consp form)
        (cond
          ;; Lambda forms are compiled with the real compiler
          ((lambda-form-p form)
           ;; We wrap the real lambda inside another one to ensure
           ;; that the compiler doesn't e.g. let convert it, thinking
           ;; that there are no external references.
           (let* ((handle (%compile `(lambda () ,form)
                                    *compile-object*
                                    :path path)))
             (when for-value-p
               (sb!fasl::dump-push handle *compile-object*)
               ;; And then call the wrapper function when loading the FASL
               (sb!fasl::dump-fop 'sb!fasl::fop-funcall *compile-object*)
               (sb!fasl::dump-byte 0 *compile-object*))))
          ;; While function names are translated to a call to FDEFINITION.
          ((legal-fun-name-p form)
           (dump-fdefinition form))
          (t
           (compiler-error "~S is not a legal function name." form)))
        (dump-fdefinition form))))

(defun fopcompile-if (args path for-value-p)
  (destructuring-bind (condition then &optional else)
      args
    (let ((else-label (incf *fopcompile-label-counter*))
          (end-label (incf *fopcompile-label-counter*)))
      (sb!fasl::dump-integer else-label *compile-object*)
      (fopcompile condition path t)
      ;; If condition was false, skip to the ELSE
      (sb!fasl::dump-fop 'sb!fasl::fop-skip-if-false *compile-object*)
      (fopcompile then path for-value-p)
      ;; The THEN branch will have produced a value even if we were
      ;; currently skipping to the ELSE branch (or over this whole
      ;; IF). This is done to ensure that the stack effects are
      ;; balanced properly when dealing with operations that are
      ;; executed even when skipping over code. But this particular
      ;; value will be bogus, so we drop it.
      (when for-value-p
        (sb!fasl::dump-fop 'sb!fasl::fop-drop-if-skipping *compile-object*))
      ;; Now skip to the END
      (sb!fasl::dump-integer end-label *compile-object*)
      (sb!fasl::dump-fop 'sb!fasl::fop-skip *compile-object*)
      ;; Start of the ELSE branch
      (sb!fasl::dump-integer else-label *compile-object*)
      (sb!fasl::dump-fop 'sb!fasl::fop-maybe-stop-skipping *compile-object*)
      (fopcompile else path for-value-p)
      ;; As before
      (when for-value-p
        (sb!fasl::dump-fop 'sb!fasl::fop-drop-if-skipping *compile-object*))
      ;; End of IF
      (sb!fasl::dump-integer end-label *compile-object*)
      (sb!fasl::dump-fop 'sb!fasl::fop-maybe-stop-skipping *compile-object*)
      ;; If we're still skipping, we must've triggered both of the
      ;; drop-if-skipping fops. To keep the stack balanced, push a
      ;; dummy value if needed.
      (when for-value-p
        (sb!fasl::dump-fop 'sb!fasl::fop-push-nil-if-skipping
                           *compile-object*)))))

(defun fopcompile-constant (form for-value-p)
  (when for-value-p
    (let ((sb!fasl::*dump-only-valid-structures* nil))
      (dump-object form *compile-object*))))
