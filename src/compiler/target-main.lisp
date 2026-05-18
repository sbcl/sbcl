;;;; functions from classic CMU CL src/compiler/main.lisp which are
;;;; needed only (and which may make sense only) on the
;;;; cross-compilation target, not the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; CL:COMPILE

;;; Find the function that is being compiled by COMPILE and bash its
;;; name to NAME. We also substitute for any references to name so
;;; that recursive calls will be compiled direct. LAMBDA is the
;;; top-level lambda for the compilation. The real function is the
;;; only thing in the top-level lambda that is returned, so it isn't
;;; too hard to find.
(defun compile-fix-function-name (lambda name)
  (declare (type clambda lambda) (type (or symbol cons) name))
  (when name
    ;; FIXME: Shouldn't we check here that NAME is not an accessor or
    ;; something already?
    (let ((fun (ref-leaf
                (lvar-use
                 (return-result (lambda-return lambda))))))
      (setf (leaf-%source-name fun) name)
      (setf (functional-%debug-name fun) nil)
      (let ((old (gethash name (free-funs *ir1-namespace*))))
        (when old (substitute-leaf-if
                   (lambda (ref)
                     (policy ref (> recognize-self-calls 0)))
                   fun old)))
      name)))

;;; If ERORRP is true signals an error immediately -- otherwise
;;; returns a function that will signal the error.
(defun %compile-in-lexenv (form *lexenv* name source-info tlf ephemeral errorp for-eval)
  ;; This ridiculous check for a NIL-returning constant function cuts out hundreds of
  ;; identical functions that result from all the turds that users seem to generate.
  ;; It's not coming from CLOS per se because our DEFCLASS knows to use :INITFUNCTION
  ;; as #'SB-INT:CONSTANTLY-NIL of its own volition when applicable. Likely it is user-
  ;; written code that employs a similar paradigm with no recognition of common cases.
  (when (and (typep form '(cons (eql lambda)))
             (let ((cdr (cdr form)))
               (and (typep cdr '(cons (eql nil)))
                    (or (null (setq cdr (cdr cdr)))
                        (equal cdr '(nil))
                        (equal cdr '('nil))))))
    ;; I sure hope that users don't expect COMPILE to necessarily return a
    ;; unique blob of code. How could they?
    (return-from %compile-in-lexenv
      (if (policy *lexenv* (= safety 0))
          (load-time-value #'constantly-nil t)
          (load-time-value #'sb-impl::0-arg-nil t))))
  (with-compilation-values (:just-values for-eval)
    (prog ((source-paths (when source-info *source-paths*))
           (compile-object (make-core-object ephemeral))
           (oops nil))
       (handler-bind ((compiled-program-error
                        (lambda (e)
                          (setf oops e)
                          ;; Unwind the compiler frames: users want the know where
                          ;; the error came from, not how the compiler got there.
                          (go :error))))
         (return
           (core-call-toplevel-lambda
            ;; FIXME: These bindings were copied from SUB-COMPILE-FILE with
            ;; few changes. Once things are stable, the shared bindings
            ;; probably be merged back together into some shared utility
            ;; macro, or perhaps both merged into one of the existing utility
            ;; macros SB-C::WITH-COMPILATION-VALUES or
            ;; CL:WITH-COMPILATION-UNIT.
            (with-compilation-unit ()
              (let* ((*last-error-context* nil)
                     (*last-message-count* (list* 0 nil nil)))
                (handler-bind ((compiler-error #'compiler-error-handler)
                               (style-warning #'compiler-style-warning-handler)
                               (warning #'compiler-warning-handler)
                               (compiler-note
                                 ;; Compiler notes just clutter up the
                                 ;; REPL: anyone caring about
                                 ;; performance should not be using
                                 ;; EVAL.
                                 (lambda (c)
                                   (when for-eval
                                     (muffle-warning c)))))
                  (with-source-paths
                    (let* ((tlf (or tlf 0))
                           ;; If we have a source-info from LOAD, we will
                           ;; also have a source-paths already set up -- so drop
                           ;; the ones from WITH-COMPILATION-VALUES.
                           (*source-paths* (or source-paths *source-paths*))
                           (*source-info* (or source-info
                                              (make-lisp-source-info
                                               form :parent *source-info*)))
                           (*allow-instrumenting* nil)
                           (*compilation*
                             (make-compilation
                              (and (member :msan *features*)
                                   (find-dynamic-foreign-symbol-address "__msan_unpoison"))))
                           (*gensym-counter* 0)
                           ;; KLUDGE: This rebinding of policy is necessary so that
                           ;; forms such as LOCALLY at the REPL actually extend the
                           ;; compilation policy correctly.  However, there is an
                           ;; invariant that is potentially violated: future
                           ;; refactoring must not allow this to be done in the file
                           ;; compiler.  At the moment we're clearly alright, as we
                           ;; call %COMPILE with a core-object, not a fasl-stream,
                           ;; but caveat future maintainers. -- CSR, 2002-10-27
                           (*policy* (lexenv-policy *lexenv*))
                           ;; see above
                           (*handled-conditions* (lexenv-handled-conditions *lexenv*))
                           ;; ditto
                           (*disabled-package-locks* (lexenv-disabled-package-locks *lexenv*))
                           ;; FIXME: ANSI doesn't say anything about CL:COMPILE
                           ;; interacting with these variables, so we shouldn't. As
                           ;; of SBCL 0.6.7, COMPILE-FILE controls its verbosity by
                           ;; binding these variables, so as a quick hack we do so
                           ;; too. But a proper implementation would have verbosity
                           ;; controlled by function arguments and lexical variables.
                           (*compile-verbose* nil)
                           (*compile-print* nil)
                           ;; in some circumstances, we can trigger execution
                           ;; of user code during optimization, which can
                           ;; re-enter the compiler through explicit calls to
                           ;; EVAL or COMPILE.  Those inner evaluations
                           ;; shouldn't attempt to report any compiler problems
                           ;; using the outer compiler error context.
                           (*compiler-error-context* nil))
                      (handler-bind (((satisfies handle-condition-p) 'handle-condition-handler))
                        (unless source-paths
                          (find-source-paths form tlf))
                        (with-ir1-namespace
                          (let* ((*lexenv* (make-lexenv
                                            :policy *policy*
                                            :handled-conditions *handled-conditions*
                                            :disabled-package-locks *disabled-package-locks*))
                                 (*compile-object* compile-object)
                                 (path (or (get-source-path form)
                                           (cons form (or (and (boundp '*current-path*)
                                                               *current-path*)
                                                          `(original-source-start 0 ,tlf)))))
                                 (lambda (ir1-toplevel form path t)))

                            (compile-fix-function-name lambda name)
                            (locall-analyze-clambdas-until-done (list lambda))

                            (multiple-value-bind (components top-components)
                                (find-initial-dfo (list lambda))
                              (dolist (component (append components top-components))
                                (compile-component component)))

                            (fix-core-source-info *source-info* *compile-object*
                                                  (policy (lambda-bind lambda)
                                                      (> store-source-form 0)))

                            lambda))))))))
            compile-object)))
      :error
      ;; Either signal the error right away, or return a function that
      ;; will signal the corresponding COMPILED-PROGRAM-ERROR. This is so
      ;; that we retain our earlier behaviour when called with erronous
      ;; lambdas via %SIMPLE-EVAL. We could legally do just either one
      ;; always, but right now keeping the old behaviour seems like less
      ;; painful option: compiler.pure.lisp is full of tests that make all
      ;; sort of assumptions about when which things are signalled. FIXME,
      ;; probably.
      (if errorp
          (%program-error (sb-kernel::program-error-message oops))
          (return
            (lambda (&rest arguments)
              (declare (ignore arguments))
              (error oops)))))))

(defun eval-with-compile-in-lexenv (form *lexenv* source-info tlf ephemeral)
  (%compile-in-lexenv form *lexenv* nil source-info tlf ephemeral t t))

(defun compile-in-lexenv (form *lexenv* name source-info tlf ephemeral errorp)
  (unless (typep form '(cons (member lambda named-lambda) t))
    (compiler-error "Not a valid lambda expression:~%  ~S"
                    form))
  (when name
    (legal-fun-name-or-type-error name))
  (%compile-in-lexenv form *lexenv* name source-info tlf ephemeral errorp nil))

(defun make-form-tracking-stream-observer (file-info)
   (lambda (arg1 arg2 arg3)
     ;; Log some kind of reader event into FILE-INFO.
     (case arg1
       (:reset ; a char macro returned zero values - "virtual whitespace".
        ;; I think this would be an ideal place at which to inquire and stash
        ;; the FILE-POSITION in bytes so that DEBUG-SOURCE-START-POSITIONS
        ;; are obtained _after_ having skipped virtual whitespace, not before.
        (setf (fill-pointer (file-info-subforms file-info)) 0))
       (t
        (let ((subforms (file-info-subforms file-info)))
          ;; (ARG1 ARG2 ARG3) = (start-pos end-pos form)
          (vector-push-extend arg1 subforms)
          (vector-push-extend arg2 subforms)
          (vector-push-extend arg3 subforms))))))

;;; COMPILE-FILE-POSITION macro

;; Macros and inline functions report the original-source position. e.g.:
;;   01: (declaim (inline foo))
;;   02: (defun foo (x) (if x (warn "fail @line ~d" (compile-file-position))))
;;   03: (defun bar (y) (foo y))
;;   04: (defun baz (y) (foo y))
;; will cause BAR to print 3 and BAZ to print 4 in the warning message.

;; For macros this seems fair enough, but for inline functions it could
;; be considered undesirable on the grounds that enabling/disabling inlining
;; should not change visible behavior. Other than working harder to figure
;; out where we are in inlined code (which may not even be feasible),
;; a viable remedy is that all inlineable functions should have their stored
;; representation not contain any macros, i.e. macros could be pre-expanded,
;; which in this case means stuffing in the literal integers.
;; I feel that that would be a general improvement to semantics, because
;; as things are, an inline function's macros are expanded at least as many
;; times as there are calls to the function - not very defensible
;; as a design choice, but just an accident of the particular implementation.
;;
(let ()
(defmacro compile-file-position (&whole this-form)
  "Return character position of this macro invocation or NIL if unavailable."
  ;; Counting characters is intuitive because the transfer element size is 1
  ;; measurement unit. The standard allows counting in something other than
  ;; characters (namely bytes) for character streams, which is basically
  ;; irrelevant here, as we don't need random access to the file.
  (values (compute-compile-file-position this-form)))

(defmacro compile-file-line (&whole this-form)
  "Return line# and column# of this macro invocation as multiple values."
  (let ((start (form-source-bounds this-form)))
    `(values ,(or (car start) 0) ,(or (cdr start) -1))))
)

(defun compute-compile-file-position (this-form)
  (let (file-info stream start-pos end-pos)
    (flet ((find-form-eq (form &optional fallback-path)
             (when (and file-info (file-info-subforms file-info))
               (with-array-data ((vect (file-info-subforms file-info))
                                 (start) (end) :check-fill-pointer t)
                 (declare (ignore start))
                 (do ((i (1- end) (- i 3)))
                     ((< i 0))
                   (declare (index-or-minus-1 i))
                   (when (eq form (svref vect i))
                     (if start-pos ; ambiguous
                         (return
                           (setf (values start-pos end-pos)
                                 (and fallback-path
                                      (compile-file-position-helper
                                       file-info fallback-path))))
                         (setq start-pos (svref vect (- i 2))
                               end-pos (svref vect (1- i))))))))))
      (let ((source-info *source-info*)
            (source-path
             (cond ((boundp '*current-path*) *current-path*)
                   ((boundp '*source-paths*) (get-source-path this-form)))))
        (when (and source-info (boundp '*current-path*))
          (setq file-info (source-info-file-info source-info)
                stream (source-info-stream source-info))
          (cond
            ((not source-path)
             ;; probably a read-time eval
             (find-form-eq this-form))
            (t
             (let* ((original-source-path (source-path-original-source source-path))
                    (path (reverse original-source-path)))
               (when (file-info-subforms file-info)
                 (let ((form (elt (file-info-forms file-info) (car path))))
                   (dolist (p (cdr path))
                     (unless (listp form)
                       ;; probably comma
                       (return))
                     (setq form (nth p form)))
                   (find-form-eq form (cdr path))))
               (unless (and start-pos end-pos)
                 (let ((parent (source-info-parent *source-info*)))
                 ;; probably in a local macro executing COMPILE-FILE-POSITION,
                 ;; not producing a sexpr containing an invocation of C-F-P.
                   (when parent
                     (setq file-info (source-info-file-info parent)
                           stream (source-info-stream parent))
                     (find-form-eq this-form (cdr path)))))))))))
    (values start-pos end-pos stream)))

;; Given the form whose source path is PATH-TO-FIND, return the values
;; corresponding to FILE-POSITION of that form's first and last characters.
;; (Note that thse are sometimes approximate depending on whitespace)
;; The form should be the currently-being-compiled toplevel form
;; or subform thereof, and findable by EQness in the FILE-INFO's forms read.
;; This is done by imparting tree structure to the annotations
;; more-or-less paralleling construction of the original sexpr.
;; Unfortunately, though this was a nice idea, it is not terribly useful.
;; FIND-SOURCE-PATHS can not supply the correct path because it assumes
;; that a form determines a path, whereas the opposite is more accurate.
;; e.g. below are two functions that cause misbehavior.
#|
 * (defun example1 (x)
     (if x
         #1=(format t "Err#2 @~D~%" (compile-file-position))
         (progn #1#)))
 * (defconstant +foo+ '(format t "Err#1 @~D~%" (compile-file-position))))
 * (defun example2 (x) (if x #.+foo+ (progn #.+foo+)))
|#
;; In each case the compiler assigns the same source path to two logically
;; different paths that it takes as it IR1-translates each half of the IF
;; expression, though the ELSE branch obviously has a longer path.
;; However, if you _could_ supply correct paths, this would compute correct
;; answers. (Modulo any bugs due to near-total lack of testing)

(defun compile-file-position-helper (file-info path-to-find)
  (let (start-char end-char)
    (labels
        ((recurse (subpath upper-bound queue)
           (let ((index -1))
             (declare (type index-or-minus-1 index))
             (loop
              (let* ((item (car queue))
                     (end (cdar item)))
                (when (> end upper-bound)
                  (return))
                (pop queue)
                (incf index)
                (when (and (eql index (car subpath)) (not (cdr subpath)))
                  ;; This does not eagerly declare victory, because we want
                  ;; to find the rightmost match. In "#1=(FOO)" there are two
                  ;; different annotations pointing to (FOO).
                  (setq start-char (caar item)
                        end-char (cdar item)))
                (unless queue (return))
                (let* ((next (car queue))
                       (next-end (cdar next)))
                  (cond ((< next-end end) ; could descend
                         ;; only scan children if we're on the correct path
                         (if (eql index (car subpath))
                             (setf queue (recurse (cdr subpath) end queue))
                             ;; else skip quickly by finding the next sibling
                             (loop
                                (pop queue)
                                (when (or (endp queue) (>= (caaar queue) end))
                                  (return))))
                           (unless queue (return)))
                        ((= next-end end) ; probably because of "#n="
                         (decf (truly-the (integer 0) index))))))))
           queue))
      (let ((list
             (with-array-data ((v (file-info-subforms file-info))
                               (start) (end) :check-fill-pointer t)
               (declare (ignore start))
               (sort (loop for i from 0 below end by 3
                           collect (acons (aref v i)
                                          (aref v (+ i 1))
                                          (aref v (+ i 2))))
                     #'< :key 'caar))))
        (recurse path-to-find (cdaar list) (cdr list))))
    (values start-char end-char)))

;;; Given FORM which must be the currently-being-compiled toplevel form or subform thereof,
;;; return (VALUES START END) of that form where each coordinate is a cons (LINE . COLUMN).
(defun form-source-bounds (form)
  (multiple-value-bind (start-pos end-pos stream) (compute-compile-file-position form)
    (if (and start-pos end-pos (form-tracking-stream-p stream))
        (values (line/col-from-charpos stream start-pos)
                (line/col-from-charpos stream end-pos))
        (values nil nil))))

(define-load-time-global *background-tasks* nil)
(defun default-compiler-worker (&aux compiled)
  (loop
    (let ((item (sb-ext:atomic-pop *background-tasks*)))
      (unless item (return compiled))
      (setq compiled t)
      (funcall item))))
