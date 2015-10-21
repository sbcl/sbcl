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

(in-package "SB!C")

;;;; CL:COMPILE

(defun get-lambda-to-compile (definition-designator)
  (if (consp definition-designator)
      definition-designator
      (multiple-value-bind (definition env-p)
                           (function-lambda-expression definition-designator)
        (when env-p
          (error "~S was defined in a non-null environment."
                 definition-designator))
        (unless definition
          (error "can't find a definition for ~S" definition-designator))
        definition)))

;;; Handle the nontrivial case of CL:COMPILE.
;;;
;;; If ERRORP is true signals an error immediately -- otherwise returns
;;; a function that will signal the error.
(defun actually-compile (name definition *lexenv* source-info tlf errorp)
  (let ((source-paths (when source-info *source-paths*)))
    (with-compilation-values
      (sb!xc:with-compilation-unit ()
        ;; FIXME: These bindings were copied from SUB-COMPILE-FILE with
        ;; few changes. Once things are stable, the shared bindings
        ;; probably be merged back together into some shared utility
        ;; macro, or perhaps both merged into one of the existing utility
        ;; macros SB-C::WITH-COMPILATION-VALUES or
        ;; CL:WITH-COMPILATION-UNIT.
        (with-source-paths
          (prog* ((tlf (or tlf 0))
                  ;; If we have a source-info from LOAD, we will
                  ;; also have a source-paths already set up -- so drop
                  ;; the ones from WITH-COMPILATION-VALUES.
                  (*source-paths* (or source-paths *source-paths*))
                  (form (get-lambda-to-compile definition))
                  (*source-info* (or source-info
                                  (make-lisp-source-info
                                   form :parent *source-info*)))
                  (*toplevel-lambdas* ())
                  (*block-compile* nil)
                  (*allow-instrumenting* nil)
                  (*code-coverage-records* nil)
                  (*code-coverage-blocks* nil)
                  (*current-path* nil)
                  (*last-source-context* nil)
                  (*last-original-source* nil)
                  (*last-source-form* nil)
                  (*last-format-string* nil)
                  (*last-format-args* nil)
                  (*last-message-count* 0)
                  (*last-error-context* nil)
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
                  (oops nil))
             (handler-bind (((satisfies handle-condition-p) #'handle-condition-handler))
               (unless source-paths
                 (find-source-paths form tlf))
               (let ((*compiler-error-bailout*
                       (lambda (e)
                         (setf oops e)
                         ;; Unwind the compiler frames: users want the know where
                         ;; the error came from, not how the compiler got there.
                         (go :error))))
                 (return
                   (with-world-lock ()
                     (%compile form (make-core-object)
                               :name name
                               :path `(original-source-start 0 ,tlf))))))
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
                 (error oops)
                 (let ((message (princ-to-string oops))
                       (source (source-to-string form)))
                   (return
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error 'compiled-program-error
                              :message message
                              :source source)))))))))))

(defun compile-in-lexenv (name definition lexenv
                          &optional source-info tlf errorp)
  (dx-flet ((really-compile (definition lexenv)
              (actually-compile
               name definition lexenv source-info tlf errorp)))
    (multiple-value-bind (compiled-definition warnings-p failure-p)
        (typecase definition
          #!+sb-fasteval
          (sb!interpreter:interpreted-function
           (multiple-value-call #'really-compile
             (sb!interpreter:prepare-for-compile definition)))
          #!+sb-eval
          (sb!eval:interpreted-function
           (multiple-value-call #'really-compile
             (sb!eval:prepare-for-compile definition)))
          (function
           (values definition nil nil))
          (t
           (really-compile definition lexenv)))
      (aver (typep compiled-definition 'compiled-function))
      (let ((result (if (not name)
                        compiled-definition
                        (progn
                          (if (and (symbolp name) (macro-function name))
                              (setf (macro-function name) compiled-definition)
                              (setf (fdefinition name) compiled-definition))
                          name))))
        (values result warnings-p failure-p)))))

(defun compile (name &optional (definition (or (and (symbolp name)
                                                    (macro-function name))
                                               (fdefinition name))))
  #!+sb-doc
  "Produce a compiled function from DEFINITION. If DEFINITION is a
lambda-expression, it is coerced to a function. If DEFINITION is an
interpreted function, it is compiled. If DEFINITION is already a compiled
function, it is used as-is. (Future versions of SBCL might try to
recompile the existing definition, but this is not currently supported.)

If NAME is NIL, the compiled function is returned as the primary value.
Otherwise the resulting compiled function replaces existing function
definition of NAME, and NAME is returned as primary value; if NAME is a symbol
that names a macro, its macro function is replaced and NAME is returned as
primary value.

Also returns a secondary value which is true if any conditions of type
WARNING occur during the compilation, and NIL otherwise.

Tertiary value is true if any conditions of type ERROR, or WARNING that are
not STYLE-WARNINGs occur during compilation, and NIL otherwise.
"
  (compile-in-lexenv name definition (make-null-lexenv)))

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
;; should not change visible behavior. Other then working harder to figure
;; out where we are in inlined code (which may not even be feasible),
;; a viable remedy is that all inlineable functions should have their stored
;; representation not contain any macros, i.e. macros could be pre-expanded,
;; which in this case means stuffing in the literal integers.
;; I feel that that would be a general improvement to semantics, because
;; as things are, an inline function's macros are expanded at least as many
;; times as there are calls to the function - not very defensible
;; as a design choice, but just an accident of the particular implementation.
;;
(defmacro compile-file-position (&whole this-form)
  #!+sb-doc
  "Return character position of this macro invocation or NIL if unavailable."
  ;; Counting characters is intuitive because the transfer element size is 1
  ;; measurement unit. The standard allows counting in something other than
  ;; characters (namely bytes) for character streams, which is basically
  ;; irrelevant here, as we don't need random access to the file.
  (compute-compile-file-position this-form nil))

(defmacro compile-file-line (&whole this-form)
  #!+sb-doc
  "Return line# and column# of this macro invocation as multiple values."
  (compute-compile-file-position this-form t))

(defun compute-compile-file-position (this-form as-line/col-p)
  (let (file-info stream charpos)
    (flet ((find-form-eq (form &optional fallback-path)
               (with-array-data ((vect (file-info-subforms file-info))
                                 (start) (end) :check-fill-pointer t)
                 (declare (ignore start))
                 (do ((i (1- end) (- i 3)))
                     ((< i 0))
                   (declare (index-or-minus-1 i))
                   (when (eq form (svref vect i))
                     (if charpos ; ambiguous
                         (return
                           (setq charpos
                                 (and fallback-path
                                      (compile-file-position-helper
                                       file-info fallback-path))))
                         (setq charpos (svref vect (- i 2)))))))))
      (let ((source-info *source-info*))
        (when (and source-info (boundp '*current-path*))
          (setq file-info (source-info-file-info source-info)
                stream (source-info-stream source-info))
          (cond
            ((not *current-path*)
             ;; probably a read-time eval
             (find-form-eq this-form))
          ;; Hmm, would a &WHOLE argument would work better or worse in general?
            (t
             (let* ((original-source-path
                     (cddr (member 'original-source-start *current-path*)))
                    (path (reverse original-source-path)))
               (when (file-info-subforms file-info)
                 (let ((form (elt (file-info-forms file-info) (car path))))
                   (dolist (p (cdr path))
                     (setq form (nth p form)))
                   (find-form-eq form (cdr path))))
               (unless charpos
                 (let ((parent (source-info-parent *source-info*)))
                 ;; probably in a local macro executing COMPILE-FILE-POSITION,
                 ;; not producing a sexpr containing an invocation of C-F-P.
                   (when parent
                     (setq file-info (source-info-file-info parent)
                           stream (source-info-stream parent))
                     (find-form-eq this-form))))))))))
    (if as-line/col-p
        (if (and charpos (form-tracking-stream-p stream))
            (let ((line/col (line/col-from-charpos stream charpos)))
              `(values ,(car line/col) ,(cdr line/col)))
            '(values 0 -1))
        charpos)))

;; Find FORM's character position in FILE-INFO by looking for PATH-TO-FIND.
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
  (let (found-form start-char)
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
                  (setq found-form (cdr item)
                        start-char (caar item)))
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
    start-char))
