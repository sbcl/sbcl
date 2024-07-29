;;;; DEFMACRO machinery

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defun macro-function (symbol &optional env)
  "If SYMBOL names a macro in ENV, returns the expansion function,
else returns NIL. If ENV is unspecified or NIL, use the global environment
only."
  ;; local function definitions (ordinary) can shadow a global macro
  (typecase env
    #+(and sb-fasteval (not sb-xc-host))
    (sb-interpreter:basic-env
     (multiple-value-bind (kind def)
         (sb-interpreter:find-lexical-fun env symbol)
       (when def
         (return-from macro-function (when (eq kind :macro) def)))))
    (lexenv
     (let ((def (cdr (assoc symbol (lexenv-funs env)))))
       (when def
         (return-from macro-function
           (when (typep def '(cons (eql macro))) (cdr def)))))))
  (values (info :function :macro-function symbol)))

(defvar *setf-macro-function-hook* nil
  "A list of functions that (SETF MACRO-FUNCTION) invokes before storing the new
   value. The functions take the macro name and the new value.")

(defun (setf macro-function) (function symbol &optional environment)
  (declare (symbol symbol) (type function function))
  (when environment
    ;; Note: Technically there could be an ENV optional argument to
    ;; SETF MACRO-FUNCTION, but since ANSI says that the consequences
    ;; of supplying a non-nil one are undefined, we don't allow it.
    ;; (Thus our implementation of this unspecified behavior is to
    ;; complain. Since the behavior is unspecified, this is
    ;; conforming.:-)
    (error "Non-NIL environment argument in SETF of MACRO-FUNCTION ~S: ~S"
           symbol environment))
  (when (eq (info :function :kind symbol) :special-form)
    (error "~S names a special form." symbol))
  (when (boundp '*setf-macro-function-hook*) ; unbound during cold init
    (dolist (f *setf-macro-function-hook*)
      (funcall f symbol function)))
  (with-single-package-locked-error (:symbol symbol "setting the macro-function of ~S")
    (clear-info :function :type symbol)
    (setf (info :function :kind symbol) :macro)
    (setf (info :function :macro-function symbol) function)
    #-sb-xc-host (install-guard-function symbol `(:macro ,symbol)))
  function)

(let ()
  (defmacro sb-xc:defmacro (name lambda-list &body body)
    (check-designator name 'defmacro)
    ;; When we are building the cross-compiler, we could be in a host
    ;; lisp which implements CL macros (e.g. CL:AND) as special
    ;; operators (while still providing a macroexpansion for
    ;; compliance): therefore can't use the host's SPECIAL-OPERATOR-P
    ;; as a discriminator, but that's OK because the set of forms the
    ;; cross-compiler compiles is tightly controlled.  -- CSR,
    ;; 2003-04-20
    #-sb-xc-host
    (when (special-operator-p name)
      (error "The special operator ~S can't be redefined as a macro."
             name))
    ;; The name of the lambda is (MACRO-FUNCTION name)
    ;; which does not conflict with any legal function name.
    (let ((def (make-macro-lambda (debug-name 'macro-function name)
                                  lambda-list body 'defmacro name)))
      `(progn
         ;; %COMPILER-DEFMACRO just performs a check for duplicate definitions
         ;; within a file.
         (eval-when (:compile-toplevel)
           (%compiler-defmacro :macro-function ',name))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (%defmacro ',name ,def (source-location)))))))

;;; Detect duplicate definitions within a file. However, no package
;;; lock check is necessary - it's handled elsewhere.
;;;
;;; Additionally, this is a STYLE-WARNING, not a WARNING, because there is
;;; meaningful behavior that can be ascribed to some redefinitions, e.g.
;;;  (defmacro foo () first-definition)
;;;  (defun f () (use-it (foo )))
;;;  (defmacro foo () other-definition)
;;; will use the first definition when compiling F, but make the second available
;;; in the loaded fasl. In this usage it would have made sense to wrap the
;;; respective definitions with EVAL-WHEN for different situations,
;;; but as long as the compile-time behavior is deterministic, it's just bad style
;;; and not flat-out wrong, though there is indeed some waste in the fasl.
;;;
;;; KIND is the globaldb KIND of this NAME
(defun %compiler-defmacro (kind name)
  (let ((name-key `(,kind ,name)))
    (when (boundp '*lexenv*)
      ;; a slight OAOO issue here wrt %COMPILER-DEFUN
      (let ((c *compilation*))
        (if (hashset-find (fun-names-in-this-file c) name-key)
            (compiler-style-warn 'same-file-redefinition-warning :name name)
            (hashset-insert (fun-names-in-this-file c) name-key))))))

(defun %defmacro (name definition source-location)
  (declare (ignorable source-location)) ; xc-host doesn't use
            ;; old note (ca. 1985, maybe:-): "Eventually %%DEFMACRO
            ;; should deal with clearing old compiler information for
            ;; the functional value."
  (let ((kind (info :function :kind name)))
              ;; Check for special form before package locks.
    (when (eq :special-form kind)
      (error "The special operator ~S can't be redefined as a macro."
             name))
    (with-single-package-locked-error (:symbol name "defining ~S as a macro")
      (when (eq :function kind)
        (style-warn
         "~S is being redefined as a macro when it was previously ~(~A~) to be a function."
         name (info :function :where-from name))
        (undefine-fun-name name))
      (clear-info :function :where-from name)
      #-sb-xc-host
      (when (fboundp name)
                  ;; Someday we could check for macro arguments
                  ;; being incompatibly redefined. Doing this right
                  ;; will involve finding the old macro lambda-list
                  ;; and comparing it with the new one.
        (warn 'redefinition-with-defmacro :name name
              :new-function definition :new-location source-location))
      (setf (macro-function name) definition)))
  name)
