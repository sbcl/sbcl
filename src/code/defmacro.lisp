;;;; DEFMACRO machinery

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(let ()
  (defmacro sb!xc:defmacro (name lambda-list &body body)
    (unless (symbolp name)
      (error "The macro name ~S is not a symbol." name))
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
    (let ((def (make-macro-lambda (sb!c::debug-name 'macro-function name)
                                  lambda-list body 'defmacro name)))
      `(progn
         ;; %COMPILER-DEFMACRO just performs a check for duplicate definitions
         ;; within a file.
         (eval-when (:compile-toplevel)
           (sb!c::%compiler-defmacro :macro-function ',name t))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (sb!c::%defmacro ',name ,def (sb!c:source-location)))))))

(defun sb!c::%defmacro (name definition source-location)
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
      (setf (sb!xc:macro-function name) definition)))
  name)

#+sb-xc-host
(let ((real-expander (macro-function 'sb!xc:defmacro)))
  ;; Inform the cross-compiler how to expand SB!XC:DEFMACRO (= DEFMACRO).
  (setf (sb!xc:macro-function 'sb!xc:defmacro)
        (lambda (form env)
          (declare (ignore env))
          ;; Since SB!KERNEL:LEXENV isn't compatible with the host,
          ;; just pass NIL. The expansion correctly captures a non-null
          ;; environment, but the expander doesn't need it.
          (funcall real-expander form nil)))
  ;; Building the cross-compiler should skip the compile-time-too
  ;; processing SB!XC:DEFMACRO.
  (setf (macro-function 'sb!xc:defmacro)
        (lambda (form env) `(let () ,(funcall real-expander form env)))))
