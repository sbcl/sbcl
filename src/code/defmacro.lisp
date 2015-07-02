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

;;; the guts of the DEFMACRO macro, pulled out into a separate
;;; function in order to make it easier to express the common
;;; bootstrap idiom
;;;   CL:DEFMACRO SB!XC:DEFMACRO
;;;   SB!XC:DEFMACRO CL:DEFMACRO
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun %expander-for-defmacro (name lambda-list body)
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
             #-sb-xc-host
             ;; Getting  this to cross-compile with the check enabled
             ;; would require %COMPILER-DEFMACRO to be defined earlier,
             ;; but symmetry suggests it be near %COMPILER-DEFUN,
             ;; which isn't soon enough. So leave it out.
             (eval-when (:compile-toplevel)
               (sb!c::%compiler-defmacro :macro-function ',name t))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (sb!c::%defmacro ',name ,def ',lambda-list
                                (sb!c:source-location)))))))

(macrolet
    ((def (times set-p)
       `(eval-when (,@times)
          (defun sb!c::%defmacro (name definition lambda-list source-location)
            (declare (ignorable source-location)) ; xc-host doesn't use
            ;; old note (ca. 1985, maybe:-): "Eventually %%DEFMACRO
            ;; should deal with clearing old compiler information for
            ;; the functional value."
            ,@(unless set-p
                '((declare (ignore lambda-list))))
            (let ((kind (info :function :kind name)))
              ;; Check for special form before package locks.
              (when (eq :special-form kind)
                (error "The special operator ~S can't be redefined as a macro."
                       name))
              (with-single-package-locked-error (:symbol name "defining ~S as a macro")
                (when (eq :function kind)
                  (style-warn
                   "~S is being redefined as a macro when it was ~
                     previously ~(~A~) to be a function."
                   name (info :function :where-from name))
                  (undefine-fun-name name))
                (clear-info :function :where-from name)
                #-sb-xc-host
                (when (fboundp name)
                  ;; Someday we could check for macro arguments
                  ;; being incompatibly redefined. Doing this right
                  ;; will involve finding the old macro lambda-list
                  ;; and comparing it with the new one.
                  (warn 'redefinition-with-defmacro
                        :name name
                        :new-function definition
                        :new-location source-location))
               (setf (sb!xc:macro-function name) definition)
               ,(when set-p
                  `(setf (%fun-lambda-list definition) lambda-list))))
            name))))
  (progn
    (def (:load-toplevel :execute) #-sb-xc-host t #+sb-xc-host nil)
    (def (#-sb-xc :compile-toplevel) nil)))

;;; Parse the definition and make an expander function. The actual
;;; definition is done by %DEFMACRO which we expand into. After the
;;; compiler has gotten the information it wants out of macro
;;; definition, it compiles a call to %DEFMACRO which happens at load
;;; time.
(defmacro sb!xc:defmacro (name lambda-list &rest body)
  (%expander-for-defmacro name lambda-list body))

;;; In the cross-compiler, we not only need to support the definition
;;; of target macros at cross-compiler-build-time (with SB!XC:DEFMACRO
;;; running in the cross-compilation host), we also need to support
;;; the definition of target macros at target compilation time (with
;;; CL:DEFMACRO processed by the cross-compiler)..
#+sb-xc-host
(sb!xc:defmacro defmacro (name lambda-list &rest body)
  (%expander-for-defmacro name lambda-list body))

;;; DEFMACRO-MUNDANELY is like SB!XC:DEFMACRO, except that it doesn't
;;; have any EVAL-WHEN or IR1 magic associated with it, so it only
;;; takes effect in :LOAD-TOPLEVEL or :EXECUTE situations.
(def!macro defmacro-mundanely (name lambda-list &body body)

  ;; old way:
  ;;(let ((whole (gensym "WHOLE-"))
  ;;      (environment (gensym "ENVIRONMENT-")))
  ;;  (multiple-value-bind (new-body local-decs doc)
  ;;      (parse-defmacro lambda-list whole body name 'defmacro
  ;;                      :environment environment)
  ;;    `(progn
  ;;       (setf (sb!xc:macro-function ',name)
  ;;             (lambda (,whole ,environment)
  ;;                 ,@local-decs
  ;;                 (block ,name
  ;;                 ,new-body)))
  ;;       (setf (fdocumentation ',name 'macro)
  ;;             ,doc)
  ;;       ',name)))

  `(let ()
     (sb!xc:defmacro ,name ,lambda-list ,@body)))
