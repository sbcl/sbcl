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
    (with-unique-names (whole environment)
      (multiple-value-bind (new-body local-decs doc)
          (parse-defmacro lambda-list whole body name 'defmacro
                          :environment environment)
        (let ((def `(lambda (,whole ,environment)
                      ,@local-decs
                      ,new-body))
              ;; If we want to move over to list-style names
              ;; [e.g. (DEFMACRO FOO), maybe to support some XREF-like
              ;; functionality] here might be a good place to start.
              (debug-name (sb!c::debug-name 'macro-function name)))
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (sb!c::%defmacro ',name #',def ',lambda-list
                              ,doc ',debug-name)))))))

(macrolet
    ((def (times set-p)
       `(eval-when (,@times)
          (defun sb!c::%defmacro (name definition lambda-list doc debug-name)
            ;; old note (ca. 1985, maybe:-): "Eventually %%DEFMACRO
            ;; should deal with clearing old compiler information for
            ;; the functional value."
            ,@(unless set-p
                '((declare (ignore lambda-list debug-name))))
            (ecase (info :function :kind name)
              ((nil))
              (:function
               ;; (remhash name *free-funs*)
               (undefine-fun-name name)
               (style-warn
                "~S is being redefined as a macro when it was ~
                 previously ~(~A~) to be a function."
                name
                (info :function :where-from name)))
              (:macro)
              (:special-form
               (error "The special form ~S can't be redefined as a macro."
                      name)))
            (clear-info :function :where-from name)
            ;; FIXME: It would be nice to warn about DEFMACRO of an
            ;; already-defined macro, but that's slightly hard to do
            ;; because in common usage DEFMACRO is defined at compile
            ;; time and then redefined at load time. We'd need to make a
            ;; distinction between the defined-at-compile-time state and
            ;; the defined-at-load-time state to make this work. (Trying
            ;; to warn about duplicate DEFTYPEs runs into the same
            ;; problem.)
            #+nil (when (sb!xc:macro-function name)
                    ;; Someday we could check for macro arguments
                    ;; being incompatibly redefined. Doing this right
                    ;; will involve finding the old macro lambda-list
                    ;; and comparing it with the new one.
                    (style-warn "redefining ~S in DEFMACRO" name))
            (setf (sb!xc:macro-function name) definition
                  (fdocumentation name 'function) doc)
            ,(when set-p
                   `(case (widetag-of definition)
                      (#.sb!vm:closure-header-widetag
                       (setf (%simple-fun-arglist (%closure-fun definition))
                             lambda-list
                             (%simple-fun-name (%closure-fun definition))
                             debug-name))
                      (#.sb!vm:simple-fun-header-widetag
                       (setf (%simple-fun-arglist definition) lambda-list
                             (%simple-fun-name definition) debug-name))))
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
