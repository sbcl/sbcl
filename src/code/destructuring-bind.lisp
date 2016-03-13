;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(macrolet (#+sb-xc-host ; Bootstrap NAMED-DS-BIND
           (named-ds-bind (name lambda-list expression &body body)
             (declare (ignore name))
             `(cl:destructuring-bind ,lambda-list ,expression ,@body)))

  (sb!xc:defmacro destructuring-bind (lambda-list expression &body body
                                                  &environment env)
    (declare (ignore env)) ; could be policy-sensitive (but isn't)
    #!+sb-doc
    "Bind the variables in LAMBDA-LIST to the corresponding values in the
tree structure resulting from the evaluation of EXPRESSION."
    `(binding* ,(sb!c::expand-ds-bind lambda-list expression t nil)
       ,@body))

  (let ()
    ;; This is a variant of destructuring-bind that provides the name
    ;; of the containing construct in generated error messages.
    ;; There is a cyclic dependence between it and DEFMACRO.
    (sb!xc:defmacro named-ds-bind (name lambda-list expression &body body
                                        &environment env)
      (declare (ignore env)) ; could be policy-sensitive (but isn't)
      `(binding* ,(sb!c::expand-ds-bind lambda-list expression t nil name
                                        (and (eq (car name) :macro)
                                             (eq (cddr name) 'deftype)
                                             ''*))
         ,@body))))

#+sb-xc-host
(progn
  ;; The expander for NAMED-DS-BIND in the host could almost always be
  ;; the above MACROLET, but that would fail to use the default of '* for
  ;; optional args of SB!XC:DEFTYPE that lack an overriding default.
  ;; So install our expander even if it produces suboptimal code for the host.
  (defmacro named-ds-bind (&whole form &rest args)
    (declare (ignore args))
    (funcall (sb!xc:macro-function 'named-ds-bind) form nil))

  ;; A similar problem in reverse: SB!XC:DEFMACRO's expansion uses NAMED-DS-BIND
  ;; which expands to BINDING* (from "early-extensions") that hand-written code
  ;; also wants to use. So expand it in the target by using the host's expander
  ;; until it gets seen again during make-host-2.
  (setf (sb!xc:macro-function 'binding*)
        (lambda (form env) (declare (ignore env)) (cl:macroexpand-1 form nil))))
