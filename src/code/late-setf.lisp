;;;; SETF-related stuff which requires COLLECT, separated into this
;;;; separate file to deal with boot order problems (since COLLECT
;;;; requires other SETF-related stuff)
;;;;
;;;; FIXME: Now that we don't do bogobootstrapping, these boot order
;;;; problems may no longer exist, so perhaps we could merge back with
;;;; other SETF logic.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defmacro-mundanely psetf (&rest args &environment env)
  #!+sb-doc
  "This is to SETF as PSETQ is to SETQ. Args are alternating place
  expressions and values to go into those places. All of the subforms and
  values are determined, left to right, and only then are the locations
  updated. Returns NIL."
  (declare (type sb!c::lexenv env))
  (collect ((let*-bindings) (mv-bindings) (setters))
    (do ((a args (cddr a)))
        ((endp a))
      (if (endp (cdr a))
          (error "Odd number of args to PSETF."))
      (multiple-value-bind (dummies vals newval setter getter)
          (sb!xc:get-setf-expansion (car a) env)
        (declare (ignore getter))
        (let*-bindings (mapcar #'list dummies vals))
        (mv-bindings (list newval (cadr a)))
        (setters setter)))
    (labels ((thunk (let*-bindings mv-bindings)
               (if let*-bindings
                   `(let* ,(car let*-bindings)
                      (multiple-value-bind ,@(car mv-bindings)
                        ,(thunk (cdr let*-bindings) (cdr mv-bindings))))
                   `(progn ,@(setters) nil))))
      (thunk (let*-bindings) (mv-bindings)))))

;;; FIXME: Compiling this definition of ROTATEF apparently blows away the
;;; definition in the cross-compiler itself, so that after that, any
;;; ROTATEF operations can no longer be compiled, because
;;; GET-SETF-EXPANSION is called instead of SB!XC:GET-SETF-EXPANSION.
(defmacro-mundanely rotatef (&rest args &environment env)
  #!+sb-doc
  "Takes any number of SETF-style place expressions. Evaluates all of the
   expressions in turn, then assigns to each place the value of the form to
   its right. The rightmost form gets the value of the leftmost.
   Returns NIL."
  (declare (type sb!c::lexenv env))
  (when args
    (collect ((let*-bindings) (mv-bindings) (setters) (getters))
      (dolist (arg args)
        (multiple-value-bind (temps subforms store-vars setter getter)
            (sb!xc:get-setf-expansion arg env)
          (loop
            for temp in temps
            for subform in subforms
            do (let*-bindings `(,temp ,subform)))
          (mv-bindings store-vars)
          (setters setter)
          (getters getter)))
      (setters nil)
      (getters (car (getters)))
      (labels ((thunk (mv-bindings getters)
                 (if mv-bindings
                     `((multiple-value-bind ,(car mv-bindings) ,(car getters)
                         ,@(thunk (cdr mv-bindings) (cdr getters))))
                     (setters))))
        `(let* ,(let*-bindings)
           ,@(thunk (mv-bindings) (cdr (getters))))))))

(sb!xc:define-setf-expander values (&rest places &environment env)
  (declare (type sb!c::lexenv env))
  (collect ((setters) (getters))
    (let ((all-dummies '())
          (all-vals '())
          (newvals '()))
      (dolist (place places)
        (multiple-value-bind (dummies vals newval setter getter)
            (sb!xc:get-setf-expansion place env)
          ;; ANSI 5.1.2.3 explains this logic quite precisely.  --
          ;; CSR, 2004-06-29
          (setq all-dummies (append all-dummies dummies (cdr newval))
                all-vals (append all-vals vals
                                 (mapcar (constantly nil) (cdr newval)))
                newvals (append newvals (list (car newval))))
          (setters setter)
          (getters getter)))
      (values all-dummies all-vals newvals
              `(values ,@(setters)) `(values ,@(getters))))))
