(in-package :sb-cltl2)

(def-ir1-translator compiler-let ((bindings &rest forms) start next result)
  (loop for binding in bindings
     if (atom binding)
        collect binding into vars
        and collect nil into values
       else do (assert (proper-list-of-length-p binding 1 2))
        and collect (first binding) into vars
        and collect (eval (second binding)) into values
     finally (return (progv vars values
                       (sb-c::ir1-convert-progn-body start next result forms)))))

(defun walk-compiler-let (form context env)
  (declare (ignore context))
  (destructuring-bind (bindings &rest body)
      (cdr form)
    (loop for binding in bindings
       if (atom binding)
          collect binding into vars
          and collect nil into values
         else do (assert (proper-list-of-length-p binding 1 2))
          and collect (first binding) into vars
          and collect (eval (second binding)) into values
       finally (return
                 (progv vars values
                   (let ((walked-body (sb-walker::walk-repeat-eval body env)))
                     (sb-walker::relist* form
                                         'compiler-let bindings walked-body)))))))

(sb-walker::define-walker-template compiler-let walk-compiler-let)
