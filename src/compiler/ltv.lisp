;;;; This file implements LOAD-TIME-VALUE.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defknown %load-time-value (t) t (flushable movable))

(def-ir1-translator load-time-value
    ((form &optional read-only-p) start next result)
  #!+sb-doc
  "Arrange for FORM to be evaluated at load-time and use the value produced as
if it were a constant. If READ-ONLY-P is non-NIL, then the resultant object is
guaranteed to never be modified, so it can be put in read-only storage."
  (let ((*allow-instrumenting* nil)
        ;; First derive an approximate type from the source form, because it allows
        ;; us to use READ-ONLY-P implicitly.
        ;;
        ;; We also use this type to augment whatever COMPILE-LOAD-TIME-VALUE
        ;; returns -- in practice it returns *WILD-TYPE* all the time, but
        ;; theoretically it could return something useful for the READ-ONLY-P case.
        (source-type (single-value-type
                      (cond ((consp form)
                             (let ((op (car form)))
                               (cond ((member op '(the truly-the))
                                      (values-specifier-type (second form)))
                                     ((eq 'function op)
                                      (specifier-type 'function))
                                     ((and (legal-fun-name-p op)
                                           (eq :declared (info :function :where-from op)))
                                      (let ((ftype (proclaimed-ftype op)))
                                        (if (fun-type-p ftype)
                                            (fun-type-returns ftype)
                                            *wild-type*)))
                                     (t
                                      *wild-type*))))
                            ((and (symbolp form)
                                  (eq :declared (info :variable :where-from form)))
                             (info :variable :type form))
                            ((constantp form)
                             (ctype-of (eval form)))
                            (t
                             *universal-type*)))))
    ;; Implictly READ-ONLY-P for immutable objects.
    (when (and (not read-only-p)
               (csubtypep source-type (specifier-type '(or character number))))
      (setf read-only-p t))
    (if (producing-fasl-file)
        (multiple-value-bind (handle type)
            ;; Value cells are allocated for non-READ-ONLY-P stop the
            ;; compiler from complaining about constant modification
            ;; -- it seems that we should be able to elide them all
            ;; the time if we had a way of telling the compiler that
            ;; "this object isn't really a constant the way you
            ;; think". --NS 2009-06-28
            (compile-load-time-value (if read-only-p
                                         form
                                         `(make-value-cell ,form)))
          (unless (csubtypep type source-type)
            (setf type source-type))
          (let ((value-form
                  (if read-only-p
                      `(%load-time-value ',handle)
                      `(value-cell-ref (%load-time-value ',handle)))))
            (the-in-policy type value-form **zero-typecheck-policy**
                           start next result)))
        (let ((value
               (flet ((eval-it (operator thing)
                        (handler-case (funcall operator thing)
                          (error (condition)
                            (compiler-error "(during EVAL of LOAD-TIME-VALUE)~%~A"
                                            condition)))))
                 (if (eq sb!ext:*evaluator-mode* :compile)
                     ;; This call to EVAL actually means compile+eval.
                     (eval-it 'eval form)
                     (let ((f (compile nil `(lambda () ,form))))
                       (if f
                           (eval-it 'funcall f)
                           (compiler-error "Failed to compile LOAD-TIME-VALUE form")))))))
          (if read-only-p
              (ir1-convert start next result `',value)
              (the-in-policy (ctype-of value) `(value-cell-ref ,(make-value-cell value))
                             **zero-typecheck-policy**
                             start next result))))))

(defoptimizer (%load-time-value ir2-convert) ((handle) node block)
  (aver (constant-lvar-p handle))
  (let ((lvar (node-lvar node))
        (tn (make-load-time-value-tn (lvar-value handle)
                                     *universal-type*)))
    (move-lvar-result node block (list tn) lvar)))
