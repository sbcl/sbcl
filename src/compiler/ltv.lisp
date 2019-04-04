;;;; This file implements LOAD-TIME-VALUE.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defknown %load-time-value (t) t (flushable movable))

;;; Compile FORM and arrange for it to be called at load-time. Return
;;; the dumper handle and our best guess at the type of the object.
;;; It would be nice if L-T-V forms were generally eligible
;;; for fopcompilation, as it could eliminate special cases below.
(defun compile-load-time-value (form &optional no-skip)
  (acond ((typecase form
            ;; This case is important for dumping packages as constants
            ;; in cold-init, but works fine in the normal target too.
            ((cons (eql find-package) (cons string null)) 'package)
            ;; Another similar case - this allows the printer to work
            ;; immediately in cold-init. (See SETUP-PRINTER-STATE.)
            ((cons (eql function)
                   (cons (satisfies legal-fun-name-p) null))
             'function)
            ;; We want to construct cold classoid cells, but in general
            ;; FIND-CLASSOID-CELL could be called with :CREATE NIL
            ;; which can not be handled in cold-load.
            #+sb-xc-host
            ((cons (eql find-classoid-cell) (cons (cons (eql quote))))
             (aver (eq (getf (cddr form) :create) t))
             'sb-kernel::classoid-cell))
          (fopcompile form nil t)
          (values (sb-fasl::dump-pop *compile-object*) (specifier-type it)))
         (t
          (let ((lambda (compile-load-time-stuff form t)))
            (values (fasl-dump-load-time-value-lambda lambda *compile-object*
                                                      no-skip)
                    (let ((type (leaf-type lambda)))
                      (if (fun-type-p type)
                          (single-value-type (fun-type-returns type))
                          *wild-type*)))))))

(def-ir1-translator load-time-value
    ((form &optional read-only-p) start next result)
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
                                      (let ((ftype (global-ftype op)))
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
            (compile-load-time-value
             ;; KLUDGE: purify on cheneygc moves everything in code
             ;; constants into read-only space, value-cell breaks the
             ;; chain.
             (cond #-gencgc
                   ((not read-only-p)
                    `(make-value-cell ,form))
                   (t
                    form)))
          (unless (csubtypep type source-type)
            (setf type source-type))
          (let ((value-form
                  (cond #-gencgc
                        ((not read-only-p)
                         `(value-cell-ref (%load-time-value ',handle)))
                        (t
                         `(%load-time-value ',handle)))))
            (the-in-policy type value-form **zero-typecheck-policy**
                           start next result)))
        ;; When compiling to memory, L-T-V is almost like `(quote ,(eval form)),
        ;; though see the :COMPILE-LOAD-TIME-VALUE-INTERPRETED-MODE regression test
        ;; to understand why that is wrong: minimal compilation must occur.
        ;; Incidentally, it is an extremely subtle issue as to whether ANSI intended
        ;; to perform compile-time semantic processing of the L-T-V form in the
        ;; current environment, but then execute in the null environment (obviously)
        ;; versus do both in the null environment. The distinction is in whether
        ;;  (LET ((X 3)) (MACROLET ((M () (HAIR))) (LOAD-TIME-VALUE (THING)))
        ;; can make use of M. We choose to say that it can't.
        (let ((value (let ((thunk ; Pass T for the EPHEMERAL flag.
                            (compile-in-lexenv `(lambda () ,form) (make-null-lexenv)
                                               nil nil nil t nil)))
                       (handler-case (funcall thunk)
                         (error (condition)
                           (compiler-error "(during EVAL of LOAD-TIME-VALUE)~%~A"
                                           condition))))))
          (if read-only-p
              (ir1-convert start next result `',value)
              #-gencgc
              (the-in-policy (ctype-of value)
                             `(value-cell-ref ,(make-value-cell value))
                             **zero-typecheck-policy**
                             start next result)
              #+gencgc
              ;; Avoid complaints about constant modification
              (ir1-convert start next result `(ltv-wrapper ',value)))))))

(defoptimizer (%load-time-value ir2-convert) ((handle) node block)
  (aver (constant-lvar-p handle))
  (let ((lvar (node-lvar node))
        (tn (make-load-time-value-tn (lvar-value handle)
                                     *universal-type*)))
    (move-lvar-result node block (list tn) lvar)))
