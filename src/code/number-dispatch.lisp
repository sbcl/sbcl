;;;; This file contains the the NUMBER-DISPATCH macro

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Grovel an individual case to NUMBER-DISPATCH, augmenting RESULT
;;; with the type dispatches and bodies. Result is a tree built of
;;; alists representing the dispatching off each arg (in order). The
;;; leaf is the body to be executed in that case.
  (defun parse-number-dispatch (vars result types var-types body)
    ;; Shouldn't be necessary, but avoids a warning in certain lisps that
    ;; seem to like to warn about self-calls in :COMPILE-TOPLEVEL situation.
    (named-let parse-number-dispatch ((vars vars) (result result) (types types)
                                      (var-types var-types) (body body))
      (cond ((null vars)
             (unless (null types) (error "More types than vars."))
             (when (cdr result)
               (error "Duplicate case: ~S." body))
             (setf (cdr result)
                   (sublis var-types body :test #'equal)))
            ((null types)
             (error "More vars than types."))
            (t
             (flet ((frob (var type)
                      (parse-number-dispatch
                       (rest vars)
                       (or (assoc type (cdr result) :test #'equal)
                           (car (setf (cdr result)
                                      (acons type nil (cdr result)))))
                       (rest types)
                       (acons `(dispatch-type ,var) type var-types)
                       body)))
               (let ((type (first types))
                     (var (first vars)))
                 (if (and (consp type) (eq (first type) 'foreach))
                     (dolist (type (rest type))
                       (frob var type))
                     (frob var type))))))))

;;; our guess for the preferred order in which to do type tests
;;; (cheaper and/or more probable first.)
  (defconstant-eqx +type-test-ordering+
      '(fixnum single-float double-float integer #+long-float long-float
        sb-vm:signed-word word bignum
        complex ratio)
    #'equal)

;;; Should TYPE1 be tested before TYPE2?
  (defun type-test-order (type1 type2)
    (let ((o1 (position type1 +type-test-ordering+))
          (o2 (position type2 +type-test-ordering+)))
      (cond ((not o1) nil)
            ((not o2) t)
            (t
             (< o1 o2)))))

;;; Return an ETYPECASE form that does the type dispatch, ordering the
;;; cases for efficiency.
;;; Check for some simple to detect problematic cases where the caller
;;; used types that are not disjoint and where this may lead to
;;; unexpected behaviour of the generated form, for example making
;;; a clause unreachable, and throw an error if such a case is found.
;;; An example:
;;;   (number-dispatch ((var1 integer) (var2 float))
;;;     ((fixnum single-float) a)
;;;     ((integer float) b))
;;; Even though the types are not reordered here, the generated form,
;;; basically
;;;   (etypecase var1
;;;     (fixnum (etypecase var2
;;;               (single-float a)))
;;;     (integer (etypecase var2
;;;                (float b))))
;;; would fail at runtime if given var1 fixnum and var2 double-float,
;;; even though the second clause matches this signature. To catch
;;; this earlier than runtime we throw an error already here.
  (defun generate-number-dispatch (vars error-tags cases)
    ;; Shouldn't be necessary, but avoids a warning in certain lisps that
    ;; seem to like to warn about self-calls in :COMPILE-TOPLEVEL situation.
    (named-let generate-number-dispatch ((vars vars) (error-tags error-tags) (cases cases))
      (if vars
          (let ((var (first vars))
                (cases (sort cases #'type-test-order :key #'car)))
            (flet ((error-if-sub-or-supertype (type1 type2)
                     (when (or (subtypep type1 type2)
                               (subtypep type2 type1))
                       (error "Types not disjoint: ~S ~S." type1 type2)))
                   (error-if-supertype (type1 type2)
                     (when (subtypep type2 type1)
                       (error "Type ~S ordered before subtype ~S."
                              type1 type2)))
                   (test-type-pairs (fun)
                     ;; Apply FUN to all (ordered) pairs of types from the
                     ;; cases.
                     (mapl (lambda (cases)
                             (when (cdr cases)
                               (let ((type1 (caar cases)))
                                 (dolist (case (cdr cases))
                                   (funcall fun type1 (car case))))))
                           cases)))
              ;; For the last variable throw an error if a type is followed
              ;; by a subtype, for all other variables additionally if a
              ;; type is followed by a supertype.
              (test-type-pairs (if (cdr vars)
                                   #'error-if-sub-or-supertype
                                   #'error-if-supertype)))
            `((typecase ,var
                ,@(mapcar (lambda (case)
                            `(,(first case)
                              ,@(generate-number-dispatch (rest vars)
                                                          (rest error-tags)
                                                          (cdr case))))
                   cases)
                (t (go ,(first error-tags))))))
          cases)))

  ) ; EVAL-WHEN

;;; This is a vaguely case-like macro that does number cross-product
;;; dispatches. The Vars are the variables we are dispatching off of.
;;; The Type paired with each Var is used in the error message when no
;;; case matches. Each case specifies a Type for each var, and is
;;; executed when that signature holds. A type may be a list
;;; (FOREACH Each-Type*), causing that case to be repeatedly
;;; instantiated for every Each-Type. In the body of each case, any
;;; list of the form (DISPATCH-TYPE Var-Name) is substituted with the
;;; type of that var in that instance of the case.
;;;
;;; [Though it says "_any_ list", it's still an example of how not to perform
;;; incomplete lexical analysis within a macro imho. Let's say that the body
;;; code passes a lambda that happens name its args DISPATCH-TYPE and X.
;;; What happens?
;;; (macroexpand-1 '(number-dispatch ((x number))
;;;                  ((float) (f x (lambda (dispatch-type x) (wat))))))
;;; -> [stuff elided]
;;;      (TYPECASE X (FLOAT (F X (LAMBDA FLOAT (WAT))))
;;;
;;; So the NUMBER-DISPATCH macro indeed substituted for *any* appearance
;;; just like it says. I wonder if we could define DISPATCH-TYPE as macrolet
;;; that expands to the type for the current branch, so that it _must_
;;; be in a for-evaluation position; but maybe I'm missing something?]
;;;
;;; As an alternate to a case spec, there may be a form whose CAR is a
;;; symbol. In this case, we apply the CAR of the form to the CDR and
;;; treat the result of the call as a list of cases. This process is
;;; not applied recursively.
;;;
;;; Be careful when using non-disjoint types in different cases for the
;;; same variable. Some uses will behave as intended, others not, as the
;;; variables are dispatched off sequentially and clauses are reordered
;;; for efficiency. Some, but not all, problematic cases are detected
;;; and lead to a compile time error; see GENERATE-NUMBER-DISPATCH above
;;; for an example.
(defmacro number-dispatch (var-specs &body cases)
  (let ((res (list nil))
        (vars (mapcar #'car var-specs))
        (block (gensym)))
    (dolist (case cases)
      (if (symbolp (first case))
          (let ((cases (apply (symbol-function (first case)) (rest case))))
            (dolist (case cases)
              (parse-number-dispatch vars res (first case) nil (rest case))))
          (parse-number-dispatch vars res (first case) nil (rest case))))

    (collect ((errors)
              (error-tags))
      (dolist (spec var-specs)
        (let ((var (first spec))
              (type (second spec))
              (tag (gensym)))
          (error-tags tag)
          (errors tag)
          (errors
           (sb-c::internal-type-error-call var type))))

      `(block ,block
         (tagbody
            (return-from ,block
              ,@(generate-number-dispatch vars (error-tags)
                                          (cdr res)))
            ,@(errors))))))

;;;; binary operation dispatching utilities

(eval-when (:compile-toplevel :execute)

;;; Return NUMBER-DISPATCH forms for rational X float.
(defun float-contagion (op x y &optional (rat-types '(fixnum bignum ratio)))
  `(((single-float single-float) (,op ,x ,y))
    (((foreach ,@rat-types)
      (foreach single-float double-float #+long-float long-float))
     (,op (coerce ,x '(dispatch-type ,y)) ,y))
    (((foreach single-float double-float #+long-float long-float)
      (foreach ,@rat-types))
     (,op ,x (coerce ,y '(dispatch-type ,x))))
    #+long-float
    (((foreach single-float double-float long-float) long-float)
     (,op (coerce ,x 'long-float) ,y))
    #+long-float
    ((long-float (foreach single-float double-float))
     (,op ,x (coerce ,y 'long-float)))
    (((foreach single-float double-float) double-float)
     (,op (coerce ,x 'double-float) ,y))
    ((double-float single-float)
     (,op ,x (coerce ,y 'double-float)))))

;;; Return NUMBER-DISPATCH forms for bignum X fixnum.
(defun bignum-cross-fixnum (fix-op big-op)
  `(((fixnum fixnum) (,fix-op x y))
    ((fixnum bignum)
     (,big-op (make-small-bignum x) y))
    ((bignum fixnum)
     (,big-op x (make-small-bignum y)))
    ((bignum bignum)
     (,big-op x y))))

) ; EVAL-WHEN
