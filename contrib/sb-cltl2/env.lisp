(in-package :sb-cltl2)

#| TODO:
function-information
declaration-information
augment-environment
define-declaration
(map-environment)
|#

(declaim (ftype (sfunction
                 (symbol &optional (or null lexenv))
                 (values (member nil :special :lexical :symbol-macro :constant)
                         boolean
                         list))
                variable-information))
(defun variable-information (var &optional env)
  "Return three values. The first indicates a binding kind of VAR; the
second is True if there is a local binding of VAR; the third is an
alist of declarations that apply to the apparent binding of VAR."
  (let* ((*lexenv* (or env (make-null-lexenv)))
         (info (lexenv-find var vars)))
    (etypecase info
      (sb-c::leaf (let ((type (type-specifier
                               (type-intersection
                                (sb-c::leaf-type info)
                                (or (lexenv-find info type-restrictions)
                                    *universal-type*)))))
                    (etypecase info
                      (sb-c::lambda-var
                       (values :lexical t
                               `((ignore . ,(sb-c::lambda-var-ignorep info))
                                 (type . ,type))))
                      (sb-c::global-var
                       (values :special t
                               `((type . ,type)) ; XXX ignore
                               ))
                      (sb-c::constant
                       (values :constant nil
                               `((type . ,type)) ; XXX ignore
                               )))))
      (cons (values :symbol-macro t
                    nil                 ; FIXME: also in the compiler
                    ))
      (null (values (ecase (info :variable :kind var)
                      (:special :special)
                      (:constant :constant)
                      (:macro :symbol-macro)
                      (:global nil))
                    nil
                    `(                  ; XXX ignore
                      (type . ,(type-specifier ; XXX local type
                                (info :variable :type var)))))))))

(declaim (ftype (sfunction (symbol &optional (or null lexenv)) t)
                declaration-information))
(defun declaration-information (declaration-name &optional env)
  (let ((policy (sb-c::lexenv-policy (or env (make-null-lexenv)))))
    (case declaration-name
      (optimize (collect ((res))
                  (dolist (name sb-c::*policy-qualities*)
                    (res (list name (cdr (assoc name policy)))))
                  (loop for (name . nil) in sb-c::*policy-dependent-qualities*
                        do (res (list name (sb-c::policy-quality policy name))))
                  (res)))
      (t (error "Unsupported declaration ~S." declaration-name)))))

(defun parse-macro (name lambda-list body &optional env)
  (declare (ignore env))
  (with-unique-names (whole environment)
    (multiple-value-bind (body decls)
        (parse-defmacro lambda-list whole body name
                        'parse-macro
                        :environment environment)
      `(lambda (,whole ,environment)
         ,@decls
         ,body))))

(defun enclose (lambda-expression &optional env)
  (let ((env (if env
                 (sb-c::make-restricted-lexenv env)
                 (make-null-lexenv))))
    (compile-in-lexenv nil lambda-expression env)))
