(in-package :sb-cltl2)

#| TODO:
function-information
declaration-information
augment-environment
define-declaration
(map-environment)
|#

(declaim (ftype (sfunction
                 (symbol &optional (or null sb-kernel:lexenv))
                 (values (member nil :special :lexical :symbol-macro :constant)
                         boolean
                         list))
                variable-information))
(defun variable-information (var &optional env)
  (let* ((*lexenv* (or env (sb-kernel:make-null-lexenv)))
         (info (lexenv-find var vars)))
    (etypecase info
      (sb-c::leaf (let ((type (sb-kernel:type-specifier
                               (sb-kernel:type-intersection
                                (sb-c::leaf-type info)
                                (or (lexenv-find info type-restrictions)
                                    sb-kernel:*universal-type*)))))
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
                      (type . ,(sb-kernel:type-specifier ; XXX local type
                                (info :variable :type var)))))))))

(defun parse-macro (name lambda-list body
                    &optional env)
  (declare (ignore env))
  (with-unique-names (whole environment)
    (multiple-value-bind (body decls)
        (sb-kernel:parse-defmacro lambda-list whole body name
                                  'parse-macro
                                  :environment environment)
      `(lambda (,whole ,environment)
         ,@decls
         ,body))))

(defun enclose (lambda-expression
                &optional env)
  (let ((env (if env
                 (sb-c::make-restricted-lexenv env)
                 (sb-kernel:make-null-lexenv))))
    (compile-in-lexenv nil lambda-expression env)))
