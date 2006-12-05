(in-package "SB!IMPL")

;;;; generalized function names
(defvar *valid-fun-names-alist* nil)

(defun %define-fun-name-syntax (symbol checker)
  (let ((found (assoc symbol *valid-fun-names-alist* :test #'eq)))
    (if found
        (setf (cdr found) checker)
        (setq *valid-fun-names-alist*
              (acons symbol checker *valid-fun-names-alist*)))))

(defmacro define-function-name-syntax (symbol (var) &body body)
  #!+sb-doc
  "Define function names of the form of a list headed by SYMBOL to be
a legal function name, subject to restrictions imposed by BODY.  BODY
is evaluated with VAR bound to the form required to check, and should
return two values: the first value is a generalized boolean indicating
legality, and the second a symbol for use as a BLOCK name or similar
situations."
  (declare (type symbol symbol))
  (let ((syntax-checker (symbolicate '%check- symbol '-fun-name)))
    `(progn
       (defun ,syntax-checker (,var) ,@body)
       ;; FIXME: is it too expensive to go through a runtime call to
       ;; FDEFINITION each time we want to check a name's syntax?
       (%define-fun-name-syntax ',symbol ',syntax-checker))))

;;; FIXME: this is a really lame name for something that has two
;;; return values.
(defun valid-function-name-p (name)
  #!+sb-doc
  "The primary return value indicates whether NAME is a valid function
name; if it is, the second return value will be a symbol suitable for
use as a BLOCK name in the function in question."
  (typecase name
    (cons
     (when (symbolp (car name))
       (let ((syntax-checker (cdr (assoc (car name) *valid-fun-names-alist*
                                         :test #'eq))))
         (when syntax-checker
           (funcall syntax-checker name)))))
    (symbol (values t name))
    (otherwise nil)))

(define-function-name-syntax setf (name)
  (when (and (cdr name)
             (consp (cdr name)))
    (destructuring-bind (fun &rest rest) (cdr name)
      (when (null rest)
        (typecase fun
          ;; ordinary (SETF FOO) case
          (symbol (values t fun))
          ;; reasonable (SETF (QUUX BAZ)) case [but not (SETF (SETF
          ;; FOO))]
          (cons (unless (eq (car fun) 'setf)
                  (valid-function-name-p fun))))))))

(defun macro-function-name (name)
  (when (and (cdr name)
             (consp (cdr name)))
    (destructuring-bind (fun &rest rest) (cdr name)
      (when (null rest)
        (typecase fun
          ;; (DEFMACRO FOO)
          (symbol (values t fun))
          ;; (DEFMACRO (SETF FOO))
          (cons (when (eq (car fun) 'setf)
                  (valid-function-name-p fun))))))))

(define-function-name-syntax defmacro (name)
  (macro-function-name name))

(define-function-name-syntax macrolet (name)
  (macro-function-name name))

#-sb-xc-host
(defun !function-names-cold-init ()
  (setf *valid-fun-names-alist* '#.*valid-fun-names-alist*))
