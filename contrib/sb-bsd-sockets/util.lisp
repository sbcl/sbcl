(in-package :sb-bsd-sockets)

;;; System call helpers

(declaim (inline interrupted-p))
(defun interrupted-p (errno)
  (or (= errno sockint::EAGAIN) (= errno sockint::EINTR)))

(defmacro syscall-error-case ((form result-var-or-vars test-form errno-form)
                              &optional no-error-form
                              &body clauses)
  (let ((interrupted-case nil)
        (error-case nil)
        (no-error-case `(,no-error-form)))
    (dolist (clause clauses)
      (ecase (first clause)
        (:interrupted
         (setf interrupted-case (rest clause)))
        (:error
         (setf error-case (rest clause)))))
    `(multiple-value-bind ,(sb-int:ensure-list result-var-or-vars) ,form
       (cond
         ,@(when interrupted-case
             `(((and ,test-form (interrupted-p ,errno-form))
                ,@interrupted-case)))
         ,@(when error-case
             `((,test-form
                ,@error-case)))
         ,@(when no-error-case
             `((t ,@no-error-case)))))))

(defmacro socket-error-case ((context form
                              &optional
                              (result-var-or-vars (gensym "RESULT"))
                              (test-form `(= ,(first (sb-int:ensure-list
                                                      result-var-or-vars))
                                             -1))
                              (errno-form `(socket-errno)))
                             &optional no-error-form
                             &body clauses)
  `(syscall-error-case (,form ,result-var-or-vars ,test-form ,errno-form)
       ,no-error-form
     ,@(unless (find :error clauses :key #'first)
         `((:error (socket-error ,context))))
     ,@clauses))

(defmacro addrinfo-error-case ((context form
                                &optional
                                (result-var (gensym "RESULT"))
                                (test-form `(not (zerop ,result-var))))
                             &optional no-error-form
                             &body clauses)
  `(syscall-error-case (,form ,result-var ,test-form nil)
       ,no-error-form
     ,@(unless (find :error clauses :key #'first)
         `((:error (name-service-error ,context ,result-var))))
     ,@clauses))
