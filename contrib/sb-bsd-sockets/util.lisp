(in-package :sb-bsd-sockets)

;;; System call helpers

(defun interrupted-p (errno)
  (member errno `(,sockint::EAGAIN ,sockint::EINTR) :test #'=))

(defmacro syscall-error-case ((form result-var test-form errno-form)
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
    `(let ((,result-var ,form))
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
                              (result-var (gensym "RESULT"))
                              (test-form `(= ,result-var -1))
                              (errno-form `(socket-errno)))
                             &optional no-error-form
                             &body clauses)
  `(syscall-error-case (,form ,result-var ,test-form ,errno-form)
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
         `((:error (addrinfo-error ,context ,result-var))))
     ,@clauses))
