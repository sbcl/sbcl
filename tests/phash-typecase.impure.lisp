(load "compiler-test-util.lisp")

;;; Try a 40-way tyepcase on frozen structures
(defglobal *datatypes*
    (loop for i from 1 to 40
          collect
          (list (sb-int:symbolicate "DATA-TYPE-" i)
                (sb-int:symbolicate "DATA-HANDLER<" i ">")
                (format nil "~r" i))))

(defstruct root-structure)
(macrolet ((define-all-structures ()
             `(progn
                ,@(mapcar (lambda (x) `(defstruct (,(car x) (:include root-structure))))
                          *datatypes*))))
  (define-all-structures))
(declaim (freeze-type root-structure))
(macrolet ((define-all-functions ()
             `(progn
                ,@(mapcar (lambda (x)
                            `(defun ,(cadr x) (data)
                               (declare (,(car x) data) (ignore data))
                               ,(caddr x)))
                          *datatypes*))))
  (define-all-functions))
(macrolet ((dispatch (var)
             `(etypecase ,var
                ,@(mapcar (lambda (x) `(,(car x) ',(cadr x))) *datatypes*))))
  (defun process-data (x)
    (funcall (dispatch x) x)))
(compile 'process-data)
(compile 'pd2
 `(lambda (data)
    (funcall (truly-the function
              (etypecase data
                ,@(mapcar (lambda (x) `(,(car x) ,(symbol-function (cadr x))))
                          *datatypes*)))
             data)))

(unless (gethash 'sb-c:jump-table sb-c::*backend-template-names*)
  (invoke-restart 'run-tests::skip-file))

(with-test (:name :phash-typecase-array-of-symbols)
  ;; should compile to an array index, not a jump table.
  ;; The resulting code used to be over 1KiB in size, now it's 180 bytes or so
  (assert (= (sb-kernel:code-jump-table-words (sb-kernel:fun-code-header #'process-data))
             1))
  (assert (loop for c in (ctu:find-code-constants #'process-data)
                thereis (and (simple-vector-p c) (every #'symbolp c))))
  (assert (string= (process-data (make-data-type-3)) "three"))
  (assert-error (process-data (make-root-structure)))
  (assert-error (process-data #p"whatever")))

(with-test (:name :phash-typecase-array-of-functions)
  (assert (= (sb-kernel:code-jump-table-words (sb-kernel:fun-code-header #'pd2))
             1))
  (assert (loop for c in (ctu:find-code-constants #'pd2)
                thereis (and (simple-vector-p c) (every #'functionp c))))
  (assert (string= (pd2 (make-data-type-10)) "ten"))
  (assert-error (pd2 (make-root-structure)))
  (assert-error (pd2 #p"whatever")))
