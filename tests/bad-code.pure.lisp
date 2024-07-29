(enable-test-parallelism)

(with-test (:name (make-array :bad-initial-contents))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (make-array '(1) :initial-contents 'foo))
               :allow-warnings t))))

(with-test (:name (make-string-output-stream :bad-element-type))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-string-output-stream :element-type '((x))))
               :allow-warnings t))))

(with-test (:name (coerce :bad-type-specifier))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (coerce (list 2) 1))
               :allow-warnings t))))

(with-test (:name :zombie-entry-point-reference)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (labels ((%f ())) (%f #'%f)))
               :allow-warnings t))))
(with-test (:name :ir1-optimize-combination-dead-node)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (flet ((%f2 (x) x))
                    (list (%f2 1)
                          (multiple-value-call #'%f2 (values)))))
               :allow-warnings t))))

(with-test (:name (:bogus-block &key))
  (assert
   (nth-value 1
              (checked-compile `(lambda (&key (x (block 1 10))) x)
                               :allow-failure t))))

(with-test (:name :type-error-reporting)
  (assert
   (nth-value 1
              (checked-compile `(lambda ()
                                  (lambda ()
                                    (let ((v3 0))
                                      (cdr (1- (block b5 (if nil v3 0)))))))
                               :allow-warnings t))))

(with-test (:name :dx-on-deleted-nodes)
  (assert
   (nth-value 1
              (checked-compile `(lambda ()
                                  (restart-bind ((1 3))))
                               :allow-warnings t))))

(with-test (:name :transform-call-dfo-consistency)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (flet ((%f (&optional x) x))
                    (%f)
                    ;; Two of the %f calls are erroneous, with an extra argument
                    (flet ((%f6 (&key (k (%f (%f -1 (%f -2 -3))))) 0))
                      5)))
               :allow-warnings t))))

(with-test (:name :&aux-check-variable-names)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (&aux (nil 10))
                  nil)
               :allow-failure t))))

(with-test (:name :mv-call-too-many-values)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (a)
                  (flet ((%f1 (x) x))
                    (apply #'%f1 a 2 (list 0))))
               :allow-warnings t))))

(with-test (:name :mv-call-too-many-values.closure)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (a b)
                  (flet ((%f1 () b))
                    (apply #'%f1 a 2 (list 0))))
               :allow-warnings t))))

(with-test (:name (map :values-type))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (map '* #'+ #(1) #(2)))
               :allow-warnings t))))


(with-test (:name :bad-type-specifier)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array 10 :element-type '((x))))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(x)))
               :allow-warnings t)))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(-10)))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions.2))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(0 . 2)))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions.3))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(0 . 2)
                              :element-type 'fixnum
                              :adjustable t))
               :allow-warnings t))))

(with-test (:name (make-array :bad-dimensions.4))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array (list 'x)))
               :allow-warnings t))))

(with-test (:name (make-array :initial-contents :bad-macro))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array '(10) :initial-contents (do)))
               :allow-failure t))))

(with-test (:name (make-array :dimensions :bad-macro))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (make-array (do)))
               :allow-failure t))))

(with-test (:name (make-array :dimensions :bad-propagated-value))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda ()
                  (let ((x '(("foo"))))
                    (make-array (list x) :fill-pointer 0)))
               :allow-warnings t))))

(with-test (:name (make-array :dimensions :unraveling-list))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (x)
                  (make-array (list (list 10)) :adjustable x))
               :allow-warnings t))))

(with-test (:name :&rest-ref-bad-n)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (&rest a) (lambda () (nth nil a)))
               :allow-warnings t))))

(with-test (:name :bad-type-specifier-handling)
  (multiple-value-bind (fun failure warnings)
      (checked-compile
       `(lambda (v) (typep v '(unsigned-byte 8 x (error ~s v))))
       :allow-warnings t)
    (declare (ignore fun))
    (assert failure)
    (mapcar #'princ-to-string warnings)))

(with-test (:name :ldb-transform-macroexpand)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (ldb (do) 0))
               :allow-failure t))))

(with-test (:name :bad-values-ftype)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (declare (values 0)))
               :allow-warnings t))))

(with-test (:name :bad-progv)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (x) (progv x 1))
               :allow-warnings t)))
  (assert
   (nth-value 1
              (checked-compile
               `(lambda (x) (progv 1 x))
               :allow-warnings t))))

(with-test (:name :coerce-to-nil)
  (assert
   (nth-value 1
              (checked-compile
               '(lambda () (coerce (list t) nil))
               :allow-warnings t))))

(with-test (:name :unknown-vector-type-conflict)
  (assert
   (nth-value 1
              (checked-compile
               '(lambda () (the (vector nonsense-type) nil))
               :allow-warnings t
               :allow-style-warnings t))))

(with-test (:name :subseq-unknown-vector-type)
  (assert
   (nth-value 1
              (checked-compile
               '(lambda () (subseq (the (vector nonsense-type) :x) 0 1))
               :allow-warnings t
               :allow-style-warnings t))))
(with-test (:name :derive-node-type-unknown-type)
  (assert
   (nth-value 3
              (checked-compile
               '(lambda (x)
                 (let ((k (make-array 8 :element-type '(unsigned-byte 8))))
                   (setf (aref k 0) (the unknown-type (the integer x)))
                   (setf k (subseq "y" 0))))
               :allow-warnings t
               :allow-style-warnings t))))

(with-test (:name :highly-nested-type-error)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (macrolet ((macro ()
                                      `((lambda (x)
                                          (declare (number x))
                                          ',@ (loop for cons = (list 1) then (list cons)
                                                    repeat 10000
                                                    finally (return cons)))
                                        t)))
                           (macro)))
                      :allow-warnings t))))

(with-test (:name :complex-member-type)
  (assert (= (length (nth-value 2
                                (checked-compile
                                 `(lambda (x)
                                    (typep x '(complex (eql t))))
                                 :allow-warnings t)))
             1)))

(with-test (:name :bad-optionals)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (z)
                        (lambda (&optional (a nil x))
                          (declare (type integer x))
                          z))
                      :allow-warnings t))))

(with-test (:name :recursive-delete-lambda)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (flet ((%f ()
                                 (lambda ())))
                          (%f :a)
                          (%f :b)))
                      :allow-warnings t)))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (flet ((%f ()
                                 (lambda (&optional m) m)))
                          (%f :a)
                          (%f :b)))
                      :allow-warnings t))))

(with-test (:name :complex-number)
  (checked-compile-and-assert
   ()
   '(lambda (x)
     (typep 1 x))
   (('(complex number)) (condition 'error))))

(with-test (:name :aref-type-mismatch)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda (x)
                         (svref x *break-on-signals*))
                      :allow-warnings t))))

(with-test (:name :unknown-keys-propagation-error-checking.1)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda (x)
                         (let ((a :tests))
                           (find 1 x a #'eql)))
                      :allow-warnings t))))

(with-test (:name :unknown-keys-propagation-error-checking.2)
  (assert (nth-value 1
                     (checked-compile
                      `(lambda ()
                         (apply 'find '(3 (1 2 3) :bad t)))
                      :allow-warnings t))))


(with-test (:name :sequence-lvar-dimensions-dotted-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda () (position 0 '(1 2 0 5 . 5)))
                      :allow-warnings t))))

(with-test (:name :source-form-context-dotted-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (y) `(defines ,@ (and x) . ,y))
                      :allow-warnings t))))

(with-test (:name :typep-transform-dotted-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (typep x (quote . z)))
                      :allow-failure t))))

(with-test (:name :member-transform-dotted-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (member x '(a . b)))
                      :allow-warnings t))))

(with-test (:name :encode-universal-time)
  (assert (nth-value 3
                     (checked-compile
                      '(lambda () (encode-universal-time 0 0 0 1 1 1900 -1))
                      :allow-style-warnings t))))

(with-test (:name :search-transform-bad-index)
  (checked-compile
   '(lambda (a)
     (search '(0 1 0 2) a :start1 4 :end1 5))))

(with-test (:name :bound-mismatch-union-types)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x)
                        (declare ((or (simple-string 10) (simple-string 15)) x))
                        (aref x 100))
                      :allow-warnings t))))

(with-test (:name :uses-with-bad-types)
  (assert (nth-value 3
                     (checked-compile
                      '(lambda (x)
                        (the integer (if x 10)))
                      :allow-style-warnings t))))

(with-test (:name :constant-modification-local-function)
  (assert (= (length (nth-value 2
                                (checked-compile
                                 '(lambda ()
                                   (flet ((z (a)
                                            (setf (aref a 0) 10)))
                                     (z #(10))
                                     (z #(a))))
                                 :allow-warnings t)))
             2)))

(with-test (:name :improper-list)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (concatenate 'string x '(#\a . #\b)))
                      :allow-warnings t)))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (concatenate 'list x '(1 2 . 3)))
                      :allow-warnings t)))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x) (concatenate 'vector x '(1 2 . 3)))
                      :allow-warnings t))))

(with-test (:name :improper-list.2)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (member-if #'(lambda (x) (evenp x)) '(1 2 3 . 4)))
                      :allow-warnings t)))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (x)
                        (search '(a . b) x))
                      :allow-warnings t))))

(with-test (:name :improper-list.3)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                          (let ((x '(1 2 . 3)))
                            (position c x)))
                      :allow-warnings t))))

(with-test (:name :call-nil)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (funcall nil))
    (() (condition 'undefined-function)))
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (if x
             10
             (funcall x)))
    ((nil) (condition 'undefined-function))))

(with-test (:name (:valid-callable-argument :toplevel-xep))
  (assert (nth-value 2 (checked-compile `(lambda (l) (find-if (lambda ()) l))
                                        :allow-warnings t))))

(with-test (:name (:valid-callable-argument :handler-bind))
  (assert (nth-value 2 (checked-compile
                        `(lambda (l) (handler-bind ((error (lambda ()))) (funcall l)))
                        :allow-warnings t))))

(with-test (:name (:valid-callable-argument :closure))
  (assert (nth-value 2 (checked-compile
                        `(lambda (l) (the (function (t)) (lambda () l)))
                        :allow-warnings t))))

(with-test (:name :bad-macros)
  (assert
   (nth-value 1
              (checked-compile
               `(lambda () (coerce 'integer (restart-bind foo)))
               :allow-failure t))))

(with-test (:name :bad-funcall-macros)
  (assert
   (nth-value 1
              (checked-compile
              `(lambda () (funcall (lambda)))
               :allow-failure t))))

(with-test (:name :inlining-bad-code)
  (assert
   (nth-value 2
              (checked-compile
               `(lambda (x &rest args)
                  (unless
                      (if (eq x :tud)
                          (zerop (first args))
                          (every #'identity args (every #'identity args)))
                    args))
               :allow-style-warnings t
               :allow-warnings t))))

(with-test (:name :keyword-type-checking)
  (assert
   (nth-value 2
              (checked-compile
               `(lambda (x)
                  (make-array 10 (list x) x))
               :allow-warnings t))))

(with-test (:name :unused-local-functions)
  (labels ((find-note (x)
             (loop for note in (nth-value 4 (checked-compile x))
                   thereis (and (typep note 'sb-ext:code-deletion-note)
                                (eql (search "deleting unused function"
                                             (princ-to-string note))
                                     0))))
           (check (f)
             (assert (find-note `(lambda () (flet (,f)))))
             (assert (not (find-note `(lambda (x)
                                        (flet (,f)
                                          (and x (not x) (f)))))))))
    (check '(f ()))
    (check '(f (&key)))
    (check '(f (&key k) k))
    (check '(f (&rest args) args))
    (check '(f (&optional o) o))
    (check '(f (&optional)))))

(with-test (:name :calling-ignored-local)
  (assert
   (nth-value 3
              (checked-compile
               `(lambda ()
                  (flet ((f ()))
                    (declare (ignore #'f))
                    (f)))
               :allow-style-warnings t))))

(with-test (:name :inappropriate-declare)
  (assert
   (nth-value 5
              (checked-compile
               `(lambda (x y) (print-unreadable-object (x y) (declare (optimize))))
               :allow-failure t)))
  (assert
   (nth-value 5
              (checked-compile
               `(lambda () (restart-bind () (declare (optimize)) 42))
               :allow-failure t)))
  (assert
   (nth-value 5
              (checked-compile
               `(lambda () (prog1 10 (declare (optimize))))
               :allow-failure t))))

(with-test (:name :reduce-initial-value)
  (assert
   (nth-value 2
              (checked-compile
               `(lambda ()
                  (reduce (lambda (x y)
                            (declare (fixnum x))
                            (+ x (char-code y)))
                          "abc"))
               :allow-warnings t)))
  (assert
   (nth-value 2
              (checked-compile
               `(lambda ()
                  (reduce (lambda (x y)
                            (declare (fixnum x))
                            (+ x (char-code y)))
                          "abc"
                          :initial-value #\a))
               :allow-warnings t)))
  (checked-compile-and-assert
      ()
      `(lambda (s)
         (declare (string s))
         (reduce (lambda (x y)
                   (declare (fixnum x))
                   (+ x (char-code y)))
                 s
                 :initial-value 0))
    (("abc") 294)))

(with-test (:name :reduce-initial-value-from-end)
  (checked-compile-and-assert
   ()
   `(lambda (s)
      (reduce #'funcall s :from-end t :initial-value '(1)))
   (('(car)) 1))
  (checked-compile-and-assert
   ()
   `(lambda (s e)
      (reduce #'funcall s :from-end e :initial-value '(1)))
   (('(car) t) 1))
  (checked-compile-and-assert
   ()
   `(lambda (e)
      (reduce #'funcall #*1 :from-end e :initial-value 1 :key (lambda (x) x 'list)))
   ((t) '(1) :test #'equal))
  (checked-compile-and-assert
   ()
   `(lambda ()
      (reduce #'funcall #*1 :from-end t :initial-value 1 :key (lambda (x) x 'list)))
   (() '(1) :test #'equal))
  (checked-compile-and-assert
   ()
   `(lambda (f l)
      (reduce (the (function ((unsigned-byte 16) (unsigned-byte 8))) f) l
              :initial-value 300))
   ((#'+ '(1 2 3)) 306)))

(with-test (:name :get-defined-fun-lambda-list-error)
  (assert (nth-value 1 (checked-compile '(lambda () (defun x 10)) :allow-failure t))))

(with-test (:name :dolist-mismatch)
  (assert (nth-value 2
                     (checked-compile '(lambda (x)
                                        (dolist (x (the integer x))))
                                      :allow-warnings 'sb-int:type-warning))))

(with-test (:name :loop-list-mismatch)
  (assert (nth-value 2
                     (checked-compile '(lambda (x)
                                        (loop for y in (the integer x)))
                                      :allow-warnings 'sb-int:type-warning)))
  (assert (nth-value 2
                     (checked-compile '(lambda (x)
                                        (loop for y on (the integer x)))
                                      :allow-warnings 'sb-int:type-warning))))

(with-test (:name :mapcar-list-mismatch)
  (assert (nth-value 2
                     (checked-compile '(lambda (z)
                                        (mapl #'car  (the integer z)))
                                      :allow-warnings 'sb-int:type-warning)))
  (assert (nth-value 2
                     (checked-compile '(lambda (f z x)
                                        (mapcar f  (the integer z) (the integer x)))
                                      :allow-warnings 'sb-int:type-warning))))

(with-test (:name :aref-too-many-subscripts)
  (assert (nth-value 2
                     (checked-compile `(lambda (a) (aref a ,@(loop repeat array-rank-limit collect 0)))
                                      :allow-warnings 'warning))))

(with-test (:name :defclass-bad-type)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda () (defclass ,(gensym) () ((s :type (2)))))
                      :allow-warnings 'warning))))

(with-test (:name :macro-as-a-function)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x) (find-if 'and x))
                      :allow-warnings 'warning)))
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x) (funcall 'if x))
                      :allow-warnings 'warning)))
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x) (mapcar 'and x))
                      :allow-warnings 'warning))))

(with-test (:name :replace-type-mismatch)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x y)
                         (declare (bit-vector x)
                                  (string y))
                         (replace x y :start1 10))
                      :allow-warnings 'warning))))

(with-test (:name :substitute-type-mismatch)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x)
                         (declare (string x))
                         (substitute 10 #\a x))
                      :allow-warnings 'warning))))

(with-test (:name :make-array-initial-contents-type-mismatch)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (n c)
                         (make-array n :element-type 'bit :initial-contents (the string c)))
                      :allow-warnings 'warning))))

(with-test (:name :make-array-initial-contents-constant-type-mismatch)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (n)
                         (make-array n :element-type 'bit :initial-contents '(a b c)))
                      :allow-warnings 'warning))))


(with-test (:name :replace-constant-type-mismatch)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x)
                         (declare (string x))
                         (replace x '(1 2 3)))
                      :allow-warnings 'warning))))

(with-test (:name :fill-type-mismatch)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x)
                         (declare (string x))
                         (fill x 1))
                      :allow-warnings 'warning))))

(with-test (:name :vector-push-type-mismatch)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x)
                         (declare (string x))
                         (vector-push 1 x))
                      :allow-warnings 'warning)))
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (x)
                         (declare (string x))
                         (vector-push-extend 1 x))
                      :allow-warnings 'warning))))

(with-test (:name :defmethod-malformed-let*)
  (assert (nth-value 5
                     (checked-compile
                      `(lambda ()
                         (cl:defmethod ,(gensym) ()
                           (let* ((a 1 2))
                             a)))
                      :allow-failure t))))

(with-test (:name :position-derive-empty-type)
  (multiple-value-bind (fun failure warning)
      (checked-compile
       `(lambda (s)
          (position #\a (the simple-string s) :start 4 :end 2))
       :allow-warnings t)
    (declare (ignore failure))
    (assert warning)
    (assert-error (funcall fun "abcdef") sb-kernel:bounding-indices-bad-error)))

(with-test (:name :cast-movement-empty-types)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda ()
                         (loop for x to 2
                               sum (the cons (signum x))))
                      :allow-warnings 'warning))))

(with-test (:name :dead-code-after-ir1-conversion)
  (assert (nth-value 5
                     (checked-compile
                      `(lambda (r v)
                         (labels ((scan (ch l)
                                    (finish l)
                                    (let ((d (digit-char-p ch r)))
                                      (labels ((fix (x i l)
                                                 (if (null l)
                                                     (scan 1
                                                           (cons (cons x i) nil))
                                                     (if i
                                                         (fix (+ (* 2 (aref v i)) x) (1+ i) nil)
                                                         (scan 1
                                                               (cons (cons x i) l))))))
                                        (fix d 0 l))))
                                  (finish (nil)))))
                      :allow-failure t))))

(with-test (:name :muffle-unknown-type)
  (assert (nth-value 3
                     (checked-compile
                      `(lambda () (declare (sb-ext:muffle-conditions foo)) nil)
                      :allow-style-warnings t))))

(with-test (:name :format-char)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda ()
                         (format t "~c" 1))
                      :allow-warnings t))))

(with-test (:name :format-r)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda ()
                         (format t "~r" t))
                      :allow-warnings t))))

(with-test (:name :multiple-uses-funargs)
  (assert (nth-value 3
                     (checked-compile
                      `(lambda (x f)
                         (sort x
                               (or f
                                   (lambda (x)
                                     (< x 0)))))
                      :allow-style-warnings t))))

(with-test (:name :see-through-mv-let+values)
  (assert (nth-value 3
                     (checked-compile
                      `(lambda (x f)
                         (multiple-value-bind (f key)
                             (if f
                                 (values f #'car)
                                 (values #'1+ #'cdr))
                           (sort x f :key key)))
                      :allow-style-warnings t))))

(with-test (:name :member-bad-test)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (m y)
                         (member m y :test #'= :key #'symbol-name))
                      :allow-warnings t))))
