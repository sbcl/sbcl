;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defun constraint-var (thing)
  (if (vector-length-constraint-p thing)
      (vector-length-constraint-var thing)
      thing))

(defun find-equality-constraint (operator x y not-p)
  (let ((constraints (lambda-var-equality-constraints (constraint-var x))))
    (flet ((constraints-eq-p (a b)
             (or (eq a b)
                 (and (vector-length-constraint-p a)
                      (vector-length-constraint-p b)
                      (eq (vector-length-constraint-var a)
                          (vector-length-constraint-var b))))))
     (when constraints
       (loop for con across constraints
             when (and (eq (equality-constraint-operator con) operator)
                       (eq (constraint-not-p con) not-p)
                       (constraints-eq-p (constraint-x con) x)
                       (constraints-eq-p (constraint-y con) y))
             return con)))))

(defun invert-operator (operator)
  (case operator
    (< '>)
    (> '<)
    (<= '>=)
    (>= '<=)
    (t operator)))

(defun find-or-create-equality-constraint (operator x y not-p)
  (or (find-equality-constraint operator x y not-p)
      (let ((new (make-equality-constraint (length *constraint-universe*)
                                           operator
                                           x y not-p)))
        (vector-push-extend new *constraint-universe* (1+ (length *constraint-universe*)))
        (flet ((add (var)
                 (conset-adjoin new (lambda-var-constraints var))
                 (macrolet ((ensure-vec (place)
                              `(or ,place
                                   (setf ,place
                                         (make-array 8 :adjustable t :fill-pointer 0)))))
                   (vector-push-extend new (ensure-vec (lambda-var-equality-constraints var))))))
          (add (constraint-var x))
          (let ((y (constraint-var y)))
            (when (lambda-var-p y)
              (add y))))
        new)))

(defun add-eq-constraint (var lvar gen)
  (let ((var2 (ok-lvar-lambda-var lvar gen)))
    (when var2
      (conset-adjoin (find-or-create-equality-constraint 'eq var var2 nil) gen))))

(defun vector-length-var-p (lvar constraints)
  (let* ((use (principal-lvar-ref-use lvar))
         (array-lvar
           (and (combination-p use)
                (lvar-fun-is (combination-fun use)
                             '(vector-length length))
                (car (combination-args use))))
         (array-var (and array-lvar
                         (ok-lvar-lambda-var array-lvar constraints))))
    (and array-var
         (make-vector-length-constraint array-var))))

(defun conset-add-equality-constraint (conset operator x y not-p)
  (conset-adjoin (find-or-create-equality-constraint operator x y not-p)
                 conset))

(defun vector-constraint-eq-p (con1 con2)
  (and (vector-length-constraint-p con1)
       (vector-length-constraint-p con2)
       (eq (vector-length-constraint-var con1)
           (vector-length-constraint-var con2))))

(defun add-equality-constraints (operator args constraints
                                 consequent-constraints
                                 alternative-constraints)
  (case operator
    ((eq eql char= two-arg-char-equal
      > < = <= >=)
     (when (= (length args) 2)
       (flet ((ok-lvar-p (lvar)
                (or (vector-length-var-p lvar constraints)
                    (ok-lvar-lambda-var lvar constraints))))
         (let* ((first (first args))
                (second (second args))
                (constant-x (constant-lvar-p first))
                (constant-y (constant-lvar-p second))
                (x (if constant-x
                       (nth-value 1 (lvar-value first))
                       (ok-lvar-p first)))
                (y (if constant-y
                       (nth-value 1 (lvar-value second))
                       (ok-lvar-p second)))
                (x-type (lvar-type first))
                (y-type (lvar-type second)))
           (constraint-propagate-back first operator second
                                      constraints
                                      consequent-constraints
                                      alternative-constraints)
           (flet ((invert-operator ()
                    (invert-operator operator)))
            (when constant-x
              (when constant-y
                (return-from add-equality-constraints))
              (rotatef x y)
              (rotatef x-type y-type)
              (rotatef constant-x constant-y)
              (setf operator (invert-operator)))
            (when (and x y)
              ;; TODO: inherit more than just EQL constraints
              (flet ((replace-var (var with)
                       (cond ((eq var with)
                              var)
                             ((vector-length-constraint-p var)
                              (make-vector-length-constraint with))
                             (t
                              with)))
                     (add (x y)
                       (let ((operator operator))
                         (when (ctype-p x)
                           (rotatef x y)
                           (setf operator (invert-operator)))
                         (conset-add-equality-constraint consequent-constraints operator x y nil)
                         (when alternative-constraints
                           (conset-add-equality-constraint alternative-constraints operator x y t)))))
                (do-eql-vars (eql-x ((constraint-var x) constraints))
                  (let ((x (replace-var x eql-x)))
                    (add x y)
                    (when (and (vector-length-constraint-p x)
                               (not constant-y)
                               (neq y-type *universal-type*))
                      (add x y-type))))
                (if constant-y
                    (add x y)
                    (do-eql-vars (eql-y ((constraint-var y) constraints))
                      (let ((y (replace-var y eql-y)))
                        (add x y)
                        (when (and (vector-length-constraint-p y)
                                   (not constant-x)
                                   (neq x-type *universal-type*))
                          (add x-type y)))))
                (cond ((vector-length-constraint-p x)
                       (unless (vector-length-constraint-p y)
                         (do-conset-constraints-intersection
                             (con (constraints (lambda-var-equality-constraints (constraint-var x))))
                           (let ((con-y (if (vector-constraint-eq-p (constraint-x con) x)
                                            (constraint-y con)
                                            (constraint-x con))))
                             (when (and (vector-length-constraint-p con-y)
                                        (eq (equality-constraint-operator con) 'eq))
                               (add con-y y)
                               (add con-y y-type))))))
                      ((vector-length-constraint-p y)
                       (do-conset-constraints-intersection
                           (con (constraints (lambda-var-equality-constraints (constraint-var y))))
                         (let ((con-x (if (vector-constraint-eq-p (constraint-y con) y)
                                          (constraint-x con)
                                          (constraint-y con))))
                           (when (and (vector-length-constraint-p con-x)
                                      (eq (equality-constraint-operator con) 'eq))
                             (add x con-x)
                             (add x-type con-x)))))))))))))))

(defun inherit-equality-constraints (vars from-var constraints target)
  (do-conset-constraints-intersection
      (con (constraints (lambda-var-equality-constraints from-var)))
    (let ((replace-x (if (eq from-var (constraint-var (constraint-x con)))
                         t
                         (aver (eq from-var (constraint-var (constraint-y con)))))))
      (dolist (var vars)
        (flet ((replace-var (var with)
                 (if (vector-length-constraint-p var)
                     (make-vector-length-constraint with)
                     with)))
         (conset-adjoin
          (find-or-create-equality-constraint (equality-constraint-operator con)
                                              (if replace-x
                                                  (replace-var (constraint-x con) var)
                                                  (constraint-x con))
                                              (if replace-x
                                                  (constraint-y con)
                                                  (replace-var (constraint-y con) var))
                                              (constraint-not-p con))
          target))))))


(defun find-ref-equality-constraint (operator lvar1 lvar2 &optional (commutative t))
  (flet ((ref (lvar)
           (let ((use (principal-lvar-use lvar)))
             (cond ((ref-p use)
                    (if (constant-lvar-p lvar)
                        (values (nth-value 1 (lvar-value lvar)) nil)
                        (values use nil)))
                   ((and (combination-p use)
                         (lvar-fun-is (combination-fun use)
                                      '(vector-length length)))
                    (let ((arg (car (combination-args use))))
                      (and (csubtypep (lvar-type arg) (specifier-type 'simple-array))
                           (let ((use (principal-lvar-use arg)))
                             (and (ref-p use)
                                  (values use t)))))))))
         (constraint-eq-p (ref leaf vector-length-p x)
           (cond (vector-length-p
                  (and (vector-length-constraint-p x)
                       (eq (vector-length-constraint-var x) leaf)))
                 ((eq leaf x))
                 ((and (lvar-p x)
                       (ref-p ref))
                  (eq (principal-lvar-ref-use (node-lvar ref))
                      (lvar-uses x))))))
    (multiple-value-bind (ref1 vector-length-p1) (ref lvar1)
      (multiple-value-bind (ref2 vector-length-p2) (ref lvar2)
        (cond ((and (ref-p ref1) ref2))
              ((and (ref-p ref1) (not ref2))
               (setf ref2 lvar2))
              ((and (ref-p ref2) (not ref1))
               (setf ref1 ref2
                     ref2 lvar1
                     operator (invert-operator operator)))
              (t
               (return-from find-ref-equality-constraint)))
        (let ((leaf1 (ref-leaf ref1))
              (leaf2 (cond ((constant-p ref2)
                            ref2)
                           ((lvar-p ref2) ref2)
                           (t
                            (ref-leaf ref2)))))
          (flet ((find-constraint (ref)
                   (loop for con in (ref-constraints ref)
                         when (and (equality-constraint-p con)
                                   (or
                                    (and (eq (equality-constraint-operator con) operator)
                                         (or (and (constraint-eq-p ref1 leaf1 vector-length-p1 (constraint-x con))
                                                  (constraint-eq-p ref2 leaf2 vector-length-p2 (constraint-y con)))
                                             (and commutative
                                                  (constraint-eq-p ref2 leaf2 vector-length-p2 (constraint-x con))
                                                  (constraint-eq-p ref1 leaf1 vector-length-p1 (constraint-y con)))))
                                    (and (eq (equality-constraint-operator con)
                                             (invert-operator operator))
                                         (constraint-eq-p ref2 leaf2 vector-length-p2 (constraint-x con))
                                         (constraint-eq-p ref1 leaf1 vector-length-p1 (constraint-y con)))))
                         return con))
                 (has-sets (leaf)
                   (and (lambda-var-p leaf)
                        (lambda-var-sets leaf))))
            (let ((ref1-con (find-constraint ref1)))
              (cond (ref1-con
                     (when (and
                            ;; If the variables are set both references
                            ;; need to have the same constraint, otherwise
                            ;; one of the references may be done before the
                            ;; set.
                            (or (constant-p leaf2)
                                (lvar-p leaf2)
                                (and (not (has-sets leaf1))
                                     (not (has-sets leaf2)))
                                (eq ref1-con (find-constraint ref2))))
                       ref1-con))
                    ((ref-p ref2)
                     (let ((ref2-con (find-constraint ref2)))
                       (when (and ref2-con
                                  (or (constant-p leaf1)
                                      (lvar-p leaf1)
                                      (and (not (has-sets leaf1))
                                           (not (has-sets leaf2)))))
                         ref2-con)))))))))))

(defun try-equality-constraint (call)
  (let ((constraint (fun-info-equality-constraint (basic-combination-fun-info call)))
        (lvar (node-lvar call)))
    (when (and constraint
               lvar)
      (let ((result (funcall constraint call)))
        (unless (eq result :give-up)
          (replace-combination-with-constant result call)
          t)))))

(defmacro equality-constraint (operator &optional type (commutative t))
  `(let ((constraint (find-ref-equality-constraint ',operator x y ,commutative)))
     ,(ecase type
        ((nil)
         `(when constraint
            (setf result (not (constraint-not-p constraint)))
            t))
        (true
         `(when (and constraint
                     (not (constraint-not-p constraint)))
            (setf result t)))
        (false
         `(when (and constraint
                     (constraint-not-p constraint))
            (setf result nil)
            t))
        (not-true
         `(when (and constraint
                     (constraint-not-p constraint))
            (setf result t)
            t))
        (inverse-true
         `(when (and constraint
                     (not (constraint-not-p constraint)))
            (setf result nil)
            t))
        (inverse
         `(when constraint
            (setf result (constraint-not-p constraint))
            t)))))

(defoptimizer (eql equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint eq true)
     (equality-constraint eql)
     (equality-constraint char=)
     (equality-constraint two-arg-char-equal false)
     (equality-constraint > inverse-true)
     (equality-constraint < inverse-true)
     (equality-constraint = false)
     (equality-constraint <= false)
     (equality-constraint >= false))
    result))

(defoptimizer (eq equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint eq)
     (equality-constraint eql)
     (equality-constraint char=)
     (equality-constraint two-arg-char-equal false)
     (equality-constraint > inverse-true)
     (equality-constraint < inverse-true)
     (equality-constraint = false)
     (equality-constraint <= false)
     (equality-constraint >= false))
    result))

(defoptimizer (= equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint =)
     (equality-constraint eq true)
     (equality-constraint eql true)
     (equality-constraint > inverse-true)
     (equality-constraint < inverse-true)
     (equality-constraint <= false)
     (equality-constraint >= false))
    result))

(defoptimizer (char= equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint char=)
     (equality-constraint eq)
     (equality-constraint eql)
     (equality-constraint two-arg-char-equal false))
    result))

(defoptimizer (two-arg-char-equal equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint two-arg-char-equal)
     (equality-constraint char= true)
     (equality-constraint eq true)
     (equality-constraint eql true))
    result))

(defoptimizer (> equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint > nil nil)
     (equality-constraint < inverse-true nil)
     (equality-constraint eq inverse-true)
     (equality-constraint eql inverse-true)
     (equality-constraint = inverse-true)
     (equality-constraint <= inverse nil)
     (equality-constraint >= false nil))
    result))

(defoptimizer (< equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint < nil nil)
     (equality-constraint > inverse-true nil)
     (equality-constraint eq inverse-true)
     (equality-constraint eql inverse-true)
     (equality-constraint = inverse-true)
     (equality-constraint >= inverse nil)
     (equality-constraint <= false nil))
    result))

(defoptimizer (>= equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint >= nil nil)
     (equality-constraint > true nil)
     (equality-constraint = true)
     (equality-constraint < not-true nil)
     (equality-constraint <= not-true nil))
    result))

(defoptimizer (<= equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint < true nil)
     (equality-constraint <= nil nil)
     (equality-constraint = true)
     (equality-constraint > not-true nil)
     (equality-constraint >= not-true nil))
    result))

(defoptimizer (- equality-constraint) ((x y) node)
  (let ((integer (specifier-type 'integer)))
    (when (and (csubtypep (lvar-type x) integer)
               (csubtypep (lvar-type y) integer))
      (macrolet ((f (op x y pos)
                   `(let ((constr (find-ref-equality-constraint ',op ,x ,y nil)))
                      (when constr
                        (if (constraint-not-p constr)
                            ,@(if pos
                                  `((go GTEZ) (go LTZ))
                                  `((go LTEZ) (go GTZ)))))))
                 (derive (type)
                   `(progn
                      (derive-node-type node (specifier-type ',type))
                      (go DONE))))
        (tagbody
           (f < x y t)
           (f > x y nil)
           (f > y x t)
           (f < y x nil)
           (go DONE)
         GTZ
           (derive (integer (0)))
         LTZ
           (derive (integer * (0)))
         GTEZ
           (derive (integer 0))
         LTEZ
           (derive (integer * 0))
         DONE))))
  :give-up)

(deftransform %check-bound ((array dimension index) ((simple-array * (*)) t t))
  (let ((array-ref (lvar-uses array))
        (index-ref (lvar-uses index)))

    (unless (and
             (ref-p array-ref)
             (ref-p index-ref)
             (or
              (let* ((index-leaf (ref-leaf index-ref))
                     (index-value (and (constant-p index-leaf)
                                       (constant-value index-leaf)))
                     (index-value (and (integerp index-value)
                                       index-value)))
                (flet ((matches-p (operator constraint x-p)
                         (and (eq (equality-constraint-operator constraint) operator)
                              (null (equality-constraint-not-p constraint))
                              (eq (if x-p
                                      (constraint-x constraint)
                                      (constraint-y constraint))
                                  index-leaf))))
                  (or
                   (loop for constraint in (ref-constraints array-ref)
                         for y = (constraint-y constraint)
                         thereis (and
                                  (eq (constraint-kind constraint) 'equality)
                                  (if index-value
                                      (and (eq (equality-constraint-operator constraint) '>)
                                           (null (equality-constraint-not-p constraint))
                                           (constant-p y)
                                           (<= index-value (constant-value y)))
                                      (or
                                       (matches-p '< constraint t)
                                       (matches-p '> constraint nil))))))))))
      (give-up-ir1-transform)))
  ;; It's in bounds but it may be of the wrong type
  `(the (and fixnum unsigned-byte) index))

;;; (ash x -y) <= x
(defoptimizer (ash constraint-propagate) ((x y) node gen)
  (cond ((and (csubtypep (lvar-type x) (specifier-type '(integer 1)))
              (csubtypep (lvar-type y) (specifier-type '(integer * -1))))
         (let ((var (ok-lvar-lambda-var x gen)))
           (when var
             (list (list 'equality '> var (node-lvar node))))))
        ((and (csubtypep (lvar-type x) (specifier-type '(integer 0)))
              (csubtypep (lvar-type y) (specifier-type '(integer * 0))))
         (let ((var (ok-lvar-lambda-var x gen)))
           (when var
             (list (list 'equality '>= var (node-lvar node))))))))

;;; (/ positive-x positive-y) <= positive-x
(defun div-constraints (x y node gen)
  (when (csubtypep (lvar-type y) (specifier-type 'rational))
    (list* (list 'typep (ok-lvar-lambda-var y gen) (specifier-type '(eql 0)) t)
           (cond ((and (csubtypep (lvar-type x) (specifier-type '(integer 1)))
                       (csubtypep (lvar-type y) (specifier-type '(integer 1))))
                  (let ((var (ok-lvar-lambda-var x gen)))
                    (when var
                      (list (list 'equality '> var (node-lvar node))))))
                 ((and (csubtypep (lvar-type x) (specifier-type '(integer 0)))
                       (csubtypep (lvar-type y) (specifier-type '(integer 0))))
                  (let ((var (ok-lvar-lambda-var x gen)))
                    (when var
                      (list (list 'equality '>= var (node-lvar node))))))))))

(defoptimizer (truncate constraint-propagate) ((x y) node gen)
  (div-constraints x y node gen))

(defoptimizer (/ constraint-propagate) ((x y) node gen)
  (div-constraints x y node gen))

(defmacro do-equality-constraints ((con op not-p) var constraints &body body)
  (once-only ((var var)
              (constraints constraints))
    `(do-conset-constraints-intersection
         (,con (,constraints (lambda-var-equality-constraints (constraint-var ,var))))
       (flet ((body (,con ,op ,not-p) ,@body))
         (cond ((eq (constraint-x ,con) ,var)
                (body (constraint-y ,con)
                      (equality-constraint-operator ,con)
                      (equality-constraint-not-p ,con)))
               ((eq (constraint-y ,con) ,var)
                (body (constraint-x ,con)
                      (invert-operator (equality-constraint-operator ,con))
                      (equality-constraint-not-p ,con))))))))

(defoptimizer (+ constraint-propagate) ((x y) node gen)
  (flet ((try (a b)
           (and (csubtypep (lvar-type a) (specifier-type 'integer))
                (csubtypep (lvar-type b) (specifier-type '(integer 0)))
                (let ((var (ok-lvar-lambda-var a gen)))
                  (when var
                    (let ((plusp (csubtypep (lvar-type b) (specifier-type '(integer 1))))
                          r)
                      (push (list 'equality (if plusp
                                                '<
                                                '<=)
                                  var (node-lvar node))
                            r)
                      (do-equality-constraints (con op not-p) var gen
                        (cond
                          ((not (lambda-var-p con)))
                          ((or (and (eq op '>)
                                    (not not-p))
                               (and (eq op '<=)
                                    not-p))
                           (push (list 'equality '< con (node-lvar node))
                                 r))
                          ((or (and (memq op '(>= = eq eql))
                                    (not not-p))
                               (and (eq op '<)
                                    not-p))
                           (push (list 'equality (if plusp
                                                     '<
                                                     '<=)
                                       con (node-lvar node))
                                 r))))
                      r))))))
    (or (try x y)
        (try y x))))
