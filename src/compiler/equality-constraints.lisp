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

(defmacro do-equality-constraints ((con op not-p) var constraints &body body)
  (once-only ((var var)
              (constraints constraints))
    `(do-conset-constraints-intersection
         (,con (,constraints (lambda-var-equality-constraints (constraint-var ,var))))
       (flet ((body (,con ,op ,not-p) ,@body))
         (cond ((vector-constraint-eq-p (constraint-x ,con) ,var)
                (body (constraint-y ,con)
                      (equality-constraint-operator ,con)
                      (equality-constraint-not-p ,con)))
               ((vector-constraint-eq-p (constraint-y ,con) ,var)
                (body (constraint-x ,con)
                      (invert-operator (equality-constraint-operator ,con))
                      (equality-constraint-not-p ,con))))))))

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
  (let* ((use (principal-lvar-use lvar))
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
  (or (eq con1 con2)
      (and (vector-length-constraint-p con1)
           (vector-length-constraint-p con2)
           (eq (vector-length-constraint-var con1)
               (vector-length-constraint-var con2)))))

(defun ok-lvar-lambda-var/vector-length (lvar constraints)
  (or (vector-length-var-p lvar constraints)
      (ok-lvar-lambda-var lvar constraints)))

(defun lambda-var/vector-length-p (x)
  (or (lambda-var-p x)
      (vector-length-constraint-p x)))

(defconstant-eqx +eq-ops+ '(eq eql =) #'equal)

(defun inherit-equality-p (new from not-p)
  (case new
    ((char= two-arg-char-equal . #.+eq-ops+)
     (unless not-p
       from))
    (<
     (if not-p
         (case from
           ((> >=) '<))
         (case from
           ((< <= . #.+eq-ops+) '<))))
    (>
     (if not-p
         (case from
           ((< <=) '>))
         (case from
           ((> >= . #.+eq-ops+) '>))))
    (<=
     (if not-p
         (case from
           ((> >=) '<=))
         (case from
           ((< <= . #.+eq-ops+) '<=))))
    (>=
     (if not-p
         (case from
           ((< <=) '>=))
         (case from
           ((> >= . #.+eq-ops+) '>=))))))

(defun add-equality-constraint (operator first second constraints consequent-constraints alternative-constraints)
  (let* ((constant-x (and (lvar-p first)
                          (constant-lvar-p first)))
         (constant-y (and (lvar-p second)
                          (constant-lvar-p second)))
         (x (cond (constant-x
                   (nth-value 1 (lvar-value first)))
                  ((lambda-var/vector-length-p first)
                   first)
                  ((ok-lvar-lambda-var/vector-length first constraints))
                  (t
                   first)))
         (y (cond (constant-y
                   (nth-value 1 (lvar-value second)))
                  ((lambda-var/vector-length-p second)
                   second)
                  ((ok-lvar-lambda-var/vector-length second constraints))
                  (t
                   second)))
         (x-type (etypecase first
                   (lvar (lvar-type first))
                   (lambda-var (lambda-var-type first))
                   (vector-length-constraint (specifier-type 'index))))
         (y-type (etypecase second
                   (lvar (lvar-type second))
                   (lambda-var (lambda-var-type second))
                   (vector-length-constraint (specifier-type 'index)))))
    (when (lvar-p x)
      (constraint-propagate-back x operator second
                                 constraints
                                 consequent-constraints
                                 alternative-constraints))
    (unless (lambda-var/vector-length-p x)
      (unless (lambda-var/vector-length-p y)
        (return-from add-equality-constraint))
      (rotatef first second)
      (rotatef x y)
      (rotatef x-type y-type)
      (rotatef constant-x constant-y)
      (setf operator (invert-operator operator)))
    (flet ((replace-var (var with)
             (cond ((eq var with)
                    var)
                   ((vector-length-constraint-p var)
                    (make-vector-length-constraint with))
                   (t
                    with)))
           (add (x y &optional (operator operator) (alternative-constraints alternative-constraints))
             (unless (lambda-var/vector-length-p x)
               (unless (lambda-var/vector-length-p y)
                 (return-from add))
               (rotatef x y)
               (setf operator (invert-operator operator)))
             (conset-add-equality-constraint consequent-constraints operator x y nil)
             (when alternative-constraints
               (conset-add-equality-constraint alternative-constraints operator x y t))))
      (do-eql-vars (eql-x ((constraint-var x) constraints))
        (let ((x (replace-var x eql-x)))
          (add x y)
          (when (and (vector-length-constraint-p x)
                     (not constant-y)
                     (neq y-type *universal-type*))
            (add x y-type))))
      (if (lambda-var/vector-length-p y)
          (do-eql-vars (eql-y ((constraint-var y) constraints))
            (let ((y (replace-var y eql-y)))
              (add x y)
              (when (and (vector-length-constraint-p y)
                         (not constant-x)
                         (neq x-type *universal-type*))
                (add x-type y))))
          (add x y))
      (flet ((inherit (x y x-type operator)
               (when (lambda-var/vector-length-p y)
                 (do-equality-constraints (in-y in-op in-not-p) y constraints
                   (unless (eq in-y y)
                     (let ((inherit (inherit-equality-p operator in-op in-not-p)))
                       (when inherit
                         (add x in-y inherit nil)
                         (when (and (vector-length-constraint-p in-y)
                                    (not (constant-p x)))
                           (add x-type in-y inherit nil)))))))))
        (inherit x y x-type operator)
        (inherit y x y-type (invert-operator operator))
        (when (lvar-p y)
          (loop for (in-op in-lvar) in (lvar-result-constraints y)
                do
                (let ((inherit (inherit-equality-p operator in-op nil)))
                  (when inherit
                    (add-equality-constraint inherit x in-lvar
                                             constraints consequent-constraints nil)))))))))

(defun add-equality-constraints (operator args constraints
                                 consequent-constraints
                                 alternative-constraints)
  (case operator
    ((char= two-arg-char-equal > <  <= >=
            . #.+eq-ops+)
     (when (= (length args) 2)
       (add-equality-constraint operator (first args) (second args)
                                constraints consequent-constraints alternative-constraints)))))

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
               (rotatef vector-length-p1 vector-length-p2)
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

(defun lvar-result-constraints (lvar)
  (let ((uses (lvar-uses lvar)))
    (cond ((combination-p uses)
           (binding* ((info (combination-fun-info uses) :exit-if-null)
                      (propagate (fun-info-constraint-propagate-result info)
                                 :exit-if-null))
             (funcall propagate uses)))
          ((proper-list-of-length-p uses 2)
           (let (r)
               ;; Detect MIN/MAX variables.
               (destructuring-bind (ref1 ref2) uses
                 (when (and (ref-p ref1) (ref-p ref2))
                   (let ((var1 (ref-leaf ref1))
                         (var2 (ref-leaf ref2)))
                     (when (and (lambda-var-p var1)
                                (lambda-var-p var2))
                       (labels ((normalize-not (op not)
                                  (if not
                                      (case op
                                        (< '>=)
                                        (> '<=))
                                      op))
                                (relations (x y con)
                                  (cond ((vector-constraint-eq-p (constraint-x con) x)
                                         (and (vector-constraint-eq-p (constraint-y con) y)
                                              (normalize-not (equality-constraint-operator con)
                                                             (equality-constraint-not-p con))))
                                        ((vector-constraint-eq-p (constraint-y con) x)
                                         (and (vector-constraint-eq-p (constraint-x con) y)
                                              (normalize-not (invert-operator (equality-constraint-operator con))
                                                             (equality-constraint-not-p con))))))
                                (find-constraint (var1 var2 ref)
                                  (loop for con in (ref-constraints ref)
                                        do (multiple-value-bind (op not)
                                               (relations var1 var2 con)
                                             (when op
                                               (return (values op not)))))))
                         (let ((op1 (find-constraint var1 var2 ref1))
                               (op2 (find-constraint var2 var1 ref2)))
                           (case op1
                             ((< <=) (and (memq op2 '(<= <))
                                          (push (list '<= var1) r)
                                          (push (list '<= var2) r)))
                             ((> >=) (and (memq op2 '(> >=))
                                          (push (list '>= var1) r)
                                          (push (list '>= var2) r))))))))

                   r)))))))

(defun add-var-result-constraints (var lvar constraints &optional (target constraints))
  (loop for (operator second) in (lvar-result-constraints lvar)
        do
        (add-equality-constraint operator var second constraints target nil))
  (when (lambda-var-p var)
    (let ((vector-length (vector-length-var-p lvar constraints)))
      (when vector-length
        (conset-add-equality-constraint target 'eq var vector-length nil)))))

(defoptimizer (- constraint-propagate-result) ((a b) node)
  (and (csubtypep (lvar-type a) (specifier-type 'integer))
       (csubtypep (lvar-type b) (specifier-type '(integer 0)))
       (let ((plusp (csubtypep (lvar-type b) (specifier-type '(integer 1)))))
         (list (list (if plusp
                         '<
                         '<=)
                     a)))))

(defoptimizer (+ constraint-propagate-result) ((x y) node)
  (let (r)
    (flet ((try (a b)
             (and (csubtypep (lvar-type a) (specifier-type 'integer))
                  (csubtypep (lvar-type b) (specifier-type '(integer 0)))
                  (let ((plusp (csubtypep (lvar-type b) (specifier-type '(integer 1)))))
                    (push (list (if plusp
                                    '>
                                    '>=)
                                a)
                          r)))))
      (try x y)
      (try y x))
    r))

;;; (ash x -y) <= x
(defoptimizer (ash constraint-propagate-result) ((x y) node)
  (cond ((and (csubtypep (lvar-type x) (specifier-type '(integer 1)))
              (csubtypep (lvar-type y) (specifier-type '(integer * -1))))
         (list (list '< x)))
        ((and (csubtypep (lvar-type x) (specifier-type '(integer 0)))
              (csubtypep (lvar-type y) (specifier-type '(integer * 0))))
         (list (list '<= x)))))

;;; (/ positive-x positive-y) <= positive-x
(defun div-constraints (x y)
  (when (csubtypep (lvar-type y) (specifier-type 'rational))
    (cond ((and (csubtypep (lvar-type x) (specifier-type '(integer 1)))
                (csubtypep (lvar-type y) (specifier-type '(integer 1))))
           (list (list '< x)))
          ((and (csubtypep (lvar-type x) (specifier-type '(integer 0)))
                (csubtypep (lvar-type y) (specifier-type '(integer 0))))
           (list (list '<= x))))))

(defoptimizer (truncate constraint-propagate-result) ((x y) node)
  (div-constraints x y))

(defoptimizer (/ constraint-propagate-result) ((x y) node)
  (div-constraints x y))
