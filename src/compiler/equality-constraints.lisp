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

(defun vector-length-var-p (lvar constraints &optional simple)
  (let* ((use (principal-lvar-use lvar))
         (array-lvar
           (and (combination-p use)
                (lvar-fun-is (combination-fun use)
                             '(vector-length length))
                (car (combination-args use))))
         (array-var (and array-lvar
                         (or (not simple)
                             (csubtypep (lvar-type array-lvar) (specifier-type 'simple-array)))
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

(defun ok-lvar-lambda-var/vector-length (lvar constraints &optional simple)
  (or (vector-length-var-p lvar constraints simple)
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
    (when (lvar-p first)
      (constraint-propagate-back first operator second
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

(defun try-equality-constraint (call gen)
  (let ((constraint (fun-info-equality-constraint (basic-combination-fun-info call)))
        (lvar (node-lvar call)))
    (when constraint
      (let ((result (funcall constraint call gen)))
        (unless (eq result :give-up)
          (when lvar
            (replace-combination-with-constant result call))
          t)))))

(defun map-equality-constraints (x y constraints
                                 function)
  (let* ((constant-x (and (lvar-p x)
                          (constant-lvar-p x)))
         (constant-y (and (lvar-p y)
                          (constant-lvar-p y)))
         (x (cond (constant-x
                   (nth-value 1 (lvar-value x)))
                  ((lambda-var/vector-length-p x)
                   x)
                  ((ok-lvar-lambda-var/vector-length x constraints t))
                  (t
                   x)))
         (y (cond (constant-y
                   (nth-value 1 (lvar-value y)))
                  ((ok-lvar-lambda-var/vector-length y constraints t))
                  (t
                   y)))
         (invert))
    (unless (lambda-var/vector-length-p x)
      (unless (lambda-var/vector-length-p y)
        (return-from map-equality-constraints))

      (rotatef x y)
      (rotatef constant-x constant-y)
      (setf invert t))
    (do-equality-constraints (in-y op not-p) x constraints
      (when (or (vector-constraint-eq-p in-y y)
                (and constant-y
                     (constant-p in-y)
                     (let ((a (constant-value in-y))
                           (b (constant-value y)))
                       (and (realp a)
                            (realp b)
                            (case op
                              (< (unless not-p
                                   (sb-xc:< a b)))
                              (> (unless not-p
                                   (sb-xc:> a b))))))))
        (funcall function (if invert
                              (invert-operator op)
                              op)
                 not-p)))))

(defoptimizer (eql equality-constraint) ((x y) node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  ((eql char=)
                                   (return (not not-p)))
                                  (eq
                                   (unless not-p
                                     (return t)))
                                  ((> <)
                                   (unless not-p
                                     (return nil)))
                                  ((= <= >= two-arg-char-equal)
                                   (when not-p
                                     (return nil))))))
    :give-up))

(defoptimizer (eq equality-constraint) ((x y) node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  ((eq eql char=)
                                   (return (not not-p)))
                                  ((> <)
                                   (unless not-p
                                     (return nil)))
                                  ((= <= >= two-arg-char-equal)
                                   (when not-p
                                     (return nil))))))
    :give-up))

(defoptimizer (equal equality-constraint) ((x y) node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  ((eq eql)
                                   (unless not-p
                                     (return t))))))
    :give-up))

(setf (fun-info-equality-constraint (fun-info-or-lose 'equalp))
      #'equal-equality-constraint-optimizer)

(defoptimizer (= equality-constraint) ((x y) node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  (=
                                   (return (not not-p)))
                                  ((eq eql)
                                   (unless not-p
                                     (return t)))
                                  ((< >)
                                   (unless not-p
                                     (return nil)))
                                  ((<= >=)
                                   (when not-p
                                     (return nil))))))
    :give-up))

(defoptimizer (char= equality-constraint) ((x y) node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  ((char= eq eql)
                                   (return (not not-p)))
                                  (two-arg-char-equal
                                   (when not-p
                                     (return nil))))))
    :give-up))

(defoptimizer (two-arg-char-equal equality-constraint) ((x y)  node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  (two-arg-char-equal
                                   (return (not not-p)))
                                  ((char= eq eql)
                                   (unless not-p
                                     (return t))))))
    :give-up))

(defoptimizer (> equality-constraint) ((x y) node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  (>
                                   (return (not not-p)))
                                  (>=
                                   (when not-p
                                     (return nil)))
                                  (<=
                                   (return not-p))
                                  ((< . #.+eq-ops+)
                                   (unless not-p
                                     (return nil))))))
    :give-up))

(defoptimizer (< equality-constraint) ((x y) node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  (<
                                   (return (not not-p)))
                                  (<=
                                   (when not-p
                                     (return nil)))
                                  (>=
                                   (return not-p))
                                  ((> . #.+eq-ops+)
                                   (unless not-p
                                     (return nil))))))
    :give-up))

(defoptimizer (>= equality-constraint) ((x y)  node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  (>=
                                   (return (not not-p)))
                                  (<
                                   (return not-p))
                                  (<=
                                   (when not-p
                                    (return t)))
                                  (>
                                   (unless not-p
                                     (return t)))
                                  (#.+eq-ops+
                                   (unless not-p
                                     (return t))))))
    :give-up))

(defoptimizer (<= equality-constraint) ((x y) node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  (<=
                                   (return (not not-p)))
                                  (>
                                   (return not-p))
                                  (>=
                                   (when not-p
                                     (return t)))
                                  (<
                                   (unless not-p
                                     (return t)))
                                  (#.+eq-ops+
                                   (unless not-p
                                     (return t))))))
    :give-up))

(defoptimizer (- equality-constraint) ((x y) node gen)
  gen
  (when (and (csubtypep (lvar-type x) (specifier-type 'integer))
             (csubtypep (lvar-type y) (specifier-type 'integer)))
    (block nil
      (macrolet ((derive (type)
                   `(progn
                      (derive-node-type node (specifier-type ',type))
                      (return))))
        (map-equality-constraints
         x y gen
         (lambda (op not-p)
           (if not-p
               (case op
                 (< (derive (integer 0)))       ; >=
                 (> (derive (integer * 0)))     ; <=
                 (<= (derive (integer (0))))    ; >
                 (>= (derive (integer * (0))))) ; <
               (case op
                 (>= (derive (integer 0)))
                 (<= (derive (integer * 0)))
                 (> (derive (integer (0))))
                 (< (derive (integer * (0)))))))))))
  :give-up)

(defoptimizer (%check-bound equality-constraint) ((array dimension index) node gen)
  (let ((array-var (ok-lvar-lambda-var array gen)))
    (when (and array-var
               (csubtypep (lvar-type array) (specifier-type '(simple-array * (*))))
               (block nil
                 (map-equality-constraints (make-vector-length-constraint array-var) index gen
                                           (lambda (op not-p)
                                             (when (and (eq op '>)
                                                        (not not-p))
                                               (return t))))))
      (reoptimize-node node)
      (setf (combination-info node) 'array-in-bounds-p)))
  :give-up)

(deftransform %check-bound ((array dimension index) ((simple-array * (*)) t t) * :node node)
  (if (eq (combination-info node) 'array-in-bounds-p)
      ;; It's in bounds but it may be of the wrong type
      `(progn (the (and fixnum unsigned-byte) index)
              (values))
      (give-up-ir1-transform)))

(defoptimizer (vector-length equality-constraint) ((vector) node gen)
  (let (type
        (vector-var (ok-lvar-lambda-var vector gen)))
    (when (and vector-var
               (csubtypep (lvar-type vector) (specifier-type 'simple-array)))
      (block nil
        (do-equality-constraints (y op not-p) (make-vector-length-constraint vector-var) gen
          (let ((constant (and (constant-p y)
                               (constant-value y))))
            (cond ((integerp constant)
                   (case op
                     (eq
                      (unless not-p
                        (setf type (specifier-type `(eql ,constant)))
                        (return)))
                     (>
                      (let ((p (specifier-type (if not-p
                                                   `(integer 0 ,constant)
                                                   `(integer (,constant))))))
                        (setf type
                              (if type
                                  (type-intersection type p)
                                  p))))
                     (<
                      (let ((p (specifier-type (if not-p
                                                   `(integer ,constant)
                                                   `(integer 0 (,constant))))))
                        (setf type
                              (if type
                                  (type-intersection type p)
                                  p))))))
                  (t
                   (when (ctype-p y)
                     (setf type
                           (type-after-comparison op not-p (or type (specifier-type 'index)) y))))))))
      (when type
        (derive-node-type node type))))
  :give-up)

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
                 (let ((var1 (ok-ref-lambda-var ref1))
                       (var2 (ok-ref-lambda-var ref2)))
                   (when (and var1 var2)
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
                                (block nil
                                  (let ((constraints (block-out (node-block ref))))
                                    (when constraints
                                      (do-conset-constraints-intersection (con (constraints
                                                                                (lambda-var-equality-constraints var1)))
                                        (multiple-value-bind (op not)
                                            (relations var1 var2 con)
                                          (when op
                                            (return (values op not))))))))))
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
                (csubtypep (lvar-type y) (specifier-type '(integer (1)))))
           (list (list '< x)))
          ((and (csubtypep (lvar-type x) (specifier-type '(integer 0)))
                (csubtypep (lvar-type y) (specifier-type '(integer 0))))
           (list (list '<= x))))))

(defoptimizer (truncate constraint-propagate-result) ((x y) node)
  (div-constraints x y))

(defoptimizer (/ constraint-propagate-result) ((x y) node)
  (div-constraints x y))
