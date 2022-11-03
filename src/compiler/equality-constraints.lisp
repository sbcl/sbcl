;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defstruct (equality-constraint
            (:include constraint)
            (:constructor make-equality-constraint
                (number operator x y not-p
                 &aux (kind 'equality))))

  (operator nil :type symbol))

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
                      (eq (vector-length-constraint-var b)
                          (vector-length-constraint-var b))))))
     (when constraints
       (loop for con across constraints
             when (and (eq (equality-constraint-operator con) operator)
                       (eq (constraint-not-p con) not-p)
                       (constraints-eq-p (constraint-x con) x)
                       (constraints-eq-p (constraint-y con) y))
             return con)))))

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

(defun add-equality-constraints (operator args constraints
                                 consequent-constraints
                                 alternative-constraints)
  (case operator
    ((eq eql char= two-arg-char-equal
      > < =)
     (when (= (length args) 2)
       (flet ((ok-lvar-p (lvar)
                (or (ok-lvar-lambda-var lvar constraints)
                    (let* ((use (principal-lvar-ref-use lvar))
                           (array-lvar
                             (and (combination-p use)
                                  (lvar-fun-is (combination-fun use)
                                               '(vector-length length))
                                  (car (combination-args use))))
                           (array-var (and array-lvar
                                           (ok-lvar-lambda-var array-lvar constraints))))
                      (and array-var
                           (make-vector-length-constraint array-var))))))
        (let* ((x (ok-lvar-p (first args)))
               (second (second args))
               (y (if (constant-lvar-p second)
                      (nth-value 1 (lvar-value second))
                      (ok-lvar-p second))))
          (when (and x y)
            ;; TODO: inherit constraints
            (let ((con (find-or-create-equality-constraint operator x y nil))
                  (not-con (find-or-create-equality-constraint operator x y t)))
              (conset-adjoin con consequent-constraints)
              (conset-adjoin not-con alternative-constraints)
              con))))))))


(defun find-ref-equality-constraint (operator lvar1 lvar2 &optional (commutative t))
  (flet ((ref (lvar)
           (let ((use (principal-lvar-use lvar)))
             (cond ((ref-p use)
                    (values use nil))
                   ((and (combination-p use)
                         (lvar-fun-is (combination-fun use)
                                      '(vector-length length)))
                    (let ((arg (car (combination-args use))))
                      (and (csubtypep (lvar-type arg) (specifier-type 'simple-array))
                           (values (principal-lvar-use arg)
                                   t)))))))
         (constraint-eq-p (leaf vector-length-p x)
           (if vector-length-p
               (and (vector-length-constraint-p x)
                    (eq (vector-length-constraint-var x) leaf))
               (eq leaf x))))
    (multiple-value-bind (ref1 vector-length-p1) (ref lvar1)
      (multiple-value-bind (ref2 vector-length-p2) (ref lvar2)
        (when (and (ref-p ref1)
                   (ref-p ref2))
          (let ((leaf1 (ref-leaf ref1))
                (leaf2 (ref-leaf ref2)))
            (flet ((find-constraint (ref)
                     (loop for con in (ref-constraints ref)
                           when (and (equality-constraint-p con)
                                     (eq (equality-constraint-operator con) operator)
                                     (or (and (constraint-eq-p leaf1 vector-length-p1 (constraint-x con))
                                              (constraint-eq-p leaf2 vector-length-p2 (constraint-y con)))
                                         (and commutative
                                              (constraint-eq-p leaf2 vector-length-p2 (constraint-x con))
                                              (constraint-eq-p leaf1 vector-length-p1 (constraint-y con)))))
                           return con))
                   (has-sets (leaf)
                     (and (lambda-var-p leaf)
                          (lambda-var-sets leaf))))
              (let ((ref1-con (find-constraint ref1)))
                (when (and ref1-con
                           ;; If the variables are set both references
                           ;; need to have the same constraint, otherwise
                           ;; one the references may be done before the
                           ;; set.
                           (or (and (not (has-sets leaf1))
                                    (not (has-sets leaf2)))
                               (eq ref1-con (find-constraint ref2))))
                  ref1-con)))))))))

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
        (inverse
         `(when (and constraint
                     (not (constraint-not-p constraint)))
            (setf result nil)
            t)))))

(defoptimizer (eql equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint eq true)
     (equality-constraint eql)
     (equality-constraint char=)
     (equality-constraint two-arg-char-equal false)
     (equality-constraint > inverse)
     (equality-constraint < inverse)
     (equality-constraint = false))
    result))

(defoptimizer (eq equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint eq)
     (equality-constraint eql)
     (equality-constraint char=)
     (equality-constraint two-arg-char-equal false)
     (equality-constraint > inverse)
     (equality-constraint < inverse)
     (equality-constraint = false))
    result))

(defoptimizer (= equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint =)
     (equality-constraint eq true)
     (equality-constraint eql true)
     (equality-constraint > inverse)
     (equality-constraint < inverse))
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
     (equality-constraint < inverse nil)
     (equality-constraint eq inverse)
     (equality-constraint eql inverse)
     (equality-constraint = inverse))
    result))

(defoptimizer (< equality-constraint) ((x y))
  (let ((result :give-up))
    (or
     (equality-constraint < nil nil)
     (equality-constraint > inverse nil)
     (equality-constraint eq inverse)
     (equality-constraint eql inverse)
     (equality-constraint = inverse))
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
