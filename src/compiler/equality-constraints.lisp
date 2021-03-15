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

(defun find-equality-constraint (operator x y not-p)
  (let ((constraints (lambda-var-equality-constraints x)))
    (when constraints
      (loop for con across constraints
            when (and (eq (equality-constraint-operator con) operator)
                      (eq (constraint-not-p con) not-p)
                      (eq (constraint-x con) x)
                      (eq (constraint-y con) y))
            return con))))

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
          (add x)
          (when (lambda-var-p y)
            (add y)))
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
       (let* ((x (ok-lvar-lambda-var (first args) constraints))
              (second (second args))
              (y (if (constant-lvar-p second)
                     (find-constant (lvar-value second))
                     (ok-lvar-lambda-var second constraints))))
         (when (and x y)
           ;; TODO: inherit constraints
           (let ((con (find-or-create-equality-constraint operator x y nil))
                 (not-con (find-or-create-equality-constraint operator x y t)))
             (conset-adjoin con consequent-constraints)
             (conset-adjoin not-con alternative-constraints))))))))


(defun find-ref-equality-constraint (operator lvar1 lvar2 &optional (commutative t))
  (let ((ref1 (principal-lvar-use lvar1))
        (ref2 (principal-lvar-use lvar2)))
    (when (and (ref-p ref1)
               (ref-p ref2))
      (let ((leaf1 (ref-leaf ref1))
            (leaf2 (ref-leaf ref2)))
        (flet ((find-constraint (ref)
                 (loop for con in (ref-constraints ref)
                       when (and (equality-constraint-p con)
                                 (eq (equality-constraint-operator con) operator)
                                 (or (and (eq (constraint-x con) leaf1)
                                          (eq (constraint-y con) leaf2))
                                     (and commutative
                                          (eq (constraint-x con) leaf2)
                                          (eq (constraint-y con) leaf1))))
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
              ref1-con)))))))

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
