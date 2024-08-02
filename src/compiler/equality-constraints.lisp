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

(defun invert-operator (operator)
  (case operator
    (< '>)
    (> '<)
    (<= '>=)
    (>= '<=)
    (t operator)))

(defun not-operator (operator)
  (case operator
    (> '<=)
    (< '>=)
    (>= '<)
    (<= '>)))

(defmacro do-equality-constraints ((con op not-p &optional amount) var constraints &body body)
  (once-only ((var var)
              (constraints constraints))
    `(do-conset-constraints-intersection
         (,con (,constraints (lambda-var-equality-constraints (constraint-var ,var))))
       (flet ((body (,con ,op ,@(and amount (list amount)) ,not-p) ,@body))
         (cond ((vector-constraint-eq-p (constraint-x ,con) ,var)
                (body (constraint-y ,con)
                      (equality-constraint-operator ,con)
                      ,@(and amount
                             `((equality-constraint-amount ,con)))
                      (equality-constraint-not-p ,con)))
               ((vector-constraint-eq-p (constraint-y ,con) ,var)
                (body (constraint-x ,con)
                      (invert-operator (equality-constraint-operator ,con))
                      ,@(and amount
                             `((equality-constraint-amount ,con)))
                      (equality-constraint-not-p ,con))))))))

(defun find-equality-constraint (operator amount x x-var y y-key not-p)
  (let ((constraints (lambda-var-equality-constraints-hash x-var)))
    (when constraints
      (let ((constraints (gethash y-key constraints)))
        (if (typep y-key 'sb-kernel::type-class)
            (loop for con in constraints
                  when (and (eq (equality-constraint-operator con) operator)
                            (eq (constraint-not-p con) not-p)
                            (eql (equality-constraint-amount con) amount)
                            (vector-constraint-eq-p (constraint-x con) x)
                            (type= (constraint-y con) y))
                  return con)
            (loop for con in constraints
                  when (and (eq (equality-constraint-operator con) operator)
                            (eq (constraint-not-p con) not-p)
                            (vector-constraint-eq-p (constraint-x con) x)
                            (vector-constraint-eq-p (constraint-y con) y)
                            (eql (equality-constraint-amount con) amount))
                  return con))))))

(defun find-or-create-equality-constraint (operator x y not-p &optional (amount 0))
  (unless amount
    (setf amount 0))
  (let ((x-var (constraint-var x))
        (cache-key (typecase y
                     (numeric-type ;; eq-comparable
                      y)
                     (ctype
                      (sb-kernel::type-class y))
                     (vector-length-constraint y
                      (vector-length-constraint-var y))
                     (t
                      y))))
    (or (find-equality-constraint operator amount x x-var y cache-key not-p)
        (let ((new (make-equality-constraint (length *constraint-universe*)
                                             operator
                                             x y not-p
                                             amount)))
          (vector-push-extend new *constraint-universe* (1+ (length *constraint-universe*)))
          (flet ((add (var)
                   (conset-adjoin new (lambda-var-constraints var))
                   (macrolet ((ensure-vec (place)
                                `(or ,place
                                     (setf ,place
                                           (make-array 8 :adjustable t :fill-pointer 0)))))
                     (vector-push-extend new (ensure-vec (lambda-var-equality-constraints var))))))
            (add x-var)
            (let ((hash (or (lambda-var-equality-constraints-hash x-var)
                            (setf (lambda-var-equality-constraints-hash x-var)
                                  (make-hash-table :test #'eq)))))
              (push new (gethash cache-key hash)))
            (let ((y (constraint-var y)))
              (when (lambda-var-p y)
                (add y))))
          new))))

(defun conset-add-equality-constraint (conset operator x y not-p &optional (amount 0))
  (conset-adjoin (find-or-create-equality-constraint operator x y not-p amount)
                 conset))

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

(defun inherit-equality-p (new from not-p &optional (min-amount 0) max-amount (from-amount 0) set)
  (unless min-amount
    (setf min-amount 0))
  (unless from-amount
    (setf from-amount 0))
  (case new
    ((char= char-equal . #.+eq-ops+)
     (unless not-p
       (values from from-amount)))
    (<
     (if not-p
         (case from
           ((> >=) (values '< (+ from-amount min-amount))))
         (case from
           ((< <= . #.+eq-ops+) (values '< (+ from-amount min-amount)))
           (>
            (when (and max-amount
                       (not set))
              (let ((diff (- from-amount max-amount)))
                (cond ((> diff 0)
                       (values '> diff)))))))))
    (>
     (if not-p
         (case from
           ((< <=) (values '> (+ from-amount min-amount))))
         (case from
           ((> >= . #.+eq-ops+) (values '> (+ from-amount min-amount)))
           (<
            (when (and max-amount
                       (not set))
              (let ((diff (- from-amount max-amount)))
                (cond ((> diff 0)
                       (values '< diff)))))))))
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

(defun add-equality-constraint (operator first second constraints consequent-constraints alternative-constraints
                                &optional (min-amount 0) max-amount)
  (case operator
    (vector-length
     (let ((var (if (lambda-var-p first)
                    first
                    (lvar-dest-var first))))
       (when (and var
                  (lambda-var-constraints var))
         (add-equality-constraint 'eq (make-vector-length-constraint var) second
                                  constraints consequent-constraints alternative-constraints))))
    (t
     (let* ((x (cond ((lambda-var/vector-length-p first)
                      first)
                     ((ok-lvar-lambda-var/vector-length first constraints))
                     (t
                      first)))
            (y (cond ((lambda-var/vector-length-p second)
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
         (setf operator (invert-operator operator)))
       (flet ((replace-var (var with)
                (cond ((eq var with)
                       var)
                      ((vector-length-constraint-p var)
                       (make-vector-length-constraint with))
                      (t
                       with)))
              (add (x y &key (operator operator)
                             (consequent consequent-constraints)
                             (alternative alternative-constraints)
                             (amount min-amount))
                (when (eq x y)
                  (return-from add))
                (unless (lambda-var/vector-length-p x)
                  (unless (lambda-var/vector-length-p y)
                    (return-from add))
                  (rotatef x y)
                  (setf operator (invert-operator operator)))
                (conset-add-equality-constraint consequent operator x y nil amount)
                (when alternative
                  (conset-add-equality-constraint alternative operator x y t amount))))
         (do-eql-vars (eql-x ((constraint-var x) constraints))
           (let ((x (replace-var x eql-x)))
             (when (neq y-type *universal-type*)
               (add x y-type :amount 0))
             (if (lambda-var/vector-length-p y)
                 (do-eql-vars (eql-y ((constraint-var y) constraints))
                   (let ((y (replace-var y eql-y)))
                     (add x y)
                     (when (neq x-type *universal-type*)
                       (add x-type y :amount 0))))
                 (add x y))
             (add x y)))
         (flet ((inherit (x y x-type operator)
                  (when (lambda-var/vector-length-p y)
                    (do-equality-constraints (in-y in-op in-not-p in-amount) y constraints
                      (unless (eq in-y y)
                        (flet ((add (operator target)
                                 (multiple-value-bind (inherit inherit-amount)
                                     (inherit-equality-p operator in-op in-not-p min-amount max-amount in-amount)
                                   (when inherit
                                     (add x in-y :operator inherit
                                                 :amount inherit-amount
                                                 :consequent target
                                                 :alternative nil)
                                     (when (neq x-type *universal-type*)
                                       (add x-type in-y :operator inherit
                                                        :alternative nil
                                                        :consequent target
                                                        :amount 0))))))
                          (add operator consequent-constraints)
                          (when alternative-constraints
                            (add (not-operator operator) alternative-constraints))))))))
           (inherit x y x-type operator)
           (inherit y x y-type (invert-operator operator))
           (when (lvar-p y)
             (loop for (in-op in-lvar in-min-amount) in (lvar-result-constraints y constraints)
                   do
                   (unless (eq in-lvar first)
                     (flet ((add (operator target)
                              (multiple-value-bind (inherit inherit-amount)
                                  (inherit-equality-p operator in-op nil min-amount max-amount in-min-amount)
                                (when inherit
                                  (add-equality-constraint inherit x in-lvar
                                                           constraints target nil inherit-amount)))))
                       (add operator consequent-constraints)
                       (when alternative-constraints
                         (add (not-operator operator) alternative-constraints))))))))))))

(defun add-equality-constraints (operator args constraints
                                 consequent-constraints
                                 alternative-constraints)
  (case operator
    ((char= char-equal > <  <= >=
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
                                              (constraint-not-p con)
                                              (equality-constraint-amount con))
          target))))))

;;; Ignore AMOUNT
(defun join-equality-constraints (var block in)
  (let* ((constraints (make-hash-table :test #'equal))
         (pred (block-pred block))
         (i -1))
    (loop for pred in pred
          do
          (let ((out (block-out-for-successor pred block)))
            (when out
              (incf i)
              (do-equality-constraints (in-con in-op not-p amount) var out
                (let ((existing (gethash (list in-con in-op not-p) constraints)))
                  (cond ((= (if existing
                                (car existing)
                                -1)
                            (1- i))
                         (setf (gethash (list in-con in-op not-p) constraints)
                               (list i
                                     (if existing
                                         (min amount (second existing))
                                         amount)
                                     (second existing))))
                        ((and existing
                              (= (car existing) i))
                         ;; Maximize the current block value while
                         ;; not exceeding the overall minimal amount.
                         (let ((overall-min (third existing)))
                           (setf (gethash (list in-con in-op not-p) constraints)
                                 (list i
                                       (if overall-min
                                           (min (max amount (second existing)) overall-min)
                                           (max amount (second existing)))
                                       overall-min))))))))))
    (dohash ((key value) constraints)
      (when (= (car value) i)
        (destructuring-bind (y op not-p) key
          (conset-adjoin
           (find-or-create-equality-constraint op
                                               var
                                               y
                                               not-p
                                               (second value))
           in))))))

(defun try-equality-constraint (call gen)
  (let ((constraint (fun-info-equality-constraint (basic-combination-fun-info call)))
        (lvar (node-lvar call)))
    (when constraint
      (let ((result (funcall constraint call gen)))
        (unless (eq result :give-up)
          (when lvar
            (replace-combination-with-constant result call))
          t)))))

(defun map-equality-constraints (lvar-x lvar-y constraints function)
  (let* ((x (cond ((lambda-var/vector-length-p lvar-x)
                   lvar-x)
                  ((ok-lvar-lambda-var/vector-length lvar-x constraints t))
                  (t
                   lvar-x)))
         (y (cond ((ok-lvar-lambda-var/vector-length lvar-y constraints t))
                  (t
                   lvar-y)))
         (invert))
    (unless (lambda-var/vector-length-p x)
      (unless (lambda-var/vector-length-p y)
        (return-from map-equality-constraints))
      (rotatef x y)
      (rotatef lvar-x lvar-y)
      (setf invert t))
    (do-equality-constraints (in-y op not-p) x constraints
      (when (or (vector-constraint-eq-p in-y y)
                (and (ctype-p in-y)
                     (lvar-p lvar-y)
                     (or (and (type-singleton-p in-y)
                              (type= in-y (lvar-type lvar-y)))
                         (let ((interval-in-y (type-approximate-interval in-y))
                               (interval-y (type-approximate-interval (lvar-type lvar-y))))
                           (when (and interval-y
                                      interval-in-y)
                             (let* ((a interval-in-y)
                                    (b interval-y))
                               (case op
                                 (< (unless not-p
                                      (interval-< a b)))
                                 (> (unless not-p
                                      (interval-< b a)))
                                 (eq
                                  (unless not-p
                                    (cond ((interval-< b a)
                                           (setf op '>))))))))))))
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
                                  ((= <= >= char-equal)
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
                                  ((= <= >= char-equal)
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
                                  (char-equal
                                   (when not-p
                                     (return nil))))))
    :give-up))

(defoptimizer (char-equal equality-constraint) ((x y)  node gen)
  (block nil
    (map-equality-constraints x y gen
                              (lambda (op not-p)
                                (case op
                                  (char-equal
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

(defoptimizer (%check-bound constraint-propagate) ((array dimension index) node gen)
  (add-equality-constraint '< index dimension gen gen nil)
  (let ((var (ok-lvar-lambda-var index gen))
        (type (if (constant-lvar-p dimension)
                  (specifier-type `(integer 0 (,(lvar-value dimension))))
                  (specifier-type 'index))))
    (when var
      (list (list 'typep var type nil)))))

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
  (if (or (eq (combination-info node) 'array-in-bounds-p)
          (let ((index (type-approximate-interval (lvar-type index)))
                (dim (type-approximate-interval (lvar-type dimension))))
            (and index dim
                 (interval-< index dim))))
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

(deftransform %in-bounds-constraint ((vector length) * * :node node)
  (delay-ir1-transform node :ir1-phases)
  nil)

(defoptimizer (%in-bounds-constraint constraint-propagate) ((vector length) node gen)
  (let ((var (ok-lvar-lambda-var vector gen)))
    (when var
      (add-equality-constraint '>= (make-vector-length-constraint var) length gen gen nil)))
  nil)

(defun lvar-result-constraints (lvar constraints)
  (let ((uses (lvar-uses lvar)))
    (cond ((combination-p uses)
           (binding* ((info (combination-fun-info uses) :exit-if-null)
                      (propagate (fun-info-constraint-propagate-result info)
                                 :exit-if-null))
             (funcall propagate uses constraints)))
          ((proper-list-of-length-p uses 2)
           (let (r)
             ;; Detect MIN/MAX variables.
             (destructuring-bind (ref1 ref2) uses
               (when (and (ref-p ref1) (ref-p ref2))
                 (let ((var1 (ok-ref-lambda-var ref1))
                       (var2 (ok-ref-lambda-var ref2)))
                   (when (and var1 var2
                              (not (lambda-var-sets var1))
                              (not (lambda-var-sets var2)))
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
  (loop for (operator second min-amount max-amount) in (lvar-result-constraints lvar constraints)
        do
        (add-equality-constraint operator var second constraints target nil min-amount max-amount))
  (when (lambda-var-p var)
    (let ((vector-length (vector-length-var-p lvar constraints)))
      (when vector-length
        (conset-add-equality-constraint target 'eq var vector-length nil)))))

(defun add-mv-let-result-constraints (call fun constraints &optional (target constraints))
  (let ((vars (lambda-vars fun))
        (lvars (basic-combination-args call)))
    (when (= (length lvars) 1)
      (loop for (nth-value operator second min-amount max-amount) in (nth-value 1 (lvar-result-constraints (car lvars) constraints))
            do
            (add-equality-constraint operator (elt vars nth-value) second constraints target nil min-amount max-amount)))))

;;; Need a separate function because a set clears the constraints of the var
(defun add-set-constraints (var lvar constraints)
  (let (gen)
    (loop for (operator second min-amount max-amount) in (lvar-result-constraints lvar constraints)
          do
          (if (vector-length-constraint-p second)
              (conset-add-equality-constraint (or gen
                                                  (setf gen (make-conset)))
                                              operator var second nil min-amount)
              (let ((y (cond ((lambda-var-p second)
                              second)
                             (t
                              (ok-lvar-lambda-var second constraints)))))
                (when y

                  (do-eql-vars (eql-y (y constraints))
                    (when (eq eql-y var)
                      (do-equality-constraints (in-y in-op in-not-p in-amount) var constraints
                        (unless (eq in-y var)
                          (multiple-value-bind (inherit inherit-amount)
                              ;; Avoid changing the direction of inequalities
                              ;; because it might be done in a loop.
                              (inherit-equality-p operator in-op in-not-p min-amount max-amount in-amount t)
                            (when inherit
                              (conset-add-equality-constraint (or gen
                                                                  (setf gen (make-conset)))
                                                              inherit var in-y nil
                                                              inherit-amount)))))))))))
    gen))

(defoptimizer (- constraint-propagate-result) ((a b) node)
  (when (csubtypep (lvar-type a) (specifier-type 'integer))
    (cond ((csubtypep (lvar-type b) (specifier-type '(integer 0)))
           (let ((interval (type-approximate-interval (lvar-type b))))
             (when interval
               (let ((low (interval-low interval)))
                 (list (if (> low 0)
                           (list '< a low (interval-high interval))
                           (list '<= a)))))))
          ((csubtypep (lvar-type b) (specifier-type '(integer * -1)))
           (let ((interval (type-approximate-interval (lvar-type b))))
             (when interval
               (let* ((interval (interval-neg interval))
                      (low (interval-low interval)))
                 (list (if (> low 0)
                           (list '> a low (interval-high interval))
                           (list '>= a))))))))))

(defoptimizer (+ constraint-propagate-result) ((x y) node)
  (let (r)
    (flet ((try (a b)
             (when (csubtypep (lvar-type a) (specifier-type 'integer))
               (cond ((csubtypep (lvar-type b) (specifier-type '(integer 0)))
                      (let ((interval (type-approximate-interval (lvar-type b))))
                        (when interval
                          (let ((low (interval-low interval)))
                            (push (if (> low 0)
                                      (list '> a low (interval-high interval))
                                      (list '>= a))
                                  r)))))
                     ((csubtypep (lvar-type b) (specifier-type '(integer * -1)))
                      (let ((interval (type-approximate-interval (lvar-type b))))
                        (when interval
                          (let* ((interval (interval-neg interval))
                                 (low (interval-low interval)))
                            (push
                             (if (> low 0)
                                 (list '< a low (interval-high interval))
                                 (list '<= a))
                             r)))))))))
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

(defoptimizer (allocate-vector constraint-propagate-result) ((#+ubsan poisoned type length words) node)
  (list (list 'vector-length length)))

(defoptimizer (%make-array constraint-propagate-result) ((length &rest args) node)
  (list (list 'vector-length length)))

(defoptimizer (make-sequence constraint-propagate-result) ((type length &rest args) node)
  (list (list 'vector-length length)))

(defoptimizer (read-sequence constraint-propagate-result) ((seq stream &key start end) node gen)
  (let ((var (ok-lvar-lambda-var seq gen)))
    (when var
      (list (list '<= (make-vector-length-constraint var))))))

(defoptimizer (floor constraint-propagate-result) ((x y) node)
  (when (csubtypep (lvar-type y) (specifier-type '(real 0)))
    (values nil
            (list (list 1 '< y)))))

(defun find-position-item-type (sequence key test &optional test-not)
  (let ((type *universal-type*))
    (flet ((fun-accepts-type (fun-lvar argument)
             (when fun-lvar
               (let ((fun-type (lvar-fun-type fun-lvar t t)))
                 (when (fun-type-p fun-type)
                   (let ((arg (nth argument (fun-type-n-arg-types (1+ argument) fun-type))))
                     (when arg
                       (setf type
                             (type-intersection type arg)))))))))
      (fun-accepts-type test 0)
      (fun-accepts-type test-not 0)
      (when (and (or (not test)
                     (lvar-fun-is test '(eq eql)))
                 (not test-not))
        (setf type
              (type-intersection type (sequence-element-type sequence key)))))
    type))

(defoptimizer (%find-position constraint-propagate-if) ((item sequence from-end start end key test) node gen nth-value)
  (let ((type (find-position-item-type sequence key test)))
    (values item (if (and (= nth-value 0)
                          (lvar-fun-is key '(identity))
                          (lvar-fun-is test '(eq eql equal equalp)))
                     (type-intersection type (specifier-type '(not null)))
                     type)
            nil nil t)))

(defoptimizers constraint-propagate-if (position) ((item sequence &key key test test-not &allow-other-keys))
  (values item (find-position-item-type sequence key test test-not) nil nil t))

(defoptimizers constraint-propagate-if (find) ((item sequence &key key test test-not &allow-other-keys))
  (let ((type (find-position-item-type sequence key test test-not)))
    (values item (if (and (not key)
                          (not test-not)
                          (or (not test)
                              (lvar-fun-is test '(eq eql equal equalp))))
                     (type-intersection type (specifier-type '(not null)))
                     type)
            nil nil t)))

(defoptimizers constraint-propagate-if (%member %member-eq %member-if %member-if-not) ((x list))
  (values list (specifier-type 'cons) nil nil t))

(defoptimizers constraint-propagate-if
    (%member-key %member-key-eq) ((item list key))
  (values nil nil (list (list 'typep list (specifier-type 'cons))
                        (list 'typep item (find-position-item-type list key nil)))))

(defoptimizers constraint-propagate-if
    (%member-test %member-test-not) ((item list test))
  (values nil nil (list (list 'typep list (specifier-type 'cons))
                        (list 'typep item (find-position-item-type list nil test)))))

(defoptimizers constraint-propagate-if
    (%member-if-key %member-if-not-key) ((x list test))
  (values list (specifier-type 'cons) nil nil t))

(defoptimizers constraint-propagate-if (%member-key-test-not) ((item list key test))
  (values nil nil (list (list 'typep list (specifier-type 'cons))
                        (list 'typep item (find-position-item-type list key nil test)))))

(defoptimizers constraint-propagate-if (%member-key-test) ((item list key test))
  (values nil nil (list (list 'typep list (specifier-type 'cons))
                        (list 'typep item (find-position-item-type list key test)))))

(defoptimizer (equal constraint-propagate-if) ((x y))
  (let ((x-type (equal-type (lvar-type x)))
        (y-type (equal-type (lvar-type y))))
    (let ((intersection (type-intersection x-type y-type)))
      (unless (or (eq intersection *empty-type*)
                  (eq intersection *universal-type*))
        (let ((constraints))
          (push (list 'typep x intersection) constraints)
          (push (list 'typep y intersection) constraints)
          (values nil nil constraints))))))

(defoptimizer (equalp constraint-propagate-if) ((x y))
  (let ((x-type (equalp-type (lvar-type x)))
        (y-type (equalp-type (lvar-type y))))
    (let ((intersection (type-intersection x-type y-type)))
      (unless (or (eq intersection *empty-type*)
                  (eq intersection *universal-type*))
        (let ((constraints))
          (push (list 'typep x intersection) constraints)
          (push (list 'typep y intersection) constraints)
          (values nil nil constraints))))))

(defun character-set-range (lvar)
  (if (constant-lvar-p lvar)
      (let ((code (char-code (lvar-value lvar))))
        (values code code))
      (let ((type (lvar-type lvar)))
        (if (typep type 'character-set-type)
            (loop for (lo . hi) in (character-set-type-pairs type)
                  minimize lo into min
                  maximize hi into max
                  finally (return (values min max)))
            (values 0 (1- char-code-limit))))))

(defun char<-constraints (char1 char2)
  (multiple-value-bind (min2 max2) (character-set-range char2)
    (values (list 'typep char1 (if (zerop max2)
                                   *empty-type*
                                   (specifier-type `(character-set ((0 . ,(1- max2)))))))
            (list 'typep char1 (specifier-type `(character-set ((,min2 . #.(1- char-code-limit)))))))))

(defun char>-constraints (char1 char2)
  (multiple-value-bind (min2 max2) (character-set-range char2)
    (values (list 'typep char1 (if (= min2 (1- char-code-limit))
                                   *empty-type*
                                   (specifier-type `(character-set ((,(1+ min2) . #.(1- char-code-limit)))))))
            (list 'typep char1 (specifier-type `(character-set ((0 . ,max2))))))))

(defoptimizer (char< constraint-propagate-if)
    ((char1 char2))
  (multiple-value-bind (if1 then1)
      (char<-constraints char1 char2)
    (multiple-value-bind (if2 then2)
        (char>-constraints char2 char1)
      (values nil nil (list if1 if2) (list then1 then2)))))

(defoptimizer (char> constraint-propagate-if)
    ((char1 char2))
  (multiple-value-bind (if1 then1)
      (char>-constraints char1 char2)
    (multiple-value-bind (if2 then2)
        (char<-constraints char2 char1)
      (values nil nil (list if1 if2) (list then1 then2)))))
