;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defglobal *combination-match-aliases* (make-hash-table :test #'eq))

(defmacro def-combination-match-alias (name ll &body body)
  `(pushnew ',(if (integerp ll)
                  (cons ll body)
                  `(lambda (.form.)
                     (when (= (length .form.)
                              ,(length ll))
                       (destructuring-bind ,ll .form.
                         ,@body))))
            (gethash ',name *combination-match-aliases*)
            :test #'equalp))


(def-combination-match-alias lognot (x)
  `((- -1 ,x)))

(def-combination-match-alias - (a)
  (values
   `((%negate ,a))
   t)) ;; don't include (- x)

(defstruct cm-operator
  (names nil :type list)
  (name-var nil :type symbol)
  (commutative nil :type boolean))

(defstruct cm-operand
  (kind :var :type (member :wildcard :var :type :constant :plus :literal :combination))
  (name nil :type symbol)
  (type nil :type t)
  (value nil :type t)
  (sub-combinations nil :type list))

(defstruct cm-combination
  (operator nil :type (or null cm-operator))
  (operands nil :type list)
  (arg-count 0 :type fixnum)
  (variable nil :type (or null fixnum))
  (plus nil :type (or null fixnum))
  (rest-var nil :type symbol)
  (casts nil :type t))

(defun parse-cm-spec (spec unravel-casts)
  (labels ((clean-name-spec (list)
             (loop for tail = list then (cdr tail)
                   while tail
                   if (eq (car tail) :name)
                   do (setf tail (cdr tail))
                   else collect (car tail)))
           (extract-name-var (op)
             (and (consp op)
                  (second (member :name op))))
           (invert-relation (op)
             (case op
               (< '>)
               (> '<)
               (<= '>=)
               (>= '<=)))
           (equal-spec (a b)
             (cond ((eq a b))
                   ((symbolp a)
                    (and (symbolp b) (eq a b)))
                   ((typep a '(cons (member :type :constant)))
                    (equal a b))
                   ((and (consp a) (consp b))
                    (and (equal (car a) (car b))
                         (= (length a) (length b))
                         (every #'equal-spec a b)))))
           (invertible-p (s)
             (and (consp s)
                  (symbolp (car s))
                  (invert-relation (car s))
                  (= (length (cdr s)) 2)
                  (not (equal-spec (first (cdr s)) (second (cdr s))))))
           (invert-spec (s)
             (list (invert-relation (car s))
                   (second (cdr s))
                   (first (cdr s))))
           (ensure-or (x)
             (let ((specs (if (typep x '(cons (eql :or)))
                              (clean-name-spec (cdr x))
                              (list x))))
               (loop for s in specs
                     collect s
                     when (invertible-p s)
                     collect (invert-spec s))))
           (expand-aliases (specs &optional exclude)
             (loop for spec in specs
                   when (when (listp spec)
                          (destructuring-bind (names . rest) spec
                            (let (name-aliases
                                  full-aliases
                                  (name-var (extract-name-var names))
                                  (names (ensure-or names))
                                  (excluded exclude))
                              (loop for name in names
                                    do (let ((aliases (gethash name *combination-match-aliases*)))
                                         (loop for alias in aliases
                                               do (if (typep alias '(cons integer))
                                                      (when (= (first alias) (length rest))
                                                        (push (second alias) name-aliases))
                                                      (multiple-value-bind (new exclude)
                                                          (funcall (eval alias) rest)
                                                        (when new
                                                          (if exclude (setf excluded t))
                                                          (setf full-aliases
                                                                (append (expand-aliases new) full-aliases))))))))
                              (let ((spec (if name-aliases
                                              `((:or ,@names ,@name-aliases ,@(when name-var `(:name ,name-var))) . ,rest)
                                              spec)))
                                (append full-aliases
                                        (unless excluded
                                          (list spec)))))))
                   append it
                   else collect spec))
           (parse-operand (s expected-type)
             (cond ((typep s '(cons (eql :type)))
                    (make-cm-operand :kind :type
                                     :type (second s)
                                     :name (third s)))
                   ((typep s '(cons (eql :constant)))
                    (make-cm-operand :kind :constant
                                     :name (second s)
                                     :type (or (third s) expected-type)))
                   ((typep s '(cons (eql :+)))
                    (make-cm-operand :kind :plus
                                     :name (second s)))
                   ((eq s '*)
                    (make-cm-operand :kind :wildcard))
                   ((symbolp s)
                    (make-cm-operand :kind :var
                                     :name s))
                   ((atom s)
                    (make-cm-operand :kind :literal
                                     :value s))
                   ((consp s)
                    (make-cm-operand :kind :combination
                                     :sub-combinations (parse-cm-spec s unravel-casts)))))
           (parse-combination (raw)
             (destructuring-bind (name-spec . args) raw
               (let* ((plus (position-if (lambda (x) (typep x '(cons (eql :+)))) args))
                      (variable (or plus (position '&rest args)))
                      (rest-var (and (not plus) (second (member '&rest args))))
                      (arg-count (if plus (length args) (or variable (length args))))
                      (name-var (extract-name-var name-spec))
                      (names (ensure-or name-spec))
                      (arg-types
                        (when (singleton-p names)
                          (let ((fun-type (info :function :type (car names))))
                            (and (fun-type-p fun-type)
                                 (fun-type-n-arg-types arg-count fun-type)))))
                      (commutative
                        (and (not plus)
                             (or (find :commutative names)
                                 (loop for name in names
                                       always (unless (eq name :*)
                                                (ir1-attributep (fun-info-attributes (fun-info-or-lose name))
                                                                commutative))))
                             (not (or (integerp (car (last args)))
                                      (typep (car (last args)) '(cons (eql :constant)))
                                      (equal-spec (first args) (second args))))))
                      (casts (and unravel-casts
                                  arg-types
                                  (find *universal-type* arg-types :test-not #'eq)
                                  `(load-time-value
                                    (list ,@(loop for type in arg-types
                                                  collect `(specifier-type ',(type-specifier type)))))))
                      (arg-type-specifiers
                        (loop for type in arg-types
                              collect (and type
                                           (not (eq type *universal-type*))
                                           (type-specifier type))))
                      (names (loop for name in names
                                   collect (if (typep name '(cons (eql :commutative)))
                                               (second name)
                                               name)))
                      (op (make-cm-operator :names names
                                            :name-var name-var
                                            :commutative commutative))
                      (positional-args (subseq args 0 arg-count))
                      (operands (loop for arg in positional-args
                                      for expected-type = (pop arg-type-specifiers)
                                      collect (parse-operand arg expected-type))))
                 (make-cm-combination :operator op
                                      :operands operands
                                      :arg-count arg-count
                                      :variable variable
                                      :plus plus
                                      :rest-var rest-var
                                      :casts casts)))))
    (let ((raw-combs (expand-aliases (ensure-or spec))))
      (mapcar #'parse-combination raw-combs))))

(defmacro combination-match2 ((node &key (transform t) (unravel-casts t)) &body clauses)
  (let (bound-vars)
    (labels ((collect-cm-pattern-vars (combinations)
               (let (vars)
                 (labels ((add (s)
                            (when (and s
                                       (symbolp s)
                                       (not (keywordp s))
                                       (not (eq s '*))
                                       (not (eq s '&rest)))
                              (pushnew s vars)))
                          (walk-operand (op)
                            (case (cm-operand-kind op)
                              ((:var :constant :plus :type)
                               (add (cm-operand-name op)))
                              (:combination
                               (mapc #'walk-combination (cm-operand-sub-combinations op)))))
                          (walk-combination (comb)
                            (let ((op (cm-combination-operator comb)))
                              (when (cm-operator-name-var op)
                                (add (cm-operator-name-var op))))
                            (mapc #'walk-operand (cm-combination-operands comb))
                            (when (cm-combination-rest-var comb)
                              (push '&rest vars)
                              (add (cm-combination-rest-var comb)))))
                   (mapc #'walk-combination combinations)
                   (nreverse vars))))

             (expand-combinations (lvars operands combs body)
               (let ((old-bound-vars bound-vars))
                 (labels ((gen (&optional sub)
                            (let* ((comb (pop combs))
                                   (op (cm-combination-operator comb))
                                   (op-names (cm-operator-names op))
                                   (name-var (cm-operator-name-var op))
                                   (commutative (cm-operator-commutative op))
                                   (arg-operands (cm-combination-operands comb))
                                   (arg-count (cm-combination-arg-count comb))
                                   (variable (cm-combination-variable comb))
                                   (plus (cm-combination-plus comb))
                                   (rest-var (cm-combination-rest-var comb))
                                   (casts (cm-combination-casts comb))
                                   (vars (make-gensym-list arg-count "ARG"))
                                   (bind-vars (if rest-var
                                                  (append vars (list rest-var))
                                                  vars)))
                              (setf bound-vars old-bound-vars)
                              (when name-var
                                (push name-var bound-vars))
                              (let* ((inner-body
                                       (lambda ()
                                         (let ((old-bound-vars bound-vars))
                                           (cond (commutative
                                                  (assert (= (length vars) 2))
                                                  `(or ,(expand-operands vars arg-operands body)
                                                       ,(progn
                                                          (setf bound-vars old-bound-vars)
                                                          (expand-operands (list (second vars) (first vars))
                                                                           arg-operands body))))
                                                 (t
                                                  (expand-operands vars arg-operands body))))))
                                     (match-args
                                       (expand-operands lvars operands
                                                        (if name-var
                                                            (lambda ()
                                                              `(let ((,name-var .name.))
                                                                 ,(funcall inner-body)))
                                                            inner-body)))
                                     (branch-form
                                       `(or (multiple-value-bind ,bind-vars
                                                ,(if casts
                                                     (cond (plus (error "todo"))
                                                           (variable
                                                            `(check-typed-min-args .args. ,casts ,arg-count))
                                                           (t
                                                            `(check-typed-args .args. ,casts ,arg-count)))
                                                     (cond (plus
                                                            `(check-min-args .args. ,arg-count ,plus))
                                                           (variable
                                                            `(check-min-args .args. ,arg-count))
                                                           (t
                                                            `(check-args .args. ,arg-count))))
                                              (declare (ignorable ,@bind-vars))
                                              (when ,(if vars
                                                         (car vars)
                                                         (progn (aver rest-var) t))
                                                ,match-args))
                                            ,@(unless sub
                                                (loop while (and combs
                                                                 (subsetp (cm-operator-names (cm-combination-operator (car combs)))
                                                                          op-names))
                                                      collect
                                                      `(case .name.
                                                         ,(gen t)))))))
                                `(,op-names ,branch-form)))))
                   (loop while combs collect (gen)))))

             (expand-operands (lvars operands body)
               (if lvars
                   (let ((lvar (car lvars))
                         (op (car operands)))
                     (flet ((match-var (name &optional allow-empty constant constant-type)
                              (cond ((or (eq name '*)
                                         (and allow-empty (not name)))
                                     (expand-operands (cdr lvars) (cdr operands) body))
                                    ((member name bound-vars)
                                     `(when ,(if constant
                                                 `(eql ,name (lvar-value ,lvar))
                                                 `(same-leaf-ref-p ,name ,lvar))
                                        ,(expand-operands (cdr lvars) (cdr operands) body)))
                                    (t
                                     (push name bound-vars)
                                     (let ((expanded (expand-operands (cdr lvars) (cdr operands) body)))
                                       `(let ((,name ,(if constant `(lvar-value ,lvar) lvar)))
                                          ,(if constant-type
                                               `(when (typep ,name ',constant-type)
                                                  ,expanded)
                                               expanded)))))))
                       (case (cm-operand-kind op)
                         (:type
                          `(when (csubtypep (lvar-type ,lvar) (specifier-type ',(cm-operand-type op)))
                             ,(match-var (cm-operand-name op) t)))
                         (:constant
                          `(when (constant-lvar-p ,lvar)
                             ,(match-var (cm-operand-name op) t t (cm-operand-type op))))
                         (:plus
                          (match-var (cm-operand-name op)))
                         (:var
                          (match-var (cm-operand-name op)))
                         (:wildcard
                          (expand-operands (cdr lvars) (cdr operands) body))
                         (:literal
                          `(when (lvar-value-is ,lvar ',(cm-operand-value op))
                             ,(expand-operands (cdr lvars) (cdr operands) body)))
                         (:combination
                          `(multiple-value-bind (.name. combination .args.) (lvar-combination/cast-name-args ,lvar)
                             (declare (notinline lvar-value-is))
                             (when combination
                               (case .name.
                                 ,@(expand-combinations (cdr lvars) (cdr operands)
                                                        (cm-operand-sub-combinations op)
                                                        body))))))))
                   (funcall body)))

             (gen-1 (clauses node)
               (let ((flets nil)
                     (name-forms (make-hash-table :test 'eq))
                     name-order
                     form-groups)
                 (dolist (clause clauses)
                   (destructuring-bind (spec &body body) clause
                     (setf bound-vars nil)
                     (let* ((parsed-combs (parse-cm-spec spec unravel-casts))
                            (top-name-var (cm-operator-name-var (cm-combination-operator (first parsed-combs))))
                            (pattern-vars (collect-cm-pattern-vars parsed-combs))
                            (restp (member '&rest pattern-vars))
                            (body-fun (gensym "MATCH-BODY"))
                            (var-names (if restp
                                           (remove '&rest pattern-vars)
                                           pattern-vars))
                            (matched (lambda ()
                                       `(,@(if restp
                                               `(apply #',body-fun)
                                               `(,body-fun))
                                         combination .args. ,@var-names)))
                            (branches (expand-combinations nil nil parsed-combs matched)))
                       (push `(,body-fun (combination .args. ,@pattern-vars)
                                         (declare (ignorable combination .args. ,@var-names))
                                         (let (,@(when (and top-name-var (not (member top-name-var pattern-vars)))
                                                   `((,top-name-var .name.)))
                                               (new (progn ,@body)))
                                           (when new
                                             ,(if transform
                                                  `(,@(if restp
                                                          '(apply #'combination-match-transform)
                                                          '(combination-match-transform))
                                                    .node.
                                                    ',(if restp
                                                          (butlast pattern-vars)
                                                          pattern-vars)
                                                    new ,@var-names)
                                                  `(return-from .combination-match. new)))))
                             flets)
                       (dolist (branch branches)
                         (destructuring-bind (names form) branch
                           (when (listp names)
                             (dolist (name names)
                               (unless (gethash name name-forms)
                                 (push name name-order))
                               (push form (gethash name name-forms)))))))))
                 (setf name-order (nreverse name-order))
                 (dolist (name name-order)
                   (let* ((forms (nreverse (gethash name name-forms)))
                          (entry (assoc forms form-groups :test #'equal)))
                     (if entry
                         (push name (cdr entry))
                         (push (cons forms (list name)) form-groups))))
                 (let ((case-branches
                         (loop for (forms . names) in (nreverse form-groups)
                               collect `(,(if (member :* names)
                                              t
                                              (nreverse names))
                                         ,(if (cdr forms)
                                              `(or ,@forms)
                                              (car forms))))))
                   `(let ((.node. ,node))
                      (flet ,flets
                        (multiple-value-bind (.name. combination .args.) (combination/cast-name-args .node.)
                          (declare (ignorable combination .args.))
                          (case .name.
                            ,@case-branches))))))))
      (let ((dest (member :dest clauses)))
        `(progn
           (block .combination-match.
             ,(gen-1 (ldiff clauses dest) node)
             ,(when dest
                (gen-1 (cdr dest) `(node-dest ,node))))
           ,@(when transform
               `((delay-ir1-transform node :ir1-phases)
                 (give-up-ir1-transform))))))))

(defun combination/cast-name (node &optional cast-type)
  (typecase node
    (combination
     (let* ((fun (combination-fun node))
            (name (lvar-fun-name fun)))
       (values name node)))
    (cast
     (when (and cast-type
                (let ((type (cast-type-to-check node)))
                  (if (functionp cast-type)
                      (funcall cast-type type)
                      (eq type cast-type))))
       (combination/cast-name (lvar-uses (cast-value node)))))))

(defun generate-combination-tree (lvar)
  (let ((vars '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
        (lvar-count 0))
    (labels ((gen-lvar (lvar)
               (labels ((gen-use (node)
                          (multiple-value-bind (name combination) (combination/cast-name node)
                            (cond (combination
                                   (list* name
                                          (mapcar #'gen-lvar (combination-args combination))))
                                  ((ref-p node)
                                   (let ((leaf (ref-leaf node)))
                                     (if (constant-p leaf)
                                         (constant-value leaf)
                                         (pop vars))))
                                  ((cast-p node)
                                   (list 'the (type-specifier (cast-asserted-type node)) (gen-lvar (cast-value node))))
                                  (t
                                   (cons (format nil "LVAR~a" (incf lvar-count))
                                         (type-specifier (single-value-type (node-derived-type node)))))))))
                 (when lvar
                   (let ((uses (lvar-uses lvar)))
                     (if (listp uses)
                         (list* 'or (mapcar #'gen-use uses))
                         (gen-use uses)))))))
      (if (node-p lvar)
          (gen-lvar (node-lvar lvar))
          (gen-lvar lvar)))))

(declaim (ftype (function * (values t (or null combination) list &optional t))
                lvar-combination/cast-name-args combination/cast-name-args))
(defun lvar-combination/cast-name-args (lvar &optional cast-type)
  (if lvar
      (multiple-value-bind (name combination) (combination/cast-name (lvar-uses lvar) cast-type)
        (if name
            (values name combination (combination-args combination))
            (values nil nil nil)))
      (values nil nil nil)))

(defun combination/cast-name-args (combination)
  (if (lvar-p combination)
      (lvar-combination/cast-name-args combination)
      (multiple-value-bind (name combination) (combination/cast-name combination)
        (if name
            (values name combination (combination-args combination))
            (values nil nil nil)))))

(defun check-args (args n-args)
  (when (= (length args) n-args)
    (values-list args)))

(defun check-min-args (args n-args &optional plus-pos)
  (when (>= (length args) n-args)
    (if plus-pos
        (let ((n-after (- n-args plus-pos 1)))
          (values-list
           (append (subseq args 0 plus-pos)
                   (list (subseq args plus-pos (- (length args) n-after)))
                   (last args n-after))))
        (values-list
         (append (subseq args 0 n-args)
                 (list (nthcdr n-args args)))))))

(defun unravel-casts-typed (lvar type)
  (labels ((rec (lvar)
             (let ((use (lvar-uses lvar)))
               (if (and (cast-p use)
                        (cast-type-check use)
                        (csubtypep (single-value-type (cast-type-to-check use)) type))
                   (rec (cast-value use))
                   lvar))))
    (rec lvar)))

(defun check-typed-args (args types n-args)
  (when (= (length args) n-args)
    (values-list (loop for arg in args
                       for type in types
                       collect (unravel-casts-typed arg type)))))


(defun check-typed-min-args (args types n-args &optional plus-pos)
  (when (>= (length args) n-args)
    (if plus-pos
        (let ((n-after (- n-args plus-pos 1)))
          (values-list
           (append (subseq args 0 plus-pos)
                   (list (subseq args plus-pos (- (length args) n-after)))
                   (last args n-after))))
        (values-list
         (flet ((check-arg ()
                  (let ((arg (pop args))
                        (type (pop types)))
                    (if type
                        (unravel-casts-typed arg type)
                        arg))))
          (append (loop repeat n-args
                        while args
                        collect (check-arg))
                  (list (loop while args
                              collect (check-arg)))))))))

(defun combination-match-transform (combination vars form &rest lvars)
  (when *show-transforms-p*
    (show-transform :combination-match (generate-combination-tree (node-lvar combination)) form combination))
  ;; Handle &rest by finding literal LVARs and replacing them with variables
  (let ((restp (position '&rest vars)))
    (when restp
      (let ((rest-args (nthcdr restp lvars))
            (added-vars))
        (setf vars (subseq vars 0 restp))
        (labels ((walk (form)
                   (cond ((listp form)
                          (mapcar #'walk form))
                         ((lvar-p form)
                          (or (getf added-vars form)
                              (let ((var (gensym)))
                                (aver (member form rest-args))
                                (setf (getf added-vars form) var)
                                (push var vars)
                                (push form lvars)
                                var)))
                         (t
                          form))))
          (setf form (walk form))))))
  (labels ((skip-cast (lvar)
             (let ((dest (lvar-dest lvar)))
               (if (cast-p dest)
                   (skip-cast (node-lvar dest))
                   lvar))))
    (loop for var in vars
          for lvar in lvars
          when (lvar-p lvar) ;; ignore constants
          collect (skip-cast lvar) into lvars*
          and
          collect var into vars*
          finally (setf vars vars*
                        lvars lvars*)))
  (let ((old-args (combination-args combination)))
    (loop for lvar in lvars
          do
          (steal-lvar lvar combination lvars)
          (setf (lvar-dest lvar) combination))
    (loop for arg in old-args
          unless (member arg lvars :test #'eq)
          do (flush-dest arg))
    (setf (combination-args combination)
          lvars)
    (transform-call combination
                    `(lambda ,vars
                       (declare (ignorable ,@vars))
                       ,(unless (eq form :nil)
                          form))
                    'combination-match2))
  (throw 'give-up-ir1-transform :none))


(defun steal-lvar (lvar final-node all-lvars)
  (let ((dest (lvar-dest lvar)))
    (unless (eq final-node dest)
      (let ((next-lvar (node-lvar dest)))
        (when next-lvar
          (%delete-lvar-use dest)
          (cond ((cast-p dest)
                 (unlink-node dest))
                (t
                 (setf (combination-args dest)
                       (remove-if (lambda (l) (memq l all-lvars))
                                  (combination-args dest)))
                 (flush-combination dest)))
          (steal-lvar next-lvar final-node all-lvars))))))

;;; Are two lvars the same or one is coming from a cast?
(defun lvar-from-lvar-p (lvar casted-lvar)
  (or (eq lvar casted-lvar)
      (let ((cast (lvar-uses casted-lvar)))
        (when (cast-p cast)
          (lvar-from-lvar-p lvar (cast-value cast))))))
