;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defun constraint-propagate-back (lvar kind constraint gen consequent alternative)
  (multiple-value-bind (node nth-value) (mv-principal-lvar-ref-use lvar)
    (when (combination-p node)
      (binding* ((info (combination-fun-info node) :exit-if-null)
                 (propagate (fun-info-constraint-propagate-back info)
                            :exit-if-null))
        (funcall propagate node nth-value kind constraint gen consequent alternative)))))

(defun add-back-constraint (gen kind x y target)
  (when x
    (let ((var (ok-lvar-lambda-var x gen)))
      (if var
          (conset-add-constraint-to-eql gen kind var y nil target)
          (constraint-propagate-back x kind y gen target nil)))))

(defun numeric-contagion-constraint-back (x y gen constraint consequent &optional complex-p
                                                                                  (x-type (lvar-type x))
                                                                                  (y-type (lvar-type y)))
  (flet ((add (lvar type)
           (add-back-constraint gen 'typep lvar type consequent)))
    (let ((real-type (if complex-p ;; complex rationals multiplied by 0 will produce an integer 0.
                         (specifier-type '(and real (not (eql 0))))
                         (specifier-type 'real))))
      (cond ((csubtypep constraint (specifier-type 'rational))
             (cond ((or (and x-type
                             (csubtypep x-type real-type))
                        (csubtypep y-type real-type))
                    (add x (specifier-type 'rational))
                    (add y (specifier-type 'rational)))
                   (t
                    (add x (specifier-type '(or rational (complex rational))))
                    (add y (specifier-type '(or rational (complex rational)))))))
            ((and (csubtypep constraint (specifier-type 'double-float))
                  (cond ((not x)
                         (add y (specifier-type 'double-float))
                         t)
                        (t
                         (let ((x-double (types-equal-or-intersect x-type (specifier-type 'double-float)))
                               (y-double (types-equal-or-intersect y-type (specifier-type 'double-float))))
                           (or (when (and x-double
                                          (not y-double)
                                          (not (csubtypep x-type (specifier-type 'double-float))))
                                 (add x (specifier-type 'double-float))
                                 t)
                               (when (and y-double
                                          (not x-double)
                                          (not (csubtypep y-type (specifier-type 'double-float))))
                                 (add x (specifier-type 'double-float))
                                 t)))))))
            ((and (csubtypep constraint (specifier-type 'single-float))
                  (cond ((not x)
                         (add y (specifier-type 'single-float))
                         t)
                        (t
                         (let ((x-double (types-equal-or-intersect x-type (specifier-type 'double-float)))
                               (y-double (types-equal-or-intersect y-type (specifier-type 'double-float))))
                           (when x-double
                             (add x (specifier-type '(not double-float))))
                           (when y-double
                             (add y (specifier-type '(not double-float))))
                           nil)))))
            ((and (not x)
                  (csubtypep constraint (specifier-type 'float)))
             (add y (specifier-type 'float)))
            ((and (not x)
                  (csubtypep constraint (specifier-type 'complex)))
             (add y (specifier-type 'complex)))
            ((csubtypep constraint (specifier-type 'real))
             (let ((x-realp (csubtypep x-type (specifier-type 'real)))
                   (y-realp (csubtypep y-type (specifier-type 'real))))
               (cond ((and x-realp
                           (not y-realp))
                      (add y (specifier-type 'real)))
                     ((and y-realp
                           (not x-realp))
                      (add x (specifier-type 'real))))))))))

(defoptimizer (+ constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     ;; (integerp (+ integer y)) means Y is an integer too.
     ;; (integerp (+ y-real x-real)) means X and Y are rational.
     (flet ((add (lvar type)
              (add-back-constraint gen 'typep lvar type consequent)))
       (cond ((and (csubtypep constraint (specifier-type 'integer))
                   (let ((x-integerp (csubtypep (lvar-type x) (specifier-type 'integer)))
                         (y-integerp (csubtypep (lvar-type y) (specifier-type 'integer))))
                     (flet ((int (c-interval x y)
                              (let* ((y-interval (type-approximate-interval (lvar-type y) t))
                                     (int (and c-interval y-interval
                                               (interval-sub c-interval y-interval))))
                                (add x (specifier-type (if int
                                                           `(integer ,(or (interval-low int) '*)
                                                                     ,(or (interval-high int) '*))
                                                           'integer))))))
                       (when (or y-integerp x-integerp)
                         (let ((interval (type-approximate-interval constraint t)))
                           (int interval y x)
                           (int interval x y))
                         t)))))
             (t
              (numeric-contagion-constraint-back x y gen constraint consequent)))))))

(defun -constraint-propagate-back (x y x-type y-type kind constraint gen consequent)
  (case kind
    (typep
     (flet ((add (lvar type)
              (add-back-constraint gen 'typep lvar type consequent)))
       (cond ((and (csubtypep constraint (specifier-type 'integer))
                   (let ((x-integerp (csubtypep x-type (specifier-type 'integer)))
                         (y-integerp (csubtypep y-type (specifier-type 'integer))))
                     (when (or y-integerp x-integerp)
                       (let ((c-interval (type-approximate-interval constraint t)))
                         (let* ((y-interval (type-approximate-interval y-type t))
                                (int (and c-interval
                                          y-interval
                                          (interval-add c-interval y-interval))))
                           (add x (specifier-type (if int
                                                      `(integer ,(or (interval-low int) '*)
                                                                ,(or (interval-high int) '*))
                                                      'integer))))
                         (let* ((x-interval (type-approximate-interval x-type t))
                                (int (and c-interval
                                          x-interval
                                          (interval-sub x-interval c-interval))))
                           (add y (specifier-type (if int
                                                      `(integer ,(or (interval-low int) '*)
                                                                ,(or (interval-high int) '*))
                                                      'integer)))))
                       t))))
             (t
              (numeric-contagion-constraint-back x y gen constraint consequent nil x-type y-type)))))))

(defoptimizer (- constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (-constraint-propagate-back x y (lvar-type x) (lvar-type y) kind constraint gen consequent))

(defoptimizer (* constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     (flet ((add (lvar type)
              (add-back-constraint gen 'typep lvar type consequent)))
       (let ((complex-p (or (types-equal-or-intersect (lvar-type x) (specifier-type 'complex))
                            (types-equal-or-intersect (lvar-type y) (specifier-type 'complex)))))
         (cond ((and
                 (csubtypep constraint (specifier-type 'integer))
                 (let* ((rational-type (if complex-p
                                           (specifier-type '(and rational (not (eql 0))))
                                           (specifier-type 'rational)))
                        (x-rationalp (csubtypep (lvar-type x) rational-type))
                        (y-rationalp (csubtypep (lvar-type y) rational-type)))
                   (flet ((int (c-interval x y)
                            (let* ((y-interval (type-approximate-interval (lvar-type y) t))
                                   (int (and c-interval
                                             y-interval
                                             (interval-div c-interval y-interval))))
                              (add x (specifier-type (if int
                                                         `(rational ,(or (interval-low int) '*)
                                                                    ,(or (interval-high int) '*))
                                                         'rational))))))
                     (when (or y-rationalp x-rationalp)
                       (let ((interval (type-approximate-interval constraint t))
                             (x-zerop (types-equal-or-intersect (lvar-type x) (specifier-type '(eql 0))))
                             (y-zerop (types-equal-or-intersect (lvar-type y) (specifier-type '(eql 0)))))
                         (cond ((not interval)
                                nil)
                               ((and (interval-contains-p 0 interval)
                                     (or x-zerop y-zerop))
                                ;; If one is not zero the other must include a zero
                                (if x-zerop
                                    (add y (specifier-type 'rational))
                                    (int interval y x))
                                (if y-zerop
                                    (add x (specifier-type 'rational))
                                    (int interval x y))
                                t)
                               (t
                                (int interval y x)
                                (int interval x y)
                                t))))))))
               (t
                (numeric-contagion-constraint-back x y gen constraint consequent complex-p))))))))

(defoptimizer (/ constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     (numeric-contagion-constraint-back x y gen constraint consequent))))

(defoptimizers constraint-propagate-back (car cdr) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     (unless (types-equal-or-intersect constraint (specifier-type 'null))
       (let ((var (ok-lvar-lambda-var x gen)))
         (when var
           (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not null)) nil consequent)))))))

;;; If the remainder is non-zero then X can't be zero.
(defoptimizer (truncate constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (let ((var (ok-lvar-lambda-var x gen)))
    (cond
      ((and var
            (eql nth-value 1)
            (csubtypep (lvar-type x) (specifier-type 'integer))
            (csubtypep (lvar-type y) (specifier-type 'integer)))
       (case kind
         (eql
          (when (and (constant-p constraint)
                     (eql (constant-value constraint) 0)
                     alternative)
            (conset-add-constraint-to-eql gen 'typep var (specifier-type '(and integer (not (eql 0)))) nil alternative)))
         (>
          (when (csubtypep (lvar-type constraint) (specifier-type '(integer 0)))
            (conset-add-constraint-to-eql gen 'typep var (specifier-type '(integer 1)) nil consequent)))))
      ((and (eq kind 'typep)
            (not nth-value))
       (flet ((add (lvar type)
                (add-back-constraint gen 'typep lvar type consequent)))
         (cond ((and
                 (csubtypep constraint (specifier-type 'integer))
                 (csubtypep (lvar-type x) (specifier-type 'integer))
                 (csubtypep (lvar-type y) (specifier-type 'integer)))
                (let ((c-interval (type-approximate-interval constraint t))
                      (y-interval (type-approximate-interval (lvar-type y) t)))
                  (when (and c-interval y-interval)
                    (let ((c-low (interval-low c-interval))
                          (c-high (interval-high c-interval))
                          (y-low (interval-low y-interval))
                          (y-high (interval-high y-interval)))
                      (when (and c-low c-high
                                 y-low y-high
                                 (>= y-low 0))
                        (add x (specifier-type `(integer ,(if (<= c-low 0)
                                                              (- (* c-low y-high) (1- y-high))
                                                              (* c-low y-low))
                                                         ,(if (< c-high 0)
                                                              (* c-high y-low)
                                                              (+ (* c-high y-high) (1- y-high)))))))))))))))))

(defoptimizer (unary-truncate constraint-propagate-back) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     (cond ((csubtypep constraint (specifier-type 'integer))
            (add-back-constraint gen 'typep x (specifier-type 'integer) consequent))
           (t
            (numeric-contagion-constraint-back nil x gen constraint consequent nil nil (lvar-type x)))))))

(defoptimizer (%negate constraint-propagate-back) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (<
     (when (and (csubtypep (lvar-type x) (specifier-type 'rational))
                (csubtypep (lvar-type constraint) (specifier-type 'rational)))
       (let ((range (type-approximate-interval (lvar-type constraint))))
         (when (and range
                    (numberp (interval-high range)))
           (add-back-constraint gen 'typep x (specifier-type `(rational (,(- (interval-high range))))) consequent)))))
    (typep
     (-constraint-propagate-back nil x (specifier-type '(eql 0)) (lvar-type x) kind constraint gen consequent))))

(defoptimizer (abs constraint-propagate-back) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     (flet ((add (lvar type)
              (add-back-constraint gen 'typep lvar type consequent)))
       (cond ((csubtypep constraint (specifier-type 'integer))
              (let ((int (type-approximate-interval constraint t)))
                (add x (specifier-type (if (and int
                                                (typep (interval-high int) 'unsigned-byte))
                                           `(integer ,(- (interval-high int))
                                                     ,(interval-high int))
                                           'integer)))
                t))
             ((csubtypep constraint (specifier-type 'rational))
              (add x (specifier-type 'rational)))
             ((csubtypep constraint (specifier-type 'float))
              (add x (specifier-type '(or complex float)))))))))

(defoptimizer (char-code constraint-propagate-back) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value))
  (case kind
    ((< > eq)
     (when (csubtypep (lvar-type constraint) (specifier-type 'rational))
       (let ((range (type-approximate-interval (lvar-type constraint))))
         (when (and range
                    (numberp (interval-high range)))
           (let ((var (ok-lvar-lambda-var x gen))
                 (low (interval-low range))
                 (high (interval-high range)))
             (when var
               (interval-high range)
               (case kind
                 (<
                  (when (and (numberp high)
                             (< high (1- char-code-limit)))
                    (conset-add-constraint-to-eql gen 'typep var
                                                  (if (<= high 0)
                                                      *empty-type*
                                                      (specifier-type `(character-set ((0 . ,(1- high))))))
                                                  nil
                                                  consequent))
                  (when (and alternative
                             (numberp low)
                             (> low 0))
                    (conset-add-constraint-to-eql gen 'typep var
                                                  (specifier-type `(character-set ((,low . #.(1- char-code-limit)))))
                                                  nil
                                                  alternative)))
                 (>
                  (when (and (numberp low)
                             (> low 0))
                    (conset-add-constraint-to-eql gen 'typep var
                                                  (if (>= low (1- char-code-limit))
                                                      *empty-type*
                                                      (specifier-type `(character-set ((,(1+ low) . #.(1- char-code-limit))))))
                                                  nil
                                                  consequent))
                  (when (and alternative
                             (numberp high)
                             (< high (1- char-code-limit)))
                    (conset-add-constraint-to-eql gen 'typep var
                                                  (specifier-type `(character-set ((0 . ,high))))
                                                  nil alternative)))
                 (eq
                  (let ((low (if (numberp low)
                                 low
                                 0))
                        (high (if (numberp high)
                                  high
                                  (1- char-code-limit))))
                    (when (and (> low 0)
                               (< high (1- char-code-limit)))
                      (let ((type (specifier-type `(character-set ((,low . ,high))))))
                        (conset-add-constraint-to-eql gen 'typep var type nil consequent)
                        (conset-add-constraint-to-eql gen 'typep var type t alternative))))))))))))))

(defoptimizer (length constraint-propagate-back) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value))
  (case kind
    (>
     (when (csubtypep (lvar-type constraint) (specifier-type '(real 0)))
       (let ((var (ok-lvar-lambda-var x gen)))
         (when var
           (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not null))
                                         nil consequent)
           (when (and alternative
                      (csubtypep (lvar-type constraint) (specifier-type '(real 0 (1)))))
             (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not cons))
                                           nil alternative))))))
    (<
     (when (csubtypep (lvar-type constraint) (specifier-type '(real (0))))
       (let ((var (ok-lvar-lambda-var x gen)))
         (when var
           (when (csubtypep (lvar-type constraint) (specifier-type '(real 0 1)))
             (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not cons))
                                           nil consequent))
           (when alternative
             (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not null))
                                           nil alternative))))))
    (eq
     (cond ((not (types-equal-or-intersect (lvar-type constraint) (specifier-type '(eql 0))))
            (let ((var (ok-lvar-lambda-var x gen)))
              (when var
                (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not null))
                                              nil consequent))))
           ((eq (lvar-type constraint) (specifier-type '(eql 0)))
            (let ((var (ok-lvar-lambda-var x gen)))
              (when var
                (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not cons))
                                              nil consequent)
                (when alternative
                  (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not null))
                                                nil alternative)))))))))
