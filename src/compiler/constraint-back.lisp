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

(defoptimizer (+ constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     ;; (integerp (+ integer y)) means Y is an integer too.
     ;; (integerp (+ y-real x-real)) means X and Y are rational.
     (flet ((add (lvar type)
              (let ((var (ok-lvar-lambda-var lvar gen)))
                (when var
                  (conset-add-constraint-to-eql gen 'typep var type nil consequent)))))
       (cond ((csubtypep constraint (specifier-type 'integer))
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
                 (cond ((or y-integerp x-integerp)
                        (let ((interval (type-approximate-interval constraint t)))
                          (int interval y x)
                          (int interval x y)))
                       ((or (csubtypep (lvar-type x) (specifier-type 'real))
                            (csubtypep (lvar-type y) (specifier-type 'real)))
                        (add x (specifier-type 'rational))
                        (add y (specifier-type 'rational)))))))
             ((csubtypep constraint (specifier-type 'real))
              (let ((x-realp (csubtypep (lvar-type x) (specifier-type 'real)))
                    (y-realp (csubtypep (lvar-type y) (specifier-type 'real))))
                (cond ((and x-realp
                            (not y-realp))
                       (add y (specifier-type 'real)))
                      ((and y-realp
                            (not x-realp))
                       (add x (specifier-type 'real)))))))))))

(defoptimizer (- constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     (flet ((add (lvar type)
              (let ((var (ok-lvar-lambda-var lvar gen)))
                (when var
                  (conset-add-constraint-to-eql gen 'typep var type nil consequent)))))
       (cond ((csubtypep constraint (specifier-type 'integer))
              (let ((x-integerp (csubtypep (lvar-type x) (specifier-type 'integer)))
                    (y-integerp (csubtypep (lvar-type y) (specifier-type 'integer))))
                (cond ((or y-integerp x-integerp)
                       (let ((c-interval (type-approximate-interval constraint t)))
                         (let* ((y-interval (type-approximate-interval (lvar-type y) t))
                                (int (and c-interval
                                          y-interval
                                          (interval-add c-interval y-interval))))
                           (add x (specifier-type (if int
                                                      `(integer ,(or (interval-low int) '*)
                                                                ,(or (interval-high int) '*))
                                                      'integer))))
                         (let* ((x-interval (type-approximate-interval (lvar-type x) t))
                                (int (and c-interval
                                          x-interval
                                          (interval-sub x-interval c-interval))))
                           (add y (specifier-type (if int
                                                      `(integer ,(or (interval-low int) '*)
                                                                ,(or (interval-high int) '*))
                                                      'integer))))))
                      ((or (csubtypep (lvar-type x) (specifier-type 'real))
                           (csubtypep (lvar-type y) (specifier-type 'real)))
                       (add x (specifier-type 'rational))
                       (add y (specifier-type 'rational))))))
             ((csubtypep constraint (specifier-type 'real))
              (let ((x-realp (csubtypep (lvar-type x) (specifier-type 'real)))
                    (y-realp (csubtypep (lvar-type y) (specifier-type 'real))))
                (cond ((and x-realp
                            (not y-realp))
                       (add y (specifier-type 'real)))
                      ((and y-realp
                            (not x-realp))
                       (add x (specifier-type 'real)))))))))))

(defoptimizer (* constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     (flet ((add (lvar type)
              (let ((var (ok-lvar-lambda-var lvar gen)))
                (when var
                  (conset-add-constraint-to-eql gen 'typep var type nil consequent)))))
       (let* ((complex-p (or (types-equal-or-intersect (lvar-type x) (specifier-type 'complex))
                             (types-equal-or-intersect (lvar-type x) (specifier-type 'complex))))
              ;; complex rationals multiplied by 0 will produce an integer 0.
              (real-type (if complex-p
                             (specifier-type '(and real (not (eql 0))))
                             (specifier-type 'real))))
         (cond ((csubtypep constraint (specifier-type 'integer))
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
                    (cond ((or y-rationalp x-rationalp)
                           (let ((interval (type-approximate-interval constraint t))
                                 (x-zerop (types-equal-or-intersect (lvar-type x) (specifier-type '(eql 0))))
                                 (y-zerop (types-equal-or-intersect (lvar-type y) (specifier-type '(eql 0)))))
                             (cond ((not interval))
                                   ((and (interval-contains-p 0 interval)
                                         (or x-zerop y-zerop))
                                    ;; If one is not zero the other must include a zero
                                    (if x-zerop
                                        (add y (specifier-type 'rational))
                                        (int interval y x))
                                    (if y-zerop
                                        (add x (specifier-type 'rational))
                                        (int interval x y)))
                                   (t
                                    (int interval y x)
                                    (int interval x y)))))
                          ((or (csubtypep (lvar-type x) real-type)
                               (csubtypep (lvar-type y) real-type))
                           (add x (specifier-type 'rational))
                           (add y (specifier-type 'rational)))))))
               ((csubtypep constraint (specifier-type 'real))
                (let ((x-realp (csubtypep (lvar-type x) real-type))
                      (y-realp (csubtypep (lvar-type y) real-type)))
                  (cond ((and x-realp
                              (not y-realp))
                         (add y (specifier-type 'real)))
                        ((and y-realp
                              (not x-realp))
                         (add x (specifier-type 'real))))))))))))

(defoptimizer (car constraint-propagate-back) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (typep
     (unless (types-equal-or-intersect constraint (specifier-type 'null))
       (let ((var (ok-lvar-lambda-var x gen)))
         (when var
           (conset-add-constraint-to-eql gen 'typep var (specifier-type '(not null)) nil consequent)))))))

(setf (fun-info-constraint-propagate-back (fun-info-or-lose 'cdr)) #'car-constraint-propagate-back-optimizer)

;;; If the remainder is non-zero then X can't be zero.
(defoptimizer (truncate constraint-propagate-back) ((x y) node nth-value kind constraint gen consequent alternative)
  (let ((var (ok-lvar-lambda-var x gen)))
   (when (and var
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
          (conset-add-constraint-to-eql gen 'typep var (specifier-type '(integer 1)) nil consequent)))))))

(defoptimizer (%negate constraint-propagate-back) ((x) node nth-value kind constraint gen consequent alternative)
  (declare (ignore nth-value alternative))
  (case kind
    (<
     (when (and (csubtypep (lvar-type x) (specifier-type 'rational))
                (csubtypep (lvar-type constraint) (specifier-type 'rational)))
       (let ((range (type-approximate-interval (lvar-type constraint))))
         (when (and range
                    (numberp (interval-high range)))
           (let ((var (ok-lvar-lambda-var x gen)))
             (when var
               (conset-add-constraint-to-eql gen 'typep var (specifier-type `(rational (,(- (interval-high range)))))
                                             nil consequent)))))))))

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
