;;;; This file contains macro-like source transformations which
;;;; convert uses of certain functions into the canonical form desired
;;;; within the compiler. FIXME: and other IR1 transforms and stuff.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Convert into an IF so that IF optimizations will eliminate redundant
;;; negations.
(define-source-transform not (x) `(if ,x nil t))
(define-source-transform null (x) `(if ,x nil t))

;;; ENDP is just NULL with a LIST assertion. The assertion will be
;;; optimized away when SAFETY optimization is low; hopefully that
;;; is consistent with ANSI's "should return an error".
(define-source-transform endp (x) `(null (the list ,x)))

;;; We turn IDENTITY into PROG1 so that it is obvious that it just
;;; returns the first value of its argument. Ditto for VALUES with one
;;; arg.
(define-source-transform identity (x) `(prog1 ,x))
(define-source-transform values (x) `(prog1 ,x))

;;; Bind the value and make a closure that returns it.
(define-source-transform constantly (value)
  (with-unique-names (rest n-value)
    `(let ((,n-value ,value))
      (lambda (&rest ,rest)
        (declare (ignore ,rest))
        ,n-value))))

;;; If the function has a known number of arguments, then return a
;;; lambda with the appropriate fixed number of args. If the
;;; destination is a FUNCALL, then do the &REST APPLY thing, and let
;;; MV optimization figure things out.
(deftransform complement ((fun) * * :node node)
  "open code"
  (multiple-value-bind (min max)
      (fun-type-nargs (lvar-type fun))
    (cond
     ((and min (eql min max))
      (let ((dums (make-gensym-list min)))
        `#'(lambda ,dums (not (funcall fun ,@dums)))))
     ((awhen (node-lvar node)
        (let ((dest (lvar-dest it)))
          (and (combination-p dest)
               (eq (combination-fun dest) it))))
      '#'(lambda (&rest args)
           (not (apply fun args))))
     (t
      (give-up-ir1-transform
       "The function doesn't have a fixed argument count.")))))

;;;; list hackery

;;; Translate CxR into CAR/CDR combos.
(defun source-transform-cxr (form)
  (if (/= (length form) 2)
      (values nil t)
      (let* ((name (car form))
             (string (symbol-name
                      (etypecase name
                        (symbol name)
                        (leaf (leaf-source-name name))))))
        (do ((i (- (length string) 2) (1- i))
             (res (cadr form)
                  `(,(ecase (char string i)
                       (#\A 'car)
                       (#\D 'cdr))
                    ,res)))
            ((zerop i) res)))))

;;; Make source transforms to turn CxR forms into combinations of CAR
;;; and CDR. ANSI specifies that everything up to 4 A/D operations is
;;; defined.
(/show0 "about to set CxR source transforms")
(loop for i of-type index from 2 upto 4 do
      ;; Iterate over BUF = all names CxR where x = an I-element
      ;; string of #\A or #\D characters.
      (let ((buf (make-string (+ 2 i))))
        (setf (aref buf 0) #\C
              (aref buf (1+ i)) #\R)
        (dotimes (j (ash 2 i))
          (declare (type index j))
          (dotimes (k i)
            (declare (type index k))
            (setf (aref buf (1+ k))
                  (if (logbitp k j) #\A #\D)))
          (setf (info :function :source-transform (intern buf))
                #'source-transform-cxr))))
(/show0 "done setting CxR source transforms")

;;; Turn FIRST..FOURTH and REST into the obvious synonym, assuming
;;; whatever is right for them is right for us. FIFTH..TENTH turn into
;;; Nth, which can be expanded into a CAR/CDR later on if policy
;;; favors it.
(define-source-transform first (x) `(car ,x))
(define-source-transform rest (x) `(cdr ,x))
(define-source-transform second (x) `(cadr ,x))
(define-source-transform third (x) `(caddr ,x))
(define-source-transform fourth (x) `(cadddr ,x))
(define-source-transform fifth (x) `(nth 4 ,x))
(define-source-transform sixth (x) `(nth 5 ,x))
(define-source-transform seventh (x) `(nth 6 ,x))
(define-source-transform eighth (x) `(nth 7 ,x))
(define-source-transform ninth (x) `(nth 8 ,x))
(define-source-transform tenth (x) `(nth 9 ,x))

;;; LIST with one arg is an extremely common operation (at least inside
;;; SBCL itself); translate it to CONS to take advantage of common
;;; allocation routines.
(define-source-transform list (&rest args)
  (case (length args)
    (1 `(cons ,(first args) nil))
    (t (values nil t))))

;;; And similarly for LIST*.
(define-source-transform list* (arg &rest others)
  (cond ((not others) arg)
        ((not (cdr others)) `(cons ,arg ,(car others)))
        (t (values nil t))))

(defoptimizer (list* derive-type) ((arg &rest args))
  (if args
      (specifier-type 'cons)
      (lvar-type arg)))

;;; Translate RPLACx to LET and SETF.
(define-source-transform rplaca (x y)
  (once-only ((n-x x))
    `(progn
       (setf (car ,n-x) ,y)
       ,n-x)))
(define-source-transform rplacd (x y)
  (once-only ((n-x x))
    `(progn
       (setf (cdr ,n-x) ,y)
       ,n-x)))

(define-source-transform nth (n l) `(car (nthcdr ,n ,l)))

(deftransform last ((list &optional n) (t &optional t))
  (let ((c (constant-lvar-p n)))
    (cond ((or (not n)
               (and c (eql 1 (lvar-value n))))
           '(%last1 list))
          ((and c (eql 0 (lvar-value n)))
           '(%last0 list))
          (t
           (let ((type (lvar-type n)))
             (cond ((csubtypep type (specifier-type 'fixnum))
                    '(%lastn/fixnum list n))
                   ((csubtypep type (specifier-type 'bignum))
                    '(%lastn/bignum list n))
                   (t
                    (give-up-ir1-transform "second argument type too vague"))))))))

(define-source-transform gethash (&rest args)
  (case (length args)
   (2 `(sb!impl::gethash3 ,@args nil))
   (3 `(sb!impl::gethash3 ,@args))
   (t (values nil t))))
(define-source-transform get (&rest args)
  (case (length args)
   (2 `(sb!impl::get2 ,@args))
   (3 `(sb!impl::get3 ,@args))
   (t (values nil t))))

(defvar *default-nthcdr-open-code-limit* 6)
(defvar *extreme-nthcdr-open-code-limit* 20)

(deftransform nthcdr ((n l) (unsigned-byte t) * :node node)
  "convert NTHCDR to CAxxR"
  (unless (constant-lvar-p n)
    (give-up-ir1-transform))
  (let ((n (lvar-value n)))
    (when (> n
             (if (policy node (and (= speed 3) (= space 0)))
                 *extreme-nthcdr-open-code-limit*
                 *default-nthcdr-open-code-limit*))
      (give-up-ir1-transform))

    (labels ((frob (n)
               (if (zerop n)
                   'l
                   `(cdr ,(frob (1- n))))))
      (frob n))))

;;;; arithmetic and numerology

(define-source-transform plusp (x) `(> ,x 0))
(define-source-transform minusp (x) `(< ,x 0))
(define-source-transform zerop (x) `(= ,x 0))

(define-source-transform 1+ (x) `(+ ,x 1))
(define-source-transform 1- (x) `(- ,x 1))

(define-source-transform oddp (x) `(logtest ,x 1))
(define-source-transform evenp (x) `(not (logtest ,x 1)))

;;; Note that all the integer division functions are available for
;;; inline expansion.

(macrolet ((deffrob (fun)
             `(define-source-transform ,fun (x &optional (y nil y-p))
                (declare (ignore y))
                (if y-p
                    (values nil t)
                    `(,',fun ,x 1)))))
  (deffrob truncate)
  (deffrob round)
  #-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
  (deffrob floor)
  #-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
  (deffrob ceiling))

;;; This used to be a source transform (hence the lack of restrictions
;;; on the argument types), but we make it a regular transform so that
;;; the VM has a chance to see the bare LOGTEST and potentiall choose
;;; to implement it differently.  --njf, 06-02-2006
(deftransform logtest ((x y) * *)
  `(not (zerop (logand x y))))

(deftransform logbitp
    ((index integer) (unsigned-byte (or (signed-byte #.sb!vm:n-word-bits)
                                        (unsigned-byte #.sb!vm:n-word-bits))))
  `(if (>= index #.sb!vm:n-word-bits)
       (minusp integer)
       (not (zerop (logand integer (ash 1 index))))))

(define-source-transform byte (size position)
  `(cons ,size ,position))
(define-source-transform byte-size (spec) `(car ,spec))
(define-source-transform byte-position (spec) `(cdr ,spec))
(define-source-transform ldb-test (bytespec integer)
  `(not (zerop (mask-field ,bytespec ,integer))))

;;; With the ratio and complex accessors, we pick off the "identity"
;;; case, and use a primitive to handle the cell access case.
(define-source-transform numerator (num)
  (once-only ((n-num `(the rational ,num)))
    `(if (ratiop ,n-num)
         (%numerator ,n-num)
         ,n-num)))
(define-source-transform denominator (num)
  (once-only ((n-num `(the rational ,num)))
    `(if (ratiop ,n-num)
         (%denominator ,n-num)
         1)))

;;;; interval arithmetic for computing bounds
;;;;
;;;; This is a set of routines for operating on intervals. It
;;;; implements a simple interval arithmetic package. Although SBCL
;;;; has an interval type in NUMERIC-TYPE, we choose to use our own
;;;; for two reasons:
;;;;
;;;;   1. This package is simpler than NUMERIC-TYPE.
;;;;
;;;;   2. It makes debugging much easier because you can just strip
;;;;   out these routines and test them independently of SBCL. (This is a
;;;;   big win!)
;;;;
;;;; One disadvantage is a probable increase in consing because we
;;;; have to create these new interval structures even though
;;;; numeric-type has everything we want to know. Reason 2 wins for
;;;; now.

;;; Support operations that mimic real arithmetic comparison
;;; operators, but imposing a total order on the floating points such
;;; that negative zeros are strictly less than positive zeros.
(macrolet ((def (name op)
             `(defun ,name (x y)
                (declare (real x y))
                (if (and (floatp x) (floatp y) (zerop x) (zerop y))
                    (,op (float-sign x) (float-sign y))
                    (,op x y)))))
  (def signed-zero->= >=)
  (def signed-zero-> >)
  (def signed-zero-= =)
  (def signed-zero-< <)
  (def signed-zero-<= <=))

;;; The basic interval type. It can handle open and closed intervals.
;;; A bound is open if it is a list containing a number, just like
;;; Lisp says. NIL means unbounded.
(defstruct (interval (:constructor %make-interval)
                     (:copier nil))
  low high)

(defun make-interval (&key low high)
  (labels ((normalize-bound (val)
             (cond #-sb-xc-host
                   ((and (floatp val)
                         (float-infinity-p val))
                    ;; Handle infinities.
                    nil)
                   ((or (numberp val)
                        (eq val nil))
                    ;; Handle any closed bounds.
                    val)
                   ((listp val)
                    ;; We have an open bound. Normalize the numeric
                    ;; bound. If the normalized bound is still a number
                    ;; (not nil), keep the bound open. Otherwise, the
                    ;; bound is really unbounded, so drop the openness.
                    (let ((new-val (normalize-bound (first val))))
                      (when new-val
                        ;; The bound exists, so keep it open still.
                        (list new-val))))
                   (t
                    (error "unknown bound type in MAKE-INTERVAL")))))
    (%make-interval :low (normalize-bound low)
                    :high (normalize-bound high))))

;;; Given a number X, create a form suitable as a bound for an
;;; interval. Make the bound open if OPEN-P is T. NIL remains NIL.
#!-sb-fluid (declaim (inline set-bound))
(defun set-bound (x open-p)
  (if (and x open-p) (list x) x))

;;; Apply the function F to a bound X. If X is an open bound, then
;;; the result will be open. IF X is NIL, the result is NIL.
(defun bound-func (f x)
  (declare (type function f))
  (and x
       (with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero)
         ;; With these traps masked, we might get things like infinity
         ;; or negative infinity returned. Check for this and return
         ;; NIL to indicate unbounded.
         (let ((y (funcall f (type-bound-number x))))
           (if (and (floatp y)
                    (float-infinity-p y))
               nil
               (set-bound y (consp x)))))))

(defun safe-double-coercion-p (x)
  (or (typep x 'double-float)
      (<= most-negative-double-float x most-positive-double-float)))

(defun safe-single-coercion-p (x)
  (or (typep x 'single-float)
      ;; Fix for bug 420, and related issues: during type derivation we often
      ;; end up deriving types for both
      ;;
      ;;   (some-op <int> <single>)
      ;; and
      ;;   (some-op (coerce <int> 'single-float) <single>)
      ;;
      ;; or other equivalent transformed forms. The problem with this is that
      ;; on some platforms like x86 (+ <int> <single>) is on the machine level
      ;; equivalent of
      ;;
      ;;   (coerce (+ (coerce <int> 'double-float)
      ;;              (coerce <single> 'double-float))
      ;;           'single-float)
      ;;
      ;; so if the result of (coerce <int> 'single-float) is not exact, the
      ;; derived types for the transformed forms will have an empty
      ;; intersection -- which in turn means that the compiler will conclude
      ;; that the call never returns, and all hell breaks lose when it *does*
      ;; return at runtime. (This affects not just +, but other operators are
      ;; well.)
      (and (not (typep x `(or (integer * (,most-negative-exactly-single-float-fixnum))
                              (integer (,most-positive-exactly-single-float-fixnum) *))))
           (<= most-negative-single-float x most-positive-single-float))))

;;; Apply a binary operator OP to two bounds X and Y. The result is
;;; NIL if either is NIL. Otherwise bound is computed and the result
;;; is open if either X or Y is open.
;;;
;;; FIXME: only used in this file, not needed in target runtime

;;; ANSI contaigon specifies coercion to floating point if one of the
;;; arguments is floating point. Here we should check to be sure that
;;; the other argument is within the bounds of that floating point
;;; type.

(defmacro safely-binop (op x y)
  `(cond
     ((typep ,x 'double-float)
      (when (safe-double-coercion-p ,y)
        (,op ,x ,y)))
     ((typep ,y 'double-float)
      (when (safe-double-coercion-p ,x)
        (,op ,x ,y)))
     ((typep ,x 'single-float)
      (when (safe-single-coercion-p ,y)
        (,op ,x ,y)))
     ((typep ,y 'single-float)
      (when (safe-single-coercion-p ,x)
        (,op ,x ,y)))
     (t (,op ,x ,y))))

(defmacro bound-binop (op x y)
  `(and ,x ,y
       (with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero)
         (set-bound (safely-binop ,op (type-bound-number ,x)
                                  (type-bound-number ,y))
                    (or (consp ,x) (consp ,y))))))

(defun coerce-for-bound (val type)
  (if (consp val)
      (list (coerce-for-bound (car val) type))
      (cond
        ((subtypep type 'double-float)
         (if (<= most-negative-double-float val most-positive-double-float)
             (coerce val type)))
        ((or (subtypep type 'single-float) (subtypep type 'float))
         ;; coerce to float returns a single-float
         (if (<= most-negative-single-float val most-positive-single-float)
             (coerce val type)))
        (t (coerce val type)))))

(defun coerce-and-truncate-floats (val type)
  (when val
    (if (consp val)
        (list (coerce-and-truncate-floats (car val) type))
        (cond
          ((subtypep type 'double-float)
           (if (<= most-negative-double-float val most-positive-double-float)
               (coerce val type)
               (if (< val most-negative-double-float)
                   most-negative-double-float most-positive-double-float)))
          ((or (subtypep type 'single-float) (subtypep type 'float))
           ;; coerce to float returns a single-float
           (if (<= most-negative-single-float val most-positive-single-float)
               (coerce val type)
               (if (< val most-negative-single-float)
                   most-negative-single-float most-positive-single-float)))
          (t (coerce val type))))))

;;; Convert a numeric-type object to an interval object.
(defun numeric-type->interval (x)
  (declare (type numeric-type x))
  (make-interval :low (numeric-type-low x)
                 :high (numeric-type-high x)))

(defun type-approximate-interval (type)
  (declare (type ctype type))
  (let ((types (prepare-arg-for-derive-type type))
        (result nil))
    (dolist (type types)
      (let ((type (if (member-type-p type)
                      (convert-member-type type)
                      type)))
        (unless (numeric-type-p type)
          (return-from type-approximate-interval nil))
        (let ((interval (numeric-type->interval type)))
          (setq result
                (if result
                    (interval-approximate-union result interval)
                    interval)))))
    result))

(defun copy-interval-limit (limit)
  (if (numberp limit)
      limit
      (copy-list limit)))

(defun copy-interval (x)
  (declare (type interval x))
  (make-interval :low (copy-interval-limit (interval-low x))
                 :high (copy-interval-limit (interval-high x))))

;;; Given a point P contained in the interval X, split X into two
;;; interval at the point P. If CLOSE-LOWER is T, then the left
;;; interval contains P. If CLOSE-UPPER is T, the right interval
;;; contains P. You can specify both to be T or NIL.
(defun interval-split (p x &optional close-lower close-upper)
  (declare (type number p)
           (type interval x))
  (list (make-interval :low (copy-interval-limit (interval-low x))
                       :high (if close-lower p (list p)))
        (make-interval :low (if close-upper (list p) p)
                       :high (copy-interval-limit (interval-high x)))))

;;; Return the closure of the interval. That is, convert open bounds
;;; to closed bounds.
(defun interval-closure (x)
  (declare (type interval x))
  (make-interval :low (type-bound-number (interval-low x))
                 :high (type-bound-number (interval-high x))))

;;; For an interval X, if X >= POINT, return '+. If X <= POINT, return
;;; '-. Otherwise return NIL.
(defun interval-range-info (x &optional (point 0))
  (declare (type interval x))
  (let ((lo (interval-low x))
        (hi (interval-high x)))
    (cond ((and lo (signed-zero->= (type-bound-number lo) point))
           '+)
          ((and hi (signed-zero->= point (type-bound-number hi)))
           '-)
          (t
           nil))))

;;; Test to see whether the interval X is bounded. HOW determines the
;;; test, and should be either ABOVE, BELOW, or BOTH.
(defun interval-bounded-p (x how)
  (declare (type interval x))
  (ecase how
    (above
     (interval-high x))
    (below
     (interval-low x))
    (both
     (and (interval-low x) (interval-high x)))))

;;; See whether the interval X contains the number P, taking into
;;; account that the interval might not be closed.
(defun interval-contains-p (p x)
  (declare (type number p)
           (type interval x))
  ;; Does the interval X contain the number P?  This would be a lot
  ;; easier if all intervals were closed!
  (let ((lo (interval-low x))
        (hi (interval-high x)))
    (cond ((and lo hi)
           ;; The interval is bounded
           (if (and (signed-zero-<= (type-bound-number lo) p)
                    (signed-zero-<= p (type-bound-number hi)))
               ;; P is definitely in the closure of the interval.
               ;; We just need to check the end points now.
               (cond ((signed-zero-= p (type-bound-number lo))
                      (numberp lo))
                     ((signed-zero-= p (type-bound-number hi))
                      (numberp hi))
                     (t t))
               nil))
          (hi
           ;; Interval with upper bound
           (if (signed-zero-< p (type-bound-number hi))
               t
               (and (numberp hi) (signed-zero-= p hi))))
          (lo
           ;; Interval with lower bound
           (if (signed-zero-> p (type-bound-number lo))
               t
               (and (numberp lo) (signed-zero-= p lo))))
          (t
           ;; Interval with no bounds
           t))))

;;; Determine whether two intervals X and Y intersect. Return T if so.
;;; If CLOSED-INTERVALS-P is T, the treat the intervals as if they
;;; were closed. Otherwise the intervals are treated as they are.
;;;
;;; Thus if X = [0, 1) and Y = (1, 2), then they do not intersect
;;; because no element in X is in Y. However, if CLOSED-INTERVALS-P
;;; is T, then they do intersect because we use the closure of X = [0,
;;; 1] and Y = [1, 2] to determine intersection.
(defun interval-intersect-p (x y &optional closed-intervals-p)
  (declare (type interval x y))
  (and (interval-intersection/difference (if closed-intervals-p
                                             (interval-closure x)
                                             x)
                                         (if closed-intervals-p
                                             (interval-closure y)
                                             y))
       t))

;;; Are the two intervals adjacent?  That is, is there a number
;;; between the two intervals that is not an element of either
;;; interval?  If so, they are not adjacent. For example [0, 1) and
;;; [1, 2] are adjacent but [0, 1) and (1, 2] are not because 1 lies
;;; between both intervals.
(defun interval-adjacent-p (x y)
  (declare (type interval x y))
  (flet ((adjacent (lo hi)
           ;; Check to see whether lo and hi are adjacent. If either is
           ;; nil, they can't be adjacent.
           (when (and lo hi (= (type-bound-number lo) (type-bound-number hi)))
             ;; The bounds are equal. They are adjacent if one of
             ;; them is closed (a number). If both are open (consp),
             ;; then there is a number that lies between them.
             (or (numberp lo) (numberp hi)))))
    (or (adjacent (interval-low y) (interval-high x))
        (adjacent (interval-low x) (interval-high y)))))

;;; Compute the intersection and difference between two intervals.
;;; Two values are returned: the intersection and the difference.
;;;
;;; Let the two intervals be X and Y, and let I and D be the two
;;; values returned by this function. Then I = X intersect Y. If I
;;; is NIL (the empty set), then D is X union Y, represented as the
;;; list of X and Y. If I is not the empty set, then D is (X union Y)
;;; - I, which is a list of two intervals.
;;;
;;; For example, let X = [1,5] and Y = [-1,3). Then I = [1,3) and D =
;;; [-1,1) union [3,5], which is returned as a list of two intervals.
(defun interval-intersection/difference (x y)
  (declare (type interval x y))
  (let ((x-lo (interval-low x))
        (x-hi (interval-high x))
        (y-lo (interval-low y))
        (y-hi (interval-high y)))
    (labels
        ((opposite-bound (p)
           ;; If p is an open bound, make it closed. If p is a closed
           ;; bound, make it open.
           (if (listp p)
               (first p)
               (list p)))
         (test-number (p int bound)
           ;; Test whether P is in the interval.
           (let ((pn (type-bound-number p)))
             (when (interval-contains-p pn (interval-closure int))
               ;; Check for endpoints.
               (let* ((lo (interval-low int))
                      (hi (interval-high int))
                      (lon (type-bound-number lo))
                      (hin (type-bound-number hi)))
                 (cond
                   ;; Interval may be a point.
                   ((and lon hin (= lon hin pn))
                    (and (numberp p) (numberp lo) (numberp hi)))
                   ;; Point matches the low end.
                   ;; [P] [P,?} => TRUE     [P] (P,?} => FALSE
                   ;; (P  [P,?} => TRUE      P) [P,?} => FALSE
                   ;; (P  (P,?} => TRUE      P) (P,?} => FALSE
                   ((and lon (= pn lon))
                    (or (and (numberp p) (numberp lo))
                        (and (consp p) (eq :low bound))))
                   ;; [P] {?,P] => TRUE     [P] {?,P) => FALSE
                   ;;  P) {?,P] => TRUE     (P  {?,P] => FALSE
                   ;;  P) {?,P) => TRUE     (P  {?,P) => FALSE
                   ((and hin (= pn hin))
                    (or (and (numberp p) (numberp hi))
                        (and (consp p) (eq :high bound))))
                   ;; Not an endpoint, all is well.
                   (t
                    t))))))
         (test-lower-bound (p int)
           ;; P is a lower bound of an interval.
           (if p
               (test-number p int :low)
               (not (interval-bounded-p int 'below))))
         (test-upper-bound (p int)
           ;; P is an upper bound of an interval.
           (if p
               (test-number p int :high)
               (not (interval-bounded-p int 'above)))))
      (let ((x-lo-in-y (test-lower-bound x-lo y))
            (x-hi-in-y (test-upper-bound x-hi y))
            (y-lo-in-x (test-lower-bound y-lo x))
            (y-hi-in-x (test-upper-bound y-hi x)))
        (cond ((or x-lo-in-y x-hi-in-y y-lo-in-x y-hi-in-x)
               ;; Intervals intersect. Let's compute the intersection
               ;; and the difference.
               (multiple-value-bind (lo left-lo left-hi)
                   (cond (x-lo-in-y (values x-lo y-lo (opposite-bound x-lo)))
                         (y-lo-in-x (values y-lo x-lo (opposite-bound y-lo))))
                 (multiple-value-bind (hi right-lo right-hi)
                     (cond (x-hi-in-y
                            (values x-hi (opposite-bound x-hi) y-hi))
                           (y-hi-in-x
                            (values y-hi (opposite-bound y-hi) x-hi)))
                   (values (make-interval :low lo :high hi)
                           (list (make-interval :low left-lo
                                                :high left-hi)
                                 (make-interval :low right-lo
                                                :high right-hi))))))
              (t
               (values nil (list x y))))))))

;;; If intervals X and Y intersect, return a new interval that is the
;;; union of the two. If they do not intersect, return NIL.
(defun interval-merge-pair (x y)
  (declare (type interval x y))
  ;; If x and y intersect or are adjacent, create the union.
  ;; Otherwise return nil
  (when (or (interval-intersect-p x y)
            (interval-adjacent-p x y))
    (flet ((select-bound (x1 x2 min-op max-op)
             (let ((x1-val (type-bound-number x1))
                   (x2-val (type-bound-number x2)))
               (cond ((and x1 x2)
                      ;; Both bounds are finite. Select the right one.
                      (cond ((funcall min-op x1-val x2-val)
                             ;; x1 is definitely better.
                             x1)
                            ((funcall max-op x1-val x2-val)
                             ;; x2 is definitely better.
                             x2)
                            (t
                             ;; Bounds are equal. Select either
                             ;; value and make it open only if
                             ;; both were open.
                             (set-bound x1-val (and (consp x1) (consp x2))))))
                     (t
                      ;; At least one bound is not finite. The
                      ;; non-finite bound always wins.
                      nil)))))
      (let* ((x-lo (copy-interval-limit (interval-low x)))
             (x-hi (copy-interval-limit (interval-high x)))
             (y-lo (copy-interval-limit (interval-low y)))
             (y-hi (copy-interval-limit (interval-high y))))
        (make-interval :low (select-bound x-lo y-lo #'< #'>)
                       :high (select-bound x-hi y-hi #'> #'<))))))

;;; return the minimal interval, containing X and Y
(defun interval-approximate-union (x y)
  (cond ((interval-merge-pair x y))
        ((interval-< x y)
         (make-interval :low (copy-interval-limit (interval-low x))
                        :high (copy-interval-limit (interval-high y))))
        (t
         (make-interval :low (copy-interval-limit (interval-low y))
                        :high (copy-interval-limit (interval-high x))))))

;;; basic arithmetic operations on intervals. We probably should do
;;; true interval arithmetic here, but it's complicated because we
;;; have float and integer types and bounds can be open or closed.

;;; the negative of an interval
(defun interval-neg (x)
  (declare (type interval x))
  (make-interval :low (bound-func #'- (interval-high x))
                 :high (bound-func #'- (interval-low x))))

;;; Add two intervals.
(defun interval-add (x y)
  (declare (type interval x y))
  (make-interval :low (bound-binop + (interval-low x) (interval-low y))
                 :high (bound-binop + (interval-high x) (interval-high y))))

;;; Subtract two intervals.
(defun interval-sub (x y)
  (declare (type interval x y))
  (make-interval :low (bound-binop - (interval-low x) (interval-high y))
                 :high (bound-binop - (interval-high x) (interval-low y))))

;;; Multiply two intervals.
(defun interval-mul (x y)
  (declare (type interval x y))
  (flet ((bound-mul (x y)
           (cond ((or (null x) (null y))
                  ;; Multiply by infinity is infinity
                  nil)
                 ((or (and (numberp x) (zerop x))
                      (and (numberp y) (zerop y)))
                  ;; Multiply by closed zero is special. The result
                  ;; is always a closed bound. But don't replace this
                  ;; with zero; we want the multiplication to produce
                  ;; the correct signed zero, if needed. Use SIGNUM
                  ;; to avoid trying to multiply huge bignums with 0.0.
                  (* (signum (type-bound-number x)) (signum (type-bound-number y))))
                 ((or (and (floatp x) (float-infinity-p x))
                      (and (floatp y) (float-infinity-p y)))
                  ;; Infinity times anything is infinity
                  nil)
                 (t
                  ;; General multiply. The result is open if either is open.
                  (bound-binop * x y)))))
    (let ((x-range (interval-range-info x))
          (y-range (interval-range-info y)))
      (cond ((null x-range)
             ;; Split x into two and multiply each separately
             (destructuring-bind (x- x+) (interval-split 0 x t t)
               (interval-merge-pair (interval-mul x- y)
                                    (interval-mul x+ y))))
            ((null y-range)
             ;; Split y into two and multiply each separately
             (destructuring-bind (y- y+) (interval-split 0 y t t)
               (interval-merge-pair (interval-mul x y-)
                                    (interval-mul x y+))))
            ((eq x-range '-)
             (interval-neg (interval-mul (interval-neg x) y)))
            ((eq y-range '-)
             (interval-neg (interval-mul x (interval-neg y))))
            ((and (eq x-range '+) (eq y-range '+))
             ;; If we are here, X and Y are both positive.
             (make-interval
              :low (bound-mul (interval-low x) (interval-low y))
              :high (bound-mul (interval-high x) (interval-high y))))
            (t
             (bug "excluded case in INTERVAL-MUL"))))))

;;; Divide two intervals.
(defun interval-div (top bot)
  (declare (type interval top bot))
  (flet ((bound-div (x y y-low-p)
           ;; Compute x/y
           (cond ((null y)
                  ;; Divide by infinity means result is 0. However,
                  ;; we need to watch out for the sign of the result,
                  ;; to correctly handle signed zeros. We also need
                  ;; to watch out for positive or negative infinity.
                  (if (floatp (type-bound-number x))
                      (if y-low-p
                          (- (float-sign (type-bound-number x) 0.0))
                          (float-sign (type-bound-number x) 0.0))
                      0))
                 ((zerop (type-bound-number y))
                  ;; Divide by zero means result is infinity
                  nil)
                 ((and (numberp x) (zerop x))
                  ;; Zero divided by anything is zero.
                  x)
                 (t
                  (bound-binop / x y)))))
    (let ((top-range (interval-range-info top))
          (bot-range (interval-range-info bot)))
      (cond ((null bot-range)
             ;; The denominator contains zero, so anything goes!
             (make-interval :low nil :high nil))
            ((eq bot-range '-)
             ;; Denominator is negative so flip the sign, compute the
             ;; result, and flip it back.
             (interval-neg (interval-div top (interval-neg bot))))
            ((null top-range)
             ;; Split top into two positive and negative parts, and
             ;; divide each separately
             (destructuring-bind (top- top+) (interval-split 0 top t t)
               (interval-merge-pair (interval-div top- bot)
                                    (interval-div top+ bot))))
            ((eq top-range '-)
             ;; Top is negative so flip the sign, divide, and flip the
             ;; sign of the result.
             (interval-neg (interval-div (interval-neg top) bot)))
            ((and (eq top-range '+) (eq bot-range '+))
             ;; the easy case
             (make-interval
              :low (bound-div (interval-low top) (interval-high bot) t)
              :high (bound-div (interval-high top) (interval-low bot) nil)))
            (t
             (bug "excluded case in INTERVAL-DIV"))))))

;;; Apply the function F to the interval X. If X = [a, b], then the
;;; result is [f(a), f(b)]. It is up to the user to make sure the
;;; result makes sense. It will if F is monotonic increasing (or
;;; non-decreasing).
(defun interval-func (f x)
  (declare (type function f)
           (type interval x))
  (let ((lo (bound-func f (interval-low x)))
        (hi (bound-func f (interval-high x))))
    (make-interval :low lo :high hi)))

;;; Return T if X < Y. That is every number in the interval X is
;;; always less than any number in the interval Y.
(defun interval-< (x y)
  (declare (type interval x y))
  ;; X < Y only if X is bounded above, Y is bounded below, and they
  ;; don't overlap.
  (when (and (interval-bounded-p x 'above)
             (interval-bounded-p y 'below))
    ;; Intervals are bounded in the appropriate way. Make sure they
    ;; don't overlap.
    (let ((left (interval-high x))
          (right (interval-low y)))
      (cond ((> (type-bound-number left)
                (type-bound-number right))
             ;; The intervals definitely overlap, so result is NIL.
             nil)
            ((< (type-bound-number left)
                (type-bound-number right))
             ;; The intervals definitely don't touch, so result is T.
             t)
            (t
             ;; Limits are equal. Check for open or closed bounds.
             ;; Don't overlap if one or the other are open.
             (or (consp left) (consp right)))))))

;;; Return T if X >= Y. That is, every number in the interval X is
;;; always greater than any number in the interval Y.
(defun interval->= (x y)
  (declare (type interval x y))
  ;; X >= Y if lower bound of X >= upper bound of Y
  (when (and (interval-bounded-p x 'below)
             (interval-bounded-p y 'above))
    (>= (type-bound-number (interval-low x))
        (type-bound-number (interval-high y)))))

;;; Return T if X = Y.
(defun interval-= (x y)
  (declare (type interval x y))
  (and (interval-bounded-p x 'both)
       (interval-bounded-p y 'both)
       (flet ((bound (v)
                (if (numberp v)
                    v
                    ;; Open intervals cannot be =
                    (return-from interval-= nil))))
         ;; Both intervals refer to the same point
         (= (bound (interval-high x)) (bound (interval-low x))
            (bound (interval-high y)) (bound (interval-low y))))))

;;; Return T if X /= Y
(defun interval-/= (x y)
  (not (interval-intersect-p x y)))

;;; Return an interval that is the absolute value of X. Thus, if
;;; X = [-1 10], the result is [0, 10].
(defun interval-abs (x)
  (declare (type interval x))
  (case (interval-range-info x)
    (+
     (copy-interval x))
    (-
     (interval-neg x))
    (t
     (destructuring-bind (x- x+) (interval-split 0 x t t)
       (interval-merge-pair (interval-neg x-) x+)))))

;;; Compute the square of an interval.
(defun interval-sqr (x)
  (declare (type interval x))
  (interval-func (lambda (x) (* x x))
                 (interval-abs x)))

;;;; numeric DERIVE-TYPE methods

;;; a utility for defining derive-type methods of integer operations. If
;;; the types of both X and Y are integer types, then we compute a new
;;; integer type with bounds determined Fun when applied to X and Y.
;;; Otherwise, we use NUMERIC-CONTAGION.
(defun derive-integer-type-aux (x y fun)
  (declare (type function fun))
  (if (and (numeric-type-p x) (numeric-type-p y)
           (eq (numeric-type-class x) 'integer)
           (eq (numeric-type-class y) 'integer)
           (eq (numeric-type-complexp x) :real)
           (eq (numeric-type-complexp y) :real))
      (multiple-value-bind (low high) (funcall fun x y)
        (make-numeric-type :class 'integer
                           :complexp :real
                           :low low
                           :high high))
      (numeric-contagion x y)))

(defun derive-integer-type (x y fun)
  (declare (type lvar x y) (type function fun))
  (let ((x (lvar-type x))
        (y (lvar-type y)))
    (derive-integer-type-aux x y fun)))

;;; simple utility to flatten a list
(defun flatten-list (x)
  (labels ((flatten-and-append (tree list)
             (cond ((null tree) list)
                   ((atom tree) (cons tree list))
                   (t (flatten-and-append
                       (car tree) (flatten-and-append (cdr tree) list))))))
    (flatten-and-append x nil)))

;;; Take some type of lvar and massage it so that we get a list of the
;;; constituent types. If ARG is *EMPTY-TYPE*, return NIL to indicate
;;; failure.
(defun prepare-arg-for-derive-type (arg)
  (flet ((listify (arg)
           (typecase arg
             (numeric-type
              (list arg))
             (union-type
              (union-type-types arg))
             (t
              (list arg)))))
    (unless (eq arg *empty-type*)
      ;; Make sure all args are some type of numeric-type. For member
      ;; types, convert the list of members into a union of equivalent
      ;; single-element member-type's.
      (let ((new-args nil))
        (dolist (arg (listify arg))
          (if (member-type-p arg)
              ;; Run down the list of members and convert to a list of
              ;; member types.
              (mapc-member-type-members
               (lambda (member)
                 (push (if (numberp member)
                           (make-member-type :members (list member))
                           *empty-type*)
                       new-args))
               arg)
              (push arg new-args)))
        (unless (member *empty-type* new-args)
          new-args)))))

;;; Convert from the standard type convention for which -0.0 and 0.0
;;; are equal to an intermediate convention for which they are
;;; considered different which is more natural for some of the
;;; optimisers.
(defun convert-numeric-type (type)
  (declare (type numeric-type type))
  ;;; Only convert real float interval delimiters types.
  (if (eq (numeric-type-complexp type) :real)
      (let* ((lo (numeric-type-low type))
             (lo-val (type-bound-number lo))
             (lo-float-zero-p (and lo (floatp lo-val) (= lo-val 0.0)))
             (hi (numeric-type-high type))
             (hi-val (type-bound-number hi))
             (hi-float-zero-p (and hi (floatp hi-val) (= hi-val 0.0))))
        (if (or lo-float-zero-p hi-float-zero-p)
            (make-numeric-type
             :class (numeric-type-class type)
             :format (numeric-type-format type)
             :complexp :real
             :low (if lo-float-zero-p
                      (if (consp lo)
                          (list (float 0.0 lo-val))
                          (float (load-time-value (make-unportable-float :single-float-negative-zero)) lo-val))
                      lo)
             :high (if hi-float-zero-p
                       (if (consp hi)
                           (list (float (load-time-value (make-unportable-float :single-float-negative-zero)) hi-val))
                           (float 0.0 hi-val))
                       hi))
            type))
      ;; Not real float.
      type))

;;; Convert back from the intermediate convention for which -0.0 and
;;; 0.0 are considered different to the standard type convention for
;;; which and equal.
(defun convert-back-numeric-type (type)
  (declare (type numeric-type type))
  ;;; Only convert real float interval delimiters types.
  (if (eq (numeric-type-complexp type) :real)
      (let* ((lo (numeric-type-low type))
             (lo-val (type-bound-number lo))
             (lo-float-zero-p
              (and lo (floatp lo-val) (= lo-val 0.0)
                   (float-sign lo-val)))
             (hi (numeric-type-high type))
             (hi-val (type-bound-number hi))
             (hi-float-zero-p
              (and hi (floatp hi-val) (= hi-val 0.0)
                   (float-sign hi-val))))
        (cond
          ;; (float +0.0 +0.0) => (member 0.0)
          ;; (float -0.0 -0.0) => (member -0.0)
          ((and lo-float-zero-p hi-float-zero-p)
           ;; shouldn't have exclusive bounds here..
           (aver (and (not (consp lo)) (not (consp hi))))
           (if (= lo-float-zero-p hi-float-zero-p)
               ;; (float +0.0 +0.0) => (member 0.0)
               ;; (float -0.0 -0.0) => (member -0.0)
               (specifier-type `(member ,lo-val))
               ;; (float -0.0 +0.0) => (float 0.0 0.0)
               ;; (float +0.0 -0.0) => (float 0.0 0.0)
               (make-numeric-type :class (numeric-type-class type)
                                  :format (numeric-type-format type)
                                  :complexp :real
                                  :low hi-val
                                  :high hi-val)))
          (lo-float-zero-p
           (cond
             ;; (float -0.0 x) => (float 0.0 x)
             ((and (not (consp lo)) (minusp lo-float-zero-p))
              (make-numeric-type :class (numeric-type-class type)
                                 :format (numeric-type-format type)
                                 :complexp :real
                                 :low (float 0.0 lo-val)
                                 :high hi))
             ;; (float (+0.0) x) => (float (0.0) x)
             ((and (consp lo) (plusp lo-float-zero-p))
              (make-numeric-type :class (numeric-type-class type)
                                 :format (numeric-type-format type)
                                 :complexp :real
                                 :low (list (float 0.0 lo-val))
                                 :high hi))
             (t
              ;; (float +0.0 x) => (or (member 0.0) (float (0.0) x))
              ;; (float (-0.0) x) => (or (member 0.0) (float (0.0) x))
              (list (make-member-type :members (list (float 0.0 lo-val)))
                    (make-numeric-type :class (numeric-type-class type)
                                       :format (numeric-type-format type)
                                       :complexp :real
                                       :low (list (float 0.0 lo-val))
                                       :high hi)))))
          (hi-float-zero-p
           (cond
             ;; (float x +0.0) => (float x 0.0)
             ((and (not (consp hi)) (plusp hi-float-zero-p))
              (make-numeric-type :class (numeric-type-class type)
                                 :format (numeric-type-format type)
                                 :complexp :real
                                 :low lo
                                 :high (float 0.0 hi-val)))
             ;; (float x (-0.0)) => (float x (0.0))
             ((and (consp hi) (minusp hi-float-zero-p))
              (make-numeric-type :class (numeric-type-class type)
                                 :format (numeric-type-format type)
                                 :complexp :real
                                 :low lo
                                 :high (list (float 0.0 hi-val))))
             (t
              ;; (float x (+0.0)) => (or (member -0.0) (float x (0.0)))
              ;; (float x -0.0) => (or (member -0.0) (float x (0.0)))
              (list (make-member-type :members (list (float (load-time-value (make-unportable-float :single-float-negative-zero)) hi-val)))
                    (make-numeric-type :class (numeric-type-class type)
                                       :format (numeric-type-format type)
                                       :complexp :real
                                       :low lo
                                       :high (list (float 0.0 hi-val)))))))
          (t
           type)))
      ;; not real float
      type))

;;; Convert back a possible list of numeric types.
(defun convert-back-numeric-type-list (type-list)
  (typecase type-list
    (list
     (let ((results '()))
       (dolist (type type-list)
         (if (numeric-type-p type)
             (let ((result (convert-back-numeric-type type)))
               (if (listp result)
                   (setf results (append results result))
                   (push result results)))
             (push type results)))
       results))
    (numeric-type
     (convert-back-numeric-type type-list))
    (union-type
     (convert-back-numeric-type-list (union-type-types type-list)))
    (t
     type-list)))

;;; FIXME: MAKE-CANONICAL-UNION-TYPE and CONVERT-MEMBER-TYPE probably
;;; belong in the kernel's type logic, invoked always, instead of in
;;; the compiler, invoked only during some type optimizations. (In
;;; fact, as of 0.pre8.100 or so they probably are, under
;;; MAKE-MEMBER-TYPE, so probably this code can be deleted)

;;; Take a list of types and return a canonical type specifier,
;;; combining any MEMBER types together. If both positive and negative
;;; MEMBER types are present they are converted to a float type.
;;; XXX This would be far simpler if the type-union methods could handle
;;; member/number unions.
(defun make-canonical-union-type (type-list)
  (let ((xset (alloc-xset))
        (fp-zeroes '())
        (misc-types '()))
    (dolist (type type-list)
      (cond ((member-type-p type)
             (mapc-member-type-members
              (lambda (member)
                (if (fp-zero-p member)
                    (unless (member member fp-zeroes)
                      (pushnew member fp-zeroes))
                    (add-to-xset member xset)))
              type))
            (t
             (push type misc-types))))
    (if (and (xset-empty-p xset) (not fp-zeroes))
        (apply #'type-union misc-types)
        (apply #'type-union (make-member-type :xset xset :fp-zeroes fp-zeroes) misc-types))))

;;; Convert a member type with a single member to a numeric type.
(defun convert-member-type (arg)
  (let* ((members (member-type-members arg))
         (member (first members))
         (member-type (type-of member)))
    (aver (not (rest members)))
    (specifier-type (cond ((typep member 'integer)
                           `(integer ,member ,member))
                          ((memq member-type '(short-float single-float
                                               double-float long-float))
                           `(,member-type ,member ,member))
                          (t
                           member-type)))))

;;; This is used in defoptimizers for computing the resulting type of
;;; a function.
;;;
;;; Given the lvar ARG, derive the resulting type using the
;;; DERIVE-FUN. DERIVE-FUN takes exactly one argument which is some
;;; "atomic" lvar type like numeric-type or member-type (containing
;;; just one element). It should return the resulting type, which can
;;; be a list of types.
;;;
;;; For the case of member types, if a MEMBER-FUN is given it is
;;; called to compute the result otherwise the member type is first
;;; converted to a numeric type and the DERIVE-FUN is called.
(defun one-arg-derive-type (arg derive-fun member-fun
                                &optional (convert-type t))
  (declare (type function derive-fun)
           (type (or null function) member-fun))
  (let ((arg-list (prepare-arg-for-derive-type (lvar-type arg))))
    (when arg-list
      (flet ((deriver (x)
               (typecase x
                 (member-type
                  (if member-fun
                      (with-float-traps-masked
                          (:underflow :overflow :divide-by-zero)
                        (specifier-type
                         `(eql ,(funcall member-fun
                                         (first (member-type-members x))))))
                      ;; Otherwise convert to a numeric type.
                      (let ((result-type-list
                             (funcall derive-fun (convert-member-type x))))
                        (if convert-type
                            (convert-back-numeric-type-list result-type-list)
                            result-type-list))))
                 (numeric-type
                  (if convert-type
                      (convert-back-numeric-type-list
                       (funcall derive-fun (convert-numeric-type x)))
                      (funcall derive-fun x)))
                 (t
                  *universal-type*))))
        ;; Run down the list of args and derive the type of each one,
        ;; saving all of the results in a list.
        (let ((results nil))
          (dolist (arg arg-list)
            (let ((result (deriver arg)))
              (if (listp result)
                  (setf results (append results result))
                  (push result results))))
          (if (rest results)
              (make-canonical-union-type results)
              (first results)))))))

;;; Same as ONE-ARG-DERIVE-TYPE, except we assume the function takes
;;; two arguments. DERIVE-FUN takes 3 args in this case: the two
;;; original args and a third which is T to indicate if the two args
;;; really represent the same lvar. This is useful for deriving the
;;; type of things like (* x x), which should always be positive. If
;;; we didn't do this, we wouldn't be able to tell.
(defun two-arg-derive-type (arg1 arg2 derive-fun fun
                                 &optional (convert-type t))
  (declare (type function derive-fun fun))
  (flet ((deriver (x y same-arg)
           (cond ((and (member-type-p x) (member-type-p y))
                  (let* ((x (first (member-type-members x)))
                         (y (first (member-type-members y)))
                         (result (ignore-errors
                                   (with-float-traps-masked
                                       (:underflow :overflow :divide-by-zero
                                                   :invalid)
                                     (funcall fun x y)))))
                    (cond ((null result) *empty-type*)
                          ((and (floatp result) (float-nan-p result))
                           (make-numeric-type :class 'float
                                              :format (type-of result)
                                              :complexp :real))
                          (t
                           (specifier-type `(eql ,result))))))
                 ((and (member-type-p x) (numeric-type-p y))
                  (let* ((x (convert-member-type x))
                         (y (if convert-type (convert-numeric-type y) y))
                         (result (funcall derive-fun x y same-arg)))
                    (if convert-type
                        (convert-back-numeric-type-list result)
                        result)))
                 ((and (numeric-type-p x) (member-type-p y))
                  (let* ((x (if convert-type (convert-numeric-type x) x))
                         (y (convert-member-type y))
                         (result (funcall derive-fun x y same-arg)))
                    (if convert-type
                        (convert-back-numeric-type-list result)
                        result)))
                 ((and (numeric-type-p x) (numeric-type-p y))
                  (let* ((x (if convert-type (convert-numeric-type x) x))
                         (y (if convert-type (convert-numeric-type y) y))
                         (result (funcall derive-fun x y same-arg)))
                    (if convert-type
                        (convert-back-numeric-type-list result)
                        result)))
                 (t
                  *universal-type*))))
    (let ((same-arg (same-leaf-ref-p arg1 arg2))
          (a1 (prepare-arg-for-derive-type (lvar-type arg1)))
          (a2 (prepare-arg-for-derive-type (lvar-type arg2))))
      (when (and a1 a2)
        (let ((results nil))
          (if same-arg
              ;; Since the args are the same LVARs, just run down the
              ;; lists.
              (dolist (x a1)
                (let ((result (deriver x x same-arg)))
                  (if (listp result)
                      (setf results (append results result))
                      (push result results))))
              ;; Try all pairwise combinations.
              (dolist (x a1)
                (dolist (y a2)
                  (let ((result (or (deriver x y same-arg)
                                    (numeric-contagion x y))))
                    (if (listp result)
                        (setf results (append results result))
                        (push result results))))))
          (if (rest results)
              (make-canonical-union-type results)
              (first results)))))))

#+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(progn
(defoptimizer (+ derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (flet ((frob (x y)
                (if (and x y)
                    (+ x y)
                    nil)))
         (values (frob (numeric-type-low x) (numeric-type-low y))
                 (frob (numeric-type-high x) (numeric-type-high y)))))))

(defoptimizer (- derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (flet ((frob (x y)
                (if (and x y)
                    (- x y)
                    nil)))
         (values (frob (numeric-type-low x) (numeric-type-high y))
                 (frob (numeric-type-high x) (numeric-type-low y)))))))

(defoptimizer (* derive-type) ((x y))
  (derive-integer-type
   x y
   #'(lambda (x y)
       (let ((x-low (numeric-type-low x))
             (x-high (numeric-type-high x))
             (y-low (numeric-type-low y))
             (y-high (numeric-type-high y)))
         (cond ((not (and x-low y-low))
                (values nil nil))
               ((or (minusp x-low) (minusp y-low))
                (if (and x-high y-high)
                    (let ((max (* (max (abs x-low) (abs x-high))
                                  (max (abs y-low) (abs y-high)))))
                      (values (- max) max))
                    (values nil nil)))
               (t
                (values (* x-low y-low)
                        (if (and x-high y-high)
                            (* x-high y-high)
                            nil))))))))

(defoptimizer (/ derive-type) ((x y))
  (numeric-contagion (lvar-type x) (lvar-type y)))

) ; PROGN

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(progn
(defun +-derive-type-aux (x y same-arg)
  (if (and (numeric-type-real-p x)
           (numeric-type-real-p y))
      (let ((result
             (if same-arg
                 (let ((x-int (numeric-type->interval x)))
                   (interval-add x-int x-int))
                 (interval-add (numeric-type->interval x)
                               (numeric-type->interval y))))
            (result-type (numeric-contagion x y)))
        ;; If the result type is a float, we need to be sure to coerce
        ;; the bounds into the correct type.
        (when (eq (numeric-type-class result-type) 'float)
          (setf result (interval-func
                        #'(lambda (x)
                            (coerce-for-bound x (or (numeric-type-format result-type)
                                                    'float)))
                        result)))
        (make-numeric-type
         :class (if (and (eq (numeric-type-class x) 'integer)
                         (eq (numeric-type-class y) 'integer))
                    ;; The sum of integers is always an integer.
                    'integer
                    (numeric-type-class result-type))
         :format (numeric-type-format result-type)
         :low (interval-low result)
         :high (interval-high result)))
      ;; general contagion
      (numeric-contagion x y)))

(defoptimizer (+ derive-type) ((x y))
  (two-arg-derive-type x y #'+-derive-type-aux #'+))

(defun --derive-type-aux (x y same-arg)
  (if (and (numeric-type-real-p x)
           (numeric-type-real-p y))
      (let ((result
             ;; (- X X) is always 0.
             (if same-arg
                 (make-interval :low 0 :high 0)
                 (interval-sub (numeric-type->interval x)
                               (numeric-type->interval y))))
            (result-type (numeric-contagion x y)))
        ;; If the result type is a float, we need to be sure to coerce
        ;; the bounds into the correct type.
        (when (eq (numeric-type-class result-type) 'float)
          (setf result (interval-func
                        #'(lambda (x)
                            (coerce-for-bound x (or (numeric-type-format result-type)
                                                    'float)))
                        result)))
        (make-numeric-type
         :class (if (and (eq (numeric-type-class x) 'integer)
                         (eq (numeric-type-class y) 'integer))
                    ;; The difference of integers is always an integer.
                    'integer
                    (numeric-type-class result-type))
         :format (numeric-type-format result-type)
         :low (interval-low result)
         :high (interval-high result)))
      ;; general contagion
      (numeric-contagion x y)))

(defoptimizer (- derive-type) ((x y))
  (two-arg-derive-type x y #'--derive-type-aux #'-))

(defun *-derive-type-aux (x y same-arg)
  (if (and (numeric-type-real-p x)
           (numeric-type-real-p y))
      (let ((result
             ;; (* X X) is always positive, so take care to do it right.
             (if same-arg
                 (interval-sqr (numeric-type->interval x))
                 (interval-mul (numeric-type->interval x)
                               (numeric-type->interval y))))
            (result-type (numeric-contagion x y)))
        ;; If the result type is a float, we need to be sure to coerce
        ;; the bounds into the correct type.
        (when (eq (numeric-type-class result-type) 'float)
          (setf result (interval-func
                        #'(lambda (x)
                            (coerce-for-bound x (or (numeric-type-format result-type)
                                                    'float)))
                        result)))
        (make-numeric-type
         :class (if (and (eq (numeric-type-class x) 'integer)
                         (eq (numeric-type-class y) 'integer))
                    ;; The product of integers is always an integer.
                    'integer
                    (numeric-type-class result-type))
         :format (numeric-type-format result-type)
         :low (interval-low result)
         :high (interval-high result)))
      (numeric-contagion x y)))

(defoptimizer (* derive-type) ((x y))
  (two-arg-derive-type x y #'*-derive-type-aux #'*))

(defun /-derive-type-aux (x y same-arg)
  (if (and (numeric-type-real-p x)
           (numeric-type-real-p y))
      (let ((result
             ;; (/ X X) is always 1, except if X can contain 0. In
             ;; that case, we shouldn't optimize the division away
             ;; because we want 0/0 to signal an error.
             (if (and same-arg
                      (not (interval-contains-p
                            0 (interval-closure (numeric-type->interval y)))))
                 (make-interval :low 1 :high 1)
                 (interval-div (numeric-type->interval x)
                               (numeric-type->interval y))))
            (result-type (numeric-contagion x y)))
        ;; If the result type is a float, we need to be sure to coerce
        ;; the bounds into the correct type.
        (when (eq (numeric-type-class result-type) 'float)
          (setf result (interval-func
                        #'(lambda (x)
                            (coerce-for-bound x (or (numeric-type-format result-type)
                                                    'float)))
                        result)))
        (make-numeric-type :class (numeric-type-class result-type)
                           :format (numeric-type-format result-type)
                           :low (interval-low result)
                           :high (interval-high result)))
      (numeric-contagion x y)))

(defoptimizer (/ derive-type) ((x y))
  (two-arg-derive-type x y #'/-derive-type-aux #'/))

) ; PROGN

(defun ash-derive-type-aux (n-type shift same-arg)
  (declare (ignore same-arg))
  ;; KLUDGE: All this ASH optimization is suppressed under CMU CL for
  ;; some bignum cases because as of version 2.4.6 for Debian and 18d,
  ;; CMU CL blows up on (ASH 1000000000 -100000000000) (i.e. ASH of
  ;; two bignums yielding zero) and it's hard to avoid that
  ;; calculation in here.
  #+(and cmu sb-xc-host)
  (when (and (or (typep (numeric-type-low n-type) 'bignum)
                 (typep (numeric-type-high n-type) 'bignum))
             (or (typep (numeric-type-low shift) 'bignum)
                 (typep (numeric-type-high shift) 'bignum)))
    (return-from ash-derive-type-aux *universal-type*))
  (flet ((ash-outer (n s)
           (when (and (fixnump s)
                      (<= s 64)
                      (> s sb!xc:most-negative-fixnum))
             (ash n s)))
         ;; KLUDGE: The bare 64's here should be related to
         ;; symbolic machine word size values somehow.

         (ash-inner (n s)
           (if (and (fixnump s)
                    (> s sb!xc:most-negative-fixnum))
             (ash n (min s 64))
             (if (minusp n) -1 0))))
    (or (and (csubtypep n-type (specifier-type 'integer))
             (csubtypep shift (specifier-type 'integer))
             (let ((n-low (numeric-type-low n-type))
                   (n-high (numeric-type-high n-type))
                   (s-low (numeric-type-low shift))
                   (s-high (numeric-type-high shift)))
               (make-numeric-type :class 'integer  :complexp :real
                                  :low (when n-low
                                         (if (minusp n-low)
                                           (ash-outer n-low s-high)
                                           (ash-inner n-low s-low)))
                                  :high (when n-high
                                          (if (minusp n-high)
                                            (ash-inner n-high s-low)
                                            (ash-outer n-high s-high))))))
        *universal-type*)))

(defoptimizer (ash derive-type) ((n shift))
  (two-arg-derive-type n shift #'ash-derive-type-aux #'ash))

#+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(macrolet ((frob (fun)
             `#'(lambda (type type2)
                  (declare (ignore type2))
                  (let ((lo (numeric-type-low type))
                        (hi (numeric-type-high type)))
                    (values (if hi (,fun hi) nil) (if lo (,fun lo) nil))))))

  (defoptimizer (%negate derive-type) ((num))
    (derive-integer-type num num (frob -))))

(defun lognot-derive-type-aux (int)
  (derive-integer-type-aux int int
                           (lambda (type type2)
                             (declare (ignore type2))
                             (let ((lo (numeric-type-low type))
                                   (hi (numeric-type-high type)))
                               (values (if hi (lognot hi) nil)
                                       (if lo (lognot lo) nil)
                                       (numeric-type-class type)
                                       (numeric-type-format type))))))

(defoptimizer (lognot derive-type) ((int))
  (lognot-derive-type-aux (lvar-type int)))

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defoptimizer (%negate derive-type) ((num))
  (flet ((negate-bound (b)
           (and b
                (set-bound (- (type-bound-number b))
                           (consp b)))))
    (one-arg-derive-type num
                         (lambda (type)
                           (modified-numeric-type
                            type
                            :low (negate-bound (numeric-type-high type))
                            :high (negate-bound (numeric-type-low type))))
                         #'-)))

#+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defoptimizer (abs derive-type) ((num))
  (let ((type (lvar-type num)))
    (if (and (numeric-type-p type)
             (eq (numeric-type-class type) 'integer)
             (eq (numeric-type-complexp type) :real))
        (let ((lo (numeric-type-low type))
              (hi (numeric-type-high type)))
          (make-numeric-type :class 'integer :complexp :real
                             :low (cond ((and hi (minusp hi))
                                         (abs hi))
                                        (lo
                                         (max 0 lo))
                                        (t
                                         0))
                             :high (if (and hi lo)
                                       (max (abs hi) (abs lo))
                                       nil)))
        (numeric-contagion type type))))

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defun abs-derive-type-aux (type)
  (cond ((eq (numeric-type-complexp type) :complex)
         ;; The absolute value of a complex number is always a
         ;; non-negative float.
         (let* ((format (case (numeric-type-class type)
                          ((integer rational) 'single-float)
                          (t (numeric-type-format type))))
                (bound-format (or format 'float)))
           (make-numeric-type :class 'float
                              :format format
                              :complexp :real
                              :low (coerce 0 bound-format)
                              :high nil)))
        (t
         ;; The absolute value of a real number is a non-negative real
         ;; of the same type.
         (let* ((abs-bnd (interval-abs (numeric-type->interval type)))
                (class (numeric-type-class type))
                (format (numeric-type-format type))
                (bound-type (or format class 'real)))
           (make-numeric-type
            :class class
            :format format
            :complexp :real
            :low (coerce-and-truncate-floats (interval-low abs-bnd) bound-type)
            :high (coerce-and-truncate-floats
                   (interval-high abs-bnd) bound-type))))))

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defoptimizer (abs derive-type) ((num))
  (one-arg-derive-type num #'abs-derive-type-aux #'abs))

#+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defoptimizer (truncate derive-type) ((number divisor))
  (let ((number-type (lvar-type number))
        (divisor-type (lvar-type divisor))
        (integer-type (specifier-type 'integer)))
    (if (and (numeric-type-p number-type)
             (csubtypep number-type integer-type)
             (numeric-type-p divisor-type)
             (csubtypep divisor-type integer-type))
        (let ((number-low (numeric-type-low number-type))
              (number-high (numeric-type-high number-type))
              (divisor-low (numeric-type-low divisor-type))
              (divisor-high (numeric-type-high divisor-type)))
          (values-specifier-type
           `(values ,(integer-truncate-derive-type number-low number-high
                                                   divisor-low divisor-high)
                    ,(integer-rem-derive-type number-low number-high
                                              divisor-low divisor-high))))
        *universal-type*)))

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(progn

(defun rem-result-type (number-type divisor-type)
  ;; Figure out what the remainder type is. The remainder is an
  ;; integer if both args are integers; a rational if both args are
  ;; rational; and a float otherwise.
  (cond ((and (csubtypep number-type (specifier-type 'integer))
              (csubtypep divisor-type (specifier-type 'integer)))
         'integer)
        ((and (csubtypep number-type (specifier-type 'rational))
              (csubtypep divisor-type (specifier-type 'rational)))
         'rational)
        ((and (csubtypep number-type (specifier-type 'float))
              (csubtypep divisor-type (specifier-type 'float)))
         ;; Both are floats so the result is also a float, of
         ;; the largest type.
         (or (float-format-max (numeric-type-format number-type)
                               (numeric-type-format divisor-type))
             'float))
        ((and (csubtypep number-type (specifier-type 'float))
              (csubtypep divisor-type (specifier-type 'rational)))
         ;; One of the arguments is a float and the other is a
         ;; rational. The remainder is a float of the same
         ;; type.
         (or (numeric-type-format number-type) 'float))
        ((and (csubtypep divisor-type (specifier-type 'float))
              (csubtypep number-type (specifier-type 'rational)))
         ;; One of the arguments is a float and the other is a
         ;; rational. The remainder is a float of the same
         ;; type.
         (or (numeric-type-format divisor-type) 'float))
        (t
         ;; Some unhandled combination. This usually means both args
         ;; are REAL so the result is a REAL.
         'real)))

(defun truncate-derive-type-quot (number-type divisor-type)
  (let* ((rem-type (rem-result-type number-type divisor-type))
         (number-interval (numeric-type->interval number-type))
         (divisor-interval (numeric-type->interval divisor-type)))
    ;;(declare (type (member '(integer rational float)) rem-type))
    ;; We have real numbers now.
    (cond ((eq rem-type 'integer)
           ;; Since the remainder type is INTEGER, both args are
           ;; INTEGERs.
           (let* ((res (integer-truncate-derive-type
                        (interval-low number-interval)
                        (interval-high number-interval)
                        (interval-low divisor-interval)
                        (interval-high divisor-interval))))
             (specifier-type (if (listp res) res 'integer))))
          (t
           (let ((quot (truncate-quotient-bound
                        (interval-div number-interval
                                      divisor-interval))))
             (specifier-type `(integer ,(or (interval-low quot) '*)
                                       ,(or (interval-high quot) '*))))))))

(defun truncate-derive-type-rem (number-type divisor-type)
  (let* ((rem-type (rem-result-type number-type divisor-type))
         (number-interval (numeric-type->interval number-type))
         (divisor-interval (numeric-type->interval divisor-type))
         (rem (truncate-rem-bound number-interval divisor-interval)))
    ;;(declare (type (member '(integer rational float)) rem-type))
    ;; We have real numbers now.
    (cond ((eq rem-type 'integer)
           ;; Since the remainder type is INTEGER, both args are
           ;; INTEGERs.
           (specifier-type `(,rem-type ,(or (interval-low rem) '*)
                                       ,(or (interval-high rem) '*))))
          (t
           (multiple-value-bind (class format)
               (ecase rem-type
                 (integer
                  (values 'integer nil))
                 (rational
                  (values 'rational nil))
                 ((or single-float double-float #!+long-float long-float)
                  (values 'float rem-type))
                 (float
                  (values 'float nil))
                 (real
                  (values nil nil)))
             (when (member rem-type '(float single-float double-float
                                            #!+long-float long-float))
               (setf rem (interval-func #'(lambda (x)
                                            (coerce-for-bound x rem-type))
                                        rem)))
             (make-numeric-type :class class
                                :format format
                                :low (interval-low rem)
                                :high (interval-high rem)))))))

(defun truncate-derive-type-quot-aux (num div same-arg)
  (declare (ignore same-arg))
  (if (and (numeric-type-real-p num)
           (numeric-type-real-p div))
      (truncate-derive-type-quot num div)
      *empty-type*))

(defun truncate-derive-type-rem-aux (num div same-arg)
  (declare (ignore same-arg))
  (if (and (numeric-type-real-p num)
           (numeric-type-real-p div))
      (truncate-derive-type-rem num div)
      *empty-type*))

(defoptimizer (truncate derive-type) ((number divisor))
  (let ((quot (two-arg-derive-type number divisor
                                   #'truncate-derive-type-quot-aux #'truncate))
        (rem (two-arg-derive-type number divisor
                                  #'truncate-derive-type-rem-aux #'rem)))
    (when (and quot rem)
      (make-values-type :required (list quot rem)))))

(defun ftruncate-derive-type-quot (number-type divisor-type)
  ;; The bounds are the same as for truncate. However, the first
  ;; result is a float of some type. We need to determine what that
  ;; type is. Basically it's the more contagious of the two types.
  (let ((q-type (truncate-derive-type-quot number-type divisor-type))
        (res-type (numeric-contagion number-type divisor-type)))
    (make-numeric-type :class 'float
                       :format (numeric-type-format res-type)
                       :low (numeric-type-low q-type)
                       :high (numeric-type-high q-type))))

(defun ftruncate-derive-type-quot-aux (n d same-arg)
  (declare (ignore same-arg))
  (if (and (numeric-type-real-p n)
           (numeric-type-real-p d))
      (ftruncate-derive-type-quot n d)
      *empty-type*))

(defoptimizer (ftruncate derive-type) ((number divisor))
  (let ((quot
         (two-arg-derive-type number divisor
                              #'ftruncate-derive-type-quot-aux #'ftruncate))
        (rem (two-arg-derive-type number divisor
                                  #'truncate-derive-type-rem-aux #'rem)))
    (when (and quot rem)
      (make-values-type :required (list quot rem)))))

(defun %unary-truncate-derive-type-aux (number)
  (truncate-derive-type-quot number (specifier-type '(integer 1 1))))

(defoptimizer (%unary-truncate derive-type) ((number))
  (one-arg-derive-type number
                       #'%unary-truncate-derive-type-aux
                       #'%unary-truncate))

(defoptimizer (%unary-truncate/single-float derive-type) ((number))
  (one-arg-derive-type number
                       #'%unary-truncate-derive-type-aux
                       #'%unary-truncate))

(defoptimizer (%unary-truncate/double-float derive-type) ((number))
  (one-arg-derive-type number
                       #'%unary-truncate-derive-type-aux
                       #'%unary-truncate))

(defoptimizer (%unary-ftruncate derive-type) ((number))
  (let ((divisor (specifier-type '(integer 1 1))))
    (one-arg-derive-type number
                         #'(lambda (n)
                             (ftruncate-derive-type-quot-aux n divisor nil))
                         #'%unary-ftruncate)))

(defoptimizer (%unary-round derive-type) ((number))
  (one-arg-derive-type number
                       (lambda (n)
                         (block nil
                           (unless (numeric-type-real-p n)
                             (return *empty-type*))
                           (let* ((interval (numeric-type->interval n))
                                  (low      (interval-low interval))
                                  (high     (interval-high interval)))
                             (when (consp low)
                               (setf low (car low)))
                             (when (consp high)
                               (setf high (car high)))
                             (specifier-type
                              `(integer ,(if low
                                             (round low)
                                             '*)
                                        ,(if high
                                             (round high)
                                             '*))))))
                       #'%unary-round))

;;; Define optimizers for FLOOR and CEILING.
(macrolet
    ((def (name q-name r-name)
       (let ((q-aux (symbolicate q-name "-AUX"))
             (r-aux (symbolicate r-name "-AUX")))
         `(progn
           ;; Compute type of quotient (first) result.
           (defun ,q-aux (number-type divisor-type)
             (let* ((number-interval
                     (numeric-type->interval number-type))
                    (divisor-interval
                     (numeric-type->interval divisor-type))
                    (quot (,q-name (interval-div number-interval
                                                 divisor-interval))))
               (specifier-type `(integer ,(or (interval-low quot) '*)
                                         ,(or (interval-high quot) '*)))))
           ;; Compute type of remainder.
           (defun ,r-aux (number-type divisor-type)
             (let* ((divisor-interval
                     (numeric-type->interval divisor-type))
                    (rem (,r-name divisor-interval))
                    (result-type (rem-result-type number-type divisor-type)))
               (multiple-value-bind (class format)
                   (ecase result-type
                     (integer
                      (values 'integer nil))
                     (rational
                      (values 'rational nil))
                     ((or single-float double-float #!+long-float long-float)
                      (values 'float result-type))
                     (float
                      (values 'float nil))
                     (real
                      (values nil nil)))
                 (when (member result-type '(float single-float double-float
                                             #!+long-float long-float))
                   ;; Make sure that the limits on the interval have
                   ;; the right type.
                   (setf rem (interval-func (lambda (x)
                                              (coerce-for-bound x result-type))
                                            rem)))
                 (make-numeric-type :class class
                                    :format format
                                    :low (interval-low rem)
                                    :high (interval-high rem)))))
           ;; the optimizer itself
           (defoptimizer (,name derive-type) ((number divisor))
             (flet ((derive-q (n d same-arg)
                      (declare (ignore same-arg))
                      (if (and (numeric-type-real-p n)
                               (numeric-type-real-p d))
                          (,q-aux n d)
                          *empty-type*))
                    (derive-r (n d same-arg)
                      (declare (ignore same-arg))
                      (if (and (numeric-type-real-p n)
                               (numeric-type-real-p d))
                          (,r-aux n d)
                          *empty-type*)))
               (let ((quot (two-arg-derive-type
                            number divisor #'derive-q #',name))
                     (rem (two-arg-derive-type
                           number divisor #'derive-r #'mod)))
                 (when (and quot rem)
                   (make-values-type :required (list quot rem))))))))))

  (def floor floor-quotient-bound floor-rem-bound)
  (def ceiling ceiling-quotient-bound ceiling-rem-bound))

;;; Define optimizers for FFLOOR and FCEILING
(macrolet ((def (name q-name r-name)
             (let ((q-aux (symbolicate "F" q-name "-AUX"))
                   (r-aux (symbolicate r-name "-AUX")))
               `(progn
                  ;; Compute type of quotient (first) result.
                  (defun ,q-aux (number-type divisor-type)
                    (let* ((number-interval
                            (numeric-type->interval number-type))
                           (divisor-interval
                            (numeric-type->interval divisor-type))
                           (quot (,q-name (interval-div number-interval
                                                        divisor-interval)))
                           (res-type (numeric-contagion number-type
                                                        divisor-type)))
                      (make-numeric-type
                       :class (numeric-type-class res-type)
                       :format (numeric-type-format res-type)
                       :low  (interval-low quot)
                       :high (interval-high quot))))

                  (defoptimizer (,name derive-type) ((number divisor))
                    (flet ((derive-q (n d same-arg)
                             (declare (ignore same-arg))
                             (if (and (numeric-type-real-p n)
                                      (numeric-type-real-p d))
                                 (,q-aux n d)
                                 *empty-type*))
                           (derive-r (n d same-arg)
                             (declare (ignore same-arg))
                             (if (and (numeric-type-real-p n)
                                      (numeric-type-real-p d))
                                 (,r-aux n d)
                                 *empty-type*)))
                      (let ((quot (two-arg-derive-type
                                   number divisor #'derive-q #',name))
                            (rem (two-arg-derive-type
                                  number divisor #'derive-r #'mod)))
                        (when (and quot rem)
                          (make-values-type :required (list quot rem))))))))))

  (def ffloor floor-quotient-bound floor-rem-bound)
  (def fceiling ceiling-quotient-bound ceiling-rem-bound))

;;; functions to compute the bounds on the quotient and remainder for
;;; the FLOOR function
(defun floor-quotient-bound (quot)
  ;; Take the floor of the quotient and then massage it into what we
  ;; need.
  (let ((lo (interval-low quot))
        (hi (interval-high quot)))
    ;; Take the floor of the lower bound. The result is always a
    ;; closed lower bound.
    (setf lo (if lo
                 (floor (type-bound-number lo))
                 nil))
    ;; For the upper bound, we need to be careful.
    (setf hi
          (cond ((consp hi)
                 ;; An open bound. We need to be careful here because
                 ;; the floor of '(10.0) is 9, but the floor of
                 ;; 10.0 is 10.
                 (multiple-value-bind (q r) (floor (first hi))
                   (if (zerop r)
                       (1- q)
                       q)))
                (hi
                 ;; A closed bound, so the answer is obvious.
                 (floor hi))
                (t
                 hi)))
    (make-interval :low lo :high hi)))
(defun floor-rem-bound (div)
  ;; The remainder depends only on the divisor. Try to get the
  ;; correct sign for the remainder if we can.
  (case (interval-range-info div)
    (+
     ;; The divisor is always positive.
     (let ((rem (interval-abs div)))
       (setf (interval-low rem) 0)
       (when (and (numberp (interval-high rem))
                  (not (zerop (interval-high rem))))
         ;; The remainder never contains the upper bound. However,
         ;; watch out for the case where the high limit is zero!
         (setf (interval-high rem) (list (interval-high rem))))
       rem))
    (-
     ;; The divisor is always negative.
     (let ((rem (interval-neg (interval-abs div))))
       (setf (interval-high rem) 0)
       (when (numberp (interval-low rem))
         ;; The remainder never contains the lower bound.
         (setf (interval-low rem) (list (interval-low rem))))
       rem))
    (otherwise
     ;; The divisor can be positive or negative. All bets off. The
     ;; magnitude of remainder is the maximum value of the divisor.
     (let ((limit (type-bound-number (interval-high (interval-abs div)))))
       ;; The bound never reaches the limit, so make the interval open.
       (make-interval :low (if limit
                               (list (- limit))
                               limit)
                      :high (list limit))))))
#| Test cases
(floor-quotient-bound (make-interval :low 0.3 :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low 0.3 :high '(10.3)))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low 0.3 :high 10))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low 0.3 :high '(10)))
=> #S(INTERVAL :LOW 0 :HIGH 9)
(floor-quotient-bound (make-interval :low '(0.3) :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low '(0.0) :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH 10)
(floor-quotient-bound (make-interval :low '(-1.3) :high 10.3))
=> #S(INTERVAL :LOW -2 :HIGH 10)
(floor-quotient-bound (make-interval :low '(-1.0) :high 10.3))
=> #S(INTERVAL :LOW -1 :HIGH 10)
(floor-quotient-bound (make-interval :low -1.0 :high 10.3))
=> #S(INTERVAL :LOW -1 :HIGH 10)

(floor-rem-bound (make-interval :low 0.3 :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH '(10.3))
(floor-rem-bound (make-interval :low 0.3 :high '(10.3)))
=> #S(INTERVAL :LOW 0 :HIGH '(10.3))
(floor-rem-bound (make-interval :low -10 :high -2.3))
#S(INTERVAL :LOW (-10) :HIGH 0)
(floor-rem-bound (make-interval :low 0.3 :high 10))
=> #S(INTERVAL :LOW 0 :HIGH '(10))
(floor-rem-bound (make-interval :low '(-1.3) :high 10.3))
=> #S(INTERVAL :LOW '(-10.3) :HIGH '(10.3))
(floor-rem-bound (make-interval :low '(-20.3) :high 10.3))
=> #S(INTERVAL :LOW (-20.3) :HIGH (20.3))
|#

;;; same functions for CEILING
(defun ceiling-quotient-bound (quot)
  ;; Take the ceiling of the quotient and then massage it into what we
  ;; need.
  (let ((lo (interval-low quot))
        (hi (interval-high quot)))
    ;; Take the ceiling of the upper bound. The result is always a
    ;; closed upper bound.
    (setf hi (if hi
                 (ceiling (type-bound-number hi))
                 nil))
    ;; For the lower bound, we need to be careful.
    (setf lo
          (cond ((consp lo)
                 ;; An open bound. We need to be careful here because
                 ;; the ceiling of '(10.0) is 11, but the ceiling of
                 ;; 10.0 is 10.
                 (multiple-value-bind (q r) (ceiling (first lo))
                   (if (zerop r)
                       (1+ q)
                       q)))
                (lo
                 ;; A closed bound, so the answer is obvious.
                 (ceiling lo))
                (t
                 lo)))
    (make-interval :low lo :high hi)))
(defun ceiling-rem-bound (div)
  ;; The remainder depends only on the divisor. Try to get the
  ;; correct sign for the remainder if we can.
  (case (interval-range-info div)
    (+
     ;; Divisor is always positive. The remainder is negative.
     (let ((rem (interval-neg (interval-abs div))))
       (setf (interval-high rem) 0)
       (when (and (numberp (interval-low rem))
                  (not (zerop (interval-low rem))))
         ;; The remainder never contains the upper bound. However,
         ;; watch out for the case when the upper bound is zero!
         (setf (interval-low rem) (list (interval-low rem))))
       rem))
    (-
     ;; Divisor is always negative. The remainder is positive
     (let ((rem (interval-abs div)))
       (setf (interval-low rem) 0)
       (when (numberp (interval-high rem))
         ;; The remainder never contains the lower bound.
         (setf (interval-high rem) (list (interval-high rem))))
       rem))
    (otherwise
     ;; The divisor can be positive or negative. All bets off. The
     ;; magnitude of remainder is the maximum value of the divisor.
     (let ((limit (type-bound-number (interval-high (interval-abs div)))))
       ;; The bound never reaches the limit, so make the interval open.
       (make-interval :low (if limit
                               (list (- limit))
                               limit)
                      :high (list limit))))))

#| Test cases
(ceiling-quotient-bound (make-interval :low 0.3 :high 10.3))
=> #S(INTERVAL :LOW 1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low 0.3 :high '(10.3)))
=> #S(INTERVAL :LOW 1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low 0.3 :high 10))
=> #S(INTERVAL :LOW 1 :HIGH 10)
(ceiling-quotient-bound (make-interval :low 0.3 :high '(10)))
=> #S(INTERVAL :LOW 1 :HIGH 10)
(ceiling-quotient-bound (make-interval :low '(0.3) :high 10.3))
=> #S(INTERVAL :LOW 1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low '(0.0) :high 10.3))
=> #S(INTERVAL :LOW 1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low '(-1.3) :high 10.3))
=> #S(INTERVAL :LOW -1 :HIGH 11)
(ceiling-quotient-bound (make-interval :low '(-1.0) :high 10.3))
=> #S(INTERVAL :LOW 0 :HIGH 11)
(ceiling-quotient-bound (make-interval :low -1.0 :high 10.3))
=> #S(INTERVAL :LOW -1 :HIGH 11)

(ceiling-rem-bound (make-interval :low 0.3 :high 10.3))
=> #S(INTERVAL :LOW (-10.3) :HIGH 0)
(ceiling-rem-bound (make-interval :low 0.3 :high '(10.3)))
=> #S(INTERVAL :LOW 0 :HIGH '(10.3))
(ceiling-rem-bound (make-interval :low -10 :high -2.3))
=> #S(INTERVAL :LOW 0 :HIGH (10))
(ceiling-rem-bound (make-interval :low 0.3 :high 10))
=> #S(INTERVAL :LOW (-10) :HIGH 0)
(ceiling-rem-bound (make-interval :low '(-1.3) :high 10.3))
=> #S(INTERVAL :LOW (-10.3) :HIGH (10.3))
(ceiling-rem-bound (make-interval :low '(-20.3) :high 10.3))
=> #S(INTERVAL :LOW (-20.3) :HIGH (20.3))
|#

(defun truncate-quotient-bound (quot)
  ;; For positive quotients, truncate is exactly like floor. For
  ;; negative quotients, truncate is exactly like ceiling. Otherwise,
  ;; it's the union of the two pieces.
  (case (interval-range-info quot)
    (+
     ;; just like FLOOR
     (floor-quotient-bound quot))
    (-
     ;; just like CEILING
     (ceiling-quotient-bound quot))
    (otherwise
     ;; Split the interval into positive and negative pieces, compute
     ;; the result for each piece and put them back together.
     (destructuring-bind (neg pos) (interval-split 0 quot t t)
       (interval-merge-pair (ceiling-quotient-bound neg)
                            (floor-quotient-bound pos))))))

(defun truncate-rem-bound (num div)
  ;; This is significantly more complicated than FLOOR or CEILING. We
  ;; need both the number and the divisor to determine the range. The
  ;; basic idea is to split the ranges of NUM and DEN into positive
  ;; and negative pieces and deal with each of the four possibilities
  ;; in turn.
  (case (interval-range-info num)
    (+
     (case (interval-range-info div)
       (+
        (floor-rem-bound div))
       (-
        (ceiling-rem-bound div))
       (otherwise
        (destructuring-bind (neg pos) (interval-split 0 div t t)
          (interval-merge-pair (truncate-rem-bound num neg)
                               (truncate-rem-bound num pos))))))
    (-
     (case (interval-range-info div)
       (+
        (ceiling-rem-bound div))
       (-
        (floor-rem-bound div))
       (otherwise
        (destructuring-bind (neg pos) (interval-split 0 div t t)
          (interval-merge-pair (truncate-rem-bound num neg)
                               (truncate-rem-bound num pos))))))
    (otherwise
     (destructuring-bind (neg pos) (interval-split 0 num t t)
       (interval-merge-pair (truncate-rem-bound neg div)
                            (truncate-rem-bound pos div))))))
) ; PROGN

;;; Derive useful information about the range. Returns three values:
;;; - '+ if its positive, '- negative, or nil if it overlaps 0.
;;; - The abs of the minimal value (i.e. closest to 0) in the range.
;;; - The abs of the maximal value if there is one, or nil if it is
;;;   unbounded.
(defun numeric-range-info (low high)
  (cond ((and low (not (minusp low)))
         (values '+ low high))
        ((and high (not (plusp high)))
         (values '- (- high) (if low (- low) nil)))
        (t
         (values nil 0 (and low high (max (- low) high))))))

(defun integer-truncate-derive-type
       (number-low number-high divisor-low divisor-high)
  ;; The result cannot be larger in magnitude than the number, but the
  ;; sign might change. If we can determine the sign of either the
  ;; number or the divisor, we can eliminate some of the cases.
  (multiple-value-bind (number-sign number-min number-max)
      (numeric-range-info number-low number-high)
    (multiple-value-bind (divisor-sign divisor-min divisor-max)
        (numeric-range-info divisor-low divisor-high)
      (when (and divisor-max (zerop divisor-max))
        ;; We've got a problem: guaranteed division by zero.
        (return-from integer-truncate-derive-type t))
      (when (zerop divisor-min)
        ;; We'll assume that they aren't going to divide by zero.
        (incf divisor-min))
      (cond ((and number-sign divisor-sign)
             ;; We know the sign of both.
             (if (eq number-sign divisor-sign)
                 ;; Same sign, so the result will be positive.
                 `(integer ,(if divisor-max
                                (truncate number-min divisor-max)
                                0)
                           ,(if number-max
                                (truncate number-max divisor-min)
                                '*))
                 ;; Different signs, the result will be negative.
                 `(integer ,(if number-max
                                (- (truncate number-max divisor-min))
                                '*)
                           ,(if divisor-max
                                (- (truncate number-min divisor-max))
                                0))))
            ((eq divisor-sign '+)
             ;; The divisor is positive. Therefore, the number will just
             ;; become closer to zero.
             `(integer ,(if number-low
                            (truncate number-low divisor-min)
                            '*)
                       ,(if number-high
                            (truncate number-high divisor-min)
                            '*)))
            ((eq divisor-sign '-)
             ;; The divisor is negative. Therefore, the absolute value of
             ;; the number will become closer to zero, but the sign will also
             ;; change.
             `(integer ,(if number-high
                            (- (truncate number-high divisor-min))
                            '*)
                       ,(if number-low
                            (- (truncate number-low divisor-min))
                            '*)))
            ;; The divisor could be either positive or negative.
            (number-max
             ;; The number we are dividing has a bound. Divide that by the
             ;; smallest posible divisor.
             (let ((bound (truncate number-max divisor-min)))
               `(integer ,(- bound) ,bound)))
            (t
             ;; The number we are dividing is unbounded, so we can't tell
             ;; anything about the result.
             `integer)))))

#+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defun integer-rem-derive-type
       (number-low number-high divisor-low divisor-high)
  (if (and divisor-low divisor-high)
      ;; We know the range of the divisor, and the remainder must be
      ;; smaller than the divisor. We can tell the sign of the
      ;; remainer if we know the sign of the number.
      (let ((divisor-max (1- (max (abs divisor-low) (abs divisor-high)))))
        `(integer ,(if (or (null number-low)
                           (minusp number-low))
                       (- divisor-max)
                       0)
                  ,(if (or (null number-high)
                           (plusp number-high))
                       divisor-max
                       0)))
      ;; The divisor is potentially either very positive or very
      ;; negative. Therefore, the remainer is unbounded, but we might
      ;; be able to tell something about the sign from the number.
      `(integer ,(if (and number-low (not (minusp number-low)))
                     ;; The number we are dividing is positive.
                     ;; Therefore, the remainder must be positive.
                     0
                     '*)
                ,(if (and number-high (not (plusp number-high)))
                     ;; The number we are dividing is negative.
                     ;; Therefore, the remainder must be negative.
                     0
                     '*))))

#+sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defoptimizer (random derive-type) ((bound &optional state))
  (let ((type (lvar-type bound)))
    (when (numeric-type-p type)
      (let ((class (numeric-type-class type))
            (high (numeric-type-high type))
            (format (numeric-type-format type)))
        (make-numeric-type
         :class class
         :format format
         :low (coerce 0 (or format class 'real))
         :high (cond ((not high) nil)
                     ((eq class 'integer) (max (1- high) 0))
                     ((or (consp high) (zerop high)) high)
                     (t `(,high))))))))

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defun random-derive-type-aux (type)
  (let ((class (numeric-type-class type))
        (high (numeric-type-high type))
        (format (numeric-type-format type)))
    (make-numeric-type
         :class class
         :format format
         :low (coerce 0 (or format class 'real))
         :high (cond ((not high) nil)
                     ((eq class 'integer) (max (1- high) 0))
                     ((or (consp high) (zerop high)) high)
                     (t `(,high))))))

#-sb-xc-host ; (See CROSS-FLOAT-INFINITY-KLUDGE.)
(defoptimizer (random derive-type) ((bound &optional state))
  (one-arg-derive-type bound #'random-derive-type-aux nil))

;;;; DERIVE-TYPE methods for LOGAND, LOGIOR, and friends

;;; Return the maximum number of bits an integer of the supplied type
;;; can take up, or NIL if it is unbounded. The second (third) value
;;; is T if the integer can be positive (negative) and NIL if not.
;;; Zero counts as positive.
(defun integer-type-length (type)
  (if (numeric-type-p type)
      (let ((min (numeric-type-low type))
            (max (numeric-type-high type)))
        (values (and min max (max (integer-length min) (integer-length max)))
                (or (null max) (not (minusp max)))
                (or (null min) (minusp min))))
      (values nil t t)))

;;; See _Hacker's Delight_, Henry S. Warren, Jr. pp 58-63 for an
;;; explanation of LOG{AND,IOR,XOR}-DERIVE-UNSIGNED-{LOW,HIGH}-BOUND.
;;; Credit also goes to Raymond Toy for writing (and debugging!) similar
;;; versions in CMUCL, from which these functions copy liberally.

(defun logand-derive-unsigned-low-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (lognor a c))) then (ash m -1)
          until (zerop m) do
          (unless (zerop (logand m (lognot a) (lognot c)))
            (let ((temp (logandc2 (logior a m) (1- m))))
              (when (<= temp b)
                (setf a temp)
                (loop-finish))
              (setf temp (logandc2 (logior c m) (1- m)))
              (when (<= temp d)
                (setf c temp)
                (loop-finish))))
          finally (return (logand a c)))))

(defun logand-derive-unsigned-high-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logxor b d))) then (ash m -1)
          until (zerop m) do
          (cond
            ((not (zerop (logand b (lognot d) m)))
             (let ((temp (logior (logandc2 b m) (1- m))))
               (when (>= temp a)
                 (setf b temp)
                 (loop-finish))))
            ((not (zerop (logand (lognot b) d m)))
             (let ((temp (logior (logandc2 d m) (1- m))))
               (when (>= temp c)
                 (setf d temp)
                 (loop-finish)))))
          finally (return (logand b d)))))

(defun logand-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logand-derive-type-aux x))
  (multiple-value-bind (x-len x-pos x-neg) (integer-type-length x)
    (declare (ignore x-pos))
    (multiple-value-bind (y-len y-pos y-neg) (integer-type-length y)
      (declare (ignore y-pos))
      (if (not x-neg)
          ;; X must be positive.
          (if (not y-neg)
              ;; They must both be positive.
              (cond ((and (null x-len) (null y-len))
                     (specifier-type 'unsigned-byte))
                    ((null x-len)
                     (specifier-type `(unsigned-byte* ,y-len)))
                    ((null y-len)
                     (specifier-type `(unsigned-byte* ,x-len)))
                    (t
                     (let ((low (logand-derive-unsigned-low-bound x y))
                           (high (logand-derive-unsigned-high-bound x y)))
                       (specifier-type `(integer ,low ,high)))))
              ;; X is positive, but Y might be negative.
              (cond ((null x-len)
                     (specifier-type 'unsigned-byte))
                    (t
                     (specifier-type `(unsigned-byte* ,x-len)))))
          ;; X might be negative.
          (if (not y-neg)
              ;; Y must be positive.
              (cond ((null y-len)
                     (specifier-type 'unsigned-byte))
                    (t (specifier-type `(unsigned-byte* ,y-len))))
              ;; Either might be negative.
              (if (and x-len y-len)
                  ;; The result is bounded.
                  (specifier-type `(signed-byte ,(1+ (max x-len y-len))))
                  ;; We can't tell squat about the result.
                  (specifier-type 'integer)))))))

(defun logior-derive-unsigned-low-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logxor a c))) then (ash m -1)
          until (zerop m) do
          (cond
            ((not (zerop (logandc2 (logand c m) a)))
             (let ((temp (logand (logior a m) (1+ (lognot m)))))
               (when (<= temp b)
                 (setf a temp)
                 (loop-finish))))
            ((not (zerop (logandc2 (logand a m) c)))
             (let ((temp (logand (logior c m) (1+ (lognot m)))))
               (when (<= temp d)
                 (setf c temp)
                 (loop-finish)))))
          finally (return (logior a c)))))

(defun logior-derive-unsigned-high-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logand b d))) then (ash m -1)
          until (zerop m) do
          (unless (zerop (logand b d m))
            (let ((temp (logior (- b m) (1- m))))
              (when (>= temp a)
                (setf b temp)
                (loop-finish))
              (setf temp (logior (- d m) (1- m)))
              (when (>= temp c)
                (setf d temp)
                (loop-finish))))
          finally (return (logior b d)))))

(defun logior-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logior-derive-type-aux x))
  (multiple-value-bind (x-len x-pos x-neg) (integer-type-length x)
    (multiple-value-bind (y-len y-pos y-neg) (integer-type-length y)
      (cond
       ((and (not x-neg) (not y-neg))
        ;; Both are positive.
        (if (and x-len y-len)
            (let ((low (logior-derive-unsigned-low-bound x y))
                  (high (logior-derive-unsigned-high-bound x y)))
              (specifier-type `(integer ,low ,high)))
            (specifier-type `(unsigned-byte* *))))
       ((not x-pos)
        ;; X must be negative.
        (if (not y-pos)
            ;; Both are negative. The result is going to be negative
            ;; and be the same length or shorter than the smaller.
            (if (and x-len y-len)
                ;; It's bounded.
                (specifier-type `(integer ,(ash -1 (min x-len y-len)) -1))
                ;; It's unbounded.
                (specifier-type '(integer * -1)))
            ;; X is negative, but we don't know about Y. The result
            ;; will be negative, but no more negative than X.
            (specifier-type
             `(integer ,(or (numeric-type-low x) '*)
                       -1))))
       (t
        ;; X might be either positive or negative.
        (if (not y-pos)
            ;; But Y is negative. The result will be negative.
            (specifier-type
             `(integer ,(or (numeric-type-low y) '*)
                       -1))
            ;; We don't know squat about either. It won't get any bigger.
            (if (and x-len y-len)
                ;; Bounded.
                (specifier-type `(signed-byte ,(1+ (max x-len y-len))))
                ;; Unbounded.
                (specifier-type 'integer))))))))

(defun logxor-derive-unsigned-low-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logxor a c))) then (ash m -1)
          until (zerop m) do
          (cond
            ((not (zerop (logandc2 (logand c m) a)))
             (let ((temp (logand (logior a m)
                                 (1+ (lognot m)))))
               (when (<= temp b)
                 (setf a temp))))
            ((not (zerop (logandc2 (logand a m) c)))
             (let ((temp (logand (logior c m)
                                 (1+ (lognot m)))))
               (when (<= temp d)
                 (setf c temp)))))
          finally (return (logxor a c)))))

(defun logxor-derive-unsigned-high-bound (x y)
  (let ((a (numeric-type-low x))
        (b (numeric-type-high x))
        (c (numeric-type-low y))
        (d (numeric-type-high y)))
    (loop for m = (ash 1 (integer-length (logand b d))) then (ash m -1)
          until (zerop m) do
          (unless (zerop (logand b d m))
            (let ((temp (logior (- b m) (1- m))))
              (cond
                ((>= temp a) (setf b temp))
                (t (let ((temp (logior (- d m) (1- m))))
                     (when (>= temp c)
                       (setf d temp)))))))
          finally (return (logxor b d)))))

(defun logxor-derive-type-aux (x y &optional same-leaf)
  (when same-leaf
    (return-from logxor-derive-type-aux (specifier-type '(eql 0))))
  (multiple-value-bind (x-len x-pos x-neg) (integer-type-length x)
    (multiple-value-bind (y-len y-pos y-neg) (integer-type-length y)
      (cond
        ((and (not x-neg) (not y-neg))
         ;; Both are positive
         (if (and x-len y-len)
             (let ((low (logxor-derive-unsigned-low-bound x y))
                   (high (logxor-derive-unsigned-high-bound x y)))
               (specifier-type `(integer ,low ,high)))
             (specifier-type '(unsigned-byte* *))))
        ((and (not x-pos) (not y-pos))
         ;; Both are negative.  The result will be positive, and as long
         ;; as the longer.
         (specifier-type `(unsigned-byte* ,(if (and x-len y-len)
                                               (max x-len y-len)
                                               '*))))
        ((or (and (not x-pos) (not y-neg))
             (and (not y-pos) (not x-neg)))
         ;; Either X is negative and Y is positive or vice-versa. The
         ;; result will be negative.
         (specifier-type `(integer ,(if (and x-len y-len)
                                        (ash -1 (max x-len y-len))
                                        '*)
                           -1)))
        ;; We can't tell what the sign of the result is going to be.
        ;; All we know is that we don't create new bits.
        ((and x-len y-len)
         (specifier-type `(signed-byte ,(1+ (max x-len y-len)))))
        (t
         (specifier-type 'integer))))))

(macrolet ((deffrob (logfun)
             (let ((fun-aux (symbolicate logfun "-DERIVE-TYPE-AUX")))
             `(defoptimizer (,logfun derive-type) ((x y))
                (two-arg-derive-type x y #',fun-aux #',logfun)))))
  (deffrob logand)
  (deffrob logior)
  (deffrob logxor))

(defoptimizer (logeqv derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux
                              (logxor-derive-type-aux x y same-leaf)))
                       #'logeqv))
(defoptimizer (lognand derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux
                              (logand-derive-type-aux x y same-leaf)))
                       #'lognand))
(defoptimizer (lognor derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (lognot-derive-type-aux
                              (logior-derive-type-aux x y same-leaf)))
                       #'lognor))
(defoptimizer (logandc1 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql 0))
                                 (logand-derive-type-aux
                                  (lognot-derive-type-aux x) y nil)))
                       #'logandc1))
(defoptimizer (logandc2 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql 0))
                                 (logand-derive-type-aux
                                  x (lognot-derive-type-aux y) nil)))
                       #'logandc2))
(defoptimizer (logorc1 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql -1))
                                 (logior-derive-type-aux
                                  (lognot-derive-type-aux x) y nil)))
                       #'logorc1))
(defoptimizer (logorc2 derive-type) ((x y))
  (two-arg-derive-type x y (lambda (x y same-leaf)
                             (if same-leaf
                                 (specifier-type '(eql -1))
                                 (logior-derive-type-aux
                                  x (lognot-derive-type-aux y) nil)))
                       #'logorc2))

;;;; miscellaneous derive-type methods

(defoptimizer (integer-length derive-type) ((x))
  (let ((x-type (lvar-type x)))
    (when (numeric-type-p x-type)
      ;; If the X is of type (INTEGER LO HI), then the INTEGER-LENGTH
      ;; of X is (INTEGER (MIN lo hi) (MAX lo hi), basically.  Be
      ;; careful about LO or HI being NIL, though.  Also, if 0 is
      ;; contained in X, the lower bound is obviously 0.
      (flet ((null-or-min (a b)
               (and a b (min (integer-length a)
                             (integer-length b))))
             (null-or-max (a b)
               (and a b (max (integer-length a)
                             (integer-length b)))))
        (let* ((min (numeric-type-low x-type))
               (max (numeric-type-high x-type))
               (min-len (null-or-min min max))
               (max-len (null-or-max min max)))
          (when (ctypep 0 x-type)
            (setf min-len 0))
          (specifier-type `(integer ,(or min-len '*) ,(or max-len '*))))))))

(defoptimizer (isqrt derive-type) ((x))
  (let ((x-type (lvar-type x)))
    (when (numeric-type-p x-type)
      (let* ((lo (numeric-type-low x-type))
             (hi (numeric-type-high x-type))
             (lo-res (if lo (isqrt lo) '*))
             (hi-res (if hi (isqrt hi) '*)))
        (specifier-type `(integer ,lo-res ,hi-res))))))

(defoptimizer (char-code derive-type) ((char))
  (let ((type (type-intersection (lvar-type char) (specifier-type 'character))))
    (cond ((member-type-p type)
           (specifier-type
            `(member
              ,@(loop for member in (member-type-members type)
                      when (characterp member)
                      collect (char-code member)))))
          ((sb!kernel::character-set-type-p type)
           (specifier-type
            `(or
              ,@(loop for (low . high)
                      in (character-set-type-pairs type)
                      collect `(integer ,low ,high)))))
          ((csubtypep type (specifier-type 'base-char))
           (specifier-type
            `(mod ,base-char-code-limit)))
          (t
           (specifier-type
            `(mod ,char-code-limit))))))

(defoptimizer (code-char derive-type) ((code))
  (let ((type (lvar-type code)))
    ;; FIXME: unions of integral ranges?  It ought to be easier to do
    ;; this, given that CHARACTER-SET is basically an integral range
    ;; type.  -- CSR, 2004-10-04
    (when (numeric-type-p type)
      (let* ((lo (numeric-type-low type))
             (hi (numeric-type-high type))
             (type (specifier-type `(character-set ((,lo . ,hi))))))
        (cond
          ;; KLUDGE: when running on the host, we lose a slight amount
          ;; of precision so that we don't have to "unparse" types
          ;; that formally we can't, such as (CHARACTER-SET ((0
          ;; . 0))).  -- CSR, 2004-10-06
          #+sb-xc-host
          ((csubtypep type (specifier-type 'standard-char)) type)
          #+sb-xc-host
          ((csubtypep type (specifier-type 'base-char))
           (specifier-type 'base-char))
          #+sb-xc-host
          ((csubtypep type (specifier-type 'extended-char))
           (specifier-type 'extended-char))
          (t #+sb-xc-host (specifier-type 'character)
             #-sb-xc-host type))))))

(defoptimizer (values derive-type) ((&rest values))
  (make-values-type :required (mapcar #'lvar-type values)))

(defun signum-derive-type-aux (type)
  (if (eq (numeric-type-complexp type) :complex)
      (let* ((format (case (numeric-type-class type)
                          ((integer rational) 'single-float)
                          (t (numeric-type-format type))))
                (bound-format (or format 'float)))
           (make-numeric-type :class 'float
                              :format format
                              :complexp :complex
                              :low (coerce -1 bound-format)
                              :high (coerce 1 bound-format)))
      (let* ((interval (numeric-type->interval type))
             (range-info (interval-range-info interval))
             (contains-0-p (interval-contains-p 0 interval))
             (class (numeric-type-class type))
             (format (numeric-type-format type))
             (one (coerce 1 (or format class 'real)))
             (zero (coerce 0 (or format class 'real)))
             (minus-one (coerce -1 (or format class 'real)))
             (plus (make-numeric-type :class class :format format
                                      :low one :high one))
             (minus (make-numeric-type :class class :format format
                                       :low minus-one :high minus-one))
             ;; KLUDGE: here we have a fairly horrible hack to deal
             ;; with the schizophrenia in the type derivation engine.
             ;; The problem is that the type derivers reinterpret
             ;; numeric types as being exact; so (DOUBLE-FLOAT 0d0
             ;; 0d0) within the derivation mechanism doesn't include
             ;; -0d0.  Ugh.  So force it in here, instead.
             (zero (make-numeric-type :class class :format format
                                      :low (- zero) :high zero)))
        (case range-info
          (+ (if contains-0-p (type-union plus zero) plus))
          (- (if contains-0-p (type-union minus zero) minus))
          (t (type-union minus zero plus))))))

(defoptimizer (signum derive-type) ((num))
  (one-arg-derive-type num #'signum-derive-type-aux nil))

;;;; byte operations
;;;;
;;;; We try to turn byte operations into simple logical operations.
;;;; First, we convert byte specifiers into separate size and position
;;;; arguments passed to internal %FOO functions. We then attempt to
;;;; transform the %FOO functions into boolean operations when the
;;;; size and position are constant and the operands are fixnums.

(macrolet (;; Evaluate body with SIZE-VAR and POS-VAR bound to
           ;; expressions that evaluate to the SIZE and POSITION of
           ;; the byte-specifier form SPEC. We may wrap a let around
           ;; the result of the body to bind some variables.
           ;;
           ;; If the spec is a BYTE form, then bind the vars to the
           ;; subforms. otherwise, evaluate SPEC and use the BYTE-SIZE
           ;; and BYTE-POSITION. The goal of this transformation is to
           ;; avoid consing up byte specifiers and then immediately
           ;; throwing them away.
           (with-byte-specifier ((size-var pos-var spec) &body body)
             (once-only ((spec `(macroexpand ,spec))
                         (temp '(gensym)))
                        `(if (and (consp ,spec)
                                  (eq (car ,spec) 'byte)
                                  (= (length ,spec) 3))
                        (let ((,size-var (second ,spec))
                              (,pos-var (third ,spec)))
                          ,@body)
                        (let ((,size-var `(byte-size ,,temp))
                              (,pos-var `(byte-position ,,temp)))
                          `(let ((,,temp ,,spec))
                             ,,@body))))))

  (define-source-transform ldb (spec int)
    (with-byte-specifier (size pos spec)
      `(%ldb ,size ,pos ,int)))

  (define-source-transform dpb (newbyte spec int)
    (with-byte-specifier (size pos spec)
      `(%dpb ,newbyte ,size ,pos ,int)))

  (define-source-transform mask-field (spec int)
    (with-byte-specifier (size pos spec)
      `(%mask-field ,size ,pos ,int)))

  (define-source-transform deposit-field (newbyte spec int)
    (with-byte-specifier (size pos spec)
      `(%deposit-field ,newbyte ,size ,pos ,int))))

(defoptimizer (%ldb derive-type) ((size posn num))
  (let ((size (lvar-type size)))
    (if (and (numeric-type-p size)
             (csubtypep size (specifier-type 'integer)))
        (let ((size-high (numeric-type-high size)))
          (if (and size-high (<= size-high sb!vm:n-word-bits))
              (specifier-type `(unsigned-byte* ,size-high))
              (specifier-type 'unsigned-byte)))
        *universal-type*)))

(defoptimizer (%mask-field derive-type) ((size posn num))
  (let ((size (lvar-type size))
        (posn (lvar-type posn)))
    (if (and (numeric-type-p size)
             (csubtypep size (specifier-type 'integer))
             (numeric-type-p posn)
             (csubtypep posn (specifier-type 'integer)))
        (let ((size-high (numeric-type-high size))
              (posn-high (numeric-type-high posn)))
          (if (and size-high posn-high
                   (<= (+ size-high posn-high) sb!vm:n-word-bits))
              (specifier-type `(unsigned-byte* ,(+ size-high posn-high)))
              (specifier-type 'unsigned-byte)))
        *universal-type*)))

(defun %deposit-field-derive-type-aux (size posn int)
  (let ((size (lvar-type size))
        (posn (lvar-type posn))
        (int (lvar-type int)))
    (when (and (numeric-type-p size)
               (numeric-type-p posn)
               (numeric-type-p int))
      (let ((size-high (numeric-type-high size))
            (posn-high (numeric-type-high posn))
            (high (numeric-type-high int))
            (low (numeric-type-low int)))
        (when (and size-high posn-high high low
                   ;; KLUDGE: we need this cutoff here, otherwise we
                   ;; will merrily derive the type of %DPB as
                   ;; (UNSIGNED-BYTE 1073741822), and then attempt to
                   ;; canonicalize this type to (INTEGER 0 (1- (ASH 1
                   ;; 1073741822))), with hilarious consequences.  We
                   ;; cutoff at 4*SB!VM:N-WORD-BITS to allow inference
                   ;; over a reasonable amount of shifting, even on
                   ;; the alpha/32 port, where N-WORD-BITS is 32 but
                   ;; machine integers are 64-bits.  -- CSR,
                   ;; 2003-09-12
                   (<= (+ size-high posn-high) (* 4 sb!vm:n-word-bits)))
          (let ((raw-bit-count (max (integer-length high)
                                    (integer-length low)
                                    (+ size-high posn-high))))
            (specifier-type
             (if (minusp low)
                 `(signed-byte ,(1+ raw-bit-count))
                 `(unsigned-byte* ,raw-bit-count)))))))))

(defoptimizer (%dpb derive-type) ((newbyte size posn int))
  (%deposit-field-derive-type-aux size posn int))

(defoptimizer (%deposit-field derive-type) ((newbyte size posn int))
  (%deposit-field-derive-type-aux size posn int))

(deftransform %ldb ((size posn int)
                    (fixnum fixnum integer)
                    (unsigned-byte #.sb!vm:n-word-bits))
  "convert to inline logical operations"
  `(logand (ash int (- posn))
           (ash ,(1- (ash 1 sb!vm:n-word-bits))
                (- size ,sb!vm:n-word-bits))))

(deftransform %mask-field ((size posn int)
                           (fixnum fixnum integer)
                           (unsigned-byte #.sb!vm:n-word-bits))
  "convert to inline logical operations"
  `(logand int
           (ash (ash ,(1- (ash 1 sb!vm:n-word-bits))
                     (- size ,sb!vm:n-word-bits))
                posn)))

;;; Note: for %DPB and %DEPOSIT-FIELD, we can't use
;;;   (OR (SIGNED-BYTE N) (UNSIGNED-BYTE N))
;;; as the result type, as that would allow result types that cover
;;; the range -2^(n-1) .. 1-2^n, instead of allowing result types of
;;; (UNSIGNED-BYTE N) and result types of (SIGNED-BYTE N).

(deftransform %dpb ((new size posn int)
                    *
                    (unsigned-byte #.sb!vm:n-word-bits))
  "convert to inline logical operations"
  `(let ((mask (ldb (byte size 0) -1)))
     (logior (ash (logand new mask) posn)
             (logand int (lognot (ash mask posn))))))

(deftransform %dpb ((new size posn int)
                    *
                    (signed-byte #.sb!vm:n-word-bits))
  "convert to inline logical operations"
  `(let ((mask (ldb (byte size 0) -1)))
     (logior (ash (logand new mask) posn)
             (logand int (lognot (ash mask posn))))))

(deftransform %deposit-field ((new size posn int)
                              *
                              (unsigned-byte #.sb!vm:n-word-bits))
  "convert to inline logical operations"
  `(let ((mask (ash (ldb (byte size 0) -1) posn)))
     (logior (logand new mask)
             (logand int (lognot mask)))))

(deftransform %deposit-field ((new size posn int)
                              *
                              (signed-byte #.sb!vm:n-word-bits))
  "convert to inline logical operations"
  `(let ((mask (ash (ldb (byte size 0) -1) posn)))
     (logior (logand new mask)
             (logand int (lognot mask)))))

(defoptimizer (mask-signed-field derive-type) ((size x))
  (let ((size (lvar-type size)))
    (if (numeric-type-p size)
        (let ((size-high (numeric-type-high size)))
          (if (and size-high (<= 1 size-high sb!vm:n-word-bits))
              (specifier-type `(signed-byte ,size-high))
              *universal-type*))
        *universal-type*)))


;;; Modular functions

;;; (ldb (byte s 0) (foo                 x  y ...)) =
;;; (ldb (byte s 0) (foo (ldb (byte s 0) x) y ...))
;;;
;;; and similar for other arguments.

(defun make-modular-fun-type-deriver (prototype kind width signedp)
  (declare (ignore kind))
  #!-sb-fluid
  (binding* ((info (info :function :info prototype) :exit-if-null)
             (fun (fun-info-derive-type info) :exit-if-null)
             (mask-type (specifier-type
                         (ecase signedp
                             ((nil) (let ((mask (1- (ash 1 width))))
                                      `(integer ,mask ,mask)))
                             ((t) `(signed-byte ,width))))))
    (lambda (call)
      (let ((res (funcall fun call)))
        (when res
          (if (eq signedp nil)
              (logand-derive-type-aux res mask-type))))))
  #!+sb-fluid
  (lambda (call)
    (binding* ((info (info :function :info prototype) :exit-if-null)
               (fun (fun-info-derive-type info) :exit-if-null)
               (res (funcall fun call) :exit-if-null)
               (mask-type (specifier-type
                           (ecase signedp
                             ((nil) (let ((mask (1- (ash 1 width))))
                                      `(integer ,mask ,mask)))
                             ((t) `(signed-byte ,width))))))
      (if (eq signedp nil)
          (logand-derive-type-aux res mask-type)))))

;;; Try to recursively cut all uses of LVAR to WIDTH bits.
;;;
;;; For good functions, we just recursively cut arguments; their
;;; "goodness" means that the result will not increase (in the
;;; (unsigned-byte +infinity) sense). An ordinary modular function is
;;; replaced with the version, cutting its result to WIDTH or more
;;; bits. For most functions (e.g. for +) we cut all arguments; for
;;; others (e.g. for ASH) we have "optimizers", cutting only necessary
;;; arguments (maybe to a different width) and returning the name of a
;;; modular version, if it exists, or NIL. If we have changed
;;; anything, we need to flush old derived types, because they have
;;; nothing in common with the new code.
(defun cut-to-width (lvar kind width signedp)
  (declare (type lvar lvar) (type (integer 0) width))
  (let ((type (specifier-type (if (zerop width)
                                  '(eql 0)
                                  `(,(ecase signedp
                                       ((nil) 'unsigned-byte)
                                       ((t) 'signed-byte))
                                     ,width)))))
    (labels ((reoptimize-node (node name)
               (setf (node-derived-type node)
                     (fun-type-returns
                      (info :function :type name)))
               (setf (lvar-%derived-type (node-lvar node)) nil)
               (setf (node-reoptimize node) t)
               (setf (block-reoptimize (node-block node)) t)
               (reoptimize-component (node-component node) :maybe))
             (cut-node (node &aux did-something)
               (when (and (not (block-delete-p (node-block node)))
                          (combination-p node)
                          (eq (basic-combination-kind node) :known))
                 (let* ((fun-ref (lvar-use (combination-fun node)))
                        (fun-name (leaf-source-name (ref-leaf fun-ref)))
                        (modular-fun (find-modular-version fun-name kind signedp width)))
                   (when (and modular-fun
                              (not (and (eq fun-name 'logand)
                                        (csubtypep
                                         (single-value-type (node-derived-type node))
                                         type))))
                     (binding* ((name (etypecase modular-fun
                                        ((eql :good) fun-name)
                                        (modular-fun-info
                                         (modular-fun-info-name modular-fun))
                                        (function
                                         (funcall modular-fun node width)))
                                      :exit-if-null))
                               (unless (eql modular-fun :good)
                                 (setq did-something t)
                                 (change-ref-leaf
                                  fun-ref
                                  (find-free-fun name "in a strange place"))
                                 (setf (combination-kind node) :full))
                               (unless (functionp modular-fun)
                                 (dolist (arg (basic-combination-args node))
                                   (when (cut-lvar arg)
                                     (setq did-something t))))
                               (when did-something
                                 (reoptimize-node node name))
                               did-something)))))
             (cut-lvar (lvar &aux did-something)
               (do-uses (node lvar)
                 (when (cut-node node)
                   (setq did-something t)))
               did-something))
      (cut-lvar lvar))))

(defun best-modular-version (width signedp)
  ;; 1. exact width-matched :untagged
  ;; 2. >/>= width-matched :tagged
  ;; 3. >/>= width-matched :untagged
  (let* ((uuwidths (modular-class-widths *untagged-unsigned-modular-class*))
         (uswidths (modular-class-widths *untagged-signed-modular-class*))
         (uwidths (merge 'list uuwidths uswidths #'< :key #'car))
         (twidths (modular-class-widths *tagged-modular-class*)))
    (let ((exact (find (cons width signedp) uwidths :test #'equal)))
      (when exact
        (return-from best-modular-version (values width :untagged signedp))))
    (flet ((inexact-match (w)
             (cond
               ((eq signedp (cdr w)) (<= width (car w)))
               ((eq signedp nil) (< width (car w))))))
      (let ((tgt (find-if #'inexact-match twidths)))
        (when tgt
          (return-from best-modular-version
            (values (car tgt) :tagged (cdr tgt)))))
      (let ((ugt (find-if #'inexact-match uwidths)))
        (when ugt
          (return-from best-modular-version
            (values (car ugt) :untagged (cdr ugt))))))))

(defoptimizer (logand optimizer) ((x y) node)
  (let ((result-type (single-value-type (node-derived-type node))))
    (when (numeric-type-p result-type)
      (let ((low (numeric-type-low result-type))
            (high (numeric-type-high result-type)))
        (when (and (numberp low)
                   (numberp high)
                   (>= low 0))
          (let ((width (integer-length high)))
            (multiple-value-bind (w kind signedp)
                (best-modular-version width nil)
              (when w
                ;; FIXME: This should be (CUT-TO-WIDTH NODE KIND WIDTH SIGNEDP).
                (cut-to-width x kind width signedp)
                (cut-to-width y kind width signedp)
                nil ; After fixing above, replace with T.
                ))))))))

(defoptimizer (mask-signed-field optimizer) ((width x) node)
  (let ((result-type (single-value-type (node-derived-type node))))
    (when (numeric-type-p result-type)
      (let ((low (numeric-type-low result-type))
            (high (numeric-type-high result-type)))
        (when (and (numberp low) (numberp high))
          (let ((width (max (integer-length high) (integer-length low))))
            (multiple-value-bind (w kind)
                (best-modular-version width t)
              (when w
                ;; FIXME: This should be (CUT-TO-WIDTH NODE KIND WIDTH T).
                (cut-to-width x kind width t)
                nil ; After fixing above, replace with T.
                ))))))))

;;; miscellanous numeric transforms

;;; If a constant appears as the first arg, swap the args.
(deftransform commutative-arg-swap ((x y) * * :defun-only t :node node)
  (if (and (constant-lvar-p x)
           (not (constant-lvar-p y)))
      `(,(lvar-fun-name (basic-combination-fun node))
        y
        ,(lvar-value x))
      (give-up-ir1-transform)))

(dolist (x '(= char= + * logior logand logxor))
  (%deftransform x '(function * *) #'commutative-arg-swap
                 "place constant arg last"))

;;; Handle the case of a constant BOOLE-CODE.
(deftransform boole ((op x y) * *)
  "convert to inline logical operations"
  (unless (constant-lvar-p op)
    (give-up-ir1-transform "BOOLE code is not a constant."))
  (let ((control (lvar-value op)))
    (case control
      (#.sb!xc:boole-clr 0)
      (#.sb!xc:boole-set -1)
      (#.sb!xc:boole-1 'x)
      (#.sb!xc:boole-2 'y)
      (#.sb!xc:boole-c1 '(lognot x))
      (#.sb!xc:boole-c2 '(lognot y))
      (#.sb!xc:boole-and '(logand x y))
      (#.sb!xc:boole-ior '(logior x y))
      (#.sb!xc:boole-xor '(logxor x y))
      (#.sb!xc:boole-eqv '(logeqv x y))
      (#.sb!xc:boole-nand '(lognand x y))
      (#.sb!xc:boole-nor '(lognor x y))
      (#.sb!xc:boole-andc1 '(logandc1 x y))
      (#.sb!xc:boole-andc2 '(logandc2 x y))
      (#.sb!xc:boole-orc1 '(logorc1 x y))
      (#.sb!xc:boole-orc2 '(logorc2 x y))
      (t
       (abort-ir1-transform "~S is an illegal control arg to BOOLE."
                            control)))))

;;;; converting special case multiply/divide to shifts

;;; If arg is a constant power of two, turn * into a shift.
(deftransform * ((x y) (integer integer) *)
  "convert x*2^k to shift"
  (unless (constant-lvar-p y)
    (give-up-ir1-transform))
  (let* ((y (lvar-value y))
         (y-abs (abs y))
         (len (1- (integer-length y-abs))))
    (unless (and (> y-abs 0) (= y-abs (ash 1 len)))
      (give-up-ir1-transform))
    (if (minusp y)
        `(- (ash x ,len))
        `(ash x ,len))))

;;; If arg is a constant power of two, turn FLOOR into a shift and
;;; mask. If CEILING, add in (1- (ABS Y)), do FLOOR and correct a
;;; remainder.
(flet ((frob (y ceil-p)
         (unless (constant-lvar-p y)
           (give-up-ir1-transform))
         (let* ((y (lvar-value y))
                (y-abs (abs y))
                (len (1- (integer-length y-abs))))
           (unless (and (> y-abs 0) (= y-abs (ash 1 len)))
             (give-up-ir1-transform))
           (let ((shift (- len))
                 (mask (1- y-abs))
                 (delta (if ceil-p (* (signum y) (1- y-abs)) 0)))
             `(let ((x (+ x ,delta)))
                ,(if (minusp y)
                     `(values (ash (- x) ,shift)
                              (- (- (logand (- x) ,mask)) ,delta))
                     `(values (ash x ,shift)
                              (- (logand x ,mask) ,delta))))))))
  (deftransform floor ((x y) (integer integer) *)
    "convert division by 2^k to shift"
    (frob y nil))
  (deftransform ceiling ((x y) (integer integer) *)
    "convert division by 2^k to shift"
    (frob y t)))

;;; Do the same for MOD.
(deftransform mod ((x y) (integer integer) *)
  "convert remainder mod 2^k to LOGAND"
  (unless (constant-lvar-p y)
    (give-up-ir1-transform))
  (let* ((y (lvar-value y))
         (y-abs (abs y))
         (len (1- (integer-length y-abs))))
    (unless (and (> y-abs 0) (= y-abs (ash 1 len)))
      (give-up-ir1-transform))
    (let ((mask (1- y-abs)))
      (if (minusp y)
          `(- (logand (- x) ,mask))
          `(logand x ,mask)))))

;;; If arg is a constant power of two, turn TRUNCATE into a shift and mask.
(deftransform truncate ((x y) (integer integer))
  "convert division by 2^k to shift"
  (unless (constant-lvar-p y)
    (give-up-ir1-transform))
  (let* ((y (lvar-value y))
         (y-abs (abs y))
         (len (1- (integer-length y-abs))))
    (unless (and (> y-abs 0) (= y-abs (ash 1 len)))
      (give-up-ir1-transform))
    (let* ((shift (- len))
           (mask (1- y-abs)))
      `(if (minusp x)
           (values ,(if (minusp y)
                        `(ash (- x) ,shift)
                        `(- (ash (- x) ,shift)))
                   (- (logand (- x) ,mask)))
           (values ,(if (minusp y)
                        `(ash (- ,mask x) ,shift)
                        `(ash x ,shift))
                   (logand x ,mask))))))

;;; And the same for REM.
(deftransform rem ((x y) (integer integer) *)
  "convert remainder mod 2^k to LOGAND"
  (unless (constant-lvar-p y)
    (give-up-ir1-transform))
  (let* ((y (lvar-value y))
         (y-abs (abs y))
         (len (1- (integer-length y-abs))))
    (unless (and (> y-abs 0) (= y-abs (ash 1 len)))
      (give-up-ir1-transform))
    (let ((mask (1- y-abs)))
      `(if (minusp x)
           (- (logand (- x) ,mask))
           (logand x ,mask)))))

;;;; arithmetic and logical identity operation elimination

;;; Flush calls to various arith functions that convert to the
;;; identity function or a constant.
(macrolet ((def (name identity result)
             `(deftransform ,name ((x y) (* (constant-arg (member ,identity))) *)
                "fold identity operations"
                ',result)))
  (def ash 0 x)
  (def logand -1 x)
  (def logand 0 0)
  (def logior 0 x)
  (def logior -1 -1)
  (def logxor -1 (lognot x))
  (def logxor 0 x))

(deftransform logand ((x y) (* (constant-arg t)) *)
  "fold identity operation"
  (let ((y (lvar-value y)))
    (unless (and (plusp y)
                 (= y (1- (ash 1 (integer-length y)))))
      (give-up-ir1-transform))
    (unless (csubtypep (lvar-type x)
                       (specifier-type `(integer 0 ,y)))
      (give-up-ir1-transform))
    'x))

(deftransform mask-signed-field ((size x) ((constant-arg t) *) *)
  "fold identity operation"
  (let ((size (lvar-value size)))
    (unless (csubtypep (lvar-type x) (specifier-type `(signed-byte ,size)))
      (give-up-ir1-transform))
    'x))

;;; These are restricted to rationals, because (- 0 0.0) is 0.0, not -0.0, and
;;; (* 0 -4.0) is -0.0.
(deftransform - ((x y) ((constant-arg (member 0)) rational) *)
  "convert (- 0 x) to negate"
  '(%negate y))
(deftransform * ((x y) (rational (constant-arg (member 0))) *)
  "convert (* x 0) to 0"
  0)

;;; Return T if in an arithmetic op including lvars X and Y, the
;;; result type is not affected by the type of X. That is, Y is at
;;; least as contagious as X.
#+nil
(defun not-more-contagious (x y)
  (declare (type continuation x y))
  (let ((x (lvar-type x))
        (y (lvar-type y)))
    (values (type= (numeric-contagion x y)
                   (numeric-contagion y y)))))
;;; Patched version by Raymond Toy. dtc: Should be safer although it
;;; XXX needs more work as valid transforms are missed; some cases are
;;; specific to particular transform functions so the use of this
;;; function may need a re-think.
(defun not-more-contagious (x y)
  (declare (type lvar x y))
  (flet ((simple-numeric-type (num)
           (and (numeric-type-p num)
                ;; Return non-NIL if NUM is integer, rational, or a float
                ;; of some type (but not FLOAT)
                (case (numeric-type-class num)
                  ((integer rational)
                   t)
                  (float
                   (numeric-type-format num))
                  (t
                   nil)))))
    (let ((x (lvar-type x))
          (y (lvar-type y)))
      (if (and (simple-numeric-type x)
               (simple-numeric-type y))
          (values (type= (numeric-contagion x y)
                         (numeric-contagion y y)))))))

(def!type exact-number ()
  '(or rational (complex rational)))

;;; Fold (+ x 0).
;;;
;;; Only safely applicable for exact numbers. For floating-point
;;; x, one would have to first show that neither x or y are signed
;;; 0s, and that x isn't an SNaN.
(deftransform + ((x y) (exact-number (constant-arg (eql 0))) *)
  "fold zero arg"
  'x)

;;; Fold (- x 0).
(deftransform - ((x y) (exact-number (constant-arg (eql 0))) *)
  "fold zero arg"
  'x)

;;; Fold (OP x +/-1)
;;;
;;; %NEGATE might not always signal correctly.
(macrolet
    ((def (name result minus-result)
         `(deftransform ,name ((x y)
                               (exact-number (constant-arg (member 1 -1))))
            "fold identity operations"
            (if (minusp (lvar-value y)) ',minus-result ',result))))
  (def * x (%negate x))
  (def / x (%negate x))
  (def expt x (/ 1 x)))

;;; Fold (expt x n) into multiplications for small integral values of
;;; N; convert (expt x 1/2) to sqrt.
(deftransform expt ((x y) (t (constant-arg real)) *)
  "recode as multiplication or sqrt"
  (let ((val (lvar-value y)))
    ;; If Y would cause the result to be promoted to the same type as
    ;; Y, we give up. If not, then the result will be the same type
    ;; as X, so we can replace the exponentiation with simple
    ;; multiplication and division for small integral powers.
    (unless (not-more-contagious y x)
      (give-up-ir1-transform))
    (cond ((zerop val)
           (let ((x-type (lvar-type x)))
             (cond ((csubtypep x-type (specifier-type '(or rational
                                                        (complex rational))))
                    '1)
                   ((csubtypep x-type (specifier-type 'real))
                    `(if (rationalp x)
                         1
                         (float 1 x)))
                   ((csubtypep x-type (specifier-type 'complex))
                    ;; both parts are float
                    `(1+ (* x ,val)))
                   (t (give-up-ir1-transform)))))
          ((= val 2) '(* x x))
          ((= val -2) '(/ (* x x)))
          ((= val 3) '(* x x x))
          ((= val -3) '(/ (* x x x)))
          ((= val 1/2) '(sqrt x))
          ((= val -1/2) '(/ (sqrt x)))
          (t (give-up-ir1-transform)))))

(deftransform expt ((x y) ((constant-arg (member -1 -1.0 -1.0d0)) integer) *)
  "recode as an ODDP check"
  (let ((val (lvar-value x)))
    (if (eql -1 val)
        '(- 1 (* 2 (logand 1 y)))
        `(if (oddp y)
             ,val
             ,(abs val)))))

;;; KLUDGE: Shouldn't (/ 0.0 0.0), etc. cause exceptions in these
;;; transformations?
;;; Perhaps we should have to prove that the denominator is nonzero before
;;; doing them?  -- WHN 19990917
(macrolet ((def (name)
             `(deftransform ,name ((x y) ((constant-arg (integer 0 0)) integer)
                                   *)
                "fold zero arg"
                0)))
  (def ash)
  (def /))

(macrolet ((def (name)
             `(deftransform ,name ((x y) ((constant-arg (integer 0 0)) integer)
                                   *)
                "fold zero arg"
                '(values 0 0))))
  (def truncate)
  (def round)
  (def floor)
  (def ceiling))

;;;; character operations

(deftransform char-equal ((a b) (base-char base-char))
  "open code"
  '(let* ((ac (char-code a))
          (bc (char-code b))
          (sum (logxor ac bc)))
     (or (zerop sum)
         (when (eql sum #x20)
           (let ((sum (+ ac bc)))
             (or (and (> sum 161) (< sum 213))
                 (and (> sum 415) (< sum 461))
                 (and (> sum 463) (< sum 477))))))))

(deftransform char-upcase ((x) (base-char))
  "open code"
  '(let ((n-code (char-code x)))
     (if (or (and (> n-code #o140)      ; Octal 141 is #\a.
                  (< n-code #o173))     ; Octal 172 is #\z.
             (and (> n-code #o337)
                  (< n-code #o367))
             (and (> n-code #o367)
                  (< n-code #o377)))
         (code-char (logxor #x20 n-code))
         x)))

(deftransform char-downcase ((x) (base-char))
  "open code"
  '(let ((n-code (char-code x)))
     (if (or (and (> n-code 64)         ; 65 is #\A.
                  (< n-code 91))        ; 90 is #\Z.
             (and (> n-code 191)
                  (< n-code 215))
             (and (> n-code 215)
                  (< n-code 223)))
         (code-char (logxor #x20 n-code))
         x)))

;;;; equality predicate transforms

;;; Return true if X and Y are lvars whose only use is a
;;; reference to the same leaf, and the value of the leaf cannot
;;; change.
(defun same-leaf-ref-p (x y)
  (declare (type lvar x y))
  (let ((x-use (principal-lvar-use x))
        (y-use (principal-lvar-use y)))
    (and (ref-p x-use)
         (ref-p y-use)
         (eq (ref-leaf x-use) (ref-leaf y-use))
         (constant-reference-p x-use))))

;;; If X and Y are the same leaf, then the result is true. Otherwise,
;;; if there is no intersection between the types of the arguments,
;;; then the result is definitely false.
(deftransform simple-equality-transform ((x y) * *
                                         :defun-only t)
  (cond
    ((same-leaf-ref-p x y) t)
    ((not (types-equal-or-intersect (lvar-type x) (lvar-type y)))
         nil)
    (t (give-up-ir1-transform))))

(macrolet ((def (x)
             `(%deftransform ',x '(function * *) #'simple-equality-transform)))
  (def eq)
  (def char=))

;;; This is similar to SIMPLE-EQUALITY-TRANSFORM, except that we also
;;; try to convert to a type-specific predicate or EQ:
;;; -- If both args are characters, convert to CHAR=. This is better than
;;;    just converting to EQ, since CHAR= may have special compilation
;;;    strategies for non-standard representations, etc.
;;; -- If either arg is definitely a fixnum, we check to see if X is
;;;    constant and if so, put X second. Doing this results in better
;;;    code from the backend, since the backend assumes that any constant
;;;    argument comes second.
;;; -- If either arg is definitely not a number or a fixnum, then we
;;;    can compare with EQ.
;;; -- Otherwise, we try to put the arg we know more about second. If X
;;;    is constant then we put it second. If X is a subtype of Y, we put
;;;    it second. These rules make it easier for the back end to match
;;;    these interesting cases.
(deftransform eql ((x y) * * :node node)
  "convert to simpler equality predicate"
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y))
        (char-type (specifier-type 'character)))
    (flet ((fixnum-type-p (type)
             (csubtypep type (specifier-type 'fixnum))))
      (cond
        ((same-leaf-ref-p x y) t)
        ((not (types-equal-or-intersect x-type y-type))
         nil)
        ((and (csubtypep x-type char-type)
              (csubtypep y-type char-type))
         '(char= x y))
        ((or (fixnum-type-p x-type) (fixnum-type-p y-type))
         (commutative-arg-swap node))
        ((or (eq-comparable-type-p x-type) (eq-comparable-type-p y-type))
         '(eq x y))
        ((and (not (constant-lvar-p y))
              (or (constant-lvar-p x)
                  (and (csubtypep x-type y-type)
                       (not (csubtypep y-type x-type)))))
         '(eql y x))
        (t
         (give-up-ir1-transform))))))

;;; similarly to the EQL transform above, we attempt to constant-fold
;;; or convert to a simpler predicate: mostly we have to be careful
;;; with strings and bit-vectors.
(deftransform equal ((x y) * *)
  "convert to simpler equality predicate"
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y))
        (string-type (specifier-type 'string))
        (bit-vector-type (specifier-type 'bit-vector)))
    (cond
      ((same-leaf-ref-p x y) t)
      ((and (csubtypep x-type string-type)
            (csubtypep y-type string-type))
       '(string= x y))
      ((and (csubtypep x-type bit-vector-type)
            (csubtypep y-type bit-vector-type))
       '(bit-vector-= x y))
      ;; if at least one is not a string, and at least one is not a
      ;; bit-vector, then we can reason from types.
      ((and (not (and (types-equal-or-intersect x-type string-type)
                      (types-equal-or-intersect y-type string-type)))
            (not (and (types-equal-or-intersect x-type bit-vector-type)
                      (types-equal-or-intersect y-type bit-vector-type)))
            (not (types-equal-or-intersect x-type y-type)))
       nil)
      (t (give-up-ir1-transform)))))

;;; Convert to EQL if both args are rational and complexp is specified
;;; and the same for both.
(deftransform = ((x y) (number number) *)
  "open code"
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y)))
    (cond ((or (and (csubtypep x-type (specifier-type 'float))
                    (csubtypep y-type (specifier-type 'float)))
               (and (csubtypep x-type (specifier-type '(complex float)))
                    (csubtypep y-type (specifier-type '(complex float))))
               #!+complex-float-vops
               (and (csubtypep x-type (specifier-type '(or single-float (complex single-float))))
                    (csubtypep y-type (specifier-type '(or single-float (complex single-float)))))
               #!+complex-float-vops
               (and (csubtypep x-type (specifier-type '(or double-float (complex double-float))))
                    (csubtypep y-type (specifier-type '(or double-float (complex double-float))))))
           ;; They are both floats. Leave as = so that -0.0 is
           ;; handled correctly.
           (give-up-ir1-transform))
          ((or (and (csubtypep x-type (specifier-type 'rational))
                    (csubtypep y-type (specifier-type 'rational)))
               (and (csubtypep x-type
                               (specifier-type '(complex rational)))
                    (csubtypep y-type
                               (specifier-type '(complex rational)))))
           ;; They are both rationals and complexp is the same.
           ;; Convert to EQL.
           '(eql x y))
          (t
           (give-up-ir1-transform
            "The operands might not be the same type.")))))

(defun maybe-float-lvar-p (lvar)
  (neq *empty-type* (type-intersection (specifier-type 'float)
                                       (lvar-type lvar))))

(flet ((maybe-invert (node op inverted x y)
         ;; Don't invert if either argument can be a float (NaNs)
         (cond
           ((or (maybe-float-lvar-p x) (maybe-float-lvar-p y))
            (delay-ir1-transform node :constraint)
            `(or (,op x y) (= x y)))
           (t
            `(if (,inverted x y) nil t)))))
  (deftransform >= ((x y) (number number) * :node node)
    "invert or open code"
    (maybe-invert node '> '< x y))
  (deftransform <= ((x y) (number number) * :node node)
    "invert or open code"
    (maybe-invert node '< '> x y)))

;;; See whether we can statically determine (< X Y) using type
;;; information. If X's high bound is < Y's low, then X < Y.
;;; Similarly, if X's low is >= to Y's high, the X >= Y (so return
;;; NIL). If not, at least make sure any constant arg is second.
(macrolet ((def (name inverse reflexive-p surely-true surely-false)
             `(deftransform ,name ((x y))
                "optimize using intervals"
                (if (and (same-leaf-ref-p x y)
                         ;; For non-reflexive functions we don't need
                         ;; to worry about NaNs: (non-ref-op NaN NaN) => false,
                         ;; but with reflexive ones we don't know...
                         ,@(when reflexive-p
                                 '((and (not (maybe-float-lvar-p x))
                                        (not (maybe-float-lvar-p y))))))
                    ,reflexive-p
                    (let ((ix (or (type-approximate-interval (lvar-type x))
                                  (give-up-ir1-transform)))
                          (iy (or (type-approximate-interval (lvar-type y))
                                  (give-up-ir1-transform))))
                      (cond (,surely-true
                             t)
                            (,surely-false
                             nil)
                            ((and (constant-lvar-p x)
                                  (not (constant-lvar-p y)))
                             `(,',inverse y x))
                            (t
                             (give-up-ir1-transform))))))))
  (def = = t (interval-= ix iy) (interval-/= ix iy))
  (def /= /= nil (interval-/= ix iy) (interval-= ix iy))
  (def < > nil (interval-< ix iy) (interval->= ix iy))
  (def > < nil (interval-< iy ix) (interval->= iy ix))
  (def <= >= t (interval->= iy ix) (interval-< iy ix))
  (def >= <= t (interval->= ix iy) (interval-< ix iy)))

(defun ir1-transform-char< (x y first second inverse)
  (cond
    ((same-leaf-ref-p x y) nil)
    ;; If we had interval representation of character types, as we
    ;; might eventually have to to support 2^21 characters, then here
    ;; we could do some compile-time computation as in transforms for
    ;; < above. -- CSR, 2003-07-01
    ((and (constant-lvar-p first)
          (not (constant-lvar-p second)))
     `(,inverse y x))
    (t (give-up-ir1-transform))))

(deftransform char< ((x y) (character character) *)
  (ir1-transform-char< x y x y 'char>))

(deftransform char> ((x y) (character character) *)
  (ir1-transform-char< y x x y 'char<))

;;;; converting N-arg comparisons
;;;;
;;;; We convert calls to N-arg comparison functions such as < into
;;;; two-arg calls. This transformation is enabled for all such
;;;; comparisons in this file. If any of these predicates are not
;;;; open-coded, then the transformation should be removed at some
;;;; point to avoid pessimization.

;;; This function is used for source transformation of N-arg
;;; comparison functions other than inequality. We deal both with
;;; converting to two-arg calls and inverting the sense of the test,
;;; if necessary. If the call has two args, then we pass or return a
;;; negated test as appropriate. If it is a degenerate one-arg call,
;;; then we transform to code that returns true. Otherwise, we bind
;;; all the arguments and expand into a bunch of IFs.
(defun multi-compare (predicate args not-p type &optional force-two-arg-p)
  (let ((nargs (length args)))
    (cond ((< nargs 1) (values nil t))
          ((= nargs 1) `(progn (the ,type ,@args) t))
          ((= nargs 2)
           (if not-p
               `(if (,predicate ,(first args) ,(second args)) nil t)
               (if force-two-arg-p
                   `(,predicate ,(first args) ,(second args))
                   (values nil t))))
          (t
           (do* ((i (1- nargs) (1- i))
                 (last nil current)
                 (current (gensym) (gensym))
                 (vars (list current) (cons current vars))
                 (result t (if not-p
                               `(if (,predicate ,current ,last)
                                    nil ,result)
                               `(if (,predicate ,current ,last)
                                    ,result nil))))
               ((zerop i)
                `((lambda ,vars (declare (type ,type ,@vars)) ,result)
                  ,@args)))))))

(define-source-transform = (&rest args) (multi-compare '= args nil 'number))
(define-source-transform < (&rest args) (multi-compare '< args nil 'real))
(define-source-transform > (&rest args) (multi-compare '> args nil 'real))
;;; We cannot do the inversion for >= and <= here, since both
;;;   (< NaN X) and (> NaN X)
;;; are false, and we don't have type-inforation available yet. The
;;; deftransforms for two-argument versions of >= and <= takes care of
;;; the inversion to > and < when possible.
(define-source-transform <= (&rest args) (multi-compare '<= args nil 'real))
(define-source-transform >= (&rest args) (multi-compare '>= args nil 'real))

(define-source-transform char= (&rest args) (multi-compare 'char= args nil
                                                           'character))
(define-source-transform char< (&rest args) (multi-compare 'char< args nil
                                                           'character))
(define-source-transform char> (&rest args) (multi-compare 'char> args nil
                                                           'character))
(define-source-transform char<= (&rest args) (multi-compare 'char> args t
                                                            'character))
(define-source-transform char>= (&rest args) (multi-compare 'char< args t
                                                            'character))

(define-source-transform char-equal (&rest args)
  (multi-compare 'sb!impl::two-arg-char-equal args nil 'character t))
(define-source-transform char-lessp (&rest args)
  (multi-compare 'sb!impl::two-arg-char-lessp args nil 'character t))
(define-source-transform char-greaterp (&rest args)
  (multi-compare 'sb!impl::two-arg-char-greaterp args nil 'character t))
(define-source-transform char-not-greaterp (&rest args)
  (multi-compare 'sb!impl::two-arg-char-greaterp args t 'character t))
(define-source-transform char-not-lessp (&rest args)
  (multi-compare 'sb!impl::two-arg-char-lessp args t 'character t))

;;; This function does source transformation of N-arg inequality
;;; functions such as /=. This is similar to MULTI-COMPARE in the <3
;;; arg cases. If there are more than two args, then we expand into
;;; the appropriate n^2 comparisons only when speed is important.
(declaim (ftype (function (symbol list t) *) multi-not-equal))
(defun multi-not-equal (predicate args type)
  (let ((nargs (length args)))
    (cond ((< nargs 1) (values nil t))
          ((= nargs 1) `(progn (the ,type ,@args) t))
          ((= nargs 2)
           `(if (,predicate ,(first args) ,(second args)) nil t))
          ((not (policy *lexenv*
                        (and (>= speed space)
                             (>= speed compilation-speed))))
           (values nil t))
          (t
           (let ((vars (make-gensym-list nargs)))
             (do ((var vars next)
                  (next (cdr vars) (cdr next))
                  (result t))
                 ((null next)
                  `((lambda ,vars (declare (type ,type ,@vars)) ,result)
                    ,@args))
               (let ((v1 (first var)))
                 (dolist (v2 next)
                   (setq result `(if (,predicate ,v1 ,v2) nil ,result))))))))))

(define-source-transform /= (&rest args)
  (multi-not-equal '= args 'number))
(define-source-transform char/= (&rest args)
  (multi-not-equal 'char= args 'character))
(define-source-transform char-not-equal (&rest args)
  (multi-not-equal 'char-equal args 'character))

;;; Expand MAX and MIN into the obvious comparisons.
(define-source-transform max (arg0 &rest rest)
  (once-only ((arg0 arg0))
    (if (null rest)
        `(values (the real ,arg0))
        `(let ((maxrest (max ,@rest)))
          (if (>= ,arg0 maxrest) ,arg0 maxrest)))))
(define-source-transform min (arg0 &rest rest)
  (once-only ((arg0 arg0))
    (if (null rest)
        `(values (the real ,arg0))
        `(let ((minrest (min ,@rest)))
          (if (<= ,arg0 minrest) ,arg0 minrest)))))

;;;; converting N-arg arithmetic functions
;;;;
;;;; N-arg arithmetic and logic functions are associated into two-arg
;;;; versions, and degenerate cases are flushed.

;;; Left-associate FIRST-ARG and MORE-ARGS using FUNCTION.
(declaim (ftype (function (symbol t list) list) associate-args))
(defun associate-args (function first-arg more-args)
  (let ((next (rest more-args))
        (arg (first more-args)))
    (if (null next)
        `(,function ,first-arg ,arg)
        (associate-args function `(,function ,first-arg ,arg) next))))

;;; Do source transformations for transitive functions such as +.
;;; One-arg cases are replaced with the arg and zero arg cases with
;;; the identity.  ONE-ARG-RESULT-TYPE is, if non-NIL, the type to
;;; ensure (with THE) that the argument in one-argument calls is.
(defun source-transform-transitive (fun args identity
                                    &optional one-arg-result-type)
  (declare (symbol fun) (list args))
  (case (length args)
    (0 identity)
    (1 (if one-arg-result-type
           `(values (the ,one-arg-result-type ,(first args)))
           `(values ,(first args))))
    (2 (values nil t))
    (t
     (associate-args fun (first args) (rest args)))))

(define-source-transform + (&rest args)
  (source-transform-transitive '+ args 0 'number))
(define-source-transform * (&rest args)
  (source-transform-transitive '* args 1 'number))
(define-source-transform logior (&rest args)
  (source-transform-transitive 'logior args 0 'integer))
(define-source-transform logxor (&rest args)
  (source-transform-transitive 'logxor args 0 'integer))
(define-source-transform logand (&rest args)
  (source-transform-transitive 'logand args -1 'integer))
(define-source-transform logeqv (&rest args)
  (source-transform-transitive 'logeqv args -1 'integer))

;;; Note: we can't use SOURCE-TRANSFORM-TRANSITIVE for GCD and LCM
;;; because when they are given one argument, they return its absolute
;;; value.

(define-source-transform gcd (&rest args)
  (case (length args)
    (0 0)
    (1 `(abs (the integer ,(first args))))
    (2 (values nil t))
    (t (associate-args 'gcd (first args) (rest args)))))

(define-source-transform lcm (&rest args)
  (case (length args)
    (0 1)
    (1 `(abs (the integer ,(first args))))
    (2 (values nil t))
    (t (associate-args 'lcm (first args) (rest args)))))

;;; Do source transformations for intransitive n-arg functions such as
;;; /. With one arg, we form the inverse. With two args we pass.
;;; Otherwise we associate into two-arg calls.
(declaim (ftype (function (symbol list t)
                          (values list &optional (member nil t)))
                source-transform-intransitive))
(defun source-transform-intransitive (function args inverse)
  (case (length args)
    ((0 2) (values nil t))
    (1 `(,@inverse ,(first args)))
    (t (associate-args function (first args) (rest args)))))

(define-source-transform - (&rest args)
  (source-transform-intransitive '- args '(%negate)))
(define-source-transform / (&rest args)
  (source-transform-intransitive '/ args '(/ 1)))

;;;; transforming APPLY

;;; We convert APPLY into MULTIPLE-VALUE-CALL so that the compiler
;;; only needs to understand one kind of variable-argument call. It is
;;; more efficient to convert APPLY to MV-CALL than MV-CALL to APPLY.
(define-source-transform apply (fun arg &rest more-args)
  (let ((args (cons arg more-args)))
    `(multiple-value-call ,fun
       ,@(mapcar (lambda (x)
                   `(values ,x))
                 (butlast args))
       (values-list ,(car (last args))))))

;;;; transforming FORMAT
;;;;
;;;; If the control string is a compile-time constant, then replace it
;;;; with a use of the FORMATTER macro so that the control string is
;;;; ``compiled.'' Furthermore, if the destination is either a stream
;;;; or T and the control string is a function (i.e. FORMATTER), then
;;;; convert the call to FORMAT to just a FUNCALL of that function.

;;; for compile-time argument count checking.
;;;
;;; FIXME II: In some cases, type information could be correlated; for
;;; instance, ~{ ... ~} requires a list argument, so if the lvar-type
;;; of a corresponding argument is known and does not intersect the
;;; list type, a warning could be signalled.
(defun check-format-args (string args fun)
  (declare (type string string))
  (unless (typep string 'simple-string)
    (setq string (coerce string 'simple-string)))
  (multiple-value-bind (min max)
      (handler-case (sb!format:%compiler-walk-format-string string args)
        (sb!format:format-error (c)
          (compiler-warn "~A" c)))
    (when min
      (let ((nargs (length args)))
        (cond
          ((< nargs min)
           (warn 'format-too-few-args-warning
                 :format-control
                 "Too few arguments (~D) to ~S ~S: requires at least ~D."
                 :format-arguments (list nargs fun string min)))
          ((> nargs max)
           (warn 'format-too-many-args-warning
                 :format-control
                 "Too many arguments (~D) to ~S ~S: uses at most ~D."
                 :format-arguments (list nargs fun string max))))))))

(defoptimizer (format optimizer) ((dest control &rest args))
  (when (constant-lvar-p control)
    (let ((x (lvar-value control)))
      (when (stringp x)
        (check-format-args x args 'format)))))

;;; We disable this transform in the cross-compiler to save memory in
;;; the target image; most of the uses of FORMAT in the compiler are for
;;; error messages, and those don't need to be particularly fast.
#+sb-xc
(deftransform format ((dest control &rest args) (t simple-string &rest t) *
                      :policy (>= speed space))
  (unless (constant-lvar-p control)
    (give-up-ir1-transform "The control string is not a constant."))
  (let ((arg-names (make-gensym-list (length args))))
    `(lambda (dest control ,@arg-names)
       (declare (ignore control))
       (format dest (formatter ,(lvar-value control)) ,@arg-names))))

(deftransform format ((stream control &rest args) (stream function &rest t))
  (let ((arg-names (make-gensym-list (length args))))
    `(lambda (stream control ,@arg-names)
       (funcall control stream ,@arg-names)
       nil)))

(deftransform format ((tee control &rest args) ((member t) function &rest t))
  (let ((arg-names (make-gensym-list (length args))))
    `(lambda (tee control ,@arg-names)
       (declare (ignore tee))
       (funcall control *standard-output* ,@arg-names)
       nil)))

(deftransform pathname ((pathspec) (pathname) *)
  'pathspec)

(deftransform pathname ((pathspec) (string) *)
  '(values (parse-namestring pathspec)))

(macrolet
    ((def (name)
         `(defoptimizer (,name optimizer) ((control &rest args))
            (when (constant-lvar-p control)
              (let ((x (lvar-value control)))
                (when (stringp x)
                  (check-format-args x args ',name)))))))
  (def error)
  (def warn)
  #+sb-xc-host ; Only we should be using these
  (progn
    (def style-warn)
    (def compiler-error)
    (def compiler-warn)
    (def compiler-style-warn)
    (def compiler-notify)
    (def maybe-compiler-notify)
    (def bug)))

(defoptimizer (cerror optimizer) ((report control &rest args))
  (when (and (constant-lvar-p control)
             (constant-lvar-p report))
    (let ((x (lvar-value control))
          (y (lvar-value report)))
      (when (and (stringp x) (stringp y))
        (multiple-value-bind (min1 max1)
            (handler-case
                (sb!format:%compiler-walk-format-string x args)
              (sb!format:format-error (c)
                (compiler-warn "~A" c)))
          (when min1
            (multiple-value-bind (min2 max2)
                (handler-case
                    (sb!format:%compiler-walk-format-string y args)
                  (sb!format:format-error (c)
                    (compiler-warn "~A" c)))
              (when min2
                (let ((nargs (length args)))
                  (cond
                    ((< nargs (min min1 min2))
                     (warn 'format-too-few-args-warning
                           :format-control
                           "Too few arguments (~D) to ~S ~S ~S: ~
                            requires at least ~D."
                           :format-arguments
                           (list nargs 'cerror y x (min min1 min2))))
                    ((> nargs (max max1 max2))
                     (warn 'format-too-many-args-warning
                           :format-control
                           "Too many arguments (~D) to ~S ~S ~S: ~
                            uses at most ~D."
                           :format-arguments
                           (list nargs 'cerror y x (max max1 max2))))))))))))))

(defoptimizer (coerce derive-type) ((value type) node)
  (cond
    ((constant-lvar-p type)
     ;; This branch is essentially (RESULT-TYPE-SPECIFIER-NTH-ARG 2),
     ;; but dealing with the niggle that complex canonicalization gets
     ;; in the way: (COERCE 1 'COMPLEX) returns 1, which is not of
     ;; type COMPLEX.
     (let* ((specifier (lvar-value type))
            (result-typeoid (careful-specifier-type specifier)))
       (cond
         ((null result-typeoid) nil)
         ((csubtypep result-typeoid (specifier-type 'number))
          ;; the difficult case: we have to cope with ANSI 12.1.5.3
          ;; Rule of Canonical Representation for Complex Rationals,
          ;; which is a truly nasty delivery to field.
          (cond
            ((csubtypep result-typeoid (specifier-type 'real))
             ;; cleverness required here: it would be nice to deduce
             ;; that something of type (INTEGER 2 3) coerced to type
             ;; DOUBLE-FLOAT should return (DOUBLE-FLOAT 2.0d0 3.0d0).
             ;; FLOAT gets its own clause because it's implemented as
             ;; a UNION-TYPE, so we don't catch it in the NUMERIC-TYPE
             ;; logic below.
             result-typeoid)
            ((and (numeric-type-p result-typeoid)
                  (eq (numeric-type-complexp result-typeoid) :real))
             ;; FIXME: is this clause (a) necessary or (b) useful?
             result-typeoid)
            ((or (csubtypep result-typeoid
                            (specifier-type '(complex single-float)))
                 (csubtypep result-typeoid
                            (specifier-type '(complex double-float)))
                 #!+long-float
                 (csubtypep result-typeoid
                            (specifier-type '(complex long-float))))
             ;; float complex types are never canonicalized.
             result-typeoid)
            (t
             ;; if it's not a REAL, or a COMPLEX FLOAToid, it's
             ;; probably just a COMPLEX or equivalent.  So, in that
             ;; case, we will return a complex or an object of the
             ;; provided type if it's rational:
             (type-union result-typeoid
                         (type-intersection (lvar-type value)
                                            (specifier-type 'rational))))))
         ((and (policy node (zerop safety))
               (csubtypep result-typeoid (specifier-type '(array * (*)))))
          ;; At zero safety the deftransform for COERCE can elide dimension
          ;; checks for the things like (COERCE X '(SIMPLE-VECTOR 5)) -- so we
          ;; need to simplify the type to drop the dimension information.
          (let ((vtype (simplify-vector-type result-typeoid)))
            (if vtype
                (specifier-type vtype)
                result-typeoid)))
         (t
          result-typeoid))))
    (t
     ;; OK, the result-type argument isn't constant.  However, there
     ;; are common uses where we can still do better than just
     ;; *UNIVERSAL-TYPE*: e.g. (COERCE X (ARRAY-ELEMENT-TYPE Y)),
     ;; where Y is of a known type.  See messages on cmucl-imp
     ;; 2001-02-14 and sbcl-devel 2002-12-12.  We only worry here
     ;; about types that can be returned by (ARRAY-ELEMENT-TYPE Y), on
     ;; the basis that it's unlikely that other uses are both
     ;; time-critical and get to this branch of the COND (non-constant
     ;; second argument to COERCE).  -- CSR, 2002-12-16
     (let ((value-type (lvar-type value))
           (type-type (lvar-type type)))
       (labels
           ((good-cons-type-p (cons-type)
              ;; Make sure the cons-type we're looking at is something
              ;; we're prepared to handle which is basically something
              ;; that array-element-type can return.
              (or (and (member-type-p cons-type)
                       (eql 1 (member-type-size cons-type))
                       (null (first (member-type-members cons-type))))
                  (let ((car-type (cons-type-car-type cons-type)))
                    (and (member-type-p car-type)
                         (eql 1 (member-type-members car-type))
                         (let ((elt (first (member-type-members car-type))))
                           (or (symbolp elt)
                               (numberp elt)
                               (and (listp elt)
                                    (numberp (first elt)))))
                         (good-cons-type-p (cons-type-cdr-type cons-type))))))
            (unconsify-type (good-cons-type)
              ;; Convert the "printed" respresentation of a cons
              ;; specifier into a type specifier.  That is, the
              ;; specifier (CONS (EQL SIGNED-BYTE) (CONS (EQL 16)
              ;; NULL)) is converted to (SIGNED-BYTE 16).
              (cond ((or (null good-cons-type)
                         (eq good-cons-type 'null))
                     nil)
                    ((and (eq (first good-cons-type) 'cons)
                          (eq (first (second good-cons-type)) 'member))
                     `(,(second (second good-cons-type))
                       ,@(unconsify-type (caddr good-cons-type))))))
            (coerceable-p (part)
              ;; Can the value be coerced to the given type?  Coerce is
              ;; complicated, so we don't handle every possible case
              ;; here---just the most common and easiest cases:
              ;;
              ;; * Any REAL can be coerced to a FLOAT type.
              ;; * Any NUMBER can be coerced to a (COMPLEX
              ;;   SINGLE/DOUBLE-FLOAT).
              ;;
              ;; FIXME I: we should also be able to deal with characters
              ;; here.
              ;;
              ;; FIXME II: I'm not sure that anything is necessary
              ;; here, at least while COMPLEX is not a specialized
              ;; array element type in the system.  Reasoning: if
              ;; something cannot be coerced to the requested type, an
              ;; error will be raised (and so any downstream compiled
              ;; code on the assumption of the returned type is
              ;; unreachable).  If something can, then it will be of
              ;; the requested type, because (by assumption) COMPLEX
              ;; (and other difficult types like (COMPLEX INTEGER)
              ;; aren't specialized types.
              (let ((coerced-type (careful-specifier-type part)))
                (when coerced-type
                  (or (and (csubtypep coerced-type (specifier-type 'float))
                           (csubtypep value-type (specifier-type 'real)))
                      (and (csubtypep coerced-type
                                      (specifier-type `(or (complex single-float)
                                                           (complex double-float))))
                          (csubtypep value-type (specifier-type 'number)))))))
            (process-types (type)
              ;; FIXME: This needs some work because we should be able
              ;; to derive the resulting type better than just the
              ;; type arg of coerce.  That is, if X is (INTEGER 10
              ;; 20), then (COERCE X 'DOUBLE-FLOAT) should say
              ;; (DOUBLE-FLOAT 10d0 20d0) instead of just
              ;; double-float.
              (cond ((member-type-p type)
                     (block punt
                       (let (members)
                         (mapc-member-type-members
                          (lambda (member)
                            (if (coerceable-p member)
                                (push member members)
                                (return-from punt *universal-type*)))
                          type)
                         (specifier-type `(or ,@members)))))
                    ((and (cons-type-p type)
                          (good-cons-type-p type))
                     (let ((c-type (unconsify-type (type-specifier type))))
                       (if (coerceable-p c-type)
                           (specifier-type c-type)
                           *universal-type*)))
                    (t
                     *universal-type*))))
         (cond ((union-type-p type-type)
                (apply #'type-union (mapcar #'process-types
                                            (union-type-types type-type))))
               ((or (member-type-p type-type)
                    (cons-type-p type-type))
                (process-types type-type))
               (t
                *universal-type*)))))))

(defoptimizer (compile derive-type) ((nameoid function))
  (when (csubtypep (lvar-type nameoid)
                   (specifier-type 'null))
    (values-specifier-type '(values function boolean boolean))))

;;; FIXME: Maybe also STREAM-ELEMENT-TYPE should be given some loving
;;; treatment along these lines? (See discussion in COERCE DERIVE-TYPE
;;; optimizer, above).
(defoptimizer (array-element-type derive-type) ((array))
  (let ((array-type (lvar-type array)))
    (labels ((consify (list)
              (if (endp list)
                  '(eql nil)
                  `(cons (eql ,(car list)) ,(consify (rest list)))))
            (get-element-type (a)
              (let ((element-type
                     (type-specifier (array-type-specialized-element-type a))))
                (cond ((eq element-type '*)
                       (specifier-type 'type-specifier))
                      ((symbolp element-type)
                       (make-member-type :members (list element-type)))
                      ((consp element-type)
                       (specifier-type (consify element-type)))
                      (t
                       (error "can't understand type ~S~%" element-type))))))
      (labels ((recurse (type)
                  (cond ((array-type-p type)
                         (get-element-type type))
                        ((union-type-p type)
                         (apply #'type-union
                                (mapcar #'recurse (union-type-types type))))
                        (t
                         *universal-type*))))
        (recurse array-type)))))

(define-source-transform sb!impl::sort-vector (vector start end predicate key)
  ;; Like CMU CL, we use HEAPSORT. However, other than that, this code
  ;; isn't really related to the CMU CL code, since instead of trying
  ;; to generalize the CMU CL code to allow START and END values, this
  ;; code has been written from scratch following Chapter 7 of
  ;; _Introduction to Algorithms_ by Corman, Rivest, and Shamir.
  `(macrolet ((%index (x) `(truly-the index ,x))
              (%parent (i) `(ash ,i -1))
              (%left (i) `(%index (ash ,i 1)))
              (%right (i) `(%index (1+ (ash ,i 1))))
              (%heapify (i)
               `(do* ((i ,i)
                      (left (%left i) (%left i)))
                 ((> left current-heap-size))
                 (declare (type index i left))
                 (let* ((i-elt (%elt i))
                        (i-key (funcall keyfun i-elt))
                        (left-elt (%elt left))
                        (left-key (funcall keyfun left-elt)))
                   (multiple-value-bind (large large-elt large-key)
                       (if (funcall ,',predicate i-key left-key)
                           (values left left-elt left-key)
                           (values i i-elt i-key))
                     (let ((right (%right i)))
                       (multiple-value-bind (largest largest-elt)
                           (if (> right current-heap-size)
                               (values large large-elt)
                               (let* ((right-elt (%elt right))
                                      (right-key (funcall keyfun right-elt)))
                                 (if (funcall ,',predicate large-key right-key)
                                     (values right right-elt)
                                     (values large large-elt))))
                         (cond ((= largest i)
                                (return))
                               (t
                                (setf (%elt i) largest-elt
                                      (%elt largest) i-elt
                                      i largest)))))))))
              (%sort-vector (keyfun &optional (vtype 'vector))
               `(macrolet (;; KLUDGE: In SBCL ca. 0.6.10, I had
                           ;; trouble getting type inference to
                           ;; propagate all the way through this
                           ;; tangled mess of inlining. The TRULY-THE
                           ;; here works around that. -- WHN
                           (%elt (i)
                            `(aref (truly-the ,',vtype ,',',vector)
                              (%index (+ (%index ,i) start-1)))))
                 (let (;; Heaps prefer 1-based addressing.
                       (start-1 (1- ,',start))
                       (current-heap-size (- ,',end ,',start))
                       (keyfun ,keyfun))
                   (declare (type (integer -1 #.(1- sb!xc:most-positive-fixnum))
                                  start-1))
                   (declare (type index current-heap-size))
                   (declare (type function keyfun))
                   (loop for i of-type index
                         from (ash current-heap-size -1) downto 1 do
                         (%heapify i))
                   (loop
                    (when (< current-heap-size 2)
                      (return))
                    (rotatef (%elt 1) (%elt current-heap-size))
                    (decf current-heap-size)
                    (%heapify 1))))))
    (if (typep ,vector 'simple-vector)
        ;; (VECTOR T) is worth optimizing for, and SIMPLE-VECTOR is
        ;; what we get from (VECTOR T) inside WITH-ARRAY-DATA.
        (if (null ,key)
            ;; Special-casing the KEY=NIL case lets us avoid some
            ;; function calls.
            (%sort-vector #'identity simple-vector)
            (%sort-vector ,key simple-vector))
        ;; It's hard to anticipate many speed-critical applications for
        ;; sorting vector types other than (VECTOR T), so we just lump
        ;; them all together in one slow dynamically typed mess.
        (locally
          (declare (optimize (speed 2) (space 2) (inhibit-warnings 3)))
          (%sort-vector (or ,key #'identity))))))

;;;; debuggers' little helpers

;;; for debugging when transforms are behaving mysteriously,
;;; e.g. when debugging a problem with an ASH transform
;;;   (defun foo (&optional s)
;;;     (sb-c::/report-lvar s "S outside WHEN")
;;;     (when (and (integerp s) (> s 3))
;;;       (sb-c::/report-lvar s "S inside WHEN")
;;;       (let ((bound (ash 1 (1- s))))
;;;         (sb-c::/report-lvar bound "BOUND")
;;;         (let ((x (- bound))
;;;               (y (1- bound)))
;;;           (sb-c::/report-lvar x "X")
;;;           (sb-c::/report-lvar x "Y"))
;;;         `(integer ,(- bound) ,(1- bound)))))
;;; (The DEFTRANSFORM doesn't do anything but report at compile time,
;;; and the function doesn't do anything at all.)
#!+sb-show
(progn
  (defknown /report-lvar (t t) null)
  (deftransform /report-lvar ((x message) (t t))
    (format t "~%/in /REPORT-LVAR~%")
    (format t "/(LVAR-TYPE X)=~S~%" (lvar-type x))
    (when (constant-lvar-p x)
      (format t "/(LVAR-VALUE X)=~S~%" (lvar-value x)))
    (format t "/MESSAGE=~S~%" (lvar-value message))
    (give-up-ir1-transform "not a real transform"))
  (defun /report-lvar (x message)
    (declare (ignore x message))))


;;;; Transforms for internal compiler utilities

;;; If QUALITY-NAME is constant and a valid name, don't bother
;;; checking that it's still valid at run-time.
(deftransform policy-quality ((policy quality-name)
                              (t symbol))
  (unless (and (constant-lvar-p quality-name)
               (policy-quality-name-p (lvar-value quality-name)))
    (give-up-ir1-transform))
  '(%policy-quality policy quality-name))
