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

;;; We turn IDENTITY into PROG1 so that it is obvious that it just
;;; returns the first value of its argument. Ditto for VALUES with one
;;; arg.
(define-source-transform identity (x) `(prog1 ,x))
(define-source-transform values (x) `(prog1 ,x))

;;; CONSTANTLY is pretty much never worth transforming, but it's good to get the type.
(defoptimizer (constantly derive-type) ((value))
  (specifier-type
   `(function (&rest t) (values ,(type-specifier (lvar-type value)) &optional))))

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

;;;; SYMBOL-VALUE &co
(defun derive-symbol-value-type (lvar node)
  (if (constant-lvar-p lvar)
      (let* ((sym (lvar-value lvar))
             (var (maybe-find-free-var sym))
             (local-type (when var
                           (let ((*lexenv* (node-lexenv node)))
                             (lexenv-find var type-restrictions))))
             (global-type (info :variable :type sym)))
        (if local-type
            (type-intersection local-type global-type)
            global-type))
      *universal-type*))

(defoptimizer (symbol-value derive-type) ((symbol) node)
  (derive-symbol-value-type symbol node))

(defoptimizer (symbol-global-value derive-type) ((symbol) node)
  (derive-symbol-value-type symbol node))

;;;; list hackery

;;; Translate CxR into CAR/CDR combos.
(defun source-transform-cxr (form env)
  (declare (ignore env))
  (if (not (singleton-p (cdr form)))
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
;;; Don't transform CAD*R, they are treated specially for &more args
;;; optimizations

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
          (unless (member buf '("CADR" "CADDR" "CADDDR")
                          :test #'equal)
            (setf (info :function :source-transform (intern buf))
                  #'source-transform-cxr)))))
(/show0 "done setting CxR source transforms")

;;; Turn FIRST..FOURTH and REST into the obvious synonym, assuming
;;; whatever is right for them is right for us. FIFTH..TENTH turn into
;;; Nth, which can be expanded into a CAR/CDR later on if policy
;;; favors it.
(define-source-transform rest (x) `(cdr ,x))
(define-source-transform first (x) `(car ,x))
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

(defoptimizer (list derive-type) ((&rest args))
  (if args
      (specifier-type 'cons)
      (specifier-type 'null)))

;;; And similarly for LIST*.
(define-source-transform list* (arg &rest others)
  (cond ((not others) arg)
        ((not (cdr others)) `(cons ,arg ,(car others)))
        (t (values nil t))))

(defoptimizer (list* derive-type) ((arg &rest args))
  (if args
      (specifier-type 'cons)
      (lvar-type arg)))

(define-source-transform make-list (length &rest rest)
  (if (or (null rest)
          ;; Use of &KEY in source xforms doesn't have all the usual semantics.
          ;; It's better to hand-roll it - cf. transforms for WRITE[-TO-STRING].
          (typep rest '(cons (eql :initial-element) (cons t null))))
      ;; Something fishy here- If THE is removed, OPERAND-RESTRICTION-OK
      ;; returns NIL because type inference on MAKE-LIST never happens.
      ;; But the fndb entry for %MAKE-LIST is right, so I'm slightly bewildered.
      `(%make-list (the (integer 0 (,(1- sb!xc:array-dimension-limit))) ,length)
                   ,(second rest))
      (values nil t))) ; give up

(deftransform %make-list ((length item) ((constant-arg (eql 0)) t)) nil)

(define-source-transform nconc (&rest args)
  (case (length args)
    (0 ())
    (1 (car args))
    (t (values nil t))))

;;; (append nil nil nil fixnum) => fixnum
;;; (append x x cons x x) => cons
;;; (append x x x x list) => list
;;; (append x x x x sequence) => sequence
;;; (append fixnum x ...) => nil
(defun derive-append-type (args)
  (when (null args)
    (return-from derive-append-type (specifier-type 'null)))
  (let* ((cons-type (specifier-type 'cons))
         (null-type (specifier-type 'null))
         (list-type (specifier-type 'list))
         (last (lvar-type (car (last args)))))
    ;; Derive the actual return type, assuming that all but the last
    ;; arguments are LISTs (otherwise, APPEND/NCONC doesn't return).
    (loop with all-nil = t       ; all but the last args are NIL?
          with some-cons = nil   ; some args are conses?
          for (arg next) on args
          for lvar-type = (type-approx-intersection2 (lvar-type arg)
                                                     list-type)
          while next
          do (multiple-value-bind (typep definitely)
                 (ctypep nil lvar-type)
               (cond ((type= lvar-type *empty-type*)
                      ;; type mismatch! insert an inline check that'll cause
                      ;; compile-time warnings.
                      (assert-lvar-type arg list-type
                                        (lexenv-policy *lexenv*)))
                     (some-cons) ; we know result's a cons -- nothing to do
                     ((and (not typep) definitely) ; can't be NIL
                      (setf some-cons t))          ; must be a CONS
                     (all-nil
                      (setf all-nil (csubtypep lvar-type null-type)))))
          finally
             ;; if some of the previous arguments are CONSes so is the result;
             ;; if all the previous values are NIL, we're a fancy identity;
             ;; otherwise, could be either
             (return (cond (some-cons cons-type)
                           (all-nil last)
                           (t (type-union last cons-type)))))))

(defoptimizer (append derive-type) ((&rest args))
  (derive-append-type args))

(defoptimizer (sb!impl::append2 derive-type) ((&rest args))
  (derive-append-type args))

(defoptimizer (nconc derive-type) ((&rest args))
  (derive-append-type args))

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
   (2 `(sb!impl::get3 ,@args nil))
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
;;;
;;; Other transforms may be useful even with direct LOGTEST VOPs; let
;;; them fire (including the type-directed constant folding below), but
;;; disable the inlining rewrite in such cases. -- PK, 2013-05-20
(deftransform logtest ((x y) * * :node node)
  (let ((type (two-arg-derive-type x y
                                   #'logand-derive-type-aux
                                   #'logand)))
    (multiple-value-bind (typep definitely)
        (ctypep 0 type)
      (cond ((and (not typep) definitely)
             t)
            ((type= type (specifier-type '(eql 0)))
             nil)
            ((neq :default (combination-implementation-style node))
             (give-up-ir1-transform))
            (t
             `(not (zerop (logand x y))))))))

(deftransform logbitp
    ((index integer) (unsigned-byte (or (signed-byte #.sb!vm:n-word-bits)
                                        (unsigned-byte #.sb!vm:n-word-bits))))
  (flet ((general-case ()
           `(if (>= index #.sb!vm:n-word-bits)
                (minusp integer)
                (not (zerop (logand integer (ash 1 index)))))))
    (if (constant-lvar-p integer)
        (let ((val (lvar-value integer)))
          (cond ((eql val 0) nil)
                ((eql val -1) t)
                (t (general-case))))
        (general-case))))

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
    (%make-interval (normalize-bound low)
                    (normalize-bound high))))

;;; Apply the function F to a bound X. If X is an open bound and the
;;; function is declared strictly monotonic, then the result will be
;;; open. IF X is NIL, the result is NIL.
(defun bound-func (f x strict)
  (declare (type function f))
  (and x
       (handler-case
         (with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero)
           ;; With these traps masked, we might get things like infinity
           ;; or negative infinity returned. Check for this and return
           ;; NIL to indicate unbounded.
           (let ((y (funcall f (type-bound-number x))))
             (if (and (floatp y)
                      (float-infinity-p y))
                 nil
                 (set-bound y (and strict (consp x))))))
         ;; Some numerical operations will signal SIMPLE-TYPE-ERROR, e.g.
         ;; in the course of converting a bignum to a float.  Default to
         ;; NIL in that case.
         (simple-type-error ()))))

(defun safe-double-coercion-p (x)
  (or (typep x 'double-float)
      (<= most-negative-double-float x most-positive-double-float)))

(defun safe-single-coercion-p (x)
  (or (typep x 'single-float)
      (and
       ;; Fix for bug 420, and related issues: during type derivation we often
       ;; end up deriving types for both
       ;;
       ;;   (some-op <int> <single>)
       ;; and
       ;;   (some-op (coerce <int> 'single-float) <single>)
       ;;
       ;; or other equivalent transformed forms. The problem with this
       ;; is that on x86 (+ <int> <single>) is on the machine level
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
       ;;
       ;; See also: SAFE-CTYPE-FOR-SINGLE-COERCION-P
       ;;
       ;; FIXME: If we ever add SSE-support for x86, this conditional needs to
       ;; change.
       #!+x86
       (not (typep x `(or (integer * (,most-negative-exactly-single-float-fixnum))
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
  (with-unique-names (xb yb res)
    `(and ,x ,y
          (with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero)
            (let* ((,xb (type-bound-number ,x))
                   (,yb (type-bound-number ,y))
                   (,res (safely-binop ,op ,xb ,yb)))
              (set-bound ,res
                         (and (or (consp ,x) (consp ,y))
                              ;; Open bounds can very easily be messed up
                              ;; by FP rounding, so take care here.
                              ,(case op
                                 (*
                                  ;; Multiplying a greater-than-zero with
                                  ;; less than one can round to zero.
                                  `(or (not (fp-zero-p ,res))
                                       (cond ((and (consp ,x) (fp-zero-p ,xb))
                                              (>= (abs ,yb) 1))
                                             ((and (consp ,y) (fp-zero-p ,yb))
                                              (>= (abs ,xb) 1)))))
                                 (/
                                  ;; Dividing a greater-than-zero with
                                  ;; greater than one can round to zero.
                                  `(or (not (fp-zero-p ,res))
                                       (cond ((and (consp ,x) (fp-zero-p ,xb))
                                              (<= (abs ,yb) 1))
                                             ((and (consp ,y) (fp-zero-p ,yb))
                                              (<= (abs ,xb) 1)))))
                                 ((+ -)
                                  ;; Adding or subtracting greater-than-zero
                                  ;; can end up with identity.
                                  `(and (not (fp-zero-p ,xb))
                                        (not (fp-zero-p ,yb))))))))))))

(defun coercion-loses-precision-p (val type)
  (typecase val
    (single-float)
    (double-float (subtypep type 'single-float))
    (rational (subtypep type 'float))
    (t (bug "Unexpected arguments to bounds coercion: ~S ~S" val type))))

(defun coerce-for-bound (val type)
  (if (consp val)
      (let ((xbound (coerce-for-bound (car val) type)))
        (if (coercion-loses-precision-p (car val) type)
            xbound
            (list xbound)))
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
        (let ((xbound (coerce-for-bound (car val) type)))
          (if (coercion-loses-precision-p (car val) type)
              xbound
              (list xbound)))
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
;;; intervals at the point P. If CLOSE-LOWER is T, then the left
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
  (make-interval :low (bound-func #'- (interval-high x) t)
                 :high (bound-func #'- (interval-low x) t)))

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
                 (t
                  (bound-binop / x y)))))
    (let ((top-range (interval-range-info top))
          (bot-range (interval-range-info bot)))
      (cond ((null bot-range)
             ;; The denominator contains zero, so anything goes!
             (make-interval))
            ((eq bot-range '-)
             ;; Denominator is negative so flip the sign, compute the
             ;; result, and flip it back.
             (interval-neg (interval-div top (interval-neg bot))))
            ((null top-range)
             ;; Split top into two positive and negative parts, and
             ;; divide each separately
             (destructuring-bind (top- top+) (interval-split 0 top t t)
               (or (interval-merge-pair (interval-div top- bot)
                                        (interval-div top+ bot))
                   (make-interval))))
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
;;; result makes sense. It will if F is monotonic increasing (or, if
;;; the interval is closed, non-decreasing).
;;;
;;; (Actually most uses of INTERVAL-FUNC are coercions to float types,
;;; which are not monotonic increasing, so default to calling
;;; BOUND-FUNC with a non-strict argument).
(defun interval-func (f x &optional increasing)
  (declare (type function f)
           (type interval x))
  (let ((lo (bound-func f (interval-low x) increasing))
        (hi (bound-func f (interval-high x) increasing)))
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
  (interval-func (lambda (x) (* x x)) (interval-abs x)))

;;;; numeric DERIVE-TYPE methods

;;; a utility for defining derive-type methods of integer operations. If
;;; the types of both X and Y are integer types, then we compute a new
;;; integer type with bounds determined by FUN when applied to X and Y.
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
                 (push (if (numberp member) (make-eql-type member) *empty-type*)
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
              (list (make-eql-type (float 0.0 lo-val))
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
              (list (make-eql-type
                     (float (load-time-value
                             (make-unportable-float :single-float-negative-zero))
                            hi-val))
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

;;; Take a list of types and return a canonical type specifier,
;;; combining any MEMBER types together. If both positive and negative
;;; MEMBER types are present they are converted to a float type.
;;; XXX This would be far simpler if the type-union methods could handle
;;; member/number unions.
;;;
;;; If we're about to generate an overly complex union of numeric types, start
;;; collapse the ranges together.
;;;
;;; FIXME: The MEMBER canonicalization parts of MAKE-DERIVED-UNION-TYPE and
;;; entire CONVERT-MEMBER-TYPE probably belong in the kernel's type logic,
;;; invoked always, instead of in the compiler, invoked only during some type
;;; optimizations.
(defvar *derived-numeric-union-complexity-limit* 6)

(defun make-derived-union-type (type-list)
  (let ((xset (alloc-xset))
        (fp-zeroes '())
        (misc-types '())
        (numeric-type *empty-type*))
    (dolist (type type-list)
      (cond ((member-type-p type)
             (mapc-member-type-members
              (lambda (member)
                (if (fp-zero-p member)
                    (unless (member member fp-zeroes)
                      (pushnew member fp-zeroes))
                    (add-to-xset member xset)))
              type))
            ((numeric-type-p type)
             (let ((*approximate-numeric-unions*
                    (when (and (union-type-p numeric-type)
                               (nthcdr *derived-numeric-union-complexity-limit*
                                       (union-type-types numeric-type)))
                      t)))
               (setf numeric-type (type-union type numeric-type))))
            (t
             (push type misc-types))))
    (if (and (xset-empty-p xset) (not fp-zeroes))
        (apply #'type-union numeric-type misc-types)
        (apply #'type-union (make-member-type xset fp-zeroes)
               numeric-type misc-types))))

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
              (make-derived-union-type results)
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
              (make-derived-union-type results)
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
      ;; remainder if we know the sign of the number.
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
      ;; negative. Therefore, the remainder is unbounded, but we might
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
  (declare (ignore state))
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
  (declare (ignore state))
  (one-arg-derive-type bound #'random-derive-type-aux nil))

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

(defoptimizer (logcount derive-type) ((x))
  (let ((x-type (lvar-type x)))
    (when (numeric-type-p x-type)
      (let ((min (numeric-type-low x-type))
            (max (numeric-type-high x-type)))
        (when (and min max)
          (specifier-type
           `(integer ,(if (or (> min 0)
                              (< max -1))
                          1
                          0)
                     ,(max (integer-length min)
                           (integer-length max)))))))))

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
            `(mod ,sb!xc:char-code-limit))))))

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
;;;; The goal of the source-transform is to avoid consing a byte specifier
;;;; to immediately throw away. A more powerful framework could recognize
;;;; in IR1 when a constructor call flows to one or more accessors for the
;;;; constructed object and nowhere else (no mutators). If so, forwarding
;;;; the constructor arguments to their reads would generally solve this.
;;;; A transform approximates that, but fails when BYTE is produced by an
;;;; inline function and not a macro.
(flet ((xform (bytespec-form env int fun &optional (new nil setter-p))
         (let ((spec (%macroexpand bytespec-form env)))
           (if (and (consp spec) (eq (car spec) 'byte))
               (if (proper-list-of-length-p (cdr spec) 2)
                   (values `(,fun ,@(if setter-p (list new))
                                  ,(second spec) ,(third spec) ,int) nil)
                   ;; No point in compiling calls to BYTE-{SIZE,POSITION}
                   (values nil t)) ; T => "pass" (meaning "fail")
               (let ((new-temp (if setter-p (copy-symbol 'new)))
                     (byte (copy-symbol 'byte)))
                 (values `(let (,@(if new-temp `((,new-temp ,new)))
                                (,byte ,spec))
                            (,fun ,@(if setter-p (list new-temp))
                                  (byte-size ,byte) (byte-position ,byte) ,int))
                         nil))))))

  ;; DEFINE-SOURCE-TRANSFORM has no compile-time effect, so it's fine that
  ;; these 4 things are non-toplevel. (xform does not need to be a macro)
  (define-source-transform ldb (spec int &environment env)
    (xform spec env int '%ldb))

  (define-source-transform dpb (newbyte spec int &environment env)
    (xform spec env int '%dpb newbyte))

  (define-source-transform mask-field (spec int &environment env)
    (xform spec env int '%mask-field))

  (define-source-transform deposit-field (newbyte spec int &environment env)
    (xform spec env int '%deposit-field newbyte)))

(defoptimizer (%ldb derive-type) ((size posn num))
  (declare (ignore posn num))
  (let ((size (lvar-type size)))
    (if (and (numeric-type-p size)
             (csubtypep size (specifier-type 'integer)))
        (let ((size-high (numeric-type-high size)))
          (if (and size-high (<= size-high sb!vm:n-word-bits))
              (specifier-type `(unsigned-byte* ,size-high))
              (specifier-type 'unsigned-byte)))
        *universal-type*)))

(defoptimizer (%mask-field derive-type) ((size posn num))
  (declare (ignore num))
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
  (declare (ignore newbyte))
  (%deposit-field-derive-type-aux size posn int))

(defoptimizer (%deposit-field derive-type) ((newbyte size posn int))
  (declare (ignore newbyte))
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
  (declare (ignore x))
  (let ((size (lvar-type size)))
    (if (numeric-type-p size)
        (let ((size-high (numeric-type-high size)))
          (if (and size-high (<= 1 size-high sb!vm:n-word-bits))
              (specifier-type `(signed-byte ,size-high))
              *universal-type*))
        *universal-type*)))

;;; Rightward ASH
#!+ash-right-vops
(progn
  (defun %ash/right (integer amount)
    (ash integer (- amount)))

  (deftransform ash ((integer amount))
    "Convert ASH of signed word to %ASH/RIGHT"
    (unless (and (csubtypep (lvar-type integer) ; do that ourselves to avoid
                            (specifier-type 'sb!vm:signed-word)) ; optimization
                 (csubtypep (lvar-type amount)  ; notes.
                            (specifier-type '(integer * 0))))
      (give-up-ir1-transform))
    (when (constant-lvar-p amount)
      (give-up-ir1-transform))
    (let ((use (lvar-uses amount)))
      (cond ((and (combination-p use)
                  (eql '%negate (lvar-fun-name (combination-fun use))))
             (splice-fun-args amount '%negate 1)
             `(lambda (integer amount)
                (declare (type unsigned-byte amount))
                (%ash/right integer (if (>= amount ,sb!vm:n-word-bits)
                                        ,(1- sb!vm:n-word-bits)
                                        amount))))
            (t
             `(%ash/right integer (if (<= amount ,(- sb!vm:n-word-bits))
                                      ,(1- sb!vm:n-word-bits)
                                      (- amount)))))))

  (deftransform ash ((integer amount))
    "Convert ASH of word to %ASH/RIGHT"
    (unless (and (csubtypep (lvar-type integer)
                            (specifier-type 'sb!vm:word))
                 (csubtypep (lvar-type amount)
                            (specifier-type '(integer * 0))))
      (give-up-ir1-transform))
    (when (constant-lvar-p amount)
      (give-up-ir1-transform))
    (let ((use (lvar-uses amount)))
      (cond ((and (combination-p use)
                  (eql '%negate (lvar-fun-name (combination-fun use))))
             (splice-fun-args amount '%negate 1)
             `(lambda (integer amount)
                (declare (type unsigned-byte amount))
                (if (>= amount ,sb!vm:n-word-bits)
                    0
                    (%ash/right integer amount))))
            (t
             `(if (<= amount ,(- sb!vm:n-word-bits))
                  0
                  (%ash/right integer (- amount)))))))

  (deftransform %ash/right ((integer amount) (integer (constant-arg unsigned-byte)))
    "Convert %ASH/RIGHT by constant back to ASH"
    `(ash integer ,(- (lvar-value amount))))

  (deftransform %ash/right ((integer amount) * * :node node)
    "strength reduce large variable right shift"
    (let ((return-type (single-value-type (node-derived-type node))))
      (cond ((type= return-type (specifier-type '(eql 0)))
             0)
            ((type= return-type (specifier-type '(eql -1)))
             -1)
            ((csubtypep return-type (specifier-type '(member -1 0)))
             `(ash integer ,(- sb!vm:n-word-bits)))
            (t
             (give-up-ir1-transform)))))

  (defun %ash/right-derive-type-aux (n-type shift same-arg)
    (declare (ignore same-arg))
    (or (and (or (csubtypep n-type (specifier-type 'sb!vm:signed-word))
                 (csubtypep n-type (specifier-type 'word)))
             (csubtypep shift (specifier-type `(mod ,sb!vm:n-word-bits)))
             (let ((n-low (numeric-type-low n-type))
                   (n-high (numeric-type-high n-type))
                   (s-low (numeric-type-low shift))
                   (s-high (numeric-type-high shift)))
               (make-numeric-type :class 'integer :complexp :real
                                  :low (when n-low
                                         (if (minusp n-low)
                                             (ash n-low (- s-low))
                                             (ash n-low (- s-high))))
                                  :high (when n-high
                                          (if (minusp n-high)
                                              (ash n-high (- s-high))
                                              (ash n-high (- s-low)))))))
        *universal-type*))

  (defoptimizer (%ash/right derive-type) ((n shift))
    (two-arg-derive-type n shift #'%ash/right-derive-type-aux #'%ash/right))
  )

;;; Not declaring it as actually being RATIO becuase it is used as one
;;; of the legs in the EXPT transform below and that may result in
;;; some unwanted type conflicts, e.g. (random (expt 2 (the integer y)))
(declaim (type (sfunction (integer) rational) reciprocate))
(defun reciprocate (x)
  (declare (optimize (safety 0)))
  #+sb-xc-host (error "Can't call reciprocate ~D" x)
  #-sb-xc-host (%make-ratio 1 x))

(deftransform expt ((base power) ((constant-arg unsigned-byte) integer))
  (let ((base (lvar-value base)))
    (cond ((/= (logcount base) 1)
           (give-up-ir1-transform))
          ((= base 1)
           1)
          (t
           `(let ((%denominator (ash 1 ,(if (= base 2)
                                            `(abs power)
                                            `(* (abs power) ,(1- (integer-length base)))))))
              (if (minusp power)
                  (reciprocate %denominator)
                  %denominator))))))

(deftransform expt ((base power) ((constant-arg unsigned-byte) unsigned-byte))
  (let ((base (lvar-value base)))
    (unless (= (logcount base) 1)
      (give-up-ir1-transform))
    `(ash 1 ,(if (= base 2)
                 `power
                 `(* power ,(1- (integer-length base)))))))

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
                      (proclaimed-ftype name)))
               (setf (lvar-%derived-type (node-lvar node)) nil)
               (setf (node-reoptimize node) t)
               (setf (block-reoptimize (node-block node)) t)
               (reoptimize-component (node-component node) :maybe))
             (insert-lvar-cut (lvar)
               "Insert a LOGAND/MASK-SIGNED-FIELD to cut the value of LVAR
                to the required bit width. Returns T if any change was made.

                When the destination of LVAR will definitely cut LVAR's value
                to width (i.e. it's a logand or mask-signed-field with constant
                other argument), do nothing. Otherwise, splice LOGAND/M-S-F in."
               (binding* ((dest (lvar-dest lvar) :exit-if-null)
                          (nil  (combination-p dest) :exit-if-null)
                          (name (lvar-fun-name (combination-fun dest) t))
                          (args (combination-args dest)))
                 (case name
                   (logand
                    (when (= 2 (length args))
                      (let ((other (if (eql (first args) lvar)
                                       (second args)
                                       (first args))))
                        (when (and (constant-lvar-p other)
                                   (ctypep (lvar-value other) type)
                                   (not signedp))
                          (return-from insert-lvar-cut)))))
                   (mask-signed-field
                    (when (and signedp
                               (eql lvar (second args))
                               (constant-lvar-p (first args))
                               (<= (lvar-value (first args)) width))
                      (return-from insert-lvar-cut)))))
               (filter-lvar lvar
                            (if signedp
                                `(mask-signed-field ,width 'dummy)
                                `(logand 'dummy ,(ldb (byte width 0) -1))))
               (do-uses (node lvar)
                 (setf (block-reoptimize (node-block node)) t)
                 (reoptimize-component (node-component node) :maybe))
               t)
             (cut-node (node)
               "Try to cut a node to width. The primary return value is
                whether we managed to cut (cleverly), and the second whether
                anything was changed.  The third return value tells whether
                the cut value might be wider than expected."
               (when (block-delete-p (node-block node))
                 (return-from cut-node (values t nil)))
               (typecase node
                 (ref
                  (typecase (ref-leaf node)
                    (constant
                     (let* ((constant-value (constant-value (ref-leaf node)))
                            (new-value
                              (cond ((not (integerp constant-value))
                                     (return-from cut-node (values t nil)))
                                    (signedp
                                     (mask-signed-field width constant-value))
                                    (t
                                     (ldb (byte width 0) constant-value)))))
                       (cond ((= constant-value new-value)
                              (values t nil)) ; we knew what to do and did nothing
                             (t
                              (change-ref-leaf node (make-constant new-value)
                                               :recklessly t)
                              (let ((lvar (node-lvar node)))
                                (setf (lvar-%derived-type lvar)
                                      (and (lvar-has-single-use-p lvar)
                                           (make-values-type :required (list (ctype-of new-value))))))
                              (setf (block-reoptimize (node-block node)) t)
                              (reoptimize-component (node-component node) :maybe)
                              (values t t)))))))
                 (combination
                  (when (eq (basic-combination-kind node) :known)
                    (let* ((fun-ref (lvar-use (combination-fun node)))
                           (fun-name (lvar-fun-name (combination-fun node)))
                           (modular-fun (find-modular-version fun-name kind
                                                              signedp width)))
                      (cond ((not modular-fun)
                             ;; don't know what to do here
                             (values nil nil))
                            ((let ((dtype (single-value-type
                                           (node-derived-type node))))
                               (and
                                (case fun-name
                                  (logand
                                   (csubtypep dtype
                                              (specifier-type 'unsigned-byte)))
                                  (logior
                                   (csubtypep dtype
                                              (specifier-type '(integer * 0))))
                                  (mask-signed-field
                                   t)
                                  (t nil))
                                (csubtypep dtype type)))
                             ;; nothing to do
                             (values t nil))
                            (t
                             (binding* ((name (etypecase modular-fun
                                                ((eql :good) fun-name)
                                                (modular-fun-info
                                                 (modular-fun-info-name modular-fun))
                                                (function
                                                 (funcall modular-fun node width)))
                                              :exit-if-null)
                                        (did-something nil)
                                        (over-wide nil))
                               (unless (eql modular-fun :good)
                                 (setq did-something t
                                       over-wide t)
                                 (change-ref-leaf
                                  fun-ref
                                  (find-free-fun name "in a strange place"))
                                 (setf (combination-kind node) :full))
                               (unless (functionp modular-fun)
                                 (dolist (arg (basic-combination-args node))
                                   (multiple-value-bind (change wide)
                                       (cut-lvar arg)
                                     (setf did-something (or did-something change)
                                           over-wide (or over-wide wide)))))
                               (when did-something
                                 (reoptimize-node node name))
                               (values t did-something over-wide)))))))))
             (cut-lvar (lvar &key head
                        &aux did-something must-insert over-wide)
               "Cut all the LVAR's use nodes. If any of them wasn't handled
                and its type is too wide for the operation we wish to perform
                insert an explicit bit-width narrowing operation (LOGAND or
                MASK-SIGNED-FIELD) between the LVAR (*) and its destination.
                The narrowing operation might not be inserted if the LVAR's
                destination is already such an operation, to avoid endless
                recursion.

                If we're at the head, forcibly insert a cut operation if the
                result might be too wide.

                (*) We can't easily do that for each node, and doing so might
                result in code bloat, anyway. (I'm also not sure it would be
                correct for complicated C/D FG)"
               (do-uses (node lvar)
                 (multiple-value-bind (handled any-change wide)
                     (cut-node node)
                   (setf did-something (or did-something any-change)
                         must-insert (or must-insert
                                         (not (or handled
                                                  (csubtypep (single-value-type
                                                              (node-derived-type node))
                                                             type))))
                         over-wide (or over-wide wide))))
               (when (or must-insert
                         (and head over-wide))
                 (setf did-something (or (insert-lvar-cut lvar) did-something)
                       ;; we're just the right width after an explicit cut.
                       over-wide nil))
               (values did-something over-wide)))
      (cut-lvar lvar :head t))))

(defun best-modular-version (width signedp)
  ;; 1. exact width-matched :untagged
  ;; 2. >/>= width-matched :tagged
  ;; 3. >/>= width-matched :untagged
  (let* ((uuwidths (modular-class-widths *untagged-unsigned-modular-class*))
         (uswidths (modular-class-widths *untagged-signed-modular-class*))
         (uwidths (if (and uuwidths uswidths)
                      (merge 'list (copy-list uuwidths) (copy-list uswidths)
                             #'< :key #'car)
                      (or uuwidths uswidths)))
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

(defun integer-type-numeric-bounds (type)
  (typecase type
    ;; KLUDGE: this is not INTEGER-type-numeric-bounds
    (numeric-type (values (numeric-type-low type)
                          (numeric-type-high type)))
    (union-type
     (let ((low  nil)
           (high nil))
       (dolist (type (union-type-types type) (values low high))
         (unless (and (numeric-type-p type)
                      (eql (numeric-type-class type) 'integer))
           (return (values nil nil)))
         (let ((this-low (numeric-type-low type))
               (this-high (numeric-type-high type)))
           (unless (and this-low this-high)
             (return (values nil nil)))
           (setf low  (min this-low  (or low  this-low))
                 high (max this-high (or high this-high)))))))))

(defoptimizer (logand optimizer) ((x y) node)
  (let ((result-type (single-value-type (node-derived-type node))))
    (multiple-value-bind (low high)
        (integer-type-numeric-bounds result-type)
      (when (and (numberp low)
                 (numberp high)
                 (>= low 0))
        (let ((width (integer-length high)))
          (multiple-value-bind (w kind signedp)
              (best-modular-version width nil)
            (when w
              ;; FIXME: This should be (CUT-TO-WIDTH NODE KIND WIDTH SIGNEDP).
              ;;
              ;; FIXME: I think the FIXME (which is from APD) above
              ;; implies that CUT-TO-WIDTH should do /everything/
              ;; that's required, including reoptimizing things
              ;; itself that it knows are necessary.  At the moment,
              ;; CUT-TO-WIDTH sets up some new calls with
              ;; combination-type :FULL, which later get noticed as
              ;; known functions and properly converted.
              ;;
              ;; We cut to W not WIDTH if SIGNEDP is true, because
              ;; signed constant replacement needs to know which bit
              ;; in the field is the signed bit.
              (let ((xact (cut-to-width x kind (if signedp w width) signedp))
                    (yact (cut-to-width y kind (if signedp w width) signedp)))
                (declare (ignore xact yact))
                nil) ; After fixing above, replace with T, meaning
                                        ; "don't reoptimize this (LOGAND) node any more".
              )))))))

(defoptimizer (mask-signed-field optimizer) ((width x) node)
  (declare (ignore width))
  (let ((result-type (single-value-type (node-derived-type node))))
    (multiple-value-bind (low high)
        (integer-type-numeric-bounds result-type)
      (when (and (numberp low) (numberp high))
        (let ((width (max (integer-length high) (integer-length low))))
          (multiple-value-bind (w kind)
              (best-modular-version (1+ width) t)
            (when w
              ;; FIXME: This should be (CUT-TO-WIDTH NODE KIND W T).
              ;; [ see comment above in LOGAND optimizer ]
              (cut-to-width x kind w t)
              nil                ; After fixing above, replace with T.
              )))))))

(defoptimizer (logior optimizer) ((x y) node)
  (let ((result-type (single-value-type (node-derived-type node))))
    (multiple-value-bind (low high)
        (integer-type-numeric-bounds result-type)
      (when (and (numberp low)
                 (numberp high)
                 (<= high 0))
        (let ((width (integer-length low)))
          (multiple-value-bind (w kind)
              (best-modular-version (1+ width) t)
            (when w
              ;; FIXME: see comment in LOGAND optimizer
              (let ((xact (cut-to-width x kind w t))
                    (yact (cut-to-width y kind w t)))
                (declare (ignore xact yact))
                nil) ; After fixing above, replace with T
              )))))))

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

;;; These must come before the ones below, so that they are tried
;;; first.
(deftransform floor ((number divisor))
  `(multiple-value-bind (tru rem) (truncate number divisor)
     (if (and (not (zerop rem))
              (if (minusp divisor)
                  (plusp number)
                  (minusp number)))
         (values (1- tru) (+ rem divisor))
         (values tru rem))))

(deftransform ceiling ((number divisor))
  `(multiple-value-bind (tru rem) (truncate number divisor)
     (if (and (not (zerop rem))
              (if (minusp divisor)
                  (minusp number)
                  (plusp number)))
         (values (+ tru 1) (- rem divisor))
         (values tru rem))))

(deftransform rem ((number divisor))
  `(nth-value 1 (truncate number divisor)))

(deftransform mod ((number divisor))
  `(let ((rem (rem number divisor)))
     (if (and (not (zerop rem))
              (if (minusp divisor)
                  (plusp number)
                  (minusp number)))
         (+ rem divisor)
         rem)))

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

;;; Return an expression to calculate the integer quotient of X and
;;; constant Y, using multiplication, shift and add/sub instead of
;;; division. Both arguments must be unsigned, fit in a machine word and
;;; Y must neither be zero nor a power of two. The quotient is rounded
;;; towards zero.
;;; The algorithm is taken from the paper "Division by Invariant
;;; Integers using Multiplication", 1994 by Torbj\"{o}rn Granlund and
;;; Peter L. Montgomery, Figures 4.2 and 6.2, modified to exclude the
;;; case of division by powers of two.
;;; The algorithm includes an adaptive precision argument.  Use it, since
;;; we often have sub-word value ranges.  Careful, in this case, we need
;;; p s.t 2^p > n, not the ceiling of the binary log.
;;; Also, for some reason, the paper prefers shifting to masking.  Mask
;;; instead.  Masking is equivalent to shifting right, then left again;
;;; all the intermediate values are still words, so we just have to shift
;;; right a bit more to compensate, at the end.
;;;
;;; The following two examples show an average case and the worst case
;;; with respect to the complexity of the generated expression, under
;;; a word size of 64 bits:
;;;
;;; (UNSIGNED-DIV-TRANSFORMER 10 MOST-POSITIVE-WORD) ->
;;; (ASH (%MULTIPLY (LOGANDC2 X 0) 14757395258967641293) -3)
;;;
;;; (UNSIGNED-DIV-TRANSFORMER 7 MOST-POSITIVE-WORD) ->
;;; (LET* ((NUM X)
;;;        (T1 (%MULTIPLY NUM 2635249153387078803)))
;;;   (ASH (LDB (BYTE 64 0)
;;;             (+ T1 (ASH (LDB (BYTE 64 0)
;;;                             (- NUM T1))
;;;                        -1)))
;;;        -2))
;;;
(defun gen-unsigned-div-by-constant-expr (y max-x)
  (declare (type (integer 3 #.most-positive-word) y)
           (type word max-x))
  (aver (not (zerop (logand y (1- y)))))
  (labels ((ld (x)
             ;; the floor of the binary logarithm of (positive) X
             (integer-length (1- x)))
           (choose-multiplier (y precision)
             (do* ((l (ld y))
                   (shift l (1- shift))
                   (expt-2-n+l (expt 2 (+ sb!vm:n-word-bits l)))
                   (m-low (truncate expt-2-n+l y) (ash m-low -1))
                   (m-high (truncate (+ expt-2-n+l
                                        (ash expt-2-n+l (- precision)))
                                     y)
                           (ash m-high -1)))
                  ((not (and (< (ash m-low -1) (ash m-high -1))
                             (> shift 0)))
                   (values m-high shift)))))
    (let ((n (expt 2 sb!vm:n-word-bits))
          (precision (integer-length max-x))
          (shift1 0))
      (multiple-value-bind (m shift2)
          (choose-multiplier y precision)
        (when (and (>= m n) (evenp y))
          (setq shift1 (ld (logand y (- y))))
          (multiple-value-setq (m shift2)
            (choose-multiplier (/ y (ash 1 shift1))
                               (- precision shift1))))
        (cond ((>= m n)
               (flet ((word (x)
                        `(truly-the word ,x)))
                 `(let* ((num x)
                         (t1 (%multiply-high num ,(- m n))))
                    (ash ,(word `(+ t1 (ash ,(word `(- num t1))
                                            -1)))
                         ,(- 1 shift2)))))
              ((and (zerop shift1) (zerop shift2))
               (let ((max (truncate max-x y)))
                 ;; Explicit TRULY-THE needed to get the FIXNUM=>FIXNUM
                 ;; VOP.
                 `(truly-the (integer 0 ,max)
                             (%multiply-high x ,m))))
              (t
               `(ash (%multiply-high (logandc2 x ,(1- (ash 1 shift1))) ,m)
                     ,(- (+ shift1 shift2)))))))))

;;; If the divisor is constant and both args are positive and fit in a
;;; machine word, replace the division by a multiplication and possibly
;;; some shifts and an addition. Calculate the remainder by a second
;;; multiplication and a subtraction. Dead code elimination will
;;; suppress the latter part if only the quotient is needed. If the type
;;; of the dividend allows to derive that the quotient will always have
;;; the same value, emit much simpler code to handle that. (This case
;;; may be rare but it's easy to detect and the compiler doesn't find
;;; this optimization on its own.)
(deftransform truncate ((x y) (word (constant-arg word))
                        *
                        :policy (and (> speed compilation-speed)
                                     (> speed space)))
  "convert integer division to multiplication"
  (let* ((y      (lvar-value y))
         (x-type (lvar-type x))
         (max-x  (or (and (numeric-type-p x-type)
                          (numeric-type-high x-type))
                     most-positive-word)))
    ;; Division by zero, one or powers of two is handled elsewhere.
    (when (zerop (logand y (1- y)))
      (give-up-ir1-transform))
    `(let* ((quot ,(gen-unsigned-div-by-constant-expr y max-x))
            (rem (ldb (byte #.sb!vm:n-word-bits 0)
                      (- x (* quot ,y)))))
       (values quot rem))))

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

(defun least-zero-bit (x)
  (and (/= x -1)
       (1- (integer-length (logxor x (1+ x))))))

(deftransform logand ((x y) (* (constant-arg t)) *)
  "fold identity operation"
  (let* ((y (lvar-value y))
         (width (or (least-zero-bit y) '*)))
    (unless (and (neq width 0) ; (logand x 0) handled elsewhere
                 (csubtypep (lvar-type x)
                            (specifier-type `(unsigned-byte ,width))))
      (give-up-ir1-transform))
    'x))

(deftransform mask-signed-field ((size x) ((constant-arg t) *) *)
  "fold identity operation"
  (let ((size (lvar-value size)))
    (when (= size 0) (give-up-ir1-transform))
    (unless (csubtypep (lvar-type x) (specifier-type `(signed-byte ,size)))
      (give-up-ir1-transform))
    'x))

(deftransform logior ((x y) (* (constant-arg integer)) *)
  "fold identity operation"
  (let* ((y (lvar-value y))
         (width (or (least-zero-bit (lognot y))
                    (give-up-ir1-transform)))) ; (logior x 0) handled elsewhere
    (unless (csubtypep (lvar-type x)
                       (specifier-type `(integer ,(- (ash 1 width)) -1)))
      (give-up-ir1-transform))
    'x))

;;; Pick off easy association opportunities for constant folding.
;;; More complicated stuff that also depends on commutativity
;;; (e.g. (f (f x k1) (f y k2)) => (f (f x y) (f k1 k2))) should
;;; probably be handled with a more general tree-rewriting pass.
(macrolet ((def (operator &key (type 'integer) (folded operator))
             `(deftransform ,operator ((x z) (,type (constant-arg ,type)))
                ,(format nil "associate ~A/~A of constants"
                         operator folded)
                (binding* ((node  (if (lvar-has-single-use-p x)
                                      (lvar-use x)
                                      (give-up-ir1-transform)))
                           (nil (or (and (combination-p node)
                                         (eq (lvar-fun-name
                                              (combination-fun node))
                                             ',folded))
                                    (give-up-ir1-transform)))
                           (y   (second (combination-args node)))
                           (nil (or (constant-lvar-p y)
                                    (give-up-ir1-transform)))
                           (y   (lvar-value y)))
                  (unless (typep y ',type)
                    (give-up-ir1-transform))
                  (splice-fun-args x ',folded 2)
                  `(lambda (x y z)
                     (declare (ignore y z))
                     ;; (operator (folded x y) z)
                     ;; == (operator x (folded z y))
                     (,',operator x ',(,folded (lvar-value z) y)))))))
  (def logand)
  (def logior)
  (def logxor)
  (def logtest :folded logand)
  (def + :type rational)
  (def + :type rational :folded -)
  (def * :type rational)
  (def * :type rational :folded /))

(deftransform mask-signed-field ((width x) ((constant-arg unsigned-byte) *))
  "Fold mask-signed-field/mask-signed-field of constant width"
  (binding* ((node  (if (lvar-has-single-use-p x)
                        (lvar-use x)
                        (give-up-ir1-transform)))
             (nil (or (combination-p node)
                      (give-up-ir1-transform)))
             (nil (or (eq (lvar-fun-name (combination-fun node))
                          'mask-signed-field)
                      (give-up-ir1-transform)))
             (x-width (first (combination-args node)))
             (nil (or (constant-lvar-p x-width)
                      (give-up-ir1-transform)))
             (x-width (lvar-value x-width)))
    (unless (typep x-width 'unsigned-byte)
      (give-up-ir1-transform))
    (splice-fun-args x 'mask-signed-field 2)
    `(lambda (width x-width x)
       (declare (ignore width x-width))
       (mask-signed-field ,(min (lvar-value width) x-width) x))))

;;; These are restricted to rationals, because (- 0 0.0) is 0.0, not -0.0, and
;;; (* 0 -4.0) is -0.0.
(deftransform - ((x y) ((constant-arg (member 0)) rational) *)
  "convert (- 0 x) to negate"
  '(%negate y))
(deftransform * ((x y) (rational (constant-arg (member 0))) *)
  "convert (* x 0) to 0"
  0)

(deftransform %negate ((x) (rational))
  "Eliminate %negate/%negate of rationals"
  (splice-fun-args x '%negate 1)
  '(the rational x))

(deftransform %negate ((x) (number))
  "Combine %negate/*"
  (let ((use (lvar-uses x))
        arg)
    (unless (and (combination-p use)
                 (eql '* (lvar-fun-name (combination-fun use)))
                 (constant-lvar-p (setf arg (second (combination-args use))))
                 (numberp (setf arg (lvar-value arg))))
      (give-up-ir1-transform))
    (splice-fun-args x '* 2)
    `(lambda (x y)
       (declare (ignore y))
       (* x ,(- arg)))))

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

(macrolet ((def (name &optional float)
             (let ((x (if float '(float x) 'x)))
               `(deftransform ,name ((x y) (integer (constant-arg (member 1 -1)))
                                     *)
                  "fold division by 1"
                  `(values ,(if (minusp (lvar-value y))
                                '(%negate ,x)
                                ',x)  0)))))
  (def truncate)
  (def round)
  (def floor)
  (def ceiling)
  (def ftruncate t)
  (def fround t)
  (def ffloor t)
  (def fceiling t))


;;;; character operations

(deftransform two-arg-char-equal ((a b) (base-char base-char) *
                                  :policy (> speed space))
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

(deftransform two-arg-char-equal ((a b) (* (constant-arg character)) *
                                  :node node)
  (let ((char (lvar-value b)))
    (if (both-case-p char)
        (let ((reverse (if (upper-case-p char)
                           (char-downcase char)
                           (char-upcase char))))
          (if (policy node (> speed space))
              `(or (char= a ,char)
                   (char= a ,reverse))
              `(char-equal-constant a ,char ,reverse)))
        '(char= a b))))

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

;;; Can't use the above thing, since TYPES-EQUAL-OR-INTERSECT is case sensitive.
(deftransform two-arg-char-equal ((x y) * *)
  (cond
    ((same-leaf-ref-p x y) t)
    (t (give-up-ir1-transform))))

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
        #!+integer-eql-vop (int-type (specifier-type 'integer))
        (char-type (specifier-type 'character)))
    (cond
      ((same-leaf-ref-p x y) t)
      ((not (types-equal-or-intersect x-type y-type))
       nil)
      ((and (csubtypep x-type char-type)
            (csubtypep y-type char-type))
       '(char= x y))
      ((or (eq-comparable-type-p x-type) (eq-comparable-type-p y-type))
       '(eq y x))
      #!+integer-eql-vop
      ((or (csubtypep x-type int-type) (csubtypep y-type int-type))
       '(%eql/integer x y))
      (t
       (give-up-ir1-transform)))))

;;; similarly to the EQL transform above, we attempt to constant-fold
;;; or convert to a simpler predicate: mostly we have to be careful
;;; with strings and bit-vectors.
(deftransform equal ((x y) * *)
  "convert to simpler equality predicate"
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y))
        (combination-type (specifier-type '(or bit-vector string
                                            cons pathname))))
    (flet ((both-csubtypep (type)
             (let ((ctype (specifier-type type)))
               (and (csubtypep x-type ctype)
                    (csubtypep y-type ctype)))))
      (cond
        ((same-leaf-ref-p x y) t)
        ((and (constant-lvar-p x)
              (equal (lvar-value x) ""))
         `(and (stringp y)
               (zerop (length y))))
        ((and (constant-lvar-p y)
              (equal (lvar-value y) ""))
         `(and (stringp x)
               (zerop (length x))))
        ((both-csubtypep 'string)
         '(string= x y))
        ((both-csubtypep 'bit-vector)
         '(bit-vector-= x y))
        ((both-csubtypep 'pathname)
         '(pathname= x y))
        ((or (not (types-equal-or-intersect x-type combination-type))
             (not (types-equal-or-intersect y-type combination-type)))
         (if (types-equal-or-intersect x-type y-type)
             '(eql x y)
             ;; Can't simply check for type intersection if both types are combination-type
             ;; since array specialization would mean types don't intersect, even when EQUAL
             ;; doesn't care for specialization.
             ;; Previously checking for intersection in the outer COND resulted in
             ;;
             ;; (equal (the (cons (or simple-bit-vector
             ;;                       simple-base-string))
             ;;             x)
             ;;        (the (cons (or (and bit-vector (not simple-array))
             ;;                       (simple-array character (*))))
             ;;             y))
             ;; being incorrectly folded to NIL
             nil))
        (t (give-up-ir1-transform))))))

(deftransform equalp ((x y) * *)
  "convert to simpler equality predicate"
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y))
        (combination-type (specifier-type '(or number array
                                            character
                                            cons pathname
                                            instance hash-table))))
    (flet ((both-csubtypep (type)
             (let ((ctype (specifier-type type)))
               (and (csubtypep x-type ctype)
                    (csubtypep y-type ctype)))))
      (cond
        ((same-leaf-ref-p x y) t)
        ((and (constant-lvar-p x)
              (equal (lvar-value x) ""))
         `(and (stringp y)
               (zerop (length y))))
        ((and (constant-lvar-p y)
              (equal (lvar-value y) ""))
         `(and (stringp x)
               (zerop (length x))))
        ((both-csubtypep 'string)
         '(string-equal x y))
        ((both-csubtypep 'bit-vector)
         '(bit-vector-= x y))
        ((both-csubtypep 'pathname)
         '(pathname= x y))
        ((both-csubtypep 'character)
         '(char-equal x y))
        ((both-csubtypep 'number)
         '(= x y))
        ((both-csubtypep 'hash-table)
         '(hash-table-equalp x y))
        ((or (not (types-equal-or-intersect x-type combination-type))
             (not (types-equal-or-intersect y-type combination-type)))
         ;; See the comment about specialized types in the EQUAL transform above
         (if (types-equal-or-intersect y-type x-type)
             '(eq x y)
             nil))
        (t (give-up-ir1-transform))))))

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
;;; are false, and we don't have type-information available yet. The
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
  (multi-compare 'two-arg-char-equal args nil 'character t))
(define-source-transform char-lessp (&rest args)
  (multi-compare 'two-arg-char-lessp args nil 'character t))
(define-source-transform char-greaterp (&rest args)
  (multi-compare 'two-arg-char-greaterp args nil 'character t))
(define-source-transform char-not-greaterp (&rest args)
  (multi-compare 'two-arg-char-greaterp args t 'character t))
(define-source-transform char-not-lessp (&rest args)
  (multi-compare 'two-arg-char-lessp args t 'character t))

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

;;; Simplify some cross-type comparisons
(macrolet ((def (comparator round)
             `(progn
                (deftransform ,comparator
                    ((x y) (rational (constant-arg float)))
                  "open-code RATIONAL to FLOAT comparison"
                  (let ((y (lvar-value y)))
                    #-sb-xc-host
                    (when (or (float-nan-p y)
                              (float-infinity-p y))
                      (give-up-ir1-transform))
                    (setf y (rational y))
                    `(,',comparator
                      x ,(if (csubtypep (lvar-type x)
                                        (specifier-type 'integer))
                             (,round y)
                             y))))
                (deftransform ,comparator
                    ((x y) (integer (constant-arg ratio)))
                  "open-code INTEGER to RATIO comparison"
                  `(,',comparator x ,(,round (lvar-value y)))))))
  (def < ceiling)
  (def > floor))

(deftransform = ((x y) (rational (constant-arg float)))
  "open-code RATIONAL to FLOAT comparison"
  (let ((y (lvar-value y)))
    #-sb-xc-host
    (when (or (float-nan-p y)
              (float-infinity-p y))
      (give-up-ir1-transform))
    (setf y (rational y))
    (if (and (csubtypep (lvar-type x)
                        (specifier-type 'integer))
             (ratiop y))
        nil
        `(= x ,y))))

(deftransform = ((x y) (integer (constant-arg ratio)))
  "constant-fold INTEGER to RATIO comparison"
  nil)

;;;; converting N-arg arithmetic functions
;;;;
;;;; N-arg arithmetic and logic functions are associated into two-arg
;;;; versions, and degenerate cases are flushed.

;;; Left-associate FIRST-ARG and MORE-ARGS using FUNCTION.
(declaim (ftype (sfunction (symbol t list) list) associate-args))
(defun associate-args (fun first-arg more-args)
  (aver more-args)
  (let ((next (rest more-args))
        (arg (first more-args)))
    (if (null next)
        `(,fun ,first-arg ,arg)
        (associate-args fun `(,fun ,first-arg ,arg) next))))

;;; Reduce constants in ARGS list.
(declaim (ftype (sfunction (symbol list symbol) list) reduce-constants))
(defun reduce-constants (fun args one-arg-result-type)
  (let ((one-arg-constant-p (ecase one-arg-result-type
                              (number #'numberp)
                              (integer #'integerp)))
        (reduced-value)
        (reduced-p nil))
    (collect ((not-constants))
      (dolist (arg args)
        (let ((value (if (constantp arg)
                         (constant-form-value arg)
                         arg)))
          (cond ((not (funcall one-arg-constant-p value))
                 (not-constants arg))
                (reduced-value
                 (setf reduced-value (funcall fun reduced-value value)
                       reduced-p t))
                (t
                 (setf reduced-value value)))))
      ;; It is tempting to drop constants reduced to identity here,
      ;; but if X is SNaN in (* X 1), we cannot drop the 1.
      (if (not-constants)
          (if reduced-p
              `(,reduced-value ,@(not-constants))
              args)
          `(,reduced-value)))))

;;; Do source transformations for transitive functions such as +.
;;; One-arg cases are replaced with the arg and zero arg cases with
;;; the identity. ONE-ARG-RESULT-TYPE is the type to ensure (with THE)
;;; that the argument in one-argument calls is.
(declaim (ftype (function (symbol list t &optional symbol list)
                          (values t &optional (member nil t)))
                source-transform-transitive))
(defun source-transform-transitive (fun args identity
                                    &optional (one-arg-result-type 'number)
                                              (one-arg-prefixes '(values)))
  (case (length args)
    (0 identity)
    (1 `(,@one-arg-prefixes (the ,one-arg-result-type ,(first args))))
    (2 (values nil t))
    (t
     (let* ((reduced-args (reduce-constants fun args one-arg-result-type))
            (first (first reduced-args))
            (rest (rest reduced-args)))
       (if rest
           (associate-args fun first rest)
           first)))))

(define-source-transform + (&rest args)
  (source-transform-transitive '+ args 0))
(define-source-transform * (&rest args)
  (source-transform-transitive '* args 1))
(define-source-transform logior (&rest args)
  (source-transform-transitive 'logior args 0 'integer))
(define-source-transform logxor (&rest args)
  (source-transform-transitive 'logxor args 0 'integer))
(define-source-transform logand (&rest args)
  (source-transform-transitive 'logand args -1 'integer))
(define-source-transform logeqv (&rest args)
  (source-transform-transitive 'logeqv args -1 'integer))
(define-source-transform gcd (&rest args)
  (source-transform-transitive 'gcd args 0 'integer '(abs)))
(define-source-transform lcm (&rest args)
  (source-transform-transitive 'lcm args 1 'integer '(abs)))

;;; Do source transformations for intransitive n-arg functions such as
;;; /. With one arg, we form the inverse. With two args we pass.
;;; Otherwise we associate into two-arg calls.
(declaim (ftype (function (symbol symbol list list &optional symbol)
                          (values list &optional (member nil t)))
                source-transform-intransitive))
(defun source-transform-intransitive (fun fun* args one-arg-prefixes
                                      &optional (one-arg-result-type 'number))
  (case (length args)
    ((0 2) (values nil t))
    (1 `(,@one-arg-prefixes (the ,one-arg-result-type ,(first args))))
    (t
     (let ((reduced-args
             (reduce-constants fun* (rest args) one-arg-result-type)))
       (associate-args fun (first args) reduced-args)))))

(define-source-transform - (&rest args)
  (source-transform-intransitive '- '+ args '(%negate)))
(define-source-transform / (&rest args)
  (source-transform-intransitive '/ '* args '(/ 1)))

;;;; transforming APPLY

;;; We convert APPLY into MULTIPLE-VALUE-CALL so that the compiler
;;; only needs to understand one kind of variable-argument call. It is
;;; more efficient to convert APPLY to MV-CALL than MV-CALL to APPLY.
(define-source-transform apply (fun arg &rest more-args)
  (let ((args (cons arg more-args)))
    `(multiple-value-call ,fun
       ,@(mapcar (lambda (x) `(values ,x)) (butlast args))
       (values-list ,(car (last args))))))

;;;; transforming references to &REST argument

;;; We add magical &MORE arguments to all functions with &REST. If ARG names
;;; the &REST argument, this returns the lambda-vars for the context and
;;; count.
(defun possible-rest-arg-context (arg)
  (when (symbolp arg)
    (let* ((var (lexenv-find arg vars))
           (info (when (lambda-var-p var)
                   (lambda-var-arg-info var))))
      (when (and info
                 (eq :rest (arg-info-kind info))
                 (consp (arg-info-default info)))
        (values-list (arg-info-default info))))))

(defun mark-more-context-used (rest-var)
  (let ((info (lambda-var-arg-info rest-var)))
    (aver (eq :rest (arg-info-kind info)))
    (destructuring-bind (context count &optional used) (arg-info-default info)
      (unless used
        (setf (arg-info-default info) (list context count t))))))

(defun mark-more-context-invalid (rest-var)
  (let ((info (lambda-var-arg-info rest-var)))
    (aver (eq :rest (arg-info-kind info)))
    (setf (arg-info-default info) t)))

;;; This determines of we the REF to a &REST variable is headed towards
;;; parts unknown, or if we can really use the context.
(defun rest-var-more-context-ok (lvar)
  (let* ((use (lvar-use lvar))
         (var (when (ref-p use) (ref-leaf use)))
         (home (when (lambda-var-p var) (lambda-var-home var)))
         (info (when (lambda-var-p var) (lambda-var-arg-info var)))
         (restp (when info (eq :rest (arg-info-kind info)))))
    (flet ((ref-good-for-more-context-p (ref)
             (when (not (node-lvar ref)) ; ref that goes nowhere is ok
               (return-from ref-good-for-more-context-p t))
             (let ((dest (principal-lvar-end (node-lvar ref))))
               (and (combination-p dest)
                    ;; If the destination is to anything but these, we're going to
                    ;; actually need the rest list -- and since other operations
                    ;; might modify the list destructively, the using the context
                    ;; isn't good anywhere else either.
                    (lvar-fun-is (combination-fun dest)
                                 '(%rest-values %rest-ref %rest-length
                                   %rest-null %rest-true))
                    ;; If the home lambda is different and isn't DX, it might
                    ;; escape -- in which case using the more context isn't safe.
                    (let ((clambda (node-home-lambda dest)))
                      (or (eq home clambda)
                          (leaf-dynamic-extent clambda)))))))
      (let ((ok (and restp
                     (consp (arg-info-default info))
                     (not (lambda-var-specvar var))
                     (not (lambda-var-sets var))
                     (every #'ref-good-for-more-context-p (lambda-var-refs var)))))
        (if ok
            (mark-more-context-used var)
            (when restp
              (mark-more-context-invalid var)))
        ok))))

;;; VALUES-LIST -> %REST-VALUES
(define-source-transform values-list (list)
  (multiple-value-bind (context count) (possible-rest-arg-context list)
    (if context
        `(%rest-values ,list ,context ,count)
        (values nil t))))

;;; NTH -> %REST-REF
(define-source-transform nth (n list)
  (multiple-value-bind (context count) (possible-rest-arg-context list)
    (if context
        `(%rest-ref ,n ,list ,context ,count)
        `(car (nthcdr ,n ,list)))))
(define-source-transform fast-&rest-nth (n list)
  (multiple-value-bind (context count) (possible-rest-arg-context list)
    (if context
        `(%rest-ref ,n ,list ,context ,count t)
        (bug "no &REST context for FAST-REST-NTH"))))

(define-source-transform elt (seq n)
  (if (policy *lexenv* (= safety 3))
      (values nil t)
      (multiple-value-bind (context count) (possible-rest-arg-context seq)
        (if context
            `(%rest-ref ,n ,seq ,context ,count)
            (values nil t)))))

;;; CAxR -> %REST-REF
(defun source-transform-car (list nth)
  (multiple-value-bind (context count) (possible-rest-arg-context list)
    (if context
        `(%rest-ref ,nth ,list ,context ,count)
        (values nil t))))

(define-source-transform car (list)
  (source-transform-car list 0))

(define-source-transform cadr (list)
  (or (source-transform-car list 1)
      `(car (cdr ,list))))

(define-source-transform caddr (list)
  (or (source-transform-car list 2)
      `(car (cdr (cdr ,list)))))

(define-source-transform cadddr (list)
  (or (source-transform-car list 3)
      `(car (cdr (cdr (cdr ,list))))))

;;; LENGTH -> %REST-LENGTH
(defun source-transform-length (list)
  (multiple-value-bind (context count) (possible-rest-arg-context list)
    (if context
        `(%rest-length ,list ,context ,count)
        (values nil t))))
(define-source-transform length (list) (source-transform-length list))
(define-source-transform list-length (list) (source-transform-length list))

;;; ENDP, NULL and NOT -> %REST-NULL
;;;
;;; Outside &REST convert into an IF so that IF optimizations will eliminate
;;; redundant negations.
(defun source-transform-null (x op)
  (multiple-value-bind (context count) (possible-rest-arg-context x)
    (cond (context
           `(%rest-null ',op ,x ,context ,count))
          ((eq 'endp op)
           `(if (the list ,x) nil t))
          (t
           `(if ,x nil t)))))
(define-source-transform not (x) (source-transform-null x 'not))
(define-source-transform null (x) (source-transform-null x 'null))
(define-source-transform endp (x) (source-transform-null x 'endp))

(deftransform %rest-values ((list context count))
  (if (rest-var-more-context-ok list)
      `(%more-arg-values context 0 count)
      `(values-list list)))

(deftransform %rest-ref ((n list context count &optional length-checked-p))
  (cond ((rest-var-more-context-ok list)
         (if (and (constant-lvar-p length-checked-p)
                  (lvar-value length-checked-p))
             `(%more-arg context n)
             `(and (< (the index n) count) (%more-arg context n))))
        ((and (constant-lvar-p n) (zerop (lvar-value n)))
         `(car list))
        (t
         `(nth n list))))

(deftransform %rest-length ((list context count))
  (if (rest-var-more-context-ok list)
      'count
      `(length list)))

(deftransform %rest-null ((op list context count))
  (aver (constant-lvar-p op))
  (if (rest-var-more-context-ok list)
      `(eql 0 count)
      `(,(lvar-value op) list)))

(deftransform %rest-true ((list context count))
  (if (rest-var-more-context-ok list)
      `(not (eql 0 count))
      `list))

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
  (declare (ignore dest))
  (when (constant-lvar-p control)
    (let ((x (lvar-value control)))
      (when (stringp x)
        (check-format-args x args 'format)))))

(defoptimizer (format derive-type) ((dest control &rest args))
  (declare (ignore control args))
  (when (and (constant-lvar-p dest)
             (null (lvar-value dest)))
    (specifier-type '(simple-array character (*)))))

;;; We disable this transform in the cross-compiler to save memory in
;;; the target image; most of the uses of FORMAT in the compiler are for
;;; error messages, and those don't need to be particularly fast.
#+sb-xc
(deftransform format ((dest control &rest args) (t simple-string &rest t) *
                      :policy (>= speed space))
  (unless (constant-lvar-p control)
    (give-up-ir1-transform "The control string is not a constant."))
  (let* ((argc (length args))
         (arg-names (make-gensym-list argc))
         (control (lvar-value control))
         ;; Expanding the control string now avoids deferring to FORMATTER
         ;; so that we don't need an internal-only variant of it that
         ;; passes through extra args to %FORMATTER.
         (expr (handler-case ; in case %formatter wants to signal an error
                   (sb!format::%formatter control argc nil)
                 ;; otherwise, let the macro complain
                 (sb!format:format-error () `(formatter ,control)))))
    `(lambda (dest control ,@arg-names)
       (declare (ignore control))
       (format dest ,expr ,@arg-names))))

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

(deftransform format ((stream control &rest args) (null function &rest t))
  (let ((arg-names (make-gensym-list (length args))))
    `(lambda (stream control ,@arg-names)
       (declare (ignore stream))
       (with-simple-output-to-string (stream)
         (funcall control stream ,@arg-names)))))

(defun concatenate-format-p (control args)
  (and
   (loop for directive in control
         always
         (or (stringp directive)
             (and (sb!format::format-directive-p directive)
                  (let ((char (sb!format::format-directive-character directive))
                        (params (sb!format::format-directive-params directive)))
                    (or
                     (and
                      (char-equal char #\a)
                      (null params)
                      (pop args))
                     (and
                      (or (eql char #\~)
                          (eql char #\%))
                      (null (sb!format::format-directive-colonp directive))
                      (null (sb!format::format-directive-atsignp directive))
                      (or (null params)
                          (typep params
                                 '(cons (cons (eql 1) unsigned-byte) null)))))))))
   (null args)))

(deftransform format ((stream control &rest args) (null (constant-arg string) &rest string))
  (let ((tokenized
          (handler-case
              (sb!format::tokenize-control-string (lvar-value control))
            (sb!format:format-error ()
              (give-up-ir1-transform)))))
    (unless (concatenate-format-p tokenized args)
      (give-up-ir1-transform))
    (let ((arg-names (make-gensym-list (length args))))
      `(lambda (stream control ,@arg-names)
         (declare (ignore stream control))
         (concatenate
          'string
          ,@(loop for directive in tokenized
                  for char = (and (not (stringp directive))
                                  (sb!format::format-directive-character directive))
                  when
                  (cond ((not char)
                         directive)
                        ((char-equal char #\a)
                         (pop arg-names))
                        (t
                         (let ((n (or (cdar (sb!format::format-directive-params directive))
                                      1)))
                           (and (plusp n)
                                (make-string n
                                             :initial-element
                                             (if (eql char #\%)
                                                 #\Newline
                                                 char))))))
                  collect it))))))

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

(defun constant-cons-type (type)
  (multiple-value-bind (singleton value)
      (type-singleton-p type)
    (if singleton
        (values value t)
        (typecase type
          (cons-type
           (multiple-value-bind (car car-good)
               (constant-cons-type (cons-type-car-type type))
             (multiple-value-bind (cdr cdr-good)
                 (constant-cons-type (cons-type-cdr-type type))
               (and car-good cdr-good
                    (values (cons car cdr) t)))))))))

(defoptimizer (coerce derive-type) ((value type) node)
  (multiple-value-bind (type constant)
      (if (constant-lvar-p type)
          (values (lvar-value type) t)
          (constant-cons-type (lvar-type type)))
    (when constant
      ;; This branch is essentially (RESULT-TYPE-SPECIFIER-NTH-ARG 2),
      ;; but dealing with the niggle that complex canonicalization gets
      ;; in the way: (COERCE 1 'COMPLEX) returns 1, which is not of
      ;; type COMPLEX.
      (let ((result-typeoid (careful-specifier-type type)))
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
           result-typeoid))))))

(defoptimizer (compile derive-type) ((nameoid function))
  (declare (ignore function))
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
                       (make-eql-type element-type))
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

(deftransform sort ((list predicate &key key)
                    (list * &rest t) *)
  `(sb!impl::stable-sort-list list
                              (%coerce-callable-to-fun predicate)
                              (if key (%coerce-callable-to-fun key) #'identity)))

(deftransform stable-sort ((sequence predicate &key key)
                           ((or vector list) *))
  (let ((sequence-type (lvar-type sequence)))
    (cond ((csubtypep sequence-type (specifier-type 'list))
           `(sb!impl::stable-sort-list sequence
                                       (%coerce-callable-to-fun predicate)
                                       (if key (%coerce-callable-to-fun key) #'identity)))
          ((csubtypep sequence-type (specifier-type 'simple-vector))
           `(sb!impl::stable-sort-simple-vector sequence
                                                (%coerce-callable-to-fun predicate)
                                                (and key (%coerce-callable-to-fun key))))
          (t
           `(sb!impl::stable-sort-vector sequence
                                         (%coerce-callable-to-fun predicate)
                                         (and key (%coerce-callable-to-fun key)))))))

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

(deftransform encode-universal-time
    ((second minute hour date month year &optional time-zone)
     ((constant-arg (mod 60)) (constant-arg (mod 60))
      (constant-arg (mod 24))
      (constant-arg (integer 1 31))
      (constant-arg (integer 1 12))
      (constant-arg (integer 1899))
      (constant-arg (rational -24 24))))
  (let ((second (lvar-value second))
        (minute (lvar-value minute))
        (hour (lvar-value hour))
        (date (lvar-value date))
        (month (lvar-value month))
        (year (lvar-value year))
        (time-zone (lvar-value time-zone)))
    (if (zerop (rem time-zone 1/3600))
        (encode-universal-time second minute hour date month year time-zone)
        (give-up-ir1-transform))))

#!-(and win32 (not sb-thread))
(deftransform sleep ((seconds) ((integer 0 #.(expt 10 8))))
  `(sb!unix:nanosleep seconds 0))

#!-(and win32 (not sb-thread))
(deftransform sleep ((seconds) ((constant-arg (real 0))))
  (let ((seconds-value (lvar-value seconds)))
    (multiple-value-bind (seconds nano)
        (sb!impl::split-seconds-for-sleep seconds-value)
      (if (> seconds (expt 10 8))
          (give-up-ir1-transform)
          `(sb!unix:nanosleep ,seconds ,nano)))))

;; On 64-bit architectures the TLS index is in the symbol header,
;; !DEFINE-PRIMITIVE-OBJECT doesn't define an accessor for it.
;; In the architectures where tls-index is an ordinary slot holding a tagged
;; object, it represents the byte offset to an aligned object and looks
;; in Lisp like a fixnum that is off by a factor of (EXPT 2 N-FIXNUM-TAG-BITS).
;; We're reading with a raw SAP accessor, so must make it look equally "off".
;; Also we don't get the defknown automatically.
#!+(and 64-bit sb-thread)
(defknown symbol-tls-index (t) fixnum (flushable))
#!+(and 64-bit sb-thread)
(define-source-transform symbol-tls-index (sym)
  `(ash (sap-ref-32 (int-sap (get-lisp-obj-address (the symbol ,sym)))
                    (- 4 sb!vm:other-pointer-lowtag))
        (- sb!vm:n-fixnum-tag-bits)))
