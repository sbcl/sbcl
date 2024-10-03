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

(in-package "SB-C")

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

(defoptimizer (complement derive-type) ((fun))
  (let ((type (lvar-fun-type fun)))
    (when (fun-type-p type)
      (specifier-type
       (append (butlast (type-specifier type))
               '(boolean))))))

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
     ((let ((lvar (node-lvar node)))
        (when lvar
          (let ((dest (lvar-dest lvar)))
            (and (combination-p dest)
                 (eq (combination-fun dest) lvar)))))
      '#'(lambda (&rest args)
           (not (apply fun args))))
     (t
      (give-up-ir1-transform
       "The function doesn't have a fixed argument count.")))))

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

;;; Pick off special cases of LIST and LIST*.
(define-source-transform list (&rest args)
  (if args (values nil t) (values nil nil)))
(define-source-transform list* (arg &rest others)
  (if others (values nil t) (values arg nil)))
;;; Use LIST* in lieu of CONS so that there are only 2 low-level allocators
;;; instead of 3. Strictly speaking, LIST is redundant as well.
(define-source-transform cons (x y) `(list* ,x ,y))
#+system-tlabs
(unless-vop-existsp (:translate acons)
  (define-source-transform acons (key datum alist &environment env)
    (if (sb-vm::env-system-tlab-p env)
        `(cons (cons ,key ,datum) ,alist)
        (values nil t))))
(defoptimizer (list derive-type) ((&rest args))
  (if args
      (specifier-type 'cons)
      (specifier-type 'null)))

(defoptimizer (list* derive-type) ((arg &rest args))
  (if args
      (specifier-type 'cons)
      (lvar-type arg)))

(unless-vop-existsp (:translate unaligned-dx-cons)
  (define-source-transform unaligned-dx-cons (arg)
    `(list ,arg)))

(define-source-transform make-list (length &rest rest &environment env)
  (if (or (null rest)
          ;; Use of &KEY in source xforms doesn't have all the usual semantics.
          ;; It's better to hand-roll it - cf. transforms for WRITE[-TO-STRING].
          (typep rest '(cons (eql :initial-element) (cons t null))))
      `(,(if (sb-vm::env-system-tlab-p env)
             'sb-impl::%sys-make-list
             '%make-list)
        ,length ,(second rest))
      (values nil t))) ; give up

(deftransform %make-list ((length item) ((constant-arg (integer 0 2)) t))
  `(list ,@(make-list (lvar-value length) :initial-element 'item)))

(define-source-transform copy-list (list &environment env)
  ;; If speed is more important than space, or cons profiling is wanted,
  ;; then inline the whole copy loop.
  (if (policy env (or (> speed space) (> instrument-consing 1)))
      (once-only ((list `(the list ,list))) `(copy-list-macro ,list))
      (values nil t))) ; give up

;;; Optimize
;;; (loop append (cond (x
;;;                     nil)
;;;                    (t
;;;                     list)))
;;; by skipping copy-list-to from the NIL path.
(defoptimizer (sb-impl::copy-list-to optimizer) ((list tail) copy-list)
  (flet ((ref (ref lvar)
           (when (and (constant-p (ref-leaf ref))
                      (null (constant-value (ref-leaf ref)))
                      (almost-immediately-used-p lvar ref))
             (let ((set (node-dest copy-list)))
               (when (and (set-p set)
                          (eq (set-var set) (lvar-lambda-var tail))
                          (immediately-used-p (node-lvar copy-list) copy-list))
                 (node-ends-block set)
                 (node-ends-block ref)
                 (unlink-blocks (node-block ref) (car (block-succ (node-block ref))))
                 (delete-lvar-use ref)
                 (link-blocks (node-block ref) (car (block-succ (node-block set)))))))))
    (do-uses (node list)
      (typecase node
        (ref (ref node list))
        (cast
         (and (eq (cast-asserted-type node) (specifier-type 'list))
              (immediately-used-p list node)
              (do-uses (node (cast-value node))
                (when (ref-p node)
                  (ref node (node-lvar node))))))))))

(define-source-transform append (&rest lists)
  (case (length lists)
    (0 nil)
    (1 (car lists))
    (2 `(sb-impl::append2 ,@lists))
    (t (values nil t))))

(define-source-transform nconc (&rest lists)
  (case (length lists)
    (0 ())
    (1 (car lists))
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
         (last (lvar-type (car (last args)))))
    ;; Derive the actual return type, assuming that all but the last
    ;; arguments are LISTs (otherwise, APPEND/NCONC doesn't return).
    (loop with all-nil = t       ; all but the last args are NIL?
          with some-cons = nil   ; some args are conses?
          for (arg next) on args
          for lvar-type = (lvar-type arg)
          while next
          do
          (multiple-value-bind (typep definitely)
              (ctypep nil lvar-type)
            (cond (some-cons) ; we know result's a cons -- nothing to do
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

(defoptimizer (sb-impl::append2 derive-type) ((&rest args))
  (derive-append-type args))

(defoptimizer (nconc derive-type) ((&rest args))
  (derive-append-type args))

(deftransform sb-impl::append2 ((x y) (null t) * :important nil)
  'y)

(deftransform sb-impl::append2 ((x y) (list t) * :important nil)
  (let* ((n (splice-fun-args x 'list nil))
         (args (make-gensym-list (length n))))
    `(lambda (,@args y) (list* ,@args y))))

(deftransform append ((x &rest args) (list &rest t) * :important nil)
  (let* ((n (splice-fun-args x 'list nil))
         (list-args (make-gensym-list (length n)))
         (append-args (make-gensym-list (length args))))
    `(lambda (,@list-args ,@append-args) (list* ,@list-args (append ,@append-args)))))

(defoptimizer (append externally-checkable-type) ((&rest lists) node lvar)
  (if (eq lvar (car (last lists)))
      (specifier-type t)
      (specifier-type 'list)))

(setf (fun-info-externally-checkable-type (fun-info-or-lose 'nconc))
      #'append-externally-checkable-type-optimizer)

(flet ((remove-nil (fun args)
         (let ((remove
                 (loop for (arg . rest) on args
                       when (and rest
                                 (eq (lvar-type arg) (specifier-type 'null)))
                       collect arg)))
           (if remove
               (let ((vars (make-gensym-list (length args))))
                 `(lambda ,vars
                    (declare (ignorable ,@vars))
                    (,fun ,@(loop for var in vars
                                  for arg in args
                                  unless (memq arg remove)
                                  collect var))))
               (give-up-ir1-transform)))))
  (deftransform append ((&rest args))
    (remove-nil 'append args))
  (deftransform nconc ((&rest args))
    (remove-nil 'nconc args)))

(flet ((transform (fun args subseq &rest prefix)
         (let* (new
                subseqp
                vars
                (args (loop for arg in args
                            if (and subseq
                                    (lvar-matches arg :fun-names '(vector-subseq* subseq))
                                    ;; Nothing should be modifying the original sequence
                                    (almost-immediately-used-p arg (lvar-use arg) :flushable t))
                            append (let ((call (lvar-uses arg)))
                                     (setf new t
                                           subseqp t)
                                     (destructuring-bind (sequence start &optional end) (combination-args call)
                                       (declare (ignorable sequence start))
                                       (splice-fun-args arg :any (if end 3 2))
                                       (list ''sb-impl::%subseq
                                             (car (push (gensym) vars))
                                             (car (push (gensym) vars))
                                             (when end
                                               (car (push (gensym) vars))))))
                            else if (or (eq (lvar-type arg) (specifier-type 'null))
                                        (csubtypep (lvar-type arg) (specifier-type '(simple-array * (0)))))
                            do (setf new t)
                               (push (gensym) vars)
                            else
                            collect (car (push (gensym) vars)))))

           (if new
               `(lambda ,(append prefix (reverse vars))
                  (declare (ignorable ,@vars))
                  (,(if subseqp
                        subseq
                        fun)
                   ,@prefix ,@args))
               (give-up-ir1-transform)))))
  (deftransform %concatenate-to-list ((&rest args))
    (transform '%concatenate-to-list args '%concatenate-to-list-subseq))

  (deftransform %concatenate-to-string ((&rest args))
    (transform '%concatenate-to-string args '%concatenate-to-string-subseq))

  (deftransform %concatenate-to-base-string ((&rest args))
    (transform '%concatenate-to-base-string args '%concatenate-to-base-string-subseq))

  (deftransform %concatenate-to-vector ((widetag &rest args))
    (transform '%concatenate-to-vector args '%concatenate-to-vector-subseq 'widetag))

  (deftransform %concatenate-to-simple-vector ((&rest args))
    (transform '%concatenate-to-simple-vector args '%concatenate-to-simple-vector-subseq))
  (deftransform concatenate ((type &rest args))
    (transform 'concatenate args nil 'type)))

(deftransform %concatenate-to-string ((string) ((array character (*))))
  `(subseq string 0))

#+sb-unicode
(deftransform %concatenate-to-string ((string) ((array base-char (*))))
  `(coerce string '(array character (*))))

(defun concatenate-subseq-type (lvar args)
  (flet ((check (arg type)
           (when (eq arg lvar)
             (return-from concatenate-subseq-type type))))
    (loop while args
          do (let ((arg (pop args)))
               (cond ((and (constant-lvar-p arg)
                           (eq (lvar-value arg) 'sb-impl::%subseq))
                      (check (pop args) (specifier-type 'sequence))
                      (check (pop args) (specifier-type 'index))
                      (check (pop args) (specifier-type '(or null index))))
                     (t
                      (check arg (specifier-type 'sequence))))))))

(defoptimizer (%concatenate-to-string-subseq externally-checkable-type) ((&rest args) node lvar)
  (concatenate-subseq-type lvar args))
(defoptimizer (%concatenate-to-base-string-subseq externally-checkable-type) ((&rest args) node lvar)
  (concatenate-subseq-type lvar args))
(defoptimizer (%concatenate-to-list-subseq externally-checkable-type) ((&rest args) node lvar)
  (concatenate-subseq-type lvar args))
(defoptimizer (%concatenate-to-simple-vector-subseq externally-checkable-type) ((&rest args) node lvar)
  (concatenate-subseq-type lvar args))
(defoptimizer (%concatenate-to-vector-subseq externally-checkable-type) ((type &rest args) node lvar)
  (concatenate-subseq-type lvar args))

(defoptimizer (%concatenate-to-list derive-type) ((&rest args))
  (loop for arg in args
        for min = (nth-value 1 (sequence-lvar-dimensions arg))
        when (typep min '(integer 1))
        return (specifier-type 'cons)))

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
  (let ((c (and n (constant-lvar-p n))))
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
   (2 `(sb-impl::gethash3 ,@args nil))
   (3 `(sb-impl::gethash3 ,@args))
   (t (values nil t))))
(define-source-transform get (&rest args)
  (case (length args)
   (2 `(sb-impl::get3 ,@args nil))
   (3 `(sb-impl::get3 ,@args))
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

(deftransform nth ((n l) (unsigned-byte t) * :node node)
  "convert NTH to CAxxR"
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
      `(car ,(frob n)))))

;;;; arithmetic and numerology

(define-source-transform plusp (x) `(sb-xc:> ,x 0))
(define-source-transform minusp (x) `(sb-xc:< ,x 0))
(define-source-transform zerop (x) `(sb-xc:= ,x 0))

(define-source-transform 1+ (x) `(+ ,x 1))
(define-source-transform 1- (x) `(- ,x 1))

(define-source-transform oddp (x) `(logtest ,x 1))
(define-source-transform evenp (x) `(not (logtest ,x 1)))

(macrolet ((deffrob (fun)
             `(define-source-transform ,fun (x &optional (y nil y-p))
                (declare (ignore y))
                (if y-p
                    (values nil t)
                    `(,',fun ,x 1)))))
  (deffrob truncate)
  (deffrob round)
  (deffrob floor)
  (deffrob ceiling))

;;; This used to be a source transform (hence the lack of restrictions
;;; on the argument types), but we make it a regular transform so that
;;; the VM has a chance to see the bare LOGTEST and potentiall choose
;;; to implement it differently.  --njf, 06-02-2006
(deftransform logtest ((x y) * * :node node)
  (delay-ir1-transform node :ir1-phases)
  `(not (zerop (logand x y))))

(defoptimizer (logtest derive-type) ((x y))
  (let ((type (two-arg-derive-type x y
                                   #'logand-derive-type-aux
                                   #'logand)))
    (when type
      (multiple-value-bind (typep definitely)
          (ctypep 0 type)
        (cond ((and (not typep) definitely)
               (specifier-type '(eql t)))
              ((type= type (specifier-type '(eql 0)))
               (specifier-type '(eql nil))))))))

(defun logbitp-to-minusp-p (index integer)
  (let* ((int (type-approximate-interval (lvar-type integer)))
         (length (max (integer-length (interval-low int))
                      (integer-length (interval-high int))))
         (index-int (type-approximate-interval (lvar-type index))))
    (>= (interval-low index-int) length)))

(deftransform logbitp ((index integer) * * :node node)
  (let ((integer-type (lvar-type integer))
        (integer-value (and (constant-lvar-p integer)
                            (lvar-value integer))))
    (cond ((eql integer-value 0)
           nil)
          ((eql integer-value -1)
           t)
          ((csubtypep integer-type (specifier-type '(or word
                                                     sb-vm:signed-word)))
           (delay-ir1-transform node :ir1-phases)
           (if (logbitp-to-minusp-p index integer)
               `(minusp integer)
               `(logtest 1 (ash integer (- index)))))
          ((csubtypep integer-type (specifier-type 'bignum))
           (if (csubtypep (lvar-type index)
                          (specifier-type `(mod ,sb-vm:n-word-bits))) ; word-index
               `(logbitp index (%bignum-ref integer 0))
               `(bignum-logbitp index integer)))
          ((and (constant-lvar-p index)
                (< (lvar-value index) sb-vm:n-word-bits))
           `(logbitp index (logand integer ,most-positive-word)))
          (t
           (give-up-ir1-transform)))))


(defoptimizer (logbitp derive-type) ((index integer))
  (let* ((one (specifier-type '(eql 1)))
         (and (two-arg-derive-type index integer
                                   (lambda (index integer same)
                                     (declare (ignore same))
                                     (logand-derive-type-aux integer
                                                             (ash-derive-type-aux one index nil)))
                                   (lambda (index integer)
                                     (logand integer (ash 1 index))))))
    (cond ((not and)
           nil)
          ((type= and (specifier-type '(eql 0)))
           (specifier-type 'null))
          ((not (types-equal-or-intersect and (specifier-type '(eql 0))))
           (specifier-type '(eql t))))))

(define-source-transform byte (size position)
  (if (and (constantp size)
           (constantp position))
      `'(,(constant-form-value size) . ,(constant-form-value position))
      `(cons ,size ,position)))
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

(defoptimizer (%numerator derive-type) ((num))
  (cond ((csubtypep (lvar-type num) (specifier-type '(rational 0)))
         (specifier-type '(integer 1)))
        ((csubtypep (lvar-type num) (specifier-type '(rational * 0)))
         (specifier-type '(integer * -1)))))

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
(defun make-interval (&key low high)
  (labels ((normalize-bound (val)
             (cond ((and (floatp val)
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

;; Some backends (and the host) have no float traps
(declaim (inline bad-float-p))
(defun bad-float-p (value)
  (declare (ignorable value))
  #+(or arm (and arm64 (not darwin)) riscv sb-xc-host)
  (or (and (floatp value)
           (float-infinity-or-nan-p value))
      (and (complex-float-p value)
           (or (float-infinity-or-nan-p (imagpart value))
               (float-infinity-or-nan-p (realpart value))))))

;;; Apply the function F to a bound X. If X is an open bound and the
;;; function is declared strictly monotonic, then the result will be
;;; open. IF X is NIL, the result is NIL.
(defun bound-func (f x strict)
  (declare (type function f))
  (when x
    (handler-case
        (let ((bound (funcall f (type-bound-number x))))
          (unless (bad-float-p bound)
            (set-bound bound (and strict (consp x)))))
      ;; Some numerical operations will signal an ERROR, e.g. in
      ;; the course of converting a bignum to a float. Default to
      ;; NIL in that case.
      (arithmetic-error ()))))

(defun safe-double-coercion-p (x)
  (or (typep x 'double-float)
      (sb-xc:<= most-negative-double-float x most-positive-double-float)))

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
       ;; FIXME: If we ever add SSE-support for x86, this conditional needs to
       ;; change.
       ;; This is somewhat dubious. Why isn't anything done about
       ;; ratios? Why isn't safe-double-coercion-p doing the same,
       ;; given that x87 floats are 80-bits internally?
       #+x86
       (typep x `(or (not integer)
                     (integer ,most-negative-exactly-single-float-integer
                              ,most-positive-exactly-single-float-integer)))
       (sb-xc:<= most-negative-single-float x most-positive-single-float))))

;;; Apply a binary operator OP to two bounds X and Y. The result is
;;; NIL if either is NIL. Otherwise bound is computed and the result
;;; is open if either X or Y is open.
;;;
;;; FIXME: only used in this file, not needed in target runtime

;;; ANSI contagion specifies coercion to floating point if one of the
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
          (handler-case
              (let* ((,xb (type-bound-number ,x))
                     (,yb (type-bound-number ,y))
                     (,res (safely-binop ,op ,xb ,yb)))
                (set-bound ,res
                           (and (or (consp ,x) (consp ,y))
                                ;; Open bounds can very easily be messed up
                                ;; by FP rounding, so take care here.
                                ,(ecase op
                                   (sb-xc:*
                                    ;; Multiplying a greater-than-zero with
                                    ;; less than one can round to zero.
                                    `(or (not (fp-zero-p ,res))
                                         (cond ((and (consp ,x) (fp-zero-p ,xb))
                                                (sb-xc:>= (abs ,yb) 1))
                                               ((and (consp ,y) (fp-zero-p ,yb))
                                                (sb-xc:>= (abs ,xb) 1)))))
                                   (sb-xc:/
                                    ;; Dividing a greater-than-zero with
                                    ;; greater than one can round to zero.
                                    `(or (not (fp-zero-p ,res))
                                         (cond ((and (consp ,x) (fp-zero-p ,xb))
                                                (sb-xc:<= (abs ,yb) 1))
                                               ((and (consp ,y) (fp-zero-p ,yb))
                                                (sb-xc:<= (abs ,xb) 1)))))
                                   ((sb-xc:+ sb-xc:-)
                                    ;; Adding or subtracting greater-than-zero
                                    ;; can end up with identity.
                                    `(and (not (fp-zero-p ,xb))
                                          (not (fp-zero-p ,yb))))))))
            (arithmetic-error ())))))

(defun coercion-loses-precision-p (val type)
  (typecase val
    (single-float)
    (double-float (subtypep type 'single-float))
    (rational (subtypep type 'float))
    (t (bug "Unexpected arguments to bounds coercion: ~S ~S" val type))))

(defun coerce-for-bound (val type)
  (cond
    ((or (null val)
         (null type))
     val)
    ((consp val)
     (let ((xbound (coerce-for-bound (car val) type)))
       (if (coercion-loses-precision-p (car val) type)
           xbound
           (list xbound))))
    ((subtypep type 'double-float)
     (if (sb-xc:<= most-negative-double-float val most-positive-double-float)
         (coerce val type)))
    ((or (subtypep type 'single-float) (subtypep type 'float))
     ;; coerce to float returns a single-float
     (if (sb-xc:<= most-negative-single-float val most-positive-single-float)
         (coerce val type)))
    (t (coerce val type))))

(defun coerce-and-truncate-floats (val type)
  (when val
    (if (consp val)
        (let ((xbound (coerce-for-bound (car val) type)))
          (if (coercion-loses-precision-p (car val) type)
              xbound
              (list xbound)))
        (cond
          ((subtypep type 'double-float)
           (if (sb-xc:<= most-negative-double-float val most-positive-double-float)
               (coerce val type)
               (if (sb-xc:< val most-negative-double-float)
                   most-negative-double-float most-positive-double-float)))
          ((or (subtypep type 'single-float) (subtypep type 'float))
           ;; coerce to float returns a single-float
           (if (sb-xc:<= most-negative-single-float val most-positive-single-float)
               (coerce val type)
               (if (sb-xc:< val most-negative-single-float)
                   most-negative-single-float most-positive-single-float)))
          (t (coerce val type))))))

;;; Convert a numeric-type object to an interval object.
(defun numeric-type->interval (x &optional integer)
  (declare (type numeric-type x))
  (let ((low (numeric-type-low x))
        (high (numeric-type-high x)))
    (make-interval :low (cond ((not integer)
                               low)
                              ((consp low)
                               (let ((low (car low)))
                                 (unless (and (floatp low)
                                              (float-infinity-or-nan-p low))
                                   (1+ (floor low)))))
                              (low
                               (unless (and (floatp low)
                                            (float-infinity-or-nan-p low))
                                 (ceiling low))))
                   :high (cond ((not integer)
                                high)
                               ((consp high)
                                (let ((high (car high)))
                                  (unless (and (floatp high)
                                               (float-infinity-or-nan-p high))
                                    (1- (ceiling high)))))
                               (high
                                (unless (and (floatp high)
                                             (float-infinity-or-nan-p high))
                                  (floor high)))))))

(defun type-approximate-interval (type &optional integer)
  (declare (type ctype type))
  (let ((types (prepare-arg-for-derive-type type))
        (result nil)
        complex)
    (dolist (type types)
      (let ((type (typecase type
                    (member-type type
                     (convert-member-type type))
                    (intersection-type
                     (find-if #'numeric-type-p
                              (intersection-type-types type)))
                    (t
                     type))))
        (unless (numeric-type-p type)
          (return-from type-approximate-interval (values nil nil)))
        (let ((interval (numeric-type->interval type integer)))
          (when (eq (numeric-type-complexp type) :complex)
            (setf complex t))
          (setq result
                (if result
                    (interval-approximate-union result interval)
                    interval)))))
    (values result complex)))

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
    (cond ((and lo (sb-xc:>= (type-bound-number lo) point))
           '+)
          ((and hi (sb-xc:>= point (type-bound-number hi)))
           '-)
          (t
           nil))))

(defun interval-range-info> (x &optional (point 0))
  (declare (type interval x))
  (let ((lo (interval-low x))
        (hi (interval-high x)))
    (cond ((and lo (sb-xc:>= (type-bound-number lo) point))
           '+)
          ((and hi (sb-xc:> point (type-bound-number hi)))
           '-))))

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
(defun interval-contains-p (p interval)
  (declare (type number p)
           (type interval interval))
  ;; Does the interval INTERVAL contain the number P?  This would be a lot
  ;; easier if all intervals were closed!
  (let ((lo (interval-low interval))
        (hi (interval-high interval)))
    (cond ((and lo hi)
           ;; The interval is bounded
           (if (and (sb-xc:<= (type-bound-number lo) p)
                    (sb-xc:<= p (type-bound-number hi)))
               ;; P is definitely in the closure of the interval.
               ;; We just need to check the end points now.
               (cond ((sb-xc:= p (type-bound-number lo))
                      (numberp lo))
                     ((sb-xc:= p (type-bound-number hi))
                      (numberp hi))
                     (t t))
               nil))
          (hi
           ;; Interval with upper bound
           (if (sb-xc:< p (type-bound-number hi))
               t
               (and (numberp hi) (sb-xc:= p hi))))
          (lo
           ;; Interval with lower bound
           (if (sb-xc:> p (type-bound-number lo))
               t
               (and (numberp lo) (sb-xc:= p lo))))
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
           (when (and lo hi (sb-xc:= (type-bound-number lo) (type-bound-number hi)))
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
                   ((and lon hin (sb-xc:= lon hin pn))
                    (and (numberp p) (numberp lo) (numberp hi)))
                   ;; Point matches the low end.
                   ;; [P] [P,?} => TRUE     [P] (P,?} => FALSE
                   ;; (P  [P,?} => TRUE      P) [P,?} => FALSE
                   ;; (P  (P,?} => TRUE      P) (P,?} => FALSE
                   ((and lon (sb-xc:= pn lon))
                    (or (and (numberp p) (numberp lo))
                        (and (consp p) (eq :low bound))))
                   ;; [P] {?,P] => TRUE     [P] {?,P) => FALSE
                   ;;  P) {?,P] => TRUE     (P  {?,P] => FALSE
                   ;;  P) {?,P) => TRUE     (P  {?,P) => FALSE
                   ((and hin (sb-xc:= pn hin))
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
        (make-interval :low (select-bound x-lo y-lo #'sb-xc:< #'sb-xc:>)
                       :high (select-bound x-hi y-hi #'sb-xc:> #'sb-xc:<))))))

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
  (make-interval :low (bound-func #'sb-xc:- (interval-high x) t)
                 :high (bound-func #'sb-xc:- (interval-low x) t)))

;;; Add two intervals.
(defun interval-add (x y)
  (declare (type interval x y))
  (make-interval :low (bound-binop sb-xc:+ (interval-low x) (interval-low y))
                 :high (bound-binop sb-xc:+ (interval-high x) (interval-high y))))

;;; Subtract two intervals.
(defun interval-sub (x y)
  (declare (type interval x y))
  (make-interval :low (bound-binop sb-xc:- (interval-low x) (interval-high y))
                 :high (bound-binop sb-xc:- (interval-high x) (interval-low y))))

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
                  (sb-xc:* (signum (type-bound-number x))
                           (signum (type-bound-number y))))
                 ((or (and (floatp x) (float-infinity-p x))
                      (and (floatp y) (float-infinity-p y)))
                  ;; Infinity times anything is infinity
                  nil)
                 (t
                  ;; General multiply. The result is open if either is open.
                  (bound-binop sb-xc:* x y)))))
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
(defun interval-div (top bot &optional integer)
  (declare (type interval top bot))
  (labels ((interval-div (top bot)
             (flet ((bound-div (x y y-low-p)
                      ;; Compute x/y
                      (cond ((null y)
                             ;; Divide by infinity means result is 0. However,
                             ;; we need to watch out for the sign of the result,
                             ;; to correctly handle signed zeros. We also need
                             ;; to watch out for positive or negative infinity.
                             (cond ((floatp (type-bound-number x))
                                    (if y-low-p
                                        (sb-xc:- (float-sign (type-bound-number x) 0.0))
                                        (float-sign (type-bound-number x) 0.0)))
                                   ((and integer
                                         (not (interval-contains-p 0 top)))
                                    '(0))
                                   (t
                                    0)))
                            ((zerop (type-bound-number y))
                             (if integer
                                 x
                                 ;; Divide by zero means result is infinity
                                 nil))
                            ((and (numberp x) (zerop x))
                             ;; Zero divided by anything is zero, but don't lose the sign
                             (sb-xc:/ x (signum (type-bound-number y))))
                            (t
                             (bound-binop sb-xc:/ x y)))))
               (let ((top-range (interval-range-info top))
                     (bot-range (interval-range-info bot)))
                 (cond ((null bot-range)
                        (if integer
                            (destructuring-bind (bot- bot+) (interval-split 0 bot t t)
                              (let ((r- (interval-div top bot-))
                                    (r+ (interval-div top bot+)))
                                (or (interval-merge-pair r- r+)
                                    (list r- r+))))
                            ;; The denominator contains zero, so anything goes!
                            (make-interval)))
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
                        (bug "excluded case in INTERVAL-DIV")))))))
    (let ((interval (interval-div top bot)))
      (if (consp interval)
          interval
          (let ((low (interval-low interval))
                (high (interval-high interval)))
            (if (and (integerp low)
                     (not (eql low 0))
                     (eql low high))
                ;; Don't return constants, as it will produce an error when divided by 0.
                (if (plusp low)
                    (make-interval :low '(0) :high low)
                    (make-interval :low low :high '(0)))
                interval))))))

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
      (cond ((sb-xc:> (type-bound-number left)
                      (type-bound-number right))
             ;; The intervals definitely overlap, so result is NIL.
             nil)
            ((sb-xc:< (type-bound-number left)
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
    (sb-xc:>= (type-bound-number (interval-low x))
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
         (sb-xc:= (bound (interval-high x)) (bound (interval-low x))
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
  (interval-func (lambda (x) (sb-xc:* x x)) (interval-abs x)))

(defun interval<n (interval n)
  (let ((high (interval-high interval)))
    (and high
         (if (consp high)
             (<= (car high) n)
             (< high n)))))

(defun interval-high<=n (interval n)
  (and interval
       (let ((high (interval-high interval)))
         (and high
              (sb-xc:<= (if (consp high)
                            (car high)
                            high)
                        n)))))

(defun interval-low>=n (interval n)
  (and interval
       (let ((low (interval-low interval)))
         (and low
              (sb-xc:>= (if (consp low)
                            (car low)
                            low)
                        n)))))

;;; Does it contain integers?
(defun interval-ratio-p (interval)
  (let ((low (interval-low interval))
        (high (interval-high interval)))
    (and (or (ratiop low)
             (if (consp low)
                 (setf low (car low))))
         (or (ratiop high)
             (and (consp high)
                  (setf high
                        (if (integerp (car high))
                            (1- (car high))
                            (car high)))))
         (= (floor low) (floor high)))))

(defun interval-float-p (interval)
  (let ((low (interval-low interval))
        (high (interval-high interval)))
    (flet ((fraction-p (x)
             (and (numberp x)
                  (not (integerp x))
                  (not (zerop (nth-value 1 (truncate x)))))))
     (and (or (fraction-p low)
              (if (consp low)
                  (setf low (car low))))
          (or (fraction-p high)
              (and (consp high)
                   (setf high
                         (if (fraction-p (car high))
                             (car high)
                             (1- (car high))))))
          (= (floor low) (floor high))))))

(defun interval-constant-p (interval)
  (let ((low (interval-low interval))
        (high (interval-high interval)))
    (and (numberp low)
         (eql low high)
         low)))

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
             (list
              arg)
             (t
              (list arg))))
         (ignore-hairy-type (type)
           (if (and (intersection-type-p type)
                    (find-if #'hairy-type-p (intersection-type-types type)))
               (find-if-not #'hairy-type-p (intersection-type-types type))
               type)))
    (unless (eq arg *empty-type*)
      ;; Make sure all args are some type of numeric-type. For member
      ;; types, convert the list of members into a union of equivalent
      ;; single-element member-type's.
      (let ((new-args nil))
        (dolist (arg (listify arg))
          (let ((arg (ignore-hairy-type arg)))
            (if (member-type-p arg)
                ;; Run down the list of members and convert to a list of
                ;; member types.
                (mapc-member-type-members
                 (lambda (member)
                   (push (if (numberp member) (make-eql-type member) *empty-type*)
                         new-args))
                 arg)
                (push arg new-args))))
        (unless (member *empty-type* new-args)
          new-args)))))

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
             (setf numeric-type (type-union type numeric-type)))
            (t
             (push type misc-types))))
    (setf numeric-type (sb-kernel::weaken-numeric-type-union *derived-numeric-union-complexity-limit* numeric-type))
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
(defun one-arg-derive-type (arg derive-fun member-fun &optional (ratio-to-rational t))
  (declare (type function derive-fun)
           (type (or null function) member-fun))
  (let ((arg-list (prepare-arg-for-derive-type (lvar-type arg))))
    (when arg-list
      (labels ((deriver (x)
                 (cond
                   ((member-type-p x)
                    (if member-fun
                        (handler-case
                            (specifier-type
                             `(eql ,(funcall member-fun
                                             (first (member-type-members x)))))
                          (arithmetic-error () nil))
                        ;; Otherwise convert to a numeric type.
                        (funcall derive-fun (convert-member-type x))))
                   ((or (numeric-type-p x)
                        (and (not ratio-to-rational)
                             (eq x (specifier-type 'ratio))))
                    (funcall derive-fun x))
                   ((eq x (specifier-type 'ratio))
                    (deriver (specifier-type 'rational))))))
        ;; Run down the list of args and derive the type of each one,
        ;; saving all of the results in a list.
        (let ((results nil))
          (dolist (arg arg-list)
            (let ((result (deriver arg)))
              (cond ((not result)
                     (return-from one-arg-derive-type))
                    ((listp result)
                     (setf results (append results result)))
                    (t
                     (push result results)))))
          (if (rest results)
              (make-derived-union-type results)
              (first results)))))))

;;; Same as ONE-ARG-DERIVE-TYPE, except we assume the function takes
;;; two arguments. DERIVE-FUN takes 3 args in this case: the two
;;; original args and a third which is T to indicate if the two args
;;; really represent the same lvar. This is useful for deriving the
;;; type of things like (* x x), which should always be positive. If
;;; we didn't do this, we wouldn't be able to tell.
(defun two-arg-derive-type (arg1 arg2 derive-fun member-fun &optional (ratio-to-rational t))
  (%two-arg-derive-type (lvar-type arg1) (lvar-type arg2)
                        derive-fun member-fun
                        (same-leaf-ref-p arg1 arg2)
                        ratio-to-rational))

(defun %two-arg-derive-type (arg1-type arg2-type derive-fun member-fun &optional same-leaf (ratio-to-rational t))
  (declare (type function derive-fun member-fun))
  (labels ((numeric-or-ratio-p (x)
             (or (numeric-type-p x)
                 (eq x (specifier-type 'ratio))))
           (deriver (x y same-arg)
             (when ratio-to-rational
               (when (eq x (specifier-type 'ratio))
                 (setf x (specifier-type 'rational)))
               (when (eq y (specifier-type 'ratio))
                 (setf y (specifier-type 'rational))))
             (cond ((and (member-type-p x) (member-type-p y))
                    (let* ((x (first (member-type-members x)))
                           (y (first (member-type-members y)))
                           (result (ignore-errors
                                    (funcall member-fun x y))))
                      (cond ((null result) *empty-type*)
                            ((and (floatp result) (float-nan-p result))
                             (make-numeric-type :class 'float
                                                :format (type-of result)
                                                :complexp :real))
                            (t
                             (specifier-type `(eql ,result))))))
                   ((and (member-type-p x) (numeric-or-ratio-p y))
                    (funcall derive-fun (convert-member-type x) y same-arg))
                   ((and (numeric-or-ratio-p x) (member-type-p y))
                    (funcall derive-fun x (convert-member-type y) same-arg))
                   ((and (numeric-or-ratio-p x) (numeric-or-ratio-p y))
                    (funcall derive-fun x y same-arg))
                   (t
                    *universal-type*)))
           (derive (type1 type2 same-arg)
             (let ((a1 (prepare-arg-for-derive-type type1))
                   (a2 (prepare-arg-for-derive-type type2)))
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
    (derive arg1-type arg2-type same-leaf)))

(defun +-derive-type-aux (x y same-arg)
  (cond ((and (numeric-type-real-p x)
              (numeric-type-real-p y))
         (let* ((x-interval (numeric-type->interval x))
                (y-interval (if same-arg
                                x-interval
                                (numeric-type->interval y)))
                (result (interval-add x-interval y-interval))
                (result-type (numeric-contagion x y)))
           ;; If the result type is a float, we need to be sure to coerce
           ;; the bounds into the correct type.
           (when (eq (numeric-type-class result-type) 'float)
             (setf result (interval-func
                           #'(lambda (x)
                               (coerce-for-bound x (or (numeric-type-format result-type)
                                                       'float)))
                           result)))
           (let ((numeric (make-numeric-type
                           :class (if (and (eq (numeric-type-class x) 'integer)
                                           (eq (numeric-type-class y) 'integer))
                                      ;; The sum of integers is always an integer.
                                      'integer
                                      (numeric-type-class result-type))
                           :format (numeric-type-format result-type)
                           :low (interval-low result)
                           :high (interval-high result))))
             (if (or (and (eq (numeric-type-class x) 'integer)
                          (interval-ratio-p y-interval))
                     (and (eq (numeric-type-class y) 'integer)
                          (interval-ratio-p x-interval)))
                 (type-intersection numeric (specifier-type 'ratio))
                 numeric))))
        ((and (eq x (specifier-type 'ratio))
              (numeric-type-p y)
              (eq (numeric-type-class y) 'integer))
         (specifier-type 'ratio))
        ((and (eq y (specifier-type 'ratio))
              (numeric-type-p x)
              (eq (numeric-type-class x) 'integer))
         (specifier-type 'ratio))
        (t
         (numeric-contagion x y))))

(defoptimizer (+ derive-type) ((x y))
  (two-arg-derive-type x y #'+-derive-type-aux #'sb-xc:+ nil))

(defun --derive-type-aux (x y same-arg)
  (cond ((and (numeric-type-real-p x)
              (numeric-type-real-p y))
         (let* ((x-interval (numeric-type->interval x))
                (y-interval (if same-arg
                                x-interval
                                (numeric-type->interval y)))
                (result
                  ;; (- X X) is always 0.
                  (if same-arg
                      (make-interval :low 0 :high 0)
                      (interval-sub x-interval y-interval)))
                (result-type (numeric-contagion x y)))
           ;; If the result type is a float, we need to be sure to coerce
           ;; the bounds into the correct type.
           (when (eq (numeric-type-class result-type) 'float)
             (setf result (interval-func
                           #'(lambda (x)
                               (coerce-for-bound x (or (numeric-type-format result-type)
                                                       'float)))
                           result)))
           (let ((numeric
                   (make-numeric-type
                    :class (if (and (eq (numeric-type-class x) 'integer)
                                    (eq (numeric-type-class y) 'integer))
                               ;; The difference of integers is always an integer.
                               'integer
                               (numeric-type-class result-type))
                    :format (numeric-type-format result-type)
                    :low (interval-low result)
                    :high (interval-high result))))
             (if (or (and (eq (numeric-type-class x) 'integer)
                          (interval-ratio-p y-interval))
                     (and (eq (numeric-type-class y) 'integer)
                          (interval-ratio-p x-interval)))
                 (type-intersection numeric (specifier-type 'ratio))
                 numeric))))
        ((and (eq x (specifier-type 'ratio))
              (numeric-type-p y)
              (eq (numeric-type-class y) 'integer))
         (specifier-type 'ratio))
        ((and (eq y (specifier-type 'ratio))
              (numeric-type-p x)
              (eq (numeric-type-class x) 'integer))
         (specifier-type 'ratio))
        ((and same-arg
              (eq x (specifier-type 'ratio)))
         (specifier-type '(integer 0 0)))
        (t
         (numeric-contagion x y))))

(defoptimizer (- derive-type) ((x y))
  (two-arg-derive-type x y #'--derive-type-aux #'sb-xc:- nil))

(defun *-derive-type-aux (x y same-arg)
  (cond ((and (numeric-type-real-p x)
              (numeric-type-real-p y))
         (let* ((x-interval (numeric-type->interval x))
                (y-interval (if same-arg
                                x-interval
                                (numeric-type->interval y)))
                (result
                  ;; (* X X) is always positive, so take care to do it right.
                  (if same-arg
                      (interval-sqr x-interval)
                      (interval-mul x-interval y-interval)))
                (result-type (numeric-contagion x y)))
           ;; If the result type is a float, we need to be sure to coerce
           ;; the bounds into the correct type.
           (when (eq (numeric-type-class result-type) 'float)
             (setf result (interval-func
                           #'(lambda (x)
                               (coerce-for-bound x (or (numeric-type-format result-type)
                                                       'float)))
                           result)))
           (let ((numeric
                   (make-numeric-type
                    :class (if (and (eq (numeric-type-class x) 'integer)
                                    (eq (numeric-type-class y) 'integer))
                               ;; The product of integers is always an integer.
                               'integer
                               (numeric-type-class result-type))
                    :format (numeric-type-format result-type)
                    :low (interval-low result)
                    :high (interval-high result))))
             (flet ((ratio-result-p (a a-interval b-interval)
                      (let (ratio)
                        (and (eq (numeric-type-class a) 'integer)
                             (ratiop (setf ratio (interval-constant-p b-interval)))
                             (interval-bounded-p a-interval 'both)
                             ;; Is the integer between two adjecents
                             ;; powers of denominator?
                             (let* ((low (interval-low a-interval))
                                    (high (interval-high a-interval))
                                    (den (denominator ratio))
                                    (rem (nth-value 1 (ceiling low den))))
                               (and (not (zerop rem))
                                    (< high (- low rem))))))))
               (if (or (ratio-result-p x x-interval y-interval)
                       (ratio-result-p y y-interval x-interval))
                   (type-intersection numeric (specifier-type 'ratio))
                   numeric)))))
        ((and same-arg
              (eq x (specifier-type 'ratio)))
         ;; TODO: should be positive, but this result is an
         ;; intersection type which other optimizers do not see.
         (specifier-type 'ratio))
        (t
         (numeric-contagion x y))))

(defoptimizer (* derive-type) ((x y))
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y)))
    (block nil
      (flet ((try-zero (x y)
               (when (and (csubtypep x (specifier-type 'integer))
                          (csubtypep (specifier-type '(eql 0)) x)
                          (csubtypep y (specifier-type '(and integer (not (eql 0))))))
                 (return
                   (let ((result (%two-arg-derive-type (type-intersection x (specifier-type '(and integer (not (eql 0)))))
                                                       y
                                                       #'*-derive-type-aux #'sb-xc:* nil)))
                     (when result
                       (type-union result (specifier-type '(eql 0)))))))))
        ;; If one of the integer arguments is non zero seperate the zero
        ;; result from the rest of the result range.
        (try-zero x-type y-type)
        (try-zero y-type x-type)
        (two-arg-derive-type x y #'*-derive-type-aux #'sb-xc:* nil)))))

(defoptimizer (%signed-multiply-high derive-type) ((x y))
  (two-arg-derive-type x y
                       (lambda (x y same-arg)
                         (let* ((type (*-derive-type-aux x y same-arg))
                                (low (numeric-type-low type))
                                (high (numeric-type-high type)))
                           (when (and low high)
                             (make-numeric-type :class 'integer
                                                :low
                                                (ash low (- sb-vm:n-word-bits))
                                                :high (ash high (- sb-vm:n-word-bits))))))
                       #'sb-xc:*))

(defoptimizer (%multiply-high derive-type) ((x y) node)
  (%signed-multiply-high-derive-type-optimizer node))

(defun /-derive-type-aux (x y same-arg)
  (cond ((and (numeric-type-real-p x)
              (numeric-type-real-p y))
         (let* ((x-interval (numeric-type->interval x))
                (y-interval (if same-arg
                                x-interval
                                (numeric-type->interval y)))
                (result-type (numeric-contagion x y))
                (y-integerp (eq (numeric-type-class y) 'integer))
                (result
                  ;; (/ X X) is always 1, except if X can contain 0. In
                  ;; that case, we shouldn't optimize the division away
                  ;; because we want 0/0 to signal an error.
                  (if (and same-arg
                           (not (interval-contains-p
                                 0 (interval-closure x-interval))))
                      (make-interval :low 1 :high 1)
                      (interval-div x-interval y-interval
                                    (and (memq (numeric-type-class x) '(integer rational))
                                         y-integerp)))))
           (cond ((consp result)
                  (type-union (make-numeric-type :class (numeric-type-class result-type)
                                                 :format (numeric-type-format result-type)
                                                 :low (interval-low (first result))
                                                 :high (interval-high (first result)))
                              (make-numeric-type :class (numeric-type-class result-type)
                                                 :format (numeric-type-format result-type)
                                                 :low (interval-low (second result))
                                                 :high (interval-high (second result)))))
                 (t
                  ;; If the result type is a float, we need to be sure to coerce
                  ;; the bounds into the correct type.
                  (when (eq (numeric-type-class result-type) 'float)
                    (setf result (interval-func
                                  #'(lambda (x)
                                      (coerce-for-bound x (or (numeric-type-format result-type)
                                                              'float)))
                                  result)))
                  (let ((numeric (make-numeric-type :class (numeric-type-class result-type)
                                                    :format (numeric-type-format result-type)
                                                    :low (interval-low result)
                                                    :high (interval-high result))))
                    (if (and y-integerp
                             (interval-ratio-p x-interval))
                        (type-intersection numeric (specifier-type 'ratio))
                        numeric))))))
        ((and (eq x (specifier-type 'ratio))
              (cond (same-arg
                     (specifier-type '(integer 1 1)))
                    ((and (numeric-type-p y)
                          (eq (numeric-type-class y) 'integer))
                     (specifier-type 'ratio)))))
        (t
         (numeric-contagion x y))))

(defoptimizer (/ derive-type) ((x y))
  (two-arg-derive-type x y #'/-derive-type-aux #'sb-xc:/ nil))

(defun ash-derive-type-aux (n-type shift same-arg)
  (declare (ignore same-arg))
  (flet ((ash-outer (n s)
           (when (and (fixnump s)
                      (<= s 64)
                      (> s most-negative-fixnum))
             (ash n s)))
         ;; KLUDGE: The bare 64's here should be related to
         ;; symbolic machine word size values somehow.

         (ash-inner (n s)
           (if (and (fixnump s)
                    (> s most-negative-fixnum))
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
  (one-arg-derive-type int #'lognot-derive-type-aux #'lognot))


(defun %negate-derive-type-aux (type)
  (if (eq type (specifier-type 'ratio))
      type
      (flet ((negate-bound (b)
               (and b
                    (set-bound (sb-xc:- (type-bound-number b))
                               (consp b)))))
        (modified-numeric-type
         type
         :low (negate-bound (numeric-type-high type))
         :high (negate-bound (numeric-type-low type))))))

(defoptimizer (%negate derive-type) ((num))
  (one-arg-derive-type num #'%negate-derive-type-aux #'sb-xc:- nil))

(defun abs-derive-type-aux (type)
  (cond ((eq type (specifier-type 'ratio))
         type)
        ((eq (numeric-type-complexp type) :complex)
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

(defoptimizer (abs derive-type) ((num))
  (one-arg-derive-type num #'abs-derive-type-aux #'abs nil))

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

(defvar *conservative-quotient-bound* t)

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
           (multiple-value-bind (quot conservative)
               (if (and (eql (interval-high divisor-interval) 1)
                        (eql (interval-low divisor-interval) 1))
                   (values number-interval nil)
                   (values (interval-div number-interval
                                         divisor-interval)))
             (let* ((*conservative-quotient-bound* conservative)
                    (quot (truncate-quotient-bound quot)))
               (specifier-type `(integer ,(or (interval-low quot) '*)
                                         ,(or (interval-high quot) '*)))))))))

(defun truncate-derive-type-rem (number-type divisor-type)
  (let* ((rem-type (rem-result-type number-type divisor-type))
         (number-interval (numeric-type->interval number-type))
         (divisor-interval (numeric-type->interval divisor-type))
         (rem (truncate-rem-bound number-interval divisor-interval)))
    (cond ((and (numberp (interval-low divisor-interval))
                (numberp (interval-high divisor-interval))
                (zerop (interval-low divisor-interval))
                (zerop (interval-high divisor-interval)))
           nil)
          ((eq rem-type 'integer)
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
                 ((single-float double-float #+long-float long-float)
                  (values 'float rem-type))
                 (float
                  (values 'float nil))
                 (real
                  (values nil nil)))
             (when (member rem-type '(float single-float double-float
                                            #+long-float long-float))
               (setf rem (interval-func #'(lambda (x)
                                            (coerce-for-bound x rem-type))
                                        rem)))
             (make-numeric-type :class class
                                :format format
                                :low (interval-low rem)
                                :high (interval-high rem)))))))

(defun truncate-derive-type-quot-aux (num div same-arg)
  (declare (ignore same-arg))
  (when (and (numeric-type-real-p num)
             (numeric-type-real-p div))
    (truncate-derive-type-quot num div)))

(defun truncate-derive-type-rem-aux (num div same-arg)
  (declare (ignore same-arg))
  (cond ((not (and (numeric-type-real-p num)
                   (numeric-type-real-p div)))
         nil)
        ;; Floats introduce rounding errors
        ((and (memq (numeric-type-class num) '(integer rational))
              (memq (numeric-type-class div) '(integer rational)))
         (truncate-derive-type-rem num div))
        (t
         (numeric-contagion num div))))

(defoptimizer (truncate derive-type) ((number divisor))
  (let ((quot (two-arg-derive-type number divisor
                                   #'truncate-derive-type-quot-aux #'truncate))
        (rem (two-arg-derive-type number divisor
                                  #'truncate-derive-type-rem-aux #'rem)))
    (when (and quot rem)
      (make-values-type (list quot rem)))))

(defun %unary-truncate-derive-type-aux (number)
  (truncate-derive-type-quot number (specifier-type '(integer 1 1))))

(defoptimizer (%unary-truncate derive-type) ((number))
  (one-arg-derive-type number
                       #'%unary-truncate-derive-type-aux
                       #'truncate))

(defoptimizer (%unary-truncate/single-float derive-type) ((number))
  (one-arg-derive-type number
                       #'%unary-truncate-derive-type-aux
                       #'truncate))

(defoptimizer (%unary-truncate/double-float derive-type) ((number))
  (one-arg-derive-type number
                       #'%unary-truncate-derive-type-aux
                       #'truncate))

(defoptimizer (unary-truncate derive-type) ((number))
  (let* ((one (specifier-type '(integer 1 1)))
         (quot (one-arg-derive-type number
                                    (lambda (x)
                                      (truncate-derive-type-quot-aux x one nil))
                                    #'truncate))
         (rem (one-arg-derive-type number
                                   (lambda (x) (truncate-derive-type-rem-aux x one nil))
                                   (lambda (x) (nth-value 1 (truncate x 1))))))
    (when (and quot rem)
      (make-values-type (list quot rem)))))

(deftransform unary-truncate ((number) (integer))
  '(values number 0))


(defun ftruncate-derive-type-quot (number-type divisor-type)
  ;; The bounds are the same as for truncate. However, the first
  ;; result is a float of some type. We need to determine what that
  ;; type is. Basically it's the more contagious of the two types.
  (let ((q-type (truncate-derive-type-quot number-type divisor-type))
        (format (numeric-type-format
                 (numeric-contagion number-type divisor-type))))
    (make-numeric-type :class 'float
                       :format format
                       :low (coerce-for-bound (numeric-type-low q-type) format)
                       :high (coerce-for-bound (numeric-type-high q-type) format))))

(defun ftruncate-derive-type-quot-aux (n d same-arg)
  (declare (ignore same-arg))
  (when (and (numeric-type-real-p n)
             (numeric-type-real-p d))
    (ftruncate-derive-type-quot n d)))


(defoptimizer (ftruncate derive-type) ((number divisor))
  (let ((quot
          (two-arg-derive-type number divisor
                               #'ftruncate-derive-type-quot-aux #'ftruncate))
        (rem (two-arg-derive-type number divisor
                                  #'truncate-derive-type-rem-aux #'rem)))
    (when (and quot rem)
      (make-values-type (list quot rem)))))

(macrolet ((derive (type)
             `(case (lvar-value mode)
                ,@(loop for mode in '(:round :floor :ceiling :truncate)
                        for fun in '(fround ffloor fceiling ftruncate)
                        collect
                        `(,mode
                          (one-arg-derive-type number
                                               (lambda (type)
                                                 (when (numeric-type-p type)
                                                   (let ((lo (numeric-type-low type))
                                                         (hi (numeric-type-high type)))
                                                     (specifier-type (list ',type
                                                                           (if lo
                                                                               (,fun (type-bound-number lo))
                                                                               '*)
                                                                           (if hi
                                                                               (,fun (type-bound-number hi))
                                                                               '*))))))
                                               (lambda (x)
                                                 (values (,fun x)))))))))
  (defoptimizer (round-single derive-type) ((number mode))
    (derive single-float))
  (defoptimizer (round-double derive-type) ((number mode))
    (derive double-float)))

(defoptimizer (%unary-round derive-type) ((number))
  (one-arg-derive-type number
                       (lambda (n)
                         (block nil
                           (unless (numeric-type-real-p n)
                             (return))
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
                       #'round))

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
                    (number-interval
                      (numeric-type->interval number-type))
                    (rem (,r-name number-interval divisor-interval))
                    (result-type (rem-result-type number-type divisor-type)))
               (multiple-value-bind (class format)
                   (ecase result-type
                     (integer
                      (values 'integer nil))
                     (rational
                      (values 'rational nil))
                     ((single-float double-float #+long-float long-float)
                      (values 'float result-type))
                     (float
                      (values 'float nil))
                     (real
                      (values nil nil)))
                 (when (member result-type '(float single-float double-float
                                             #+long-float long-float))
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
                      (when (and (numeric-type-real-p n)
                                 (numeric-type-real-p d))
                        (,q-aux n d)))
                    (derive-r (num div same-arg)
                      (declare (ignore same-arg))
                      (cond ((not (and (numeric-type-real-p num)
                                       (numeric-type-real-p div)))
                             nil)
                            ;; Floats introduce rounding errors
                            ((and (memq (numeric-type-class num) '(integer rational))
                                  (memq (numeric-type-class div) '(integer rational)))
                             (,r-aux num div))
                            (t
                             (numeric-contagion num div)))))
               (let ((quot (two-arg-derive-type
                            number divisor #'derive-q #',name))
                     (rem (two-arg-derive-type
                           number divisor #'derive-r #'mod)))
                 (when (and quot rem)
                   (make-values-type (list quot rem))))))))))

  (def floor floor-quotient-bound floor-rem-bound)
  (def ceiling ceiling-quotient-bound ceiling-rem-bound))

;;; The quotient for floats depends on the divisor,
;;; make the result conservative, without letting it cross 0
(defmacro conservative-quotient-bound (result direction bound)
  (let ((result-sym (gensym)))
    `(let ((,result-sym ,result))
       (,direction ,result-sym
                   (if (and *conservative-quotient-bound*
                            (floatp ,bound)
                            (/= ,result-sym 0))
                       1
                       0)))))

;;; functions to compute the bounds on the quotient and remainder for
;;; the FLOOR function
(defun floor-quotient-bound (quot)
  ;; Take the floor of the quotient and then massage it into what we
  ;; need.
  (let ((lo (interval-low quot))
        (hi (interval-high quot)))
    (make-interval
     ;; Take the floor of the lower bound. The result is always a
     ;; closed lower bound.
     :low
     (and lo
          (conservative-quotient-bound
           (floor (type-bound-number lo))
           -
           (type-bound-number lo)))
     :high
     (and hi
          (conservative-quotient-bound
           (if (consp hi)
               ;; An open bound. We need to be careful here because
               ;; the floor of '(10.0) is 9, but the floor of
               ;; 10.0 is 10.
               (multiple-value-bind (q r) (floor (first hi))
                 (if (zerop r)
                     (1- q)
                     q))
               ;; A closed bound, so the answer is obvious.
               (floor hi))
           +
           (type-bound-number hi))))))

(defun floor-rem-bound (num div)
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
       ;; The remainder can't be greater than the number
       ;; if it's getting truncated towards zero.
       (when (and (eq (interval-range-info num) '+)
                  (numberp (interval-high num))
                  (interval-contains-p (interval-high num) rem))
         (setf (interval-high rem) (interval-high num)))
       rem))
    (-
     ;; The divisor is always negative.
     (let ((rem (interval-neg (interval-abs div))))
       (setf (interval-high rem) 0)
       (when (numberp (interval-low rem))
         ;; The remainder never contains the lower bound.
         (setf (interval-low rem) (list (interval-low rem))))
       ;; The remainder can't be greater than the number
       ;; if it's getting truncated towards zero.
       (when (and (eq (interval-range-info num) '-)
                  (numberp (interval-low num))
                  (interval-contains-p (interval-low num) rem))
         (setf (interval-low rem) (interval-low num)))
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
    (make-interval
     :low
     (and lo
          (conservative-quotient-bound
           (if (consp lo)
               ;; An open bound. We need to be careful here because
               ;; the ceiling of '(10.0) is 11, but the ceiling of
               ;; 10.0 is 10.
               (multiple-value-bind (q r) (ceiling (first lo))
                 (if (zerop r)
                     (1+ q)
                     q))
               ;; A closed bound, so the answer is obvious.
               (ceiling lo))
           -
           (type-bound-number lo)))
     :high
     ;; Take the ceiling of the upper bound. The result is always a
     ;; closed upper bound.
     (and hi
          (conservative-quotient-bound
           (ceiling (type-bound-number hi))
           +
           (type-bound-number hi))))))

(defun ceiling-rem-bound (num div)
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
       ;; The remainder can't be greater than the number
       ;; if it's getting truncated towards zero.
       (when (and (eq (interval-range-info num) '-)
                  (numberp (interval-low num))
                  (interval-contains-p (interval-low num) rem))
         (setf (interval-low rem) (interval-low num)))
       rem))
    (-
     (let ((rem (interval-abs div)))
       ;; Divisor is always negative. The remainder is positive
       (setf (interval-low rem) 0)
       (when (numberp (interval-high rem))
         ;; The remainder never contains the lower bound.
         (setf (interval-high rem)
               (list (interval-high rem))))
       ;; The remainder can't be greater than the number
       ;; if it's getting truncated towards zero.
       (when (and (eq (interval-range-info num) '+)
                  (numberp (interval-high num))
                  (interval-contains-p (interval-high num) rem))
         (setf (interval-high rem) (interval-high num)))
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
        (floor-rem-bound num div))
       (-
        (ceiling-rem-bound num div))
       (otherwise
        (destructuring-bind (neg pos) (interval-split 0 div t t)
          (interval-merge-pair (truncate-rem-bound num neg)
                               (truncate-rem-bound num pos))))))
    (-
     (case (interval-range-info div)
       (+
        (ceiling-rem-bound num div))
       (-
        (floor-rem-bound num div))
       (otherwise
        (destructuring-bind (neg pos) (interval-split 0 div t t)
          (interval-merge-pair (truncate-rem-bound num neg)
                               (truncate-rem-bound num pos))))))
    (otherwise
     (destructuring-bind (neg pos) (interval-split 0 num t t)
       (interval-merge-pair (truncate-rem-bound neg div)
                            (truncate-rem-bound pos div))))))

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

(defoptimizer (random derive-type) ((bound &optional state))
  (one-arg-derive-type bound #'random-derive-type-aux nil))

;;;; miscellaneous derive-type methods

(defoptimizer (integer-length derive-type) ((x))
  (one-arg-derive-type
   x
   (lambda (x-type)
     ;; If the X is of type (INTEGER LO HI), then the INTEGER-LENGTH
     ;; of X is (INTEGER (MIN lo hi) (MAX lo hi), basically.  Be
     ;; careful about LO or HI being NIL, though.  Also, if 0 is
     ;; contained in X, the lower bound is obviously 0.
     (flet ((min-il (a b)
              (min (integer-length a)
                   (integer-length b)))
            (max-il (a b)
              (max (integer-length a)
                   (integer-length b))))
       (let ((lo (numeric-type-low x-type))
             (hi (numeric-type-high x-type)))
         (cond ((and lo hi)
                (specifier-type `(integer ,(if (<= lo 0 hi)
                                               0
                                               (min-il lo hi))
                                          ,(max-il lo hi))))
               (lo
                (when (> lo 0)
                  (specifier-type `(integer ,(integer-length lo)))))
               (hi
                (when (< hi 0)
                  (specifier-type `(integer ,(integer-length hi)))))))))
   #'integer-length))

(defoptimizer (logcount derive-type) ((x))
  (one-arg-derive-type
   x
   (lambda (x-type)
     (let ((lo (numeric-type-low x-type))
           (hi (numeric-type-high x-type)))
       (cond ((and lo hi)
              (let ((adjust 0))
                (make-numeric-type :class 'integer
                                   :low
                                   (cond ((<= lo 0 hi)
                                          0)
                                         ((progn
                                            (when (minusp lo)
                                              (psetf lo (lognot hi)
                                                     hi (lognot lo)))
                                            (= (integer-length lo)
                                               (integer-length hi)))
                                          ;; Count the bits that are always the same
                                          (let ((first-diff (integer-length (logxor lo hi))))
                                            (psetf
                                             adjust (logcount (ash lo (- first-diff)))
                                             lo (ldb (byte first-diff 0) lo)
                                             hi (ldb (byte first-diff 0) hi))
                                            (+ adjust
                                               (if (= lo 0)
                                                   0
                                                   1))))
                                         ((= lo 0)
                                          0)
                                         (t
                                          1))
                                   :high
                                   (let ((l (max (integer-length lo)
                                                 (integer-length hi))))
                                     (+ adjust
                                        l
                                        ;; Only one number can have all the bits turned on
                                        (if (or (= hi (1- (ash 1 l)))
                                                (= lo (ash -1 l)))
                                            0
                                            -1))))))
             (lo
              (when (> lo 0)
                (specifier-type `(integer 1))))
             (hi
              (when (< hi -1)
                (specifier-type `(integer 1)))))))
   #'logcount))

(defoptimizer (isqrt derive-type) ((x))
  (one-arg-derive-type
   x
   (lambda (x-type)
     (let* ((lo (numeric-type-low x-type))
            (hi (numeric-type-high x-type))
            (lo-res (if (typep lo 'unsigned-byte)
                        (isqrt lo)
                        0))
            (hi-res (if (typep hi 'unsigned-byte)
                        (isqrt hi)
                        '*)))
       (specifier-type `(integer ,lo-res ,hi-res))))
   #'isqrt))

(defoptimizer (char-code derive-type) ((char))
  (let ((type (type-intersection (lvar-type char) (specifier-type 'character))))
    (cond ((member-type-p type)
           (specifier-type
            `(member
              ,@(loop for member in (member-type-members type)
                      when (characterp member)
                      collect (char-code member)))))
          ((sb-kernel::character-set-type-p type)
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
  (one-arg-derive-type code
                       (lambda (type)
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
                                #-sb-xc-host type))))
                       nil))

(deftransform code-char ((code))
  (splice-fun-args code 'char-code 1)
  'code)

(deftransform char-code ((char))
  (splice-fun-args char 'code-char 1)
  'char)

(deftransform digit-char ((code &optional radix) ((integer 0 9) t))
  (if (or (not radix)
          (interval-< (type-approximate-interval (lvar-type code))
                      (type-approximate-interval (lvar-type radix))))
      `(code-char (+ code #.(char-code #\0)))
      (give-up-ir1-transform)))

(defoptimizer (digit-char-p derive-type) ((code &optional radix))
  (if radix
      (let ((max (type-approximate-interval (lvar-type radix))))
        (when (interval-high max)
          (specifier-type `(or null (mod ,(interval-high max))))))
      (specifier-type '(or null (mod 10)))))

(defoptimizer (values derive-type) ((&rest values))
  (make-values-type (mapcar #'lvar-type values)))

(deftransform digit-char-p ((char &optional (radix 10))
                            ((or base-char
                                 #+(and (not sb-xc-host) sb-unicode) (character-set ((0 . 1632))))
                             &optional (integer 2 10))
                            *
                            :important nil)
  `(let ((digit (- (char-code char) (char-code #\0))))
     (if (< -1 digit radix)
         digit)))

(deftransform digit-char-p ((char radix)
                            ((or base-char
                                 #+(and (not sb-xc-host) sb-unicode) (character-set ((0 . 1632))))
                             (constant-arg (integer 11)))
                            *
                            :important nil)
  `(let* ((code (char-code char))
          (digit (- code (char-code #\0))))
     (if (< -1 digit 10)
         digit
         (let ((weight (- (logior #x20 code) ;; downcase
                          (char-code #\a))))
           (if (< -1 weight (- radix 10))
               (+ weight 10))))))

(defun signum-derive-type-aux (type)
  (cond ((eq type (specifier-type 'ratio))
         (specifier-type '(or (eql 1) (eql -1))))
        ((eq (numeric-type-complexp type) :complex)
         (let* ((format (case (numeric-type-class type)
                          ((integer rational) 'single-float)
                          (t (numeric-type-format type))))
                (bound-format (or format 'float)))
           (make-numeric-type :class 'float
                              :format format
                              :complexp :complex
                              :low (coerce -1 bound-format)
                              :high (coerce 1 bound-format))))
        (t
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
                                         :low (sb-xc:- zero) :high zero)))
           (let ((result
                   (case range-info
                     (+ (if contains-0-p (type-union plus zero) plus))
                     (- (if contains-0-p (type-union minus zero) minus))
                     (t (type-union minus zero plus)))))
             (if (eq (numeric-type-complexp type) :real)
                 result
                 (type-union result (make-numeric-type :class 'float
                                                       :complexp :complex
                                                       :low -1
                                                       :high 1))))))))

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
         (let ((spec (handler-case (%macroexpand bytespec-form env)
                       (error ()
                         (return-from xform (values nil t))))))
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
  ;; (logand (ash num (- posn)) (lognot (ash -1 size)))
  (let* ((shifted (two-arg-derive-type num posn
                                       (lambda (num posn same)
                                         (declare (ignore same))
                                         (ash-derive-type-aux num (%negate-derive-type-aux posn) nil))
                                       (lambda (num posn)
                                         (ash num (- posn)))))
         (minus-one (specifier-type '(eql -1)))
         (mask (one-arg-derive-type size
                                    (lambda (x)
                                      (lognot-derive-type-aux
                                       (ash-derive-type-aux minus-one x nil)))
                                    (lambda (x)
                                      (lognot (ash -1 x))))))
    (when (and shifted mask)
      (%two-arg-derive-type shifted mask #'logand-derive-type-aux #'logand))))

(defoptimizer (%mask-field derive-type) ((size posn num))
  (let ((size-high (nth-value 1 (integer-type-numeric-bounds (lvar-type size))))
        (posn-high (nth-value 1 (integer-type-numeric-bounds (lvar-type posn)))))
    (if (and size-high posn-high
             (<= (+ size-high posn-high) sb-vm:n-word-bits))
        (specifier-type `(unsigned-byte* ,(+ size-high posn-high)))
        (specifier-type 'unsigned-byte))))

(defoptimizer (%dpb derive-type) ((newbyte size posn int))
  ;; (let ((mask (lognot (ash -1 size))))
  ;;        (logior (ash (logand newbyte mask) posn)
  ;;                (logandc2 int (ash mask posn))))
  (block nil
    (let* ((minus-one (specifier-type '(eql -1)))
           (mask
             (or (one-arg-derive-type size
                                      (lambda (x)
                                        (lognot-derive-type-aux
                                         (ash-derive-type-aux minus-one x nil)))
                                      (lambda (x)
                                        (lognot (ash -1 x))))
                 (return)))
           (mask-shifted
             (or (%two-arg-derive-type mask (lvar-type posn)
                                       #'ash-derive-type-aux #'ash)
                 (return)))
           (int
             (or (%two-arg-derive-type (lvar-type int) mask-shifted
                                       (lambda (int mask same)
                                         (declare (ignore same))
                                         (%two-arg-derive-type
                                          int
                                          (lognot-derive-type-aux mask)
                                          #'logand-derive-type-aux
                                          #'logand))
                                       (lambda (int mask)
                                         (logandc2 int mask)))
                 (return)))
           (new-masked (or (%two-arg-derive-type mask (lvar-type newbyte)
                                                 #'logand-derive-type-aux #'logand)
                           (return)))
           (new
             (or (%two-arg-derive-type new-masked (lvar-type posn)
                                       #'ash-derive-type-aux #'ash)
                 (return))))
      (%two-arg-derive-type int new #'logior-derive-type-aux #'logior))))

(defoptimizer (%deposit-field derive-type) ((newbyte size posn int))
  ;; (let ((mask (ash (lognot (ash -1 size)) posn)))
  ;;   (logior (logand newbyte mask)
  ;;           (logandc2 int mask)))
  (block nil
    (let* ((minus-one (specifier-type '(eql -1)))
           (mask
             (or (two-arg-derive-type size posn
                                      (lambda (size posn same)
                                        (declare (ignore same))
                                        (ash-derive-type-aux
                                         (lognot-derive-type-aux
                                          (ash-derive-type-aux minus-one size nil))
                                         posn nil))
                                      (lambda (x)
                                        (lognot (ash -1 x))))
                 (return)))
           (int
             (or (%two-arg-derive-type (lvar-type int) mask
                                       (lambda (int mask same)
                                         (declare (ignore same))
                                         (%two-arg-derive-type
                                          int
                                          (lognot-derive-type-aux mask)
                                          #'logand-derive-type-aux
                                          #'logand))
                                       (lambda (int mask)
                                         (logandc2 int mask)))
                 (return)))
           (new (or (%two-arg-derive-type mask (lvar-type newbyte)
                                          #'logand-derive-type-aux #'logand)
                    (return))))
      (%two-arg-derive-type int new #'logior-derive-type-aux #'logior))))

(deftransform %ldb ((size posn int) (fixnum fixnum integer) word :node node)
  "convert to inline logical operations"
  (let* ((size-max (nth-value 1 (integer-type-numeric-bounds (lvar-type size))))
         (posn-max (nth-value 1 (integer-type-numeric-bounds (lvar-type posn))))
         (width (and size-max posn-max
                     (+ size-max posn-max))))
    (cond
      ((or (csubtypep (lvar-type int) (specifier-type 'sb-vm:signed-word))
           (csubtypep (lvar-type int) (specifier-type 'word)))
       `(logandc2 (ash int (- posn))
                  (ash -1 size)))
      ((cond ((not width) nil)
             ((<= width sb-vm:n-fixnum-bits)
              `(%ldb size posn (mask-signed-field sb-vm:n-fixnum-bits int)))
             ((<= width sb-vm:n-word-bits)
              `(%ldb size posn (logand int most-positive-word)))))
      (t
       (delay-ir1-transform node :ir1-phases)
       (give-up-ir1-transform "not a word-sized integer")))))

(deftransform %mask-field ((size posn int) ((integer 0 #.sb-vm:n-word-bits) fixnum integer) word)
  "convert to inline logical operations"
  `(logand int (ash (ash ,most-positive-word (- size ,sb-vm:n-word-bits)) posn)))

;;; Note: for %DPB and %DEPOSIT-FIELD, we can't use
;;;   (OR (SIGNED-BYTE N) (UNSIGNED-BYTE N))
;;; as the result type, as that would allow result types that cover
;;; the range -2^(n-1) .. 1-2^n, instead of allowing result types of
;;; (UNSIGNED-BYTE N) and result types of (SIGNED-BYTE N).

(deftransform %dpb ((new size posn int) ((constant-arg integer) (constant-arg integer) t t) * :important nil)
  (let* ((new (lvar-value new))
         (size (lvar-value size))
         (cut (ldb (byte size 0) new)))
    (cond ((/= new cut)
           `(%dpb ,cut ,size posn int))
          ((not (csubtypep (lvar-type posn) (specifier-type `(integer 0 (,(- sb-vm:n-fixnum-bits size))))))
           (give-up-ir1-transform))
          ((= (logcount new) size)
           (let ((uses (lvar-uses int)))
             ;; Move the cast after the ash for cast-externally-checkable-p to work.
             (when (cast-p uses)
               (delete-cast uses)))
           `(logior (ash new posn)
                    (the integer int)))
          ((zerop new)
           (let ((uses (lvar-uses int)))
             (when (cast-p uses)
               (delete-cast uses)))
           `(logandc2 int
                      (ash (ldb (byte size 0) -1) posn)))

          (t
           (give-up-ir1-transform)))))

(deftransform %dpb ((new size posn int) (:or (* word)
                                             (* sb-vm:signed-word)) * :node node)
  "convert to inline logical operations"
  (delay-ir1-transform node :ir1-phases)
  (or (and (constant-lvar-p size)
           (constant-lvar-p new)
           (let* ((size (lvar-value size))
                  (new (ldb (byte size 0) (lvar-value new))))
             (cond ((zerop new)
                    `(logandc2 int
                               (ash (ldb (byte size 0) -1) posn)))
                   ((= (logcount new) size)
                    `(logior int
                             (ash new posn))))))
      `(let ((mask (ldb (byte size 0) -1)))
         (logior (ash (logand new mask) posn)
                 (logandc2 int (ash mask posn))))))

(deftransform %deposit-field ((new size posn int) (:or (* word)
                                                       (* sb-vm:signed-word)))
  "convert to inline logical operations"
  `(let ((mask (ash (ldb (byte size 0) -1) posn)))
     (logior (logand new mask)
             (logand int (lognot mask)))))

(defoptimizer (mask-signed-field derive-type) ((size x))
  (let ((size (lvar-type size)))
    (if (numeric-type-p size)
        (let ((size-high (numeric-type-high size)))
          (if (and size-high (<= 1 size-high sb-vm:n-word-bits))
              (specifier-type `(signed-byte ,size-high))
              *universal-type*))
        *universal-type*)))

;;; Rightward ASH

(when-vop-existsp (:translate sb-kernel:%ash/right)
  (defun %ash/right (integer amount)
    (ash integer (- amount)))

  (deftransform ash ((integer amount) (sb-vm:signed-word (integer * 0)) *
                     :important nil)
    "Convert ASH of signed word to %ASH/RIGHT"
    (when (constant-lvar-p amount)
      (give-up-ir1-transform))
    (let ((use (lvar-uses amount)))
      (cond ((and (combination-p use)
                  (eql '%negate (lvar-fun-name (combination-fun use))))
             (splice-fun-args amount '%negate 1)
             `(lambda (integer amount)
                (declare (type unsigned-byte amount))
                (%ash/right integer (if (>= amount ,sb-vm:n-word-bits)
                                        ,(1- sb-vm:n-word-bits)
                                        amount))))
            (t
             `(%ash/right integer (if (<= amount ,(- sb-vm:n-word-bits))
                                      ,(1- sb-vm:n-word-bits)
                                      (- amount)))))))

  (deftransform ash ((integer amount) (word (integer * 0)) *
                     :important nil)
    "Convert ASH of word to %ASH/RIGHT"
    (when (constant-lvar-p amount)
      (give-up-ir1-transform))
    (let ((use (lvar-uses amount)))
      (cond ((and (combination-p use)
                  (eql '%negate (lvar-fun-name (combination-fun use))))
             (splice-fun-args amount '%negate 1)
             `(lambda (integer amount)
                (declare (type unsigned-byte amount))
                (if (>= amount ,sb-vm:n-word-bits)
                    0
                    (%ash/right integer amount))))
            (t
             `(if (<= amount ,(- sb-vm:n-word-bits))
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
             `(ash integer ,(- sb-vm:n-word-bits)))
            (t
             (give-up-ir1-transform)))))

  (defun %ash/right-derive-type-aux (n-type shift same-arg)
    (declare (ignore same-arg))
    (or (and (or (csubtypep n-type (specifier-type 'sb-vm:signed-word))
                 (csubtypep n-type (specifier-type 'word)))
             (csubtypep shift (specifier-type `(mod ,sb-vm:n-word-bits)))
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
    (two-arg-derive-type n shift #'%ash/right-derive-type-aux #'%ash/right)))

(defmacro combination-typed-p (node name &rest types)
  (labels ((gen (type)
             (typecase type
               ((cons (eql :or))
                `(or ,@(loop for type in (cdr type)
                             collect
                             (gen type))))
               ((cons (eql :and))
                `(and ,@(loop for type in (cdr type)
                              collect
                              (gen type))))
               ((cons (eql :not))
                `(not ,(gen (cadr type))))
               (t
                `(csubtypep (lvar-type arg) (specifier-type ',type))))))
    `(and (combination-p ,node)
          (eql (lvar-fun-name (combination-fun ,node)) ',name)
          (let ((args (combination-args ,node)))
            ,@(loop for type in types
                    collect
                    `(let ((arg (pop args)))
                       (and arg
                            ,(gen type))))))))

(when-vop-existsp (:translate ash-inverted)
  (defun ash-inverted (integer amount)
    (ash integer (- amount)))

  (deftransform ash ((integer amount) (word t) *
                     :important nil :node node)
    (when (constant-lvar-p amount)
      (give-up-ir1-transform))
    (delay-ir1-transform node :ir1-phases)
    (let ((use (lvar-uses amount))
          (result-type (single-value-type (node-derived-type node)))
          (dest (node-dest node))
          truly-type)
      (unless (csubtypep result-type (specifier-type 'word))
        (cond ((or (combination-typed-p dest mask-signed-field (eql #. (1- sb-vm:n-word-bits)))
                   (combination-typed-p dest logand t word))
               (setf truly-type t))
              (t
               (give-up-ir1-transform))))
      (cond ((combination-typed-p use %negate (:and (:not (integer * 0))
                                                    (:or word
                                                         sb-vm:signed-word)))
             (splice-fun-args amount '%negate 1)
             (if truly-type
                 `(truly-the word (ash-inverted integer amount))
                 `(ash-inverted integer amount)))
            (t
             (give-up-ir1-transform)))))

  (deftransform ash ((integer amount) (sb-vm:signed-word t) *
                     :important nil :node node)
    (when (constant-lvar-p amount)
      (give-up-ir1-transform))
    (delay-ir1-transform node :ir1-phases)
    (let ((use (lvar-uses amount))
          (result-type (single-value-type (node-derived-type node)))
          (dest (node-dest node))
          truly-type)
      (unless (csubtypep result-type (specifier-type 'sb-vm:signed-word))
        (cond ((or (combination-typed-p dest mask-signed-field (eql #.(1- sb-vm:n-word-bits)))
                   (combination-typed-p dest logand t word))
               (setf truly-type t))
              (t
               (give-up-ir1-transform))))
      (cond ((combination-typed-p use %negate (:and (:not (integer * 0))
                                                    (:or word
                                                         sb-vm:signed-word)))
             (splice-fun-args amount '%negate 1)
             (if truly-type
                 `(truly-the sb-vm:signed-word (ash-inverted integer amount))
                 `(ash-inverted integer amount)))
            (t
             (give-up-ir1-transform))))))

;;; Not declaring it as actually being RATIO because it is used as one
;;; of the legs in the EXPT transform below and that may result in
;;; some unwanted type conflicts, e.g. (random (expt 2 (the integer y)))
(declaim (ftype (sfunction (integer) rational) reciprocate))
(defun reciprocate (x)
  (declare (optimize (safety 0)))
  (%make-ratio 1 x))

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

;;; Handle the case of a constant BOOLE-CODE.
(deftransform boole ((op x y) * *)
  "convert to inline logical operations"
  (unless (constant-lvar-p op)
    (give-up-ir1-transform "BOOLE code is not a constant."))
  (let ((control (lvar-value op)))
    (case control
      (#.boole-clr 0)
      (#.boole-set -1)
      (#.boole-1 'x)
      (#.boole-2 'y)
      (#.boole-c1 '(lognot x))
      (#.boole-c2 '(lognot y))
      (#.boole-and '(logand x y))
      (#.boole-ior '(logior x y))
      (#.boole-xor '(logxor x y))
      (#.boole-eqv '(logeqv x y))
      (#.boole-nand '(lognand x y))
      (#.boole-nor '(lognor x y))
      (#.boole-andc1 '(logandc1 x y))
      (#.boole-andc2 '(logandc2 x y))
      (#.boole-orc1 '(logorc1 x y))
      (#.boole-orc2 '(logorc2 x y))
      (t
       (abort-ir1-transform "~S is an illegal control arg to BOOLE."
                            control)))))

;;;; converting special case multiply/divide to shifts

;;; If arg is a constant power of two, turn * into a shift.
(deftransform * ((x y) (integer (constant-arg unsigned-byte)) * :node node)
  "convert x*2^k to shift"
  ;; Delay to make sure the surrounding casts are apparent.
  (delay-ir1-transform node :ir1-phases)
  (let* ((type (single-value-type (node-asserted-type node)))
         (y (lvar-value y))
         (len (1- (integer-length y))))
    (unless (or (not (csubtypep (lvar-type x) (specifier-type '(or word sb-vm:signed-word))))
                (csubtypep type (specifier-type 'word))
                (csubtypep type (specifier-type 'sb-vm:signed-word))
                (>= len sb-vm:n-word-bits))
      (give-up-ir1-transform))
    (unless (= y (ash 1 len))
      (give-up-ir1-transform))
    `(ash x ,len)))

;;; * deals better with ASH that overflows
(deftransform ash ((integer amount) ((or word sb-vm:signed-word)
                                     (constant-arg (integer 1 *))) *
                   :important nil
                   :node node)
  ;; Give modular arithmetic optimizers a chance
  (delay-ir1-transform node :ir1-phases)
  (let ((type (single-value-type (node-asserted-type node)))
        (shift (lvar-value amount)))
    (when (or (csubtypep type (specifier-type 'word))
              (csubtypep type (specifier-type 'sb-vm:signed-word))
              (>= shift sb-vm:n-word-bits))
      (give-up-ir1-transform))
    `(* integer ,(ash 1 shift))))

(defun cast-or-check-bound-type (node &optional fixnum)
  (multiple-value-bind (dest lvar) (immediately-used-let-dest (node-lvar node) node t)
    (cond ((cast-p dest)
           (and (cast-type-check dest)
                (single-value-type (cast-type-to-check dest))))
          ((and (combination-p dest)
                (equal (combination-fun-debug-name dest) '(transform-for check-bound))
                (eq (third (combination-args dest)) lvar))
           (if fixnum
               (specifier-type 'index)
               (specifier-type 'sb-vm:signed-word))))))

(defun overflow-transform (name x y node &optional (swap t))
  (unless (node-lvar node)
    (give-up-ir1-transform))
  (delay-ir1-transform node :ir1-phases)
  (let ((type (single-value-type (node-derived-type node))))
    (when (or (csubtypep type (specifier-type 'word))
              (csubtypep type (specifier-type 'sb-vm:signed-word)))
      (give-up-ir1-transform))
    (unless (and (or (csubtypep (lvar-type x) (specifier-type 'word))
                     (csubtypep (lvar-type x) (specifier-type 'sb-vm:signed-word)))
                 (or (csubtypep (lvar-type y) (specifier-type 'word))
                     (csubtypep (lvar-type y) (specifier-type 'sb-vm:signed-word))))
      (give-up-ir1-transform))
    (let* ((vops (fun-info-templates (fun-info-or-lose name)))
           (cast (or (cast-or-check-bound-type node)
                     (give-up-ir1-transform)))
           (result-type (type-intersection type cast)))
      (when (eq result-type *empty-type*)
        (give-up-ir1-transform))
      (flet ((subp (lvar type)
               (cond
                 ((not (constant-type-p type))
                  (csubtypep (lvar-type lvar) type))
                 ((not (constant-lvar-p lvar))
                  nil)
                 (t
                  (let ((value (lvar-value lvar))
                        (type (type-specifier (constant-type-type type))))
                    (if (typep type '(cons (eql satisfies)))
                        (funcall (second type) value)
                        (sb-xc:typep value type)))))))
        (loop with rotate
              for vop in vops
              for (x-type y-type) = (fun-type-required (vop-info-type vop))
              when (and (csubtypep result-type (single-value-type (fun-type-returns (vop-info-type vop))))
                        (neq x-type *universal-type*)
                        (neq y-type *universal-type*)
                        (or (and (subp x x-type)
                                 (subp y y-type))
                            (and swap
                                 (subp y x-type)
                                 (subp x y-type)
                                 (setf rotate t))))
              return `(%primitive ,(vop-info-name vop)
                                  ,@(if rotate
                                        '(y x)
                                        '(x y))
                                  ',(type-specifier cast))
              finally (give-up-ir1-transform))))))

(deftransform * ((x y) ((or word sb-vm:signed-word) (or word sb-vm:signed-word))
                 * :node node :important nil)
  (overflow-transform 'overflow* x y node))

(deftransform + ((x y) ((or word sb-vm:signed-word) (or word sb-vm:signed-word))
                 * :node node :important nil)
  (overflow-transform 'overflow+ x y node))

(deftransform - ((x y) ((or word sb-vm:signed-word) (or word sb-vm:signed-word))
                 * :node node :important nil)
  (overflow-transform 'overflow- x y node nil))

(deftransform ash ((x y) ((or word sb-vm:signed-word) (or word sb-vm:signed-word))
                 * :node node :important nil)
  (overflow-transform 'overflow-ash x y node nil))

(defun overflow-transform-unknown-x (name x y node &optional swap)
  (unless (node-lvar node)
    (give-up-ir1-transform))
  (delay-ir1-transform node :ir1-phases)
  (let ((type (single-value-type (node-derived-type node)))
        (x-type (lvar-type x))
        (y-type (lvar-type y)))
    (when (or (csubtypep type (specifier-type 'word))
              (csubtypep type (specifier-type 'sb-vm:signed-word))
              (if swap
                  (or
                   (csubtypep y-type (specifier-type 'word))
                   (csubtypep y-type (specifier-type 'sb-vm:signed-word)))
                  (or (csubtypep x-type (specifier-type 'word))
                      (csubtypep x-type (specifier-type 'sb-vm:signed-word)))))
      (give-up-ir1-transform))
    (let* ((vops (fun-info-templates (fun-info-or-lose name)))
           (cast (or (cast-or-check-bound-type node t)
                     (give-up-ir1-transform)))
           (result-type (type-intersection type cast)))
      (when (eq result-type *empty-type*)
        (give-up-ir1-transform))
      (multiple-value-bind (cast-low cast-high) (integer-type-numeric-bounds
                                                 (type-intersection (specifier-type 'integer) cast))
        (unless (and (fixnump cast-low)
                     (fixnump cast-high))
          (give-up-ir1-transform))
        (multiple-value-bind (y-low y-high) (if swap
                                                (integer-type-numeric-bounds x-type)
                                                (integer-type-numeric-bounds y-type))
          (unless (and (fixnump y-low)
                       (fixnump y-high))
            (give-up-ir1-transform))
          (let ((distance-low (- cast-low (1- most-negative-fixnum)))
                (distance-high (- cast-high (1+ most-positive-fixnum))))
            (unless (ecase name
                      (overflow+
                       (and (> y-low distance-high)
                            (< y-high distance-low)))
                      (overflow-
                       (if swap
                           (and (< y-high (+ (1+ most-positive-fixnum) cast-low))
                                (> y-low (+ (1- most-negative-fixnum) cast-high)))
                           (and (> (- y-high) distance-high)
                                (< (- y-low) distance-low))))
                      (overflow*
                       (or (> y-low 0)
                           (< y-high -1))))
              (give-up-ir1-transform)))
          (flet ((subp (lvar type)
                   (cond
                     ((not (constant-type-p type))
                      (csubtypep (lvar-type lvar) type))
                     ((not (constant-lvar-p lvar))
                      nil)
                     (t
                      (let ((value (lvar-value lvar))
                            (type (type-specifier (constant-type-type type))))
                        (if (typep type '(cons (eql satisfies)))
                            (funcall (second type) value)
                            (sb-xc:typep value type)))))))
            (loop for vop in vops
                  for (x-type y-type) = (fun-type-required (vop-info-type vop))
                  when (and (csubtypep result-type (single-value-type (fun-type-returns (vop-info-type vop))))
                            (if swap
                                (eq y-type *universal-type*)
                                (eq x-type *universal-type*))
                            (and (subp x x-type)
                                 (subp y y-type)))
                  return `(%primitive ,(vop-info-name vop)
                                      x y
                                      ',(type-specifier cast))
                  finally (give-up-ir1-transform))))))))

(deftransform + ((x y) (t (or word sb-vm:signed-word))
                 * :node node :important nil)
  (overflow-transform-unknown-x 'overflow+ x y node))

(deftransform + ((y x) ((or word sb-vm:signed-word) t)
                 * :node node :important nil)
  (overflow-transform-unknown-x 'overflow+ x y node))

(deftransform * ((x y) ((not ratio) (or word sb-vm:signed-word))
                 * :node node :important nil)
  (overflow-transform-unknown-x 'overflow* x y node))

(deftransform * ((y x) ((or word sb-vm:signed-word) (not ratio))
                 * :node node :important nil)
  (overflow-transform-unknown-x 'overflow* x y node))

(deftransform - ((x y) (t (or word sb-vm:signed-word))
                 * :node node :important nil)
  (overflow-transform-unknown-x 'overflow- x y node))

(deftransform - ((x y) ((or word sb-vm:signed-word) t)
                 * :node node :important nil)
  (overflow-transform-unknown-x 'overflow- x y node t))

(defun overflow-transform-1 (name x node)
  (unless (node-lvar node)
    (give-up-ir1-transform))
  (delay-ir1-transform node :ir1-phases)
  (let ((type (single-value-type (node-derived-type node))))
    (when (or (csubtypep type (specifier-type 'word))
              (csubtypep type (specifier-type 'sb-vm:signed-word)))
      (give-up-ir1-transform))
    (unless (and (or (csubtypep (lvar-type x) (specifier-type 'word))
                     (csubtypep (lvar-type x) (specifier-type 'sb-vm:signed-word))))
      (give-up-ir1-transform))
    (let* ((vops (fun-info-templates (fun-info-or-lose name)))
           (cast (or (cast-or-check-bound-type node)
                     (give-up-ir1-transform)))
           (result-type (type-intersection type cast)))
      (loop for vop in vops
            for (x-type) = (fun-type-required (vop-info-type vop))
            when (and (csubtypep result-type (single-value-type (fun-type-returns (vop-info-type vop))))
                      (neq x-type *universal-type*)
                      (csubtypep (lvar-type x) x-type))
            return `(%primitive ,(vop-info-name vop) x ',(type-specifier cast))
            finally (give-up-ir1-transform)))))

(deftransform %negate ((x) ((or word sb-vm:signed-word))
                       * :node node :important nil)
  (overflow-transform-1 'overflow-negate x node))

(defun template-translates (fun-name args result-type)
  (let ((vops (fun-info-templates (fun-info-or-lose fun-name))))
    (flet ((subp (lvar type)
             (cond
               ((not (constant-type-p type))
                (csubtypep (lvar-type lvar) type))
               ((not (constant-lvar-p lvar))
                nil)
               (t
                (let ((value (lvar-value lvar))
                      (type (type-specifier (constant-type-type type))))
                  (if (typep type '(cons (eql satisfies)))
                      (funcall (second type) value)
                      (sb-xc:typep value type)))))))
      (loop for vop in vops
            for params = (fun-type-required (vop-info-type vop))
            thereis (and (= (length args)
                            (length params))
                         (csubtypep result-type (single-value-type (fun-type-returns (vop-info-type vop))))
                         (loop for param in params
                               for arg in args
                               always (subp arg param)))))))

(deftransform floor ((number divisor) (integer integer) * :node node)
  (let ((truncate-type (truncate-derive-type-optimizer node)))
    (if (template-translates 'truncate (combination-args node) (single-value-type truncate-type))
        (let* ((rem-int (type-approximate-interval (lvar-type divisor)))
               (rem-type (let* ((low (interval-low rem-int))
                                (high (interval-high rem-int))
                                (rem-low (cond ((not low) '*)
                                               ((>= low 0)
                                                0)
                                               (t
                                                (1+ low))))
                                (rem-high (cond ((not high) '*)
                                                ((<= high 0)
                                                 0)
                                                (t
                                                 (1- high)))))
                           `(integer ,rem-low ,rem-high)))
               (n-int (type-approximate-interval (lvar-type number)))
               (quot-type (let* ((low (interval-low n-int))
                                 (high (interval-high n-int)))
                            (if (and low high)
                                (let ((max (max (abs low) (abs high))))
                                  `(integer ,(- max) ,max))
                                t))))
          `(multiple-value-bind (tru rem) (truncate number divisor)
             (if (if (minusp divisor)
                     (> rem 0)
                     (< rem 0))
                 (values (truly-the ,quot-type (1- tru)) (truly-the ,rem-type (+ rem divisor)))
                 (values tru (truly-the ,rem-type rem)))))
        (give-up-ir1-transform))))

(deftransform ceiling ((number divisor) (integer integer) * :node node)
  (let ((truncate-type (truncate-derive-type-optimizer node)))
    (if (template-translates 'truncate (combination-args node) (single-value-type truncate-type))
        (let* ((rem-int (type-approximate-interval (lvar-type divisor)))
               (rem-type (let* ((low (interval-low rem-int))
                                (high (interval-high rem-int))
                                (rem-low (cond ((not high) '*)
                                               ((<= high 1)
                                                0)
                                               (t (- 1 high))))
                                (rem-high (cond ((not low) '*)
                                                ((>= low -1)
                                                 0)
                                                (t
                                                 (- -1 low)))))
                           `(integer ,rem-low ,rem-high)))
               (n-int (type-approximate-interval (lvar-type number)))
               (quot-type (let* ((low (interval-low n-int))
                                 (high (interval-high n-int)))
                            (if (and low high)
                                (let ((max (max (abs low) (abs high))))
                                  `(integer ,(- max) ,max))
                                t))))
          `(multiple-value-bind (tru rem) (truncate number divisor)
             (if (if (minusp divisor)
                     (< rem 0)
                     (> rem 0))
                 (values (truly-the ,quot-type (+ tru 1))
                         (truly-the ,rem-type (- rem divisor)))
                 (values tru
                         (truly-the ,rem-type rem)))))
        (give-up-ir1-transform))))

(define-source-transform rem (number divisor)
  `(nth-value 1 (truncate ,number ,divisor)))

(define-source-transform mod (number divisor)
  `(nth-value 1 (floor ,number ,divisor)))

(deftransform ceiling ((number divisor) ((real (0) (1)) (integer * (0))) * :important nil)
  `(values 0 number))
(deftransform ceiling ((number divisor) ((real (-1) (0)) (integer (0) *)) * :important nil)
  `(values 0 number))

(deftransform floor ((number divisor) ((real (-1) (0)) (integer * (0))) * :important nil)
  `(values 0 number))
(deftransform floor ((number divisor) ((real (0) (1)) (integer (0) *)) * :important nil)
  `(values 0 number))

(deftransform truncate ((number divisor) ((and (real (-1) (1)) (not (eql -0d0)) (not (eql -0f0)))
                                          (and integer (not (eql 0))))
                        * :important nil)
  `(values 0 number))

;;; If arg is a constant power of two, turn FLOOR into a shift and
;;; mask. If CEILING, add in (1- (ABS Y)), do FLOOR and correct a
;;; remainder.
(flet ((frob (y ceil-p)
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
  (deftransform floor ((x y) (integer (constant-arg integer)) *)
    "convert division by 2^k to shift"
    (frob y nil))
  (deftransform ceiling ((x y) (integer (constant-arg integer)) *)
    "convert division by 2^k to shift"
    (frob y t)))

;;; If arg is a constant power of two, turn TRUNCATE into a shift and mask.
(deftransform truncate ((x y) (integer (constant-arg integer)) *  :result result :node node)
  "convert division by 2^k to shift"
  (let* ((y (lvar-value y))
         (y-abs (abs y))
         (len (1- (integer-length y-abs))))
    (unless (and (> y-abs 0) (= y-abs (ash 1 len)))
      (give-up-ir1-transform))
    (delay-ir1-transform node :ir1-phases)
    (let* ((rem (mv-bind-dest result 1))
           (zerop (combination-matches 'eq '(* 0) rem)))
      (let ((shift (- len))
            (mask (1- y-abs)))
        (cond (zerop
               (setf (node-derived-type node)
                     (values-specifier-type '(values integer unsigned-byte &optional)))
               (erase-lvar-type result)
               `(values
                 (values (truncate x y))
                 (logand x ,mask)))
              ((when-vop-existsp (:named sb-vm::truncate/signed-power-of-two)
                 (and (csubtypep (lvar-type x) (specifier-type 'sb-vm:signed-word))
                      (not (csubtypep (lvar-type x) (specifier-type 'word)))))
               (give-up-ir1-transform))
              (t
               `(if (minusp x)
                    (values ,(if (minusp y)
                                 `(ash (- x) ,shift)
                                 `(- (ash (- x) ,shift)))
                            (- (logand (- x) ,mask)))
                    (values ,(if (minusp y)
                                 `(- (ash x ,shift))
                                 `(ash x ,shift))
                            (logand x ,mask)))))))))

;;; Floats could be transformed if we had some declaration to ignore NaNs
(deftransform truncate ((x y) (rational (or (rational (0) *)
                                            (rational * (0))))
                        *
                        :important nil)
  (if (same-leaf-ref-p x y)
      `(values 1 0)
      (give-up-ir1-transform)))

(defoptimizer (truncate constraint-propagate)
    ((x y) node gen)
  (when (csubtypep (lvar-type y) (specifier-type 'rational))
    (let ((var (ok-lvar-lambda-var y gen)))
      (when var
        (list (list 'typep var (specifier-type '(eql 0)) t))))))

(defoptimizer (/ constraint-propagate)
    ((x y) node gen)
  (when (csubtypep (lvar-type y) (specifier-type 'rational))
    (let ((var (ok-lvar-lambda-var y gen)))
      (when var
        (list (list 'typep var (specifier-type '(eql 0)) t))))))

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
                   (expt-2-n+l (expt 2 (+ sb-vm:n-word-bits l)))
                   (m-low (truncate expt-2-n+l y) (ash m-low -1))
                   (m-high (truncate (+ expt-2-n+l
                                        (ash expt-2-n+l (- precision)))
                                     y)
                           (ash m-high -1)))
                  ((not (and (< (ash m-low -1) (ash m-high -1))
                             (> shift 0)))
                   (values m-high shift)))))
    (let ((n (expt 2 sb-vm:n-word-bits))
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

(when-vop-existsp (:translate %signed-multiply-high)
 (defun %signed-multiply-high (x y)
   (%signed-multiply-high x y))

 (defun gen-signed-truncate-by-constant-expr (y precision)
   (declare (type sb-vm:signed-word y)
            (type fixnum precision))
   (aver (not (zerop (logand y (1- y)))))
   (labels ((ld (x)
              ;; the floor of the binary logarithm of (positive) X
              (integer-length (1- x)))
            (choose-multiplier (y precision)
              (do* ((l (ld y))
                    (shift l (1- shift))
                    (expt-2-n+l (expt 2 (+ sb-vm:n-word-bits l)))
                    (m-low (truncate expt-2-n+l y) (ash m-low -1))
                    (m-high (truncate (+ expt-2-n+l
                                         (ash expt-2-n+l (- precision)))
                                      y)
                            (ash m-high -1)))
                   ((not (and (< (ash m-low -1) (ash m-high -1))
                              (> shift 0)))
                    (values m-high shift)))))
     (let ((n (expt 2 sb-vm:n-word-bits))
           (n-1 (expt 2 (1- sb-vm:n-word-bits))))
       (multiple-value-bind (m shift) (choose-multiplier (abs y) precision)
         (let ((code
                 (cond ((< m n-1)
                        `(ash (%signed-multiply-high x ,m)
                                 ,(- shift)))
                       (t
                        `(ash (truly-the sb-vm:signed-word
                                         (+ x (%signed-multiply-high x ,(- m n))))
                              ,(- shift))))))
           (if (minusp y)
               `(- (ash x (- 1 sb-vm:n-word-bits)) ,code)
               `(- ,code (ash x (- 1 sb-vm:n-word-bits)))))))))

 (deftransform truncate ((x y) (sb-vm:signed-word
                                (constant-arg sb-vm:signed-word))
                         *
                         :policy (and (> speed compilation-speed)
                                      (> speed space))
                         :node node
                         :important nil)
   "convert integer division to multiplication"
   (delay-ir1-transform node :ir1-phases)
   (let* ((y      (lvar-value y))
          (abs-y  (abs y))
          (x-type (lvar-type x)))
     (multiple-value-bind (precision max-x)
         (if (and (numeric-type-p x-type)
                  (numeric-type-high x-type)
                  (numeric-type-low x-type))
             (values (max (integer-length (numeric-type-high x-type))
                          (integer-length (numeric-type-low x-type)))
                     (max (numeric-type-high x-type)
                          (abs (numeric-type-low x-type))))
             (values (1- sb-vm:n-word-bits)
                     (expt 2 (1- sb-vm:n-word-bits))))
         ;; Division by zero, one or powers of two is handled elsewhere.
         (when (or (zerop (logand abs-y (1- abs-y)))
                   ;; Leave it for the unsigned transform
                   (and (plusp y)
                        (not (types-equal-or-intersect x-type
                                                       (specifier-type
                                                        '(and sb-vm:signed-word
                                                          (not unsigned-byte)))))))
           (give-up-ir1-transform))
      `(let* ((quot (truly-the
                     (integer ,(- (truncate max-x abs-y)) ,(truncate max-x abs-y))
                     ,(gen-signed-truncate-by-constant-expr y precision)))
              (rem (truly-the (integer ,(- 1 abs-y) ,(1- abs-y))
                              (- x (truly-the sb-vm:signed-word (* quot ,y))))))
         (values quot rem))))))

;;; The paper also has this,
;;; but it seems to overflow when adding to X
;; (defun gen-signed-floor-by-constant-expr (y max-x)
;;   (declare (type sb-vm:signed-word y)
;;            (type word max-x))
;;   (let ((trunc (gen-signed-truncate-by-constant-expr y
;;                                                      max-x)))
;;     (let ((y-sign (xsign y)))
;;       `(let* ((x-sign (xsign (logior x (+ x ,y-sign))))
;;               (x (+ ,y-sign (- x-sign) x)))
;;          (truly-the sb-vm:signed-word
;;                     (+ ,trunc
;;                        (logxor x-sign ,y-sign)))))))

(unless-vop-existsp (:translate %multiply-high)
                    (define-source-transform %multiply-high (x y)
                      `(values (sb-bignum:%multiply ,x ,y))))

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
                                     (> speed space))
                        :node node
                        :important nil)
  "convert integer division to multiplication"
  (delay-ir1-transform node :ir1-phases)
  (let* ((y      (lvar-value y))
         (x-type (lvar-type x))
         (max-x  (or (and (numeric-type-p x-type)
                          (numeric-type-high x-type))
                     most-positive-word)))
    ;; Division by zero, one or powers of two is handled elsewhere.
    (when (zerop (logand y (1- y)))
      (give-up-ir1-transform))
    `(let* ((quot (truly-the (integer 0 ,(truncate max-x y))
                             ,(gen-unsigned-div-by-constant-expr y max-x)))
            (rem (truly-the (mod ,y)
                            (- x (* quot ,y)))))
       (values quot rem))))

;;; No-op when Y is greater than X
(deftransform truncate ((x y) (rational rational) * :important nil)
  (flet ((strip (x)
           (if (consp x)
               (car x)
               x)))
    (let* ((x-interval (or (type-approximate-interval (lvar-type x))
                           (give-up-ir1-transform)))
           (x-low (strip (interval-low x-interval)))
           (x-high (strip (interval-high x-interval)))
           (y-interval (or (type-approximate-interval (lvar-type y))
                           (give-up-ir1-transform)))
           (y-low (strip (interval-low y-interval)))
           (y-high (strip (interval-high y-interval)))
           (x-max (and x-low x-high
                       (max (abs x-low) (abs x-high))))
           (y-min (cond ((and y-low
                              (> y-low 0))
                         y-low)
                        ((and y-high
                              (< y-high 0))
                         (abs y-high)))))
      (if (and x-max y-min
               (> y-min x-max))
          `(values 0 x)
          (give-up-ir1-transform)))))


;;;; arithmetic and logical identity operation elimination

;;; Flush calls to various arith functions that convert to the
;;; identity function or a constant.
(macrolet ((def (name identity result)
             `(deftransform ,name ((x y) (t (constant-arg (member ,identity))) *)
                "fold identity operations"
                ',result)))
  (def ash 0 x)
  (def logand -1 x)
  (def logand 0 0)
  (def logior 0 x)
  (def logior -1 -1)
  (def logxor -1 (lognot x))
  (def logxor 0 x)
  (def logandc2 0 x))

(defun least-zero-bit (x)
  (and (/= x -1)
       (1- (integer-length (logxor x (1+ x))))))

(deftransform logand ((x y) (t (constant-arg integer)) *)
  "fold identity operation"
  (let* ((y (lvar-value y))
         (width (or (least-zero-bit y) '*)))
    (unless (and (neq width 0) ; (logand x 0) handled elsewhere
                 (csubtypep (lvar-type x)
                            (specifier-type `(unsigned-byte ,width))))
      (give-up-ir1-transform))
    'x))

(deftransform logand ((x y) (t (constant-arg integer)) word
                      :node node :important nil)
  ;; Reduce constant width
  (let* ((high (interval-high (type-approximate-interval (single-value-type (node-asserted-type node)))))
         (mask (lvar-value y))
         (cut (ldb (byte (integer-length high) 0) mask)))
    (if (= cut mask)
        (give-up-ir1-transform)
        `(logand x ,cut))))

(deftransform logandc2 ((x y) ((constant-arg (eql -1)) t) *)
  `(lognot y))

(deftransform logandc2 ((x y) * * :important nil :node node)
  (delay-ir1-transform node :ir1-phases)
  (if (and (not (or (csubtypep (lvar-type x) (specifier-type 'word))
                    (csubtypep (lvar-type x) (specifier-type 'sb-vm:signed-word))))
           (csubtypep (lvar-type y) (specifier-type 'fixnum))
           (csubtypep (one-arg-derive-type y #'lognot-derive-type-aux #'lognot)
                      (specifier-type 'fixnum)))
      `(logand x (lognot y))
      (give-up-ir1-transform)))

(deftransform mask-signed-field ((size x) ((constant-arg t) t) *)
  "fold identity operation"
  (let ((size (lvar-value size)))
    (cond ((= size 0) 0)
          ((csubtypep (lvar-type x) (specifier-type `(signed-byte ,size)))
           'x)
          (t
           (give-up-ir1-transform)))))

(deftransform logior ((x y) (t (constant-arg integer)) *)
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
(macrolet ((def (operator &key (type 'integer) (folded (list operator)))
             `(deftransform ,operator ((x z) (,type (constant-arg ,type)))
                ,(format nil "associate ~A/~A of constants"
                         operator folded)
                (binding* ((node  (if (lvar-has-single-use-p x)
                                      (lvar-use x)
                                      (give-up-ir1-transform)))
                           (folded (or (and (combination-p node)
                                            (car (memq (lvar-fun-name
                                                        (combination-fun node))
                                                       ',folded)))
                                       (give-up-ir1-transform)))
                           (y   (second (combination-args node)))
                           (nil (or (constant-lvar-p y)
                                    (give-up-ir1-transform)))
                           (y   (lvar-value y)))
                  (unless (typep y ',type)
                    (give-up-ir1-transform))
                  (splice-fun-args x folded 2)
                  `(lambda (x y z)
                     (declare (ignore y z))
                     ;; (operator (folded x y) z)
                     ;; == (operator x (folded z y))
                     (,',operator x (,folded ,(lvar-value z) ,y)))))))
  (def logand)
  (def logior)
  (def logxor)
  (def logtest :folded (logand))
  (def + :type rational :folded (+ -))
  (def * :type rational :folded (* /)))

(deftransform * ((x y) (rational (constant-arg ratio)))
  (let ((y (/ (lvar-value y))))
    (if (integerp y)
        `(/ x ,y)
        (give-up-ir1-transform))))

(deftransform / ((x y) (rational (constant-arg ratio)))
  (let ((y (/ (lvar-value y))))
    (if (integerp y)
        `(* x ,y)
        (give-up-ir1-transform))))

(deftransform mask-signed-field ((width x) ((constant-arg unsigned-byte) t))
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
(defun not-more-contagious (x y)
  (let ((x (lvar-type x))
        (y (lvar-type y)))
    (cond
      ((csubtypep x (specifier-type 'rational)))
      ((csubtypep x (specifier-type 'single-float))
       (csubtypep y (specifier-type 'float)))
      ((csubtypep x (specifier-type 'double-float))
       (csubtypep y (specifier-type 'double-float))))))

(sb-xc:deftype exact-number ()
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
;;; If a signaling nan somehow got here without signaling anything then
;;; why signal now.
(macrolet
    ((def (name result minus-result type)
         `(deftransform ,name ((x y)
                               (,type (constant-arg (member 1 -1))))
            "fold identity operations"
            (if (minusp (lvar-value y)) ',minus-result ',result))))
  (def * x (%negate x) number)
  (def / x (%negate x) number)
  (def expt x (/ 1 x) (or exact-number real))) ;; (expt #c(2d0 2d0) 1) doesn't return #c(2d0 2d0)

(deftransform + ((x y) (number number))
  (cond ((splice-fun-args y '%negate 1 nil)
         `(- x y))
        ((splice-fun-args x '%negate 1 nil)
         `(- y x))
        (t
         (give-up-ir1-transform))))

(deftransform - ((x y) (number number))
  (splice-fun-args y '%negate 1)
  `(+ x y))

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

(deftransform expt ((x y) ((or rational (complex rational)) integer) * :node node)
  (delay-ir1-transform node :ir1-phases)
  `(sb-kernel::intexp x y))

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

(deftransform char-equal ((a b) (base-char base-char) *
                          :policy (>= speed space) :node node)
  "open code"
  (delay-ir1-transform node :constraint)
  '(let* ((ac (char-code a))
          (bc (char-code b))
          (sum (logxor ac bc)))
     (or (zerop sum)
         (when (eql sum #x20)
           (let ((sum (+ ac bc)))
             (or (and (> sum 161) (< sum 213))
                 #-sb-unicode
                 (and (> sum 415) (< sum 461))
                 #-sb-unicode
                 (and (> sum 463) (< sum 477))))))))

#+sb-unicode
(deftransform char-equal ((a b) (base-char character) *
                          :policy (>= speed space) :node node)
  "open code"
  (delay-ir1-transform node :constraint)
  '(let* ((ac (char-code a))
          (bc (char-code b))
          (sum (logxor ac bc)))
     (or (zerop sum)
         (when (eql sum #x20)
           (let ((sum (+ ac bc)))
             (and (> sum 161) (< sum 213)))))))

#+sb-unicode
(deftransform char-equal ((a b) (character base-char) *
                          :policy (>= speed space) :node node)
  "open code"
  (delay-ir1-transform node :constraint)
  '(let* ((ac (char-code a))
          (bc (char-code b))
          (sum (logxor ac bc)))
     (or (zerop sum)
         (when (eql sum #x20)
           (let ((sum (+ ac bc)))
             (and (> sum 161) (< sum 213)))))))

(defun transform-constant-char-equal (a b &optional (op 'char=))
  (let ((char (lvar-value b)))
    (if (both-case-p char)
        (let ((reverse (if (upper-case-p char)
                           (char-downcase char)
                           (char-upcase char))))
          `(or (,op ,a ,char)
               (,op ,a ,reverse)))
        `(,op ,a ,char))))

(deftransform char-equal ((a b) (t (constant-arg character)) *
                          :node node)
  (transform-constant-char-equal 'a b))

(defun no-case-character-type-p (type)
  (and (typep type 'character-set-type)
       (loop for (start . end) in (character-set-type-pairs type)
             always (loop for c from start to end
                          always (not (and #+sb-xc-host
                                           (or (< 31 c 127)
                                               (= c 10))
                                           (both-case-p (code-char c))))))))

(deftransform char-equal ((a b))
  (cond ((same-leaf-ref-p a b) t)
        ((or (no-case-character-type-p (lvar-type a))
             (no-case-character-type-p (lvar-type b)))
         `(char= a b))
        (t
         (give-up-ir1-transform))))

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

;;; If X and Y are the same leaf, then the result is true. Otherwise,
;;; if there is no intersection between the types of the arguments,
;;; then the result is definitely false.
(deftransform char= ((x y) * *)
  "Simple equality transform"
  (let ((use (lvar-uses x))
        arg)
    (declare (ignorable use arg))
    (cond
      ((same-leaf-ref-p x y) t)
      ((not (types-equal-or-intersect (lvar-type x) (lvar-type y)))
       nil)
      ;; Reduce (eq (%instance-ref x i) Y) to 1 instruction
      ;; if possible, but do not defer the memory load unless doing
      ;; so can have no effect, i.e. Y is a constant or provably not
      ;; effectful. For now, just handle constant Y.
      ((and (vop-existsp :translate %instance-ref-eq)
            (constant-lvar-p y)
            (combination-p use)
            (almost-immediately-used-p x use)
            (eql '%instance-ref (lvar-fun-name (combination-fun use)))
            (constant-lvar-p (setf arg (second (combination-args use))))
            (typep (lvar-value arg) '(unsigned-byte 16)))
       (splice-fun-args x '%instance-ref 2)
       `(lambda (obj i val) (%instance-ref-eq obj i val)))
      (t (give-up-ir1-transform)))))

(defun transform-eq-on-words (fun x y)
  (flet ((try-sword (x y x-v y-v)
           (when (not (or (csubtypep (lvar-type x) (specifier-type 'fixnum))
                          (csubtypep (lvar-type y) (specifier-type 'word))))
             `(if (#+64-bit sb-kernel:signed-byte-64-p
                   #-64-bit sb-kernel:signed-byte-32-p
                   ,y-v)
                  (,fun ,x-v (truly-the sb-vm:signed-word ,y-v))
                  nil)))
         (try-word (x y x-v y-v)
           (when (not (or (csubtypep (lvar-type x) (specifier-type 'fixnum))
                          (csubtypep (lvar-type y) (specifier-type 'sb-vm:signed-word))))
             `(if (#+64-bit sb-kernel:unsigned-byte-64-p
                   #-64-bit sb-kernel:unsigned-byte-32-p
                   ,y-v)
                  (,fun ,x-v (truly-the word ,y-v))
                  nil)))
         (do-float (x-v y-v predicate type)
           `(if (,predicate ,y-v)
                (,fun ,x-v (truly-the ,type ,y-v))
                nil)))
    (let ((x-swordp (csubtypep (lvar-type x) (specifier-type 'sb-vm:signed-word)))
          (y-swordp (csubtypep (lvar-type y) (specifier-type 'sb-vm:signed-word))))
      (cond ((and x-swordp (not y-swordp))
             (try-sword x y 'x 'y))
            ((and y-swordp (not x-swordp))
             (try-sword y y 'y 'x))
            (t
             (let ((x-wordp (csubtypep (lvar-type x) (specifier-type 'word)))
                   (y-wordp (csubtypep (lvar-type y) (specifier-type 'word))))
               (cond ((and x-wordp (not y-wordp))
                      (try-word x y 'x 'y))
                     ((and y-wordp (not x-wordp))
                      (try-word y y 'y 'x))
                     ((eq fun 'eql)
                      (let ((x-dfp (csubtypep (lvar-type x) (specifier-type 'double-float)))
                            (y-dfp (csubtypep (lvar-type y) (specifier-type 'double-float))))
                        (cond ((and x-dfp (not y-dfp))
                               (do-float 'x 'y 'double-float-p 'double-float))
                              ((and y-dfp (not x-dfp))
                               (do-float 'y 'x 'double-float-p 'double-float))
                              #-64-bit
                              (t
                               (let ((x-sfp (csubtypep (lvar-type x) (specifier-type 'single-float)))
                                     (y-sfp (csubtypep (lvar-type y) (specifier-type 'single-float))))
                                 (cond ((and x-sfp (not y-sfp))
                                        (do-float 'x 'y 'single-float-p 'single-float))
                                       ((and y-sfp (not x-sfp))
                                        (do-float 'y 'x 'single-float-p 'single-float)))))))))))))))

(deftransform eq ((x y) * *)
  "Simple equality transform"
  (let ((use (lvar-uses x))
        arg)
    (declare (ignorable use arg))
    (cond
      ((same-leaf-ref-p x y) t)
      ((not (types-equal-or-intersect (lvar-type x) (lvar-type y)))
       nil)
      ;; Reduce (eq (%instance-ref x i) Y) to 1 instruction
      ;; if possible, but do not defer the memory load unless doing
      ;; so can have no effect, i.e. Y is a constant or provably not
      ;; effectful. For now, just handle constant Y.
      ((and (vop-existsp :translate %instance-ref-eq)
            (constant-lvar-p y)
            (combination-p use)
            (almost-immediately-used-p x use)
            (eql '%instance-ref (lvar-fun-name (combination-fun use)))
            (constant-lvar-p (setf arg (second (combination-args use))))
            (typep (lvar-value arg) '(unsigned-byte 16)))
       (splice-fun-args x '%instance-ref 2)
       `(lambda (obj i val) (%instance-ref-eq obj i val)))
      ((transform-eq-on-words 'eq x y))
      (t (give-up-ir1-transform)))))

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
  (let* ((x-type (lvar-type x))
         (y-type (lvar-type y))
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
      ((transform-eq-on-words 'eql x y))
      (t
       (give-up-ir1-transform)))))

(defun array-type-dimensions-mismatch (x-type y-type)
  (and (csubtypep x-type (specifier-type 'array))
       (csubtypep y-type (specifier-type 'array))
       (let ((x-dims (sb-kernel::ctype-array-union-dimensions x-type))
             (y-dims (sb-kernel::ctype-array-union-dimensions y-type)))
         (unless (or (eq (car x-dims) '*)
                     (eq (car y-dims) '*))
           (loop with simple = (and (csubtypep x-type (specifier-type 'simple-array))
                                    (csubtypep y-type (specifier-type 'simple-array)))
                 for x-dim in x-dims
                 never
                 (loop for y-dim in y-dims
                       thereis
                       (and (= (length x-dim)
                               (length y-dim))
                            ;; Can compare dimensions only for simple
                            ;; arrays due to fill-pointer and
                            ;; adjust-array.
                            (or (not simple)
                                (loop for x in x-dim
                                      for y in y-dim
                                      always (or (eq x '*)
                                                 (eq y '*)
                                                 (= x y)))))))))))

;;; Only a simple array will always remain non-empty
(defun array-type-non-empty-p (type)
  (and (csubtypep type (specifier-type 'simple-array))
       (let ((dimensions (ctype-array-dimensions type)))
         (and (consp dimensions)
              (every (lambda (dim)
                       (typep dim '(integer 1)))
                     dimensions)))))

(defun equal-comparable-types (x y)
  (flet ((both-intersect-p (type)
           (let ((ctype (specifier-type type)))
             (and (types-equal-or-intersect x ctype)
                  (types-equal-or-intersect y ctype)))))
    (or (both-intersect-p 'string)
        ;; Even though PATHNAME doesn't have any parameters it
        ;; may appear in an EQL type.
        (both-intersect-p 'pathname)
        (both-intersect-p 'bit-vector)
        (both-intersect-p 'cons))))

(defun equalp-comparable-types (x y)
  (flet ((both-intersect-p (type)
           (let ((ctype (specifier-type type)))
             (and (types-equal-or-intersect x ctype)
                  (types-equal-or-intersect y ctype)))))
    (or (both-intersect-p 'number)
        (both-intersect-p 'array)
        (both-intersect-p 'character)
        (both-intersect-p 'cons)
        ;; Even though these don't have any parameters they may
        ;; appear in an EQL type.
        (both-intersect-p 'pathname)
        (both-intersect-p 'instance)
        (both-intersect-p 'hash-table))))

(defun equalp-eql-comparable-types (x y)
  (flet ((both-intersect-p (type)
           (let ((ctype (specifier-type type)))
             (and (types-equal-or-intersect x ctype)
                  (types-equal-or-intersect y ctype)))))
    (not (or (and (let ((int-x (type-intersection x (specifier-type 'number)))
                        (int-y (type-intersection y (specifier-type 'number))))
                    (and (neq int-x *empty-type*)
                         (neq int-y *empty-type*)
                         (not
                          (and (csubtypep int-x (specifier-type 'integer))
                               (csubtypep int-y (specifier-type 'integer)))))))
             (both-intersect-p 'array)
             (both-intersect-p 'character)
             (both-intersect-p 'cons)
             (both-intersect-p 'pathname)
             (both-intersect-p 'instance)
             (both-intersect-p 'hash-table)))))

(defun equal-remove-incompatible-types (x y &optional equalp)
  (block nil
    (let ((n-x (type-intersection x (specifier-type 'number)))
          (n-y (type-intersection y (specifier-type 'number))))
      (unless (or (eq n-x *empty-type*)
                  (eq n-y *empty-type*))
        (let ((i-x (type-approximate-interval n-x))
              (i-y (type-approximate-interval n-y)))
          (when (and i-x i-y
                     (interval-/= i-x i-y))
            (return
              (values (type-difference x n-x)
                      (type-difference y n-y)))))))
    (let ((a-x (type-intersection x (specifier-type 'array)))
          (a-y (type-intersection y (specifier-type 'array))))
      (when (not (or (eq a-x *empty-type*)
                     (eq a-y *empty-type*)))
        (when (or (not equalp)
                  ;; At least one array has to be longer than 0
                  ;; and not adjustable, because #() and "" are equalp.
                  (array-type-non-empty-p a-x)
                  (array-type-non-empty-p a-y))
          (let ((x-et (type-array-element-type a-x))
                (y-et (type-array-element-type a-y)))
            (when (and (neq x-et *wild-type*)
                       (neq y-et *wild-type*)
                       (not (types-equal-or-intersect x-et y-et)))
              (return
                (values (type-difference x a-x)
                        (type-difference y a-y))))))
        (when (array-type-dimensions-mismatch a-x a-y)
          (return
            (values (type-difference x a-x)
                    (type-difference y a-y))))))
    (values x y)))

;;; similarly to the EQL transform above, we attempt to constant-fold
;;; or convert to a simpler predicate: mostly we have to be careful
;;; with strings and bit-vectors.
(deftransform equal ((x y) * *)
  "convert to simpler equality predicate"
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y)))
    (flet ((unroll-constant (x y)
             (when (constant-lvar-p x)
               (let ((value (lvar-value x)))
                 (cond ((equal value "")
                        `(and (stringp ,y)
                              (zerop (length ,y))))
                       ((typep value '(cons (or fixnum symbol character) null))
                        `(and (,(if (car value)
                                    'listp
                                    'consp)
                               ,y)
                              (eq (car ,y) ',(car value))
                              (null (cdr ,y)))))))))
      (cond ((same-leaf-ref-p x y) t)
            ((array-type-dimensions-mismatch x-type y-type)
             nil)
            ((unroll-constant x 'y))
            ((unroll-constant y 'x))
            (t
             (flet ((try (x-type y-type)
                      (flet ((both-csubtypep (type)
                               (let ((ctype (specifier-type type)))
                                 (and (csubtypep x-type ctype)
                                      (csubtypep y-type ctype))))
                             (some-csubtypep (type)
                               (let ((ctype (specifier-type type)))
                                 (or (csubtypep x-type ctype)
                                     (csubtypep y-type ctype))))
                             (non-equal-array-p (type)
                               (and (csubtypep type (specifier-type 'array))
                                    (let ((equal-types (specifier-type '(or bit character)))
                                          (element-types (ctype-array-specialized-element-types type)))
                                      (and (neq element-types *wild-type*)
                                           (notany (lambda (x)
                                                     (csubtypep x equal-types))
                                                   element-types))))))
                        (cond
                          ((or (some-csubtypep 'symbol)
                               (some-csubtypep 'character))
                           `(eq x y))
                          ((both-csubtypep 'string)
                           '(string= x y))
                          ((both-csubtypep 'bit-vector)
                           '(bit-vector-= x y))
                          ((both-csubtypep 'pathname)
                           '(pathname= x y))
                          ((or (non-equal-array-p x-type)
                               (non-equal-array-p y-type))
                           '(eq x y))
                          ((multiple-value-bind (x-type y-type)
                               (equal-remove-incompatible-types x-type y-type)
                             (cond
                               ((or (eq x-type *empty-type*)
                                    (eq y-type *empty-type*))
                                nil)
                               ((equal-comparable-types x-type y-type)
                                :give-up)
                               ((types-equal-or-intersect x-type y-type)
                                '(eql x y)))))))))
               (let ((r (try x-type y-type)))
                 (if (eq r :give-up)
                     (let* ((not-x-type (type-difference x-type (specifier-type 'null)))
                            (r (try not-x-type y-type)))
                       (cond ((not r)
                              `(eq x y))
                             ((neq r :give-up)
                              `(when x
                                 ,r))
                             (t
                              (let* ((not-y-type (type-difference y-type (specifier-type 'null)))
                                     (r (try x-type not-y-type)))
                                (cond ((not r)
                                       `(eq x y))
                                      ((neq r :give-up)
                                       `(when y
                                          ,r))
                                      (t
                                       (let ((r (try not-x-type not-y-type)))
                                         (if (neq r :give-up)
                                             `(if (null x)
                                                  (null y)
                                                  (and y
                                                       ,r))
                                             (give-up-ir1-transform)))))))))
                     r))))))))

(deftransform equalp ((x y) * *)
  "convert to simpler equality predicate"
  (let ((x-type (lvar-type x))
        (y-type (lvar-type y)))
    (flet ((unroll-constant (x y)
             (when (constant-lvar-p x)
               (let ((value (lvar-value x)))
                 (cond ((typep value '(simple-array * (0)))
                        `(and (vectorp ,y)
                              (zerop (length ,y))))
                       ((typep value '(cons symbol null))
                        `(and (,(if (car value)
                                    'listp
                                    'consp)
                               ,y)
                              (eq (car ,y) ',(car value))
                              (null (cdr ,y))))
                       ((and (proper-or-dotted-list-p value)
                             (loop with cdr = value
                                   while (consp cdr)
                                   always (symbolp (pop cdr))))
                        `(equal x y)))))))
      (cond ((same-leaf-ref-p x y) t)
            ((array-type-dimensions-mismatch x-type y-type)
             nil)
            ((unroll-constant x 'y))
            ((unroll-constant y 'x))
            (t
             (flet ((try (x-type y-type)
                      (flet ((both-csubtypep (type)
                               (let ((ctype (specifier-type type)))
                                 (and (csubtypep x-type ctype)
                                      (csubtypep y-type ctype))))
                             (some-csubtypep (type)
                               (let ((ctype (specifier-type type)))
                                 (or (csubtypep x-type ctype)
                                     (csubtypep y-type ctype))))
                             (transform-char-equal (x y)
                               (and (constant-lvar-p y)
                                    (characterp (lvar-value y))
                                    (transform-constant-char-equal x y 'eq))))
                        (cond
                          ((some-csubtypep 'symbol)
                           `(eq x y))
                          ((transform-char-equal 'x y))
                          ((transform-char-equal 'y x))
                          ((both-csubtypep 'string)
                           '(string-equal x y))
                          ((both-csubtypep 'bit-vector)
                           '(bit-vector-= x y))
                          ((and (both-csubtypep 'array)
                                (not (types-equal-or-intersect x-type (specifier-type '(or string bit-vector))))
                                (not (types-equal-or-intersect y-type (specifier-type '(or string bit-vector)))))
                           `(sb-impl::array-equalp x y))
                          ((both-csubtypep 'pathname)
                           '(pathname= x y))
                          ((both-csubtypep 'character)
                           '(char-equal x y))
                          ((both-csubtypep 'number)
                           '(= x y))
                          ((both-csubtypep 'hash-table)
                           '(hash-table-equalp x y))
                          ;; TODO: two instances of the same type should dispatch
                          ;; directly to the EQUALP-IMPL function in the layout.
                          (t
                           (multiple-value-bind (x-type y-type)
                               (equal-remove-incompatible-types x-type y-type t)
                             (cond
                               ((or (eq x-type *empty-type*)
                                    (eq y-type *empty-type*))
                                nil)
                               ((types-equal-or-intersect x-type y-type)
                                (if (equalp-eql-comparable-types x-type y-type)
                                    '(eql x y)
                                    :give-up))
                               ((equalp-comparable-types x-type y-type)
                                :give-up))))))))
               (let ((r (try x-type y-type)))
                 (if (eq r :give-up)
                     (let* ((not-x-type (type-difference x-type (specifier-type 'null)))
                            (r (try not-x-type y-type)))
                       (cond ((not r)
                              `(eq x y))
                             ((neq r :give-up)
                              `(when x
                                 ,r))
                             (t
                              (let* ((not-y-type (type-difference y-type (specifier-type 'null)))
                                     (r (try x-type not-y-type)))
                                (cond ((not r)
                                       `(eq x y))
                                      ((neq r :give-up)
                                       `(when y
                                          ,r))
                                      (t
                                       (let ((r (try not-x-type not-y-type)))
                                         (if (neq r :give-up)
                                             `(if (null x)
                                                  (null y)
                                                  (and y
                                                       ,r))
                                             (give-up-ir1-transform)))))))))
                     r))))))))

;;; Convert to EQL if both args are rational and complexp is specified
;;; and the same for both.
(deftransform = ((x y) (number number) *)
  "open code"
  (let* ((x-type (lvar-type x))
         (y-type (lvar-type y)))
    ;; Inline if the intersecting type is simple:
    ;; (= (the (or single-float integer) x) 1.5)
    ;; => (and (single-float-p x) (= x 1.5))
    (labels ((transform (x-type y-type x y type test)
               (when (and (csubtypep x-type type)
                          (not (and (eq test 'fixnump)
                                    (csubtypep y-type (specifier-type 'rational))))
                          (not (csubtypep y-type type))
                          (let ((excluded (type-difference y-type type)))
                            (and (neq excluded *empty-type*)
                                 (let ((x-int (type-approximate-interval x-type))
                                       (y-int (type-approximate-interval excluded)))
                                   (and x-int
                                        (or (and y-int
                                                 (interval-/= x-int y-int))
                                            (and (interval-float-p x-int)
                                                 (csubtypep excluded (specifier-type 'integer)))))))))
                 `(and (,test ,y)
                       (= ,x (truly-the ,(type-specifier type) ,y)))))
             (intersecting (type test)
               (or (transform x-type y-type 'x 'y type test)
                   (transform y-type x-type 'y 'x type test))))
      (cond ((intersecting (specifier-type 'single-float) 'single-float-p))
            ((intersecting (specifier-type 'double-float) 'double-float-p))
            ((intersecting (specifier-type 'fixnum) 'fixnump))
            ;; Convert to EQL if both args are rational and complexp is specified
            ;; and the same for both.
            ((or (and (csubtypep x-type (specifier-type 'float))
                      (csubtypep y-type (specifier-type 'float)))
                 (and (csubtypep x-type (specifier-type '(complex float)))
                      (csubtypep y-type (specifier-type '(complex float))))
                 (and (vop-existsp :named sb-vm::=/complex-single-float)
                      (csubtypep x-type (specifier-type '(or single-float (complex single-float))))
                      (csubtypep y-type (specifier-type '(or single-float (complex single-float)))))
                 (and (vop-existsp :named sb-vm::=/complex-double-float)
                      (csubtypep x-type (specifier-type '(or double-float (complex double-float))))
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
            ((or (and (csubtypep x-type (specifier-type 'real))
                      (csubtypep y-type
                                 (specifier-type '(complex rational))))
                 (and (csubtypep y-type (specifier-type 'real))
                      (csubtypep x-type
                                 (specifier-type '(complex rational)))))
             ;; Can't be EQL since imagpart can't be 0.
             nil)
            (t
             (give-up-ir1-transform
              "The operands might not be the same type."))))))

(defun maybe-float-lvar-p (lvar)
  (neq *empty-type* (type-intersection (specifier-type 'float)
                                       (lvar-type lvar))))

#+(or arm arm64 x86-64 x86)
(flet ((maybe-invert (op inverted x y)
         (cond
           ((and (not (vop-existsp :translate >=))
                 (csubtypep (lvar-type x) (specifier-type 'float))
                 (csubtypep (lvar-type y) (specifier-type 'float)))
            `(or (,op x y) (= x y)))
           ;; Don't invert if either argument can be a float (NaNs)
           ((or (maybe-float-lvar-p x) (maybe-float-lvar-p y))
            (give-up-ir1-transform))
           (t
            `(if (,inverted x y) nil t)))))
  (deftransform >= ((x y) (number number) * :node node)
    "invert or open code"
    (maybe-invert '> '< x y))
  (deftransform <= ((x y) (number number) * :node node)
    "invert or open code"
    (maybe-invert '< '> x y)))

;;; FIXME: for some reason these do not survive cold-init with <=
#-(or arm arm64 x86-64 x86)
(flet ((maybe-invert (node op inverted x y)
         (cond
           ;; Don't invert if either argument can be a float (NaNs)
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

;;; Make sure any constant arg is second.
(macrolet ((def (name inverse)
             `(deftransform ,name ((x y))
                (if (and (constant-lvar-p x)
                         (not (constant-lvar-p y)))
                    `(,',inverse y x)
                    (give-up-ir1-transform)))))
  (def = =)
  (def < >)
  (def > <)
  (def <= >=)
  (def >= <=))

;;; See whether we can statically determine (< X Y) using type
;;; information. If X's high bound is < Y's low, then X < Y.
;;; Similarly, if X's low is >= to Y's high, the X >= Y (so return
;;; NIL).
(macrolet ((def (name reflexive-p surely-true surely-false)
             `(defoptimizer (,name derive-type) ((x y))
                (if (and (same-leaf-ref-p x y)
                         ;; For non-reflexive functions we don't need
                         ;; to worry about NaNs: (non-ref-op NaN NaN) => false,
                         ;; but with reflexive ones we don't know...
                         ,@(when reflexive-p
                             '((and (not (maybe-float-lvar-p x))
                                (not (maybe-float-lvar-p y))))))
                    (specifier-type '(eql ,reflexive-p))
                    (multiple-value-bind (ix x-complex)
                        (type-approximate-interval (lvar-type x))
                      (when ix
                        (multiple-value-bind (iy y-complex)
                            (type-approximate-interval (lvar-type y))
                          (when iy
                            (cond ((and (or (not x-complex)
                                            (interval-contains-p 0 ix))
                                        (or (not y-complex)
                                            (interval-contains-p 0 iy))
                                        ,surely-true)
                                   (specifier-type '(eql t)))
                                  (,surely-false
                                   (specifier-type '(eql nil))))))))))))
  (def =  t (interval-= ix iy) (interval-/= ix iy))
  (def <  nil (interval-< ix iy) (interval->= ix iy))
  (def >  nil (interval-< iy ix) (interval->= iy ix))
  (def <= t (interval->= iy ix) (interval-< iy ix))
  (def >= t (interval->= ix iy) (interval-< ix iy)))

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
;;; comparison functions other than inequality.  If the call has two
;;; args, then we pass or return a negated test as appropriate. If it
;;; is a degenerate one-arg call, then we transform to code that
;;; returns true. Otherwise, we bind all the arguments and expand into
;;; a bunch of IFs.
(defun multi-compare (predicate args not-p type)
  (let ((nargs (length args)))
    (cond ((< nargs 1) (values nil t))
          ((= nargs 1) `(progn (the ,type ,@args) t))
          ((= nargs 2)
           (if not-p
               `(if (,predicate ,(first args) ,(second args)) nil t)
               (values nil t)))
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
                 `((lambda ,vars
                     ;; the first two arguments will be checked by the comparison function.
                     (declare (type ,type ,@(subseq vars 2)))
                     ,result)
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
  (multi-compare 'char-equal args nil 'character))
(define-source-transform char-lessp (&rest args)
  (multi-compare 'char-lessp args nil 'character))
(define-source-transform char-greaterp (&rest args)
  (multi-compare 'char-greaterp args nil 'character))
(define-source-transform char-not-greaterp (&rest args)
  (multi-compare 'char-greaterp args t 'character))
(define-source-transform char-not-lessp (&rest args)
  (multi-compare 'char-lessp args t 'character))

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
          ((or (> (length args) 50)
               (not (policy *lexenv*
                        (and (>= speed space)
                             (>= speed compilation-speed)))))
           (values nil t))
          (t
           (let ((vars (make-gensym-list nargs)))
             `((lambda ,vars
                 (declare (type ,type ,@vars))
                 (block nil
                   (tagbody
                      ,@(loop for (var . rest) on vars
                              nconc (loop for var2 in rest
                                          collect `(if (,predicate ,var ,var2)
                                                       (go return-nil))))
                      (return-from nil t)
                    return-nil
                      (return-from nil nil))))
               ,@args))))))

(define-source-transform /= (&rest args)
  (multi-not-equal '= args 'number))
(define-source-transform char/= (&rest args)
  (multi-not-equal 'char= args 'character))
(define-source-transform char-not-equal (&rest args)
  (multi-not-equal 'char-equal args 'character))

;;; Expand MAX and MIN into the obvious comparisons.
(define-source-transform max (arg0 &rest rest)
  (if (null rest)
      `(values (the real ,arg0))
      (labels ((expand (arg0 &rest rest)
                 (if (null rest)
                     arg0
                     (once-only ((arg0 arg0)
                                 (minrest (apply #'expand rest)))
                       `(if (> ,minrest ,arg0)
                            ,minrest
                            ,arg0)))))
        (apply #'expand arg0 rest))))

(define-source-transform min (arg0 &rest rest)
  (if (null rest)
      `(values (the real ,arg0))
      (labels ((expand (arg0 &rest rest)
                 (if (null rest)
                     arg0
                     (once-only ((arg0 arg0)
                                 (maxrest (apply #'expand rest)))
                       `(if (< ,maxrest ,arg0)
                            ,maxrest
                            ,arg0)))))
        (apply #'expand arg0 rest))))

;;; Simplify some cross-type comparisons
(macrolet ((def (comparator round)
             `(progn
                (deftransform ,comparator
                    ((x y) (rational (constant-arg float)))
                  "open-code RATIONAL to FLOAT comparison"
                  (let ((y (lvar-value y)))
                    (when (float-infinity-or-nan-p y)
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

(macrolet ((def (comparator not-equal round)
             `(progn
                (deftransform ,comparator
                    ((x y) (rational (constant-arg float)))
                  "open-code RATIONAL to FLOAT comparison"
                  (let ((y (lvar-value y)))
                    (when (float-infinity-or-nan-p y)
                      (give-up-ir1-transform))
                    (setf y (rational y))
                    (multiple-value-bind (qout rem)
                        (if (csubtypep (lvar-type x)
                                       (specifier-type 'integer))
                            (,round y)
                            (values y 0))
                      `(,(if (zerop rem)
                             ',comparator
                             ',not-equal)
                        x ,qout))))
                (deftransform ,comparator
                    ((x y) (integer (constant-arg ratio)))
                  "open-code INTEGER to RATIO comparison"
                  `(,',not-equal x ,(,round (lvar-value y)))))))
  (def <= < ceiling)
  (def >= > floor))

(macrolet ((def (name x y type-x type-y &optional non-fixnum)
             `(deftransform ,name ((,x ,y) (,type-x ,type-y) * :node node :important nil)
                (cond ((or (csubtypep (lvar-type i) (specifier-type 'word))
                           (csubtypep (lvar-type i) (specifier-type 'sb-vm:signed-word)))
                       (give-up-ir1-transform))
                      (t
                       ;; Give the range-transform optimizers a chance to trigger.
                       (delay-ir1-transform node :ir1-phases)
                       `(if (fixnump i)
                            (let ((i (truly-the fixnum i)))
                              (,',name ,',x ,',y))
                            ,,non-fixnum))))))

  (def < i f (integer #.most-negative-fixnum) fixnum)
  (def > f i fixnum (integer #.most-negative-fixnum))

  (def > i f (integer * #.most-positive-fixnum) fixnum)
  (def < f i fixnum (integer * #.most-positive-fixnum))

  (def > i f (integer #.most-negative-fixnum) fixnum t)
  (def < f i fixnum (integer #.most-negative-fixnum) t)

  (def < i f (integer * #.most-positive-fixnum) fixnum t)
  (def > f i fixnum (integer * #.most-positive-fixnum) t))

(deftransform < ((x y) (integer (eql #.(1+ most-positive-fixnum))) * :important nil)
  `(not (> x most-positive-fixnum)))

(deftransform > ((x y) (integer (eql #.(1- most-negative-fixnum))) * :important nil)
  `(not (< x most-negative-fixnum)))

(deftransform = ((x y) (rational (constant-arg float)))
  "open-code RATIONAL to FLOAT comparison"
  (let ((y (lvar-value y)))
    (when (float-infinity-or-nan-p y)
      (give-up-ir1-transform))
    (setf y (rational y))
    (if (and (csubtypep (lvar-type x)
                        (specifier-type 'integer))
             (sb-xc:typep y 'ratio))
        nil
        `(= x ,y))))

(deftransform = ((x y) (t (constant-arg integer)))
  (let ((y (lvar-value y)))
    (if (and (if (cast-p (lvar-uses x))
                 ;; Only when X is already a number.
                 (csubtypep (lvar-type (cast-value (lvar-uses x)))
                            (specifier-type 'number))
                 t)
             (handler-case (not (sb-xc:= y (coerce y 'double-float)))
               (floating-point-overflow ()
                 t))
             (handler-case (not (sb-xc:= y (coerce y 'single-float)))
               (floating-point-overflow ()
                 t)))
        `(eql x y)
        (give-up-ir1-transform))))

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
                  (handler-case (funcall fun reduced-value value)
                    (arithmetic-error ()
                      (not-constants arg))
                    (:no-error (value)
                      (cond ((bad-float-p value)
                             (not-constants arg))
                            (t
                             (setf reduced-value value
                                   reduced-p t))))))
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
                          * ; KLUDGE: avoid "assertion too complex to check"
                          #|(values t &optional (member nil t))|#)
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
#-(or arm arm64 mips x86 x86-64 riscv) ; defined in compiler/{arch}/arith.lisp
(define-source-transform logeqv (&rest args)
  (source-transform-transitive 'logeqv args -1 'integer))
(define-source-transform gcd (&rest args)
  (source-transform-transitive 'gcd args 0 'integer '(abs)))
(define-source-transform lcm (&rest args)
  (source-transform-transitive 'lcm args 1 'integer '(abs)))
(deftransform logandc1 ((x y))
  `(logandc2 y x))

(deftransform gcd ((x y) ((and fixnum (not (eql 0)))
                          (and fixnum (not (eql 0)))))
  `(sb-kernel::fixnum-gcd x y))

(deftransforms (gcd sb-kernel::fixnum-gcd) ((x y))
  (cond ((or (same-leaf-ref-p x y)
             (lvar-value-is y 0))
         '(abs x))
        ((lvar-value-is x 0)
         '(abs y))
        (t
         (give-up-ir1-transform))))

(deftransform lcm ((x y))
  (cond ((or (same-leaf-ref-p x y)
             (csubtypep (lvar-type y) (specifier-type '(or (eql -1) (eql 1)))))
         '(abs x))
        ((csubtypep (lvar-type x) (specifier-type '(or (eql -1) (eql 1))))
         '(abs y))
        ((or (lvar-value-is x 0)
             (lvar-value-is y 0))
         0)
        (t
         (give-up-ir1-transform))))

(defun derive-gcd (args)
  (let ((min)
        (max)
        (includes-zero t)
        unbounded
        primes)
    (loop for arg in args
          for type = (lvar-type arg)
          do (multiple-value-bind (low high) (integer-type-numeric-bounds type)
               (let ((zero (types-equal-or-intersect type (specifier-type '(eql 0)))))
                 (unless zero
                   (setf includes-zero nil))
                 (cond ((not (and low high))
                        (setf unbounded t))
                       ((and (= (abs low) 1)
                             (= (abs high) 1)
                             (not zero))
                        (return-from derive-gcd (specifier-type '(eql 1))))
                       #-sb-xc-host
                       ((and (= low high)
                             (typep (abs low)
                                    `(integer 0
                                              ,(min (expt 2 32)
                                                    most-positive-fixnum)))
                             ;; Get some extra points
                             (positive-primep (abs low)))
                        (pushnew (abs low) primes))))
               (cond (unbounded)
                     (min
                      (setf min (min min low)
                            max (max max high)))
                     (t
                      (setf min low
                            max high)))))
    (specifier-type (cond ((not primes)
                           `(integer ,(if includes-zero
                                          0
                                          1)
                                     ,(if unbounded
                                          '*
                                          (max (abs min)
                                               (abs max)))))
                          ((cdr primes)
                           '(eql 1))
                          (t
                           `(or (eql 1) (eql ,(car primes))))))))

(defoptimizer (gcd derive-type) ((&rest args))
  (derive-gcd args))
(defoptimizer (sb-kernel::fixnum-gcd derive-type) ((&rest args))
  (derive-gcd args))

(defoptimizer (lcm derive-type) ((&rest args))
  (let (min
        maxes)
    (loop for arg in args
          for type = (lvar-type arg)
          do (multiple-value-bind (low high) (integer-type-numeric-bounds type)
               (unless (and low high)
                 (return-from lcm-derive-type-optimizer))
               (let* ((crosses-zero (<= low 0 high))
                      (abs-low (abs low))
                      (abs-high (abs high))
                      (low (if crosses-zero
                               0
                               (min abs-high abs-low)))
                      (high (max abs-low abs-high)))
                 (if min
                     (setf min (min min low))
                     (setf min low))
                 (push high maxes))))
    (specifier-type `(integer ,min ,(reduce #'* maxes)))))

;;; Do source transformations for intransitive n-arg functions such as
;;; /. With one arg, we form the inverse. With two args we pass.
;;; Otherwise we associate into two-arg calls.
(declaim (ftype (function (symbol symbol list list &optional symbol)
                          * ; KLUDGE: avoid "assertion too complex to check"
                          #|(values list &optional (member nil t))|#)
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

;;;; a hack to clean up divisions

(defun count-low-order-zeros (thing)
  (typecase thing
    (lvar
     (if (constant-lvar-p thing)
         (count-low-order-zeros (lvar-value thing))
         (count-low-order-zeros (lvar-uses thing))))
    (combination
     (case (let ((name (lvar-fun-name (combination-fun thing))))
             (or (modular-version-info name :untagged nil) name))
       ((+ -)
        (let ((min most-positive-fixnum)
              (itype (specifier-type 'integer)))
          (dolist (arg (combination-args thing) min)
            (if (csubtypep (lvar-type arg) itype)
                (setf min (min min (count-low-order-zeros arg)))
                (return 0)))))
       (*
        (let ((result 0)
              (itype (specifier-type 'integer)))
          (dolist (arg (combination-args thing) result)
            (if (csubtypep (lvar-type arg) itype)
                (setf result (+ result (count-low-order-zeros arg)))
                (return 0)))))
       (ash
        (let ((args (combination-args thing)))
          (if (= (length args) 2)
              (let ((amount (second args)))
                (if (constant-lvar-p amount)
                    (max (+ (count-low-order-zeros (first args))
                            (lvar-value amount))
                         0)
                    0))
              0)))
       (t
        0)))
    (integer
     (if (zerop thing)
         most-positive-fixnum
         (do ((result 0 (1+ result))
              (num thing (ash num -1)))
             ((logbitp 0 num) result))))
    (cast
     (count-low-order-zeros (cast-value thing)))
    (t
     0)))

(deftransform / ((numerator denominator) (integer integer))
  "convert x/2^k to shift"
  (unless (constant-lvar-p denominator)
    (give-up-ir1-transform))
  (let* ((denominator (lvar-value denominator))
         (bits (1- (integer-length denominator))))
    (unless (and (> denominator 0) (= (ash 1 bits) denominator))
      (give-up-ir1-transform))
    (let ((alignment (count-low-order-zeros numerator)))
      (unless (>= alignment bits)
        (give-up-ir1-transform))
      `(ash numerator ,(- bits)))))

(deftransforms (rational rationalize) ((x) (rational))
  'x)

(defoptimizer (rational derive-type) ((x))
  (one-arg-derive-type x (lambda (type)
                           (labels ((%%rational (x)
                                      (unless (and (floatp x)
                                                   (float-infinity-or-nan-p x))
                                        (rational x)))
                                    (%rational (bound)
                                      (typecase bound
                                        (cons (list (%%rational (car bound))))
                                        (null nil)
                                        (t (%%rational bound)))))
                             (make-numeric-type
                              :class 'rational
                              :low (%rational (numeric-type-low type))
                              :high (%rational (numeric-type-high type)))))
                       #'rational))

(defoptimizer (rationalize derive-type) ((x))
  (one-arg-derive-type x (lambda (type)
                           (labels ((%%rationalize (x)
                                      (unless (and (floatp x)
                                                   (float-infinity-or-nan-p x))
                                        (rationalize x)))
                                    (%rationalize (bound)
                                      (typecase bound
                                        (cons (list (%%rationalize (car bound))))
                                        (null nil)
                                        (t (%%rationalize bound)))))
                             (make-numeric-type
                              :class 'rational
                              :low (%rationalize (numeric-type-low type))
                              :high (%rationalize (numeric-type-high type)))))
                       #'rationalize))


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

;;; This determines if the REF to a &REST variable is headed towards
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
  (let ((skip 0))
    (when-vop-existsp (:named sb-vm::%more-arg-values-skip)
      (when (typep list '(cons (member cdr rest) (cons t null)))
        (setf skip 1)
        (setf list (second list))))
    (multiple-value-bind (context count )
        (possible-rest-arg-context list)
      (if context
          `(%rest-values ,list ,context ,skip ,count)
          (values nil t)))))

;;; NTH -> %REST-REF
(define-source-transform nth (n list)
  (multiple-value-bind (context count) (possible-rest-arg-context list)
    (if context
        `(%rest-ref ,n ,list ,context ,count)
        (values nil t))))
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

(defoptimizer (length derive-type) ((sequence))
  (when (csubtypep (lvar-type sequence) (specifier-type 'list))
    (specifier-type '(mod #.(truncate sb-vm::max-dynamic-space-size
                             (* sb-vm:cons-size sb-vm:n-word-bytes))))))

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

(deftransform %rest-values ((list context skip count))
  (if (rest-var-more-context-ok list)
      `(%more-arg-values context skip count)
      `(values-list (nthcdr skip list))))

(deftransform %rest-ref ((n list context count &optional length-checked-p))
  (cond ((not (rest-var-more-context-ok list))
         `(nth n list))
        ((and length-checked-p
              (constant-lvar-p length-checked-p)
              (lvar-value length-checked-p))
         `(%more-arg context n))
        (t
         `(and (< (the index n) count) (%more-arg context n)))))

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
(defglobal *optimize-format-strings* t)
(defun check-format-args (node fun arg-n verify-arg-count
                          &aux (combination-args (basic-combination-args node)))
  ;; ARG-N is the index into COMBINATION-ARGS of a format control string,
  ;; usually 0 or 1.
  (flet ((maybe-replace (control)
           (binding* ((string (lvar-value control))
                      ((symbols new-string)
                       (sb-format::extract-user-fun-directives string)))
             (when (or symbols (and new-string (string/= string new-string)))
               (change-ref-leaf
                (lvar-use control)
                (find-constant
                 (cond ((not symbols) new-string)
                       ((producing-fasl-file)
                        (acond ((assoc string (constant-cache *compilation*) :test 'equal)
                                (cdr it))
                               (t
                                (let ((proxy (sb-format::make-fmt-control-proxy
                                                         new-string symbols)))
                                  (maybe-emit-make-load-forms proxy)
                                  (push (cons string proxy) (constant-cache *compilation*))
                                  proxy))))
                       #-sb-xc-host ; no such object as a FMT-CONTROL
                       (t
                        (sb-format::make-fmt-control new-string symbols))))
                :recklessly t)))))
    (when (list-of-length-at-least-p combination-args (1+ arg-n))
      (let* ((args (nthcdr arg-n combination-args))
             (control (pop args)))
        (when (and (constant-lvar-p control) (stringp (lvar-value control)))
          (when verify-arg-count
            (binding* ((string (lvar-value control))
                       (*compiler-error-context* node)
                       ((min max)
                        (handler-case (sb-format:%compiler-walk-format-string
                                       string args)
                          (sb-format:format-error (c)
                            (compiler-warn "~A" c)))
                        :exit-if-null)
                       (nargs (length args)))
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
                     :format-arguments (list nargs fun string max))))))
          ;; Now possibly replace the control string
          (when *optimize-format-strings*
            (maybe-replace control))
          (return-from check-format-args)))
      ;; Look for a :FORMAT-CONTROL and possibly replace that. Always do that
      ;; when cross-compiling, but in the target, cautiously skip this step
      ;; if the first argument is not known to be a symbol. Why?
      ;; Well if the first argument is a string (or function), then that object
      ;; is *arbitrary* format-control, and the arguments that follow it do not
      ;; necessarily comprise a keyword/value pair list as they would for
      ;; constructing a condition instance. Moreover, in that case you could
      ;; randomly have a symbol named :FORMAT-CONTROL as an argument, randomly
      ;; followed by a string that is not actually a format-control!
      ;; But when the first argument is a symbol, then the following arguments
      ;; must be a plist passed to MAKE-CONDITION. [See COERCE-TO-CONDITION]
      ;;
      ;; In this cross-compiler, this processing is not only always right, but
      ;; in fact mandatory, to make our format strings agnostic of package names.
      (when (and *optimize-format-strings*
                 (member fun '(error warn style-warn
                               compiler-warn compiler-style-warn))
                 ;; Hmm, should we additionally require that this symbol be
                 ;; known to designate a subtype of SIMPLE-CONDITION? Perhaps.
                 #-sb-xc-host
                 (csubtypep (lvar-type (car combination-args))
                            (specifier-type 'symbol)))
        (let ((keywords (cdr combination-args)))
          (loop
            (unless (and keywords (constant-lvar-p (car keywords))) (return))
            (when (eq (lvar-value (car keywords)) :format-control)
              (let ((control (cadr keywords)))
                (when (and (constant-lvar-p control) (stringp (lvar-value control)))
                  (maybe-replace control)))
              (return))
            (setq keywords (cddr keywords))))))))

;;; FORMAT control string best-effort sanity checker and compactor
(dolist (fun (append '(format error warn style-warn %program-error)
                     #+sb-xc-host ; No need for these after self-build
                     '(bug compiler-mumble compiler-notify
                       compiler-style-warn compiler-warn compiler-error
                       maybe-compiler-notify
                       note-lossage note-unwinnage)))
  (setf (fun-info-optimizer (fun-info-or-lose fun))
        (let ((arg-n (if (eq fun 'format) 1 0))
              ;; in some Lisps, DOLIST uses SETQ instead of LET on the iteration
              ;; variable, so the closures would all share one binding of FUN,
              ;; which is not as intended. An extra LET solves that.
              (fun fun))
          (lambda (node) (check-format-args node fun arg-n t)))))

;; Can these appear in the expansion of FORMATTER?
#+sb-xc-host
(dolist (fun '(sb-format:format-error
               sb-format::format-error-at
               sb-format::format-error-at*))
  (setf (fun-info-optimizer (fun-info-or-lose fun))
        (let ((arg-n (if (eq fun 'sb-format:format-error) 0 2))
              (fun fun))
          (lambda (node) (check-format-args node fun arg-n nil)))))

(defoptimizer (format derive-type) ((dest control &rest args))
  (when (lvar-value-is dest nil)
    (specifier-type 'simple-string)))

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
         ;; FIXME: instead of checking the condition report, define a
         ;; dedicated condition class
         (expr (handler-case ; in case %formatter wants to signal an error
                   (sb-format::%formatter control argc nil)
                 ;; otherwise, let the macro complain
                 (sb-format:format-error (c)
                   (if (string= (sb-format::format-error-complaint c)
                                "No package named ~S")
                       ;; "~/apackage:afun/" might become legal later.
                       ;; To put it in perspective, "~/f" (no closing slash)
                       ;; *will* be a runtime error, but this only *might* be
                       ;; a runtime error, so we can't signal a full warning.
                       ;; At absolute worst it should be a style-warning.
                       (give-up-ir1-transform "~~// directive mentions unknown package")
                      `(formatter ,control))))))
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
       (%with-output-to-string (stream)
         (funcall control stream ,@arg-names)))))

(defun concatenate-format-p (control args)
  (and
   (loop for directive in control
         always
         (or (stringp directive)
             (and (sb-format::format-directive-p directive)
                  (and (eql (sb-format::directive-bits directive) (char-code #\A))
                       (null (sb-format::directive-params directive))
                       (pop args)))))
   (null args)))

(deftransform format ((stream control &rest args) (null (constant-arg string) &rest t))
  (let ((tokenized
          (handler-case
              (sb-format::tokenize-control-string (coerce (lvar-value control) 'simple-string))
            (sb-format:format-error ()
              (give-up-ir1-transform)))))
    (unless (concatenate-format-p tokenized args)
      (give-up-ir1-transform))
    (let* ((arg-names (make-gensym-list (length args)))
           (stringsp t)
           (args (let ((arg-names arg-names))
                   (mapcar (lambda (directive)
                             (if (stringp directive)
                                 directive
                                 (let ((arg (pop args))
                                       (arg-name (pop arg-names)))
                                   (unless (csubtypep (lvar-type arg) (specifier-type 'string))
                                     (setf stringsp nil))
                                   (if (constant-lvar-p arg)
                                       `',(lvar-value arg)
                                       arg-name))))
                           tokenized))))
      (if (= (length args) 1)
          `(lambda (stream control ,@arg-names)
             (declare (ignore stream control)
                      (ignorable ,@arg-names))
             (princ-to-string ,@args))
          `(lambda (stream control ,@arg-names)
             (declare (ignore stream control)
                      (ignorable ,@arg-names))
             (,@(if stringsp
                    `(concatenate 'string)
                    `(sb-format::princ-multiple-to-string))
              ,@args))))))

(deftransform sb-format::princ-multiple-to-string ((&rest args) (&rest string) * :important nil)
  (let ((arg-names (make-gensym-list (length args))))
    `(lambda ,arg-names
       (concatenate 'string ,@arg-names))))

(deftransform sb-format::format-integer ((object base stream) * * :node node)
  (delay-ir1-transform node :ir1-phases)
  `(let ((*print-base* base)
         (*print-radix* nil))
     (princ object stream)))

(deftransform sb-format::format-integer ((object base stream) (integer t t))
  `(sb-impl::%output-integer-in-base object base stream))

(deftransform pathname ((pathspec) (pathname) *)
  'pathspec)

(deftransform pathname ((pathspec) (string) *)
  '(values (parse-namestring pathspec)))

(defoptimizer (cerror optimizer) ((report control &rest args))
  (when (and (constant-lvar-p control)
             (constant-lvar-p report))
    (let ((x (lvar-value control))
          (y (lvar-value report)))
      (when (and (stringp x) (stringp y))
        (multiple-value-bind (min1 max1)
            (handler-case
                (sb-format:%compiler-walk-format-string x args)
              (sb-format:format-error (c)
                (compiler-warn "~A" c)))
          (when min1
            (multiple-value-bind (min2 max2)
                (handler-case
                    (sb-format:%compiler-walk-format-string y args)
                  (sb-format:format-error (c)
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

(deftransform error ((x &key datum expected-type format-control format-arguments))
  (if (and format-control format-arguments
           (let ((use (lvar-uses format-arguments)))
             (and (combination-p use)
                  (lvar-fun-is (combination-fun use) '(list)))))
      (cond ((and datum expected-type)
             `(apply #'%simple-type-error x datum expected-type format-control format-arguments))
            ((not (and datum expected-type))
             `(apply #'%simple-error x format-control format-arguments))
            (t
             (give-up-ir1-transform)))
      (give-up-ir1-transform)))

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

(defoptimizer (coerce derive-type) ((value type))
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
                  #+long-float
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
          (t
           result-typeoid))))))

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

(deftransform array-element-type ((array))
  (let ((type (lvar-type array)))
    (flet ((element-type (type)
             (and (array-type-p type)
                  (neq (array-type-specialized-element-type type) *wild-type*)
                  (type-specifier (array-type-specialized-element-type type)))))
      (cond ((let ((type (element-type type)))
               (and type
                    `',type)))
            ((union-type-p type)
             (let (result)
               (loop for type in (union-type-types type)
                     for et = (element-type type)
                     unless (and et
                                 (if result
                                     (equal result et)
                                     (setf result et)))
                     do (give-up-ir1-transform))
               `',result))
            ((intersection-type-p type)
             (loop for type in (intersection-type-types type)
                   for et = (element-type type)
                   when et
                   return `',et
                   finally (give-up-ir1-transform)))
            (t
             (give-up-ir1-transform))))))

(define-source-transform sb-impl::sort-vector (vector start end predicate key)
  ;; Like CMU CL, we use HEAPSORT. However, other than that, this code
  ;; isn't really related to the CMU CL code, since instead of trying
  ;; to generalize the CMU CL code to allow START and END values, this
  ;; code has been written from scratch following Chapter 7 of
  ;; _Introduction to Algorithms_ by Corman, Rivest, and Shamir.
  `(macrolet ((%index (x) `(truly-the index ,x))
              (%parent (i) `(ash ,i -1))
              (%left (i) `(%index (ash ,i 1)))
              (%right (i) `(%index (1+ (ash ,i 1))))
              (%elt (i)
                `(aref ,',vector
                       (%index (+ (%index ,i) start-1))))
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
              (%sort-vector (keyfun)
                `(let ( ;; Heaps prefer 1-based addressing.
                       (start-1 (1- ,',start))
                       (current-heap-size (- ,',end ,',start))
                       (keyfun ,keyfun))
                   (declare (type (integer -1 #.(1- most-positive-fixnum))
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
                    (%heapify 1)))))
     (declare (optimize (insert-array-bounds-checks 0) speed))
     (if (typep ,vector 'simple-vector)
         ;; (VECTOR T) is worth optimizing for, and SIMPLE-VECTOR is
         ;; what we get from (VECTOR T) inside WITH-ARRAY-DATA.
         (if (null ,key)
             ;; Special-casing the KEY=NIL case lets us avoid some
             ;; function calls.
             (%sort-vector #'identity)
             (%sort-vector ,key))
         ;; It's hard to anticipate many speed-critical applications for
         ;; sorting vector types other than (VECTOR T), so we just lump
         ;; them all together in one slow dynamically typed mess.
         (locally
             (declare (optimize (inhibit-warnings 3)))
           (%sort-vector (or ,key #'identity))))))

(deftransform sort ((list predicate &key key)
                    (list t &rest t) *)
  `(sb-impl::stable-sort-list list
                              (%coerce-callable-to-fun predicate)
                              (if key (%coerce-callable-to-fun key) #'identity)))

(deftransform sort ((vector predicate &key key)
                      (vector t &rest t) *
                      :policy (= space 0))
  (when (eq (array-type-upgraded-element-type (lvar-type vector)) *wild-type*)
    (give-up-ir1-transform))
  `(progn
     (with-array-data ((vector vector)
                       (start)
                       (end)
                       :check-fill-pointer t)
       (sb-impl::sort-vector vector
                             start end
                             (%coerce-callable-to-fun predicate)
                             (if key (%coerce-callable-to-fun key) #'identity)))
     vector))

(deftransform stable-sort ((sequence predicate &key key)
                           ((or vector list) t))
  (let ((sequence-type (lvar-type sequence)))
    (cond ((csubtypep sequence-type (specifier-type 'list))
           `(sb-impl::stable-sort-list sequence
                                       (%coerce-callable-to-fun predicate)
                                       (if key (%coerce-callable-to-fun key) #'identity)))
          ((csubtypep sequence-type (specifier-type 'simple-vector))
           `(sb-impl::stable-sort-simple-vector sequence
                                                (%coerce-callable-to-fun predicate)
                                                (and key (%coerce-callable-to-fun key))))
          (t
           `(sb-impl::stable-sort-vector sequence
                                         (%coerce-callable-to-fun predicate)
                                         (and key (%coerce-callable-to-fun key)))))))

;;;; transforms for SB-EXT:OCTETS-TO-STRING and SB-EXT:STRING-TO-OCTETS

#+sb-xc ; not needed to cross-compile
(progn
(deftransform string-to-octets ((string &key external-format (start 0) end null-terminate)
                                (t &rest t)
                                *
                                :node node)
  "precompute external-format lookup"
  (unless external-format
    (give-up-ir1-transform))
  (unless (constant-lvar-p external-format)
    (give-up-ir1-transform))
  (let ((xf-designator (lvar-value external-format)))
    (when (eql xf-designator :default)
      (give-up-ir1-transform))
    (let ((xf (get-external-format xf-designator)))
      (unless xf
        (give-up-ir1-transform))
      (let ((form `(let ((fun (load-time-value
                               (sb-impl::ef-string-to-octets-fun
                                (get-external-format ',xf-designator))
                               t))
                         (replacement
                          (load-time-value
                           (sb-impl::ef-replacement
                            (get-external-format ',xf-designator)))))
                     (sb-impl::%string-to-octets fun string start end
                                                 (if null-terminate 1 0) replacement))))
        (if (or (csubtypep (lvar-type string) (specifier-type 'simple-string))
                (policy node (> speed space)))
            `(locally (declare (inline sb-impl::%string-to-octets))
               ,form)
            form)))))
(deftransform octets-to-string ((vector &key external-format (start 0) end)
                                (t &rest t)
                                *
                                :node node)
  "precompute external-format lookup"
  (unless external-format
    (give-up-ir1-transform))
  (unless (constant-lvar-p external-format)
    (give-up-ir1-transform))
  (let ((xf-designator (lvar-value external-format)))
    (when (eql xf-designator :default)
      (give-up-ir1-transform))
    (let ((xf (get-external-format xf-designator)))
      (unless xf
        (give-up-ir1-transform))
      (let ((form `(let ((fun (load-time-value
                               (sb-impl::ef-octets-to-string-fun
                                (get-external-format ',xf-designator))
                               t))
                         (replacement
                          (load-time-value
                           (sb-impl::ef-replacement
                            (get-external-format ',xf-designator)))))
                     (sb-impl::%octets-to-string fun vector start end replacement))))
        (if (or (csubtypep (lvar-type vector) (specifier-type '(simple-array (unsigned-byte 8) (*))))
                (policy node (> speed space)))
            `(locally (declare (inline sb-impl::%octets-to-string))
               ,form)
            form)))))
) ; PROGN

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
#+sb-show
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

;;; Can fold only when time-zone is supplied.
(defoptimizer (encode-universal-time optimizer)
    ((second minute hour date month year time-zone) node)
  (when (every #'constant-lvar-p (basic-combination-args node))
    (constant-fold-call node)
    t))

#-(and win32 (not sb-thread))
(deftransform sleep ((seconds) ((integer 0 #.(expt 10 8))))
  `(if sb-impl::*deadline*
       (locally (declare (notinline sleep)) (sleep seconds))
       (sb-unix:nanosleep seconds 0)))

#-(and win32 (not sb-thread))
(deftransform sleep ((seconds)
                     ((constant-arg (or rational (not (satisfies float-infinity-p))))))
  (let ((seconds-value (lvar-value seconds)))
    (multiple-value-bind (seconds nano)
        (sb-impl::split-seconds-for-sleep seconds-value)
      (if (> seconds (expt 10 8))
          (give-up-ir1-transform)
          `(if sb-impl::*deadline*
               (locally (declare (notinline sleep)) (sleep seconds))
               (sb-unix:nanosleep ,seconds ,nano))))))

;;; Define SYMBOL-TLS-INDEX to return the byte offset of this symbol's
;;; value cell in the lisp tls area.
#+sb-thread
(define-source-transform symbol-tls-index (sym)
  #+64-bit `(ash (get-header-data ,sym) -24)
  #-64-bit `(truly-the (and fixnum unsigned-byte)
             (ash (sb-vm::%symbol-tls-index ,sym) sb-vm:n-fixnum-tag-bits)))


(deftransform make-string-output-stream ((&key element-type))
  (case (cond ((not element-type) #+sb-unicode 'character #-sb-unicode 'base-char)
              ((not (constant-lvar-p element-type)) nil)
              (t (let ((requested-type
                        (ir1-transform-specifier-type (lvar-value element-type))))
                   (cond ((eq requested-type *empty-type*) nil) ; what a loser
                         ((csubtypep requested-type (specifier-type 'base-char))
                          'base-char)
                         ((csubtypep requested-type (specifier-type 'character))
                          'character)))))
    (character `(sb-impl::%make-character-string-ostream))
    (base-char `(sb-impl::%make-base-string-ostream))
    (t (give-up-ir1-transform))))

(flet ((xform (symbol match-kind)
         (when (constant-lvar-p symbol)
           (let* ((symbol (lvar-value symbol))
                  (kind (info :variable :kind symbol))
                  (state (deprecated-thing-p 'variable symbol)))
             (when state
               (check-deprecated-thing 'variable symbol)
               (case state
                 ((:early :late)
                  (unless (gethash symbol (free-vars *ir1-namespace*))
                    (setf (gethash symbol (free-vars *ir1-namespace*)) :deprecated)))))
             ;; :global in the test below is redundant if match-kind is :global
             ;; but it's harmless and a convenient way to express this.
             ;; Note that some 3rd-party libraries use variations on DEFCONSTANT
             ;; expanding into expressions such as:
             ;;  (CL:DEFCONSTANT S (IF (BOUNDP 'S) (SYMBOL-VALUE 'S) (COMPUTE)))
             ;; which means we have to use care if S in for-evaluation position would
             ;; be converted to (LOAD-TIME-VALUE (SYMBOL-VALUE 'S)).
             ;; When S's value is directly dumpable, it works fine, but otherwise
             ;; it's dangerous. If the user wishes to avoid eager evaluation entirely,
             ;; a local notinline declaration on SYMBOL-VALUE will do.
             (when (or (eq kind match-kind)
                       (eq kind :global)
                       (and (eq kind :constant)
                            (boundp symbol)
                            (typep (symbol-value symbol) '(or character symbol
                                                           fixnum #+64-bit single-float))))
               symbol)))))
  (deftransform symbol-global-value ((symbol))
    (or (xform symbol :global)
        (give-up-ir1-transform)))
  (deftransform symbol-value ((symbol))
    (or (xform symbol :special)
        (give-up-ir1-transform))))

(deftransform symbol-value ((symbol) ((constant-arg symbol)))
  (let* ((symbol (lvar-value symbol))
         (kind (info :variable :kind symbol)))
    (if (and (eq kind :constant)
             (boundp symbol)
             (typep (symbol-value symbol) '(or character symbol
                                            fixnum #+64-bit single-float)))
        symbol
        (give-up-ir1-transform))))

(deftransform boundp ((symbol) ((constant-arg symbol)) * :policy (< safety 3))
  (if (always-boundp (lvar-value symbol))
      t
      (give-up-ir1-transform)))

(flet ((xform (symbol match-kind)
         (let* ((symbol (lvar-value symbol))
                (kind (info :variable :kind symbol)))
           (if (or (eq kind match-kind) (memq kind '(:constant :global))) ; as above
               `(setq ,symbol value)
               (give-up-ir1-transform)))))
  (deftransform set-symbol-global-value ((symbol value) ((constant-arg symbol) t))
    (xform symbol :global))
  (deftransform set ((symbol value) ((constant-arg symbol) t))
    (xform symbol :special)))

(deftransform symbol-package ((s) (symbol) *)
  (if (cast-p (lvar-uses s))
      ;; Avoid inlining a type check because %symbol-package is not
      ;; cast-externally-checkable-p
      (give-up-ir1-transform)
      `(%symbol-package s)))

(deftransforms (prin1-to-string princ-to-string) ((object) (number) * :important nil)
  `(stringify-object object))

(deftransform princ ((object &optional stream) (string &optional t) * :important nil)
  `(write-string object stream))

#+sb-thread
(progn
  (defoptimizer (sb-thread::call-with-mutex derive-type) ((function mutex))
    (let ((type (lvar-fun-type function t t)))
      (when (fun-type-p type)
        (fun-type-returns type))))

  (defoptimizer (sb-thread::call-with-mutex-timed derive-type) ((function mutex waitp timeout))
    (let ((type (lvar-fun-type function t t)))
      (when (fun-type-p type)
        (let ((null-p (not (and (constant-lvar-p waitp)
                                (lvar-value waitp)
                                (lvar-value-is timeout nil)))))
          (if null-p
              (values-type-union (fun-type-returns type)
                                 (values-specifier-type '(values null &optional)))
              (fun-type-returns type))))))

  (macrolet ((copy (to from)
               `(setf (fun-info-derive-type (fun-info-or-lose ',to))
                      (fun-info-derive-type (fun-info-or-lose ',from)))))
    (copy sb-thread::call-with-recursive-lock-timed sb-thread::call-with-mutex-timed)
    (copy sb-thread::call-with-recursive-lock sb-thread::call-with-mutex)
    (copy sb-thread::fast-call-with-mutex sb-thread::call-with-mutex)
    (copy sb-thread::fast-call-with-recursive-lock sb-thread::call-with-recursive-lock))

  (defoptimizer (sb-thread::call-with-system-mutex derive-type) ((function mutex))
    (let ((type (lvar-fun-type function t t)))
      (when (fun-type-p type)
        (fun-type-returns type))))

  (setf (fun-info-derive-type (fun-info-or-lose 'sb-thread::call-with-system-mutex/allow-with-interrupts))
        (setf (fun-info-derive-type (fun-info-or-lose 'sb-thread::call-with-system-mutex/without-gcing))
              (fun-info-derive-type (fun-info-or-lose 'sb-thread::call-with-system-mutex)))))

(defoptimizer (sb-impl::%with-standard-io-syntax derive-type) ((function))
  (let ((type (lvar-fun-type function t t)))
    (when (fun-type-p type)
      (fun-type-returns type))))

(defoptimizer (call-with-timing derive-type) ((timer function &rest arguments))
  (let ((type (lvar-fun-type function t t)))
    (when (fun-type-p type)
      (fun-type-returns type))))

(deftransform pointerp ((object))
  (let ((type (lvar-type object)))
    (cond ((csubtypep type (specifier-type '(or fixnum character #+64-bit single-float)))
           'nil)
          ((csubtypep type (specifier-type '(or symbol list instance function)))
           't)
          (t
           (give-up-ir1-transform)))))

;;; Add transforms in reverse of the order you want them tried
;;; (because of stupid semantics)
(deftransform fboundp ((symbol) (symbol))
  #+linkage-space `(sb-kernel:fdefn-fun symbol)
  #-linkage-space
  `(let ((fdefn (sb-vm::%symbol-fdefn symbol)))
     (and (not (eq fdefn 0))
          ;; On 32-bit, where %SYMBOL-FDEFN of NIL returns NIL instead of 0,
          ;; this is valid code! Watch closely:
          ;; FDEFN-FUN is "MOV EDX, [EDX+1]"
          ;; But [NIL+1] is the same as CDR of NIL, which is NIL.
          (fdefn-fun (truly-the fdefn fdefn)))))
;;; Normally we don't create fdefns by side-effect of calling FBOUNDP,
;;; but this transform is neutral in terms of the sum of code and data size.
;;; So for the cost of an FDEFN that might never store a function, the code
;;; is smaller by about the size of an fdefn; and it's faster, so do it.
#-linkage-space
(deftransform fboundp ((name) ((constant-arg symbol)))
  `(fdefn-fun (load-time-value (find-or-create-fdefn ',(lvar-value name)) t)))

;;; Remove special bindings with empty bodies
(deftransform %cleanup-point (() * * :node node)
  (let ((prev (ctran-use (node-prev (ctran-use (node-prev node))))))
    (cond ((and (combination-p prev)
                (eq (combination-fun-source-name prev nil) '%special-bind)
                (not (node-next node))
                (let ((succ (car (block-succ (node-block node)))))
                  (and succ
                       (block-start succ)
                       (let ((start-cleanup (block-start-cleanup succ)))
                         (and (neq (node-enclosing-cleanup node) start-cleanup)
                              (do-nested-cleanups (cleanup (node-block node) t)
                                (when (eq cleanup start-cleanup)
                                  (return t))
                                (when (eq (cleanup-kind cleanup) :dynamic-extent)
                                  (return))))))))
           (setf (lexenv-cleanup (node-lexenv node)) nil)
           (flush-combination prev)
           nil)
          (t
           (give-up-ir1-transform)))))

(deftransform parse-integer ((string &key (start 0) end radix junk-allowed)
                             (t &key (:radix (constant-arg (or null (member 10 16))))
                                (:start t) (:end t) (:junk-allowed t)))
  `(,(if (and radix
              (= (lvar-value radix) 16))
         'sb-impl::parse-integer16
         'sb-impl::parse-integer10)
    string start end junk-allowed))

(deftransform %coerce-to-policy ((thing) (policy))
  'thing)


(defun prev-node (node &key type (cast t))
  (let (ctran)
    (tagbody
     :next
       (setf ctran (node-prev node))
       (setf node (ctran-use ctran))
     :next-node
       (typecase node
         (ref
          (unless (eq type :non-ref)
            (return-from prev-node node)))
         (cast
          (unless cast
            (return-from prev-node node)))
         (enclose)
         (null
          (let ((pred (block-pred (ctran-block ctran))))
            (when (cdr pred)
              (return-from prev-node))
            (setf node (block-last (car pred)))
            (go :next-node)))
         (t
          (return-from prev-node
            (unless (eq type :ref)
              node))))
       (go :next))))

(defun next-node (node-or-block &key type (cast t) single-predecessor
                                     strict)
  (let ((node node-or-block)
        ctran)
    (tagbody
       (when (block-p node-or-block)
         (when (and single-predecessor
                    (cdr (block-pred node-or-block)))
           (return-from next-node))
         (setf ctran (block-start node-or-block))
         (go :next-ctran))
     :next
       (setf ctran (node-next node))
     :next-ctran
       (cond (ctran
              (setf node (ctran-next ctran))
              (typecase node
                (ref (unless (eq type :non-ref)
                       (return-from next-node node)))
                (cast
                 (when (or strict
                           (not cast))
                   (return-from next-node node)))
                (enclose
                 (when strict
                   (return-from next-node node)))
                (t (return-from next-node
                     (unless (eq type :ref)
                       node))))
              (go :next))
             (t
              (let* ((succ (first (block-succ (node-block node))))
                     (start (block-start succ)))
                (when (and start
                           (not (and single-predecessor
                                     (cdr (block-pred succ)))))
                  (setf ctran start)
                  (go :next-ctran))))))))

(defun next-block (node)
  (and (not (node-next node))
       (car (block-succ (node-block node)))))

(defun range-transform (op a b node)
  (unless (delay-ir1-optimizer node :ir1-phases)
    (let ((if (node-dest node)))
      (flet ((try (consequent alternative)
               (let ((then (next-node consequent :type :non-ref
                                                 :single-predecessor t)))
                 (when (and (combination-p then)
                            (eq (combination-kind then) :known)) ;; no notinline
                   (let ((op2 (combination-fun-debug-name then)))
                     (when (memq op2 '(< <= > >=))
                       (flet ((try2 (&optional reverse-if)
                                (let ((a a)
                                      (b b)
                                      (op op)
                                      (op2 op2)
                                      (after-then))
                                  (destructuring-bind (a2 b2) (combination-args then)
                                    (when (and (cond ((same-leaf-ref-p a a2))
                                                     ((same-leaf-ref-p a b2)
                                                      (rotatef a2 b2)
                                                      (setf op2 (invert-operator op2)))
                                                     ((same-leaf-ref-p b a2)
                                                      (rotatef a b)
                                                      (setf op (invert-operator op))))
                                               (memq op2
                                                     (case op
                                                       ((< <=) '(> >=))
                                                       ((> >=) '(< <=))))
                                               (and (if-p (setf after-then (next-node then)))
                                                    (eq alternative
                                                        (if reverse-if
                                                            (if-consequent after-then)
                                                            (if-alternative after-then)))))
                                      (let* ((integerp (csubtypep (lvar-type a) (specifier-type 'integer)))
                                             (form
                                               (cond ((when (and integerp
                                                                 (constant-lvar-p b)
                                                                 (constant-lvar-p b2))
                                                        (let ((b (lvar-value b))
                                                              (b2 (lvar-value b2)))
                                                          (multiple-value-bind (l h)
                                                              (case op
                                                                (>=
                                                                 (if (eq op2 '<=)
                                                                     (values b b2)
                                                                     (values b (1- b2))))
                                                                (<=
                                                                 (if (eq op2 '>=)
                                                                     (values b2 b)
                                                                     (values (1+ b2) b)))
                                                                (>
                                                                 (if (eq op2 '<=)
                                                                     (values (1+ b) b2)
                                                                     (values (1+ b) (1- b2))))
                                                                (<
                                                                 (if (eq op2 '>=)
                                                                     (values b2 (1- b))
                                                                     (values (1+ b2) (1- b)))))
                                                            (cond ((not l) nil)
                                                                  ((and (= l most-negative-fixnum)
                                                                        (= h most-positive-fixnum))
                                                                   `(fixnump x))
                                                                  ((and (= l 0)
                                                                        (= h most-positive-word))
                                                                   `(#-64-bit unsigned-byte-32-p #+64-bit unsigned-byte-64-p
                                                                     x))
                                                                  ((and (= l (- (expt 2 (1- sb-vm:n-word-bits))))
                                                                        (= h (1- (expt 2 (1- sb-vm:n-word-bits)))))
                                                                   `(#-64-bit signed-byte-32-p #+64-bit signed-byte-64-p
                                                                     x)))))))
                                                     ((not (and (csubtypep (lvar-type b) (specifier-type 'fixnum))
                                                                (csubtypep (lvar-type b2) (specifier-type 'fixnum))))
                                                      nil)
                                                     ((or (not integerp)
                                                          (and (vop-existsp :translate range<)
                                                               (or (vop-existsp :named range<)
                                                                   (and (constant-lvar-p b)
                                                                        (constant-lvar-p b2)))))
                                                      `(,(case op
                                                           (>=
                                                            (case op2
                                                              (<= 'range<=)
                                                              (< 'range<=<)))
                                                           (>
                                                            (case op2
                                                              (<= 'range<<=)
                                                              (< 'range<)))
                                                           (<=
                                                            (case op2
                                                              (>= 'range<=)
                                                              (> 'range<<=)))
                                                           (<
                                                            (case op2
                                                              (>= 'range<=<)
                                                              (> 'range<))))
                                                        l x h))
                                                     ((csubtypep (lvar-type a) (specifier-type 'fixnum))
                                                      nil)
                                                     (t
                                                      `(and (fixnump x)
                                                            ,(case op
                                                               (>=
                                                                (case op2
                                                                  (<= '(<= l (truly-the fixnum x) h))
                                                                  (< '(and (<= l (truly-the fixnum x)) (< (truly-the fixnum x) h)))))
                                                               (>
                                                                (case op2
                                                                  (<= '(and (< l (truly-the fixnum x)) (<= (truly-the fixnum x) h)))
                                                                  (< '(< l (truly-the fixnum x) h))))
                                                               (<=
                                                                (case op2
                                                                  (>= '(<= l (truly-the fixnum x) h))
                                                                  (> '(and (< l (truly-the fixnum x)) (<= (truly-the fixnum x) h)))))
                                                               (<
                                                                (case op2
                                                                  (>= '(and (<= l (truly-the fixnum x)) (< (truly-the fixnum x) h)))
                                                                  (> '(< l (truly-the fixnum x) h))))))))))
                                        (when form
                                          (kill-if-branch-1 if (if-test if)
                                                            (node-block if)
                                                            alternative)
                                          (setf (combination-args node) nil)
                                          (setf (lvar-dest b) then
                                                (lvar-dest a) then)
                                          (flush-combination node)
                                          (setf (combination-args then)
                                                (case op
                                                  ((>= >)
                                                   (list b a b2))
                                                  (t
                                                   (list b2 a b))))
                                          (flush-dest a2)
                                          (transform-call then
                                                          `(lambda (l x h)
                                                             (declare (ignorable l h))
                                                             ,(if reverse-if
                                                                  `(not ,form)
                                                                  form))
                                                          'range<))
                                        t))))))
                         (cond ((try2))
                               (t
                                (setf op2 (not-operator op2))
                                (try2 t))))))))))
        (when (and (if-p if)
                   (immediately-used-p (node-lvar node) node t))
          (cond ((try (if-consequent if) (if-alternative if)))
                (t
                 ;; Deal with (not (< .. ...)) which is transformed from >=.
                 (setf op (not-operator op))
                 (try (if-alternative if) (if-consequent if)))))))))

(defoptimizer (> optimizer) ((a b) node)
  (range-transform '> a b node))

(defoptimizer (< optimizer) ((a b) node)
  (range-transform '< a b node))

(defoptimizer (>= optimizer) ((a b) node)
  (range-transform '>= a b node))

(defoptimizer (<= optimizer) ((a b) node)
  (range-transform '<= a b node))

(when-vop-existsp (:translate check-range<=)
  (deftransform check-range<= ((l x h) (t sb-vm:signed-word t) * :important nil)
    `(range<= l x h))
  (deftransform check-range<= ((l x h) (t sb-vm:word t) * :important nil)
    `(range<= l x h))

  (deftransform check-range<=
      ((l x h) ((constant-arg fixnum) t (constant-arg fixnum)) * :important nil)
    (let* ((type (lvar-type x))
           (l (lvar-value l))
           (h (lvar-value h))
           (range-type (specifier-type `(integer ,l ,h)))
           (intersect (type-intersection type (specifier-type 'fixnum))))
      (cond ((eq intersect *empty-type*)
             nil)
            ((csubtypep intersect range-type)
             `(fixnump x))
            ((and (< l 0)
                  (csubtypep intersect
                             (specifier-type 'unsigned-byte)))
             `(check-range<= 0 x ,h))
            ((let ((int (type-approximate-interval intersect)))
               (when int
                 (let ((power-of-two (1- (ash 1 (integer-length (interval-high int))))))
                   (when (< 0 power-of-two h)
                     `(check-range<= l x ,power-of-two))))))
            (t
             (give-up-ir1-transform)))))

  (macrolet ((def (name ld hd)
               `(deftransform ,name ((l x h) ((constant-arg fixnum) integer (constant-arg fixnum)) * :important nil)
                  (let* ((type (lvar-type x))
                         (l (+ (lvar-value l) ,ld))
                         (h (+ (lvar-value h) ,hd))
                         (range-type (specifier-type `(integer ,l ,h)))
                         (unsigned-type (type-intersection type (specifier-type 'unsigned-byte))))
                    (cond ((and (neq unsigned-type *empty-type*)
                                (csubtypep unsigned-type
                                           range-type))
                           `(>= x l))
                          ((and (< l 0)
                                (csubtypep type
                                           (specifier-type 'unsigned-byte)))
                           `(range<= 0 x ,h))
                          (t
                           (give-up-ir1-transform)))))))
    (def range<= 0 0)
    (def range< 1 -1)
    (def range<<= 1 0)
    (def range<=< 0 -1)))

(defun find-or-chains (node op)
  (let ((chains (make-array 1 :adjustable t :fill-pointer 1 :initial-element nil)))
    (labels ((chain (node)
               (unless (combination-or-chain-computed node)
                 (let ((if (node-dest node)))
                   (when (and (if-p if)
                              (immediately-used-p (node-lvar node) node t))
                     (destructuring-bind (a b) (combination-args node)
                       (when (and (constant-lvar-p b)
                                  (let ((value (lvar-value b)))
                                    (or (fixnump value)
                                        (symbolp value)
                                        (characterp value))))
                         (push (list (lvar-value b) node if)
                               (aref chains (1- (length chains))))
                         (setf (combination-or-chain-computed node) t)
                         (let ((else (next-node (if-alternative if) :type :non-ref
                                                                    :single-predecessor t)))
                           (when (and (combination-p else)
                                      (only-harmless-cleanups (node-block if)
                                                              (if-alternative if))
                                      (eq (combination-kind else) :known)) ;; no notinline
                             (let ((op2 (combination-fun-debug-name else))
                                   after-else)
                               (when (eq op2 op)
                                 (let ((a2 (car (combination-args else))))
                                   (when (and (same-leaf-ref-p a a2)
                                              (if-p (setf after-else (next-node else))))
                                     (unless (eq (if-consequent if)
                                                 (if-consequent after-else))
                                       (vector-push-extend nil chains))
                                     (chain else))))))))))))))
      (chain node)
      (map-into chains #'nreverse chains))))

(defun contiguous-sequence (sorted-values)
  (when (loop for i below (1- (length sorted-values))
              for a = (svref sorted-values i)
              always (= (1+ a) (svref sorted-values (1+ i))))
    (values (svref sorted-values 0)
            (svref sorted-values (1- (length sorted-values))))))

(defun bit-test-sequence (sorted-values)
  (let ((min (elt sorted-values 0))
        (max (elt sorted-values (1- (length sorted-values)))))
    (and (>= min 0) ;; negative numbers can be handled too
         (< (- max min) sb-vm:n-word-bits)
         (values min max))))

(defun or-eq-transform-p (values)
  (and (> (length values) 1)
       (let (fixnump
             characterp)
         (map nil (lambda (value)
                    (unless
                        (cond (fixnump
                               (fixnump value))
                              (characterp
                               (characterp value))
                              ((fixnump value)
                               (setf fixnump t))
                              ((characterp value)
                               (setf characterp t)))
                      (return-from or-eq-transform-p)))
              values)
         (let ((values (sort (map 'vector (if characterp
                                              #'char-code
                                              #'identity)
                                  values)
                             #'<)))
           (or (contiguous-sequence values)
               (bit-test-sequence values))))))

(defun replace-chain (chain form &optional kill-last)
  (let* ((node (second (first chain)))
         (lvar (first (combination-args node))))
    (flush-dest (second (combination-args node)))
    (setf (combination-args node) nil)
    (loop for ((nil node if) next) on chain
          for (a2 b2) = (combination-args node)
          do
          (cond (next
                 (kill-if-branch-1 if (if-test if)
                                   (node-block if)
                                   (if-consequent if))
                 (flush-combination node))
                (t
                 (setf (lvar-dest lvar) node)
                 (setf (combination-args node)
                       (list lvar b2))
                 (flush-dest a2)
                 (transform-call node
                                 form
                                 'or-eq-transform)
                 (when kill-last
                   (kill-if-branch-1 if (if-test if)
                                     (node-block if)
                                     (if-consequent if))))))))

(defun single-or-chain (chain)
  (let* ((node (second (first chain)))
         (lvar (first (combination-args node)))
         (characterp)
         (fixnump)
         (constants (sort (map 'vector
                               (lambda (e)
                                 (let ((value (first e)))
                                   (or
                                    (cond (fixnump
                                           (and (fixnump value)
                                                value))
                                          (characterp
                                           (and (characterp value)
                                                (char-code value)))
                                          ((fixnump value)
                                           (setf fixnump t)
                                           value)
                                          ((characterp value)
                                           (setf characterp t)
                                           (char-code value)))
                                    (return-from single-or-chain))))
                               chain)
                          #'<))
         (type-check (if characterp
                         (not (csubtypep (lvar-type lvar) (specifier-type 'character)))
                         (not (csubtypep (lvar-type lvar) (specifier-type 'fixnum)))))
         (range-check (if (and type-check
                               (not characterp)
                               (vop-existsp :translate check-range<=))
                          'check-range<=
                          '<=)))
    (flet ((type-check (check-fixnum form)
             (if (and type-check
                      (or (not (vop-existsp :translate check-range<=))
                          characterp
                          check-fixnum))
                 `(when (,(if characterp
                              'characterp
                              'fixnump)
                         a)
                    (let ((a (truly-the ,(if characterp
                                             'character
                                             'fixnum)
                                        a)))
                      ,form))
                 form)))
      ;; Transform contiguous ranges into range<=.
      (when (or (vop-existsp :translate range<)
                (> (length constants) 2))
        (multiple-value-bind (min max)
            (contiguous-sequence constants)
          (when min
            (replace-chain chain
                           `(lambda (a b)
                              (declare (ignore b))
                              ,(type-check nil
                                           `(,range-check ,min ,(if characterp
                                                                    '(char-code a)
                                                                    'a)
                                                          ,max))))
            (return-from single-or-chain t))))
      ;; Turn into a bit mask
      (multiple-value-bind (min max)
          (and (> (length constants)
                  (if type-check
                      3
                      2))
               (bit-test-sequence constants))
        (when min
          (replace-chain chain
                         `(lambda (a b)
                            (declare (ignore b))
                            ,(type-check
                              (>= max sb-vm:n-word-bits)
                              `(let ((a ,(if characterp
                                             '(char-code a)
                                             'a)))
                                 ,(cond ((< max sb-vm:n-word-bits)
                                         (let ((interval (type-approximate-interval (lvar-type lvar) t)))
                                           (when (and interval
                                                      (interval-high interval)
                                                      (< (interval-high interval) sb-vm:n-word-bits))
                                             (setf max (interval-high interval))))
                                         `(and (,range-check 0 a ,max)
                                               (logbitp (truly-the (integer 0 ,max) a)
                                                        ,(reduce (lambda (x y) (logior x (ash 1 y)))
                                                                 constants :initial-value 0))))
                                        (t
                                         (decf max min)
                                         `(let ((a (- a ,min)))
                                            (and (<= 0 a ,max)
                                                 (logbitp (truly-the (integer 0 ,max) a)
                                                          ,(reduce (lambda (x y) (logior x (ash 1 (- y min))))
                                                                   constants :initial-value 0))))))))))
          (return-from single-or-chain t)))

      (labels ((%one-bit-diff-p (c1 c2)
                 (and (= (ash c1 (- sb-vm:n-word-bits)) ;; same sign
                         (ash c2 (- sb-vm:n-word-bits)))
                      (= (logcount (logxor c1 c2)) 1)))
               (one-bit-diff-p (c1 c2)
                 (or (%one-bit-diff-p c1 c2)
                     ;; If the difference is a single bit
                     ;; then it can be masked off and compared to 0.
                     (let ((c1 (min c1 c2))
                           (c2 (max c1 c2)))
                       (= (logcount (- c2 c1)) 1)))))
        ;; Comparing integers that differ by only one bit,
        ;; which is useful for case-insensitive comparison of ASCII characters.
        (loop for ((c1 node if) (c2 next-node next-if)) = chain
              while next-node
              do
              (pop chain)
              (destructuring-bind (a b) (combination-args node)
                (destructuring-bind (a2 b2) (combination-args next-node)
                  (let* ((c1-orig c1)
                         (c2-orig c2))
                    (when (and (if characterp
                                   (and (characterp c1)
                                        (characterp c2)
                                        (setf c1 (char-code c1)
                                              c2 (char-code c2)))
                                   (and (fixnump c1)
                                        (fixnump c2)))
                               (one-bit-diff-p c1 c2))
                      (pop chain)
                      (kill-if-branch-1 if (if-test if)
                                        (node-block if)
                                        (if-consequent if))
                      (setf (combination-args node) nil)
                      (flush-combination node)
                      (setf (lvar-dest a) next-node)
                      (setf (combination-args next-node)
                            (list a b2))
                      (flush-dest a2)
                      (flush-dest b)
                      (let* ((value (cond (type-check
                                           ;; Operate on tagged values
                                           (cond (characterp
                                                  (setf c1 (get-lisp-obj-address c1-orig)
                                                        c2 (get-lisp-obj-address c2-orig))
                                                  '(get-lisp-obj-address a))
                                                 (t
                                                  (setf c1 (ash c1 sb-vm:n-fixnum-tag-bits)
                                                        c2 (ash c2 sb-vm:n-fixnum-tag-bits))
                                                  '(mask-signed-field sb-vm:n-word-bits (get-lisp-obj-address a)))))
                                          (characterp
                                           '(char-code a))
                                          (t
                                           'a)))
                             (min (min c1 c2))
                             (max (max c1 c2)))
                        (transform-call next-node
                                        `(lambda (a b)
                                           (declare (ignore b))
                                           ,(cond ((%one-bit-diff-p c1 c2)
                                                   (if (zerop min)
                                                       `(not (logtest ,value ,(lognot (logxor c1 c2))))
                                                       `(eq (logandc2 ,value
                                                                      ,(logxor c1 c2))
                                                            ,min)))
                                                  (t
                                                   `(not (logtest (,@(if type-check
                                                                         '(logand most-positive-word)
                                                                         '(mask-signed-field sb-vm:n-fixnum-bits)) (- ,value ,min))
                                                                  ,(lognot (- max min)))))))
                                        'or-eq-transform)))))))))))

;;; Prevent slow perfect hash finder from hogging time if time spent compiling
;;; is strictly more important than execution speed
(defun slow-findhash-allowed (node)
  (policy node (>= speed compilation-speed)))

(defun or-eq-to-aref (keys key-lists targets last-if chains otherwise)
  (let (constant-targets
        constant-refs
        constant-target
        (ref (next-node (if-consequent last-if) :strict t)))
    (when (and (ref-p ref)
               (constant-p (ref-leaf ref)))
      (let ((lvar (node-lvar ref)))
        (setf constant-target (next-block ref))
        (when lvar
          (loop with constant
                for keys in key-lists
                for target in targets
                for ref = (next-node target :strict t)
                do (unless (and (ref-p ref)
                                (eq lvar (node-lvar ref))
                                (constant-p (setf constant (ref-leaf ref)))
                                (eq constant-target
                                    (next-block ref))
                                (typep (constant-value constant)
                                       '(or symbol number character (and array (not (array t))))))
                     (setf constant-targets nil)
                     (return))
                   (when (and (eq (ctran-kind (node-prev ref)) :block-start)
                              (= (length (block-pred (node-block ref)))
                                 (length keys)))
                     (push ref constant-refs))
                   (push (constant-value constant) constant-targets)))))
    (when constant-targets
      (let ((code (and (slow-findhash-allowed last-if)
                       (expand-hash-case-for-jump-table keys key-lists nil
                                                   (coerce (nreverse constant-targets) 'vector)
                                                   otherwise))))
        (when code
          (replace-chain (reduce #'append chains)
                         `(lambda (key b)
                            (declare (ignore b))
                            (to-lvar ,(node-lvar ref)
                                     ,(or constant-target
                                          (node-ends-block ref))
                                     (the* (,(lvar-type (node-lvar ref)) :truly t)
                                           ,code))))
          (loop for ref in constant-refs
                do
                (delete-ref ref)
                (unlink-node ref))
          t)))))

(defun or-eq-to-jump-table (chains node)
  (let* (keys
         targets
         last-if
         (key-lists
           (loop for chain across chains
                 when chain
                 do
                 (push (if-consequent (third (first chain))) targets)
                 and
                 collect (loop for (key node if) in chain
                               do
                               (when (memq key keys)
                                 (return-from or-eq-to-jump-table))
                               (push key keys)
                               (setf last-if if)
                               collect key)))
         (targets (nreverse targets))
         (lvar (first (combination-args node)))
         (otherwise (and last-if
                         (if-alternative last-if))))
    (cond ((not (and keys
                     (sb-impl::should-attempt-hash-based-case-dispatch keys)
                     (not (key-lists-for-or-eq-transform-p key-lists))))
           nil)
          ((suitable-jump-table-keys-p node keys)
           (replace-chain (reduce #'append chains)
                          `(lambda (key b)
                             (declare (ignore b))
                             (jump-table ,(if (characterp (first keys))
                                              `(if-to-blocks (characterp key)
                                                             (char-code (truly-the character key))
                                                             ,(if-alternative last-if))
                                              `(if-to-blocks (fixnump key)
                                                             (truly-the fixnum key)
                                                             ,(if-alternative last-if)))
                                         ,@(loop for group in key-lists
                                                 for target in targets
                                                 append (loop for key in group
                                                              collect (cons (if (characterp key)
                                                                                (char-code key)
                                                                                key)
                                                                            target)))

                                         (otherwise . ,otherwise)))
                          t)
           t)
          ((let ((diff (type-difference (lvar-type lvar)
                                        (specifier-type `(member ,@keys)))))
             ;; If it's an exhaustive case add the missing case back,
             ;; that way the hash doesn't need to be checked for collisions.
             (multiple-value-bind (p value) (type-singleton-p diff)
               (when (and p
                          (typecase (car keys)
                            (sb-xc:fixnum (fixnump value))
                            (symbol (symbolp value))
                            (character (characterp value))))
                 (push value keys)
                 (setf targets (append targets (list otherwise))
                       key-lists (append key-lists
                                         (list (list value)))
                       otherwise nil)))
             nil))
          ((or-eq-to-aref keys key-lists targets last-if chains otherwise))
          (t
           (multiple-value-bind (code new-targets)
               (and (slow-findhash-allowed node)
                    (expand-hash-case-for-jump-table keys key-lists
                                                     (append targets
                                                             (and otherwise
                                                                  (list (cons 'otherwise
                                                                              otherwise))))))
             (when code
               (replace-chain (reduce #'append chains)
                              `(lambda (key b)
                                 (declare (ignore b))
                                 ,(if new-targets
                                      `(jump-table ,code
                                                   ,@new-targets
                                                   ,@(and otherwise
                                                          `((otherwise . ,otherwise))))
                                      code))
                              t))
             t)))))

;;; Do something when comparing the same value to multiple things.
(defun or-eq-transform (op a b node)
  (declare (ignorable b))
  (unless (delay-ir1-optimizer node :ir1-phases)
    (when (types-equal-or-intersect (lvar-type a) (specifier-type '(or character
                                                                    fixnum
                                                                    symbol)))
      (let ((chains (find-or-chains node op)))
        (or (and (policy node (> jump-table 0))
                 (vop-existsp :named jump-table)
                 (or-eq-to-jump-table chains node))
            (loop for chain across chains
                  do (when (cdr chain)
                       (single-or-chain chain))))
        (unless (node-prev node)
          ;; Don't proceed optimizing this node
          t)))))


(defoptimizer (eq optimizer) ((a b) node)
  (or-eq-transform 'eq a b node))

(defoptimizer (char= optimizer) ((a b) node)
  (or-eq-transform 'char= a b node))

(defun change-jump-table-targets (jump-table new-targets)
  (let* ((block (node-block jump-table)))
    (loop for (nil . target) in new-targets
          unless (memq target (block-succ block))
          do (link-blocks block target))
    (loop for succ in (block-succ block)
          unless (find succ new-targets :key #'cdr)
          do (unlink-blocks block succ))
    (setf (jump-table-targets jump-table) new-targets)
    (reoptimize-node jump-table)))

;;; Return T if KEYS are all of a type that is _directly_ amenable to being
;;; the branch selector in the jump-table vop, and are within a sufficiently
;;; dense range that the resulting table of assembler labels would be reasonably full.
;;; Finally, ensure that any operand encoding restrictions would be adhered to.
(defun suitable-jump-table-keys-p (node keys)
  (unless keys
    (return-from suitable-jump-table-keys-p nil))
  (cond ((every #'fixnump keys))
        ((every #'characterp keys) (setq keys (mapcar #'char-code keys)))
        (t (return-from suitable-jump-table-keys-p nil)))
  (when (policy node (= jump-table 3)) ;; trust it
    (return-from suitable-jump-table-keys-p t))
  ;; There could be a backend-aware aspect to the decision about whether to
  ;; convert to a jump table.
  (flet ((can-encode (min max)
           (declare (ignorable min max))
           #+(or ppc ppc64)
           (and (typep (sb-vm:fixnumize min) '(signed-byte 16))
                (typep (sb-vm:fixnumize (- max min)) '(signed-byte 16)))
           #+(or x86 x86-64 arm64) t))
    (let* ((min (reduce #'min keys))
           (max (reduce #'max keys))
           (table-size (1+ (- max min)))
           ;; TOOD: this size could be reduced now. For spread-out fixnum keys,
           ;; we'll use a perfect hash, making the table exactly sized.
           ;; So the situation where low load factor is beneficial are few.
           (size-limit (* (length keys) 2)))
      ;; Don't waste too much space, e.g. {5,6,10,20} would require 16 words
      ;; for 4 entries, which is excessive.
      (and (<= table-size size-limit)
           (can-encode min max)))))

(defun expand-hash-case-for-jump-table (keys key-lists targets &optional constants default)
  (let* ((phash-lexpr (or (perfectly-hashable keys)
                          (return-from expand-hash-case-for-jump-table (values nil nil))))
         (temp '#1=#:key)               ; GENSYM considered harmful
         (object-hash (prehash-for-perfect-hash temp keys))
         (hashfn
           (compile-perfect-hash
            `(lambda (,temp) (,phash-lexpr ,object-hash))
            (coerce keys 'vector)))
         (result-vector
           (make-array (length (if constants
                                   keys key-lists))
                       :initial-element nil))
         (key-vector (make-array (length keys)
                                 :element-type
                                 (cond #+sb-unicode ((every #'base-char-p keys) 'base-char)
                                       ((every #'characterp keys) 'character)
                                       (t 't))))
         (new-targets))
    (loop for key-list in key-lists
          for target = (pop targets)
          for index from 0
          do (dolist (key key-list)
               (let ((phash (funcall hashfn key)))
                 (setf (aref key-vector phash) key)
                 (cond (constants
                        (setf (aref result-vector phash) (aref constants index)))
                       (t
                        (push (cons phash target) new-targets)
                        (push phash (aref result-vector index)))))))
    (when (simple-vector-p keys)
      (setq keys (coerce-to-smallest-eltype keys)))
    (let* ((typed-h `(truly-the (mod ,(length (if constants
                                                  result-vector
                                                  key-vector))) h))
           (first-target (and new-targets
                              (cdar new-targets)))
           (same-targets (and new-targets
                              (loop for (nil . block) in (cdr new-targets)
                                    always (eq block first-target)))))
      (values `(let* ((#1# key)
                      (h (,phash-lexpr ,object-hash)))
                 ;; EQL reduces to EQ for all object this expanders accepts as keys
                 ,(if constants
                      (if default
                          `(if-to-blocks (and (< h ,(length key-vector))
                                              (eq (aref ,key-vector ,typed-h) #1#))
                                         ,(let ((all-equal (not (position (aref result-vector 0) result-vector :test-not #'eql))))
                                            (if all-equal
                                                `',(aref result-vector 0)
                                                `(aref ,result-vector ,typed-h)))
                                         ,default)
                          `(aref ,result-vector (truly-the (mod ,(length result-vector)) h)))
                      (let ((otherwise (cdr (assoc 'otherwise targets))))
                        (if otherwise
                            `(if-to-blocks
                              (and (< h ,(length key-vector))
                                   (eq (aref ,key-vector ,typed-h) #1#))
                              ,(if same-targets
                                   first-target
                                   typed-h)
                              ,otherwise)
                            typed-h))))
              (unless same-targets
                new-targets)))))

(defun key-lists-for-or-eq-transform-p (key-lists)
  (and (= (length key-lists) 1)
       (or-eq-transform-p (first key-lists))))

(defun ensure-lvar-fun-form (lvar lvar-name &key (coercer '%coerce-callable-to-fun)
                                                 give-up
                                                 node)
  (aver (and lvar-name (symbolp lvar-name)))
  (if (csubtypep (lvar-type lvar) (specifier-type 'function))
      lvar-name
      (let ((cname (lvar-constant-global-fun-name lvar)))
        (cond (cname
               (when node
                 (record-late-xref :calls cname node))
               (if (lvar-annotations lvar)
                   `(with-annotations ,(lvar-annotations lvar)
                      (global-function ,cname))
                   `(global-function ,cname)))
              (give-up
               (give-up-ir1-transform
                ;; No ~S here because if fallback is shown, it wants no quotes.
                "~A is not known to be a function"
                ;; LVAR-NAME is not what to show - if only it were that easy.
                (source-variable-or-else lvar "callable expression")))
              (t
               `(,coercer ,lvar-name))))))

(deftransform %coerce-callable-to-fun ((thing) * * :node node)
  "optimize away possible call to FDEFINITION at runtime"
  (ensure-lvar-fun-form thing 'thing :give-up t :node node))

;;; Behaves just like %COERCE-CALLABLE-TO-FUN but has an ir2-convert optimizer.
(deftransform %coerce-callable-for-call ((thing) * * :node node)
  "optimize away possible call to FDEFINITION at runtime"
  (ensure-lvar-fun-form thing 'thing :give-up t :coercer '%coerce-callable-for-call :node node))

(define-source-transform %coerce-callable-to-fun (thing &environment env)
  (ensure-source-fun-form thing env :give-up t))

;;; Change 'f to #'f
(defoptimizer (%coerce-callable-for-call optimizer) ((fun) node)
  (let ((uses (lvar-uses fun)))
    (when (consp uses)
      (loop for use in uses
            when (ref-p use)
            do (let ((leaf (ref-leaf use)))
                 (when (and (constant-p leaf)
                            (or (internal-name-p (constant-value leaf))
                                (almost-immediately-used-p fun use)))
                   (let ((name (constant-value leaf)))
                     (record-late-xref :calls name use)
                     (change-ref-leaf use (find-global-fun name t) :recklessly t)))))))
  nil)

(defoptimizer (open derive-type) ((filename
                                   &key
                                   direction
                                   if-exists
                                   if-does-not-exist
                                   &allow-other-keys))
  (when (and (or (not if-exists)
                 (not (types-equal-or-intersect (lvar-type if-exists) (specifier-type 'null))))
             (or (not if-does-not-exist)
                 (not (types-equal-or-intersect (lvar-type if-does-not-exist) (specifier-type 'null))))
             (or (not direction)
                 if-does-not-exist
                 (not (types-equal-or-intersect (lvar-type direction) (specifier-type '(eql :probe))))))
    (specifier-type 'stream)))
