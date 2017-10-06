;;;; functions to implement lists

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Limitation: no list might have more than INDEX conses.

;;;; KLUDGE: comment from CMU CL, what does it mean?
;;;;   NSUBLIS, things at the beginning broken.
;;;; -- WHN 20000127

(declaim (maybe-inline
          tree-equal %setnth nthcdr
          tailp union
          nunion intersection nintersection set-difference nset-difference
          set-exclusive-or nset-exclusive-or subsetp acons
          subst subst-if
          ;; NSUBLIS is >400 lines of assembly. How is it helpful to inline?
          subst-if-not nsubst nsubst-if nsubst-if-not sublis nsublis))

;;; These functions perform basic list operations.
(defun car (list) "Return the 1st object in a list." (car list))
(defun cdr (list)
  "Return all but the first object in a list."
  (cdr list))
(defun cadr (list) "Return the 2nd object in a list." (cadr list))
(defun cdar (list) "Return the cdr of the 1st sublist." (cdar list))
(defun caar (list) "Return the car of the 1st sublist." (caar list))
(defun cddr (list)
  "Return all but the 1st two objects of a list."
  (cddr list))
(defun caddr (list)
  "Return the 1st object in the cddr of a list."
  (caddr list))
(defun caadr (list)
  "Return the 1st object in the cadr of a list."
  (caadr list))
(defun caaar (list)
  "Return the 1st object in the caar of a list."
  (caaar list))
(defun cdaar (list)
  "Return the cdr of the caar of a list."
  (cdaar list))
(defun cddar (list)
  "Return the cdr of the cdar of a list."
  (cddar list))
(defun cdddr (list)
  "Return the cdr of the cddr of a list."
  (cdddr list))
(defun cadar (list)
  "Return the car of the cdar of a list."
  (cadar list))
(defun cdadr (list)
  "Return the cdr of the cadr of a list."
  (cdadr list))
(defun caaaar (list)
  "Return the car of the caaar of a list."
  (caaaar list))
(defun caaadr (list)
  "Return the car of the caadr of a list."
  (caaadr list))
(defun caaddr (list)
  "Return the car of the caddr of a list."
  (caaddr list))
(defun cadddr (list)
  "Return the car of the cdddr of a list."
  (cadddr list))
(defun cddddr (list)
  "Return the cdr of the cdddr of a list."
  (cddddr list))
(defun cdaaar (list)
  "Return the cdr of the caaar of a list."
  (cdaaar list))
(defun cddaar (list)
  "Return the cdr of the cdaar of a list."
  (cddaar list))
(defun cdddar (list)
  "Return the cdr of the cddar of a list."
  (cdddar list))
(defun caadar (list)
  "Return the car of the cadar of a list."
  (caadar list))
(defun cadaar (list)
  "Return the car of the cdaar of a list."
  (cadaar list))
(defun cadadr (list)
  "Return the car of the cdadr of a list."
  (cadadr list))
(defun caddar (list)
  "Return the car of the cddar of a list."
  (caddar list))
(defun cdaadr (list)
  "Return the cdr of the caadr of a list."
  (cdaadr list))
(defun cdadar (list)
  "Return the cdr of the cadar of a list."
  (cdadar list))
(defun cdaddr (list)
  "Return the cdr of the caddr of a list."
  (cdaddr list))
(defun cddadr (list)
  "Return the cdr of the cdadr of a list."
  (cddadr list))
(defun cons (se1 se2)
  "Return a list with SE1 as the CAR and SE2 as the CDR."
  (cons se1 se2))

(declaim (maybe-inline tree-equal-test tree-equal-test-not))

(defun tree-equal-test-not (x y test-not)
  (declare (type function test-not))
  (cond ((consp x)
         (and (consp y)
              (tree-equal-test-not (car x) (car y) test-not)
              (tree-equal-test-not (cdr x) (cdr y) test-not)))
        ((consp y) nil)
        ((not (funcall test-not x y)) t)
        (t ())))

(defun tree-equal-test (x y test)
  (declare (type function test))
  (cond ((consp x)
         (and (consp y)
              (tree-equal-test (car x) (car y) test)
              (tree-equal-test (cdr x) (cdr y) test)))
        ((consp y) nil)
        ((funcall test x y) t)
        (t ())))

(defun tree-equal-eql (x y)
  (labels ((recurse (x y)
             (if (eq x y)
                 t
                 (do ((x x (cdr x))
                      (y y (cdr y)))
                     ((or (atom x)
                          (atom y))
                      (eql x y))
                   (cond ((consp (car x))
                          (when (atom (car y))
                            (return-from tree-equal-eql))
                          (recurse (car x) (car y)))
                         ((not (eql (car x) (car y)))
                          (return-from tree-equal-eql)))))))
    (recurse x y)))

(defun tree-equal (x y &key (test nil testp) (test-not nil notp))
  "Return T if X and Y are isomorphic trees with identical leaves."
  (declare (explicit-check))
  (cond (notp
         (when testp
           (error ":TEST and :TEST-NOT were both supplied."))
         (tree-equal-test-not x y (%coerce-callable-to-fun test-not)))
        ((or (not test)
             (eql test #'eql)
             (eql test 'eql))
         (tree-equal-eql x y))
        (t
         (tree-equal-test x y (%coerce-callable-to-fun test)))))

(defun endp (object)
  "This is the recommended way to test for the end of a proper list. It
   returns true if OBJECT is NIL, false if OBJECT is a CONS, and an error
   for any other type of OBJECT."
  (endp object))

(defun list-length (list)
  "Return the length of the given List, or Nil if the List is circular."
  (do ((n 0 (+ n 2))
       (y list (cddr y))
       (z list (cdr z)))
      (())
    (declare (type fixnum n)
             (type list y z))
    (when (endp y) (return n))
    (when (endp (cdr y)) (return (+ n 1)))
    (when (and (eq y z) (> n 0)) (return nil))))

(defun nth (n list)
  "Return the nth object in a list where the car is the zero-th element."
  (declare (explicit-check))
  (car (nthcdr n list)))

(defun first (list)
  "Return the 1st object in a list or NIL if the list is empty."
  (car list))
(defun second (list)
  "Return the 2nd object in a list or NIL if there is no 2nd object."
  (cadr list))
(defun third (list)
  "Return the 3rd object in a list or NIL if there is no 3rd object."
  (caddr list))
(defun fourth (list)
  "Return the 4th object in a list or NIL if there is no 4th object."
  (cadddr list))
(defun fifth (list)
  "Return the 5th object in a list or NIL if there is no 5th object."
  (car (cddddr list)))
(defun sixth (list)
  "Return the 6th object in a list or NIL if there is no 6th object."
  (cadr (cddddr list)))
(defun seventh (list)
  "Return the 7th object in a list or NIL if there is no 7th object."
  (caddr (cddddr list)))
(defun eighth (list)
  "Return the 8th object in a list or NIL if there is no 8th object."
  (cadddr (cddddr list)))
(defun ninth (list)
  "Return the 9th object in a list or NIL if there is no 9th object."
  (car (cddddr (cddddr list))))
(defun tenth (list)
  "Return the 10th object in a list or NIL if there is no 10th object."
  (cadr (cddddr (cddddr list))))
(defun rest (list)
  "Means the same as the cdr of a list."
  (cdr list))

(defun nthcdr (n list)
  "Performs the cdr function n times on a list."
  (flet ((fast-nthcdr (n list)
           (declare (type index n))
           (do ((i n (1- i))
                (result list (cdr result)))
               ((not (plusp i)) result)
             (declare (type index i)))))
    (typecase n
      (index (fast-nthcdr n list))
      ;; Such a large list can only be circular
      (t (do ((i 0 (1+ i))
              (r-i list (cdr r-i))
              (r-2i list (cddr r-2i)))
             ((and (eq r-i r-2i) (not (zerop i)))
              (fast-nthcdr (mod n i) r-i))
           (declare (type index i)))))))

;;; For [n]butlast
(defun dotted-nthcdr (n list)
  (flet ((fast-nthcdr (n list)
           (declare (type index n))
           (do ((i n (1- i))
                (result list (cdr result)))
               ((not (plusp i)) result)
             (declare (type index i))
             (when (atom result)
               (return)))))
    (typecase n
      (index (fast-nthcdr n list))
      ;; Such a large list can only be circular
      (t (do ((i 0 (1+ i))
              (r-i list (cdr r-i))
              (r-2i list (cddr r-2i)))
             ((and (eq r-i r-2i) (not (zerop i)))
              (fast-nthcdr (mod n i) r-i))
           (declare (type index i)))))))

;;; LAST
;;;
;;; Transforms in src/compiler/srctran.lisp pick the most specific
;;; version possible. %LAST/BIGNUM is admittedly somewhat academic...
(macrolet ((last0-macro ()
             `(let ((rest list)
                    (list list))
                (loop (unless (consp rest)
                        (return rest))
                  (shiftf list rest (cdr rest)))))
           (last1-macro ()
             `(let ((rest list)
                    (list list))
                (loop (unless (consp rest)
                        (return list))
                  (shiftf list rest (cdr rest)))))
           (lastn-macro (type)
             `(let ((returned-list list)
                    (checked-list list)
                    (n (truly-the ,type n)))
                (declare (,type n))
                (tagbody
                 :scan
                   (pop checked-list)
                   (when (atom checked-list)
                     (go :done))
                   (if (zerop (truly-the ,type (decf n)))
                       (go :pop)
                       (go :scan))
                 :pop
                   (pop returned-list)
                   (pop checked-list)
                   (if (atom checked-list)
                       (go :done)
                       (go :pop))
                 :done)
                returned-list)))

  (defun %last0 (list)
    (declare (optimize speed (sb!c::verify-arg-count 0)))
    (last0-macro))

  (defun %last1 (list)
    (declare (optimize speed (sb!c::verify-arg-count 0)))
    (last1-macro))

  (defun %lastn/fixnum (list n)
    (declare (optimize speed (sb!c::verify-arg-count 0))
             (type (and unsigned-byte fixnum) n))
    (case n
      (1 (last1-macro))
      (0 (last0-macro))
      (t (lastn-macro fixnum))))

  (defun %lastn/bignum (list n)
    (declare (optimize speed (sb!c::verify-arg-count 0))
             (type (and unsigned-byte bignum) n))
    (lastn-macro unsigned-byte))

  (defun last (list &optional (n 1))
    "Return the last N conses (not the last element!) of a list."
    (case n
      (1 (last1-macro))
      (0 (last0-macro))
      (t
       (typecase n
         (fixnum
          (lastn-macro fixnum))
         (bignum
          (lastn-macro unsigned-byte)))))))

(define-compiler-macro last (&whole form list &optional (n 1) &environment env)
  (if (sb!xc:constantp n env)
      (case (constant-form-value n env)
        (0 `(%last0 ,list))
        (1 `(%last1 ,list))
        (t form))
      form))

(defun list (&rest args)
  "Return constructs and returns a list of its arguments."
  args)

;;; LIST* is done the same as LIST, except that the last cons is made
;;; a dotted pair.

(defun list* (arg &rest others)
  "Return a list of the arguments with last cons a dotted pair."
  (let ((length (length others)))
    (cond ((= length 0) arg)
          ((= length 1)
           (cons arg (fast-&rest-nth 0 others)))
          (t
           (let* ((cons (list arg))
                  (result cons)
                  (index 0)
                  (1-length (1- length)))
             (loop
              (cond
                ((< index 1-length)
                 (setf cons
                       (setf (cdr cons)
                             (list (fast-&rest-nth index others))))
                 (incf index))
                (t (return nil))))
             (setf (cdr cons) (fast-&rest-nth index others))
             result)))))

(defun make-list (size &key initial-element)
  "Constructs a list with size elements each set to value"
  (declare (explicit-check))
  (%make-list size initial-element))
;;; This entry point is to be preferred, irrespective of
;;; whether or not the backend has vops for %MAKE-LIST.
(defun %make-list (size initial-element)
  (declare (type index size))
  (do ((count size (1- count))
       (result '() (cons initial-element result)))
      ((<= count 0) result)
    (declare (type index count))))

(defun append (&rest lists)
  "Construct a new list by concatenating the list arguments"
  (let* ((result (list nil))
         (tail result)
         (index 0)
         (length (length lists))
         (last (1- length)))
    (declare (truly-dynamic-extent result))
    (loop
     (cond
       ((< (truly-the index index) last)
        (let ((list (fast-&rest-nth (truly-the index index) lists)))
          (dolist (elt list)
            (setf (cdr (truly-the cons tail)) (list elt)
                  tail (cdr tail))))
        (incf index))
       (t (return nil))))
    (cond
      ((zerop length) nil)
      ((null (cdr result))
       (fast-&rest-nth (truly-the index last) lists))
      (t
       (setf (cdr (truly-the cons tail))
             (fast-&rest-nth (truly-the index last) lists))
       (cdr result)))))

(defun append2 (x y)
  (declare (optimize (sb!c::verify-arg-count 0)))
  (if (null x)
      y
      (let ((result (list (car x))))
        (do ((more (cdr x) (cdr more))
             (tail result (cdr tail)))
            ((null more)
             (rplacd (truly-the cons tail) y)
             result)
                    (rplacd (truly-the cons tail) (list (car more)))))))


;;;; list copying functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb!xc:defmacro !copy-list-macro (list &key check-proper-list)
    ;; Unless CHECK-PROPER-LIST is true, the list is copied correctly
    ;; even if the list is not terminated by NIL. The new list is built
    ;; by CDR'ing SPLICE which is always at the tail of the new list.
    `(when ,list
       (let ((copy (list (car ,list))))
         (do ((orig (cdr ,list) (cdr orig))
              (splice copy (cdr (rplacd splice (cons (car orig) nil)))))
             (,@(if check-proper-list
                    '((endp orig))
                    '((atom orig)
                      (unless (null orig)
                        (rplacd splice orig))))
              copy))))))

(defun copy-list (list)
  "Return a new list which is EQUAL to LIST. LIST may be improper."
  (!copy-list-macro list))

(defun copy-alist (alist)
  "Return a new association list which is EQUAL to ALIST."
  (if (endp alist)
      alist
      (let ((result
             (cons (if (atom (car alist))
                       (car alist)
                       (cons (caar alist) (cdar alist)))
                   nil)))
        (do ((x (cdr alist) (cdr x))
             (splice result
                     (cdr (rplacd splice
                                  (cons
                                   (if (atom (car x))
                                       (car x)
                                       (cons (caar x) (cdar x)))
                                   nil)))))
            ((endp x)))
        result)))

(defun copy-tree (object)
  "Recursively copy trees of conses."
  (if (consp object)
      (let ((result (list (if (consp (car object))
                              (copy-tree (car object))
                              (car object)))))
        (loop for last-cons = result then new-cons
              for cdr = (cdr object) then (cdr cdr)
              for car = (if (consp cdr)
                            (car cdr)
                            (return (setf (cdr last-cons) cdr)))
              for new-cons = (list (if (consp car)
                                       (copy-tree car)
                                       car))
              do (setf (cdr last-cons) new-cons))
        result)
      object))


;;;; more commonly-used list functions

(defun revappend (x y)
  "Return (append (reverse x) y)."
  (do ((top x (cdr top))
       (result y (cons (car top) result)))
      ((endp top) result)))

;;; NCONC finds the first non-null list, so it can make splice point
;;; to a cons. After finding the first cons element, it holds it in a
;;; result variable while running down successive elements tacking
;;; them together. While tacking lists together, if we encounter a
;;; null list, we set the previous list's last cdr to nil just in case
;;; it wasn't already nil, and it could have been dotted while the
;;; null list was the last argument to NCONC. The manipulation of
;;; splice (that is starting it out on a first cons, setting LAST of
;;; splice, and setting splice to ele) inherently handles (nconc x x),
;;; and it avoids running down the last argument to NCONC which allows
;;; the last argument to be circular.
(defun nconc (&rest lists)
   "Concatenates the lists given as arguments (by changing them)"
   (declare (optimize speed))
   (flet ((fail (object)
            (error 'type-error
                   :datum object
                   :expected-type 'list)))
     (do-rest-arg ((result index) lists)
         (typecase result
           (cons
            (let ((splice result))
              (do-rest-arg ((ele index) lists (1+ index))
                  (typecase ele
                    (cons (rplacd (last splice) ele)
                          (setf splice ele))
                    (null (rplacd (last splice) nil))
                    (atom (if (< (1+ index) (length lists))
                              (fail ele)
                              (rplacd (last splice) ele)))))
              (return result)))
           (null)
           (atom
            (if (< (1+ index) (length lists))
                (fail result)
                (return result)))))))

(defun nreconc (x y)
  "Return (NCONC (NREVERSE X) Y)."
  (do ((1st (cdr x) (if (endp 1st) 1st (cdr 1st)))
       (2nd x 1st)              ;2nd follows first down the list.
       (3rd y 2nd))             ;3rd follows 2nd down the list.
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))


(defun butlast (list &optional (n 1))
  (cond ((zerop n)
         (copy-list list))
        ((not (typep n 'index))
         nil)
        (t
         (let ((head (dotted-nthcdr (1- n) list)))
           (and (consp head)      ; there are at least n
                (collect ((copy)) ; conses; copy!
                  (do ((trail list (cdr trail))
                       (head head (cdr head)))
                      ;; HEAD is n-1 conses ahead of TRAIL;
                      ;; when HEAD is at the last cons, return
                      ;; the data copied so far.
                      ((atom (cdr head))
                       (copy))
                    (copy (car trail)))))))))

(defun nbutlast (list &optional (n 1))
  (cond ((zerop n)
         list)
        ((not (typep n 'index))
         nil)
        (t
         (let ((head (dotted-nthcdr (1- n) list)))
           (and (consp head)       ; there are more than n
                (consp (cdr head)) ; conses.
                ;; TRAIL trails by n cons to be able to
                ;; cut the list at the cons just before.
                (do ((trail list (cdr trail))
                     (head (cdr head) (cdr head)))
                    ((atom (cdr head))
                     (setf (cdr trail) nil)
                     list)))))))

(defun ldiff (list object)
  "Return a new list, whose elements are those of LIST that appear before
   OBJECT. If OBJECT is not a tail of LIST, a copy of LIST is returned.
   LIST must be a proper list or a dotted list."
  (do* ((list list (cdr list))
        (result (list ()))
        (splice result))
       ((atom list)
        (if (eql list object)
            (cdr result)
            (progn (rplacd splice list) (cdr result))))
    (if (eql list object)
        (return (cdr result))
        (setq splice (cdr (rplacd splice (list (car list))))))))

;;;; functions to alter list structure

(defun rplaca (cons x)
  "Change the CAR of CONS to X and return the CONS."
  (rplaca cons x))

(defun rplacd (cons x)
  "Change the CDR of CONS to X and return the CONS."
  (rplacd cons x))

;;; The following are for use by SETF.

(defun %rplaca (x val) (rplaca x val) val)

(defun %rplacd (x val) (rplacd x val) val)

;;; Set the Nth element of LIST to NEWVAL.
(defun %setnth (n list newval)
  (typecase n
    (index
     (do ((count n (1- count))
          (list list (cdr list)))
         ((endp list)
          (error "~S is too large an index for SETF of NTH." n))
       (declare (type fixnum count))
       (when (<= count 0)
         (rplaca list newval)
         (return newval))))
    (t (let ((cons (nthcdr n list)))
         (when (endp cons)
           (error "~S is too large an index for SETF of NTH." n))
         (rplaca cons newval)
         newval))))

;;;; :KEY arg optimization to save funcall of IDENTITY

;;; APPLY-KEY saves us a function call sometimes.
;;;    This isn't wrapped in an (EVAL-WHEN (COMPILE EVAL) ..)
;;;    because it's used in seq.lisp and sort.lisp.
(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

(defmacro apply-key-function (key element)
  `(if ,key
       (funcall (truly-the function ,key) ,element)
       ,element))

;;;; macros for (&KEY (KEY #'IDENTITY) (TEST #'EQL TESTP) (TEST-NOT NIL NOTP))

;;; Use these with the following &KEY args:
(defmacro with-set-keys (funcall)
  `(if notp
       ,(append funcall '(:key key :test-not test-not))
       ,(append funcall '(:key key :test test))))

(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
      (cond (testp (funcall test ,item ,key-tmp))
            (notp (not (funcall test-not ,item ,key-tmp)))
            (t (funcall test ,item ,key-tmp))))))

;;;; substitution of expressions

(defun subst (new old tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitutes new for subtrees matching old."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    (labels ((s (subtree)
               (cond ((satisfies-the-test old subtree) new)
                     ((atom subtree) subtree)
                     (t (let ((car (s (car subtree)))
                              (cdr (s (cdr subtree))))
                          (if (and (eq car (car subtree))
                                   (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr)))))))
      (s tree))))

(defun subst-if (new test tree &key key)
  "Substitutes new for subtrees for which test is true."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (labels ((s (subtree)
               (cond ((funcall test (apply-key key subtree)) new)
                     ((atom subtree) subtree)
                     (t (let ((car (s (car subtree)))
                              (cdr (s (cdr subtree))))
                          (if (and (eq car (car subtree))
                                   (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr)))))))
      (s tree))))

(defun subst-if-not (new test tree &key key)
  "Substitutes new for subtrees for which test is false."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (labels ((s (subtree)
               (cond ((not (funcall test (apply-key key subtree))) new)
                     ((atom subtree) subtree)
                     (t (let ((car (s (car subtree)))
                              (cdr (s (cdr subtree))))
                          (if (and (eq car (car subtree))
                                   (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr)))))))
      (s tree))))

(defun nsubst (new old tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitute NEW for subtrees matching OLD."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    (labels ((s (subtree)
               (cond ((satisfies-the-test old subtree) new)
                     ((atom subtree) subtree)
                     (t (do* ((last nil subtree)
                              (subtree subtree (cdr subtree)))
                             ((atom subtree)
                              (if (satisfies-the-test old subtree)
                                  (setf (cdr last) new)))
                          (if (satisfies-the-test old subtree)
                              (return (setf (cdr last) new))
                              (setf (car subtree) (s (car subtree)))))
                        subtree))))
      (s tree))))

(defun nsubst-if (new test tree &key key)
  "Substitute NEW for subtrees of TREE for which TEST is true."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (labels ((s (subtree)
               (cond ((funcall test (apply-key key subtree)) new)
                     ((atom subtree) subtree)
                     (t (do* ((last nil subtree)
                              (subtree subtree (cdr subtree)))
                             ((atom subtree)
                              (if (funcall test (apply-key key subtree))
                                  (setf (cdr last) new)))
                          (if (funcall test (apply-key key subtree))
                              (return (setf (cdr last) new))
                              (setf (car subtree) (s (car subtree)))))
                        subtree))))
      (s tree))))

(defun nsubst-if-not (new test tree &key key)
  "Substitute NEW for subtrees of TREE for which TEST is false."
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (labels ((s (subtree)
               (cond ((not (funcall test (apply-key key subtree))) new)
                     ((atom subtree) subtree)
                     (t (do* ((last nil subtree)
                              (subtree subtree (cdr subtree)))
                             ((atom subtree)
                              (if (not (funcall test (apply-key key subtree)))
                                  (setf (cdr last) new)))
                          (if (not (funcall test (apply-key key subtree)))
                              (return (setf (cdr last) new))
                              (setf (car subtree) (s (car subtree)))))
                        subtree))))
      (s tree))))

(defun sublis (alist tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitute from ALIST into TREE nondestructively."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    (declare (inline assoc))
    (labels ((s (subtree)
               (let* ((key-val (apply-key key subtree))
                      (assoc (if notp
                                 (assoc key-val alist :test-not test-not)
                                 (assoc key-val alist :test test))))
                 (cond (assoc (cdr assoc))
                       ((atom subtree) subtree)
                       (t (let ((car (s (car subtree)))
                                (cdr (s (cdr subtree))))
                            (if (and (eq car (car subtree))
                                     (eq cdr (cdr subtree)))
                                subtree
                                (cons car cdr))))))))
      (s tree))))

;;; This is in run-time env (i.e. not wrapped in EVAL-WHEN (COMPILE EVAL))
;;; because it can be referenced in inline expansions.
(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key subtree)))
       (if notp
           (assoc ,key-tmp alist :test-not test-not)
           (assoc ,key-tmp alist :test test)))))

(defun nsublis (alist tree &key key (test #'eql testp) (test-not #'eql notp))
  "Substitute from ALIST into TREE destructively."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not)
             (inline assoc))
    (let (temp)
      (labels ((s (subtree)
                 (cond ((setq temp (nsublis-macro))
                        (cdr temp))
                       ((atom subtree) subtree)
                       (t (do* ((last nil subtree)
                                (subtree subtree (cdr subtree)))
                               ((atom subtree)
                                (if (setq temp (nsublis-macro))
                                    (setf (cdr last) (cdr temp))))
                            (if (setq temp (nsublis-macro))
                                (return (setf (cdr last) (cdr temp)))
                                (setf (car subtree) (s (car subtree)))))
                          subtree))))
        (s tree)))))

;;;; functions for using lists as sets

(defun member (item list &key key (test nil testp) (test-not nil notp))
  "Return the tail of LIST beginning with first element satisfying EQLity,
   :TEST, or :TEST-NOT with the given ITEM."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (and testp (%coerce-callable-to-fun test)))
        (test-not (and notp (%coerce-callable-to-fun test-not))))
    (cond (test
           (if key
               (%member-key-test item list key test)
               (%member-test item list test)))
          (test-not
           (if key
               (%member-key-test-not item list key test-not)
               (%member-test-not item list test-not)))
          (t
           (if key
               (%member-key item list key)
               (%member item list))))))

(defun member-if (test list &key key)
  "Return tail of LIST beginning with first element satisfying TEST."
  (declare (explicit-check))
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%member-if-key test list key)
        (%member-if test list))))

(defun member-if-not (test list &key key)
  "Return tail of LIST beginning with first element not satisfying TEST."
  (declare (explicit-check))
  (let ((test (%coerce-callable-to-fun test))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%member-if-not-key test list key)
        (%member-if-not test list))))

(defun tailp (object list)
  "Return true if OBJECT is the same as some tail of LIST, otherwise
   returns false. LIST must be a proper list or a dotted list."
  (do ((list list (cdr list)))
      ((atom list) (eql list object))
    (if (eql object list)
        (return t))))

(defun adjoin (item list &key key (test #'eql testp) (test-not nil notp))
  "Add ITEM to LIST unless it is already a member"
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (and testp (%coerce-callable-to-fun test)))
        (test-not (and notp (%coerce-callable-to-fun test-not))))
    (cond (test
           (if key
               (%adjoin-key-test item list key test)
               (%adjoin-test item list test)))
          (test-not
           (if key
               (%adjoin-key-test-not item list key test-not)
               (%adjoin-test-not item list test-not)))
          (t
           (if key
               (%adjoin-key item list key)
               (%adjoin item list))))))

;;; For cases where MEMBER is called in a loop this allows to perform
;;; the dispatch that the MEMBER function does only once.
(defmacro with-member-test ((test-var &optional first-clause) &body body)
  `(let* ((key (and key (%coerce-callable-to-fun key)))
          (,test-var (cond ,@(and first-clause ; used by LIST-REMOVE-DUPLICATES*
                                  `(,first-clause))
                           (notp
                            (if key
                                (lambda (x list2 key test)
                                  (%member-key-test-not (funcall (truly-the function key) x)
                                                        list2 key test))
                                (lambda (x list2 key test)
                                  (declare (ignore key))
                                  (%member-test-not x list2 test))))
                           (testp
                            (if key
                                (lambda (x list2 key test)
                                  (%member-key-test (funcall (truly-the function key) x)
                                                    list2 key test))
                                (lambda (x list2 key test)
                                  (declare (ignore key))
                                  (%member-test x list2 test))))
                           (key
                            (lambda (x list2 key test)
                              (declare (ignore test))
                              (%member-key (funcall (truly-the function key) x) list2 key)))
                           (t
                            (lambda (x list2 key test)
                              (declare (ignore key test))
                              (%member x list2)))))
          (test (cond (notp
                       (%coerce-callable-to-fun test-not))
                      (testp
                       (%coerce-callable-to-fun test)))))

     ,@body))

(defconstant +list-based-union-limit+ 80)

(defun hash-table-test-p (fun)
  (or (eq fun #'eq)
      (eq fun #'eql)
      (eq fun #'equal)
      (eq fun #'equalp)
      (eq fun 'eq)
      (eq fun 'eql)
      (eq fun 'equal)
      (eq fun 'equalp)))

(defun union (list1 list2 &key key (test nil testp) (test-not nil notp))
  "Return the union of LIST1 and LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  ;; We have two possibilities here: for shortish lists we pick up the
  ;; shorter one as the result, and add the other one to it. For long
  ;; lists we use a hash-table when possible.
  (let ((n1 (length list1))
        (n2 (length list2)))
    (multiple-value-bind (short long n-short)
        (if (< n1 n2)
            (values list1 list2 n1)
            (values list2 list1 n2))
      (if (or (< n-short +list-based-union-limit+)
              notp
              (and testp
                   (not (hash-table-test-p test))))
          (with-member-test (member-test)
            (let ((orig short))
              (dolist (elt long)
                (unless (funcall member-test elt orig key test)
                  (push elt short)))
              short))
          (let ((table (make-hash-table :test (if testp
                                                  test
                                                  #'eql) :size (+ n1 n2)))
                (key (and key (%coerce-callable-to-fun key)))
                (union nil))
            (dolist (elt long)
              (setf (gethash (apply-key key elt) table) elt))
            (dolist (elt short)
              (setf (gethash (apply-key key elt) table) elt))
            (maphash (lambda (k v)
                       (declare (ignore k))
                       (push v union))
                     table)
            union)))))

(defun nunion (list1 list2 &key key (test nil testp) (test-not nil notp))
  "Destructively return the union of LIST1 and LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  ;; We have two possibilities here: for shortish lists we pick up the
  ;; shorter one as the result, and add the other one to it. For long
  ;; lists we use a hash-table when possible.
  (let ((n1 (length list1))
        (n2 (length list2)))
    (multiple-value-bind (short long n-short)
        (if (< n1 n2)
            (values list1 list2 n1)
            (values list2 list1 n2))
      (if (or (< n-short +list-based-union-limit+)
              notp
              (and testp
                   (not (hash-table-test-p test))))
          (with-member-test (member-test)
            (do ((orig short)
                 (elt (car long) (car long)))
                ((endp long))
              (if (funcall member-test elt orig key test)
                  (pop long)
                  (shiftf long (cdr long) short long)))
            short)
          (let ((table (make-hash-table :test (if testp
                                                  test
                                                  #'eql) :size (+ n1 n2)))
                (key (and key (%coerce-callable-to-fun key))))
            (dolist (elt long)
              (setf (gethash (apply-key key elt) table) elt))
            (dolist (elt short)
              (setf (gethash (apply-key key elt) table) elt))
            (let ((union long)
                  (head long))
              (maphash (lambda (k v)
                         (declare (ignore k))
                         (if head
                             (setf (car head) v
                                   head (cdr head))
                             (push v union)))
                      table)
              union))))))

(defun intersection (list1 list2
                     &key key (test nil testp) (test-not nil notp))
  "Return the intersection of LIST1 and LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (when (and list1 list2)
    (with-member-test (member-test)
      (let ((res nil))
        (dolist (elt list1)
          (when (funcall member-test elt list2 key test)
            (push elt res)))
        res))))

(defun nintersection (list1 list2
                      &key key (test nil testp) (test-not nil notp))
  "Destructively return the intersection of LIST1 and LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (when (and list1 list2)
    (with-member-test (member-test)
      (let ((res nil)
            (list1 list1))
        (do () ((endp list1))
          (if (funcall member-test (car list1) list2 key test)
              (shiftf list1 (cdr list1) res list1)
              (setf list1 (cdr list1))))
        res))))

(defun set-difference (list1 list2
                       &key key (test nil testp) (test-not nil notp))
  "Return the elements of LIST1 which are not in LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (if list2
      (with-member-test (member-test)
        (let ((res nil))
          (dolist (elt list1)
            (unless (funcall member-test elt list2 key test)
              (push elt res)))
          res))
      list1))

(defun nset-difference (list1 list2
                        &key key (test nil testp) (test-not nil notp))
  "Destructively return the elements of LIST1 which are not in LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (if list2
      (with-member-test (member-test)
        (let ((res nil)
              (list1 list1))
          (do () ((endp list1))
            (if (funcall member-test (car list1) list2 key test)
                (setf list1 (cdr list1))
                (shiftf list1 (cdr list1) res list1)))
          res))
      list1))

(defun set-exclusive-or (list1 list2
                         &key key (test nil testp) (test-not nil notp))
  "Return new list of elements appearing exactly once in LIST1 and LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((result nil))
    (with-member-test (member-test)
      (dolist (elt list1)
        (unless (funcall member-test elt list2 key test)
          (push elt result)))
      (dx-flet ((test (x y) (funcall (truly-the function test) y x)))
        (dolist (elt list2)
          (unless (funcall member-test elt list1 key #'test)
            (push elt result)))))
    result))

(defun nset-exclusive-or (list1 list2
                          &key key (test #'eql testp) (test-not #'eql notp))
  "Destructively return a list with elements which appear but once in LIST1
   and LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (if testp (%coerce-callable-to-fun test) test))
        (test-not (if notp (%coerce-callable-to-fun test-not) test-not)))
    (declare (type function test test-not))
    ;; The outer loop examines LIST1 while the inner loop examines
    ;; LIST2. If an element is found in LIST2 "equal" to the element
    ;; in LIST1, both are spliced out. When the end of LIST1 is
    ;; reached, what is left of LIST2 is tacked onto what is left of
    ;; LIST1. The splicing operation ensures that the correct
    ;; operation is performed depending on whether splice is at the
    ;; top of the list or not.
    (do ((list1 list1)
         (list2 list2)
         (x list1 (cdr x))
         (splicex ())
         (deleted-y ())
         ;; elements of LIST2, which are "equal" to some processed
         ;; earlier elements of LIST1
         )
        ((endp x)
         (if (null splicex)
             (setq list1 list2)
             (rplacd splicex list2))
         list1)
      (let ((key-val-x (apply-key key (car x)))
            (found-duplicate nil))

        ;; Move all elements from LIST2, which are "equal" to (CAR X),
        ;; to DELETED-Y.
        (do* ((y list2 next-y)
              (next-y (cdr y) (cdr y))
              (splicey ()))
             ((endp y))
          (cond ((let ((key-val-y (apply-key key (car y))))
                   (if notp
                       (not (funcall test-not key-val-x key-val-y))
                       (funcall test key-val-x key-val-y)))
                 (if (null splicey)
                     (setq list2 (cdr y))
                     (rplacd splicey (cdr y)))
                 (setq deleted-y (rplacd y deleted-y))
                 (setq found-duplicate t))
                (t (setq splicey y))))

        (unless found-duplicate
          (setq found-duplicate (with-set-keys (member key-val-x deleted-y))))

        (if found-duplicate
            (if (null splicex)
                (setq list1 (cdr x))
                (rplacd splicex (cdr x)))
            (setq splicex x))))))

(defun subsetp (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Return T if every element in LIST1 is also in LIST2."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (with-member-test (member-test)
    (dolist (elt list1)
      (unless (funcall member-test elt list2 key test)
        (return-from subsetp nil)))
    t))

;;;; functions that operate on association lists

(defun acons (key datum alist)
  "Construct a new alist by adding the pair (KEY . DATUM) to ALIST."
  (cons (cons key datum) alist))

(defun pairlis (keys data &optional (alist '()))
  "Construct an association list from KEYS and DATA (adding to ALIST)."
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((and (endp x) (endp y)) alist)
    (if (or (endp x) (endp y))
        (error "The lists of keys and data are of unequal length."))
    (setq alist (acons (car x) (car y) alist))))

(defun assoc (item alist &key key (test nil testp) (test-not nil notp))
  "Return the cons in ALIST whose car is equal (by a given test or EQL) to
   the ITEM."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (and testp (%coerce-callable-to-fun test)))
        (test-not (and notp (%coerce-callable-to-fun test-not))))
    (cond (test
           (if key
               (%assoc-key-test item alist key test)
               (%assoc-test item alist test)))
          (test-not
           (if key
               (%assoc-key-test-not item alist key test-not)
               (%assoc-test-not item alist test-not)))
          (t
           (if key
               (%assoc-key item alist key)
               (%assoc item alist))))))

(defun assoc-if (predicate alist &key key)
  "Return the first cons in ALIST whose CAR satisfies PREDICATE. If
   KEY is supplied, apply it to the CAR of each cons before testing."
  (declare (explicit-check))
  (let ((predicate (%coerce-callable-to-fun predicate))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%assoc-if-key predicate alist key)
        (%assoc-if predicate alist))))

(defun assoc-if-not (predicate alist &key key)
  "Return the first cons in ALIST whose CAR does not satisfy PREDICATE.
  If KEY is supplied, apply it to the CAR of each cons before testing."
  (declare (explicit-check))
  (let ((predicate (%coerce-callable-to-fun predicate))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%assoc-if-not-key predicate alist key)
        (%assoc-if-not predicate alist))))

(defun rassoc (item alist &key key (test nil testp) (test-not nil notp))
  "Return the cons in ALIST whose CDR is equal (by a given test or EQL) to
   the ITEM."
  (declare (explicit-check))
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  (let ((key (and key (%coerce-callable-to-fun key)))
        (test (and testp (%coerce-callable-to-fun test)))
        (test-not (and notp (%coerce-callable-to-fun test-not))))
    (cond (test
           (if key
               (%rassoc-key-test item alist key test)
               (%rassoc-test item alist test)))
          (test-not
           (if key
               (%rassoc-key-test-not item alist key test-not)
               (%rassoc-test-not item alist test-not)))
          (t
           (if key
               (%rassoc-key item alist key)
               (%rassoc item alist))))))

(defun rassoc-if (predicate alist &key key)
  "Return the first cons in ALIST whose CDR satisfies PREDICATE. If KEY
  is supplied, apply it to the CDR of each cons before testing."
  (declare (explicit-check))
  (let ((predicate (%coerce-callable-to-fun predicate))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%rassoc-if-key predicate alist key)
        (%rassoc-if predicate alist))))

(defun rassoc-if-not (predicate alist &key key)
  "Return the first cons in ALIST whose CDR does not satisfy PREDICATE.
  If KEY is supplied, apply it to the CDR of each cons before testing."
  (declare (explicit-check))
  (let ((predicate (%coerce-callable-to-fun predicate))
        (key (and key (%coerce-callable-to-fun key))))
    (if key
        (%rassoc-if-not-key predicate alist key)
        (%rassoc-if-not predicate alist))))

;;;; mapping functions

;;; a helper function for implementation of MAPC, MAPCAR, MAPCAN,
;;; MAPL, MAPLIST, and MAPCON
;;;
;;; Map the designated function over the arglists in the appropriate
;;; way. It is done when any of the arglists runs out. Until then, it
;;; CDRs down the arglists calling the function and accumulating
;;; results as desired.
(defun map1 (fun-designator arglists accumulate take-car)
  (do* ((fun (%coerce-callable-to-fun fun-designator))
        (non-acc-result (car arglists))
        (ret-list (list nil))
        (temp ret-list)
        (res nil)
        (args (make-list (length arglists))))
       ((dolist (x arglists) (or x (return t)))
        (if accumulate
            (cdr ret-list)
            non-acc-result))
    (do ((l arglists (cdr l))
         (arg args (cdr arg)))
        ((null l))
      (setf (car arg) (if take-car (caar l) (car l)))
      (setf (car l) (cdar l)))
    (setq res (apply fun args))
    (case accumulate
      (:nconc
       (when res
         (setf (cdr temp) res)
         ;; KLUDGE: it is said that MAPCON is equivalent to
         ;; (apply #'nconc (maplist ...)) which means (nconc 1) would
         ;; return 1, but (nconc 1 1) should signal an error.
         ;; The transformed MAP code returns the last result, do that
         ;; here as well for consistency and simplicity.
         (when (consp res)
           (setf temp (last res)))))
      (:list (setf (cdr temp) (list res)
                   temp (cdr temp))))))

(defun mapc (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists. Return the second argument."
  (declare (explicit-check))
  (map1 function (cons list more-lists) nil t))

(defun mapcar (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return list of FUNCTION
   return values."
  (declare (explicit-check))
  (map1 function (cons list more-lists) :list t))

(defun mapcan (function list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. Return NCONC of FUNCTION
   results."
  (declare (explicit-check))
  (map1 function (cons list more-lists) :nconc t))

(defun mapl (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return the second argument."
  (declare (explicit-check))
  (map1 function (cons list more-lists) nil nil))

(defun maplist (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of list. Return list of results."
  (declare (explicit-check))
  (map1 function (cons list more-lists) :list nil))

(defun mapcon (function list &rest more-lists)
  "Apply FUNCTION to successive CDRs of lists. Return NCONC of results."
  (declare (explicit-check))
  (map1 function (cons list more-lists) :nconc nil))

;;;; Specialized versions

;;; %ADJOIN-*, %ASSOC-*, %MEMBER-*, and %RASSOC-* functions. Deftransforms
;;; delegate to TRANSFORM-LIST-PRED-SEEK and TRANSFORM-LIST-ITEM-SEEK which
;;; pick the appropriate versions. These win because they have only positional
;;; arguments, the TEST, TEST-NOT & KEY functions are known to exist (or not),
;;; and are known to be functions instead of function designators. We are also
;;; able to transform many common cases to -EQ versions, which are
;;; substantially faster then EQL using ones.
(macrolet
    ((def (funs form &optional variant)
       (flet ((%def (name &optional conditional)
                (let* ((body-loop
                        `(do ((list list (cdr list)))
                             ((null list) nil)
                           (declare (list list))
                           (let ((this (car list)))
                             ,(let ((cxx (if (char= #\A (char (string name) 0))
                                             'car    ; assoc, assoc-if, assoc-if-not
                                             'cdr))) ; rassoc, rassoc-if, rassoc-if-not
                                   (ecase name
                                      ((assoc rassoc)
                                       (if funs
                                           `(when this
                                              (let ((target (,cxx this)))
                                                (when ,form
                                                  (return this))))
                                           ;; If there is no TEST/TEST-NOT or
                                           ;; KEY, do the EQ/EQL test first,
                                           ;; before checking for NIL.
                                           `(let ((target (,cxx this)))
                                              (when (and ,form this)
                                                (return this)))))
                                 ((assoc-if assoc-if-not rassoc-if rassoc-if-not)
                                  (aver (equal '(eql x) (subseq form 0 2)))
                                  `(when this
                                     (let ((target (,cxx this)))
                                       (,conditional (funcall ,@(cdr form))
                                                     (return this)))))
                                 (member
                                  `(let ((target this))
                                     (when ,form
                                       (return list))))
                                 ((member-if member-if-not)
                                  (aver (equal '(eql x) (subseq form 0 2)))
                                  `(let ((target this))
                                     (,conditional (funcall ,@(cdr form))
                                                   (return list))))
                                 (adjoin
                                  `(let ((target this))
                                     (when ,form
                                       (return t)))))))))
                       (body (if (eq 'adjoin name)
                                 `(if (let ,(when (member 'key funs)
                                                  `((x (funcall key x))))
                                        ,body-loop)
                                      list
                                      (cons x list))
                                 body-loop)))
                  `(defun ,(intern (format nil "%~A~{-~A~}~@[-~A~]" name funs variant))
                       (x list ,@funs)
                     (declare (optimize speed (sb!c::verify-arg-count 0)))
                     ,@(when funs `((declare (function ,@funs))))
                     ,@(unless (member name '(member assoc adjoin rassoc)) `((declare (function x))))
                     (declare (explicit-check))
                     ,body))))
         `(progn
            ,(%def 'adjoin)
            ,(%def 'assoc)
            ,(%def 'member)
            ,(%def 'rassoc)
            ,@(when (and (not variant) (member funs '(() (key)) :test #'equal))
                    (list (%def 'member-if 'when)
                          (%def 'member-if-not 'unless)
                          (%def 'assoc-if 'when)
                          (%def 'assoc-if-not 'unless)
                          (%def 'rassoc-if 'when)
                          (%def 'rassoc-if-not 'unless)))))))
  (def ()
      (eql x target))
  (def ()
      (eq x target)
    eq)
  (def (key)
      (eql x (funcall key target)))
  (def (key)
      (eq x (funcall key target))
    eq)
  (def (key test)
      (funcall test x (funcall key target)))
  (def (key test-not)
      (not (funcall test-not x (funcall key target))))
  (def (test)
      (funcall test x target))
  (def (test-not)
      (not (funcall test-not x target))))
