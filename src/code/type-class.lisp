;;;; stuff related to the TYPE-CLASS structure

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(!begin-collecting-cold-init-forms)

(defvar *type-classes*)
(!cold-init-forms
  (unless (boundp '*type-classes*) ; FIXME: How could this be bound?
    (setq *type-classes* (make-hash-table :test 'eq))))

(defun type-class-or-lose (name)
  (or (gethash name *type-classes*)
      (error "~S is not a defined type class." name)))

(defun must-supply-this (&rest foo)
  (/show0 "failing in MUST-SUPPLY-THIS")
  (error "missing type method for ~S" foo))

;;; A TYPE-CLASS object represents the "kind" of a type. It mainly
;;; contains functions which are methods on that kind of type, but is
;;; also used in EQ comparisons to determined if two types have the
;;; "same kind".
(def!struct (type-class
             #-no-ansi-print-object
             (:print-object (lambda (x stream)
                              (print-unreadable-object (x stream :type t)
                                (prin1 (type-class-name x) stream)))))
  ;; the name of this type class (used to resolve references at load time)
  (name nil :type symbol) ; FIXME: should perhaps be (MISSING-ARG) default?
  ;; Dyadic type methods. If the classes of the two types are EQ, then
  ;; we call the SIMPLE-xxx method. If the classes are not EQ, and
  ;; either type's class has a COMPLEX-xxx method, then we call it.
  ;;
  ;; Although it is undefined which method will get precedence when
  ;; both types have a complex method, the complex method can assume
  ;; that the second arg always is in its class, and the first always
  ;; is not. The arguments to commutative operations will be swapped
  ;; if the first argument has a complex method.
  ;;
  ;; Since SUBTYPEP is not commutative, we have two complex methods.
  ;; The ARG1 method is only called when the first argument is in its
  ;; class, and the ARG2 method is only called when called when the
  ;; second type is. If either is specified, both must be.
  (simple-subtypep #'must-supply-this :type function)
  (complex-subtypep-arg1 nil :type (or function null))
  (complex-subtypep-arg2 nil :type (or function null))
  ;; SIMPLE-UNION2, COMPLEX-UNION2, SIMPLE-INTERSECTION2, and
  ;; COMPLEX-INTERSECTION2 methods take pairs of types and try to find
  ;; a new type which expresses the result nicely, better than could
  ;; be done by just stuffing the two component types into an
  ;; UNION-TYPE or INTERSECTION-TYPE object. They return NIL on
  ;; failure, or a CTYPE for success.
  ;;
  ;; Note: These methods are similar to CMU CL's SIMPLE-UNION,
  ;; COMPLEX-UNION, SIMPLE-INTERSECTION, and COMPLEX-UNION methods.
  ;; They were reworked in SBCL because SBCL has INTERSECTION-TYPE
  ;; objects (where CMU CL just punted to HAIRY-TYPE) and because SBCL
  ;; wants to simplify unions and intersections by considering all
  ;; possible pairwise simplifications (where the CMU CL code only
  ;; considered simplifications between types which happened to appear
  ;; next to each other the argument sequence).
  ;;
  ;; Differences in detail from old CMU CL methods:
  ;;   * SBCL's methods are more parallel between union and
  ;;     intersection forms. Each returns one values, (OR NULL CTYPE).
  ;;   * SBCL doesn't use type methods to deal with unions or
  ;;     intersections of the COMPOUND-TYPE of the corresponding form.
  ;;     Instead the wrapper functions TYPE-UNION2, TYPE-INTERSECTION2,
  ;;     TYPE-UNION, and TYPE-INTERSECTION handle those cases specially
  ;;     (and deal with canonicalization/simplification issues at the
  ;;     same time).
  (simple-union2 #'hierarchical-union2 :type function)
  (complex-union2 nil :type (or function null))
  (simple-intersection2 #'hierarchical-intersection2 :type function)
  (complex-intersection2 nil :type (or function null))
  (simple-= #'must-supply-this :type function)
  (complex-= nil :type (or function null))
  ;; monadic functions
  (negate #'must-supply-this :type function)
  ;; a function which returns a Common Lisp type specifier
  ;; representing this type
  (unparse #'must-supply-this :type function)
  ;; a function which returns T if the CTYPE is inhabited by a single
  ;; object and, as a value, the object.  Otherwise, returns NIL, NIL.
  ;; The default case (NIL) is interpreted as a function that always
  ;; returns NIL, NIL.
  (singleton-p nil :type (or function null))

  #|
  Not used, and not really right. Probably we want a TYPE= alist for the
  unary operations, since there are lots of interesting unary predicates that
  aren't equivalent to an entire class
  ;; Names of functions used for testing the type of objects in this type
  ;; class. UNARY-PREDICATE takes just the object, whereas PREDICATE gets
  ;; passed both the object and the CTYPE. Normally one or the other will be
  ;; supplied for any type that can be passed to TYPEP; there is no point in
  ;; supplying both.
  (unary-typep nil :type (or symbol null))
  (typep nil :type (or symbol null))
  ;; These are like TYPEP and UNARY-TYPEP except they coerce objects to
  ;; the type.
  (unary-coerce nil :type (or symbol null))
  (coerce :type (or symbol null))
  |#
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; KLUDGE: If the slots of TYPE-CLASS ever change, the slots here
  ;; will have to be tweaked to match. -- WHN 19991021
  (defparameter *type-class-fun-slots*
    '((:simple-subtypep . type-class-simple-subtypep)
      (:complex-subtypep-arg1 . type-class-complex-subtypep-arg1)
      (:complex-subtypep-arg2 . type-class-complex-subtypep-arg2)
      (:simple-union2 . type-class-simple-union2)
      (:complex-union2 . type-class-complex-union2)
      (:simple-intersection2 . type-class-simple-intersection2)
      (:complex-intersection2 . type-class-complex-intersection2)
      (:simple-= . type-class-simple-=)
      (:complex-= . type-class-complex-=)
      (:negate . type-class-negate)
      (:unparse . type-class-unparse)
      (:singleton-p . type-class-singleton-p))))

(declaim (ftype (function (type-class) type-class) copy-type-class-coldly))
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
;;; Copy TYPE-CLASS object X, using only operations which will work
;;; early in cold load. (COPY-STRUCTURE won't work early in cold load,
;;; because it needs RAW-INDEX and RAW-LENGTH information from
;;; LAYOUT-INFO, and LAYOUT-INFO isn't initialized early in cold
;;; load.)
;;;
;;; FIXME: It's nasty having to maintain this hand-written copy
;;; function. And it seems intrinsically dain-bramaged to have
;;; RAW-INDEX and RAW-LENGTH in LAYOUT-INFO instead of directly in
;;; LAYOUT. We should fix this:
;;;   * Move RAW-INDEX and RAW-LENGTH slots into LAYOUT itself.
;;;   * Rewrite the various CHECK-LAYOUT-related functions so that
;;;     they check RAW-INDEX and RAW-LENGTH too.
;;;   * Remove this special hacked copy function, just use
;;;     COPY-STRUCTURE instead.
;;; (For even more improvement, it might be good to move the raw slots
;;; into the same object as the ordinary slots, instead of having the
;;; unfortunate extra level of indirection. But that'd probably
;;; require a lot of work, including updating the garbage collector to
;;; understand it. And it might even hurt overall performance, because
;;; the positive effect of removing indirection could be cancelled by
;;; the negative effect of imposing an unnecessary GC write barrier on
;;; raw data which doesn't actually affect GC.)
(defun copy-type-class-coldly (x)
  ;; KLUDGE: If the slots of TYPE-CLASS ever change in a way not
  ;; reflected in *TYPE-CLASS-FUN-SLOTS*, the slots here will
  ;; have to be hand-tweaked to match. -- WHN 2001-03-19
  (make-type-class :name (type-class-name x)
                   . #.(mapcan (lambda (type-class-fun-slot)
                                 (destructuring-bind (keyword . slot-accessor)
                                     type-class-fun-slot
                                   `(,keyword (,slot-accessor x))))
                               *type-class-fun-slots*)))

(defun class-fun-slot-or-lose (name)
  (or (cdr (assoc name *type-class-fun-slots*))
      (error "~S is not a defined type class method." name)))
;;; FIXME: This seems to be called at runtime by cold init code.
;;; Make sure that it's not being called at runtime anywhere but
;;; one-time toplevel initialization code.

) ; EVAL-WHEN

(defmacro !define-type-method ((class method &rest more-methods)
                               lambda-list &body body)
  (let ((name (symbolicate class "-" method "-TYPE-METHOD")))
    `(progn
       (defun ,name ,lambda-list
         ,@body)
       (!cold-init-forms
        ,@(mapcar (lambda (method)
                    `(setf (,(class-fun-slot-or-lose method)
                            (type-class-or-lose ',class))
                           #',name))
                  (cons method more-methods)))
       ',name)))

(defmacro !define-type-class (name &key inherits)
  `(!cold-init-forms
     ,(once-only ((n-class (if inherits
                               `(copy-type-class-coldly (type-class-or-lose
                                                         ',inherits))
                               '(make-type-class))))
        `(progn
           (setf (type-class-name ,n-class) ',name)
           (setf (gethash ',name *type-classes*) ,n-class)
           ',name))))

;;; Invoke a type method on TYPE1 and TYPE2. If the two types have the
;;; same class, invoke the simple method. Otherwise, invoke any
;;; complex method. If there isn't a distinct COMPLEX-ARG1 method,
;;; then swap the arguments when calling TYPE1's method. If no
;;; applicable method, return DEFAULT.
;;;
;;; KLUDGE: It might be a lot easier to understand this and the rest
;;; of the type system code if we used CLOS to express it instead of
;;; trying to maintain this squirrely hand-crufted object system.
;;; Unfortunately that'd require reworking PCL bootstrapping so that
;;; all the compilation can get done by the cross-compiler, which I
;;; suspect is hard, so we'll bear with the old system for the time
;;; being. -- WHN 2001-03-11
(defmacro !invoke-type-method (simple complex-arg2 type1 type2 &key
                                      (default '(values nil t))
                                      (complex-arg1 :foo complex-arg1-p))
  (declare (type keyword simple complex-arg1 complex-arg2))
  (let ((simple (class-fun-slot-or-lose simple))
        (cslot1 (class-fun-slot-or-lose
                 (if complex-arg1-p complex-arg1 complex-arg2)))
        (cslot2 (class-fun-slot-or-lose complex-arg2)))
    (once-only ((ntype1 type1)
                (ntype2 type2))
      (once-only ((class1 `(type-class-info ,ntype1))
                  (class2 `(type-class-info ,ntype2)))
        `(if (eq ,class1 ,class2)
             (funcall (,simple ,class1) ,ntype1 ,ntype2)
             ,(once-only ((complex2 `(,cslot2 ,class2)))
                `(if ,complex2
                     (funcall ,complex2 ,ntype1 ,ntype2)
                     ,(once-only ((complex1 `(,cslot1 ,class1)))
                        `(if ,complex1
                             (if ,complex-arg1-p
                                 (funcall ,complex1 ,ntype1 ,ntype2)
                                 (funcall ,complex1 ,ntype2 ,ntype1))
                          ,default)))))))))

;;; This is a very specialized implementation of CLOS-style
;;; CALL-NEXT-METHOD within our twisty little type class object
;;; system, which works given that it's called from within a
;;; COMPLEX-SUBTYPEP-ARG2 method. (We're particularly motivated to
;;; implement CALL-NEXT-METHOD in that case, because ANSI imposes some
;;; strict limits on when SUBTYPEP is allowed to return (VALUES NIL NIL),
;;; so instead of just complacently returning (VALUES NIL NIL) from a
;;; COMPLEX-SUBTYPEP-ARG2 method we usually need to CALL-NEXT-METHOD.)
;;;
;;; KLUDGE: In CLOS, this could just be CALL-NEXT-METHOD and
;;; everything would Just Work without us having to think about it. In
;;; our goofy type dispatch system, it's messier to express. It's also
;;; more fragile, since (0) there's no check that it's called from
;;; within a COMPLEX-SUBTYPEP-ARG2 method as it should be, and (1) we
;;; rely on our global knowledge that the next (and only) relevant
;;; method is COMPLEX-SUBTYPEP-ARG1, and (2) we rely on our global
;;; knowledge of the appropriate default for the CSUBTYPEP function
;;; when no next method exists. -- WHN 2002-04-07
;;;
;;; (We miss CLOS! -- CSR and WHN)
(defun invoke-complex-subtypep-arg1-method (type1 type2 &optional subtypep win)
  (let* ((type-class (type-class-info type1))
         (method-fun (type-class-complex-subtypep-arg1 type-class)))
    (if method-fun
        (funcall (the function method-fun) type1 type2)
        (values subtypep win))))

;;; KLUDGE: This function is dangerous, as its overuse could easily
;;; cause stack exhaustion through unbounded recursion.  We only use
;;; it in one place; maybe it ought not to be a function at all?
(defun invoke-complex-=-other-method (type1 type2)
  (let* ((type-class (type-class-info type1))
         (method-fun (type-class-complex-= type-class)))
    (if method-fun
        (funcall (the function method-fun) type2 type1)
        (values nil t))))

(!defun-from-collected-cold-init-forms !type-class-cold-init)
