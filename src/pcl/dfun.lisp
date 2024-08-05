;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

#|

This implementation of method lookup was redone in early August of 89.

It has the following properties:

 - Its modularity makes it easy to modify the actual caching algorithm.
   The caching algorithm is almost completely separated into the files
   cache.lisp and dlap.lisp. This file just contains the various uses
   of it. There will be more tuning as we get more results from Luis'
   measurements of caching behavior.

 - The metacircularity issues have been dealt with properly. All of
   PCL now grounds out properly. Moreover, it is now possible to have
   metaobject classes which are themselves not instances of standard
   metaobject classes.

** Modularity of the code **

The actual caching algorithm is isolated in a modest number of functions.
The code which generates cache lookup code is all found in cache.lisp and
dlap.lisp. Certain non-wrapper-caching special cases are in this file.

** Handling the metacircularity **

In CLOS, method lookup is the potential source of infinite metacircular
regress. The metaobject protocol specification gives us wide flexibility
in how to address this problem. PCL uses a technique which handles the
problem not only for the metacircular language described in Chapter 3, but
also for the PCL protocol which includes additional generic functions
which control more aspects of the CLOS implementation.

The source of the metacircular regress can be seen in a number of ways.
One is that the specified method lookup protocol must, as part of doing
the method lookup (or at least the cache miss case), itself call generic
functions. It is easy to see that if the method lookup for a generic
function ends up calling that same generic function there can be trouble.

Fortunately, there is an easy solution at hand. The solution is based on
the restriction that portable code cannot change the class of a specified
metaobject. This restriction implies that for specified generic functions,
the method lookup protocol they follow is fixed.

More precisely, for such specified generic functions, most generic functions
that are called during their own method lookup will not run portable methods.
This allows the implementation to usurp the actual generic function call in
this case. In short, method lookup of a standard generic function, in the
case where the only applicable methods are themselves standard doesn't
have to do any method lookup to implement itself.

And so, we are saved.

Except see also BREAK-VICIOUS-METACIRCLE.  -- CSR, 2003-05-28

|#

;;; an alist in which each entry is of the form
;;;   (<generator> . (<subentry> ...)).
;;; Each subentry is of the form
;;;   (<args> <constructor> <system>).
(define-load-time-global *dfun-constructors* ())

;;; If this is NIL, then the whole mechanism for caching dfun constructors is
;;; turned off. The only time that makes sense is when debugging LAP code.
(defvar *enable-dfun-constructor-caching* t)

(defun show-dfun-constructors ()
  (format t "~&DFUN constructor caching is ~A."
          (if *enable-dfun-constructor-caching*
              "enabled" "disabled"))
  (dolist (generator-entry *dfun-constructors*)
    (dolist (args-entry (cdr generator-entry))
      (format t "~&~S ~S"
              (cons (car generator-entry) (caar args-entry))
              (caddr args-entry)))))

(defvar *raise-metatypes-to-class-p* t)

(defun get-dfun-constructor (generator &rest args)
  (when (and *raise-metatypes-to-class-p*
             (member generator '(emit-checking emit-caching
                                 emit-in-checking-cache-p emit-constant-value)))
    (setq args (cons (mapcar (lambda (mt)
                               (if (eq mt t)
                                   mt
                                   'class))
                             (car args))
                     (cdr args))))
  (let* ((generator-entry (assq generator *dfun-constructors*))
         (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (if (null *enable-dfun-constructor-caching*)
        (apply (fdefinition generator) args)
        (or (cadr args-entry)
            (multiple-value-bind (new not-best-p)
                (apply (symbol-function generator) args)
              (let ((entry (list (copy-list args) new (unless not-best-p 'pcl)
                                 not-best-p)))
                (if generator-entry
                    (push entry (cdr generator-entry))
                    (push (list generator entry)
                          *dfun-constructors*)))
              (values new not-best-p))))))

(defun load-precompiled-dfun-constructor (generator args system constructor)
  (let* ((generator-entry (assq generator *dfun-constructors*))
         (args-entry (assoc args (cdr generator-entry) :test #'equal)))
    (if args-entry
        (when (fourth args-entry)
          (let* ((dfun-type (case generator
                              (emit-checking 'checking)
                              (emit-caching 'caching)
                              (emit-constant-value 'constant-value)
                              (emit-default-only 'default-method-only)))
                 (metatypes (car args))
                 (gfs (when dfun-type (gfs-of-type dfun-type))))
            (dolist (gf gfs)
              (when (and (equal metatypes
                                (arg-info-metatypes (gf-arg-info gf)))
                         (let ((gf-name (generic-function-name gf)))
                           (and (not (eq gf-name 'slot-value-using-class))
                                (not (equal gf-name
                                            '(setf slot-value-using-class)))
                                (not (eq gf-name 'slot-boundp-using-class))
                                (not (eq gf-name 'slot-makunbound-using-class)))))
                (update-dfun gf)))
            (setf (second args-entry) constructor)
            (setf (third args-entry) system)
            (setf (fourth args-entry) nil)))
        (let ((entry (list args constructor system nil)))
          (if generator-entry
              (push entry (cdr generator-entry))
              (push (list generator entry) *dfun-constructors*))))))

(defmacro precompile-dfun-constructors (&optional system)
  (let ((*precompiling-lap* t))
    `(progn
       ,@(let (collect)
           (dolist (generator-entry *dfun-constructors*)
             (dolist (args-entry (cdr generator-entry))
               (when (or (null (caddr args-entry))
                         (eq (caddr args-entry) system))
                 (when system (setf (caddr args-entry) system))
                 (push `(load-precompiled-dfun-constructor
                         ',(car generator-entry)
                         ',(car args-entry)
                         ',system
                         ,(apply (fdefinition (car generator-entry))
                                 (car args-entry)))
                       collect))))
           (nreverse collect)))))

;;; Standardized class slot access: when trying to break vicious
;;; metacircles, we need a way to get at the values of slots of some
;;; standard classes without going through the whole meta machinery,
;;; because that would likely enter the vicious circle again.  The
;;; following are helper functions that short-circuit the generic
;;; lookup machinery.

(defconstant-eqx +standard-classes+
  ;; KLUDGE: order matters!  finding effective slot definitions
  ;; involves calling slot-definition-name, and we need to do that to
  ;; break metacycles, so STANDARD-EFFECTIVE-SLOT-DEFINITION must
  ;; precede STANDARD-DIRECT-SLOT-DEFINITION in this list, at least
  ;; until ACCESSES-STANDARD-CLASS-SLOT-P is generalized
  '(standard-method standard-generic-function standard-class
    standard-effective-slot-definition standard-direct-slot-definition)
  #'equal)

(define-load-time-global *standard-slot-locations* (make-hash-table :test 'equal))

(defun compute-standard-slot-locations ()
  (let ((new (make-hash-table :test 'equal)))
    (dolist (class-name +standard-classes+)
      (let ((class (find-class class-name)))
        (dolist (slot (class-slots class))
          (setf (gethash (cons class (slot-definition-name slot)) new)
                (slot-definition-location slot)))))
    (setf *standard-slot-locations* new)))

(defun standard-slot-value (object slot-name class)
  (declare (notinline standard-instance-access
                      funcallable-standard-instance-access))
  ;; I'm sure there's a super easy way to feed the mix of the CLASS and SLOT-NAME
  ;; hashes into a perfect hash fun, but this function seems never to be called except
  ;; by MOP some tests. Therefore I don't care to improve it beyond the avoidance
  ;; of 1 cons operation.
  (let* ((key (cons class slot-name))
         (location (gethash key *standard-slot-locations*)))
    (declare (dynamic-extent key))
    (if location
        (let ((value (if (funcallable-instance-p object)
                         (funcallable-standard-instance-access object location)
                         (standard-instance-access object location))))
          (when (unbound-marker-p value)
            (error "~@<slot ~S of class ~S is unbound in object ~S~@:>"
                   slot-name class object))
          value)
        (error "~@<cannot get standard value of slot ~S of class ~S ~
                in object ~S~@:>"
               slot-name class object))))

(defun standard-slot-value/gf (gf slot-name)
  (standard-slot-value gf slot-name *the-class-standard-generic-function*))

(defun standard-slot-value/method (method slot-name)
  (standard-slot-value method slot-name *the-class-standard-method*))

(defun standard-slot-value/eslotd (slotd slot-name)
  (standard-slot-value slotd slot-name
                       *the-class-standard-effective-slot-definition*))

(defun standard-slot-value/dslotd (slotd slot-name)
  (standard-slot-value slotd slot-name
                       *the-class-standard-direct-slot-definition*))

(defun standard-slot-value/class (class slot-name)
  (standard-slot-value class slot-name *the-class-standard-class*))

;;; When all the methods of a generic function are automatically
;;; generated reader or writer methods a number of special
;;; optimizations are possible. These are important because of the
;;; large number of generic functions of this type.
;;;
;;; There are a number of cases:
;;;
;;;   ONE-CLASS-ACCESSOR
;;;     In this case, the accessor generic function has only been
;;;     called with one class of argument. There is no cache vector,
;;;     the wrapper of the one class, and the slot index are stored
;;;     directly as closure variables of the discriminating function.
;;;     This case can convert to either of the next kind.
;;;
;;;   TWO-CLASS-ACCESSOR
;;;     Like above, but two classes. This is common enough to do
;;;     specially. There is no cache vector. The two classes are
;;;     stored a separate closure variables.
;;;
;;;   ONE-INDEX-ACCESSOR
;;;     In this case, the accessor generic function has seen more than
;;;     one class of argument, but the index of the slot is the same
;;;     for all the classes that have been seen. A cache vector is
;;;     used to store the wrappers that have been seen, the slot index
;;;     is stored directly as a closure variable of the discriminating
;;;     function. This case can convert to the next kind.
;;;
;;;   N-N-ACCESSOR
;;;     This is the most general case. In this case, the accessor
;;;     generic function has seen more than one class of argument and
;;;     more than one slot index. A cache vector stores the wrappers
;;;     and corresponding slot indexes.

(defstruct (dfun-info (:constructor nil)
                      (:copier nil))
  (cache nil))

(defstruct (no-methods (:constructor no-methods-dfun-info ())
                       (:include dfun-info)
                       (:copier nil)))

(defstruct (initial (:constructor initial-dfun-info ())
                    (:include dfun-info)
                    (:copier nil)))

(defstruct (dispatch (:constructor dispatch-dfun-info ())
                     (:include dfun-info)
                     (:copier nil)))

(defstruct (default-method-only (:constructor default-method-only-dfun-info ())
                                (:include dfun-info)
                                (:copier nil)))

;without caching:
;  dispatch one-class two-class default-method-only

;with caching:
;  one-index n-n checking caching

;accessor:
;  one-class two-class one-index n-n
(defstruct (accessor-dfun-info (:constructor nil)
                               (:include dfun-info)
                               (:copier nil))
  accessor-type) ; (member reader writer)

(defmacro dfun-info-accessor-type (di)
  `(accessor-dfun-info-accessor-type ,di))

(defstruct (one-index-dfun-info (:constructor nil)
                                (:include accessor-dfun-info)
                                (:copier nil))
  index)

(defmacro dfun-info-index (di)
  `(one-index-dfun-info-index ,di))

(defstruct (n-n (:constructor n-n-dfun-info (accessor-type cache))
                (:include accessor-dfun-info)
                (:copier nil)))

(defstruct (one-class (:constructor one-class-dfun-info
                                    (accessor-type index wrapper0))
                      (:include one-index-dfun-info)
                      (:copier nil))
  wrapper0)

(defmacro dfun-info-wrapper0 (di)
  `(one-class-wrapper0 ,di))

(defstruct (two-class (:constructor two-class-dfun-info
                                    (accessor-type index wrapper0 wrapper1))
                      (:include one-class)
                      (:copier nil))
  wrapper1)

(defmacro dfun-info-wrapper1 (di)
  `(two-class-wrapper1 ,di))

(defstruct (one-index (:constructor one-index-dfun-info
                                    (accessor-type index cache))
                      (:include one-index-dfun-info)
                      (:copier nil)))

(defstruct (checking (:constructor checking-dfun-info (function cache))
                     (:include dfun-info)
                     (:copier nil))
  function)

(defmacro dfun-info-function (di)
  `(checking-function ,di))

(defstruct (caching (:constructor caching-dfun-info (cache))
                    (:include dfun-info)
                    (:copier nil)))

(defstruct (constant-value (:constructor constant-value-dfun-info (cache))
                           (:include dfun-info)
                           (:copier nil)))

(defmacro dfun-update (generic-function function &rest args)
  `(multiple-value-bind (dfun cache info)
       (funcall ,function ,generic-function ,@args)
     (update-dfun ,generic-function dfun cache info)))

(defun accessor-miss-function (gf dfun-info)
  (ecase (dfun-info-accessor-type dfun-info)
    ((reader boundp makunbound)
     (lambda (arg)
       (accessor-miss gf nil arg dfun-info)))
    (writer
     (lambda (new arg)
       (accessor-miss gf new arg dfun-info)))))

(declaim (sb-ext:freeze-type dfun-info))

(defun make-one-class-accessor-dfun (gf type wrapper index)
  (let ((emit (ecase type
                (reader 'emit-one-class-reader)
                (boundp 'emit-one-class-boundp)
                (makunbound 'emit-one-class-makunbound)
                (writer 'emit-one-class-writer)))
        (dfun-info (one-class-dfun-info type index wrapper)))
    (values
     (funcall (get-dfun-constructor emit (consp index))
              wrapper index
              (accessor-miss-function gf dfun-info))
     nil
     dfun-info)))

(defun make-two-class-accessor-dfun (gf type w0 w1 index)
  (let ((emit (ecase type
                (reader 'emit-two-class-reader)
                (boundp 'emit-two-class-boundp)
                (makunbound 'emit-two-class-makunbound)
                (writer 'emit-two-class-writer)))
        (dfun-info (two-class-dfun-info type index w0 w1)))
    (values
     (funcall (get-dfun-constructor emit (consp index))
              w0 w1 index
              (accessor-miss-function gf dfun-info))
     nil
     dfun-info)))

;;; std accessors same index dfun
(defun make-one-index-accessor-dfun (gf type index &optional cache)
  (let* ((emit (ecase type
                 (reader 'emit-one-index-readers)
                 (boundp 'emit-one-index-boundps)
                 (makunbound 'emit-one-index-makunbounds)
                 (writer 'emit-one-index-writers)))
         (cache (or cache (make-cache :key-count 1 :value nil :size 4)))
         (dfun-info (one-index-dfun-info type index cache)))
    (declare (type cache cache))
    (values
     (funcall (get-dfun-constructor emit (consp index))
              cache
              index
              (accessor-miss-function gf dfun-info))
     cache
     dfun-info)))

(defun make-n-n-accessor-dfun (gf type &optional cache)
  (let* ((emit (ecase type
                 (reader 'emit-n-n-readers)
                 (boundp 'emit-n-n-boundps)
                 (makunbound 'emit-n-n-makunbounds)
                 (writer 'emit-n-n-writers)))
         (cache (or cache (make-cache :key-count 1 :value t :size 2)))
         (dfun-info (n-n-dfun-info type cache)))
    (declare (type cache cache))
    (values
     (funcall (get-dfun-constructor emit)
              cache
              (accessor-miss-function gf dfun-info))
     cache
     dfun-info)))

(defun make-checking-dfun (generic-function function &optional cache)
  (unless (or cache (use-default-method-only-dfun-p generic-function))
    (when (use-caching-dfun-p generic-function)
      (return-from make-checking-dfun (make-caching-dfun generic-function)))
    (when (use-dispatch-dfun-p generic-function)
      (return-from make-checking-dfun (make-dispatch-dfun generic-function))))
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-fun-info generic-function)
    (declare (ignore nreq))
    (if (every (lambda (mt) (eq mt t)) metatypes)
        (let ((dfun-info (default-method-only-dfun-info)))
          (values
           (let* ((constructor (get-dfun-constructor 'emit-default-only metatypes applyp))
                  (fun (funcall constructor function))
                  (name (generic-function-name generic-function)))
             (set-fun-name fun `(default-only ,name))
             fun)
           nil
           dfun-info))
        (let* ((cache (or cache (make-cache :key-count nkeys :value nil :size 2)))
               (dfun-info (checking-dfun-info function cache)))
          (values
           (funcall (get-dfun-constructor 'emit-checking metatypes applyp)
                    cache
                    function
                    (lambda (&rest args)
                      (checking-miss generic-function args dfun-info)))
           cache
           dfun-info)))))

(defun make-final-checking-dfun (generic-function function classes-list new-class)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-fun-info generic-function)
    (declare (ignore nreq applyp nkeys))
    (if (every (lambda (mt) (eq mt t)) metatypes)
        (values (lambda (&rest args)
                  (invoke-emf function args))
                nil (default-method-only-dfun-info))
        (let ((cache (make-final-ordinary-dfun-cache
                      generic-function nil classes-list new-class)))
          (make-checking-dfun generic-function function cache)))))

(defun use-default-method-only-dfun-p (generic-function)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-fun-info generic-function)
    (declare (ignore nreq applyp nkeys))
    (every (lambda (mt) (eq mt t)) metatypes)))

(defun use-caching-dfun-p (generic-function)
  (some (lambda (method) (method-plist-value method :slot-name-lists))
        ;; KLUDGE: As of sbcl-0.6.4, it's very important for
        ;; efficiency to know the type of the sequence argument to
        ;; quantifiers (SOME/NOTANY/etc.) at compile time, but
        ;; the compiler isn't smart enough to understand the :TYPE
        ;; slot option for DEFCLASS, so we just tell
        ;; it the type by hand here.
        (the list
             (if (early-gf-p generic-function)
                 (early-gf-methods generic-function)
                 (generic-function-methods generic-function)))))

(defun make-caching-dfun (generic-function &optional cache)
  (unless cache
    (when (use-constant-value-dfun-p generic-function)
      (return-from make-caching-dfun
        (make-constant-value-dfun generic-function)))
    (when (use-dispatch-dfun-p generic-function)
      (return-from make-caching-dfun
        (make-dispatch-dfun generic-function))))
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-fun-info generic-function)
    (declare (ignore nreq))
    (let* ((cache (or cache (make-cache :key-count nkeys :value t :size 2)))
           (dfun-info (caching-dfun-info cache)))
      (values
       (funcall (get-dfun-constructor 'emit-caching metatypes applyp)
                cache
                (lambda (&rest args)
                  (caching-miss generic-function args dfun-info)))
       cache
       dfun-info))))

(defun make-final-caching-dfun (generic-function classes-list new-class)
  (let ((cache (make-final-ordinary-dfun-cache
                generic-function t classes-list new-class)))
    (make-caching-dfun generic-function cache)))

(defun insure-caching-dfun (gf)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-fun-info gf)
    (declare (ignore nreq nkeys))
    (when (and metatypes
               (not (null (car metatypes)))
               (dolist (mt metatypes nil)
                 (unless (eq mt t) (return t))))
      (get-dfun-constructor 'emit-caching metatypes applyp))))

(defun use-constant-value-dfun-p (gf &optional boolean-values-p)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-fun-info gf)
    (declare (ignore nreq metatypes nkeys))
    (let* ((early-p (early-gf-p gf))
           (methods (if early-p
                        (early-gf-methods gf)
                        (generic-function-methods gf)))
           (default '(unknown)))
      (and (null applyp)
           (or (not (eq **boot-state** 'complete))
               ;; If COMPUTE-APPLICABLE-METHODS is specialized, we
               ;; can't use this, of course, because we can't tell
               ;; which methods will be considered applicable.
               ;;
               ;; Also, don't use this dfun method if the generic
               ;; function has a non-standard method combination,
               ;; because if it has, it's not sure that method
               ;; functions are used directly as effective methods,
               ;; which CONSTANT-VALUE-MISS depends on.  The
               ;; pre-defined method combinations like LIST are
               ;; examples of that.
               (and (compute-applicable-methods-emf-std-p gf)
                    (eq (generic-function-method-combination gf)
                        *standard-method-combination*)))
           ;; Check that no method is eql-specialized, and that all
           ;; methods return a constant value.  If BOOLEAN-VALUES-P,
           ;; check that all return T or NIL.  Also, check that no
           ;; method has qualifiers, to make sure that emfs are really
           ;; method functions; see above.
           (dolist (method methods t)
             (when (eq **boot-state** 'complete)
               (when (or (some #'eql-specializer-p
                               (safe-method-specializers method))
                         (safe-method-qualifiers method))
                 (return nil)))
             (let ((value (method-plist-value method :constant-value default)))
               (when (or (eq value default)
                         (and boolean-values-p
                              (not (member value '(t nil)))))
                 (return nil))))))))

(defun make-constant-value-dfun (generic-function &optional cache)
  (multiple-value-bind (nreq applyp metatypes nkeys)
      (get-generic-fun-info generic-function)
    (declare (ignore nreq applyp))
    (let* ((cache (or cache (make-cache :key-count nkeys :value t :size 2)))
           (dfun-info (constant-value-dfun-info cache)))
      (declare (type cache cache))
      (values
       (funcall (get-dfun-constructor 'emit-constant-value metatypes)
                cache
                (lambda (&rest args)
                  (constant-value-miss generic-function args dfun-info)))
       cache
       dfun-info))))

(defun make-final-constant-value-dfun (generic-function classes-list new-class)
  (let ((cache (make-final-ordinary-dfun-cache
                generic-function :constant-value classes-list new-class)))
    (make-constant-value-dfun generic-function cache)))

(defun gf-has-method-with-nonstandard-specializer-p (gf)
  (let ((methods (generic-function-methods gf)))
    (dolist (method methods nil)
      (unless (every (lambda (s) (standard-specializer-p s))
                     (method-specializers method))
        (return t)))))

(defun use-dispatch-dfun-p (gf &optional (caching-p (use-caching-dfun-p gf)))
  (when (eq **boot-state** 'complete)
    (unless (or caching-p
                (gf-requires-emf-keyword-checks gf)
                ;; DISPATCH-DFUN-COST will error if it encounters a
                ;; method with a non-standard specializer.
                (gf-has-method-with-nonstandard-specializer-p gf))
      ;; This should return T when almost all dispatching is by
      ;; eql specializers or built-in classes. In other words,
      ;; return NIL if we might ever need to do more than
      ;; one (non built-in) typep.
      ;; Otherwise, it is probably at least as fast to use
      ;; a caching dfun first, possibly followed by secondary dispatching.

      #||;;; Original found in cmu 17f -- S L O W
      (< (dispatch-dfun-cost gf) (caching-dfun-cost gf))
      ||#
      ;; This uses improved dispatch-dfun-cost below
      (let ((cdc  (caching-dfun-cost gf))) ; fast
        (> cdc (dispatch-dfun-cost gf cdc))))))

(defparameter *non-system-typep-cost* 100)
(defparameter *structure-typep-cost*  15)
(defparameter *system-typep-cost* 5)

;;; According to comments in the original CMU CL version of PCL,
;;; the cost LIMIT is important to cut off exponential growth for
;;; large numbers of gf methods and argument lists.
(defun dispatch-dfun-cost (gf &optional limit)
  (generate-discrimination-net-internal
   gf (generic-function-methods gf) nil
   (lambda (methods known-types)
     (declare (ignore methods known-types))
     0)
   (lambda (position type true-value false-value)
     (declare (ignore position))
     (let* ((type-test-cost
             (if (eq 'class (car type))
                 (let* ((metaclass (class-of (cadr type)))
                        (mcpl (class-precedence-list metaclass)))
                   (cond ((memq *the-class-system-class* mcpl)
                          *system-typep-cost*)
                         ((memq *the-class-structure-class* mcpl)
                          *structure-typep-cost*)
                         (t *non-system-typep-cost*)))
                 0))
            (max-cost-so-far
             (+ (max true-value false-value) type-test-cost)))
       (when (and limit (<= limit max-cost-so-far))
         (return-from dispatch-dfun-cost max-cost-so-far))
       max-cost-so-far))
   #'identity))

(defparameter *cache-lookup-cost*  30)
(defparameter *wrapper-of-cost* 15)
(defparameter *secondary-dfun-call-cost* 30)

(defun caching-dfun-cost (gf)
  (let ((nreq (get-generic-fun-info gf)))
    (+ *cache-lookup-cost*
       (* *wrapper-of-cost* nreq)
       (if (methods-contain-eql-specializer-p
            (generic-function-methods gf))
           *secondary-dfun-call-cost*
           0))))

(declaim (inline make-callable))
(defun make-callable (generator method-alist wrappers)
  (funcall (the function generator) method-alist wrappers))

(defun make-dispatch-dfun (gf)
  (values (get-dispatch-function gf) nil (dispatch-dfun-info)))

(defun get-dispatch-function (gf)
  (let* ((methods (generic-function-methods gf))
         (generator (get-secondary-dispatch-function1
                     gf methods nil nil nil nil nil t)))
    (make-callable generator nil nil)))

(defun make-final-dispatch-dfun (gf)
  (make-dispatch-dfun gf))

(defun update-dispatch-dfuns ()
  (dolist (gf (gfs-of-type '(dispatch)))
    (dfun-update gf #'make-dispatch-dfun)))

(defun make-final-ordinary-dfun-cache
    (generic-function valuep classes-list new-class)
  (let* ((arg-info (gf-arg-info generic-function))
         (nkeys (arg-info-nkeys arg-info))
         (new-class (and new-class
                         (equal (type-of (gf-dfun-info generic-function))
                                (cond ((eq valuep t) 'caching)
                                      ((eq valuep :constant-value) 'constant-value)
                                      ((null valuep) 'checking)))
                         new-class))
         (cache (if new-class
                    (copy-cache (gf-dfun-cache generic-function))
                    (make-cache :key-count nkeys :value (not (null valuep))
                                :size 4))))
    (make-emf-cache generic-function valuep cache classes-list new-class)))

(defvar *dfun-miss-gfs-on-stack* ())

(defmacro dfun-miss ((gf args wrappers invalidp nemf
                      &optional type index caching-p applicable)
                     &body body)
  (unless applicable (setq applicable (gensym)))
  `(multiple-value-bind (,nemf ,applicable ,wrappers ,invalidp
                         ,@(when type `(,type ,index)))
       (cache-miss-values ,gf ,args ',(cond (caching-p 'caching)
                                            (type 'accessor)
                                            (t 'checking)))
    (when (and ,applicable (not (memq ,gf *dfun-miss-gfs-on-stack*)))
      (let ((*dfun-miss-gfs-on-stack* (cons ,gf *dfun-miss-gfs-on-stack*)))
        ,@body))
    ,(if type
         ;; Munge the EMF so that INVOKE-EMF can do the right thing:
         ;; BOUNDP and MAKUNBOUND get a structure, WRITER the logical
         ;; not of the index, so that READER can use the raw index.
         ;;
         ;; FIXME: could the NEMF not be a CONS (for :CLASS-allocated
         ;; slots?)
         `(if (integerp ,nemf)
              (case ,type
                (makunbound
                 (invoke-emf (make-fast-instance-boundp :index (lognot ,nemf)) ,args))
                (boundp (invoke-emf (make-fast-instance-boundp :index ,nemf) ,args))
                (reader (invoke-emf ,nemf ,args))
                (writer (invoke-emf (lognot ,nemf) ,args)))
              (invoke-emf ,nemf ,args))
         `(invoke-emf ,nemf ,args))))

;;; The dynamically adaptive method lookup algorithm is implemented is
;;; implemented as a kind of state machine. The kinds of
;;; discriminating function is the state, the various kinds of reasons
;;; for a cache miss are the state transitions.
;;;
;;; The code which implements the transitions is all in the miss
;;; handlers for each kind of dfun. Those appear here.
;;;
;;; Note that within the states that cache, there are dfun updates
;;; which simply select a new cache or cache field. Those are not
;;; considered as state transitions.
(defvar *early-p* nil)

(defun make-initial-dfun (gf)
  (let ((initial-dfun #'(lambda (&rest args) (initial-dfun gf args))))
    (multiple-value-bind (dfun cache info)
        (if (eq **boot-state** 'complete)
            (values initial-dfun nil (initial-dfun-info))
            (let ((arg-info (if (early-gf-p gf)
                                (early-gf-arg-info gf)
                                (gf-arg-info gf)))
                  (type nil))
              (if (and (gf-precompute-dfun-and-emf-p arg-info)
                       (setq type (final-accessor-dfun-type gf)))
                  (if *early-p*
                      (values (make-early-accessor gf type) nil nil)
                      (make-final-accessor-dfun gf type))
                  (values initial-dfun nil (initial-dfun-info)))))
      (set-dfun gf dfun cache info))))

(defun make-early-accessor (gf type)
  (let* ((methods (early-gf-methods gf))
         (slot-name (early-method-standard-accessor-slot-name (car methods))))
    (ecase type
      (reader #'(lambda (instance)
                  (let* ((class (class-of instance))
                         (class-name (!bootstrap-get-slot 'class class 'name)))
                    (!bootstrap-get-slot class-name instance slot-name))))
      (boundp #'(lambda (instance)
                  (let* ((class (class-of instance))
                         (class-name (!bootstrap-get-slot 'class class 'name)))
                    (not (unbound-marker-p
                          (!bootstrap-get-slot class-name instance slot-name))))))
      (writer #'(lambda (new-value instance)
                  (let* ((class (class-of instance))
                         (class-name (!bootstrap-get-slot 'class class 'name)))
                    (!bootstrap-set-slot class-name instance slot-name new-value))))
      (makunbound #'(lambda (instance)
                      (let* ((class (class-of instance))
                             (class-name (!bootstrap-get-slot 'class class 'name)))
                        (!bootstrap-set-slot class-name instance slot-name +slot-unbound+)
                        instance))))))

(defun initial-dfun (gf args)
  (dfun-miss (gf args wrappers invalidp nemf ntype nindex)
    (cond (invalidp)
          ((and ntype nindex)
           (dfun-update
            gf #'make-one-class-accessor-dfun ntype wrappers nindex))
          ((use-caching-dfun-p gf)
           (dfun-update gf #'make-caching-dfun))
          (t
           (dfun-update gf #'make-checking-dfun
            ;; nemf is suitable only for caching, have to do this:
            (cache-miss-values gf args 'checking))))))

(defun make-final-dfun (gf &optional classes-list)
  (multiple-value-bind (dfun cache info)
      (make-final-dfun-internal gf classes-list)
    (set-dfun gf dfun cache info)))

;;; FIXME: What is this?
(defvar *new-class* nil)

(defun final-accessor-dfun-type (gf)
  (let ((methods (if (early-gf-p gf)
                     (early-gf-methods gf)
                     (generic-function-methods gf))))
    (cond ((every (lambda (method)
                    (if (consp method)
                        (let ((class (early-method-class method)))
                          (or (eq class *the-class-standard-reader-method*)
                              (eq class *the-class-global-reader-method*)))
                        (or (standard-reader-method-p method)
                            (global-reader-method-p method))))
                  methods)
           'reader)
          ((every (lambda (method)
                    (if (consp method)
                        (let ((class (early-method-class method)))
                          (or (eq class *the-class-standard-writer-method*)
                              (eq class *the-class-global-writer-method*)))
                        (and
                         (or (standard-writer-method-p method)
                             (global-writer-method-p method))
                         (not (safe-p
                               (slot-definition-class
                                (accessor-method-slot-definition method)))))))
                  methods)
           'writer)
          ((every (lambda (method)
                    (if (consp method)
                        (let ((class (early-method-class method)))
                          (eq class *the-class-global-boundp-method*))
                        (global-boundp-method-p method)))
                  methods)
           'boundp)
          ((every (lambda (method)
                    (if (consp method)
                        (let ((class (early-method-class method)))
                          (eq class *the-class-global-makunbound-method*))
                        (global-makunbound-method-p method)))
                  methods)
           'makunbound))))

(defun make-final-accessor-dfun (gf type &optional classes-list new-class)
  (let ((table (make-hash-table :test #'eq)))
    (multiple-value-bind (table all-index first second size no-class-slots-p)
        (make-accessor-table gf type table)
      (if table
          (cond ((= size 1)
                 (let ((w (class-wrapper first)))
                   (make-one-class-accessor-dfun gf type w all-index)))
                ((and (= size 2) (or (integerp all-index) (consp all-index)))
                 (let ((w0 (class-wrapper first))
                       (w1 (class-wrapper second)))
                   (make-two-class-accessor-dfun gf type w0 w1 all-index)))
                ((or (integerp all-index) (consp all-index))
                 (let ((cache (hash-table-to-cache table :value nil :key-count 1)))
                   (make-one-index-accessor-dfun gf type all-index cache)))
                (no-class-slots-p
                 (let ((cache (hash-table-to-cache table :value t :key-count 1)))
                   (make-n-n-accessor-dfun gf type cache)))
                (t
                 (make-final-caching-dfun gf classes-list new-class)))
          (make-final-caching-dfun gf classes-list new-class)))))

(defun make-final-dfun-internal (gf &optional classes-list)
  (let ((methods (generic-function-methods gf)) type
        (new-class *new-class*) (*new-class* nil)
        specls all-same-p)
    (cond ((null methods)
           (values
            #'(lambda (&rest args)
                (call-no-applicable-method gf args))
            nil
            (no-methods-dfun-info)))
          ((setq type (final-accessor-dfun-type gf))
           (make-final-accessor-dfun gf type classes-list new-class))
          ((and (not (and (every (lambda (specl) (eq specl *the-class-t*))
                                 (setq specls
                                       (method-specializers (car methods))))
                          (setq all-same-p
                                (every (lambda (method)
                                         (and (equal specls
                                                     (method-specializers
                                                      method))))
                                       methods))))
                (use-constant-value-dfun-p gf))
           (make-final-constant-value-dfun gf classes-list new-class))
          ((use-dispatch-dfun-p gf)
           (make-final-dispatch-dfun gf))
          ((and all-same-p (not (use-caching-dfun-p gf)))
           (let ((emf (get-secondary-dispatch-function gf methods nil)))
             (make-final-checking-dfun gf emf classes-list new-class)))
          (t
           (make-final-caching-dfun gf classes-list new-class)))))

(defun accessor-miss (gf new object dfun-info)
  (let* ((ostate (type-of dfun-info))
         (otype (dfun-info-accessor-type dfun-info))
         oindex ow0 ow1 cache
         (args (ecase otype
                 ((reader boundp makunbound) (list object))
                 (writer (list new object)))))
    (dfun-miss (gf args wrappers invalidp nemf ntype nindex)
      ;; The following lexical functions change the state of the
      ;; dfun to that which is their name.  They accept arguments
      ;; which are the parameters of the new state, and get other
      ;; information from the lexical variables bound above.
      (flet ((two-class (index w0 w1)
               (when (zerop (random 2 (load-time-value *pcl-misc-random-state*)))
                 (psetf w0 w1 w1 w0))
               (dfun-update gf
                            #'make-two-class-accessor-dfun
                            ntype
                            w0
                            w1
                            index))
             (one-index (index &optional cache)
               (dfun-update gf
                            #'make-one-index-accessor-dfun
                            ntype
                            index
                            cache))
             (n-n (&optional cache)
               (if (consp nindex)
                   (dfun-update gf #'make-checking-dfun nemf)
                   (dfun-update gf #'make-n-n-accessor-dfun ntype cache)))
             (caching () ; because cached accessor emfs are much faster
                         ; for accessors
               (dfun-update gf #'make-caching-dfun))
             (do-fill (update-fn)
               (let ((ncache (fill-cache cache wrappers nindex)))
                 (unless (eq ncache cache)
                   (funcall update-fn ncache)))))
        (cond ((null ntype)
               (caching))
              ((or invalidp
                   (null nindex)))
              ((not (pcl-instance-p object))
               (caching))
              ((or (neq ntype otype) (listp wrappers))
               (caching))
              (t
               (ecase ostate
                 (one-class
                  (setq oindex (dfun-info-index dfun-info))
                  (setq ow0 (dfun-info-wrapper0 dfun-info))
                  (unless (eq ow0 wrappers)
                    (if (eql nindex oindex)
                        (two-class nindex ow0 wrappers)
                        (n-n))))
                 (two-class
                  (setq oindex (dfun-info-index dfun-info))
                  (setq ow0 (dfun-info-wrapper0 dfun-info))
                  (setq ow1 (dfun-info-wrapper1 dfun-info))
                  (unless (or (eq ow0 wrappers) (eq ow1 wrappers))
                    (if (eql nindex oindex)
                        (one-index nindex)
                        (n-n))))
                 (one-index
                  (setq oindex (dfun-info-index dfun-info))
                  (setq cache (dfun-info-cache dfun-info))
                  (if (eql nindex oindex)
                      (do-fill (lambda (ncache)
                                 (one-index nindex ncache)))
                      (n-n)))
                 (n-n
                  (setq cache (dfun-info-cache dfun-info))
                  (if (consp nindex)
                      (caching)
                      (do-fill #'n-n))))))))))

(defun checking-miss (generic-function args dfun-info)
  (let ((oemf (dfun-info-function dfun-info))
        (cache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp nemf)
      (cond (invalidp)
            ((eq oemf nemf)
             ;; The cache of a checking dfun doesn't hold any values,
             ;; so this NIL appears to be just a dummy-value we use in
             ;; order to insert the wrappers into the cache.
             (let ((ncache (fill-cache cache wrappers nil)))
               (unless (eq ncache cache)
                 (dfun-update generic-function #'make-checking-dfun
                              nemf ncache))))
            (t
             (dfun-update generic-function #'make-caching-dfun))))))

(defun caching-miss (generic-function args dfun-info)
  (let ((ocache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp emf nil nil t)
      (cond (invalidp)
            (t
             (let ((ncache (fill-cache ocache wrappers emf)))
               (unless (eq ncache ocache)
                 (dfun-update generic-function
                              #'make-caching-dfun ncache))))))))

(defun constant-value-miss (generic-function args dfun-info)
  (let ((ocache (dfun-info-cache dfun-info)))
    (dfun-miss (generic-function args wrappers invalidp emf nil nil t)
      (unless invalidp
        (let* ((value
                (typecase emf
                  (constant-fast-method-call
                   (constant-fast-method-call-value emf))
                  (constant-method-call
                   (constant-method-call-value emf))
                  (t
                   (bug "~S with non-constant EMF ~S" 'constant-value-miss emf))))
               (ncache (fill-cache ocache wrappers value)))
          (unless (eq ncache ocache)
            (dfun-update generic-function
                         #'make-constant-value-dfun ncache)))))))

;;; Given a generic function and a set of arguments to that generic
;;; function, return a mess of values.
;;;
;;;  <function>   The compiled effective method function for this set of
;;;            arguments.
;;;
;;;  <applicable> Sorted list of applicable methods.
;;;
;;;  <wrappers>   Is a single wrapper if the generic function has only
;;;            one key, that is arg-info-nkeys of the arg-info is 1.
;;;            Otherwise a list of the wrappers of the specialized
;;;            arguments to the generic function.
;;;
;;;            Note that all these wrappers are valid. This function
;;;            does invalid wrapper traps when it finds an invalid
;;;            wrapper and then returns the new, valid wrapper.
;;;
;;;  <invalidp>   True if any of the specialized arguments had an invalid
;;;            wrapper, false otherwise.
;;;
;;;  <type>       READER or WRITER when the only method that would be run
;;;            is a standard reader or writer method. To be specific,
;;;            the value is READER when the method combination is eq to
;;;            *standard-method-combination*; there are no applicable
;;;            :before, :after or :around methods; and the most specific
;;;            primary method is a standard reader method.
;;;
;;;  <index>      If <type> is READER or WRITER, and the slot accessed is
;;;            an :instance slot, this is the index number of that slot
;;;            in the object argument.
(defvar *cache-miss-values-stack* ())

(defun cache-miss-values (gf args state)
  (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
      (get-generic-fun-info gf)
    (declare (ignore nreq applyp nkeys))
    (with-dfun-wrappers (args metatypes)
      (dfun-wrappers invalid-wrapper-p wrappers classes types)
      (error-need-at-least-n-args gf (length metatypes))
      (multiple-value-bind (emf methods accessor-type index)
          (cache-miss-values-internal
           gf arg-info wrappers classes types state)
        (values emf methods
                dfun-wrappers
                invalid-wrapper-p
                accessor-type index)))))

(defun cache-miss-values-internal (gf arg-info wrappers classes types state)
  (if (and classes (equal classes (cdr (assq gf *cache-miss-values-stack*))))
      (break-vicious-metacircle gf classes arg-info)
      (let ((*cache-miss-values-stack*
             (acons gf classes *cache-miss-values-stack*))
            (cam-std-p (or (null arg-info)
                           (gf-info-c-a-m-emf-std-p arg-info))))
        (multiple-value-bind (methods all-applicable-and-sorted-p)
            (if cam-std-p
                (compute-applicable-methods-using-types gf types)
                (compute-applicable-methods-using-classes gf classes))

  (let* ((for-accessor-p (eq state 'accessor))
         (for-cache-p (or (eq state 'caching) (eq state 'accessor)))
         (emf (if (or cam-std-p all-applicable-and-sorted-p)
                  (let ((generator
                         (get-secondary-dispatch-function1
                          gf methods types nil (and for-cache-p wrappers)
                          all-applicable-and-sorted-p)))
                    (make-callable generator nil (and for-cache-p wrappers)))
                  (default-secondary-dispatch-function gf))))
    (multiple-value-bind (index accessor-type)
        (and for-accessor-p all-applicable-and-sorted-p methods
             (accessor-values gf arg-info classes methods))
      (values (if (integerp index) index emf)
              methods accessor-type index)))))))

;;; Try to break a vicious circle while computing a cache miss.
;;; GF is the generic function, CLASSES are the classes of actual
;;; arguments, and ARG-INFO is the generic functions' arg-info.
;;;
;;; A vicious circle can be entered when the computation of the cache
;;; miss values itself depends on the values being computed.  For
;;; instance, adding a method which is an instance of a subclass of
;;; STANDARD-METHOD leads to cache misses for slot accessors of
;;; STANDARD-METHOD like METHOD-SPECIALIZERS, and METHOD-SPECIALIZERS
;;; is itself used while we compute cache miss values.
(defun break-vicious-metacircle (gf classes arg-info)
  (when (typep gf 'standard-generic-function)
    (multiple-value-bind (class slotd accessor-type)
        (accesses-standard-class-slot-p gf)
      (when class
        (let ((method (find-standard-class-accessor-method
                       gf class accessor-type))
              (index (standard-slot-value/eslotd slotd 'location))
              (type (gf-info-simple-accessor-type arg-info)))
          (when (and method
                     (let ((method-class (ecase accessor-type
                                           ((reader) (car classes))
                                           ((writer) (cadr classes)))))
                       (or (eq method-class class)
                           ;; SUBTYPEP doesn't work because it calls the CLASS-WRAPPER GF.
                           (block nil
                             (sb-kernel::do-subclassoids ((subclassoid layout)
                                                          (layout-classoid (standard-slot-value/class class 'wrapper)))
                               (declare (ignore layout))
                               (when (eq method-class (classoid-pcl-class subclassoid))
                                 (return t)))))))
            (return-from break-vicious-metacircle
              (values index (list method) type index)))))))
  (error "~@<vicious metacircle:  The computation of an ~
          effective method of ~s for arguments of types ~s uses ~
          the effective method being computed.~@:>"
         gf classes))

;;; Return (CLASS SLOTD ACCESSOR-TYPE) if some method of generic
;;; function GF accesses a slot of some class in +STANDARD-CLASSES+.
;;; CLASS is the class accessed, SLOTD is the effective slot definition
;;; object of the slot accessed, and ACCESSOR-TYPE is one of the symbols
;;; READER or WRITER describing the slot access.
(defun accesses-standard-class-slot-p (gf)
  (labels
      ((all-dslotds (class &aux done)
         (labels ((all-dslotds-aux (class)
                    (if (or (member class done) (not (eq (class-of class) *the-class-standard-class*)))
                        nil
                        (progn
                          (push class done)
                          (append (standard-slot-value/class class 'direct-slots)
                                  (mapcan #'(lambda (c)
                                              (copy-list (all-dslotds-aux c)))
                                          (standard-slot-value/class class 'direct-superclasses)))))))
           (all-dslotds-aux class)))
       (standard-class-slot-access (gf class)

         (loop with gf-name = (standard-slot-value/gf gf 'name)
            with eslotds = (standard-slot-value/class class 'slots)
            with dslotds = (all-dslotds class)
            for dslotd in dslotds
            as readers = (standard-slot-value/dslotd dslotd 'readers)
            as writers = (standard-slot-value/dslotd dslotd 'writers)
            as name = (standard-slot-value/dslotd dslotd 'name)
            as eslotd = (find name eslotds :key (lambda (x) (standard-slot-value/eslotd x 'name)))
            if (member gf-name readers :test #'equal)
            return (values eslotd 'reader)
            else if (member gf-name writers :test #'equal)
            return (values eslotd 'writer))))
    (dolist (class-name +standard-classes+)
      (let ((class (find-class class-name)))
        (multiple-value-bind (slotd accessor-type)
            (standard-class-slot-access gf class)
          (when slotd
            (return (values class slotd accessor-type))))))))

;;; Find a slot reader/writer method among the methods of generic
;;; function GF which reads/writes instances of class CLASS.
;;; TYPE is one of the symbols READER or WRITER.
(defun find-standard-class-accessor-method (gf class type)
  (let ((cpl (standard-slot-value/class class '%class-precedence-list))
        (found-specializer *the-class-t*)
        (found-method nil))
    (dolist (method (standard-slot-value/gf gf 'methods) found-method)
      (let ((specializers (standard-slot-value/method method 'specializers))
            (qualifiers (standard-slot-value/method method 'qualifiers)))
        (when (and (null qualifiers)
                   (let ((subcpl (member (ecase type
                                           (reader (car specializers))
                                           (writer (cadr specializers)))
                                         cpl :test #'eq)))
                     (and subcpl (member found-specializer subcpl :test #'eq))))
          (setf found-specializer (ecase type
                                    (reader (car specializers))
                                    (writer (cadr specializers))))
          (setf found-method method))))))

(defun accessor-values (gf arg-info classes methods)
  (declare (ignore gf))
  (let* ((accessor-type (gf-info-simple-accessor-type arg-info))
         (accessor-class (case accessor-type
                           ((reader boundp makunbound) (car classes))
                           (writer (cadr classes)))))
    (accessor-values-internal accessor-type accessor-class methods)))

(defun accessor-values1 (gf accessor-type accessor-class)
  (let* ((type `(class-eq ,accessor-class))
         (types (ecase accessor-type
                  ((reader boundp makunbound) `(,type))
                  (writer `(t ,type))))
         (methods (compute-applicable-methods-using-types gf types)))
    (accessor-values-internal accessor-type accessor-class methods)))

(defun accessor-values-internal (accessor-type accessor-class methods)
  (unless accessor-class
    (return-from accessor-values-internal (values nil nil)))
  (dolist (meth methods)
    (when (if (consp meth)
              (early-method-qualifiers meth)
              (safe-method-qualifiers meth))
      (return-from accessor-values-internal (values nil nil))))
  (let* ((meth (car methods))
         (early-p (not (eq **boot-state** 'complete)))
         (slot-name
          (cond
            ((and (consp meth)
                  (early-method-standard-accessor-p meth))
             (early-method-standard-accessor-slot-name meth))
            ((and (accessor-method-p meth)
                  (member *the-class-standard-object*
                          (if early-p
                              (early-class-precedence-list accessor-class)
                              (class-precedence-list accessor-class))))
             (accessor-method-slot-name meth))
            (t (return-from accessor-values-internal (values nil nil)))))
         (slotd (if early-p
                    (dolist (slot (early-class-slotds accessor-class) nil)
                      (when (eql slot-name (early-slot-definition-name slot))
                        (return slot)))
                    (find-slot-definition accessor-class slot-name))))
    (when (and slotd
               (or early-p (slot-accessor-std-p slotd accessor-type))
               (or early-p (not (safe-p accessor-class))))
      (values (if early-p
                  (early-slot-definition-location slotd)
                  (slot-definition-location slotd))
              accessor-type))))

(defun make-accessor-table (gf type &optional table)
  (unless table (setq table (make-hash-table :test 'eq)))
  (let ((methods (if (early-gf-p gf)
                     (early-gf-methods gf)
                     (generic-function-methods gf)))
        (all-index nil)
        (no-class-slots-p t)
        (early-p (not (eq **boot-state** 'complete)))
        first second (size 0))
    (declare (fixnum size))
    ;; class -> {(specl slotd)}
    (dolist (method methods)
      (let* ((specializers (if (consp method)
                               (early-method-specializers method t)
                               (method-specializers method)))
             (specl (ecase type
                      ((reader boundp makunbound) (car specializers))
                      (writer (cadr specializers))))
             (specl-cpl (if early-p
                            (early-class-precedence-list specl)
                            (when (class-finalized-p specl)
                              (class-precedence-list specl))))
             (so-p (member *the-class-standard-object* specl-cpl :test #'eq))
             (slot-name (if (consp method)
                            (and (early-method-standard-accessor-p method)
                                 (early-method-standard-accessor-slot-name
                                  method))
                            (accessor-method-slot-name method))))
        (when (or (null specl-cpl)
                  (null so-p)
                  (member *the-class-structure-object* specl-cpl :test #'eq))
          (return-from make-accessor-table nil))
        ;; Collect all the slot-definitions for SLOT-NAME from SPECL and
        ;; all of its subclasses. If either SPECL or one of the subclasses
        ;; is not a standard-class, bail out.
        (labels ((aux (class)
                   (let ((slotd (find-slot-definition class slot-name)))
                     (when slotd
                       (unless (or early-p (slot-accessor-std-p slotd type))
                         (return-from make-accessor-table nil))
                       (push (cons specl slotd) (gethash class table))))
                   (dolist (subclass (sb-pcl::class-direct-subclasses class))
                     (unless (class-finalized-p subclass)
                       (return-from make-accessor-table nil))
                     (aux subclass))))
          (aux specl))))
    (maphash (lambda (class specl+slotd-list)
               (dolist (sclass (if early-p
                                   (early-class-precedence-list class)
                                   (class-precedence-list class))
                               (error "This can't happen."))
                 (let ((a (assq sclass specl+slotd-list)))
                   (when a
                     (let* ((slotd (cdr a))
                            (index (if early-p
                                       (early-slot-definition-location slotd)
                                       (slot-definition-location slotd))))
                       (unless index (return-from make-accessor-table nil))
                       (setf (gethash class table) index)
                       (when (consp index) (setq no-class-slots-p nil))
                       (setq all-index (if (or (null all-index)
                                               (eql all-index index))
                                           index t))
                       (incf size)
                       (cond ((= size 1) (setq first class))
                             ((= size 2) (setq second class)))
                       (return nil))))))
             table)
    (values table all-index first second size no-class-slots-p)))

(defun compute-applicable-methods-using-types (generic-function types)
  (let ((definite-p t) (possibly-applicable-methods nil))
    (dolist (method (if (early-gf-p generic-function)
                        (early-gf-methods generic-function)
                        (safe-generic-function-methods generic-function)))
      (let ((specls (if (consp method)
                        (early-method-specializers method t)
                        (safe-method-specializers method)))
            (types types)
            (possibly-applicable-p t) (applicable-p t))
        (dolist (specl specls)
          (multiple-value-bind (specl-applicable-p specl-possibly-applicable-p)
              (specializer-applicable-using-type-p specl (pop types))
            (unless specl-applicable-p
              (setq applicable-p nil))
            (unless specl-possibly-applicable-p
              (setq possibly-applicable-p nil)
              (return nil))))
        (when possibly-applicable-p
          (unless applicable-p (setq definite-p nil))
          (push method possibly-applicable-methods))))
    (multiple-value-bind (nreq applyp metatypes nkeys arg-info)
        (get-generic-fun-info generic-function)
      (declare (ignore nreq applyp metatypes nkeys))
      (let* ((precedence (arg-info-precedence arg-info)))
        (values (sort-applicable-methods precedence
                                         (nreverse possibly-applicable-methods)
                                         types)
                definite-p)))))

(defun sort-applicable-methods (precedence methods types)
  (sort-methods methods
                precedence
                (lambda (class1 class2 index)
                  (let* ((class (type-class (nth index types)))
                         (cpl (if (eq **boot-state** 'complete)
                                  (class-precedence-list class)
                                  (early-class-precedence-list class))))
                    (if (memq class2 (memq class1 cpl))
                        class1 class2)))))

(defun sort-methods (methods precedence compare-classes-function)
  (flet ((sorter (method1 method2)
           (dolist (index precedence)
             (let* ((specl1 (nth index (if (listp method1)
                                           (early-method-specializers method1
                                                                      t)
                                           (method-specializers method1))))
                    (specl2 (nth index (if (listp method2)
                                           (early-method-specializers method2
                                                                      t)
                                           (method-specializers method2))))
                    (order (order-specializers
                             specl1 specl2 index compare-classes-function)))
               (when order
                 (return-from sorter (eq order specl1)))))))
    (stable-sort methods #'sorter)))

(defun order-specializers (specl1 specl2 index compare-classes-function)
  (let ((type1 (if (eq **boot-state** 'complete)
                   (specializer-type specl1)
                   (!bootstrap-get-slot 'specializer specl1 '%type)))
        (type2 (if (eq **boot-state** 'complete)
                   (specializer-type specl2)
                   (!bootstrap-get-slot 'specializer specl2 '%type))))
    (cond ((eq specl1 specl2)
           nil)
          ((atom type1)
           specl2)
          ((atom type2)
           specl1)
          (t
           (case (car type1)
             (class    (case (car type2)
                         (class (funcall compare-classes-function
                                         specl1 specl2 index))
                         (t specl2)))
             (prototype (case (car type2)
                         (class (funcall compare-classes-function
                                         specl1 specl2 index))
                         (t specl2)))
             (class-eq (case (car type2)
                         (eql specl2)
                         ;; FIXME: This says that all CLASS-EQ
                         ;; specializers are equally specific, which
                         ;; is fair enough because only one CLASS-EQ
                         ;; specializer can ever be appliable.  If
                         ;; ORDER-SPECIALIZERS should only ever be
                         ;; called on specializers from applicable
                         ;; methods, we could replace this with a BUG.
                         (class-eq nil)
                         (class type1)))
             (eql      (case (car type2)
                         ;; similarly.
                         (eql nil)
                         (t specl1))))))))

(defun map-all-orders (methods precedence function)
  (let ((choices nil))
    (flet ((compare-classes-function (class1 class2 index)
             (declare (ignore index))
             (let ((choice nil))
               (dolist (c choices nil)
                 (when (or (and (eq (first c) class1)
                                (eq (second c) class2))
                           (and (eq (first c) class2)
                                (eq (second c) class1)))
                   (return (setq choice c))))
               (unless choice
                 (setq choice
                       (if (class-might-precede-p class1 class2)
                           (if (class-might-precede-p class2 class1)
                               (list class1 class2 nil t)
                               (list class1 class2 t))
                           (if (class-might-precede-p class2 class1)
                               (list class2 class1 t)
                               (let ((name1 (class-name class1))
                                     (name2 (class-name class2)))
                                 (if (and name1
                                          name2
                                          (symbolp name1)
                                          (symbolp name2)
                                          (string< (symbol-name name1)
                                                   (symbol-name name2)))
                                     (list class1 class2 t)
                                     (list class2 class1 t))))))
                 (push choice choices))
               (car choice))))
      (loop (funcall function
                     (sort-methods methods
                                   precedence
                                   #'compare-classes-function))
            (unless (dolist (c choices nil)
                      (unless (third c)
                        (rotatef (car c) (cadr c))
                        (return (setf (third c) t))))
              (return nil))))))

;;; CMUCL comment: used only in map-all-orders
(defun class-might-precede-p (class1 class2)
  (not (member class1 (cdr (class-precedence-list class2)) :test #'eq)))

(defun compute-precedence (lambda-list nreq argument-precedence-order)
  (if (null argument-precedence-order)
      (let ((list nil))
        (dotimes-fixnum (i nreq list) (push (- (1- nreq) i) list)))
      (mapcar (lambda (x) (position x lambda-list))
              argument-precedence-order)))

(defun cpl-or-nil (class)
  (if (eq **boot-state** 'complete)
      (progn
        ;; KLUDGE: why not use (slot-boundp class
        ;; 'class-precedence-list)?  Well, unfortunately, CPL-OR-NIL is
        ;; used within COMPUTE-APPLICABLE-METHODS, including for
        ;; SLOT-BOUNDP-USING-CLASS... and the available mechanism for
        ;; breaking such nasty cycles in effective method computation
        ;; only works for readers and writers, not boundps.  It might
        ;; not be too hard to make it work for BOUNDP accessors, but in
        ;; the meantime we use an extra slot for exactly the result of
        ;; the SLOT-BOUNDP that we want.  (We cannot use
        ;; CLASS-FINALIZED-P, because in the process of class
        ;; finalization we need to use the CPL which has been computed
        ;; to cache effective methods for slot accessors.) -- CSR,
        ;; 2004-09-19.

        (when (cpl-available-p class)
          (return-from cpl-or-nil (class-precedence-list class)))

        ;; if we can finalize an unfinalized class, then do so
        (when (and (not (class-finalized-p class))
                   (not (class-has-a-forward-referenced-superclass-p class))
                   (not (class-has-a-cpl-protocol-violation-p class)))
          (finalize-inheritance class)
          (class-precedence-list class)))

      (early-class-precedence-list class)))

(defun saut-and (specl type)
  (let ((applicable nil)
        (possibly-applicable t))
    (dolist (type (cdr type))
      (multiple-value-bind (appl poss-appl)
          (specializer-applicable-using-type-p specl type)
        (when appl (return (setq applicable t)))
        (unless poss-appl (return (setq possibly-applicable nil)))))
    (values applicable possibly-applicable)))

(defun saut-not (specl type)
  (let ((ntype (cadr type)))
    (values nil
            (case (car ntype)
              (class      (saut-not-class specl ntype))
              (class-eq   (saut-not-class-eq specl ntype))
              (prototype  (saut-not-prototype specl ntype))
              (eql      (saut-not-eql specl ntype))
              (t (error "~S cannot handle the second argument ~S"
                        'specializer-applicable-using-type-p type))))))

(defun saut-not-class (specl ntype)
  (let* ((class (type-class specl))
         (cpl (cpl-or-nil class)))
    (not (memq (cadr ntype) cpl))))

(defun saut-not-prototype (specl ntype)
  (let* ((class (case (car specl)
                  (eql       (class-of (cadr specl)))
                  (class-eq  (cadr specl))
                  (prototype (cadr specl))
                  (class     (cadr specl))))
         (cpl (cpl-or-nil class)))
    (not (memq (cadr ntype) cpl))))

(defun saut-not-class-eq (specl ntype)
  (let ((class (case (car specl)
                 (eql      (class-of (cadr specl)))
                 (class-eq (cadr specl)))))
    (not (eq class (cadr ntype)))))

(defun saut-not-eql (specl ntype)
  (case (car specl)
    (eql (not (eql (cadr specl) (cadr ntype))))
    (t   t)))

(defun class-applicable-using-class-p (specl type)
  (let ((pred (memq specl (cpl-or-nil type))))
    (values pred
            (or pred
                (if (not *in-*subtypep*)
                    ;; classes might get common subclass
                    (superclasses-compatible-p specl type)
                    ;; worry only about existing classes
                    (classes-have-common-subclass-p specl type))))))

(defun classes-have-common-subclass-p (class1 class2)
  (or (eq class1 class2)
      (let ((class1-subs (class-direct-subclasses class1)))
        (or (memq class2 class1-subs)
            (dolist (class1-sub class1-subs nil)
              (when (classes-have-common-subclass-p class1-sub class2)
                (return t)))))))

(defun saut-class (specl type)
  (case (car specl)
    (class (class-applicable-using-class-p (cadr specl) (cadr type)))
    (t     (values nil (let ((class (type-class specl)))
                         (memq (cadr type)
                               (cpl-or-nil class)))))))

(defun saut-class-eq (specl type)
  (if (eq (car specl) 'eql)
      (values nil (eq (class-of (cadr specl)) (cadr type)))
      (let ((pred (case (car specl)
                    (class-eq
                     (eq (cadr specl) (cadr type)))
                    (class
                     (or (eq (cadr specl) (cadr type))
                         (memq (cadr specl) (cpl-or-nil (cadr type))))))))
        (values pred pred))))

(defun saut-prototype (specl type)
  (declare (ignore specl type))
  (values nil nil)) ; XXX original PCL comment: fix this someday

(defun saut-eql (specl type)
  (let ((pred (case (car specl)
                (eql    (eql (cadr specl) (cadr type)))
                (class-eq   (eq (cadr specl) (class-of (cadr type))))
                (class      (memq (cadr specl)
                                  (let ((class (class-of (cadr type))))
                                    (cpl-or-nil class)))))))
    (values pred pred)))

(defun specializer-applicable-using-type-p (specl type)
  (setq specl (type-from-specializer specl))
  (when (eq specl t)
    (return-from specializer-applicable-using-type-p (values t t)))
  ;; This is used by C-A-M-U-T and GENERATE-DISCRIMINATION-NET-INTERNAL,
  ;; and has only what they need.
  (if (or (atom type) (eq (car type) t))
      (values nil t)
      (case (car type)
        (and    (saut-and specl type))
        (not    (saut-not specl type))
        (class      (saut-class specl type))
        (prototype  (saut-prototype specl type))
        (class-eq   (saut-class-eq specl type))
        (eql    (saut-eql specl type))
        (t        (error "~S cannot handle the second argument ~S."
                           'specializer-applicable-using-type-p
                           type)))))

(defun map-all-classes (fun &optional (root t))
  (let ((all-classes (make-hash-table :test 'eq))
        (braid-p (or (eq **boot-state** 'braid)
                     (eq **boot-state** 'complete))))
    (labels ((do-class (class)
               (unless (gethash class all-classes)
                 (setf (gethash class all-classes) t)
                 (funcall fun class)
                 (mapc #'do-class
                       (if braid-p
                           (class-direct-subclasses class)
                           (early-class-direct-subclasses class))))))
      (do-class (if (symbolp root)
                    (find-class root)
                    root)))
    nil))

(defun flush-effective-method-cache (generic-function)
  (dolist (method (generic-function-methods generic-function))
    (let ((cache
           (if (listp method) (sixth method) (method-em-cache method))))
      (when cache
        (rplaca cache nil)
        (rplacd cache nil)))))

(defun get-secondary-dispatch-function (gf methods types
                                        &optional method-alist wrappers)
  (let ((generator
         (get-secondary-dispatch-function1
          gf methods types (not (null method-alist)) (not (null wrappers))
          (not (methods-contain-eql-specializer-p methods)))))
    (make-callable generator method-alist wrappers)))

(defun get-secondary-dispatch-function1 (gf methods types method-alist-p
                                            wrappers-p
                                            &optional
                                            all-applicable-p
                                            (all-sorted-p t)
                                            function-p)
  (if (null methods)
      (lambda (method-alist wrappers)
        (declare (ignore method-alist wrappers))
        (lambda (&rest args)
          (call-no-applicable-method gf args)))
      (let* ((key (car methods))
             (cache
              (if (listp key)           ; early method
                  (sixth key)           ; See !EARLY-MAKE-A-METHOD
                  (or (method-em-cache key)
                      (setf (method-em-cache key) (cons nil nil))))))
        (if (and (null (cdr methods)) all-applicable-p ; the most common case
                 (null method-alist-p) wrappers-p (not function-p))
            (or (car cache)
                (setf (car cache)
                      (get-secondary-dispatch-function2
                       gf methods types method-alist-p wrappers-p
                       all-applicable-p all-sorted-p function-p)))
            (let ((akey (list methods
                              (if all-applicable-p 'all-applicable types)
                              method-alist-p wrappers-p function-p)))
              (or (cdr (assoc akey (cdr cache) :test #'equal))
                  (let ((value (get-secondary-dispatch-function2
                                gf methods types method-alist-p wrappers-p
                                all-applicable-p all-sorted-p function-p)))
                    (push (cons akey value) (cdr cache))
                    value)))))))

(defun get-secondary-dispatch-function2 (gf methods types method-alist-p
                                         wrappers-p all-applicable-p
                                         all-sorted-p function-p)
  (flet ((maybe-wrap (effective)
           (if (gf-requires-emf-keyword-checks gf)
               (multiple-value-bind (valid-keys keyargs-start)
                   (compute-applicable-keywords gf methods)
                 (wrap-with-applicable-keyword-check effective valid-keys keyargs-start))
               effective)))
    (cond
      ((not (and all-applicable-p all-sorted-p (not function-p)))
       (let ((net (generate-discrimination-net
                   gf methods types all-sorted-p)))
         (compute-secondary-dispatch-function1 gf net function-p)))
      ((eq **boot-state** 'complete)
       (let ((combin (generic-function-method-combination gf)))
         (if (null (compute-primary-methods gf combin methods))
             (lambda (method-alist wrappers)
               (declare (ignore method-alist wrappers))
               (lambda (&rest args)
                 (call-no-primary-method gf args)))
             (let ((effective (maybe-wrap (compute-effective-method gf combin methods))))
               (make-effective-method-function1
                gf effective method-alist-p wrappers-p)))))
      ((eq (generic-function-name gf) 'make-specializer-form-using-class)
       ;; FIXME: instead of the above form, this should be
       ;; (eq (generic-function-method-combination gf) *or-method-combination*)
       ;; but that does not work for reasons I (JM) do not understand.
       (let* ((combin (generic-function-method-combination gf))
              (effective (maybe-wrap (short-compute-effective-method gf combin methods))))
         (make-effective-method-function1
          gf effective method-alist-p wrappers-p)))
      (t
       (let ((effective (maybe-wrap (standard-compute-effective-method gf nil methods))))
         (make-effective-method-function1
          gf effective method-alist-p wrappers-p))))))

(defun get-effective-method-function (gf methods
                                         &optional method-alist wrappers)
  (let ((generator
         (get-secondary-dispatch-function1
          gf methods nil (not (null method-alist)) (not (null wrappers)) t)))
    (make-callable generator method-alist wrappers)))

(defun get-effective-method-function1 (gf methods &optional (sorted-p t))
  (get-secondary-dispatch-function1 gf methods nil nil nil t sorted-p))

(defun methods-contain-eql-specializer-p (methods)
  (and (eq **boot-state** 'complete)
       (dolist (method methods nil)
         (when (dolist (spec (method-specializers method) nil)
                 (when (eql-specializer-p spec) (return t)))
           (return t)))))

(defun update-dfun (generic-function &optional dfun cache info)
  (let ((early-p (early-gf-p generic-function)))
    (flet ((update ()
             ;; If GENERIC-FUNCTION has a CALL-NEXT-METHOD argument
             ;; checker, the methods of the checker (the checker is a
             ;; generic function, each method caches a computation for
             ;; a combination of original and C-N-M argument classes)
             ;; must be re-computed.
             (when (eq **boot-state** 'complete)
               (let ((checker (gf-info-cnm-checker (gf-arg-info generic-function))))
                 (when checker
                   (remove-methods checker))))
             ;; Save DFUN-STATE, so that COMPUTE-DISCRIMINATING-FUNCTION can
             ;; access it, and so that it's there for eg. future cache updates.
             (set-dfun generic-function dfun cache info)
             (let ((dfun (if early-p
                             (or dfun (make-initial-dfun generic-function))
                             (compute-discriminating-function generic-function))))
               (set-funcallable-instance-function generic-function dfun)
               dfun)))
      ;; This needs to be atomic per generic function, consider:
      ;;   1. T1 sets dfun-state to S1 and computes discr. fun using S1
      ;;   2. T2 sets dfun-state to S2 and computes discr. fun using S2
      ;;   3. T2 sets fin
      ;;   4. T1 sets fin
      ;; Oops: now dfun-state and fin don't match! Since just calling
      ;; a generic can cause the dispatch function to be updated we
      ;; need a lock here.
      ;;
      ;; We need to accept recursion, because PCL is nasty and twisty,
      ;; and we need to disable interrupts because it would be bad if
      ;; we updated the DFUN-STATE but not the dispatch function.
      ;;
      ;; This is sufficient, because all the other calls to SET-DFUN
      ;; are part of this same code path (done while the lock is held),
      ;; which we AVER.
      ;;
      ;; KLUDGE: No need to lock during bootstrap.
      (if early-p
          (update)
          (let ((lock (gf-lock generic-function)))
            ;; FIXME: GF-LOCK is a generic function... Are there cases
            ;; where we can end up in a metacircular loop here? In
            ;; case there are, better fetch it while interrupts are
            ;; still enabled...
            (sb-thread::call-with-recursive-system-lock #'update lock))))))

;;; These functions aren't used in SBCL, or documented anywhere that
;;; I'm aware of, but they look like they might be useful for
;;; debugging or performance tweaking or something, so I've just
;;; commented them out instead of deleting them. -- WHN 2001-03-28
#||
(defvar *dfun-count* nil)
(defvar *dfun-list* nil)
(defvar *minimum-cache-size-to-list*)

(defun list-dfun (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
         (a (assq sym *dfun-list*)))
    (unless a
      (push (setq a (list sym)) *dfun-list*))
    (push (generic-function-name gf) (cdr a))))

(defun list-all-dfuns ()
  (setq *dfun-list* nil)
  (map-all-generic-functions #'list-dfun)
  *dfun-list*)

(defun list-large-cache (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
         (cache (gf-dfun-cache gf)))
    (when cache
      (let ((size (cache-size cache)))
        (when (>= size *minimum-cache-size-to-list*)
          (let ((a (assoc size *dfun-list*)))
            (unless a
              (push (setq a (list size)) *dfun-list*))
            (push (let ((name (generic-function-name gf)))
                    (if (eq sym 'caching) name (list name sym)))
                  (cdr a))))))))

(defun list-large-caches (&optional (*minimum-cache-size-to-list* 130))
  (setq *dfun-list* nil)
  (map-all-generic-functions #'list-large-cache)
  (setq *dfun-list* (sort *dfun-list* #'< :key #'car))
  (mapc #'print *dfun-list*)
  (values))

(defun count-dfun (gf)
  (let* ((sym (type-of (gf-dfun-info gf)))
         (cache (gf-dfun-cache gf))
         (a (assq sym *dfun-count*)))
    (unless a
      (push (setq a (list sym 0 nil)) *dfun-count*))
    (incf (cadr a))
    (when cache
      (let* ((size (cache-size cache))
             (b (assoc size (third a))))
        (unless b
          (push (setq b (cons size 0)) (third a)))
        (incf (cdr b))))))

(defun count-all-dfuns ()
  (setq *dfun-count* (mapcar (lambda (type) (list type 0 nil))
                             '(ONE-CLASS TWO-CLASS DEFAULT-METHOD-ONLY
                               ONE-INDEX N-N CHECKING CACHING
                               DISPATCH)))
  (map-all-generic-functions #'count-dfun)
  (mapc (lambda (type+count+sizes)
          (setf (third type+count+sizes)
                (sort (third type+count+sizes) #'< :key #'car)))
        *dfun-count*)
  (mapc (lambda (type+count+sizes)
          (format t "~&There are ~W dfuns of type ~S."
                  (cadr type+count+sizes) (car type+count+sizes))
          (format t "~%   ~S~%" (caddr type+count+sizes)))
        *dfun-count*)
  (values))
||#

(defun gfs-of-type (type)
  (unless (consp type) (setq type (list type)))
  (let ((gf-list nil))
    (map-all-generic-functions (lambda (gf)
                                 (when (memq (type-of (gf-dfun-info gf))
                                             type)
                                   (push gf gf-list))))
    gf-list))
