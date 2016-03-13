(in-package "SB!IMPL")

;;;; COMPARE-AND-SWAP
;;;;
;;;; SB-EXT:COMPARE-AND-SWAP is the public API for now.
;;;;
;;;; Internally our interface has CAS, GET-CAS-EXPANSION, DEFINE-CAS-EXPANDER,
;;;; DEFCAS, and #'(CAS ...) functions -- making things mostly isomorphic with
;;;; SETF.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun expand-structure-slot-cas (info name place)
  (let* ((dd (car info))
         (structure (dd-name dd))
         (slotd (cdr info))
         (index (dsd-index slotd))
         (type (dsd-type slotd))
         (casser
          (case (dsd-raw-type slotd)
            ((t) '%instance-cas)
            #!+(or x86 x86-64)
            ((word) '%raw-instance-cas/word))))
    (unless casser
      (error "Cannot use COMPARE-AND-SWAP with structure accessor ~
                for a typed slot: ~S"
             place))
    (when (dsd-read-only slotd)
      (error "Cannot use COMPARE-AND-SWAP with structure accessor ~
                for a read-only slot: ~S"
             place))
    (destructuring-bind (op arg) place
      (aver (eq op name))
      (with-unique-names (instance old new)
        (values (list instance)
                (list `(the ,structure ,arg))
                old
                new
                `(truly-the (values ,type &optional)
                            (,casser ,instance ,index
                             (the ,type ,old)
                             (the ,type ,new)))
                `(,op ,instance))))))

(defun get-cas-expansion (place &optional environment)
  #!+sb-doc
  "Analogous to GET-SETF-EXPANSION. Returns the following six values:

 * list of temporary variables

 * list of value-forms whose results those variable must be bound

 * temporary variable for the old value of PLACE

 * temporary variable for the new value of PLACE

 * form using the aforementioned temporaries which performs the
   compare-and-swap operation on PLACE

 * form using the aforementioned temporaries with which to perform a volatile
   read of PLACE

Example:

  (get-cas-expansion '(car x))
  ; => (#:CONS871), (X), #:OLD872, #:NEW873,
  ;    (SB-KERNEL:%COMPARE-AND-SWAP-CAR #:CONS871 #:OLD872 :NEW873).
  ;    (CAR #:CONS871)

  (defmacro my-atomic-incf (place &optional (delta 1) &environment env)
    (multiple-value-bind (vars vals old new cas-form read-form)
        (get-cas-expansion place env)
     (let ((delta-value (gensym \"DELTA\")))
       `(let* (,@(mapcar 'list vars vals)
               (,old ,read-form)
               (,delta-value ,delta)
               (,new (+ ,old ,delta-value)))
          (loop until (eq ,old (setf ,old ,cas-form))
                do (setf ,new (+ ,old ,delta-value)))
          ,new))))

EXPERIMENTAL: Interface subject to change."
  ;; FIXME: this seems wrong on two points:
  ;; 1. if TRULY-THE had a CAS expander (which it doesn't) we'd want
  ;;    to use %MACROEXPAND[-1] so as not to lose the "truly-the"-ness
  ;; 2. if both a CAS expander and a macro exist, the CAS expander
  ;;    should be preferred before macroexpanding (just like SETF does)
    (let ((expanded (sb!xc:macroexpand place environment)))
      (flet ((invalid-place ()
           (error "Invalid place to CAS: ~S -> ~S" place expanded)))
      (unless (consp expanded)
        ;; FIXME: Allow (CAS *FOO* <OLD> <NEW>), maybe?
        (invalid-place))
      (let ((name (car expanded)))
        (unless (symbolp name)
          (invalid-place))
        (acond
            ((info :cas :expander name)
            ;; CAS expander.
             (funcall it expanded environment))

            ;; Structure accessor
            ((structure-instance-accessor-p name)
             (expand-structure-slot-cas it name expanded))

            ;; CAS function
            (t
             (with-unique-names (old new)
               (let ((vars nil)
                     (vals nil)
                     (args nil))
                 (dolist (x (reverse (cdr expanded)))
                   (cond ((sb!xc:constantp x environment)
                          (push x args))
                         (t
                          (let ((tmp (gensymify x)))
                            (push tmp args)
                            (push tmp vars)
                            (push x vals)))))
                 (values vars vals old new
                         `(funcall #'(cas ,name) ,old ,new ,@args)
                         `(,name ,@args))))))))))
)

;;; This is what it all comes down to.
(defmacro cas (place old new &environment env)
  #!+sb-doc
  "Synonym for COMPARE-AND-SWAP.

Additionally DEFUN, DEFGENERIC, DEFMETHOD, FLET, and LABELS can be also used to
define CAS-functions analogously to SETF-functions:

  (defvar *foo* nil)

  (defun (cas foo) (old new)
    (cas (symbol-value '*foo*) old new))

First argument of a CAS function is the expected old value, and the second
argument of is the new value. Note that the system provides no automatic
atomicity for CAS functions, nor can it verify that they are atomic: it is up
to the implementor of a CAS function to ensure its atomicity.

EXPERIMENTAL: Interface subject to change."
  (multiple-value-bind (temps place-args old-temp new-temp cas-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar #'list temps place-args)
            (,old-temp ,old)
            (,new-temp ,new))
       ,cas-form)))

(defmacro define-cas-expander (accessor lambda-list &body body)
  #!+sb-doc
  "Analogous to DEFINE-SETF-EXPANDER. Defines a CAS-expansion for ACCESSOR.
BODY must return six values as specified in GET-CAS-EXPANSION.

Note that the system provides no automatic atomicity for CAS expansion, nor
can it verify that they are atomic: it is up to the implementor of a CAS
expansion to ensure its atomicity.

EXPERIMENTAL: Interface subject to change."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (info :cas :expander ',accessor)
           ,(make-macro-lambda `(cas-expand ,accessor) lambda-list body
                               'define-cas-expander accessor))))

;; FIXME: this interface is bogus - short-form DEFSETF/CAS does not
;; want a lambda-list. You just blindly substitute
;;  (CAS (PLACE arg1 ... argN) old new) -> (F arg1 ... argN old new).
;; What role can this lambda-list have when there is no user-provided
;; code to read the variables?
;; And as mentioned no sbcl-devel, &REST is beyond bogus, it's broken.
;;
(defmacro defcas (accessor lambda-list function &optional docstring)
  #!+sb-doc
  "Analogous to short-form DEFSETF. Defines FUNCTION as responsible
for compare-and-swap on places accessed using ACCESSOR. LAMBDA-LIST
must correspond to the lambda-list of the accessor.

Note that the system provides no automatic atomicity for CAS expansions
resulting from DEFCAS, nor can it verify that they are atomic: it is up to the
user of DEFCAS to ensure that the function specified is atomic.

EXPERIMENTAL: Interface subject to change."
  (multiple-value-bind (llks reqs opts rest)
      (parse-lambda-list lambda-list
                         :accept (lambda-list-keyword-mask '(&optional &rest))
                         :context "a DEFCAS lambda-list")
    (declare (ignore llks))
    `(define-cas-expander ,accessor ,lambda-list
       ,@(when docstring (list docstring))
       ;; FIXME: if a &REST arg is present, this is really weird.
       (let ((temps (mapcar #'gensymify ',(append reqs opts rest)))
             (args (list ,@(append reqs opts rest)))
             (old (gensym "OLD"))
             (new (gensym "NEW")))
         (values temps
                 args
                 old
                 new
                 `(,',function ,@temps ,old ,new)
                 `(,',accessor ,@temps))))))

(defmacro compare-and-swap (place old new)
  #!+sb-doc
  "Atomically stores NEW in PLACE if OLD matches the current value of PLACE.
Two values are considered to match if they are EQ. Returns the previous value
of PLACE: if the returned value is EQ to OLD, the swap was carried out.

PLACE must be an CAS-able place. Built-in CAS-able places are accessor forms
whose CAR is one of the following:

 CAR, CDR, FIRST, REST, SVREF, SYMBOL-PLIST, SYMBOL-VALUE, SVREF, SLOT-VALUE
 SB-MOP:STANDARD-INSTANCE-ACCESS, SB-MOP:FUNCALLABLE-STANDARD-INSTANCE-ACCESS,

or the name of a DEFSTRUCT created accessor for a slot whose declared type is
either FIXNUM or T. Results are unspecified if the slot has a declared type
other than FIXNUM or T.

In case of SLOT-VALUE, if the slot is unbound, SLOT-UNBOUND is called unless
OLD is EQ to SB-PCL:+SLOT-UNBOUND+ in which case SB-PCL:+SLOT-UNBOUND+ is
returned and NEW is assigned to the slot. Additionally, the results are
unspecified if there is an applicable method on either
SB-MOP:SLOT-VALUE-USING-CLASS, (SETF SB-MOP:SLOT-VALUE-USING-CLASS), or
SB-MOP:SLOT-BOUNDP-USING-CLASS.

Additionally, the PLACE can be a anything for which a CAS-expansion has been
specified using DEFCAS, DEFINE-CAS-EXPANDER, or for which a CAS-function has
been defined. (See SB-EXT:CAS for more information.)
"
  `(cas ,place ,old ,new))

(define-cas-expander symbol-value (name &environment env)
  (multiple-value-bind (tmp val cname)
      (if (sb!xc:constantp name env)
          (values nil nil (constant-form-value name env))
          (values (gensymify name) name nil))
    (let ((symbol (or tmp `',cname)))
      (with-unique-names (old new)
        (values (when tmp (list tmp))
                (when val (list val))
                old
                new
                (let ((slow
                        `(progn
                           (about-to-modify-symbol-value ,symbol 'compare-and-swap ,new)
                           (%compare-and-swap-symbol-value ,symbol ,old ,new))))
                  (if cname
                      (if (member (info :variable :kind cname) '(:special :global))
                          ;; We can generate the type-check reasonably.
                          `(%compare-and-swap-symbol-value
                            ',cname ,old (the ,(info :variable :type cname) ,new))
                          slow)
                      slow))
                `(symbol-value ,symbol))))))

(define-cas-expander svref (vector index)
  (with-unique-names (v i old new)
    (values (list v i)
            (list vector index)
            old
            new
            `(locally (declare (simple-vector ,v))
               (%compare-and-swap-svref ,v (check-bound ,v (length ,v) ,i) ,old ,new))
            `(svref ,v ,i))))

;;;; ATOMIC-INCF and ATOMIC-DECF

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun expand-atomic-frob
    (name specified-place diff env
          &aux (place (sb!xc:macroexpand specified-place env)))
       (declare (type (member atomic-incf atomic-decf) name))
  (flet ((invalid-place ()
           (error "Invalid first argument to ~S: ~S" name specified-place))
         (compute-newval (old) ; used only if no atomic inc vop
           `(logand (,(case name (atomic-incf '+) (atomic-decf '-)) ,old
                     (the sb!vm:signed-word ,diff)) sb!ext:most-positive-word))
         (compute-delta () ; used only with atomic inc vop
           `(logand ,(case name
                       (atomic-incf `(the sb!vm:signed-word ,diff))
                       (atomic-decf `(- (the sb!vm:signed-word ,diff))))
                    sb!ext:most-positive-word)))
    (declare (ignorable #'compute-newval #'compute-delta))
    (when (and (symbolp place)
               (eq (info :variable :kind place) :global)
               (type= (info :variable :type place) (specifier-type 'fixnum)))
      ;; Global can't be lexically rebound.
      (return-from expand-atomic-frob
        `(truly-the fixnum (,(case name
                               (atomic-incf '%atomic-inc-symbol-global-value)
                               (atomic-decf '%atomic-dec-symbol-global-value))
                            ',place (the fixnum ,diff)))))
    (unless (consp place) (invalid-place))
    (destructuring-bind (op . args) place
      ;; FIXME: The lexical environment should not be disregarded.
      ;; CL builtins can't be lexically rebound, but structure accessors can.
      (case op
        (aref
         (unless (singleton-p (cdr args))
           (invalid-place))
         (with-unique-names (array)
           `(let ((,array (the (simple-array word (*)) ,(car args))))
              #!+compare-and-swap-vops
              (%array-atomic-incf/word
               ,array
               (check-bound ,array (array-dimension ,array 0) ,(cadr args))
               ,(compute-delta))
              #!-compare-and-swap-vops
              ,(with-unique-names (index old-value)
                `(without-interrupts
                  (let* ((,index ,(cadr args))
                         (,old-value (aref ,array ,index)))
                    (setf (aref ,array ,index) ,(compute-newval old-value))
                    ,old-value))))))
        ((car cdr first rest)
         (when (cdr args)
           (invalid-place))
         `(truly-the
           fixnum
           (,(case op
              ((first car) (case name
                            (atomic-incf '%atomic-inc-car)
                            (atomic-decf '%atomic-dec-car)))
              ((rest cdr)  (case name
                            (atomic-incf '%atomic-inc-cdr)
                            (atomic-decf '%atomic-dec-cdr))))
             ,(car args) (the fixnum ,diff))))
        (t
         (when (or (cdr args)
         ;; Because accessor info is identical for the writer and reader
         ;; functions, without a SYMBOLP check this would erroneously allow
         ;;   (ATOMIC-INCF ((SETF STRUCT-SLOT) x))
                   (not (symbolp op))
                   (not (structure-instance-accessor-p op)))
             (invalid-place))
         (let* ((accessor-info (structure-instance-accessor-p op))
                (slotd (cdr accessor-info))
                (type (dsd-type slotd)))
           (unless (and (eq 'sb!vm:word (dsd-raw-type slotd))
                        (type= (specifier-type type) (specifier-type 'sb!vm:word)))
             (error "~S requires a slot of type (UNSIGNED-BYTE ~S), not ~S: ~S"
                    name sb!vm:n-word-bits type place))
           (when (dsd-read-only slotd)
             (error "Cannot use ~S with structure accessor for a read-only slot: ~S"
                    name place))
           #!+compare-and-swap-vops
           `(truly-the sb!vm:word
             (%raw-instance-atomic-incf/word
              (the ,(dd-name (car accessor-info)) ,@args)
              ,(dsd-index slotd)
              ,(compute-delta)))
           #!-compare-and-swap-vops
           (with-unique-names (structure old-value)
             `(without-interrupts
               (let* ((,structure ,@args)
                      (,old-value (,op ,structure)))
                 (setf (,op ,structure) ,(compute-newval old-value))
                 ,old-value))))))))))

(defmacro atomic-incf (&environment env place &optional (diff 1))
  #!+sb-doc
  #.(format nil
  "Atomically increments PLACE by DIFF, and returns the value of PLACE before
the increment.

PLACE must access one of the following:
 - a DEFSTRUCT slot with declared type (UNSIGNED-BYTE ~D~:*)
   or AREF of a (SIMPLE-ARRAY (UNSIGNED-BYTE ~D~:*) (*))
   The type SB-EXT:WORD can be used for these purposes.
 - CAR or CDR (respectively FIRST or REST) of a CONS.
 - a variable defined using DEFGLOBAL with a proclaimed type of FIXNUM.
Macroexpansion is performed on PLACE before expanding ATOMIC-INCF.

Incrementing is done using modular arithmetic,
which is well-defined over two different domains:
 - For structures and arrays, the operation accepts and produces
   an (UNSIGNED-BYTE ~D~:*), and DIFF must be of type (SIGNED-BYTE ~D).
   ATOMIC-INCF of #x~x by one results in #x0 being stored in PLACE.
 - For other places, the domain is FIXNUM, and DIFF must be a FIXNUM.
   ATOMIC-INCF of #x~x by one results in #x~x
   being stored in PLACE.

DIFF defaults to 1.

EXPERIMENTAL: Interface subject to change."
  sb!vm:n-word-bits most-positive-word
  sb!xc:most-positive-fixnum sb!xc:most-negative-fixnum)
  (expand-atomic-frob 'atomic-incf place diff env))

(defmacro atomic-decf (&environment env place &optional (diff 1))
  #!+sb-doc
  #.(format nil
  "Atomically decrements PLACE by DIFF, and returns the value of PLACE before
the decrement.

PLACE must access one of the following:
 - a DEFSTRUCT slot with declared type (UNSIGNED-BYTE ~D~:*)
   or AREF of a (SIMPLE-ARRAY (UNSIGNED-BYTE ~D~:*) (*))
   The type SB-EXT:WORD can be used for these purposes.
 - CAR or CDR (respectively FIRST or REST) of a CONS.
 - a variable defined using DEFGLOBAL with a proclaimed type of FIXNUM.
Macroexpansion is performed on PLACE before expanding ATOMIC-DECF.

Decrementing is done using modular arithmetic,
which is well-defined over two different domains:
 - For structures and arrays, the operation accepts and produces
   an (UNSIGNED-BYTE ~D~:*), and DIFF must be of type (SIGNED-BYTE ~D).
   ATOMIC-DECF of #x0 by one results in #x~x being stored in PLACE.
 - For other places, the domain is FIXNUM, and DIFF must be a FIXNUM.
   ATOMIC-DECF of #x~x by one results in #x~x
   being stored in PLACE.

DIFF defaults to 1.

EXPERIMENTAL: Interface subject to change."
  sb!vm:n-word-bits most-positive-word
  sb!xc:most-negative-fixnum sb!xc:most-positive-fixnum)
  (expand-atomic-frob 'atomic-decf place diff env))

;; Interpreter stubs for ATOMIC-INCF.
#!+(and compare-and-swap-vops (host-feature sb-xc))
(progn
  ;; argument types are declared in vm-fndb
  (defun %array-atomic-incf/word (array index diff)
    (%array-atomic-incf/word array index diff))
  (defun %raw-instance-atomic-incf/word (instance index diff)
    (%raw-instance-atomic-incf/word instance index diff)))
