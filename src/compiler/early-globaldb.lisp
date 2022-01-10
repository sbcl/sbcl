;;;; This file contains stuff that was split out from 'globaldb.lisp'
;;;; to satisfy build-order constraints.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Similar to FUNCTION, but the result type is "exactly" specified:
;;; if it is an object type, then the function returns exactly one
;;; value, if it is a short form of VALUES, then this short form
;;; specifies the exact number of values.
(def!type sfunction (args &optional result)
  (let ((result (cond ((eq result '*) '*)
                      ((or (atom result)
                           (not (eq (car result) 'values)))
                       `(values ,result &optional))
                      ((intersection (cdr result) lambda-list-keywords)
                       result)
                      (t `(values ,@(cdr result) &optional)))))
    `(function ,args ,result)))

(declaim (ftype (sfunction (t t t) (values t t)) info)
         (ftype (sfunction (t t t) t) clear-info)
         (ftype (sfunction (t t t t) t) (setf info)))

;;; (:FUNCTION :TYPE) information is extracted through a wrapper.
;;; The globaldb representation is not necessarily literally a CTYPE.
#-sb-xc-host
(declaim (ftype (sfunction (t) ctype) global-ftype))

;;; A bit about the physical representation of the packed info format:
;;; With #+compact-instance-header it is possible to represent a vector of N things
;;; in a structure using (ALIGN-UP (1+ N) 2) words of memory. This is a saving
;;; of 1 word on average when compared to SIMPLE-VECTOR which needs
;;; (ALIGN-UP (+ N 2) 2) words.  Granted that either might have a padding word,
;;; but I've observed 5% to 10% space reduction by eliminating one slot.
;;; Without compact-instance-header, we'e indifferent, in terms of space,
;;; as to whether this is an INSTANCE or a SIMPLE-VECTOR. For consistency,
;;; we use an INSTANCE regardless of presence of the compact-header feature.
;;; This makes assembly routines (e.g. CALL-SYMBOL) slightly less sensitive
;;; to the feature's absence.

;;; Since variable-length instances aren't portably a thing,
;;; we use a structure of one slot holding a vector.
#+sb-xc-host (defstruct (packed-info
                          (:constructor %make-packed-info (cells))
                          (:copier nil))
               ;; These objects are immutable.
               (cells #() :type simple-vector :read-only t))
;;; Some abstractions for host/target compatibility of all the defuns.
#+sb-xc-host
(progn
  (defmacro make-packed-info (n) `(%make-packed-info (make-array ,n)))
  (defmacro copy-packed-info (info)
    `(%make-packed-info (copy-seq (packed-info-cells ,info))))
  (defmacro packed-info-len (info)
    `(length (packed-info-cells ,info)))
  (defmacro  %info-ref (info index) `(svref (packed-info-cells ,info) ,index)))
#-sb-xc-host
(progn
  #+nil
  (defmethod print-object ((obj packed-info) stream)
    (format stream "[~{~W~^ ~}]"
            (loop for i below (%instance-length obj)
               collect (%instance-ref obj i))))
  (eval-when (:compile-toplevel)
    (sb-xc:defmacro make-packed-info (n)
      ;; this file is earlier than early-vm, so we have to take
      ;; INSTANCE-DATA-START from the value set in make-host-1.
      `(let ((new (%make-instance (+ ,n #.sb-vm:instance-data-start))))
         (setf (%instance-wrapper new) #.(find-layout 'packed-info))
         new))
    ;; We can't merely call COPY-STRUCTURE due to two issues:
    ;; 1. bootstrapping- the DEFSTRUCT-DESCRIPTION is not available
    ;;    in cold-init by the first call to COPY-PACKED-INFO,
    ;;    but COPY-STRUCTURE needs it, because of raw slots.
    ;; 2. the transform for COPY-STRUCTURE would think that
    ;;    it needs to copy exactly 1 slot. Well, it would think that,
    ;;    if the FREEZE-TYPE wasn't commented out.
    (sb-xc:defmacro copy-packed-info (info)
      ;; not bothering with ONCE-ONLY here (doesn't matter)
      `(%copy-instance (%make-instance (%instance-length ,info)) ,info)))
  (defmacro packed-info-len (info)
      `(- (%instance-length ,info) #.sb-vm:instance-data-start))
  (defmacro %info-ref (v i)
    `(%instance-ref ,v (+ ,i #.sb-vm:instance-data-start)))
  (defmethod print-object ((self packed-info) stream)
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "len=~d" (packed-info-len self)))))

;;; At run time, we represent the type of a piece of INFO in the globaldb
;;; by a small integer between 1 and 63.  [0 is reserved for internal use.]
;;; CLISP, and maybe others, need EVAL-WHEN because without it, the constant
;;; is not seen by the "#." expression a few lines down.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant info-number-bits 6))
(deftype info-number () `(unsigned-byte ,info-number-bits))

;;; A map from info-number to its META-INFO object.
;;; The reverse mapping is obtained by reading the META-INFO.
(declaim (type (simple-vector #.(ash 1 info-number-bits)) *info-types*))
(define-load-time-global *info-types* (make-array (ash 1 info-number-bits) :initial-element nil))

(defstruct (meta-info
            (:constructor
             !make-meta-info (number category kind type-spec
                              type-checker validate-function default))
            (:copier nil))
  ;; a number that uniquely identifies this object
  (number nil :type info-number :read-only t)
  ;; 2-part key to this piece of metainfo
  (category nil :type keyword :read-only t)
  (kind nil :type keyword :read-only t)
  ;; a type specifier which info of this type must satisfy
  (type-spec nil :type t :read-only t)
  ;; Two functions called by (SETF INFO) before calling SET-INFO-VALUE.
  ;; 1. A function that type-checks its argument and returns it,
  ;;    or signals an error.
  ;;    Some Lisps trip over their shoelaces trying to assert that
  ;;    a function is (function (t) t). Our code is fine though.
  (type-checker nil :type #+sb-xc-host function #-sb-xc-host (sfunction (t) t)
                :read-only t)
  ;; 2. a function of two arguments, a name and new-value, which performs
  ;;    any other checks and/or side-effects including signaling an error.
  (validate-function nil :type (or function null) :read-only t)
  ;; If FUNCTIONP, then a function called when there is no information of
  ;; this type. If not FUNCTIONP, then any object serving as a default.
  (default nil :read-only t))

(declaim (freeze-type meta-info))

(defconstant +info-metainfo-type-num+ 0)

;; Refer to info-vector.lisp for the meaning of this constant.
(defconstant +no-auxiliary-key+ 0)

;;; Return the globaldb info for SYMBOL. With respect to the state diagram
;;; presented at the definition of SYMBOL-PLIST, if the object in SYMBOL's
;;; info slot is LISTP, it is in state 1 or 3. Either way, take the CDR.
;;; Otherwise, it is in state 2 so return the value as-is.
;;; NIL is an acceptable substitute for +NIL-PACKED-INFOS+,
;;; but I might change that.
;;;
;;; Define SYMBOL-INFO as an inline function unless a vop translates it.
;;; (Inlining occurs first, which would cause the vop not to be used.)
#-sb-xc-host
(sb-c::unless-vop-existsp (:translate sb-kernel:symbol-dbinfo)
  (declaim (inline symbol-dbinfo))
  (defun symbol-dbinfo (symbol)
    (let ((info-holder (symbol-%info symbol)))
      (truly-the (or null instance)
                 (if (listp info-holder) (cdr info-holder) info-holder)))))

;; Perform the equivalent of (GET-INFO-VALUE KIND +INFO-METAINFO-TYPE-NUM+)
;; but skipping the defaulting logic.
;; Return zero or more META-INFOs that match on KIND, which is usually
;; - though not always - a unique identifier for the (:TYPE :KIND) pair.
;; Note that bypassing of defaults is critical for bootstrapping,
;; since INFO is used to retrieve its own META-INFO at system-build time.
(defmacro get-meta-infos (kind)
  `(let* ((packed-info (symbol-dbinfo ,kind))
          (index (if packed-info
                     (packed-info-value-index packed-info +no-auxiliary-key+
                                              +info-metainfo-type-num+))))
     (if index (%info-ref packed-info index))))

;; (UNSIGNED-BYTE 11) is an arbitrarily generous limit on the number of
;; cells in a packed-info. Most packed-infos have fewer than a handful of things,
;; and performance would need to be re-thought if more than about a dozen
;; cells were in use. (It would want to become hash-based probably)
;; It has to be smaller than INSTANCE_LENGTH_MASK certainly,
;; plus leaving room for a layout slot if #-compact-instance-header.
(declaim (ftype (function (packed-info (or (eql 0) symbol) info-number)
                          (or null (unsigned-byte 11)))
                packed-info-value-index))

;; Return the META-INFO object for CATEGORY and KIND, signaling an error
;; if not found and ERRORP is non-nil. Note that the two-level logical hierarchy
;; of CATEGORY + KIND is physically grouped by KIND first, then CATEGORY.
;; e.g. Searching for (:SETF :EXPANDER) searches for :EXPANDER and finds
;;   (#<:CAS :EXPANDER, 44> #<:SETF :EXPANDER, 43> #<:TYPE :EXPANDER, 25>)
;; from which one is picked. This is slightly faster than searching first by
;; CATEGORY, because in the case of :FUNCTION there would be ~11 things to sift
;; through, whereas typically no more than 3 or 4 items have the same KIND.
;;
(defun meta-info (category kind &optional (errorp t))
  (or (let ((metadata (get-meta-infos kind)))
        (cond ((listp metadata) ; conveniently handles NIL
               (dolist (info metadata nil) ; FIND is slower :-(
                 (when (eq (meta-info-category (truly-the meta-info info))
                           category)
                   (return info))))
              ((eq (meta-info-category (truly-the meta-info metadata)) category)
               metadata)))
      ;; GET-META-INFOS enforces that KIND is a symbol, therefore
      ;; if a metaobject was found, CATEGORY was necessarily a symbol too.
      ;; Otherwise, if the caller wants no error to be signaled on missing info,
      ;; we must nevertheless enforce that CATEGORY was actually a symbol.
      (if errorp
          (error "(~S ~S) is not a defined info type." category kind)
          (progn (the symbol category) nil)))) ; THE is for type-safety

;;; Compiler macros for INFO functions.
;;;
;;; These are defined ASAP so that when building the cross-compiler, all calls
;;; occurring after compilation of "globaldb" (for known constant meta-info)
;;; are transformed; and when executing the cross-compiler, *all* inlineable
;;; calls for known constant meta-info are transformed;
;;; and when running target code, calls with legal constants for the first two
;;; arguments are transformed.
(macrolet ((def (name lambda-list form)
             (assert (and (member 'category lambda-list)
                          (member 'kind lambda-list)))
             `(define-compiler-macro ,name ,(append '(&whole .whole.) lambda-list)
                (if (and (keywordp category) (keywordp kind))
                    ;; In the target Lisp, it's a STYLE-WARNING if this macro
                    ;; defers to a full call to #'INFO.
                    ;; If the cross-compilation host, if any info-type is
                    ;; defined, then it's an error not to find the meta-info.
                    ;; If no info-types are defined, silently defer.
                    (let ((meta-info
                           (and #+sb-xc-host (find-if #'identity *info-types*)
                                (meta-info category kind #-sb-xc-host nil))))
                      (if meta-info
                          ,form
                          (progn
                            #-sb-xc-host
                            (style-warn "(~S ~S) is not a defined info type."
                                        category kind)
                            .whole.)))
                    .whole.))))

  ;; ECL bug workaround: INFO ceases to be a valid macrolet name
  ;; because it tries to run the compiler-macro before the local macro.
  ;; In particular, "(collect ((info)) ...)" will not compile correctly.
  #-host-quirks-ecl
  (def info (category kind name)
    `(truly-the (values ,(meta-info-type-spec meta-info) boolean)
                (get-info-value ,name ,(meta-info-number meta-info))))

  (def (setf info) (new-value category kind name)
    (let* (#+sb-xc-host (sb-xc:*gensym-counter* sb-xc:*gensym-counter*)
           (tin (meta-info-number meta-info)) ; info-type id number
           (type-spec (meta-info-type-spec meta-info))
           (new (make-symbol "NEW"))
           (check
            (when (meta-info-validate-function meta-info)
              ;; is (or ... null), but non-null at macroexpansion time
              ;; implies non-null at runtime.
              `(truly-the function
                (meta-info-validate-function
                 (truly-the meta-info (svref *info-types* ,tin)))))))
      `(let ((,new ,new-value))
         ;; enforce type-correctness regardless of enclosing policy
         (let ((,new (locally (declare (optimize (safety 3)))
                       (the ,type-spec ,new))))
           ,@(when check
               `((funcall ,check ,name ,new)))
           (set-info-value ,name ,tin ,new)))))

  (def clear-info (category kind name)
    `(clear-info-values ,name '(,(meta-info-number meta-info)))))

;; interface to %ATOMIC-SET-INFO-VALUE
;; GET-INFO-VALUE-INITIALIZING is a restricted case of this,
;; and perhaps could be implemented as such.
;; Atomic update will be important for making the fasloader threadsafe
;; using a predominantly lock-free design, and other nice things.
(defmacro atomic-set-info-value (category kind name lambda)
  (with-unique-names (info-number proc)
    `(let ((,info-number
            ,(if (and (keywordp category) (keywordp kind))
                 (meta-info-number (meta-info category kind))
                 `(meta-info-number (meta-info ,category ,kind)))))
       ,(if (and (listp lambda) (eq (car lambda) 'lambda))
            ;; rewrite as FLET because the compiler is unable to dxify
            ;;   (DX-LET ((x (LAMBDA <whatever>))) (F x))
            (destructuring-bind (lambda-list . body) (cdr lambda)
              `(dx-flet ((,proc ,lambda-list ,@body))
                 (%atomic-set-info-value ,name ,info-number #',proc)))
            `(%atomic-set-info-value ,name ,info-number ,lambda)))))

;; Perform the approximate equivalent operations of retrieving
;; (INFO :CATEGORY :KIND NAME), but if no info is found, invoke CREATION-FORM
;; to produce an object that becomes the value for that piece of info, storing
;; and returning it. The entire sequence behaves atomically but with a proviso:
;; the creation form's result may be discarded, and another object returned
;; instead (presumably) from another thread's execution of the creation form.
;; If constructing the object has either non-trivial cost, or deleterious
;; side-effects from making and discarding its result, do NOT use this macro.
;; A mutex-guarded table would probably be more appropriate in such cases.
;;
(defmacro get-info-value-initializing (category kind name creation-form)
  (let ((proc (make-symbol "THUNK")))
    `(dx-flet ((,proc () ,creation-form))
       (%get-info-value-initializing
        ,(if (and (keywordp category) (keywordp kind))
             (meta-info-number (meta-info category kind))
             `(meta-info-number (meta-info ,category ,kind)))
        ,name #',proc))))

;;;; boolean attribute utilities
;;;;
;;;; We need to maintain various sets of boolean attributes for known
;;;; functions and VOPs. To save space and allow for quick set
;;;; operations, we represent the attributes as bits in a fixnum.

(in-package "SB-C")
(deftype attributes () 'fixnum)

;;; Given a list of attribute names and an alist that translates them
;;; to masks, return the OR of the masks.
(defun encode-attribute-mask (names universe)
  (loop for name in names
        for pos = (position name universe)
        sum (if pos (ash 1 pos) (error "unknown attribute name: ~S" name))))

(defun decode-attribute-mask (bits universe)
  (loop for name across universe
        for mask = 1 then (ash mask 1)
        when (logtest mask bits) collect name))

;;; Define a new class of boolean attributes, with the attributes
;;; having the specified ATTRIBUTE-NAMES. NAME is the name of the
;;; class, which is used to generate some macros to manipulate sets of
;;; the attributes:
;;;
;;;    NAME-attributep attributes attribute-name*
;;;      Return true if one of the named attributes is present, false
;;;      otherwise. When set with SETF, updates the place Attributes
;;;      setting or clearing the specified attributes.
;;;
;;;    NAME-attributes attribute-name*
;;;      Return a set of the named attributes.
(defmacro !def-boolean-attribute (name &rest attribute-names)
  (let ((vector (coerce attribute-names 'vector))
        (constructor (symbolicate name "-ATTRIBUTES"))
        (test-name (symbolicate name "-ATTRIBUTEP")))
    `(progn
       (defmacro ,constructor (&rest attribute-names)
         "Automagically generated boolean attribute creation function.
  See !DEF-BOOLEAN-ATTRIBUTE."
         (encode-attribute-mask attribute-names ,vector))
       (defun ,(symbolicate "DECODE-" name "-ATTRIBUTES") (attributes)
         (decode-attribute-mask attributes ,vector))
       (defmacro ,test-name (attributes &rest attribute-names)
         "Automagically generated boolean attribute test function.
  See !DEF-BOOLEAN-ATTRIBUTE."
         `(logtest (the attributes ,attributes)
                   (,',constructor ,@attribute-names)))
       (define-setf-expander ,test-name (place &rest attributes
                                               &environment env)
         "Automagically generated boolean attribute setter. See
 !DEF-BOOLEAN-ATTRIBUTE."
         (multiple-value-bind (temps values stores setter getter)
             (#+sb-xc-host cl:get-setf-expansion
              #-sb-xc-host get-setf-expansion place env)
           (when (cdr stores)
             (error "multiple store variables for ~S" place))
           (let ((newval (sb-xc:gensym))
                 (n-place (sb-xc:gensym))
                 (mask (encode-attribute-mask attributes ,vector)))
             (values `(,@temps ,n-place)
                     `(,@values ,getter)
                     `(,newval)
                     `(let ((,(first stores)
                             (if ,newval
                                 (logior ,n-place ,mask)
                                 (logandc2 ,n-place ,mask))))
                        ,setter
                        ,newval)
                     `(,',test-name ,n-place ,@attributes))))))))

;;; And now for some gratuitous pseudo-abstraction...
;;;
;;; ATTRIBUTES-UNION
;;;   Return the union of all the sets of boolean attributes which are its
;;;   arguments.
;;; ATTRIBUTES-INTERSECTION
;;;   Return the intersection of all the sets of boolean attributes which
;;;   are its arguments.
;;; ATTRIBUTES=
;;;   True if the attributes present in ATTR1 are identical to
;;;   those in ATTR2.
(defmacro attributes-union (&rest attributes)
  `(the attributes
        (logior ,@(mapcar (lambda (x) `(the attributes ,x)) attributes))))
(defmacro attributes-intersection (&rest attributes)
  `(the attributes
        (logand ,@(mapcar (lambda (x) `(the attributes ,x)) attributes))))
(declaim (ftype (function (attributes attributes) boolean) attributes=))
(declaim (inline attributes=))
(defun attributes= (attr1 attr2)
  (eql attr1 attr2))
