;;;; This file provides a functional interface to global information
;;;; about named things in the system. Information is considered to be
;;;; global if it must persist between invocations of the compiler. The
;;;; use of a functional interface eliminates the need for the compiler
;;;; to worry about the actual representation. This is important, since
;;;; the information may well have several representations.
;;;;
;;;; The database contains arbitrary Lisp values, addressed by a
;;;; combination of Name, Class and Type. The Name is an EQUAL-thing
;;;; which is the name of the thing we are recording information
;;;; about. Class is the kind of object involved. Typical classes are
;;;; :FUNCTION, :VARIABLE, :TYPE, ... A Type names a particular piece
;;;; of information within a given class. Class and Type are keywords,
;;;; and are compared with EQ.

;;;; The relation between this file and 'info-vectors' is that the
;;;; latter provides a fundamental mechanism to create property-list-like
;;;; things whose "indicators" are restricted to small integers
;;;; and whose values are anything; whereas the globaldb provides the
;;;; facility of looking up the properties by keyword, a/k/a Class+Type.
;;;; The keyword regime is somewhat arbitrary because ultimately the
;;;; pair of keywords just translates to a small integer, usually
;;;; resolvable at compile-time for the most part.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(!begin-collecting-cold-init-forms)
#!+sb-show (!cold-init-forms (/show0 "early in globaldb.lisp cold init"))

;;; This is sorta semantically equivalent to SXHASH, but better-behaved for
;;; legal function names. It performs more work by not cutting off as soon
;;; in the CDR direction, thereby improving the distribution of method names.
;;; More work here equates to less work in the global hashtable.
;;; To wit: (eq (sxhash '(foo a b c bar)) (sxhash '(foo a b c d))) => T
;;; but the corresponding globaldb-sxhashoids differ.
;;; This is no longer inline because for the cases where it is needed -
;;; names which are not just symbols or (SETF F) - an extra call has no impact.
(defun globaldb-sxhashoid (name)
  ;; we can't use MIX because it's in 'target-sxhash',
  ;; so use the host's sxhash, but ensure that the result is a target fixnum.
  #+sb-xc-host (logand (sxhash name) sb!xc:most-positive-fixnum)
  #-sb-xc-host
  (locally
      (declare (optimize (safety 0))) ; after the argc check
    ;; TRAVERSE will walk across more cons cells than RECURSE will descend.
    ;; That's why this isn't just one self-recursive function.
    (labels ((traverse (accumulator x length-limit)
               (declare (fixnum length-limit))
               (cond ((atom x) (sb!int:mix (sxhash x) accumulator))
                     ((zerop length-limit) accumulator)
                     (t (traverse (sb!int:mix (recurse (car x) 4) accumulator)
                                  (cdr x) (1- length-limit)))))
             (recurse (x depthoid) ; depthoid = a blend of level and length
               (declare (fixnum depthoid))
               (cond ((atom x) (sxhash x))
                     ((zerop depthoid)
                      #.(logand sb!xc:most-positive-fixnum #36Rglobaldbsxhashoid))
                     (t (sb!int:mix (recurse (car x) (1- depthoid))
                                    (recurse (cdr x) (1- depthoid)))))))
      (traverse 0 name 10))))

;;; Given any non-negative integer, return a prime number >= to it.
;;;
;;; FIXME: This logic should be shared with ALMOST-PRIMIFY in
;;; hash-table.lisp. Perhaps the merged logic should be
;;; PRIMIFY-HASH-TABLE-SIZE, implemented as a lookup table of primes
;;; after integral powers of two:
;;;    #(17 37 67 131 ..)
;;; (Or, if that's too coarse, after half-integral powers of two.) By
;;; thus getting rid of any need for primality testing at runtime, we
;;; could punt POSITIVE-PRIMEP, too.
(defun primify (x)
  (declare (type unsigned-byte x))
  (do ((n (logior x 1) (+ n 2)))
      ((positive-primep n) n)))

;;;; info classes, info types, and type numbers, part I: what's needed
;;;; not only at compile time but also at run time

;;;; Note: This section is a blast from the past, a little trip down
;;;; memory lane to revisit the weird host/target interactions of the
;;;; CMU CL build process. Because of the way that the cross-compiler
;;;; and target compiler share stuff here, if you change anything in
;;;; here, you'd be well-advised to nuke all your fasl files and
;;;; restart compilation from the very beginning of the bootstrap
;;;; process.

;;; Why do we suppress the :COMPILE-TOPLEVEL situation here when we're
;;; running the cross-compiler? The cross-compiler (which was built
;;; from these sources) has its version of these data and functions
;;; defined in the same places we'd be defining into. We're happy with
;;; its version, since it was compiled from the same sources, so
;;; there's no point in overwriting its nice compiled version of this
;;; stuff with our interpreted version. (And any time we're *not*
;;; happy with its version, perhaps because we've been editing the
;;; sources partway through bootstrapping, tch tch, overwriting its
;;; version with our version would be unlikely to help, because that
;;; would make the cross-compiler very confused.)
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

;;; a map from type numbers to TYPE-INFO objects. There is one type
;;; number for each defined CLASS/TYPE pair.
;;;
;;; We build its value at build-the-cross-compiler time (with calls to
;;; DEFINE-INFO-TYPE), then generate code to recreate the compile time
;;; value, and arrange for that code to be called in cold load.
;;; KLUDGE: We don't try to reset its value when cross-compiling the
;;; compiler, since that creates too many bootstrapping problems,
;;; instead just reusing the built-in-the-cross-compiler version,
;;; which is theoretically a little bit ugly but pretty safe in
;;; practice because the cross-compiler is as close to the target
;;; compiler as we can make it, i.e. identical in most ways, including
;;; this one. -- WHN 2001-08-19
(declaim (type (simple-vector #.(ash 1 type-number-bits)) *info-types*))
(defglobal *info-types* (make-array (ash 1 type-number-bits) :initial-element nil))

(defstruct (type-info
            #-no-ansi-print-object
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s)
                               (format s
                                       "~S ~S, Number = ~W"
                                       (type-info-class x)
                                       (type-info-name x)
                                       (type-info-number x)))))
            (:constructor
             make-globaldb-info-metadata (number class name type-spec))
            (:copier nil))
  ;; the name of this type
  (name nil :type keyword)
  ;; this type's class
  (class nil :type keyword)
  ;; a number that uniquely identifies this type (and implicitly its class)
  (number nil :type type-number)
  ;; a type specifier which info of this type must satisfy
  (type-spec nil :type t)
  ;; If FUNCTIONP, then a function called when there is no information of
  ;; this type. If not FUNCTIONP, then any object serving as a default.
  (default nil)
  ;; Two functions called by (SETF INFO) before calling SET-INFO-VALUE.
  ;; Regarding the type specifiers on these slots, I wanted to write them
  ;; as (SFUNCTION (T) T) for documentation - and it elides the check for
  ;; multiple values returned - but doing that causes failure building the
  ;; cross-compiler under CMUCL 20c because it tries to call TYPEP on that,
  ;; and complains that it can't.
  ;; 1. A function that type-checks its argument and returns it,
  ;;    or signals an error.
  (type-checker #'identity :type function)
  ;; 2. a function of two arguments, a name and new-value, which performs
  ;;    any other checks and/or side-effects including signaling an error.
  (validate-function nil :type (or function null)))
(declaim (freeze-type type-info))

(defconstant +info-metainfo-type-num+ 0)

;; Perform the equivalent of (GET-INFO-VALUE sym +INFO-METAINFO-TYPE-NUM+)
;; but without the AVER that metadata already exists, and bypassing the
;; defaulting logic.
(defun %get-type-info-metadata (sym)
  (let* ((info-vector (symbol-info-vector sym))
         (index (if info-vector
                    (packed-info-value-index info-vector +no-auxilliary-key+
                                             +info-metainfo-type-num+))))
    (if index (svref info-vector index))))

;; Find or create a TYPE-INFO object designated by CLASS- and TYPE-KEYWORD.
;; If not found, the specified TYPE-NUM and TYPE-SPEC are used to
;; initialize it. If TYPE-NUM is -1, the next available number is assigned.
;; Return the new type-num.
(defun register-info-metadata (type-num class-keyword type-keyword type-spec)
    (let ((metainfo (find-type-info class-keyword type-keyword)))
      (cond (metainfo) ; Do absolutely positively nothing.
            (t
             (when (eql type-num -1) ; pick a new type-num
               ;; The zeroth type-num is reserved for INFO's own private use.
               ;; +fdefn-type-num+ is also reserved and must be special-cased.
               ;; Generalizing DEFINE-INFO-TYPE to optionally pass a type-number
               ;; would also mean changing the fact that a specified number is
               ;; used only for restoring *INFO-TYPES* during cold-init.
               (setq type-num
                     (or (if (and (eq class-keyword :function)
                                  (eq type-keyword :definition))
                             +fdefn-type-num+
                             (position nil *info-types* :start 1))
                         (error "no more INFO type numbers available"))))
             (setf metainfo (make-globaldb-info-metadata
                             type-num class-keyword type-keyword type-spec)
                   (aref *info-types* type-num) metainfo)
             (let ((list (%get-type-info-metadata type-keyword)))
               (set-info-value
                type-keyword +info-metainfo-type-num+
                (cond ((not list) metainfo) ; unique, just store it
                      ((listp list) (cons metainfo list)) ; prepend to the list
                      (t (list metainfo list))))))) ; convert atom to a list
      (type-info-number metainfo)))

;; If CLASS-KEYWORD/TYPE-KEYWORD designate an info-type,
;; return the corresponding TYPE-INFO object, otherwise NIL.
(defun find-type-info (class-keyword type-keyword)
    (declare (type keyword class-keyword type-keyword))
    (let ((metadata (%get-type-info-metadata type-keyword)))
      ;; Most TYPE-KEYWORDs uniquely designate an object, so we store only that.
      ;; Otherwise we store a list which has a small handful of (<= 4) items.
      (cond ((listp metadata)
             ;; Can we *please* make (FIND ...) not call GENERIC+
             ;; so that I don't feel compelled to express this as a DOLIST ?
             (dolist (info metadata nil)
               (when (eq (type-info-class (truly-the type-info info))
                         class-keyword)
                 (return info))))
            ((eq (type-info-class (truly-the type-info metadata)) class-keyword)
             metadata))))

(declaim (ftype (function (keyword keyword) type-info) type-info-or-lose))
(defun type-info-or-lose (class type)
  #+sb-xc (/noshow0 "entering TYPE-INFO-OR-LOSE, CLASS,TYPE=..")
  #+sb-xc (/nohexstr class)
  #+sb-xc (/nohexstr type)
  (or (find-type-info class type)
      (error "(~S ~S) is not a defined info type." class type)))

) ; EVAL-WHEN

;;;; info types, and type numbers, part II: what's
;;;; needed only at compile time, not at run time

(eval-when (:compile-toplevel :execute)

;;; a list of forms for initializing the DEFAULT slots of TYPE-INFO
;;; objects, accumulated during compilation and eventually converted
;;; into a function to be called at cold load time after the
;;; appropriate TYPE-INFO objects have been created
;;;
;;; Note: This is quite similar to the !COLD-INIT-FORMS machinery, but
;;; we can't conveniently use the ordinary !COLD-INIT-FORMS machinery
;;; here. The problem is that the natural order in which the
;;; default-slot-initialization forms are generated relative to the
;;; order in which the TYPE-INFO-creation forms are generated doesn't
;;; match the relative order in which the forms need to be executed at
;;; cold load time.
(defparameter *!reversed-type-info-init-forms* nil)

;;; Define a new type of global information.
;;; CLASS/TYPE form a two-piece name for the kind of information,
;;; DEFAULT is a defaulting expression, and TYPE-SPEC
;;; is a type specifier which values of the type must satisfy.
;;; Roughly speaking there is a hierarchy to the two-piece names
;;; but this is a fiction that is not maintained anywhere in the internals.
;;;
;;; If the defaulting expression's value is a function, it is called with
;;; the name for which the information is being looked up; otherwise it is
;;; taken as the default value. The defaulting expression is used each time
;;; a value is needed when one hasn't been previously set. (The result
;;; does not automatically become the new value for the piece of info.)
;;; Should a default value be itself a function, this must be expressed as
;;;  :DEFAULT (CONSTANTLY #'<a-function-name>) to adhere to the convention
;;; that default objects satisfying FUNCTIONP will always be funcalled.
;;;
;;; The main thing we do is determine the type's number. We need to do
;;; this at macroexpansion time, since both the COMPILE and LOAD time
;;; calls to %DEFINE-INFO-TYPE must use the same type number.
(#+sb-xc-host defmacro
 #-sb-xc-host sb!xc:defmacro
    define-info-type ((class type)
                       &key (type-spec (missing-arg))
                            (validate-function)
                            default)
  (declare (type keyword class type))
  `(progn
     (eval-when (:compile-toplevel :execute)
       ;; At compile time, ensure that the type number exists. It will
       ;; need to be forced to exist at cold load time, too, but
       ;; that's not handled here; it's handled by later code which
       ;; looks at the compile time state and generates code to
       ;; replicate it at cold load time.
       (let ((num (register-info-metadata -1 ,class ,type ',type-spec)))
       ;; Arrange for TYPE-INFO-DEFAULT, TYPE-INFO-TYPE-CHECKER, and
       ;; TYPE-INFO-VALIDATE-FUNCTION to be set at cold load
       ;; time. (They can't very well be set at cross-compile time,
       ;; since they differ between host and target and are
       ;; host-compiled closures.)
         (push `(let ((type-info (aref *info-types* ,num)))
                  ;; cold-init can't actually AVER without crashing hard,
                  ;; but what the heck, let's do it.
                  (aver type-info)
                ,@',(unless (eq type-spec 't)
                      ;; avoid re-inventing #'IDENTITY N times over
                      `((setf (type-info-type-checker type-info)
                              (lambda (x) (declare (type ,type-spec x)) x))))
                (setf (type-info-validate-function type-info)
                      ,',validate-function
                      (type-info-default type-info) ,',default))
             *!reversed-type-info-init-forms*)))
     ',type))

) ; EVAL-WHEN


;;; INFO is the standard way to access the database. It's settable.
;;;
;;; Return the information of the specified TYPE and CLASS for NAME.
;;; The second value returned is true if there is any such information
;;; recorded. If there is no information, the first value returned is
;;; the default and the second value returned is NIL.
(defun info (class type name)
  (let ((info (type-info-or-lose class type)))
    (get-info-value name (type-info-number info))))

(defun (setf info) (new-value class type name)
  (let ((info (type-info-or-lose class type)))
    (funcall (type-info-type-checker info) new-value)
    (awhen (type-info-validate-function info)
      (funcall it name new-value))
    (set-info-value name (type-info-number info) new-value)))

;;; Clear the information of the specified TYPE and CLASS for NAME in
;;; the current environment. Return true if there was any info.
(defun clear-info (class type name)
  (let* ((info (type-info-or-lose class type))
         (type-number-list (list (type-info-number info))))
    (declare (dynamic-extent type-number-list))
    (clear-info-values name type-number-list)))

(defun clear-info-values (name type-numbers)
  (dolist (type type-numbers)
    (aver (and (typep type 'type-number) (svref *info-types* type))))
  ;; A call to UNCROSS was suspiciously absent, so I added this ERROR
  ;; to be certain that it's not supposed to happen when building the xc.
  #+sb-xc-xhost (error "Strange CLEAR-INFO building the xc: ~S ~S" name type)
  (let (new)
    (with-globaldb-name (key1 key2) name
      :simple
      ;; If PACKED-INFO-REMOVE has nothing to do, it returns NIL,
      ;; corresponding to the input that UPDATE-SYMBOL-INFO expects.
      (dx-flet ((clear-simple (old)
                  (setq new (packed-info-remove old key2 type-numbers))))
        (update-symbol-info key1 #'clear-simple))
      :hairy
      ;; The global hashtable is not imbued with knowledge of the convention
      ;; for PACKED-INFO-REMOVE because that would render it less useful
      ;; as a general-purpose global hashtable for other kinds of stuff
      ;; that I might want it to store aside from packed infos.
      ;; So here UPDATE might receive NIL but must not return NIL if
      ;; there was a non-nil input. NIL doesn't mean "do nothing".
      (dx-flet ((clear-hairy (old)
                  (if old
                      ;; if -REMOVE => nil, then update NEW but return OLD
                      (or (setq new (packed-info-remove
                                     old +no-auxilliary-key+ type-numbers))
                          old))))
        (info-puthash *info-environment* name #'clear-hairy)))
    (not (null new))))

;;;; *INFO-ENVIRONMENT*

(!cold-init-forms
  (setq *info-environment* (make-info-hashtable))
  (/show0 "done setting *INFO-ENVIRONMENT*"))

;;;; GET-INFO-VALUE

;;; Return the value of NAME / TYPE-NUMBER from the global environment,
;;; or return the default if there is no global info.
;;; The secondary value indicates whether info was found vs defaulted.
(declaim (ftype (sfunction (t type-number) (values t boolean))
                get-info-value))
(defun get-info-value (name type-number)
  ;; sanity check: If we have screwed up initialization somehow, then
  ;; *INFO-TYPES* could still be uninitialized at the time we try to
  ;; get an info value, and then we'd be out of luck. (This happened,
  ;; and was confusing to debug, when rewriting EVAL-WHEN in
  ;; sbcl-0.pre7.x.)
  (let ((metainfo (aref *info-types* type-number)))
    (aver metainfo)
    (multiple-value-bind (vector aux-key)
        (let ((name (uncross name)))
          (with-globaldb-name (key1 key2) name
            :simple (values (symbol-info-vector key1) key2)
            :hairy (values (info-gethash name *info-environment*)
                           +no-auxilliary-key+)))
      (when vector
        (let ((index
               (packed-info-value-index vector aux-key type-number)))
          (when index
            (return-from get-info-value (values (svref vector index) t))))))
    (let ((val (type-info-default metainfo)))
      (values (if (functionp val) (funcall val name) val) nil))))

;; Perform the approximate equivalent operations of retrieving
;; (INFO :CLASS :TYPE NAME), but if no info is found, invoke CREATION-FORM
;; to produce an object that becomes the value for that piece of info, storing
;; and returning it. The entire sequence behaves atomically but with a proviso:
;; the creation form's result may be discarded, and another object returned
;; instead (presumably) from another thread's execution of the creation form.
;; If constructing the object has either non-trivial cost, or deleterious
;; side-effects from making and discarding its result, do NOT use this macro.
;; A mutex-guarded table would probably be more appropriate in such cases.
;;
(def!macro get-info-value-initializing (info-class info-type name creation-form)
  (with-unique-names (type-number proc)
    `(let ((,type-number
            ,(if (and (keywordp info-type) (keywordp info-class))
                 (type-info-number (type-info-or-lose info-class info-type))
                 `(type-info-number
                   (type-info-or-lose ,info-class ,info-type)))))
       (dx-flet ((,proc () ,creation-form))
         (%get-info-value-initializing ,name ,type-number #',proc)))))

;; interface to %ATOMIC-SET-INFO-VALUE
;; GET-INFO-VALUE-INITIALIZING is a restricted case of this,
;; and perhaps could be implemented as such.
;; Atomic update will be important for making the fasloader threadsafe
;; using a predominantly lock-free design, and other nice things.
(def!macro atomic-set-info-value (info-class info-type name lambda)
  (with-unique-names (type-number proc)
    `(let ((,type-number
            ,(if (and (keywordp info-type) (keywordp info-class))
                 (type-info-number (type-info-or-lose info-class info-type))
                 `(type-info-number
                   (type-info-or-lose ,info-class ,info-type)))))
       ,(if (and (listp lambda) (eq (car lambda) 'lambda))
            ;; rewrite as FLET because the compiler is unable to dxify
            ;;   (DX-LET ((x (LAMBDA <whatever>))) (F x))
            (destructuring-bind (lambda-list . body) (cdr lambda)
              `(dx-flet ((,proc ,lambda-list ,@body))
                 (%atomic-set-info-value ,name ,type-number #',proc)))
            `(%atomic-set-info-value ,name ,type-number ,lambda)))))

;; Call FUNCTION once for each Name in globaldb that has information associated
;; with it, passing the function the Name as its only argument.
;;
(defun call-with-each-globaldb-name (fun-designator)
  (let ((function (coerce fun-designator 'function)))
    (dolist (package (list-all-packages))
      (do-symbols (symbol package)
        (when (eq (symbol-package symbol) package)
          (let ((vector (symbol-info-vector symbol)))
            (when vector
              ;; Check whether SYMBOL has info for itself
              (when (plusp (packed-info-field vector 0 0))
                (funcall function symbol))
              ;; Now deal with (<othersym> SYMBOL) names
              (do-packed-info-vector-aux-key (vector key-index)
                (funcall function
                         (construct-globaldb-name (svref vector key-index)
                                                  symbol))))))))
    (info-maphash (lambda (name data)
                    (declare (ignore data))
                    (funcall function name))
                  *info-environment*)))

;;;; ":FUNCTION" subsection - Data pertaining to globally known functions.

;; must be info type number 1
(define-info-type (:function :definition) :type-spec (or fdefn null))
(eval-when (:compile-toplevel)
  (aver (= (type-info-number (type-info-or-lose :function :definition))
           +fdefn-type-num+)))

;;; the kind of functional object being described. If null, NAME isn't
;;; a known functional object.
(define-info-type (:function :kind)
  :type-spec (member nil :function :macro :special-form)
  ;; I'm a little confused what the correct behavior of this default
  ;; is. It's not clear how to generalize the FBOUNDP expression to
  ;; the cross-compiler. As far as I can tell, NIL is a safe default
  ;; -- it might keep the compiler from making some valid
  ;; optimization, but it shouldn't produce incorrect code. -- WHN
  ;; 19990330
  :default
  #+sb-xc-host nil
  #-sb-xc-host (lambda (name) (if (fboundp name) :function nil)))

;;; The type specifier for this function.
(define-info-type (:function :type)
  :type-spec ctype
  ;; Again (as in DEFINE-INFO-TYPE :CLASS :FUNCTION :TYPE :KIND) it's
  ;; not clear how to generalize the FBOUNDP expression to the
  ;; cross-compiler. -- WHN 19990330
  :default
  ;; Delay evaluation of (SPECIFIER-TYPE) since it can't work yet
  #+sb-xc-host (lambda (x) (declare (ignore x)) (specifier-type 'function))
  #-sb-xc-host (lambda (name)
                 (if (fboundp name)
                     (handler-bind ((style-warning #'muffle-warning))
                       (specifier-type (sb!impl::%fun-type (fdefinition name))))
                     ;; I think this should be *universal-fun-type*
                     (specifier-type 'function))))

;;; the ASSUMED-TYPE for this function, if we have to infer the type
;;; due to not having a declaration or definition
(define-info-type (:function :assumed-type)
  ;; FIXME: The type-spec really should be
  ;;   (or approximate-fun-type null)).
  ;; It was changed to T as a hopefully-temporary hack while getting
  ;; cold init problems untangled.
  :type-spec t)

;;; where this information came from:
;;;    :ASSUMED  = from uses of the object
;;;    :DEFINED  = from examination of the definition
;;;    :DEFINED-METHOD = implicit, incremental declaration by CLOS.
;;;    :DECLARED = from a declaration
;;; :DEFINED trumps :ASSUMED, :DEFINED-METHOD trumps :DEFINED,
;;; and :DECLARED trumps :DEFINED-METHOD.
;;; :DEFINED and :ASSUMED are useful for issuing compile-time warnings,
;;; :DEFINED-METHOD and :DECLARED are useful for ANSIly specializing
;;; code which implements the function, or which uses the function's
;;; return values.
(define-info-type (:function :where-from)
  :type-spec (member :declared :defined-method :assumed :defined)
  :default
  ;; Again (as in DEFINE-INFO-TYPE :CLASS :FUNCTION :TYPE :KIND) it's
  ;; not clear how to generalize the FBOUNDP expression to the
  ;; cross-compiler. -- WHN 19990606
  #+sb-xc-host :assumed
  #-sb-xc-host (lambda (name) (if (fboundp name) :defined :assumed)))

;;; something which can be decoded into the inline expansion of the
;;; function, or NIL if there is none
;;;
;;; To inline a function, we want a lambda expression, e.g.
;;; '(LAMBDA (X) (+ X 1)). That can be encoded here in one of two
;;; ways.
;;;   * The value in INFO can be the lambda expression itself, e.g.
;;;       (SETF (INFO :FUNCTION :INLINE-EXPANSION-DESIGNATOR 'FOO)
;;;             '(LAMBDA (X) (+ X 1)))
;;;     This is the ordinary way, the natural way of representing e.g.
;;;       (DECLAIM (INLINE FOO))
;;;       (DEFUN FOO (X) (+ X 1))
;;;   * The value in INFO can be a closure which returns the lambda
;;;     expression, e.g.
;;;       (SETF (INFO :FUNCTION :INLINE-EXPANSION-DESIGNATOR 'BAR-LEFT-CHILD)
;;;             (LAMBDA ()
;;;               '(LAMBDA (BAR) (BAR-REF BAR 3))))
;;;     This twisty way of storing values is supported in order to
;;;     allow structure slot accessors, and perhaps later other
;;;     stereotyped functions, to be represented compactly.
(define-info-type (:function :inline-expansion-designator)
  :type-spec (or list function))

;;; This specifies whether this function may be expanded inline. If
;;; null, we don't care.
(define-info-type (:function :inlinep) :type-spec inlinep)

;;; Track how many times IR2 converted a call to this function as a full call
;;; that was not in the scope of a local or global notinline declaration.
;;; Useful for finding functions that were supposed to have been converted
;;; through some kind of transformation but were not.
(define-info-type (:function :static-full-call-count) :type-spec list)

;;; a macro-like function which transforms a call to this function
;;; into some other Lisp form. This expansion is inhibited if inline
;;; expansion is inhibited
(define-info-type (:function :source-transform) :type-spec (or function null))

;;; the macroexpansion function for this macro
(define-info-type (:function :macro-function) :type-spec (or function null))

;;; the compiler-macroexpansion function for this macro
(define-info-type (:function :compiler-macro-function)
  :type-spec (or function null))

;;; a function which converts this special form into IR1
(define-info-type (:function :ir1-convert) :type-spec (or function null))

;;; If a function is "known" to the compiler, then this is a FUN-INFO
;;; structure containing the info used to special-case compilation.
(define-info-type (:function :info) :type-spec (or fun-info null))

(define-info-type (:function :structure-accessor)
  :type-spec (or defstruct-description null))

;;;; ":VARIABLE" subsection - Data pertaining to globally known variables.

;;; the kind of variable-like thing described
(define-info-type (:variable :kind)
  :type-spec (member :special :constant :macro :global :alien :unknown)
  :default (lambda (name)
             (if (typep name '(or boolean keyword))
                 :constant
                 :unknown)))

(define-info-type (:variable :always-bound)
  :type-spec (member nil :eventually :always-bound))

(define-info-type (:variable :deprecated) :type-spec t)

;;; the declared type for this variable
(define-info-type (:variable :type)
  :type-spec ctype
  ;; This gets set to *UNIVERSAL-TYPE* in 'late-type'
  :default (lambda (x) (declare (ignore x)) (error "Too early for INFO")))

;;; where this type and kind information came from
(define-info-type (:variable :where-from)
  :type-spec (member :declared :assumed :defined) :default :assumed)

;;; the macro-expansion for symbol-macros
(define-info-type (:variable :macro-expansion) :type-spec t)

(define-info-type (:variable :alien-info)
  :type-spec (or heap-alien-info null))

(define-info-type (:variable :documentation) :type-spec (or string null))

;; :WIRED-TLS describes how SYMBOL-VALUE (implicit or not) should be compiled.
;;  - :ALWAYS-HAS-TLS means that calls to SYMBOL-VALUE should access the TLS
;;     with a fixed offset. The index is assigned no later than load-time of
;;     the file containing code thus compiled. Presence of an index in the
;;     image that performed compilation is irrelevant (for now).
;;  - :ALWAYS-THREAD-LOCAL implies a fixed offset, *and* that the check for
;;     no-tls-value may be elided. There is currently no way to set this.
;;     Note that this does not affect elision of the check for unbound-marker
;;     which is under control of the :ALWAYS-BOUND info.
;;  - an integer is a permanent index, and also implies :ALWAYS-THREAD-LOCAL.
;; Specials in the CL package (notably reader/printer controls) use a wired-tls,
;; whether or not we bind per-thread [if we don't, that's a bug!]
;; We don't assume wired TLS more generally, because user code often defines
;; thousands of DEFVARs, possibly due to poor style, or due to ANSI's stance
;; that DEFCONSTANT is only for EQL-comparable objects. In such cases with
;; more symbols than can be bound per-thread, the compiler won't exacerbate
;; things by making the loader eagerly assign a TLS index to every symbol
;; ever referenced by SYMBOL-VALUE or SET. Depletion should occur lazily.
;;
(define-info-type (:variable :wired-tls)
    :type-spec (or (member nil :always-has-tls :always-thread-local)
                   fixnum) ; the actual index, for thread slots (to be done)
    :default
    (lambda (symbol)
      (declare (symbol symbol))
      (and (eq (info :variable :kind symbol) :special)
           #-sb-xc-host
           (eq (symbol-package symbol) *cl-package*)
           #+sb-xc-host
           (flet ((external-in-package-p (pkg)
                    (and (string= (package-name (symbol-package symbol)) pkg)
                         (eq (nth-value 1 (find-symbol (string symbol) pkg))
                             :external))))
             ;; I'm not worried about random extra externals in some bizarro
             ;; host lisp. TLS assignment has no bearing on semantics at all.
             (or (external-in-package-p "COMMON-LISP")
                 (external-in-package-p "SB-XC")))
           :always-has-tls)))

;;;; ":TYPE" subsection - Data pertaining to globally known types.

;;; the kind of type described. We return :INSTANCE for standard types
;;; that are implemented as structures. For PCL classes, that have
;;; only been compiled, but not loaded yet, we return
;;; :FORTHCOMING-DEFCLASS-TYPE.
(define-info-type (:type :kind)
  :type-spec (member :primitive :defined :instance
                     :forthcoming-defclass-type nil)
  :validate-function (lambda (name new-value)
                       (declare (ignore new-value)
                                (notinline info))
                       (when (info :declaration :recognized name)
                         (error 'declaration-type-conflict-error
                                :format-arguments (list name)))))

;;; the expander function for a defined type
(define-info-type (:type :expander) :type-spec (or function null))

(define-info-type (:type :documentation) :type-spec (or string null))

;;; function that parses type specifiers into CTYPE structures
(define-info-type (:type :translator) :type-spec (or function null))

;;; If true, then the type coresponding to this name. Note that if
;;; this is a built-in class with a translation, then this is the
;;; translation, not the class object. This info type keeps track of
;;; various atomic types (NIL etc.) and also serves as a cache to
;;; ensure that common standard types (atomic and otherwise) are only
;;; consed once.
(define-info-type (:type :builtin) :type-spec (or ctype null))

;;; The classoid-cell for this type
(define-info-type (:type :classoid-cell) :type-spec t)

;;; layout for this type being used by the compiler
(define-info-type (:type :compiler-layout)
  :type-spec (or layout null)
  :default (lambda (name)
             (let ((class (find-classoid name nil)))
               (when class (classoid-layout class)))))

;;; DEFTYPE lambda-list
(define-info-type (:type :lambda-list) :type-spec list)

(define-info-type (:type :source-location) :type-spec t)

;;;; ":TYPED-STRUCTURE" subsection.
;;;; Data pertaining to structures that used DEFSTRUCT's :TYPE option.
(define-info-type (:typed-structure :info) :type-spec t)
(define-info-type (:typed-structure :documentation) :type-spec (or string null))

;;;; ":DECLARATION" subsection - Data pertaining to user-defined declarations.
;; CLTL2 offers an API to provide a list of known declarations, but it is
;; inefficient to iterate over all symbols to find ones which have the
;; (:DECLARATION :RECOGNIZED) info.
;; Therefore maintain a list of recognized declarations. This list makes the
;; globaldb storage of same redundant, but oh well.
(defglobal *recognized-declarations* nil)
(define-info-type (:declaration :recognized)
  :type-spec boolean
  ;; There's no portable way to unproclaim that a symbol is a declaration,
  ;; but at the low-level permit new-value to be NIL.
  :validate-function (lambda (name new-value)
                       (declare (symbol name)
                                (notinline info))
                       (cond (new-value
                              (when (info :type :kind name)
                                (error 'declaration-type-conflict-error
                                       :format-arguments (list name)))
                              (pushnew name *recognized-declarations*))
                             (t
                              (setq *recognized-declarations*
                                    (delete name *recognized-declarations*))))))

(define-info-type (:declaration :handler) :type-spec (or function null))

;;;; ":ALIEN-TYPE" subsection - Data pertaining to globally known alien-types.
(define-info-type (:alien-type :kind)
  :type-spec (member :primitive :defined :unknown)
  :default :unknown)
(define-info-type (:alien-type :translator) :type-spec (or function null))
(define-info-type (:alien-type :definition) :type-spec (or alien-type null))
(define-info-type (:alien-type :struct) :type-spec (or alien-type null))
(define-info-type (:alien-type :union) :type-spec (or alien-type null))
(define-info-type (:alien-type :enum) :type-spec (or alien-type null))

;;;; ":SETF" subsection - Data pertaining to expansion of the omnipotent macro.
(define-info-type (:setf :inverse) :type-spec (or symbol null))
(define-info-type (:setf :documentation) :type-spec (or string null))
(define-info-type (:setf :expander) :type-spec (or function null))

;;;; ":CAS" subsection - Like SETF but there are no "inverses", just expanders
(define-info-type (:cas :expander) :type-spec (or function null))

;;;; ":RANDOM-DOCUMENTATION" subsection.
;;; This is used for storing miscellaneous documentation types. The
;;; stuff is an alist translating documentation kinds to values.
(define-info-type (:random-documentation :stuff) :type-spec list)

;;;; ":SOURCE-LOCATION" subsection.
;;; This is kind of the opposite of what I'd have thought more logical,
;;; where each of the above subsections - also called "info classes" -
;;; has one of its kinds of information being :SOURCE-LOCATION.  And in fact
;;; that *is* how :TYPE was handled. However, many global entities
;;; store their source-location hanging off some other hook, avoiding the
;;; globaldb entirely, such as functions using a #<code-component>.
;;; So either way is basically a hodgepodge.

(define-info-type (:source-location :variable) :type-spec t)
(define-info-type (:source-location :constant) :type-spec t)
(define-info-type (:source-location :typed-structure) :type-spec t)
(define-info-type (:source-location :symbol-macro) :type-spec t)

#!-sb-fluid (declaim (freeze-type basic-info-env))

;; This is for the SB-INTROSPECT contrib module, and debugging.
(defun call-with-each-info (function symbol)
  (awhen (symbol-info-vector symbol)
    (%call-with-each-info function it symbol)))

;; This is for debugging at the REPL.
(defun show-info (sym)
  (let ((prev 0))
    (call-with-each-info
     (lambda (name type-num val)
       (unless (eq name prev)
         (format t "~&~S" (setq prev name)))
       (let ((type (svref *info-types* type-num)))
         (format t "~&  ~@[type ~D~]~@[~{~S ~S~}~] = "
                 (if (not type) type-num)
                 (if type
                     (list (type-info-class type) (type-info-name type))))
         (write val :level 1)))
     sym)))


;;; Now that we have finished initializing
;;; *INFO-TYPES* (at compile time), generate code to set them at cold
;;; load time to the same state they have currently.
(!cold-init-forms
  (/show0 "beginning *INFO-TYPES* initialization")
  #-sb-xc-host
  ;; Host already has this array, do not clobber it
  (setq *info-types* (make-array (ash 1 type-number-bits) :initial-element nil))
  (mapc (lambda (x)
          (register-info-metadata (first x) (second x) (third x) (fourth x)))
        '#.(loop for info-type across *info-types*
                  when info-type
                 collect (list (type-info-number info-type)
                               (type-info-class info-type)
                               (type-info-name info-type)
                               ;; KLUDGE: for repeatable xc fasls, to
                               ;; avoid different cross-compiler
                               ;; treatment of equal constants here we
                               ;; COPY-TREE, which is not in general a
                               ;; valid identity transformation
                               ;; [e.g. on (EQL (FOO))] but is OK for
                               ;; all the types we use here.
                               (copy-tree (type-info-type-spec info-type)))))
  (/show0 "done with *INFO-TYPES* initialization"))

;;; At cold load time, after the INFO-TYPE objects have been created,
;;; we can set their DEFAULT and TYPE slots.
(macrolet ((frob ()
             `(!cold-init-forms
               ;; I [dpk] really think reversal now is a red herring.
               ;; I see nothing that would fail here regardless of order.
                ,@(reverse *!reversed-type-info-init-forms*))))
  (frob))

;;; Source transforms / compiler macros for INFO functions.
;;;
;;; When building the XC, we give it a source transform, so that it can
;;; compile INFO calls in the target efficiently; we also give it a compiler
;;; macro, so that at least those INFO calls compiled after this file can be
;;; efficient. (Host compiler-macros do not fire when compiling the target,
;;; and source transforms don't fire when building the XC, so we need both.)
;;;
;;; Target needs just one, since there compiler macros and source-transforms
;;; are equivalent.
(macrolet ((def (name lambda-list form)
             (aver (member 'class lambda-list))
             (aver (member 'type lambda-list))
             `(progn
                #+sb-xc-host
                (define-source-transform ,name ,lambda-list
                  (if (and (keywordp class) (keywordp type))
                      ,form
                      (values nil t)))
                (define-compiler-macro ,name ,(append '(&whole .whole.) lambda-list)
                  (if (and (keywordp class) (keywordp type))
                      ,form
                      .whole.)))))

  (def info (class type name)
    (let (#+sb-xc-host (sb!xc:*gensym-counter* sb!xc:*gensym-counter*)
          (info (type-info-or-lose class type)))
      (with-unique-names (value foundp)
        `(multiple-value-bind (,value ,foundp)
             (get-info-value ,name ,(type-info-number info))
           (values (truly-the ,(type-info-type-spec info) ,value) ,foundp)))))

  (def (setf info) (new-value class type name)
    (let* (#+sb-xc-host (sb!xc:*gensym-counter* sb!xc:*gensym-counter*)
           (info (type-info-or-lose class type))
           (tin (type-info-number info))
           (type-spec (type-info-type-spec info))
           (check
            (when (type-info-validate-function info)
              ;; is (or ... null), but non-null in host implies non-null
              `(truly-the function
                (type-info-validate-function
                 (truly-the type-info (svref *info-types* ,tin)))))))
      (with-unique-names (new)
        `(let ((,new ,new-value))
           ;; enforce type-correctness regardless of enclosing policy
           (let ((,new (locally (declare (optimize (safety 3)))
                         (the ,type-spec ,new))))
             ,@(when check
                 `((funcall ,check ,name ,new)))
             (set-info-value ,name ,tin ,new))))))

  (def clear-info (class type name)
    (let ((info (type-info-or-lose class type)))
      `(clear-info-values ,name '(,(type-info-number info))))))

(!defun-from-collected-cold-init-forms !globaldb-cold-init)
