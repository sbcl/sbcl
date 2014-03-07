;;;; This file provides a functional interface to global information
;;;; about named things in the system. Information is considered to be
;;;; global if it must persist between invocations of the compiler. The
;;;; use of a functional interface eliminates the need for the compiler
;;;; to worry about the actual representation. This is important, since
;;;; the information may well have several representations.
;;;;
;;;; The database contains arbitrary Lisp values, addressed by a
;;;; combination of Name, Class and Type. The Name is a EQUAL-thing
;;;; which is the name of the thing we are recording information
;;;; about. Class is the kind of object involved. Typical classes are
;;;; :FUNCTION, :VARIABLE, :TYPE, ... A Type names a particular piece
;;;; of information within a given class. Class and Type are keywords,
;;;; and are compared with EQ.

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

;;; The DEFVAR for this appears later.
;;; FIXME: centralize
(declaim (special *universal-type*))

;;; This is sorta semantically equivalent to SXHASH, but optimized for
;;; legal function names.
;;;
;;; Why optimize? We want to avoid the fully-general TYPECASE in ordinary
;;; SXHASH, because
;;;   1. This hash function has to run when we're initializing the globaldb,
;;;      so it has to run before the type system is initialized, and it's
;;;      easier to make it do this if we don't try to do a general TYPECASE.
;;;   2. This function is in a potential bottleneck for the compiler,
;;;      and avoiding the general TYPECASE lets us improve performance
;;;      because
;;;     2a. the general TYPECASE is intrinsically slow, and
;;;     2b. the general TYPECASE is too big for us to easily afford
;;;         to inline it, so it brings with it a full function call.
;;;
;;; Why not specialize instead of optimize? (I.e. why fall through to
;;; general SXHASH as a last resort?) Because the INFO database is used
;;; to hold all manner of things, e.g. (INFO :TYPE :BUILTIN ..)
;;; which is called on values like (UNSIGNED-BYTE 29). Falling through
;;; to SXHASH lets us support all manner of things (as long as they
;;; aren't used too early in cold boot for SXHASH to run).
#!-sb-fluid (declaim (inline globaldb-sxhashoid))
(defun globaldb-sxhashoid (x)
  (logand sb!xc:most-positive-fixnum
          (cond ((symbolp x) (sxhash x))
                ((and (listp x)
                      (eq (first x) 'setf)
                      (let ((rest (rest x)))
                        (and (symbolp (car rest))
                             (null (cdr rest)))))
                 ;; We need to declare the type of the value we're feeding to
                 ;; SXHASH so that the DEFTRANSFORM on symbols kicks in.
                 (let ((symbol (second x)))
                   (declare (symbol symbol))
                   (logxor (sxhash symbol) 110680597)))
                (t (sxhash x)))))

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

(defstruct (class-info
            (:constructor make-class-info (name))
            #-no-ansi-print-object
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s :type t)
                               (prin1 (class-info-name x) s))))
            (:copier nil))
  ;; name of this class
  (name nil :type keyword :read-only t)
  ;; list of Type-Info structures for each type in this class
  (types () :type list))

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
(defvar *info-types*)
(declaim (type simple-vector *info-types*))
#-sb-xc ; as per KLUDGE note above
(eval-when (:compile-toplevel :execute)
  (setf *info-types*
        (make-array (ash 1 type-number-bits) :initial-element nil)))

(defstruct (type-info
            #-no-ansi-print-object
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s)
                               (format s
                                       "~S ~S, Number = ~W"
                                       (class-info-name (type-info-class x))
                                       (type-info-name x)
                                       (type-info-number x)))))
            (:copier nil))
  ;; the name of this type
  (name (missing-arg) :type keyword)
  ;; this type's class
  (class (missing-arg) :type class-info)
  ;; a number that uniquely identifies this type (and implicitly its class)
  (number (missing-arg) :type type-number)
  ;; a type specifier which info of this type must satisfy
  (type nil :type t)
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

;;; a map from class names to CLASS-INFO structures
;;;
;;; We build the value for this at compile time (with calls to
;;; DEFINE-INFO-CLASS), then generate code to recreate the compile time
;;; value, and arrange for that code to be called in cold load.
;;; KLUDGE: Just as for *INFO-TYPES*, we don't try to rebuild this
;;; when cross-compiling, but instead just reuse the cross-compiler's
;;; version for the target compiler. -- WHN 2001-08-19
(defvar *info-classes*)
(declaim (hash-table *info-classes*))
#-sb-xc ; as per KLUDGE note above
(eval-when (:compile-toplevel :execute)
  (setf *info-classes* (make-hash-table :test #'eq)))

;;; If NAME is the name of a type in CLASS, then return the TYPE-INFO,
;;; otherwise NIL.
(defun find-type-info (name class)
  (declare (type keyword name) (type class-info class))
  (dolist (type (class-info-types class) nil)
    (when (eq (type-info-name type) name)
      (return type))))

;;; Return the info structure for an info class or type, or die trying.
(declaim (ftype (function (keyword) class-info) class-info-or-lose))
(defun class-info-or-lose (class)
  (declare (type keyword class))
  #+sb-xc (/noshow0 "entering CLASS-INFO-OR-LOSE, CLASS=..")
  #+sb-xc (/nohexstr class)
  (prog1
      (flet ((lookup (class)
               (or (gethash class *info-classes*)
                   (error "~S is not a defined info class." class))))
        (if (symbolp class)
            (or (get class 'class-info-or-lose-cache)
                (setf (get class 'class-info-or-lose-cache)
                      (lookup class)))
            (lookup class)))
    #+sb-xc (/noshow0 "returning from CLASS-INFO-OR-LOSE")))
(declaim (ftype (function (keyword keyword) type-info) type-info-or-lose))
(defun type-info-or-lose (class type)
  #+sb-xc (/noshow0 "entering TYPE-INFO-OR-LOSE, CLASS,TYPE=..")
  #+sb-xc (/nohexstr class)
  #+sb-xc (/nohexstr type)
  (prog1
      (or (find-type-info type (class-info-or-lose class))
          (error "~S is not a defined info type." type))
    #+sb-xc (/noshow0 "returning from TYPE-INFO-OR-LOSE")))

) ; EVAL-WHEN

;;;; info classes, info types, and type numbers, part II: what's
;;;; needed only at compile time, not at run time

;;; FIXME: Perhaps this stuff (the definition of DEFINE-INFO-CLASS
;;; and the calls to it) could/should go in a separate file,
;;; perhaps info-classes.lisp?

(eval-when (:compile-toplevel :execute)

;;; Set up the data structures to support an info class.
;;;
;;; comment from CMU CL:
;;;   We make sure that the class exists at compile time so that
;;;   macros can use it, but we don't actually store the init function
;;;   until load time so that we don't break the running compiler.
;;; KLUDGE: I don't think that's the way it is any more, but I haven't
;;; looked into it enough to write a better comment. -- WHN 2001-03-06
(#+sb-xc-host defmacro
 #-sb-xc-host sb!xc:defmacro
     define-info-class (class)
  (declare (type keyword class))
  `(progn
     ;; (We don't need to evaluate this at load time, compile time is
     ;; enough. There's special logic elsewhere which deals with cold
     ;; load initialization by inspecting the info class data
     ;; structures at compile time and generating code to recreate
     ;; those data structures.)
     (eval-when (:compile-toplevel :execute)
       (unless (gethash ,class *info-classes*)
         (setf (gethash ,class *info-classes*) (make-class-info ,class))))
     ,class))

;;; Find a type number not already in use by looking for a null entry
;;; in *INFO-TYPES*.
;;; The zeroth type is reserved as a tombstone to support deletion
;;; from a compact info environment.
(defun find-unused-type-number ()
  (or (position nil *info-types* :start 1)
      (error "no more INFO type numbers available")))

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

;;; Define a new type of global information for CLASS. TYPE is the
;;; name of the type, DEFAULT is a defaulting expression, and TYPE-SPEC
;;; is a type specifier which values of the type must satisfy.
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
       (let* ((class-info (class-info-or-lose ',class))
              (old-type-info (find-type-info ',type class-info)))
         (unless old-type-info
           (let* ((new-type-number (find-unused-type-number))
                  (new-type-info
                   (make-type-info :name ',type
                                   :class class-info
                                   :number new-type-number
                                   :type ',type-spec)))
             (setf (aref *info-types* new-type-number) new-type-info)
             (push new-type-info (class-info-types class-info)))))
       ;; Arrange for TYPE-INFO-DEFAULT, TYPE-INFO-TYPE-CHECKER, and
       ;; TYPE-INFO-VALIDATE-FUNCTION to be set at cold load
       ;; time. (They can't very well be set at cross-compile time,
       ;; since they differ between host and target and are
       ;; host-compiled closures.)
       (push `(let ((type-info (type-info-or-lose ,',class ,',type)))
                ,@',(unless (eq type-spec 't)
                      ;; avoid re-inventing #'IDENTITY N times over
                      `((setf (type-info-type-checker type-info)
                              (lambda (x) (declare (type ,type-spec x)) x))))
                (setf (type-info-validate-function type-info)
                      ,',validate-function
                      (type-info-default type-info) ,',default))
             *!reversed-type-info-init-forms*))
     ',type))

) ; EVAL-WHEN

;;;; generic info environments

(defstruct (basic-info-env (:constructor nil) (:copier nil))
  ;; some string describing what is in this environment, for
  ;; printing/debugging purposes only
  (name (missing-arg) :type string))
(def!method print-object ((x basic-info-env) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (basic-info-env-name x) stream)))

;;;; generic interfaces

(defmacro do-info ((env &key (name (gensym)) (class (gensym)) (type (gensym))
                        (type-number (gensym)) (value (gensym)) known-volatile)
                   &body body)
  #!+sb-doc
  "DO-INFO (Env &Key Name Class Type Value) Form*
  Iterate over all the values stored in the Info-Env Env. Name is bound to
  the entry's name, Class and Type are bound to the class and type
  (represented as keywords), and Value is bound to the entry's value."
  (once-only ((n-env env))
    (if known-volatile
        (do-volatile-info name class type type-number value n-env body)
        `(if (typep ,n-env 'volatile-info-env)
             ,(do-volatile-info name class type type-number value n-env body)
             ,(do-compact-info name class type type-number value
                               n-env body)))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

;;; Return code to iterate over a compact info environment.
(defun do-compact-info (name-var class-var type-var type-number-var value-var
                                 n-env body)
  (let ((n-index (gensym))
        (n-type (gensym))
        (punt (gensym)))
    (once-only ((n-table `(compact-info-env-table ,n-env))
                (n-entries-index `(compact-info-env-index ,n-env))
                (n-entries `(compact-info-env-entries ,n-env))
                (n-entries-info `(compact-info-env-entries-info ,n-env))
                (n-info-types '*info-types*))
      `(dotimes (,n-index (length ,n-table))
         (declare (type index ,n-index))
         (block ,punt
           (let ((,name-var (svref ,n-table ,n-index)))
             (unless (eql ,name-var 0)
               (do-anonymous ((,n-type (aref ,n-entries-index ,n-index)
                                       (1+ ,n-type)))
                             (nil)
                 (declare (type index ,n-type))
                 ,(once-only ((n-info `(aref ,n-entries-info ,n-type)))
                    `(let ((,type-number-var
                            (logand ,n-info compact-info-entry-type-mask)))
                       ,(once-only ((n-type-info
                                     `(svref ,n-info-types
                                             ,type-number-var)))
                          `(let ((,type-var (type-info-name ,n-type-info))
                                 (,class-var (class-info-name
                                              (type-info-class ,n-type-info)))
                                 (,value-var (svref ,n-entries ,n-type)))
                             (declare (ignorable ,type-var ,class-var
                                                 ,value-var))
                             ,@body
                             (unless (zerop (logand ,n-info
                                                    compact-info-entry-last))
                               (return-from ,punt))))))))))))))

;;; Return code to iterate over a volatile info environment.
(defun do-volatile-info (name-var class-var type-var type-number-var value-var
                                  n-env body)
  (let ((n-index (gensym)) (n-names (gensym)) (n-types (gensym)))
    (once-only ((n-table `(volatile-info-env-table ,n-env))
                (n-info-types '*info-types*))
      `(dotimes (,n-index (length ,n-table))
         (declare (type index ,n-index))
         (do-anonymous ((,n-names (svref ,n-table ,n-index)
                                  (cdr ,n-names)))
                       ((null ,n-names))
           (let ((,name-var (caar ,n-names)))
             (declare (ignorable ,name-var))
             (do-anonymous ((,n-types (cdar ,n-names) (cdr ,n-types)))
                           ((null ,n-types))
               (let ((,type-number-var (caar ,n-types)))
                 ,(once-only ((n-type `(svref ,n-info-types
                                              ,type-number-var)))
                    `(let ((,type-var (type-info-name ,n-type))
                           (,class-var (class-info-name
                                        (type-info-class ,n-type)))
                           (,value-var (cdar ,n-types)))
                       (declare (ignorable ,type-var ,class-var ,value-var))
                       ,@body))))))))))

) ; EVAL-WHEN


;;;; compact info environments

;;; The upper limit on the size of the ENTRIES vector in a COMPACT-INFO-ENV.
;;;
;;; "Why (U-B 28)?", you might wonder. Originally this was (U-B 16),
;;; presumably to ensure that the arrays of :ELEMENT-TYPE
;;; COMPACT-INFO-ENTRIES-INDEX could use a more space-efficient representation.
;;; It turns out that a environment of of only 65536 entries is insufficient in
;;; the modern world (see message from Cyrus Harmon to sbcl-devel, "Subject:
;;; purify failure when compact-info-env-entries-bits is too small"). Using
;;; (U-B 28) instead of (U-B 29) is to avoid the need for bignum overflow
;;; checks, a probably pointless micro-optimization. Hardcoding the amount of
;;; bits instead of deriving it from SB!VM::N-WORD-BITS is done to allow
;;; use of a more efficient array representation on 64-bit platforms.
;;;   -- JES, 2005-04-06
(def!constant compact-info-env-entries-bits 28)
(deftype compact-info-entries-index () `(unsigned-byte ,compact-info-env-entries-bits))

;;; the type of the values in COMPACT-INFO-ENTRIES-INFO
(deftype compact-info-entry () `(unsigned-byte ,(1+ type-number-bits)))

;;; This is an open hashtable with rehashing. Since modification is
;;; not allowed, we don't have to worry about deleted entries. We
;;; indirect through a parallel vector to find the index in the
;;; ENTRIES at which the entries for a given name starts.
(defstruct (compact-info-env (:include basic-info-env)
                             #-sb-xc-host (:pure :substructure)
                             (:copier nil))
  ;; hashtable of the names in this environment. If a bucket is
  ;; unused, it is 0.
  (table (missing-arg) :type simple-vector)
  ;; an indirection vector parallel to TABLE, translating indices in
  ;; TABLE to the start of the ENTRIES for that name. Unused entries
  ;; are undefined.
  (index (missing-arg) :type (simple-array compact-info-entries-index (*)))
  ;; a vector contining in contiguous ranges the values of for all the
  ;; types of info for each name.
  (entries (missing-arg) :type simple-vector)
  ;; a vector parallel to ENTRIES, indicating the type number for the
  ;; value stored in that location and whether this location is the
  ;; last type of info stored for this name. The type number is in the
  ;; low TYPE-NUMBER-BITS bits, and the next bit is set if this is the
  ;; last entry.
  (entries-info (missing-arg) :type (simple-array compact-info-entry (*))))

(def!constant compact-info-entry-type-mask (ldb (byte type-number-bits 0) -1))
(def!constant compact-info-entry-last (ash 1 type-number-bits))

;;; Return the index of the info-type corresponding to NUMBER starting
;;; at INDEX in compact ENV and scanning linearly until a match occurs,
;;; or hitting an entry whose 'last' flag is set.
;;; Return NIL if no match was found.
#!-sb-fluid (declaim (inline compact-info-lookup-index))
(defun compact-info-lookup-index (env number index)
  (declare (type compact-info-env env) (type type-number number))
  (let ((entries-info (compact-info-env-entries-info env)))
    (do ((index index (1+ index)))
        (nil)
      (declare (type index index))
      (let ((info (aref entries-info index)))
        (when (= (logand info compact-info-entry-type-mask) number)
          (return index))
        (when (logtest compact-info-entry-last info)
          (return nil))))))

;;; Look up the key composed of (TYPE-NUMBER,NAME) in the compact ENV.
;;; HASH is the GLOBALDB-SXHASHOID of NAME.
;;; If found, return the index into the parallel arrays 'entries-info'
;;; and 'entries' for that key.
(defun compact-info-find-key (env type-number name hash)
  (declare (type compact-info-env env)
           (type (integer 0 #.sb!xc:most-positive-fixnum) hash))
  (let* ((table (compact-info-env-table env))
         (len (length table))
         (len-2 (- len 2))
         (hash2 (- len-2 (rem hash len-2))))
    (declare (type index len-2 hash2))
    (macrolet ((lookup (test)
                 `(do ((probe (rem hash len)
                              (let ((new (+ probe hash2)))
                                (declare (type index new))
                                ;; same as (MOD NEW LEN), but faster.
                                (if (>= new len)
                                    (the index (- new len))
                                    new))))
                      (nil)
                    (let ((entry (svref table probe)))
                      (when (eql entry 0)
                        (return nil))
                      (when (,test entry name)
                        (return (compact-info-lookup-index
                                 env
                                 type-number
                                 (aref (compact-info-env-index env) probe))))))))
          (lookup equal))))

;; Find and return the value for (TYPE-NUMBER,NAME) in compact ENV
;; given also the HASH of NAME, and returning as a secondary value
;; a boolean flag indicating whether the key pair was found.
#!-sb-fluid (declaim (inline compact-info-lookup))
(defun compact-info-lookup (env type-number name hash)
  (let ((index (compact-info-find-key env type-number name hash)))
    (if index
        (values (svref (compact-info-env-entries env) index) t)
        (values nil nil))))

;; Clear the value for (TYPE-NUMBER,NAME) if present in compact ENV,
;; given also the HASH of NAME. Return true if anything was cleared.
(defun compact-info-clear (env type-number name hash)
  (let ((index (compact-info-find-key env type-number name hash)))
    (when index
      (let ((entries-info (compact-info-env-entries-info env)))
        ;; Change the type-number of this entry to 0 and its data to NIL.
        ;; Preserve the 'last' flag intact.
        (setf (aref entries-info index) (logand (aref entries-info index)
                                                compact-info-entry-last)
              (svref (compact-info-env-entries env) index) nil)
        t))))

;;; the exact density (modulo rounding) of the hashtable in a compact
;;; info environment in names/bucket
(def!constant compact-info-environment-density 65)

;;; Return a new compact info environment that holds the same
;;; information as ENV.
(defun compact-info-environment (env &key (name (basic-info-env-name env)))
  (let ((name-count 0)
        (prev-name 0)
        (entry-count 0))
    (/show0 "before COLLECT in COMPACT-INFO-ENVIRONMENT")

    ;; Iterate over the environment once to find out how many names
    ;; and entries it has, then build the result. This code assumes
    ;; that all the entries for a name well be iterated over
    ;; contiguously, which holds true for the implementation of
    ;; iteration over both kinds of environments.
    (collect ((names))

      (/show0 "at head of COLLECT in COMPACT-INFO-ENVIRONMENT")
      (let ((types ()))
        (do-info (env :name name :type-number num :value value)
          (/noshow0 "at head of DO-INFO in COMPACT-INFO-ENVIRONMENT")
          (unless (eq name prev-name)
            (/noshow0 "not (EQ NAME PREV-NAME) case")
            (incf name-count)
            (unless (eql prev-name 0)
              (names (cons prev-name types)))
            (setq prev-name name)
            (setq types ()))
          (incf entry-count)
          (push (cons num value) types))
        (unless (eql prev-name 0)
          (/show0 "not (EQL PREV-NAME 0) case")
          (names (cons prev-name types))))

      ;; Now that we know how big the environment is, we can build
      ;; a table to represent it.
      ;;
      ;; When building the table, we sort the entries by pointer
      ;; comparison in an attempt to preserve any VM locality present
      ;; in the original load order, rather than randomizing with the
      ;; original hash function.
      (/show0 "about to make/sort vectors in COMPACT-INFO-ENVIRONMENT")
      (let* ((table-size (primify
                          (+ (truncate (* name-count 100)
                                       compact-info-environment-density)
                             3)))
             (table (make-array table-size :initial-element 0))
             (index (make-array table-size
                                :element-type 'compact-info-entries-index))
             (entries (make-array entry-count))
             (entries-info (make-array entry-count
                                       :element-type 'compact-info-entry))
             (sorted (sort (names)
                           #+sb-xc-host #'<
                           ;; POINTER-HASH hack implements pointer
                           ;; comparison, as explained above.
                           #-sb-xc-host (lambda (x y)
                                          (< (pointer-hash x)
                                             (pointer-hash y))))))
        (/show0 "done making/sorting vectors in COMPACT-INFO-ENVIRONMENT")
        (let ((entries-idx 0))
          (dolist (types sorted)
            (let* ((name (first types))
                   (hash (globaldb-sxhashoid name))
                   (len-2 (- table-size 2))
                   (hash2 (- len-2 (rem hash len-2))))
              (do ((probe (rem hash table-size)
                          (rem (+ probe hash2) table-size)))
                  (nil)
                (let ((entry (svref table probe)))
                  (when (eql entry 0)
                    (setf (svref table probe) name)
                    (setf (aref index probe) entries-idx)
                    (return))
                  (aver (not (equal entry name))))))

            (unless (zerop entries-idx)
              (setf (aref entries-info (1- entries-idx))
                    (logior (aref entries-info (1- entries-idx))
                            compact-info-entry-last)))

            (loop for (num . value) in (rest types) do
              (setf (aref entries-info entries-idx) num)
              (setf (aref entries entries-idx) value)
              (incf entries-idx)))
          (/show0 "done w/ DOLIST (TYPES SORTED) in COMPACT-INFO-ENVIRONMENT")

          (unless (zerop entry-count)
            (/show0 "nonZEROP ENTRY-COUNT")
            (setf (aref entries-info (1- entry-count))
                  (logior (aref entries-info (1- entry-count))
                          compact-info-entry-last)))

          (/show0 "falling through to MAKE-COMPACT-INFO-ENV")
          (make-compact-info-env :name name
                                 :table table
                                 :index index
                                 :entries entries
                                 :entries-info entries-info))))))

;;;; volatile environments

;;; This is a closed hashtable, with the bucket being computed by
;;; taking the GLOBALDB-SXHASHOID of the NAME modulo the table size.
(defstruct (volatile-info-env (:include basic-info-env)
                              (:copier nil))
  ;; vector of alists of alists of the form:
  ;;    ((Name . ((Type-Number . Value) ...) ...)
  (table (missing-arg) :type simple-vector)
  ;; the number of distinct names currently in this table. Each name
  ;; may have multiple entries, since there can be many types of info.
  (count 0 :type index)
  ;; the number of names at which we should grow the table and rehash
  (threshold 0 :type index))

;;; Just like COMPACT-INFO-LOOKUP, only do it on a volatile environment.
(defun volatile-info-lookup (env type-number name hash)
  (declare (type volatile-info-env env)
           (type (integer 0 #.sb!xc:most-positive-fixnum) hash))
  (let ((table (volatile-info-env-table env)))
    (macrolet ((lookup (test)
                 `(dolist (entry (svref table (mod hash (length table))) ())
                    (when (,test (car entry) name)
                      (dolist (type (cdr entry))
                        (when (eql (car type) type-number)
                          (return-from volatile-info-lookup
                            (values (cdr type) t))))
                      (return-from volatile-info-lookup
                        (values nil nil))))))
      (if (symbolp name)
          (lookup eq)
          (lookup equal)))))

;;; Given a volatile environment ENV, bind TABLE-VAR the environment's table
;;; and INDEX-VAR to the index of NAME's bucket in the table.
(eval-when (:compile-toplevel :execute)
  (#+sb-xc-host cl:defmacro
   #-sb-xc-host sb!xc:defmacro
      with-info-bucket ((table-var index-var name env) &body body)
    (once-only ((n-name name)
                (n-env env))
      `(progn
         (let* ((,table-var (volatile-info-env-table ,n-env))
                (,index-var (mod (globaldb-sxhashoid ,n-name)
                                 (length ,table-var))))
           ,@body)))))

;;; Get the info environment that we use for write/modification operations.
;;; This is always the first environment in the list, and must be a
;;; VOLATILE-INFO-ENV.
#!-sb-fluid (declaim (inline get-write-info-env))
(defun get-write-info-env (&optional (env-list *info-environment*))
  (let ((env (car env-list)))
    (unless env
      (error "no info environment?"))
    (unless (typep env 'volatile-info-env)
      (error "cannot modify this environment: ~S" env))
    (the volatile-info-env env)))

(declaim (ftype (function (simple-vector (or symbol (eql 0)) type-number)
                          (or null index))
                packed-info-value-index))

;;; Some of this stuff belongs in 'symbol.lisp', but is here
;;; because 'symbol.lisp' is :NOT-HOST in build-order.

;; Return the globaldb info for SYMBOL. With respect to the state diagram
;; presented at the definition of SYMBOL-PLIST, if the object in SYMBOL's
;; info slot is LISTP, it is in state 1 or 3. Either way, take the CDR.
;; Otherwise, it is in state 2 so return the value as-is.
;; In terms of this function being named "-vector", implying always a vector,
;; it is understood that NIL is a proxy for +NIL-PACKED-INFOS+, a vector.
;;
#!-symbol-info-vops (declaim (inline symbol-info-vector))
(defun symbol-info-vector (symbol)
  (let ((info-holder (symbol-info symbol)))
    (truly-the (or null simple-vector)
               (if (listp info-holder) (cdr info-holder) info-holder))))

;; Atomically update SYMBOL's info/plist slot to contain a new info vector.
;; The vector is computed by calling UPDATE-FN on the old vector,
;; repeatedly as necessary, until no conflict happens with other updaters.

#+sb-xc-host
;; In the Host Lisp, there is no such thing as a symbol-info slot,
;; even if the host is SBCL. Instead, symbol-info is kept in the symbol-plist.
(macrolet ((get-it () '(get symbol :sb-xc-globaldb-info)))
  (defun symbol-info (symbol) (get-it))
  (defun update-symbol-info (symbol update-fn)
    ;; Never pass NIL to an update-fn. Pass the minimal info-vector instead,
    ;; a vector describing 0 infos and 0 auxilliary keys.
    (let ((newval (funcall update-fn (or (get-it) +nil-packed-infos+))))
      (when newval
        (setf (get-it) newval))
      (values))))

#-sb-xc-host
(defun update-symbol-info (symbol update-fn)
  (declare (symbol symbol)
           (type (function (t) t) update-fn))
  (let* ((info-holder (symbol-info symbol))
         ;; Do not use SYMBOL-INFO-VECTOR here. We must always refer to the
         ;; value most recently read from the info slot,
         ;; because only that way can three states be distinguished.
         (current-info (if (listp info-holder) (cdr info-holder) info-holder)))
    (loop
       ;; KLUDGE: The "#." on +nil-packed-infos+ is due to slightly crippled
       ;; fops in genesis's fasload. Anonymizing the constant works around the
       ;; issue, at the expense of an extra copy of the empty info vector.
       (let ((new-vect (funcall update-fn
                                (or current-info #.+nil-packed-infos+))))
         (when (null new-vect) (return)) ; nothing to do
         (if (consp info-holder) ; State 3: exchange the CDR
             (let ((old
                    (%compare-and-swap-cdr info-holder current-info new-vect)))
               (if (eq old current-info) (return)) ; win
               (setq current-info old)) ; Don't touch holder- it's still a cons
             ;; State 1 or 2: info-holder is NIL or a vector.
             (let ((old ; Exchange the contents of the info slot.
                    (%compare-and-swap-symbol-info
                     symbol info-holder new-vect)))
               (if (eq old info-holder) (return)) ; win
               ;; Check whether we're in state 2 or 3 now.
               ;; Impossible to be in state 1: nobody ever puts NIL in the slot.
               ;; Up above, we bailed out if the update-fn returned NIL.
               ;; See also ({SETF|CAS} SYMBOL-PLIST) for their invariant.
               (setq info-holder old
                     current-info (if (listp old) (cdr old) old)))))))
  (values))

;; Helper for SET-INFO-VALUE to update packed info vectors.
;; If NAME is one that supports storage of its infos in a symbol-info cell,
;; then do the update and return T. Otherwise return NIL.
;;
;; If the old info can be updated without unpacking/repacking, then copy it
;; and do the update; otherwise unpack, grow the vector, and repack.
;; Truly in-place updates are never permitted due to the very minimal lockfree
;; protocol. Since info vectors are short, it's ok that the old one is discarded
;; every time a value is written. A more intricate lockfree protocol would
;; allow re-use of the vector when changing the value in an existing cell.
;;
(declaim (ftype (sfunction (t type-number t) boolean) symbol-set-info-value))
(defun symbol-set-info-value (name type-number new-value)
  (with-possibly-compound-name (key1 key2) name
    (dx-flet ((update (vect) ; VECT is a packed vector, never NIL
                (declare (simple-vector vect))
                (let ((index (packed-info-value-index vect key2 type-number)))
                  (if (not index)
                      (packed-info-insert vect key2 type-number new-value)
                      (let ((copy (copy-seq vect)))
                        (setf (svref copy index) new-value)
                        copy)))))
      (update-symbol-info key1 #'update)
      (return-from symbol-set-info-value t))))

;;; If Name is already present in the table, then just create or
;;; modify the specified type. Otherwise, add the new name and type,
;;; checking for rehashing.
;;;
;;; We rehash by making a new larger environment, copying all of the
;;; entries into it, then clobbering the old environment with the new
;;; environment's table. We clear the old table to prevent it from
;;; holding onto garbage if it is statically allocated.
;;;
;;; We return the new value so that this can be conveniently used in a
;;; SETF function.
(defun set-info-value (name0 type new-value)
  (let ((name (uncross name0)))
    (when (eql name 0)
      (error "0 is not a legal INFO name."))
    (labels ((set-it (name type new-value env)
               (declare (type type-number type)
                         (type volatile-info-env env))
               (with-info-bucket (table index name env)
                 (let ((types (assoc name (svref table index) :test #'equal)))
                   (cond
                     (types
                      (let ((value (assoc type (cdr types))))
                        (if value
                            (setf (cdr value) new-value)
                            (push (cons type new-value) (cdr types)))))
                     (t
                      (push (cons name (list (cons type new-value)))
                            (svref table index))

                      (let ((count (incf (volatile-info-env-count env))))
                        (when (>= count (volatile-info-env-threshold env))
                          (let ((new (make-info-environment :size (* count 2))))
                            (do-info (env :name entry-name :type-number entry-num
                                          :value entry-val :known-volatile t)
                              (set-it entry-name entry-num entry-val new))
                            (fill (volatile-info-env-table env) nil)
                            (setf (volatile-info-env-table env)
                                  (volatile-info-env-table new))
                            (setf (volatile-info-env-threshold env)
                                  (volatile-info-env-threshold new)))))))))))
      (or (symbol-set-info-value name type new-value)
          (set-it name type new-value (get-write-info-env))))
    new-value))

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
  (let ((info (type-info-or-lose class type)))
    (clear-info-value name (type-info-number info))))

(defun clear-info-value (name type &aux anything-cleared-p)
  (declare (type type-number type) (inline assoc))
  (with-possibly-compound-name (key1 key2) name
    (let (new)
      (dx-flet ((update (old) (setq new (packed-info-remove old key2 type))))
        (update-symbol-info key1 #'update))
      (return-from clear-info-value (not (null new)))))
  ;; Clear the frontmost environment.
  (with-info-bucket (table index name (get-write-info-env))
    (let ((types (assoc name (svref table index) :test #'equal)))
      (when (assoc type (cdr types))
        (setf (cdr types) (delete type (cdr types) :key #'car)
              anything-cleared-p t))))
  ;; Clear the older (compact) environments.
  (let ((hash (globaldb-sxhashoid name)))
    (dolist (env (cdr *info-environment*))
      (aver (compact-info-env-p env))
      (when (compact-info-clear env type name hash)
        (setq anything-cleared-p t))))
  anything-cleared-p)

;;; the maximum density of the hashtable in a volatile env (in
;;; names/bucket)
;;;
;;; FIXME: actually seems to be measured in percent, should be
;;; converted to be measured in names/bucket
(def!constant volatile-info-environment-density 50)

;;; Make a new volatile environment of the specified size.
(defun make-info-environment (&key (size 42) (name "Unknown"))
  (declare (type (integer 1) size))
  (let ((table-size (primify (truncate (* size 100)
                                       volatile-info-environment-density))))
    (make-volatile-info-env :name name
                            :table (make-array table-size :initial-element nil)
                            :threshold size)))

;;;; *INFO-ENVIRONMENT*

;;; We do info access relative to the current *INFO-ENVIRONMENT*, a
;;; list of INFO-ENVIRONMENT structures.
(defvar *info-environment*)
(declaim (type list *info-environment*))
(!cold-init-forms
  (setq *info-environment*
        (list (make-info-environment :name "initial global")))
  (/show0 "done setting *INFO-ENVIRONMENT*"))
;;; FIXME: should perhaps be *INFO-ENV-LIST*. And rename
;;; all FOO-INFO-ENVIRONMENT-BAR stuff to FOO-INFO-ENV-BAR.

;;;; GET-INFO-VALUE

;;; Return the value of NAME / TYPE-NUMBER from the first environment that
;;; has it defined, or return the default if none does. We used to
;;; do a lot of complicated caching here, but that was removed for
;;; thread-safety reasons.
(declaim (ftype (sfunction (t type-number) (values t boolean))
                get-info-value hash-get-info-value))
(macrolet ((default ()
             `(let ((val (type-info-default metainfo)))
                (values (if (functionp val) (funcall val name) val) nil))))
  (defun hash-get-info-value (name0 type-number)
  ;; sanity check: If we have screwed up initialization somehow, then
  ;; *INFO-TYPES* could still be uninitialized at the time we try to
  ;; get an info value, and then we'd be out of luck. (This happened,
  ;; and was confusing to debug, when rewriting EVAL-WHEN in
  ;; sbcl-0.pre7.x.)
    (let* ((metainfo (aref *info-types* type-number))
           (name (uncross name0))
           (hash (globaldb-sxhashoid name)))
      (aver metainfo)
      (dolist (env *info-environment* (default))
        (multiple-value-bind (value winp)
            (etypecase env
              (volatile-info-env
               (volatile-info-lookup env type-number name hash))
              (compact-info-env
               (compact-info-lookup env type-number name hash)))
          (when winp (return (values value winp)))))))

  (defun get-info-value (name0 type-number) ; general entry point
    (declare (type type-number type-number))
    (let ((metainfo (aref *info-types* type-number))
          (name (uncross name0)))
      (aver metainfo)
      (with-possibly-compound-name (key1 key2) name
        (let ((vector (symbol-info-vector key1)))
          (when vector
            (awhen (packed-info-value-index vector key2 type-number)
              (return-from get-info-value
                (values (svref vector it) t)))))
        (return-from get-info-value (default))))
    (hash-get-info-value name0 type-number)))

;; Return the fdefn object for NAME, or NIL if there is no fdefn.
;; Signal an error if name isn't valid.
;; Trying to get this to work properly in file 'fdefinition.lisp'
;; was an exercise in futility.
;; Creation of new fdefinitions is still defined there though.
;; Assume that exists-p implies LEGAL-FUN-NAME-P.
;;
(defun find-fdefinition (name0)
  ;; Maybe we should proclaim VALUES as a declaration in 'cross-foo.lisp' ?
  #-sb-xc-host(declare (values (or fdefn null)))
  ;; Since this emulates GET-INFO-VALUE, we have to uncross the name.
  (let ((name (uncross name0)))
    (declare (optimize (safety 0)))
    (when (symbolp name) ; Don't bother with LEGAL-FUN-NAME-P.
      (return-from find-fdefinition
        (awhen (symbol-info-vector name)
          (let ((word (the fixnum (svref it 0))))
            ;; Require the first type-number to be +fdefn-type-num+
            ;; and the n-infos field to be nonzero.
            ;; No info vector has fewer than one element, so this code is safe.
            (and (eql (mask-field (byte type-number-bits type-number-bits) word)
                      (ash +fdefn-type-num+ type-number-bits))
                 (ldb-test (byte type-number-bits 0) word)
                 (svref it (1- (length it))))))))
    (with-compound-name (key1 key2) name
      (awhen (symbol-info-vector key1)
        (multiple-value-bind (data-idx descriptor-idx field-idx)
            (info-find-aux-key/packed it key2)
          (declare (index descriptor-idx)
                   (type (integer 0 #.+infos-per-word+) field-idx))
          ;; Secondary names must have at least one info, so if a descriptor
          ;; exists, there's no need to extract the n-infos field.
          (when data-idx
            (when (eql (incf field-idx) +infos-per-word+)
              (setq field-idx 0 descriptor-idx (1+ descriptor-idx)))
            (when (eql (packed-info-field it descriptor-idx field-idx)
                       +fdefn-type-num+)
              (return-from find-fdefinition
                (aref it (1- (the index data-idx))))))))
      (return-from find-fdefinition
        ;; (SETF SYM) needs no extra test. Otherwise check legality.
        (if (eq key1 'setf) nil (legal-fun-name-or-type-error name0))))
    (or (hash-get-info-value name0 +fdefn-type-num+)
        (legal-fun-name-or-type-error name0))))


;;;; definitions for function information

(define-info-class :function)

;; must be info type number 1
(define-info-type (:function :definition) :type-spec (or fdefn null))
(eval-when (:compile-toplevel)
  (aver (= 1 (type-info-number (type-info-or-lose :function :definition)))))

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

;;;; definitions for other miscellaneous information

(define-info-class :variable)

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
  ;; Delay evaluation of *UNIVERSAL-TYPE* since it can't work yet
  :default (lambda (x) (declare (ignore x)) *universal-type*))

;;; where this type and kind information came from
(define-info-type (:variable :where-from)
  :type-spec (member :declared :assumed :defined) :default :assumed)

;;; the macro-expansion for symbol-macros
(define-info-type (:variable :macro-expansion) :type-spec t)

(define-info-type (:variable :alien-info)
  :type-spec (or heap-alien-info null))

(define-info-type (:variable :documentation) :type-spec (or string null))

(define-info-class :type)

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

;;; layout for this type being used by the compiler
(define-info-type (:type :compiler-layout)
  :type-spec (or layout null)
  :default (lambda (name)
             (let ((class (find-classoid name nil)))
               (when class (classoid-layout class)))))

;;; DEFTYPE lambda-list
(define-info-type (:type :lambda-list) :type-spec list)

(define-info-type (:type :source-location) :type-spec t)

(define-info-class :typed-structure)
(define-info-type (:typed-structure :info) :type-spec t)
(define-info-type (:typed-structure :documentation) :type-spec (or string null))

;; CLTL2 offers an API to provide a list of known declarations, but it is
;; inefficient to iterate over info environments to find all such declarations,
;; and this is likely to be even slower when info is attached
;; directly to symbols, as it would entail do-all-symbols or similar.
;; Therefore maintain a list of recognized declarations. This list makes the
;; globaldb storage of same redundant, but oh well.
(defglobal *recognized-declarations* nil)
(define-info-class :declaration)
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

(define-info-class :alien-type)
(define-info-type (:alien-type :kind)
  :type-spec (member :primitive :defined :unknown)
  :default :unknown)
(define-info-type (:alien-type :translator) :type-spec (or function null))
(define-info-type (:alien-type :definition) :type-spec (or alien-type null))
(define-info-type (:alien-type :struct) :type-spec (or alien-type null))
(define-info-type (:alien-type :union) :type-spec (or alien-type null))
(define-info-type (:alien-type :enum) :type-spec (or alien-type null))

(define-info-class :setf)

(define-info-type (:setf :inverse) :type-spec (or symbol null))
(define-info-type (:setf :documentation) :type-spec (or string null))
(define-info-type (:setf :expander) :type-spec (or function null))

;;; This is used for storing miscellaneous documentation types. The
;;; stuff is an alist translating documentation kinds to values.
(define-info-class :random-documentation)
(define-info-type (:random-documentation :stuff) :type-spec list)

;;; Used to record the source location of definitions.
(define-info-class :source-location)

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
         (format t "~&  ~@[type ~D~]~@[~{~S ~S~}~] ~S = "
                 (if (not type) type-num)
                 (if type
                     (list (class-info-name (type-info-class type))
                           (type-info-name type)))
                 name)
         (write val :level 1)))
     sym)))


;;; Now that we have finished initializing *INFO-CLASSES* and
;;; *INFO-TYPES* (at compile time), generate code to set them at cold
;;; load time to the same state they have currently.
(!cold-init-forms
  (/show0 "beginning *INFO-CLASSES* init, calling MAKE-HASH-TABLE")
  (setf *info-classes*
        (make-hash-table :test 'eq :size #.(* 2 (hash-table-count *info-classes*))))
  (/show0 "done with MAKE-HASH-TABLE in *INFO-CLASSES* init")
  (dolist (class-info-name '#.(let ((result nil))
                                (maphash (lambda (key value)
                                           (declare (ignore value))
                                           (push key result))
                                         *info-classes*)
                                (sort result #'string<)))
    (let ((class-info (make-class-info class-info-name)))
      (setf (gethash class-info-name *info-classes*)
            class-info)))
  (/show0 "done with *INFO-CLASSES* initialization")
  (/show0 "beginning *INFO-TYPES* initialization")
  (setf *info-types*
        (map 'vector
             (lambda (x)
               (/show0 "in LAMBDA (X), X=..")
               (/hexstr x)
               (when x
                 (let* ((class-info (class-info-or-lose (second x)))
                        (type-info (make-type-info :name (first x)
                                                   :class class-info
                                                   :number (third x)
                                                   :type (fourth x))))
                   (/show0 "got CLASS-INFO in LAMBDA (X)")
                   (push type-info (class-info-types class-info))
                   type-info)))
             '#.(map 'list
                     (lambda (info-type)
                       (when info-type
                         (list (type-info-name info-type)
                               (class-info-name (type-info-class info-type))
                               (type-info-number info-type)
                               ;; KLUDGE: for repeatable xc fasls, to
                               ;; avoid different cross-compiler
                               ;; treatment of equal constants here we
                               ;; COPY-TREE, which is not in general a
                               ;; valid identity transformation
                               ;; [e.g. on (EQL (FOO))] but is OK for
                               ;; all the types we use here.
                               (copy-tree (type-info-type info-type)))))
                     *info-types*)))
  (/show0 "done with *INFO-TYPES* initialization"))

;;; At cold load time, after the INFO-TYPE objects have been created,
;;; we can set their DEFAULT and TYPE slots.
(macrolet ((frob ()
             `(!cold-init-forms
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
           (values (truly-the ,(type-info-type info) ,value) ,foundp)))))

  (def (setf info) (new-value class type name)
    (let* (#+sb-xc-host (sb!xc:*gensym-counter* sb!xc:*gensym-counter*)
           (info (type-info-or-lose class type))
           (tin (type-info-number info))
           (type-spec (type-info-type info))
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
      `(clear-info-value ,name ,(type-info-number info)))))

(!defun-from-collected-cold-init-forms !globaldb-cold-init)
