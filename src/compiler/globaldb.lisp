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

;;; At run time, we represent the type of info that we want by a small
;;; non-negative integer.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (def!constant type-number-bits 6))
(deftype type-number () `(unsigned-byte ,type-number-bits))

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
  ;; a function called when there is no information of this type
  (default (lambda () (error "type not defined yet")) :type function)
  ;; called by (SETF INFO) before calling SET-INFO-VALUE
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
(defun find-unused-type-number ()
  (or (position nil *info-types*)
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
;;; name of the type, DEFAULT is the value for that type when it
;;; hasn't been set, and TYPE-SPEC is a type specifier which values of
;;; the type must satisfy. The default expression is evaluated each
;;; time the information is needed, with NAME bound to the name for
;;; which the information is being looked up.
;;;
;;; The main thing we do is determine the type's number. We need to do
;;; this at macroexpansion time, since both the COMPILE and LOAD time
;;; calls to %DEFINE-INFO-TYPE must use the same type number.
(#+sb-xc-host defmacro
 #-sb-xc-host sb!xc:defmacro
    define-info-type (&key (class (missing-arg))
                           (type (missing-arg))
                           (type-spec (missing-arg))
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
       ;; Arrange for TYPE-INFO-DEFAULT and
       ;; TYPE-INFO-VALIDATE-FUNCTION to be set at cold load
       ;; time. (They can't very well be set at cross-compile time,
       ;; since they differ between host and target and are
       ;; host-compiled closures.)
       (push `(let ((type-info (type-info-or-lose ,',class ,',type)))
                (setf (type-info-validate-function type-info)
                      ,',validate-function)
                (setf (type-info-default type-info)
                       ;; FIXME: This code is sort of nasty. It would
                       ;; be cleaner if DEFAULT accepted a real
                       ;; function, instead of accepting a statement
                       ;; which will be turned into a lambda assuming
                       ;; that the argument name is NAME. It might
                       ;; even be more microefficient, too, since many
                       ;; DEFAULTs could be implemented as (CONSTANTLY
                       ;; NIL) instead of full-blown (LAMBDA (X) NIL).
                       (lambda (name)
                         (declare (ignorable name))
                         ,',default)))
             *!reversed-type-info-init-forms*))
     ',type))

) ; EVAL-WHEN

;;;; generic info environments

(defstruct (info-env (:constructor nil)
                     (:copier nil))
  ;; some string describing what is in this environment, for
  ;; printing/debugging purposes only
  (name (missing-arg) :type string))
(def!method print-object ((x info-env) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (info-env-name x) stream)))

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
(defstruct (compact-info-env (:include info-env)
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

;;; Return the value of the type corresponding to NUMBER for the
;;; index INDEX in ENV.
#!-sb-fluid (declaim (inline compact-info-lookup-index))
(defun compact-info-lookup-index (env number index)
  (declare (type compact-info-env env) (type type-number number))
  (let ((entries-info (compact-info-env-entries-info env)))
    (if index
        (do ((index index (1+ index)))
            (nil)
          (declare (type index index))
          (let ((info (aref entries-info index)))
            (when (= (logand info compact-info-entry-type-mask) number)
              (return (values (svref (compact-info-env-entries env) index)
                              t)))
            (unless (zerop (logand compact-info-entry-last info))
              (return (values nil nil)))))
        (values nil nil))))

;;; Look up NAME in the compact environment ENV. HASH is the
;;; GLOBALDB-SXHASHOID of NAME.
(defun compact-info-lookup (env name hash number)
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
                                 number
                                 (aref (compact-info-env-index env) probe))))))))
      (if (symbolp name)
          (lookup eq)
          (lookup equal)))))

;;; the exact density (modulo rounding) of the hashtable in a compact
;;; info environment in names/bucket
(def!constant compact-info-environment-density 65)

;;; Return a new compact info environment that holds the same
;;; information as ENV.
(defun compact-info-environment (env &key (name (info-env-name env)))
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
(defstruct (volatile-info-env (:include info-env)
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
(defun volatile-info-lookup (env name hash number)
  (declare (type volatile-info-env env)
           (type (integer 0 #.sb!xc:most-positive-fixnum) hash))
  (let ((table (volatile-info-env-table env)))
    (macrolet ((lookup (test)
                 `(dolist (entry (svref table (mod hash (length table))) ())
                    (when (,test (car entry) name)
                      (dolist (type (cdr entry))
                        (when (eql (car type) number)
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
(defun set-info-value (name0 type new-value
                             &optional (env (get-write-info-env)))
  (declare (type type-number type) (type volatile-info-env env)
           (inline assoc))
  (let ((name (uncross name0)))
    (when (eql name 0)
      (error "0 is not a legal INFO name."))
    (with-info-bucket (table index name env)
      (let ((types (if (symbolp name)
                       (assoc name (svref table index) :test #'eq)
                       (assoc name (svref table index) :test #'equal))))
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
                         (set-info-value entry-name entry-num entry-val new))
                (fill (volatile-info-env-table env) nil)
                (setf (volatile-info-env-table env)
                      (volatile-info-env-table new))
                (setf (volatile-info-env-threshold env)
                      (volatile-info-env-threshold new)))))))))
    new-value))

;;; FIXME: It should be possible to eliminate the hairy compiler macros below
;;; by declaring INFO and (SETF INFO) inline and making a simple compiler macro
;;; for TYPE-INFO-OR-LOSE. (If we didn't worry about efficiency of the
;;; cross-compiler, we could even do it by just making TYPE-INFO-OR-LOSE
;;; foldable.)

;;; INFO is the standard way to access the database. It's settable.
;;;
;;; Return the information of the specified TYPE and CLASS for NAME.
;;; The second value returned is true if there is any such information
;;; recorded. If there is no information, the first value returned is
;;; the default and the second value returned is NIL.
(defun info (class type name &optional (env-list nil env-list-p))
  ;; FIXME: At some point check systematically to make sure that the
  ;; system doesn't do any full calls to INFO or (SETF INFO), or at
  ;; least none in any inner loops.
  (let ((info (type-info-or-lose class type)))
    (if env-list-p
        (get-info-value name (type-info-number info) env-list)
        (get-info-value name (type-info-number info)))))
#!-sb-fluid
(define-compiler-macro info
  (&whole whole class type name &optional (env-list nil env-list-p))
  ;; Constant CLASS and TYPE is an overwhelmingly common special case,
  ;; and we can implement it much more efficiently than the general case.
  (if (and (keywordp class) (keywordp type))
      (let (#+sb-xc-host (sb!xc:*gensym-counter* sb!xc:*gensym-counter*)
            (info (type-info-or-lose class type)))
        (with-unique-names (value foundp)
          `(multiple-value-bind (,value ,foundp)
               (get-info-value ,name
                               ,(type-info-number info)
                               ,@(when env-list-p `(,env-list)))
             (declare (type ,(type-info-type info) ,value))
             (values ,value ,foundp))))
      whole))

(defun (setf info)
    (new-value class type name &optional (env-list nil env-list-p))
  (let* ((info (type-info-or-lose class type))
         (tin (type-info-number info)))
    (when (type-info-validate-function info)
      (funcall (type-info-validate-function info) name new-value))
    (if env-list-p
        (set-info-value name
                        tin
                        new-value
                        (get-write-info-env env-list))
        (set-info-value name
                        tin
                        new-value)))
  new-value)
#!-sb-fluid
(progn
  ;; Not all xc hosts are happy about SETF compiler macros: CMUCL 19
  ;; does not accept them at all, and older SBCLs give a full warning.
  ;; So the easy thing is to hide this optimization from all xc hosts.
  #-sb-xc-host
  (define-compiler-macro (setf info)
      (&whole whole new-value class type name &optional (env-list nil env-list-p))
    ;; Constant CLASS and TYPE is an overwhelmingly common special case,
    ;; and we can resolve it much more efficiently than the general
    ;; case.
    (if (and (keywordp class) (keywordp type))
        (let* ((info (type-info-or-lose class type))
               (tin (type-info-number info)))
          (if env-list-p
              `(set-info-value ,name
                               ,tin
                               ,new-value
                               (get-write-info-env ,env-list))
              `(set-info-value ,name
                               ,tin
                               ,new-value))))
    whole))

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

;;; Clear the information of the specified TYPE and CLASS for NAME in
;;; the current environment, allowing any inherited info to become
;;; visible. We return true if there was any info.
(defun clear-info (class type name)
  (let ((info (type-info-or-lose class type)))
    (clear-info-value name (type-info-number info))))
#!-sb-fluid
(define-compiler-macro clear-info (&whole whole class type name)
  ;; Constant CLASS and TYPE is an overwhelmingly common special case, and
  ;; we can resolve it much more efficiently than the general case.
  (if (and (keywordp class) (keywordp type))
    (let ((info (type-info-or-lose class type)))
      `(clear-info-value ,name ,(type-info-number info)))
    whole))
(defun clear-info-value (name type)
  (declare (type type-number type) (inline assoc))
  (with-info-bucket (table index name (get-write-info-env))
    (let ((types (assoc name (svref table index) :test #'equal)))
      (when (and types
                 (assoc type (cdr types)))
        (setf (cdr types)
              (delete type (cdr types) :key #'car))
        t))))

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

;;; Return the value of NAME / TYPE from the first environment where
;;; has it defined, or return the default if none does. We used to
;;; do a lot of complicated caching here, but that was removed for
;;; thread-safety reasons.
(defun get-info-value (name0 type &optional (env-list nil env-list-p))
  (declare (type type-number type))
  ;; sanity check: If we have screwed up initialization somehow, then
  ;; *INFO-TYPES* could still be uninitialized at the time we try to
  ;; get an info value, and then we'd be out of luck. (This happened,
  ;; and was confusing to debug, when rewriting EVAL-WHEN in
  ;; sbcl-0.pre7.x.)
  (aver (aref *info-types* type))
  (let ((name (uncross name0)))
    (flet ((lookup (env-list)
             (let ((hash nil))
               (dolist (env env-list
                        (multiple-value-bind (val winp)
                            (funcall (type-info-default
                                      (svref *info-types* type))
                                     name)
                          (values val winp)))
                 (macrolet ((frob (lookup)
                              `(progn
                                 (setq hash (globaldb-sxhashoid name))
                                 (multiple-value-bind (value winp)
                                     (,lookup env name hash type)
                                   (when winp (return (values value t)))))))
                   (etypecase env
                     (volatile-info-env (frob volatile-info-lookup))
                     (compact-info-env (frob compact-info-lookup))))))))
      (if env-list-p
          (lookup env-list)
          (lookup *info-environment*)))))

;;;; definitions for function information

(define-info-class :function)

;;; the kind of functional object being described. If null, NAME isn't
;;; a known functional object.
(define-info-type
  :class :function
  :type :kind
  :type-spec (member nil :function :macro :special-form)
  ;; I'm a little confused what the correct behavior of this default
  ;; is. It's not clear how to generalize the FBOUNDP expression to
  ;; the cross-compiler. As far as I can tell, NIL is a safe default
  ;; -- it might keep the compiler from making some valid
  ;; optimization, but it shouldn't produce incorrect code. -- WHN
  ;; 19990330
  :default
  #+sb-xc-host nil
  #-sb-xc-host (if (fboundp name) :function nil))

;;; The type specifier for this function.
(define-info-type
  :class :function
  :type :type
  :type-spec ctype
  ;; Again (as in DEFINE-INFO-TYPE :CLASS :FUNCTION :TYPE :KIND) it's
  ;; not clear how to generalize the FBOUNDP expression to the
  ;; cross-compiler. -- WHN 19990330
  :default
  #+sb-xc-host (specifier-type 'function)
  #-sb-xc-host (if (fboundp name)
                   (specifier-type (sb!impl::%fun-type (fdefinition name)))
                   (specifier-type 'function)))

;;; the ASSUMED-TYPE for this function, if we have to infer the type
;;; due to not having a declaration or definition
(define-info-type
  :class :function
  :type :assumed-type
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
(define-info-type
  :class :function
  :type :where-from
  :type-spec (member :declared :defined-method :assumed :defined)
  :default
  ;; Again (as in DEFINE-INFO-TYPE :CLASS :FUNCTION :TYPE :KIND) it's
  ;; not clear how to generalize the FBOUNDP expression to the
  ;; cross-compiler. -- WHN 19990606
  #+sb-xc-host :assumed
  #-sb-xc-host (if (fboundp name) :defined :assumed))

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
(define-info-type
  :class :function
  :type :inline-expansion-designator
  :type-spec (or list function)
  :default nil)

;;; This specifies whether this function may be expanded inline. If
;;; null, we don't care.
(define-info-type
  :class :function
  :type :inlinep
  :type-spec inlinep
  :default nil)

;;; a macro-like function which transforms a call to this function
;;; into some other Lisp form. This expansion is inhibited if inline
;;; expansion is inhibited
(define-info-type
  :class :function
  :type :source-transform
  :type-spec (or function null))

;;; the macroexpansion function for this macro
(define-info-type
  :class :function
  :type :macro-function
  :type-spec (or function null)
  :default nil)

;;; the compiler-macroexpansion function for this macro
(define-info-type
  :class :function
  :type :compiler-macro-function
  :type-spec (or function null)
  :default nil)

;;; a function which converts this special form into IR1
(define-info-type
  :class :function
  :type :ir1-convert
  :type-spec (or function null))

;;; If a function is "known" to the compiler, then this is a FUN-INFO
;;; structure containing the info used to special-case compilation.
(define-info-type
  :class :function
  :type :info
  :type-spec (or fun-info null)
  :default nil)

(define-info-type
  :class :function
  :type :definition
  :type-spec (or fdefn null)
  :default nil)

(define-info-type
  :class :function
  :type :structure-accessor
  :type-spec (or defstruct-description null)
  :default nil)

;;;; definitions for other miscellaneous information

(define-info-class :variable)

;;; the kind of variable-like thing described
(define-info-type
  :class :variable
  :type :kind
  :type-spec (member :special :constant :macro :global :alien :unknown)
  :default (if (typep name '(or boolean keyword))
               :constant
               :unknown))

(define-info-type
  :class :variable
  :type :always-bound
  :type-spec boolean
  :default nil)

;;; the declared type for this variable
(define-info-type
  :class :variable
  :type :type
  :type-spec ctype
  :default *universal-type*)

;;; where this type and kind information came from
(define-info-type
  :class :variable
  :type :where-from
  :type-spec (member :declared :assumed :defined)
  :default :assumed)

;;; We only need a mechanism different from the
;;; usual SYMBOL-VALUE for the cross compiler.
#+sb-xc-host
(define-info-type
  :class :variable
  :type :xc-constant-value
  :type-spec t
  :default nil)

;;; the macro-expansion for symbol-macros
(define-info-type
  :class :variable
  :type :macro-expansion
  :type-spec t
  :default nil)

(define-info-type
  :class :variable
  :type :alien-info
  :type-spec (or heap-alien-info null)
  :default nil)

(define-info-type
  :class :variable
  :type :documentation
  :type-spec (or string null)
  :default nil)

(define-info-class :type)

;;; the kind of type described. We return :INSTANCE for standard types
;;; that are implemented as structures. For PCL classes, that have
;;; only been compiled, but not loaded yet, we return
;;; :FORTHCOMING-DEFCLASS-TYPE.
(define-info-type
  :class :type
  :type :kind
  :type-spec (member :primitive :defined :instance
                     :forthcoming-defclass-type nil)
  :default nil
  :validate-function (lambda (name new-value)
                       (declare (ignore new-value)
                                (notinline info))
                       (when (info :declaration :recognized name)
                         (error 'declaration-type-conflict-error
                                :format-arguments (list name)))))

;;; the expander function for a defined type
(define-info-type
  :class :type
  :type :expander
  :type-spec (or function null)
  :default nil)

(define-info-type
  :class :type
  :type :documentation
  :type-spec (or string null))

;;; function that parses type specifiers into CTYPE structures
(define-info-type
  :class :type
  :type :translator
  :type-spec (or function null)
  :default nil)

;;; If true, then the type coresponding to this name. Note that if
;;; this is a built-in class with a translation, then this is the
;;; translation, not the class object. This info type keeps track of
;;; various atomic types (NIL etc.) and also serves as a cache to
;;; ensure that common standard types (atomic and otherwise) are only
;;; consed once.
(define-info-type
  :class :type
  :type :builtin
  :type-spec (or ctype null)
  :default nil)

;;; layout for this type being used by the compiler
(define-info-type
  :class :type
  :type :compiler-layout
  :type-spec (or layout null)
  :default (let ((class (find-classoid name nil)))
             (when class (classoid-layout class))))

;;; DEFTYPE lambda-list
(define-info-type
   :class :type
   :type :lambda-list
   :type-spec list
   :default nil)

(define-info-type
   :class :type
   :type :source-location
   :type-spec t
   :default nil)

(define-info-class :typed-structure)
(define-info-type
  :class :typed-structure
  :type :info
  :type-spec t
  :default nil)
(define-info-type
  :class :typed-structure
  :type :documentation
  :type-spec (or string null)
  :default nil)

(define-info-class :declaration)
(define-info-type
  :class :declaration
  :type :recognized
  :type-spec boolean
  :validate-function (lambda (name new-value)
                       (declare (ignore new-value)
                                (notinline info))
                       (when (info :type :kind name)
                         (error 'declaration-type-conflict-error
                                :format-arguments (list name)))))
(define-info-type
  :class :declaration
  :type :handler
  :type-spec (or function null))

(define-info-class :alien-type)
(define-info-type
  :class :alien-type
  :type :kind
  :type-spec (member :primitive :defined :unknown)
  :default :unknown)
(define-info-type
  :class :alien-type
  :type :translator
  :type-spec (or function null)
  :default nil)
(define-info-type
  :class :alien-type
  :type :definition
  :type-spec (or alien-type null)
  :default nil)
(define-info-type
  :class :alien-type
  :type :struct
  :type-spec (or alien-type null)
  :default nil)
(define-info-type
  :class :alien-type
  :type :union
  :type-spec (or alien-type null)
  :default nil)
(define-info-type
  :class :alien-type
  :type :enum
  :type-spec (or alien-type null)
  :default nil)

(define-info-class :setf)

(define-info-type
  :class :setf
  :type :inverse
  :type-spec (or symbol null)
  :default nil)

(define-info-type
  :class :setf
  :type :documentation
  :type-spec (or string null)
  :default nil)

(define-info-type
  :class :setf
  :type :expander
  :type-spec (or function null)
  :default nil)

;;; This is used for storing miscellaneous documentation types. The
;;; stuff is an alist translating documentation kinds to values.
(define-info-class :random-documentation)
(define-info-type
  :class :random-documentation
  :type :stuff
  :type-spec list
  :default ())

;;; Used to record the source location of definitions.
(define-info-class :source-location)

(define-info-type
  :class :source-location
  :type :variable
  :type-spec t
  :default nil)

(define-info-type
  :class :source-location
  :type :constant
  :type-spec t
  :default nil)

(define-info-type
  :class :source-location
  :type :typed-structure
  :type-spec t
  :default nil)

(define-info-type
  :class :source-location
  :type :symbol-macro
  :type-spec t
  :default nil)

#!-sb-fluid (declaim (freeze-type info-env))

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

;;;; a hack for detecting
;;;;   (DEFUN FOO (X Y)
;;;;     ..
;;;;     (SETF (BAR A FFH) 12) ; compiles to a call to #'(SETF BAR)
;;;;     ..)
;;;;   (DEFSETF BAR SET-BAR) ; can't influence previous compilation
;;;;
;;;; KLUDGE: Arguably it should be another class/type combination in
;;;; the globaldb. However, IMHO the whole globaldb/fdefinition
;;;; treatment of SETF functions is a mess which ought to be
;;;; rewritten, and I'm not inclined to mess with it short of that. So
;;;; I just put this bag on the side of it instead..

;;; true for symbols FOO which have been assumed to have '(SETF FOO)
;;; bound to a function
(defvar *setf-assumed-fboundp*)
(!cold-init-forms (setf *setf-assumed-fboundp* (make-hash-table)))

(!defun-from-collected-cold-init-forms !globaldb-cold-init)
